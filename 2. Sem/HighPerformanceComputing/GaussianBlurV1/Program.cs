// MSE-2A SS2024
// Polley, Simanek, Klement

// input and output arrays
using System.Drawing;
using OpenCL.Net;

Console.WriteLine("Current directory: " + Directory.GetCurrentDirectory());
using var file = File.OpenRead("input.jpg");
using var inputImage = new Bitmap(file);
using var outputImage = new Bitmap(inputImage.Size.Width, inputImage.Size.Height);
var inputImageData = inputImage.LockBits(new(0, 0, inputImage.Size.Width, inputImage.Size.Height), System.Drawing.Imaging.ImageLockMode.ReadOnly, System.Drawing.Imaging.PixelFormat.Format32bppArgb);
var outputImageData = outputImage.LockBits(new(0, 0, outputImage.Size.Width, outputImage.Size.Height), System.Drawing.Imaging.ImageLockMode.WriteOnly, System.Drawing.Imaging.PixelFormat.Format32bppArgb);
int blurRadius = 4, convKernelSize = blurRadius * 2 + 1;
float[] convKernel = CreateGaussianKernel(convKernelSize, blurRadius);

// used for checking error status of api calls
ErrorCode status;

// retrieve the number of platforms
uint numPlatforms = 0;
CheckStatus(Cl.GetPlatformIDs(0, null, out numPlatforms));

if (numPlatforms == 0)
{
    Console.WriteLine("Error: No OpenCL platform available!");
    System.Environment.Exit(1);
}

// select the platform
Platform[] platforms = new Platform[numPlatforms];
CheckStatus(Cl.GetPlatformIDs(1, platforms, out numPlatforms));
Platform platform = platforms[0];

// retrieve the number of devices
uint numDevices = 0;
CheckStatus(Cl.GetDeviceIDs(platform, DeviceType.All, 0, null, out numDevices));

if (numDevices == 0)
{
    Console.WriteLine("Error: No OpenCL device available for platform!");
    System.Environment.Exit(1);
}

// select the device
Device[] devices = new Device[numDevices];
CheckStatus(Cl.GetDeviceIDs(platform, DeviceType.All, numDevices, devices, out numDevices));
Device device = devices[0];

// create context
Context context = Cl.CreateContext(null, 1, new Device[] { device }, null, IntPtr.Zero, out status);
CheckStatus(status);

// create command queue
CommandQueue commandQueue = Cl.CreateCommandQueue(context, device, 0, out status);
CheckStatus(status);

// allocate two input and one output buffer for the three vectors
IMem<uchar4> bufferInputImage = Cl.CreateBuffer<uchar4>(context, MemFlags.ReadOnly, inputImageData.Stride * inputImageData.Height, out status);
CheckStatus(status);
IMem<float> bufferConvKernel = Cl.CreateBuffer<float>(context, MemFlags.ReadOnly, convKernel.Length, out status);
CheckStatus(status);
IMem<uchar4> bufferOutputImage = Cl.CreateBuffer<uchar4>(context, MemFlags.WriteOnly, outputImageData.Stride * outputImageData.Height, out status);
CheckStatus(status);

// write data from the input vectors to the buffers
CheckStatus(Cl.EnqueueWriteBuffer(commandQueue, bufferInputImage, Bool.True, IntPtr.Zero, inputImageData.Stride * inputImageData.Height, inputImageData.Scan0, 0, null, out var _));
CheckStatus(Cl.EnqueueWriteBuffer(commandQueue, bufferConvKernel, Bool.True, IntPtr.Zero, convKernel.Length * sizeof(float), convKernel, 0, null, out var _));

// create the program
string programSource = File.ReadAllText("kernel.cl");
OpenCL.Net.Program program = Cl.CreateProgramWithSource(context, 1, new string[] { programSource }, null, out status);
CheckStatus(status);

// build the program
status = Cl.BuildProgram(program, 1, new Device[] { device }, "", null, IntPtr.Zero);
if (status != ErrorCode.Success)
{
    InfoBuffer infoBuffer = Cl.GetProgramBuildInfo(program, device, ProgramBuildInfo.Log, out status);
    CheckStatus(status);
    Console.WriteLine("Build Error: " + infoBuffer.ToString());
    System.Environment.Exit(1);
}

// create the vector addition kernel
OpenCL.Net.Kernel kernel = Cl.CreateKernel(program, "blur", out status);
CheckStatus(status);

// set the kernel arguments
CheckStatus(Cl.SetKernelArg(kernel, 0, bufferInputImage));
CheckStatus(Cl.SetKernelArg(kernel, 1, inputImage.Width));
CheckStatus(Cl.SetKernelArg(kernel, 2, inputImage.Height));
CheckStatus(Cl.SetKernelArg(kernel, 3, bufferConvKernel));
CheckStatus(Cl.SetKernelArg(kernel, 4, convKernelSize));
CheckStatus(Cl.SetKernelArg(kernel, 5, bufferOutputImage));

// output device capabilities
IntPtr paramSize;
CheckStatus(Cl.GetDeviceInfo(device, DeviceInfo.MaxWorkGroupSize, IntPtr.Zero, InfoBuffer.Empty, out paramSize));
InfoBuffer maxWorkGroupSizeBuffer = new InfoBuffer(paramSize);
CheckStatus(Cl.GetDeviceInfo(device, DeviceInfo.MaxWorkGroupSize, paramSize, maxWorkGroupSizeBuffer, out paramSize));
int maxWorkGroupSize = maxWorkGroupSizeBuffer.CastTo<int>();
Console.WriteLine("Device Capabilities: Max work items in single group: " + maxWorkGroupSize);

CheckStatus(Cl.GetDeviceInfo(device, DeviceInfo.MaxWorkItemDimensions, IntPtr.Zero, InfoBuffer.Empty, out paramSize));
InfoBuffer dimensionInfoBuffer = new InfoBuffer(paramSize);
CheckStatus(Cl.GetDeviceInfo(device, DeviceInfo.MaxWorkItemDimensions, paramSize, dimensionInfoBuffer, out paramSize));
int maxWorkItemDimensions = dimensionInfoBuffer.CastTo<int>();
Console.WriteLine("Device Capabilities: Max work item dimensions: " + maxWorkItemDimensions);

CheckStatus(Cl.GetDeviceInfo(device, DeviceInfo.MaxWorkItemSizes, IntPtr.Zero, InfoBuffer.Empty, out paramSize));
InfoBuffer maxWorkItemSizesInfoBuffer = new InfoBuffer(paramSize);
CheckStatus(Cl.GetDeviceInfo(device, DeviceInfo.MaxWorkItemSizes, paramSize, maxWorkItemSizesInfoBuffer, out paramSize));
IntPtr[] maxWorkItemSizes = maxWorkItemSizesInfoBuffer.CastToArray<IntPtr>(maxWorkItemDimensions);
Console.Write("Device Capabilities: Max work items in group per dimension:");
for (int i = 0; i < maxWorkItemDimensions; ++i)
    Console.Write(" " + i + ":" + maxWorkItemSizes[i]);
Console.WriteLine();

// execute the kernel
// ndrange capabilities only need to be checked when we specify a local work group size manually
// in our case we provide NULL as local work group size, which means groups get formed automatically
CheckStatus(Cl.EnqueueNDRangeKernel(commandQueue, kernel, 2, null, new IntPtr[] { new(inputImage.Size.Width), new(inputImage.Size.Height) }, null, 0, null, out var _));

// read the device output buffer to the host output array
CheckStatus(Cl.EnqueueReadBuffer(commandQueue, bufferOutputImage, Bool.True, IntPtr.Zero, outputImageData.Stride * outputImageData.Height, outputImageData.Scan0, 0, null, out var _));
inputImage.UnlockBits(inputImageData);
outputImage.UnlockBits(outputImageData);

// output result
outputImage.Save("output.png", System.Drawing.Imaging.ImageFormat.Png);

// release opencl objects
CheckStatus(Cl.ReleaseKernel(kernel));
CheckStatus(Cl.ReleaseProgram(program));
CheckStatus(Cl.ReleaseMemObject(bufferInputImage));
CheckStatus(Cl.ReleaseMemObject(bufferConvKernel));
CheckStatus(Cl.ReleaseMemObject(bufferOutputImage));
CheckStatus(Cl.ReleaseCommandQueue(commandQueue));
CheckStatus(Cl.ReleaseContext(context));

static void CheckStatus(ErrorCode err)
{
    if (err != ErrorCode.Success)
    {
        Console.WriteLine("OpenCL Error: " + err.ToString());
        System.Environment.Exit(1);
    }
}

static float[] CreateGaussianKernel(int size, double sigma)
{
    float[] kernel = new float[size * size];
    double sumTotal = 0;
    int radius = size / 2;
    double calculatedEuler = 1.0 / (2.0 * Math.PI * sigma * sigma);

    for (int filterY = -radius; filterY <= radius; filterY++)
    {
        for (int filterX = -radius; filterX <= radius; filterX++)
        {
            double distance = ((filterX * filterX) + (filterY * filterY)) / (2 * (sigma * sigma));
            var value = (float)(calculatedEuler * Math.Exp(-distance));

            kernel[(filterY + radius) * size + (filterX + radius)] = value;

            sumTotal += value;
        }
    }

    for (int y = 0; y < size; y++)
    {
        for (int x = 0; x < size; x++)
        {
            kernel[y * size + x] = (float)(kernel[y * size + x] / sumTotal);
        }
    }

    return kernel;
}
