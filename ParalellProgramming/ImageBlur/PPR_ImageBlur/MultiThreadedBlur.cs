using System.Drawing;

namespace PPR_ImageBlur;

public static class MultiThreadedBlur
{
    public static Bitmap ApplyGaussianBlur_ParallelWidth(Bitmap image, int blurRadius)
    {
        int kernelSize = blurRadius * 2 + 1;
        float[,] kernel = CreateGaussianKernel(kernelSize, blurRadius);
        return ConvolutionFilter_ParallelWidth(image, kernel);
    }

    public static Bitmap ApplyGaussianBlur_ParallelHeight(Bitmap image, int blurRadius)
    {
        int kernelSize = blurRadius * 2 + 1;
        float[,] kernel = CreateGaussianKernel(kernelSize, blurRadius);
        return ConvolutionFilter_ParallelHeight(image, kernel);
    }

    public static Bitmap ApplyGaussianBlur_ParallelDual(Bitmap image, int blurRadius)
    {
        int kernelSize = blurRadius * 2 + 1;
        float[,] kernel = CreateGaussianKernel(kernelSize, blurRadius);
        return ConvolutionFilter_ParallelDual(image, kernel);
    }

    private static float[,] CreateGaussianKernel(int size, double sigma)
    {
        float[,] kernel = new float[size, size];
        double sumTotal = 0;
        int radius = size / 2;
        double calculatedEuler = 1.0 / (2.0 * Math.PI * sigma * sigma);

        for (int filterY = -radius; filterY <= radius; filterY++)
        {
            for (int filterX = -radius; filterX <= radius; filterX++)
            {
                double distance = ((filterX * filterX) + (filterY * filterY)) / (2 * (sigma * sigma));
                var value = (float)(calculatedEuler * Math.Exp(-distance));

                kernel[filterY + radius, filterX + radius] = value;

                sumTotal += value;
            }
        }

        for (int y = 0; y < size; y++)
        {
            for (int x = 0; x < size; x++)
            {
                kernel[y, x] = (float)(kernel[y, x] / sumTotal);
            }
        }

        return kernel;
    }

    private static unsafe Bitmap ConvolutionFilter_ParallelWidth(Bitmap inImage, float[,] kernel)
    {
        int width = inImage.Width, kWidth = kernel.GetLength(0);
        int height = inImage.Height, kHeight = kernel.GetLength(1);
        Bitmap outImage = new(width, height);
        var inData = inImage.LockBits(new Rectangle(default, inImage.Size), System.Drawing.Imaging.ImageLockMode.ReadOnly, System.Drawing.Imaging.PixelFormat.Format24bppRgb);
        var outData = outImage.LockBits(new Rectangle(default, outImage.Size), System.Drawing.Imaging.ImageLockMode.ReadWrite, System.Drawing.Imaging.PixelFormat.Format24bppRgb);

        try
        {
            BgrColor* inPtr = (BgrColor*)inData.Scan0;
            BgrColor* outPtr = (BgrColor*)outData.Scan0;

            Parallel.For(0, width, x =>
            {
                //Console.WriteLine($"{x}/{width}");

                for (int y = 0; y < height; y++)
                {
                    float r = 0, g = 0, b = 0;

                    for (int i = 0; i < kWidth; i++)
                    {
                        for (int j = 0; j < kHeight; j++)
                        {
                            int offsetX = x + i - kWidth / 2;
                            int offsetY = y + j - kHeight / 2;

                            offsetX = Math.Clamp(offsetX, 0, width - 1);
                            offsetY = Math.Clamp(offsetY, 0, height - 1);

                            BgrColor pixel = inPtr[offsetY * width + offsetX];
                            float weight = kernel[i, j];

                            r += pixel.r * weight;
                            g += pixel.g * weight;
                            b += pixel.b * weight;
                        }
                    }

                    outPtr[y * width + x].r = (byte)r;
                    outPtr[y * width + x].g = (byte)g;
                    outPtr[y * width + x].b = (byte)b;
                }
            });
        }
        finally
        {
            inImage.UnlockBits(inData);
            outImage.UnlockBits(outData);
        }

        return outImage;
    }

    private static unsafe Bitmap ConvolutionFilter_ParallelHeight(Bitmap inImage, float[,] kernel)
    {
        int width = inImage.Width, kWidth = kernel.GetLength(0);
        int height = inImage.Height, kHeight = kernel.GetLength(1);
        Bitmap outImage = new(width, height);
        var inData = inImage.LockBits(new Rectangle(default, inImage.Size), System.Drawing.Imaging.ImageLockMode.ReadOnly, System.Drawing.Imaging.PixelFormat.Format24bppRgb);
        var outData = outImage.LockBits(new Rectangle(default, outImage.Size), System.Drawing.Imaging.ImageLockMode.ReadWrite, System.Drawing.Imaging.PixelFormat.Format24bppRgb);

        try
        {
            BgrColor* inPtr = (BgrColor*)inData.Scan0;
            BgrColor* outPtr = (BgrColor*)outData.Scan0;

            for (int x = 0; x < width; x++)
            {
                //Console.WriteLine($"{x}/{width}");

                Parallel.For(0, height, y =>
                {
                    float r = 0, g = 0, b = 0;

                    for (int i = 0; i < kWidth; i++)
                    {
                        for (int j = 0; j < kHeight; j++)
                        {
                            int offsetX = x + i - kWidth / 2;
                            int offsetY = y + j - kHeight / 2;

                            offsetX = Math.Clamp(offsetX, 0, width - 1);
                            offsetY = Math.Clamp(offsetY, 0, height - 1);

                            BgrColor pixel = inPtr[offsetY * width + offsetX];
                            float weight = kernel[i, j];

                            r += pixel.r * weight;
                            g += pixel.g * weight;
                            b += pixel.b * weight;
                        }
                    }

                    outPtr[y * width + x].r = (byte)r;
                    outPtr[y * width + x].g = (byte)g;
                    outPtr[y * width + x].b = (byte)b;
                });
            }
        }
        finally
        {
            inImage.UnlockBits(inData);
            outImage.UnlockBits(outData);
        }

        return outImage;
    }

    private static unsafe Bitmap ConvolutionFilter_ParallelDual(Bitmap inImage, float[,] kernel)
    {
        int width = inImage.Width, kWidth = kernel.GetLength(0);
        int height = inImage.Height, kHeight = kernel.GetLength(1);
        Bitmap outImage = new(width, height);
        var inData = inImage.LockBits(new Rectangle(default, inImage.Size), System.Drawing.Imaging.ImageLockMode.ReadOnly, System.Drawing.Imaging.PixelFormat.Format24bppRgb);
        var outData = outImage.LockBits(new Rectangle(default, outImage.Size), System.Drawing.Imaging.ImageLockMode.ReadWrite, System.Drawing.Imaging.PixelFormat.Format24bppRgb);

        try
        {
            BgrColor* inPtr = (BgrColor*)inData.Scan0;
            BgrColor* outPtr = (BgrColor*)outData.Scan0;

            Parallel.For(0, width, x =>
            {
                //Console.WriteLine($"{x}/{width}");

                Parallel.For(0, height, y =>
                {
                    float r = 0, g = 0, b = 0;

                    for (int i = 0; i < kWidth; i++)
                    {
                        for (int j = 0; j < kHeight; j++)
                        {
                            int offsetX = x + i - kWidth / 2;
                            int offsetY = y + j - kHeight / 2;

                            offsetX = Math.Clamp(offsetX, 0, width - 1);
                            offsetY = Math.Clamp(offsetY, 0, height - 1);

                            BgrColor pixel = inPtr[offsetY * width + offsetX];
                            float weight = kernel[i, j];

                            r += pixel.r * weight;
                            g += pixel.g * weight;
                            b += pixel.b * weight;
                        }
                    }

                    outPtr[y * width + x].r = (byte)r;
                    outPtr[y * width + x].g = (byte)g;
                    outPtr[y * width + x].b = (byte)b;
                });
            });
        }
        finally
        {
            inImage.UnlockBits(inData);
            outImage.UnlockBits(outData);
        }

        return outImage;
    }
}
