using System.Diagnostics;
using System.Drawing;
using System.Drawing.Imaging;
using PPR_ImageBlur;

namespace PPRImageBlur;

public class Program
{
    public static void Main()
    {
        Directory.CreateDirectory("./out");
        string[] inputImages = Directory.GetFiles(".", "*.jpg");

        foreach (var inputImagePath in inputImages)
        {
            Console.WriteLine("Using image: " + inputImagePath);
            Bitmap inputImage = new(inputImagePath);
            var sw = new Stopwatch();
            string outputImagePath = "./out/" + Path.GetFileNameWithoutExtension(inputImagePath) + ".{0}." + Path.GetExtension(inputImagePath);

            sw.Start();
            using Bitmap blurredImage1 = SingleThreadedBlur.ApplyGaussianBlur(inputImage, 10); // 10% blur
            sw.Stop();
            Console.WriteLine("Singlethreaded Time: " + sw.Elapsed);
            blurredImage1.Save(String.Format(outputImagePath, "out_single"), ImageFormat.Jpeg);

            sw.Restart();
            using Bitmap blurredImage2 = MultiThreadedBlur.ApplyGaussianBlur_ParallelWidth(inputImage, 10);
            blurredImage2.Save(String.Format(outputImagePath, "out_multi_width"), ImageFormat.Jpeg);
            sw.Stop();
            Console.WriteLine("Multithreaded (width) Time: " + sw.Elapsed);

            sw.Restart();
            using Bitmap blurredImage3 = MultiThreadedBlur.ApplyGaussianBlur_ParallelHeight(inputImage, 10);
            blurredImage3.Save(String.Format(outputImagePath, "out_multi_height"), ImageFormat.Jpeg);
            sw.Stop();
            Console.WriteLine("Multithreaded (height) Time: " + sw.Elapsed);

            sw.Restart();
            using Bitmap blurredImage4 = MultiThreadedBlur.ApplyGaussianBlur_ParallelDual(inputImage, 10);
            blurredImage3.Save(String.Format(outputImagePath, "out_multi_dual"), ImageFormat.Jpeg);
            sw.Stop();
            Console.WriteLine("Multithreaded (dual) Time: " + sw.Elapsed);

            Console.WriteLine();
        }
    }
}
