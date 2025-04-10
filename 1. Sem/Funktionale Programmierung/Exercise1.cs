using System.Diagnostics;

namespace Exercise1;

public class Exercise1Solution
{
    public static int ImperativeSumOfOddNumbers(List<int> myList)
    {
        int result = 0;
        foreach (int num in myList)
        {
            if (num % 2 != 0)
            {
                result += num;
            }
        }
        return result;
    }
    public static int FunctionalSumOfOddNumbers(List<int> myList)
    {
        return myList.Where(num => num % 2 != 0).Sum();
    }
    public static double ImperativeAverageOfOddNumbers(List<int> myList)
    {
        int count = 0;
        double sum = 0;
        foreach (int num in myList)
        {
            if (num % 2 != 0)
            {
                sum += num;
                count++;
            }
        }
        return sum / count;
    }
    public static double FunctionalAverageOfOddNumbers(List<int> myList)
    {
        return myList.Where(num => num % 2 != 0).Average();
    }
    /// <summary>
    /// How does the code of ImperativeSumOfOddNumbers and FunctionalSumOfOddNumberscompare in terms of succinctness?
    /// The functional version is much shorter in this case. A small amount of code could be reused.
    /// 
    /// Analyze Exercise 1 and 2 in terms of runtime performance.
    /// Output:
    /// Elapsed time of ImperativeSumOfOddNumbers: 0 milliseconds
    /// Elapsed time of FunctionalSumOfOddNumbers: 1 milliseconds
    /// </summary>
    public static void Main()
    {
        List<int> myList = new() { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
        Stopwatch stopwatch = new();
        //Analyze Exercise 1 in terms of runtime performance. START
        stopwatch.Start();
        int result1 = ImperativeSumOfOddNumbers(myList);
        stopwatch.Stop();
        long elapsedMilliseconds = stopwatch.ElapsedMilliseconds;
        Console.WriteLine("Elapsed time of ImperativeSumOfOddNumbers: " + elapsedMilliseconds + " milliseconds");
        //END
        //Analyze Exercise 1 in terms of runtime performance. START
        stopwatch.Start();
        int result2 = FunctionalSumOfOddNumbers(myList);
        stopwatch.Stop();
        long elapsedMilliseconds2 = stopwatch.ElapsedMilliseconds;
        Console.WriteLine("Elapsed time of FunctionalSumOfOddNumbers: " + elapsedMilliseconds2 + " milliseconds");
        //END
        double result3 = ImperativeAverageOfOddNumbers(myList);
        double result4 = FunctionalAverageOfOddNumbers(myList);
        Console.WriteLine($"ImperativeSumOfOddNumbers Solution: {result1}");
        Console.WriteLine($"FunctionalSumOfOddNumbers Solution: {result2}");
        Console.WriteLine($"ImperativeAverageOfOddNumbers Solution: {result3}");
        Console.WriteLine($"FunctionalAverageOfOddNumbers Solution: {result4}");
    }
}
