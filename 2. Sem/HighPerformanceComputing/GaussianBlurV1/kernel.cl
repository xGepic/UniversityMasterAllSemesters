/*
* a kernel that blurs a single pixel
*/
__kernel void blur(
	__global uchar4 const * const input,
	__private const int cols,
	__private const int rows,
	__global float const * const conv_kernel,
	__private const int conv_kernel_cols,
	__global uchar4 * const output)
{
	// determine current position
	const int2 pos = {get_global_id(0), get_global_id(1)};
	float4 sum = {0.0f,0.0f,0.0f,0.0f};

	// iterate over rows
	for (int r = 0; r < conv_kernel_cols; ++r)
	{
		int y = pos.y + (r - conv_kernel_cols / 2);

		y = clamp(y, 0, rows - 1);
		
		// iterate over cols
		for (int c = 0; c < conv_kernel_cols; ++c)
		{
			int x = pos.x + (c - conv_kernel_cols / 2);
			x = clamp(x, 0, cols - 1);

			// sum values up
			sum += convert_float(input[(y * cols) + x]) * conv_kernel[r * conv_kernel_cols + c];
		}
	}

	// write to output buffer
	output[pos.y * cols + pos.x] = convert_uchar4_sat(sum);
}
