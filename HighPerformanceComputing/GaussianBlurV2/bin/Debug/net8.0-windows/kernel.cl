
__kernel void blur(
	__global uchar4 const * const input,
	__private const int cols,
	__global float const * const conv_kernel,
	__private const int conv_kernel_size,
	__global uchar4 * const output)
{
	__private const int2 pos = {get_global_id(0), get_global_id(1)}; // total pixel x/y coords
	__private int local_id = max(get_local_id(0), get_local_id(1));
	__private int local_size = max(get_local_size(0), get_local_size(1));
	__local uchar4 local_column[1024]; // specific to current column
	__private float4 sum = {0.0f,0.0f,0.0f,0.0f};

	// load pixel to be blurred into local memory
	local_column[local_id] = input[pos.y * cols + pos.x];
	barrier(CLK_LOCAL_MEM_FENCE);

	// for each value in conv_kernel
	for (int i = 0; i < conv_kernel_size; ++i)
	{
		// pixel to process
		int px = local_id + (i - conv_kernel_size / 2);
		px = clamp(px, 0, local_size - 1);

		sum += convert_float4(local_column[px]) * conv_kernel[i];
	}

	output[pos.y * cols + pos.x] = convert_uchar4_sat(sum);
}
