using System.Runtime.InteropServices;

namespace PPR_ImageBlur;

[StructLayout(LayoutKind.Explicit)]
internal struct BgrColor
{
    [FieldOffset(2)]
    public byte r;

    [FieldOffset(1)]
    public byte g;

    [FieldOffset(0)]
    public byte b;
}
