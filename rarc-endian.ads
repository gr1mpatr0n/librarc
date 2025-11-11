--  rarc-endian.ads
--  Byte order utilities for RARC format (big-endian archive, portable handling)

with Interfaces;

package RARC.Endian is
    use Interfaces;

    -- Swap endianness of 16-bit unsigned values
    function Swap_U16 (Value : Unsigned_16) return Unsigned_16;

    -- Swap endianness of 32-bit unsigned values
    function Swap_U32 (Value : Unsigned_32) return Unsigned_32;

    -- Convert value read from archive (big-endian) to host order
    function From_Big_Endian_U16 (Value : Unsigned_16) return Unsigned_16;
    function From_Big_Endian_U32 (Value : Unsigned_32) return Unsigned_32;

    -- Convert value in host order to archive order (big-endian)
    function To_Big_Endian_U16 (Value : Unsigned_16) return Unsigned_16;
    function To_Big_Endian_U32 (Value : Unsigned_32) return Unsigned_32;

end RARC.Endian;
