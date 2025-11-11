--  rarc-endian.adb
--  Implementation: Endian conversion utilities for RARC (big-endian) archives

with System; use System;

package body RARC.Endian is
    use Interfaces;

    function Swap_U16 (Value : Unsigned_16) return Unsigned_16 is
        -- Swaps byte order of 16-bit value
    begin
        return Interfaces.Shift_Right (Value, 8) or
        Interfaces.Shift_Left  (Value and 16#FF#, 8);
    end Swap_U16;

    function Swap_U32 (Value : Unsigned_32) return Unsigned_32 is
        -- Swaps byte order of 32-bit value
        B0, B1, B2, B3 : Unsigned_32;
    begin
        B0 := Interfaces.Shift_Right (Value, 24);
        B1 := Interfaces.Shift_Right (Value and 16#00FF0000#, 8);
        B2 := Interfaces.Shift_Left  (Value and 16#0000FF00#, 8);
        B3 := Interfaces.Shift_Left  (Value and 16#000000FF#, 24);
        return B0 or B1 or B2 or B3;
    end Swap_U32;

    function From_Big_Endian_U16 (Value : Unsigned_16) return Unsigned_16 is
    begin
        if System.Default_Bit_Order = System.Low_Order_First then
            return Swap_U16 (Value);
        else
            return Value;
        end if;
    end From_Big_Endian_U16;

    function From_Big_Endian_U32 (Value : Unsigned_32) return Unsigned_32 is
    begin
        if System.Default_Bit_Order = System.Low_Order_First then
            return Swap_U32 (Value);
        else
            return Value;
        end if;
    end From_Big_Endian_U32;

    function To_Big_Endian_U16 (Value : Unsigned_16) return Unsigned_16 is
    begin
        if System.Default_Bit_Order = System.Low_Order_First then
            return Swap_U16 (Value);
        else
            return Value;
        end if;
    end To_Big_Endian_U16;

    function To_Big_Endian_U32 (Value : Unsigned_32) return Unsigned_32 is
    begin
        if System.Default_Bit_Order = System.Low_Order_First then
            return Swap_U32 (Value);
        else
            return Value;
        end if;
    end To_Big_Endian_U32;

end RARC.Endian;
