--  Test_RARC_Endian.adb
--  Simple unit tests for RARC.Endian endian conversion functions

with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Exceptions;           use Ada.Exceptions;
with Interfaces;
with RARC.Endian;
with System;

procedure Test_RARC_Endian is

    use Interfaces;
    use type System.Bit_Order;

    package E renames RARC.Endian;

    procedure Assert (Cond : Boolean; Msg : String := "") is
    begin
        if not Cond then
            Put_Line ("Test failed: " & Msg);
            raise Program_Error with Msg;
        end if;
    end Assert;

begin
    Put ("Testing RARC.Endian... ");

    -- Test data values (hex values are big-endian representations)
    declare
        Orig16 : constant Unsigned_16 := 16#1234#;
        Swap16 : constant Unsigned_16 := 16#3412#;
        Orig32 : constant Unsigned_32 := 16#12345678#;
        Swap32 : constant Unsigned_32 := 16#78563412#;
        Host_LE : constant Boolean := (System.Default_Bit_Order = System.Low_Order_First);
        Host_BE : constant Boolean := (System.Default_Bit_Order = System.High_Order_First);
    begin
        -- Test Swap_U16
        Assert (E.Swap_U16 (Orig16) = Swap16, "Swap_U16 did not match expected");
        Assert (E.Swap_U16 (Swap16) = Orig16, "Swap_U16 reversal failed");

        -- Test Swap_U32
        Assert (E.Swap_U32 (Orig32) = Swap32, "Swap_U32 did not match expected");
        Assert (E.Swap_U32 (Swap32) = Orig32, "Swap_U32 reversal failed");

        -- Test idempotency
        Assert (E.Swap_U16 (E.Swap_U16 (Orig16)) = Orig16, "Swap_U16 not idempotent");
        Assert (E.Swap_U32 (E.Swap_U32 (Orig32)) = Orig32, "Swap_U32 not idempotent");

        -- Test From_Big_Endian_U16
        if Host_LE then
            Assert (E.From_Big_Endian_U16 (Orig16) = Swap16, "From_Big_Endian_U16 on LE failed");
            Assert (E.From_Big_Endian_U16 (Swap16) = Orig16, "From_Big_Endian_U16 LE roundtrip failed");
        elsif Host_BE then
            Assert (E.From_Big_Endian_U16 (Orig16) = Orig16, "From_Big_Endian_U16 on BE failed");
            Assert (E.From_Big_Endian_U16 (Swap16) = Swap16, "From_Big_Endian_U16 BE roundtrip failed");
        end if;

        -- Test From_Big_Endian_U32
        if Host_LE then
            Assert (E.From_Big_Endian_U32 (Orig32) = Swap32, "From_Big_Endian_U32 on LE failed");
            Assert (E.From_Big_Endian_U32 (Swap32) = Orig32, "From_Big_Endian_U32 LE roundtrip failed");
        elsif Host_BE then
            Assert (E.From_Big_Endian_U32 (Orig32) = Orig32, "From_Big_Endian_U32 on BE failed");
            Assert (E.From_Big_Endian_U32 (Swap32) = Swap32, "From_Big_Endian_U32 BE roundtrip failed");
        end if;

        -- Test To_Big_Endian_U16
        if Host_LE then
            Assert (E.To_Big_Endian_U16 (Orig16) = Swap16, "To_Big_Endian_U16 on LE failed");
            Assert (E.To_Big_Endian_U16 (Swap16) = Orig16, "To_Big_Endian_U16 LE roundtrip failed");
        elsif Host_BE then
            Assert (E.To_Big_Endian_U16 (Orig16) = Orig16, "To_Big_Endian_U16 on BE failed");
            Assert (E.To_Big_Endian_U16 (Swap16) = Swap16, "To_Big_Endian_U16 BE roundtrip failed");
        end if;

        -- Test To_Big_Endian_U32
        if Host_LE then
            Assert (E.To_Big_Endian_U32 (Orig32) = Swap32, "To_Big_Endian_U32 on LE failed");
            Assert (E.To_Big_Endian_U32 (Swap32) = Orig32, "To_Big_Endian_U32 LE roundtrip failed");
        elsif Host_BE then
            Assert (E.To_Big_Endian_U32 (Orig32) = Orig32, "To_Big_Endian_U32 on BE failed");
            Assert (E.To_Big_Endian_U32 (Swap32) = Swap32, "To_Big_Endian_U32 BE roundtrip failed");
        end if;

        Put_Line ("PASS");

    end;

exception
    when E : others =>
        Put_Line ("Test exception: " & Exception_Message (E));
end Test_RARC_Endian;
