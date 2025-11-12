---
--- Copyright (c) 2025, Benjamin Mordaunt
---

--  Test_RARC_Endian.adb
--  Simple unit tests for RARC.Endian endian conversion functions

with AUnit.Assertions; use AUnit.Assertions;
with System;

package body RARC.Endian.Test is
   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test RARC.Endian");
   end Name;

   procedure Run_Test (T : in out Test) is
      use type System.Bit_Order;

      Orig16 : constant Unsigned_16 := 16#1234#;
      Swap16 : constant Unsigned_16 := 16#3412#;
      Orig32 : constant Unsigned_32 := 16#12345678#;
      Swap32 : constant Unsigned_32 := 16#78563412#;
      Host_LE : constant Boolean := (System.Default_Bit_Order = System.Low_Order_First);
      Host_BE : constant Boolean := (System.Default_Bit_Order = System.High_Order_First);
   begin
      -- Test Swap_U16
      Assert (Swap_U16 (Orig16) = Swap16, "Swap_U16 did not match expected");
      Assert (Swap_U16 (Swap16) = Orig16, "Swap_U16 reversal failed");

      -- Test Swap_U32
      Assert (Swap_U32 (Orig32) = Swap32, "Swap_U32 did not match expected");
      Assert (Swap_U32 (Swap32) = Orig32, "Swap_U32 reversal failed");

      -- Test idempotency
      Assert (Swap_U16 (Swap_U16 (Orig16)) = Orig16, "Swap_U16 not idempotent");
      Assert (Swap_U32 (Swap_U32 (Orig32)) = Orig32, "Swap_U32 not idempotent");

      -- Test From_Big_Endian_U16
      if Host_LE then
            Assert (From_Big_Endian_U16 (Orig16) = Swap16, "From_Big_Endian_U16 on LE failed");
            Assert (From_Big_Endian_U16 (Swap16) = Orig16, "From_Big_Endian_U16 LE roundtrip failed");
      elsif Host_BE then
            Assert (From_Big_Endian_U16 (Orig16) = Orig16, "From_Big_Endian_U16 on BE failed");
            Assert (From_Big_Endian_U16 (Swap16) = Swap16, "From_Big_Endian_U16 BE roundtrip failed");
      end if;

      -- Test From_Big_Endian_U32
      if Host_LE then
            Assert (From_Big_Endian_U32 (Orig32) = Swap32, "From_Big_Endian_U32 on LE failed");
            Assert (From_Big_Endian_U32 (Swap32) = Orig32, "From_Big_Endian_U32 LE roundtrip failed");
      elsif Host_BE then
            Assert (From_Big_Endian_U32 (Orig32) = Orig32, "From_Big_Endian_U32 on BE failed");
            Assert (From_Big_Endian_U32 (Swap32) = Swap32, "From_Big_Endian_U32 BE roundtrip failed");
      end if;

      -- Test To_Big_Endian_U16
      if Host_LE then
            Assert (To_Big_Endian_U16 (Orig16) = Swap16, "To_Big_Endian_U16 on LE failed");
            Assert (To_Big_Endian_U16 (Swap16) = Orig16, "To_Big_Endian_U16 LE roundtrip failed");
      elsif Host_BE then
            Assert (To_Big_Endian_U16 (Orig16) = Orig16, "To_Big_Endian_U16 on BE failed");
            Assert (To_Big_Endian_U16 (Swap16) = Swap16, "To_Big_Endian_U16 BE roundtrip failed");
      end if;

      -- Test To_Big_Endian_U32
      if Host_LE then
            Assert (To_Big_Endian_U32 (Orig32) = Swap32, "To_Big_Endian_U32 on LE failed");
            Assert (To_Big_Endian_U32 (Swap32) = Orig32, "To_Big_Endian_U32 LE roundtrip failed");
      elsif Host_BE then
            Assert (To_Big_Endian_U32 (Orig32) = Orig32, "To_Big_Endian_U32 on BE failed");
            Assert (To_Big_Endian_U32 (Swap32) = Swap32, "To_Big_Endian_U32 BE roundtrip failed");
      end if;
   end Run_Test;
end RARC.Endian.Test;
