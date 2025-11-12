---
--- Copyright (c) 2025, Benjamin Mordaunt
---

with AUnit.Simple_Test_Cases; use AUnit.Simple_Test_Cases;
with RARC.Endian.Test;

package body RARC_Suite is
   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test (Test_Case_Access'(new RARC.Endian.Test.Test));
      return Ret;
   end Suite;

end RARC_Suite;
