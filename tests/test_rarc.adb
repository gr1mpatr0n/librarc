---
--- Copyright (c) 2025, Benjamin Mordaunt
---

with AUnit.Reporter.Text;
with AUnit.Run;
with RARC_Suite; use RARC_Suite;

procedure Test_RARC is
   procedure Runner is new AUnit.Run.Test_Runner (Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Runner (Reporter);
end Test_RARC;
