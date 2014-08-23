-- Author: Anthony Spoerl
-- Team:   Grounded Dutchman

-- *** Comment these out if not running under MaRTE
with MaRTE_OS;
pragma Warnings (Off, MaRTE_OS);
-- ***

with Ada.Text_IO;
with Sound; use Sound;
with Dallee; use Dallee;

procedure sound_test is
   package Signal_IO is new
     Ada.Text_IO.Enumeration_IO (Horn_Signal);
   package Test_Unit_IO is new
     Ada.Text_IO.Integer_IO (Installed_Range);

   Test_Unit : Installed_Range := Installed_Range'First;

   procedure Horn_Test is
   begin
      for Signal in Horn_Signal loop
	 Sound_Horn (Unit   => Test_Unit,
	             Signal => Signal);
	 Ada.Text_IO.Put ("Sounding ");
	 Signal_IO.Put (Signal);
	 Ada.Text_IO.Put (" on unit ");
	 Test_Unit_IO.Put (Test_Unit);
	 Ada.Text_IO.New_Line;
	 Ada.Text_IO.Put_Line ("Press enter to test next signal.");
	 Ada.Text_IO.Skip_Line;

	 if Test_Unit = Installed_Range'Last then
	    Test_Unit := Installed_Range'First;
	 else
	    Test_Unit := Test_Unit + Installed_Range (1);
	 end if;
      end loop;
   end Horn_Test;

   procedure Bell_Test is
   begin
      for Unit in Installed_Range loop
	 Ada.Text_IO.Put ("Testing bell on unit ");
	 Test_Unit_IO.Put (Unit);
	 Ada.Text_IO.Put (" for 3 seconds. ");
	 Ada.Text_IO.Put ("Make sure throttle is below 4 notches and press Enter");
	 Ada.Text_IO.Skip_Line;

	 Bell_On (Unit);
	 delay 3.0;
	 Bell_Off (Unit);
      end loop;
   end Bell_Test;

   procedure Multiple_Sounds_Test is
   begin
      Ada.Text_IO.Put_Line ("Testing multiple simultaneous sounds.");

      -- Turn on all bells
      for Unit in Installed_Range loop
	 Bell_On (Unit);
      end loop;
      Ada.Text_IO.Put_Line ("Turning on bell on all units.");

      -- Sound horn on each unit
      Sound_Horn (Unit   => 1,
		  Signal => Start);
      Ada.Text_IO.Put_Line ("Sounding Start signal on Unit 1.");
      Sound_Horn (Unit   => 2,
		  Signal => Warning);
      Ada.Text_IO.Put_Line ("Sounding Warning signal on Unit 2.");
      Sound_Horn (Unit   => 3,
		  Signal => Stop);
      Ada.Text_IO.Put_Line ("Sounding Stop signal on Unit 3.");

      -- Turn all bells off
      for Unit in Installed_Range loop
	 Bell_Off (Unit);
      end loop;

   end Multiple_Sounds_Test;

begin
   Horn_Test;
   Bell_Test;
   Multiple_Sounds_Test;

   Ada.Text_IO.Put ("Test Complete.");
end sound_test;
