-- Author: Anthony Spoerl
-- Team:   Grounded Dutchman

--  -- *** Comment these out if not running under MaRTE
--  with MaRTE_OS;
--  pragma Warnings (Off, MaRTE_OS);
--  -- ***

with Ada.Text_IO;
with Cabs;

procedure cabs_test is
   package Percentage_IO is new
     Ada.Text_IO.Integer_IO (Cabs.Percent);
   package Cab_IO is new
     Ada.Text_IO.Integer_IO (Cabs.Control_Cab_ID);

   procedure Set_And_Check_Output (Unit  : in Cabs.Control_Cab_ID;
				   Value : in Cabs.Percent) is

      Set_Percent : Cabs.Percent := Value;
   begin
      Cabs.Set (Cab   => Unit,
		Value => Set_Percent);
      Ada.Text_IO.Put ("Setting cab #");
      Cab_IO.Put (Unit);
      Ada.Text_IO.Put (" to ");
      Percentage_IO.Put (Set_Percent);
      Ada.Text_IO.Put_Line ("%");

      Cabs.Get (Cab   => Unit,
		Value => Set_Percent);
      Ada.Text_IO.Put ("Cab #");
      Cab_IO.Put (Unit);
      Ada.Text_IO.Put (" is set to ");
      Percentage_IO.Put (Set_Percent);
      Ada.Text_IO.Put ("%. Check output voltage. Enter to continue testing");
      Ada.Text_IO.Skip_Line;
   end Set_And_Check_Output;

   procedure Set_And_Check_Limit (Unit  : in Cabs.Control_Cab_ID;
				  Limit : in Cabs.Percent) is
   begin
      Ada.Text_IO.Put ("Setting limit on cab #");
      Cab_IO.Put (Unit);
      Ada.Text_IO.Put (" to ");
      Percentage_IO.Put (Limit);
      Ada.Text_IO.Put_Line (".");
      Cabs.Set_Limit (Cab  => Unit,
		      Value => Limit);

      Set_And_Check_Output (Unit  => Unit,
			    Value => 25);
      Set_And_Check_Output (Unit  => Unit,
			    Value => 50);
      Set_And_Check_Output (Unit  => Unit,
			    Value => 75);
      Set_And_Check_Output (Unit  => Unit,
			    Value => 100);
   end Set_And_Check_Limit;


   Cab_Value : Cabs.Percent;
begin
   -- Test initialization code
   Ada.Text_IO.Put ("Check each cab is outputting ");
   Percentage_IO.Put (Cabs.Percent'First);
   Ada.Text_IO.Put_Line (" percent of max voltage. Enter to continue testing.");
   Ada.Text_IO.Skip_Line;

   -- Test set/get on each cab
   for Unit in Cabs.Control_Cab_ID loop
      -- Initialization should set limits to 0, we need to override this
      Cabs.Set_Limit (Cab   => Unit,
		      Value => 100);

      Set_And_Check_Output (Unit  => Unit,
			    Value => 100);
      Set_And_Check_Output (Unit  => Unit,
			    Value => 50);
   end loop;
   Ada.Text_IO.Put_Line ("End set/get tests.");

   -- Test set/get limit
   Set_And_Check_Limit (Unit  => 1,
			Limit => 0);
   Set_And_Check_Limit (Unit  => 1,
			Limit => 50);

   -- Test setting limit higher than current value
   Cabs.Set_Limit (Cab   => 1,
		   Value => 50);
   Cabs.Set (Cab   => 1,
	     Value => 50);
   Ada.Text_IO.Put_Line ("Unit 1 limit and value at 50%");
   Cabs.Set_Limit (Cab   => 1,
		   Value => 100);
   Ada.Text_IO.Put_Line ("Setting Unit 1 limit to 100%");
   Ada.Text_IO.Put ("Unit 1 output at ");
   Cabs.Get (Cab => 1,
	     Value => Cab_Value);
   Percentage_IO.Put (Cab_Value);
   Ada.Text_IO.Put_Line ("%");
   Ada.Text_IO.Skip_Line;

   Ada.Text_IO.Put_Line ("Testing complete.");

end cabs_test;
