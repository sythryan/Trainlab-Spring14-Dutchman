with Engineers; use Engineers;
with Ada.Text_IO;
with EngineerControl; use EngineerControl;
with Train_Types;
with Trains;
with Layout;
with Cabs;

-- *** Comment these out if not running under MaRTE
with MaRTE_OS;
pragma Warnings (Off, MaRTE_OS);
with Engineer_Types; use Engineer_Types;
-- ***

procedure Engineer_Test is
   -- Keep every index in the same range, shouldn't have more of anything than
   -- we have Engineers to use them anyway.
   type Engineer_Range is new Integer range 0 .. Engineer_ID'Size;
   package Index_IO is new Ada.Text_IO.Integer_IO (Engineer_Range);

   type Engineer_Array is array (Engineer_Range) of Engineer_ID;
   type Train_Array is array (Engineer_Range) of Train_Types.Train_ID;
   type Controller_Array is array (Engineer_Range) of Controller_ID;

   All_Engineers   : constant Engineer_Array   := (1, 2, 3);
   All_Trains      : constant Train_Array      := (1, 2, 3);
   All_Controllers : constant Controller_Array := (A, B, C);

   -----------------------------------------------------------------------------
   function Test_1 return Boolean is
      -- Check that all engineers are disabled by default on initialization
      -- preconditions : Must be first test to run
      Passed : Boolean := True;
   begin
      for Index in Engineer_Range loop
         if Check_Status (Engineer => All_Engineers (Index)) /= Disabled then
            Ada.Text_IO.Put ("Failed to enable Engineer ");
            Index_IO.Put (Index);
            Ada.Text_IO.New_Line;
	    Passed := False;
	 end if;
      end loop;
      return Passed;
   end Test_1;

   -----------------------------------------------------------------------------
   function Test_2 return Boolean is
      -- Check engineers are able to be enabled and initialized at Novice level
      -- preconditions : Engineers must be currently in the disabled state
      Passed : Boolean := True;
   begin
      for Index in Engineer_Range loop
	 Enable (Engineer   => All_Engineers (Index),
	  Controller => All_Controllers (Index),
	  Train      => All_Trains (Index));
         if Check_Status (Engineer => All_Engineers (Index)) /= Enabled then
            Ada.Text_IO.Put_Line ("Failed to enable Engineer ");
            Index_IO.Put (Index);
            Ada.Text_IO.New_Line;
	    Passed := False;
	 end if;
         if Check_Level (Engineer => All_Engineers (Index)) /= Novice then
            Ada.Text_IO.Put_Line ("Engineer ");
            Index_IO.Put (Index);
            Ada.Text_IO.Put_Line (" was not initialized to Novice.");
	    Passed := False;
	 end if;
      end loop;
      return Passed;
   end Test_2;

   function Test_3 return Boolean is
      -- Check changing skill level
      -- preconditions: Engineers are enabled and skills are Novice
      Passed : Boolean := True;
   begin
      for Index in Engineer_Range loop
	 Ada.Text_IO.Put_Line ("Changing level");
	 Change_Level (Engineer => All_Engineers (Index));
	 Ada.Text_IO.Put_Line ("Level Changed.");
         if Check_Level (Engineer => All_Engineers (Index)) /= Expert then
            Ada.Text_IO.Put_Line ("Engineer ");
            Index_IO.Put (Index);
            Ada.Text_IO.Put_Line (" was not set to Expert.");
            Passed := False;
         end if;
      end loop;

      return Passed;
   end Test_3;


   function Test_4 return Boolean is
      -- Check setting disabled
      -- preconditions: Engineers are enabled
      Passed : Boolean := True;
   begin
      for Index in Engineer_Range loop
	 Ada.Text_IO.Put_Line ("Disabling Engineer");
	 Disable (Engineer => All_Engineers (Index));
         Ada.Text_IO.Put_Line ("Engineer Disabled");
         delay 0.1;
         if Check_Status (Engineer => All_Engineers (Index)) /= Disabled then
            Ada.Text_IO.Put_Line ("Engineer ");
            Index_IO.Put (Index);
            Ada.Text_IO.Put_Line (" was not Disabled.");
            Passed := False;
         end if;
      end loop;

      return Passed;
   end Test_4;



   -----------------------------------------------------------------------------

begin
   for Index in Train_Types.Train_ID'Range loop
      Trains.Initialize_Train (Train     => Index,
			       Loco      => 1,
			       Caboose   => 1,
			       Direction => Layout.Normal);
   end loop;

   Ada.Text_IO.Put_Line ("************Engineer Testing Begin *************");
   -- Test 1
   Ada.Text_IO.Put_Line ("Running Test 1.");
   if not Test_1 then
      Ada.Text_IO.Put_Line ("*** Test 1 Failed ***");
   else
      Ada.Text_IO.Put_Line ("Test 1 Complete.");
   end if;
   Ada.Text_IO.Skip_Line;

   -- Test 2
   Ada.Text_IO.Put_Line ("Running Test 2.");
   if not Test_2 then
      Ada.Text_IO.Put_Line ("*** Test 2 Failed ***");
   else
      Ada.Text_IO.Put_Line ("Test 2 Complete.");
   end if;
   Ada.Text_IO.Skip_Line;

   -- Test 3
   Ada.Text_IO.Put_Line ("Running Test 3.");
   if not Test_3 then
      Ada.Text_IO.Put_Line ("*** Test 3 Failed ***");
   else
      Ada.Text_IO.Put_Line ("Test 3 Complete.");
   end if;
   Ada.Text_IO.Skip_Line;

   -- Test 4
   Ada.Text_IO.Put_Line ("Running Test 4.");
   if not Test_4 then
      Ada.Text_IO.Put_Line ("*** Test 4 Failed ***");
   else
      Ada.Text_IO.Put_Line ("Test 4 Complete.");
   end if;

   Ada.Text_IO.Put_Line ("*** End Engineer Testing ***");

end Engineer_Test;
