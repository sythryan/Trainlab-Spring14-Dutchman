-- Author : Anthony
-- Team   : The Grounded Dutchman
-- Date:    3/11/2014

--  -- *** Comment these out if not running under MaRTE
with MaRTE_OS;
pragma Warnings (Off, MaRTE_OS);
--  -- ***

with Motors; use Motors;
with Layout;
with Ada.Text_IO;

procedure Motors_Test is
   package Turnout_IO is new Ada.Text_IO.Integer_IO (Layout.Turnout_ID);
   package Direction_IO is new Ada.Text_IO.Enumeration_IO (Layout.Turn_Choice);

   type Turnout_Array is array (Layout.Turnout_ID) of Layout.Turnout_ID;
   Test_Turnouts : constant Turnout_Array :=
                     (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
                      16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26);


   Test_Count : Integer := 0;
begin
   for Turnout in Test_Turnouts'Range loop
      for Direction in Layout.Turn_Choice loop
	 -- Set a turnout and confirm that it is in the correct position
	 Set (Motor     => Test_Turnouts (Turnout),
              Direction => Direction);
	 -- Delay until set
	 loop
            delay 1.0;
            --Ada.Text_IO.Put (Boolean'Image (In_Position (Motor =>
            --                                       Test_Turnouts (Turnout))));
	    exit when In_Position (Test_Turnouts (Turnout));
	    Turnout_IO.Put (Test_Turnouts (Turnout));
	    Ada.Text_IO.Put_Line (" not in position yet.");

	    Test_Count := Test_Count + 1;
	    --exit when Test_Count > 6;
	 end loop;
	 --exit when Test_Count > 6;
	 Ada.Text_IO.Put ("Check turnout #");
	 Turnout_IO.Put (Test_Turnouts (Turnout));
	 Ada.Text_IO.Put (" is in position ");
         Direction_IO.Put (Direction);
         Ada.Text_IO.Put_Line ("");
	 Ada.Text_IO.Put_Line ("Press Enter to continue testing.");
	 Ada.Text_IO.Skip_Line;
      end loop;
   end loop;
   Ada.Text_IO.Put ("Testing complete");
end Motors_Test;
