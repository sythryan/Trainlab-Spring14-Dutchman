-- Author: Anthony Spoerl
-- Team:   Grounded Dutchman

--  -- *** Comment these out if not running under MaRTE
--  with MaRTE_OS;
--  pragma Warnings (Off, MaRTE_OS);
--  -- ***

with Ada.Text_IO;
with Turnouts;
with Layout; use Layout;
with Turnout_Test_Callbacks;
with Trains; use Trains;

procedure turnouts_test is
   package Turnout_IO is new Ada.Text_IO.Integer_IO (Layout.Turnout_ID);
   package Direction_IO is new Ada.Text_IO.Enumeration_IO (Layout.Turn_Choice);

begin

   -- Setup callbacks to send info to terminal
   Turnouts.Set_Failure_Callback
     (Turnout_Test_Callbacks.Turnout_Failure'Access);
   Turnouts.Set_Recovery_Callback
     (Turnout_Test_Callbacks.Turnout_Recovered'Access);
   Turnouts.Set_Change_Callback
     (Turnout_Test_Callbacks.Turnout_Changed'Access);

   Ada.Text_IO.Put_Line ("Testing successful turnout changes.");
   for Turnout in Layout.Turnout_ID (1) .. Layout.Turnout_ID (5) loop
      for Turn_Choice in Layout.Turn_Choice loop
         Ada.Text_IO.Put ("*** Setting turnout ");
         Turnout_IO.Put (Turnout);
         Ada.Text_IO.Put (" to ");
         Direction_IO.Put (Turn_Choice);

         Turnouts.Set (Requestor => Train_Types.Request_ID (0),
                       Turnout   => Turnout,
                       Direction => Turn_Choice);

         Ada.Text_IO.Skip_Line;
      end loop;
   end loop;

   Ada.Text_IO.Put_Line
     ("*** Testing unsuccessful turnout change and recovery on Turnout 1.");
   Ada.Text_IO.Put_Line ("*** Disconnect sense wire on Turnout 1.");
   Ada.Text_IO.Put_Line ("*** Press enter to test failure.");
   Ada.Text_IO.Skip_Line;

   Turnouts.Set (Requestor => 0,
		 Turnout   => 1,
		 Direction => Left);

   Ada.Text_IO.Skip_Line;
   Ada.Text_IO.Put_Line ("*** Reconnect sense wire on Turnout 1.");
   Ada.Text_IO.Skip_Line;

   Turnouts.Shut_Down;
   Ada.Text_IO.Put_Line
     ("*** After 30 seconds all turnouts should be set to left");
end turnouts_test;
