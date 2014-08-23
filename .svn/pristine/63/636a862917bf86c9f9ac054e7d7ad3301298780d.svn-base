with Ada.Text_IO;

package body Turnout_Test_Callbacks is
   package Turnout_IO is new Ada.Text_IO.Integer_IO (Layout.Turnout_ID);
   package Direction_IO is new Ada.Text_IO.Enumeration_IO (Layout.Turn_Choice);
   package Train_IO is new Ada.Text_IO.Integer_IO (Train_Types.Request_ID);

   procedure Turnout_Failure (Requestor : in Train_Types.Request_ID;
			      Turnout   : in Layout.Turnout_ID) is
   begin
      Ada.Text_IO.Put_Line ("Failure on turnout ");
      Turnout_IO.Put (Turnout);
      Ada.Text_IO.Put (". Called by train ");
      Train_IO.Put (Requestor);
      Ada.Text_IO.New_Line;
   end Turnout_Failure;

   procedure Turnout_Recovered (Turnout   : in Layout.Turnout_ID) is
   begin
      Ada.Text_IO.Put_Line ("Turnout ");
      Turnout_IO.Put (Turnout);
      Ada.Text_IO.Put_Line (" has recovered from a failure");
   end Turnout_Recovered;

   procedure Turnout_Changed (Turnout   : in Layout.Turnout_ID;
			      Direction : in Layout.Turn_Choice;
			      Moving    : in Boolean) is
   begin
      if Moving then
	 Ada.Text_IO.Put_Line ("Turnout ");
	 Turnout_IO.Put (Turnout);
	 Ada.Text_IO.Put (" is moving to the ");
	 Direction_IO.Put (Direction);
	 Ada.Text_IO.New_Line;
      else
	 Ada.Text_IO.Put ("Turnout ");
	 Turnout_IO.Put (Turnout);
	 Ada.Text_IO.Put (" is in the ");
	 Direction_IO.Put (Direction);
	 Ada.Text_IO.Put_Line (" position");
      end if;
   end Turnout_Changed;
end Turnout_Test_Callbacks;
