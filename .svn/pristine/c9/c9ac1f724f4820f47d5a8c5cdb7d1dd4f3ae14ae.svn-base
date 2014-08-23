-- Author: Anthony Spoerl
-- Date:   3/7/2014

with Command; use Command;
with Ada.Text_IO;
with Trains;
with Layout; use Layout; -- For /= operator
with Engineers;
with Ada.Integer_Text_IO;
with Ada.IO_Exceptions;

procedure command_test is
   package Command_Type_IO is new
     Ada.Text_IO.Enumeration_IO (Command.Command_Type);
   package Train_IO is new Ada.Text_IO.Integer_IO (Train_Types.Train_ID);
   package Turnout_IO is new Ada.Text_IO.Integer_IO (Layout.Turnout_ID);
   package Block_IO is new Ada.Text_IO.Integer_IO (Layout.Block_ID);
   package Engineer_IO is new Ada.Text_IO.Integer_IO (Engineers.Engineer_ID);

   Returned_Command : Command.Command_Rec;
   Expected_Type    : Command.Command_Type;
   Test_Number      : Integer := 0;

   -- Used for checking discriminates. Are all these vars necessary?
   Train            : Train_Types.Train_ID;
   Turnout          : Layout.Turnout_ID;
   Block            : Layout.Block_ID;
   Engineer         : Engineers.Engineer_ID;
begin
   Ada.Text_IO.Put_Line ("Please redirect test text file to the command test "&
     "  i.e. command_test < Command-Tests.txt ");
   loop
      -- Get Command from file (Should be running file as stdin
      Command.Get (Returned_Command);
      -- Read in expected command type and create a Command Rec
      Command_Type_IO.Get (Item => Expected_Type);

      Test_Number := Test_Number + 1;
      Ada.Text_IO.Put ("Test ");
      Ada.Integer_Text_IO.Put (Test_Number);
      Ada.Text_IO.Put (": ");

      if Returned_Command.Which /= Expected_Type then
	 Ada.Text_IO.Put_Line
	   ("*** ERROR: TYPES DO NOT MATCH EXPECTED OUTCOME ***");
      else
	 case Returned_Command.Which is
	    when Stop_All | Restart | Quit | Error =>
	       Ada.Text_IO.Put_Line ("SUCCESS");
	    when Stop | Go =>
	       Train_IO.Get (Item => Train);
	       if Returned_Command.Train /= Train then
		  Ada.Text_IO.Put_Line
		    ("*** ERROR: TRAIN IDS DO NOT MATCH ***");
	       else
		  Ada.Text_IO.Put_Line ("SUCCESS");
	       end if;
	    when Left | Right =>
	       Turnout_IO.Get (Item => Turnout);
	       if Returned_Command.Turnout /= Turnout then
		  Ada.Text_IO.Put_Line
		    ("*** ERROR: TURNOUT IDS DO NOT MATCH ***");
	       else
		  Ada.Text_IO.Put_Line ("SUCCESS");
	       end if;
	    when Free =>
	       Block_IO.Get (Item => Block);
	       if Returned_Command.Block /= Block then
		  Ada.Text_IO.Put_Line
		    ("*** ERROR: BLOCK IDS DO NOT MATCH ***");
	       else
		  Ada.Text_IO.Put_Line ("SUCCESS");
	       end if;
	    when Skill =>
	       Engineer_IO.Get (Item => Engineer);
	       if Returned_Command.Engineer /= Engineer then
		  Ada.Text_IO.Put_Line
		    ("*** ERROR: ENGINEER IDS DO NOT MATCH ***");
	       else
		  Ada.Text_IO.Put_Line ("SUCCESS");
	       end if;
	 end case;
      end if;
   end loop;

exception
   when Ada.IO_Exceptions.End_Error =>
      Ada.Text_IO.Put ("Testing Complete");

end command_test;
