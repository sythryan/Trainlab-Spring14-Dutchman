with Ada.Text_IO;
with EngineerControl;
with Common_Units;
procedure Controller_Test is

-- Author: Abubakar
-- Team  : The Grounded Dutchman

   package Button_IO is
     new Ada.Text_IO.Enumeration_IO (Enum => EngineerControl.Button);
   package Two_Way_Toggle_IO is new
     Ada.Text_IO.Enumeration_IO (Enum => EngineerControl.Two_Way_Direction);
   package Three_Way_Toggle_IO is new
     Ada.Text_IO.Enumeration_IO (Enum => EngineerControl.Three_Way_Direction);
   package Controller_ID_IO is new
     Ada.Text_IO.Enumeration_IO (Enum => EngineerControl.Controller_ID);
   package Percent_IO is new Ada.Text_IO.Integer_IO (Common_Units.Percent);

   Red_Button       : EngineerControl.Button;
   Black_Button     : EngineerControl.Button;
   Two_Way_Toggle   : EngineerControl.Two_Way_Direction;
   Three_Way_Toggle : EngineerControl.Three_Way_Direction;
   Knob             : Common_Units.Percent;

begin

   loop
      Ada.Text_IO.Put_Line
        ("Press enter to see the status of the hand controllers");
      Ada.Text_IO.Skip_Line;
      -- Display the controllers_status'
      for Index in EngineerControl.A .. EngineerControl.C loop
	 Ada.Text_IO.Put ("Hand Controller ");
	 Controller_ID_IO.Put (Index);
	 Ada.Text_IO.Put_Line (": ");

	 EngineerControl.Get (Controller       => Index,
		       Red_Button       => Red_Button,
		       Black_Button     => Black_Button,
		       Two_Way_Toggle   => Two_Way_Toggle,
		       Three_Way_Toggle => Three_Way_Toggle);
	 Ada.Text_IO.Put ("Black Button: ");
	 Button_IO.Put (Black_Button);
	 Ada.Text_IO.New_Line;
	 Ada.Text_IO.Put ("Red Button: ");
	 Button_IO.Put (Red_Button);
	 Ada.Text_IO.New_Line;
	 Ada.Text_IO.Put ("Two Way Toggle: ");
	 Two_Way_Toggle_IO.Put (Two_Way_Toggle);
	 Ada.Text_IO.New_Line;
	 Ada.Text_IO.Put ("Three Way Toggle: ");
	 Three_Way_Toggle_IO.Put (Three_Way_Toggle);
	 Ada.Text_IO.New_Line;

	 EngineerControl.Get (Controller => Index,
		              Knob       => Knob);
	 Ada.Text_IO.Put ("Knob percent: ");
	 Percent_IO.Put (Knob);
	 Ada.Text_IO.New_Line;
	 Ada.Text_IO.Put_Line ("---------------------------------------");
      end loop;
   end loop;
end Controller_Test;
