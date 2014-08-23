-- *** Comment these out if not running under MaRTE
--  with MaRTE_OS;
--  pragma Warnings (Off, MaRTE_OS);
-- ***
with EngineerControl; use EngineerControl;
with Ada.Integer_Text_IO;
with Ada.Text_IO;

-- Author : Anthony
-- Team   : The Grounded Dutchman

procedure Button_Press_Count is
   Count                : Integer := 0;
   Red_Button_Pressed   : Boolean := False;
   Black_Button_Pressed : Boolean := False;

   Controller       : constant Controller_ID := B;
   Red_Button       : Button;
   Black_Button     : Button;
   Two_Way_Toggle   : Two_Way_Direction;
   Three_Way_Toggle : Three_Way_Direction;

begin
   loop
      Get (Controller       => Controller,
           Red_Button       => Red_Button,
           Black_Button     => Black_Button,
           Two_Way_Toggle   => Two_Way_Toggle,
           Three_Way_Toggle => Three_Way_Toggle);

      if Black_Button_Pressed then
         if Black_Button = Up then
            Count := Count + 1;
	    Black_Button_Pressed := False;
         end if;
      elsif Black_Button = Down then
	 Black_Button_Pressed := True;
      end if;

      if Red_Button_Pressed then
         if Red_Button = Up then
            Ada.Integer_Text_IO.Put (Count);
            Ada.Text_IO.New_Line;
            Count := 0;
            Red_Button_Pressed := False;
         end if;
      elsif Red_Button = Down then
         Red_Button_Pressed := True;
      end if;
      --Keep from confusing values when button switches from pressed to released
      delay 0.01;
   end loop;
end Button_Press_Count;
