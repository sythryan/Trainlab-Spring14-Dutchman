-- *** Comment these out if not running under MaRTE
with MaRTE_OS;
pragma Warnings (Off, MaRTE_OS);
-- ***

with Ada.Text_IO;
with ADC;
with Common_Units;
with Ada.Exceptions;

procedure Adc_Test_Program is
   -- Author : Syth Ryan
   -- Team   :  The Grounded Dutchman

   package Channel_IO is new Ada.Text_IO.Integer_IO (ADC.Channel_Number);
   package Volts_IO is new Ada.Text_IO.Fixed_IO (Common_Units.Volts);

   -----------------------------------------------------------------------------
   procedure Test_Hand_Controller is
      -- Hand Controller Test
      -- Uses Channels 0, 1, 2

      Channel : ADC.Channel_Number;
      Reading : Common_Units.Volts;

   begin
      loop
	 Ada.Text_IO.Put_Line ("----------------------------------------");
	 Ada.Text_IO.Put_Line
	   ("Please enter a hand controller channel number");
	 Ada.Text_IO.Put_Line ("Choices: A=>0, B=>1, C=>2");
	 Channel_IO.Get (Channel);
	 if Integer (Channel) > 2 then
	    Ada.Text_IO.Put_Line ("This test is only using channels 0 - 2");
	    Ada.Text_IO.New_Line;
	 else
	    ADC.Read (Channel => Channel,
	       Value   => Reading);
	    Ada.Text_IO.Put_Line ("The Reading from Channel #");
	    Channel_IO.Put (Item  => Channel,
		     Width => 0);
	    Ada.Text_IO.Put_Line (" is ");
	    Volts_IO.Put (Item => Reading,
		   Fore => 0,
		   Aft  => 0,
		   Exp  => 0);
	    Ada.Text_IO.New_Line (2);

	 end if;
      end loop;
   exception
      when Error : others =>
	 Ada.Text_IO.Put ("Exception was raised:    ");
	 Ada.Text_IO.Put (Ada.Exceptions.Exception_Name (Error) & " ");
	 Ada.Text_IO.Put (Ada.Exceptions.Exception_Message (Error));
	 Ada.Text_IO.New_Line;
	 Ada.Text_IO.Put_Line
	   ("An Error has occured double check your data is valid");
	 Ada.Text_IO.Put_Line ("Available channels are 0 - 7");
   end Test_Hand_Controller;

   -----------------------------------------------------------------------------
   -- ADC Test Program
begin
   ---------------------------
   -- Hand Controllers Test --
   ---------------------------
   Ada.Text_IO.Put_Line ("         Hand Controller ADC Test");
   Ada.Text_IO.Put_Line ("**********************************************");
   loop
      Ada.Text_IO.Put_Line ("Adjust Knob and press enter to test..");
      Ada.Text_IO.Skip_Line;
      Test_Hand_Controller;
      Ada.Text_IO.Skip_Line;
      Ada.Text_IO.Put_Line ("----------------------------------------");
      Ada.Text_IO.New_Line;
   end loop;
end Adc_Test_Program;
