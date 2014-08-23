-- Author: Nigel Ploof
-- Right now only tests our algorithm for converting voltage to 12 bit digital
with dac; use dac;
with Ada.Text_IO;

procedure dac_test is
   package Channel_IO is new Ada.Text_IO.Integer_IO (dac.Channel_Number);
   package Volts_IO is new Ada.Text_IO.Fixed_IO (dac.Input_Volts);

   Channel : dac.Channel_Number;
   Value   : dac.Input_Volts;
   Ready   : Character;

begin
   loop
      Ada.Text_IO.Put_Line ("----------------------------------------");
      Ada.Text_IO.Put_Line
        ("Please enter a DAC channel number");
      Ada.Text_IO.Put_Line ("Choices: 0 - 5");
      Channel_IO.Get (Channel);
      Ada.Text_IO.Put_Line
        ("Please enter a voltage");
      Ada.Text_IO.Put_Line ("Choices: -5 to +5");
      Volts_IO.Get (Value);
      dac.Write (Channel => Channel,
		 Value   => Value);
      Ada.Text_IO.Put_Line ("#############################################");
      Ada.Text_IO.Put_Line (" ");
      Ada.Text_IO.Put ("Please Measure channel ");
      Channel_IO.Put (Channel);
      Ada.Text_IO.Put (" for ");
      Volts_IO.Put (Value);
      Ada.Text_IO.Put_Line (" volts, press enter when ready to take " &
			      "a different measurement");

      Ada.Text_IO.Get (Ready);
      Ada.Text_IO.Put_Line (" ");
   end loop;
exception
   when others =>
      Ada.Text_IO.Put_Line
        ("An Error has occured double check your data is valid");
      Ada.Text_IO.Put_Line ("Available channels are 0 - 5, Voltages are -5" &
			      "to + 5");
end dac_test;
