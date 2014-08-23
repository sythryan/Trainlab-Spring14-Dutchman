-- Author: Anthony Spoerl
-- Team  : The Grounded Dutchman
with Port_IO;
use Port_IO;

package body dac is
   subtype Digital_Volts is Port_IO.Word range 0 .. 4095;

   Base : constant Port_IO.Address_Range := (16#240#);
   -- Address in form of: Base + (Channel_Number * 2)

   procedure Write (Channel : in Channel_Number;
		    Value   : in Output_Volts) is

      Digital_Value : Digital_Volts;

   begin
      -- Convert voltage to 12 bit integer value within 4095
      Digital_Value := Digital_Volts ((Value + 5.0) * 409.5);

      Port_IO.Out_Word (Address => Base + (Address_Range (Channel) * 2),
			Data    => Digital_Value);
   end Write;

-- Package initialization set all channels to 0 volts
begin
   for Channel in Channel_Number loop
      Port_IO.Out_Word (Address => Base + (Address_Range (Channel) * 2),
			Data    => 4095 / 2);
   end loop;
end dac;
