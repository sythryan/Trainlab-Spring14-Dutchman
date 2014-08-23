-- Author : Nigel Ploof
-- Team   : The Grounded Dutchman

with Common_Units;
use type Common_Units.Volts;

package dac is
   --Specification for CIO-DDA06/Jr
   type Channel_Number is range 0 .. 5;
   subtype Output_Volts is Common_Units.Volts range -5.0 .. 5.0;

   -----------------------------------------------------------------------------
   procedure Write (Channel : in Channel_Number;
		    Value   : in Output_Volts);
   --Outputs the voltage on the given channel
   -----------------------------------------------------------------------------
end dac;
