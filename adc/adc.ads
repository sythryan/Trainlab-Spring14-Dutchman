-- Author: Nigel Ploof
-- Team   : The Grounded Dutchman

-- 12 bit resolution at a fixed +-5V Range
-- 8 inputs
with Common_Units; use Common_Units;

package ADC is
   --Specification for CIO-DAS08/JR ADC
   type Channel_Number is range 0 .. 7;
   subtype Input_Volts is Common_Units.Volts range -5.0 .. 5.0;

   protected type Semaphore (Initial_Value : Natural) is
      procedure Signal;
      entry Wait;
   private
      Count : Natural := Initial_Value;
   end Semaphore;

   -----------------------------------------------------------------------------
   procedure Read (Channel : in Channel_Number;
                   Value   : out Input_Volts);
   -- Returns the voltage on the given channel

end ADC;


