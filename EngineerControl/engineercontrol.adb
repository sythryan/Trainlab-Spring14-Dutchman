with Port_IO;
use type Port_IO.Address_Range;
with Ada.Unchecked_Conversion;
with System; use System;
with ADC;
with Common_Units; use Common_Units;
-- Author: Syth Ryan
-- Team  : The Grounded Dutchman
package body EngineerControl is

   type Controller_Rec is
      record
	 Red_Button       : Button;
	 Black_Button     : Button;
	 Direction_Switch : Two_Way_Direction;
	 Turn_Switch      : Three_Way_Direction;
      end record;

   for Controller_Rec use
      record
         Red_Button       at 0 range 0 .. 0;
         Black_Button     at 0 range 1 .. 1;
         Direction_Switch at 0 range 2 .. 2;
         Turn_Switch      at 0 range 3 .. 4;
      end record;

   for Controller_Rec'Size use 8;
   for Controller_Rec'Bit_Order use Low_Order_First;

   function To_Bits is new Ada.Unchecked_Conversion (Source => Port_IO.Byte,
						     Target => Controller_Rec);

   Base : constant Port_IO.Address_Range := (16#240#);

   type Address_Array is array (Controller_ID) of Port_IO.Address_Range;

   Address_Of : constant Address_Array := (Base + 12,
					   Base + 13,
					   Base + 14);
   ---------
   -- Get --
   ---------

   procedure Get
     (Controller       : in Controller_ID;
      Red_Button       : out Button;
      Black_Button     : out Button;
      Two_Way_Toggle   : out Two_Way_Direction;
      Three_Way_Toggle : out Three_Way_Direction) is

      My_Byte : Port_IO.Byte;
      Controller_Status  : Controller_Rec;

      for Controller_Status'Size use 8;
   begin
	 for count in 1 .. 2 loop
	    My_Byte := Port_IO.In_Byte (Address => Address_Of (Controller));
	    delay 0.005;
	 end loop;

	 Controller_Status := To_Bits (My_Byte);

	 Red_Button        := Controller_Status.Red_Button;
	 Black_Button      := Controller_Status.Black_Button;
	 Two_Way_Toggle    := Controller_Status.Direction_Switch;
	 Three_Way_Toggle  := Controller_Status.Turn_Switch;

   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
     (Controller	  : in Controller_ID;
      Knob	          : out Common_Units.Percent) is

      Volt_Reading : Common_Units.Volts;
      Channel      : ADC.Channel_Number;
   begin
      case Controller is
	 when A =>
	    Channel := 0;
	 when B =>
	    Channel := 1;
	 when C =>
	    Channel := 2;
      end case;

      ADC.Read (Channel => Channel,
		Value   => Volt_Reading);
      Knob :=  Percent (Common_Units.Volts ((Volt_Reading / 5) * 100));
   end Get;
end EngineerControl;
