with Common_Units;
with dac;

-- Author : Syth Ryan
-- Team : The Grounded Dutchman

package body Cabs is

   type Cab_Array is array (Cab_ID) of Percent;

   Limit  : Cab_Array := (others => 0); -- initialize to zero
   Shadow : Cab_Array := (others => 0); -- possible need for mutual exclusion

   ---------
   -- Set --
   ---------

   procedure Set (Cab   : in Control_Cab_ID;
		  Value : in Percent) is
   begin
      if Value <= Limit (Cab) then
         Shadow (Cab) := Value;
         dac.Write (Channel => dac.Channel_Number (Cab - 1),
		    Value  => Common_Units.Volts (5.0 * Float (Value) / 100.0));
      end if;
   end Set;

   ---------
   -- Get --
   ---------

   procedure Get (Cab   : in  Cab_ID;
		  Value : out Percent) is
   begin
      Value := Shadow (Cab);
   end Get;

   ---------------
   -- Set_Limit --
   ---------------

   procedure Set_Limit (Cab   : in Control_Cab_ID;
			Value : in Percent) is
      Cab_Value : Percent;
   begin
      Limit (Cab) := Value;
      Get (Cab   => Cab,
	   Value => Cab_Value);
      if Cab_Value > Limit (Cab) then
	 Set (Cab   => Cab,
              Value => Limit (Cab));
      end if;

   end Set_Limit;

   ---------------
   -- Get_Limit --
   ---------------

   procedure Get_Limit (Cab   : in  Cab_ID;
			Value : out Percent) is
   begin
      Value := Limit (Cab);
   end Get_Limit;

end Cabs;
