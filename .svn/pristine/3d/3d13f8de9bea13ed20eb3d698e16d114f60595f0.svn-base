with Port_IO; use Port_IO;
with Ada.Unchecked_Conversion;
package body Dallee is
   -- Author : Syth Ryan
   -- Team   : The Grounded Dutchman
   type Sound is (Horn, Bell);
   type Dallee_Unit is array (Sound_Unit, Sound) of Sound_Status;
   for Dallee_Unit'Component_Size use 1;
   for Dallee_Unit'Size use 8;

   function To_Byte is new Ada.Unchecked_Conversion (Source => Dallee_Unit,
						     Target => Byte);

   --Base : constant Port_IO.Address_Range := (16#240#);
   Base : constant Port_IO.Address_Range := (16#260#);

   Dallee_Status : Dallee_Unit := (others => (others => Off)); --Shadow Register
   for Dallee_Status'Size use 8;
   -----------------------------------------------------------------------------
   procedure Sound_Horn (Number : in Installed_Unit;
			 Set_To : in Sound_Status) is
   begin
      Dallee_Status (Number, Horn) := Set_To;
      Out_Byte (Address => Base + 3,
		Data    => To_Byte (Dallee_Status));

   end Sound_Horn;

   -----------------------------------------------------------------------------
   procedure Sound_Bell (Number : in Installed_Unit;
			 Set_To : in Sound_Status) is
   begin
      Dallee_Status (Number, Bell) := Set_To;
      Out_Byte (Address => Base + 3,
		Data    => To_Byte (Dallee_Status));
   end Sound_Bell;

begin
   Out_Byte (Address => Base + 3,
	     Data    => 0);
end Dallee;
