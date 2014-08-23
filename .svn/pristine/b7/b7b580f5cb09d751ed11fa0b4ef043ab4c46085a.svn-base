-- Author : Syth Ryan
-- Team   : The Grounded Dutchman
package Dallee is

   type Sound_Status is (Off, On);
   type Sound_Unit is range 1 .. 4;
   subtype Installed_Unit is Sound_Unit range 1 .. 3;

   -----------------------------------------------------------------------------
   procedure Sound_Horn (Number : in Installed_Unit;
			 Set_To : in Sound_Status);
   -- Turns a horn on or off specified by Number
   --
   -----------------------------------------------------------------------------
   procedure Sound_Bell (Number : in Installed_Unit;
			 Set_To : in Sound_Status);
   -- Turns a bell on or off specified by Number
   --
private
   for Sound_Status use (Off => 0,
			 On  => 1);
   for Sound_Status'Size use 1;
end Dallee;
