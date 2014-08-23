-- Author: Nigel
-- Team  : The Grounded Dutchman

with Port_IO; use type Port_IO.Address_Range;
with Layout;
use Layout;
with Ada.Unchecked_Conversion;
package body Motors is

   subtype Byte_Range is Natural range 0 .. 7;
   type Bit_Array is array (Byte_Range) of Boolean;

   for Bit_Array'Component_Size use 1;



   function To_Bit_Array is new Ada.Unchecked_Conversion
     (Source => Port_IO.Byte,
      Target => Bit_Array);

   function To_Byte is new Ada.Unchecked_Conversion
     (Source => Bit_Array,
      Target => Port_IO.Byte);

   protected Data_Register is
      procedure Write (Port_Address : in Port_IO.Address_Range;
                       Motor	    : in Layout.Turnout_ID;
                       Direction    : in Layout.Turn_Choice);
      procedure Read (Port_Address  : in Port_IO.Address_Range;
                      Data_Byte	    : out Bit_Array);
   private
      Data_Byte	 : 	Bit_Array;
   end Data_Register;
   ---------------------------------------------------------------------------
   protected body Data_Register is
      procedure Read (Port_Address : in Port_IO.Address_Range;
                      Data_Byte	   : out Bit_Array) is
      begin
         Data_Byte := To_Bit_Array (Port_IO.In_Byte (Address => Port_Address));
      end Read;

      procedure Write (Port_Address : in Port_IO.Address_Range;
                       Motor        : in Layout.Turnout_ID;
                       Direction    : in Layout.Turn_Choice) is
      begin
         -- Read in from register so we replace everything like it was execept
         -- the bit we want to manipulate

         Data_Byte := To_Bit_Array (Port_IO.In_Byte (Address => Port_Address));

         --Manipulate bits in Array
         if Direction = Right then
            Data_Byte (Byte_Range ((Motor - 1) rem 8)) := True;
         else
            Data_Byte (Byte_Range ((Motor - 1) rem 8)) := False;
         end if;

         --Write out to the register
         Port_IO.Out_Byte (Address => Port_Address,
                           Data    => To_Byte (Data_Byte));
      end Write;

   end Data_Register;

   ---------------------------------------------------------------------------

   ---------
   -- Set --
   ---------

   procedure Set
     (Motor	 : in Layout.Turnout_ID;
      Direction	 : in Layout.Turn_Choice) is

      Base        : Port_IO.Address_Range := 16#220#;
      Base_Offset : Natural range  0 .. 15;

   begin
      ------Add card offset to Base_Offset
      Base_Offset := Natural (Motor / 25 * 8);

      ---Add port offset to Base_Offset
      if (((Motor - 1) / 8) rem 3) = 1 then
         Base_Offset := 2 + Base_Offset; --Should be Port C, +2
      elsif (((Motor - 1) / 8) rem 3) = 2 then
         Base_Offset := 1 + Base_Offset; --Should be Port B, +1
      end if;


      -- Add Chipset offset to Base
      if (Motor / 17) - (Motor / 25) /= 1 then -- Is_Low
         Base_Offset := Base_Offset + 4;
      end if;

      --- At this point Card, Port and Chipset should be calc'd into Base --

      Base := Base + Port_IO.Address_Range (Base_Offset);

      Data_Register.Write (Port_Address => Base,
                           Motor        => Motor,
                           Direction    => Direction);
   end Set;

   -----------------
   -- In_Position --
   -----------------

   function In_Position (Motor : in Layout.Turnout_ID) return Boolean is
      Base            : Port_IO.Address_Range := 16#220#;
      Data_Byte       : Bit_Array;
      Base_Offset     : Natural range 0 .. 15;

   begin
      --Calc Card Offset
      Base_Offset := Natural (Motor / 25 * 8);

      --Calc Port Offset
      if ((Motor - 1) / 8) rem 3 = 0 then
         Base_Offset := Base_Offset + 1;
         --Not nessisary to check for port A where added offset = 0
      elsif ((Motor - 1) / 8) rem 3 = 2 then
         Base_Offset := Base_Offset + 2;
      end if;

      -- Add Chipset offset to Base
      if Motor < 9 or Motor > 24 then -- Is_High
         Base_Offset := Base_Offset + 4;
      end if;

      --- At this point Card, Port and Chipset should be calc'd into Base ---
      Base := Base + Port_IO.Address_Range (Base_Offset);

      -- Read in from register to check In_Position Bit
      --Protected version
      Data_Register.Read (Port_Address => Base,
                          Data_Byte    => Data_Byte);

      --Check output bits in Array, returns True/False
      return Data_Byte (Byte_Range ((Motor - 1) rem 8));

   end In_Position;


   procedure init is
      type Address_Array is array (0 .. 11) of Port_IO.Address_Range;
      Base            : constant Port_IO.Address_Range := 16#220#;
      Addresses_To_Set_Left : constant Address_Array :=
              (220, 221, 222, 224, 225, 226, 228, 229, 230, 232, 233, 234);
      Data_Byte : constant Bit_Array :=
                    (False, False, False, False, False, False, False, False);
   begin
      --Config Low ports Card 1 and 2
      Port_IO.Out_Byte (Address => Base + Port_IO.Address_Range (3),
                        Data    => 153);
      Port_IO.Out_Byte (Address => Base + Port_IO.Address_Range (11),
                        Data    => 153);
      --Config High Ports Card 1 & 2
      Port_IO.Out_Byte (Address => Base + Port_IO.Address_Range (7),
                        Data    => 130);
      Port_IO.Out_Byte (Address => Base + Port_IO.Address_Range (15),
                        Data    => 130);
      for Address in Addresses_To_Set_Left'Range loop
         Port_IO.Out_Byte (Address => Addresses_To_Set_Left (Address),
                           Data    => To_Byte (Data_Byte));
      end loop;
   end init;

begin
   init;
   --Init programs chips so that ports connected to turnout motor control bits
   --are set as output ports
end Motors;
