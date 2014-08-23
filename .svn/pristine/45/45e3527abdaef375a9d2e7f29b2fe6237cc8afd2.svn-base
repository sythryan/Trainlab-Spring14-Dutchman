--Authors: Abubakar Audu, Nigel Ploof
--Team  :  The Grounded Dutchman
with Ada.Unchecked_Conversion;
with Port_IO; use type Port_IO.Address_Range;
with Interfaces;
package body ADC is
   --Address range is 0 to 608 = 260 Hex
   Base : constant Port_IO.Address_Range := 16#260#;
   -- Twenty five microseconds
   Max_Conversion_Time : constant Duration := 25.0E-6;

   My_Semaphore        : Semaphore (Initial_Value => 1);

   type CSR_Rec is -- Type for the control Status register
      record
         Channel	: Channel_Number;
         EOC	 	: Boolean;
      end record;

   for CSR_Rec use
      record
         Channel at 0 range 0 .. 2;
         EOC    at 0 range 7 .. 7;
      end record;

   for CSR_Rec'Size use 8;

   subtype Data_Range is Port_IO.Word;

   function To_CSR is new Ada.Unchecked_Conversion
     (Source => Port_IO.Byte,
      Target => CSR_Rec);
   function To_Byte is new Ada.Unchecked_Conversion
     (Source => CSR_Rec,
      Target => Port_IO.Byte);
   function To_Unsigned_16 is new Ada.Unchecked_Conversion
     (Source => Data_Range,
      Target => Interfaces.Unsigned_16);

   protected body Semaphore is
      procedure Signal is
      begin
	 Count := Count + 1;
      end Signal;

      entry Wait when Count > 0 is
      begin
	 Count := Count - 1;
      end Wait;
   end Semaphore;

   procedure Read (Channel : in Channel_Number;
                   Value   : out Input_Volts) is
      Shadow : CSR_Rec;
      for Shadow'Size use 8;
      Data_Reg : Data_Range;
      --for Data_Reg'Size use 16;
      Shifted_Data_Reg : Interfaces.Unsigned_16;
      --for Shifted_Data_Reg'Size use 16;
   begin
      My_Semaphore.Wait;
      Shadow := (EOC => True, Channel => Channel);
      for Index in 1 .. 2 loop
	 Port_IO.Out_Byte (Address => (Base + 2),    --CSR_Address (Base + 2)
                           Data    => To_Byte (Shadow)); --Using unchecked conv.
	 Port_IO.Out_Byte (Address => (Base + 1),
		           Data    => 0);
	 loop
	    delay Max_Conversion_Time / 10;
	    Shadow := To_CSR (Port_IO.In_Byte (Base + 2)); --Use unchecked conv.
	    exit when not Shadow.EOC;
	 end loop;
      end loop;
      Data_Reg := Port_IO.In_Word (Base); --Data_Address (Base + 1)
      Shifted_Data_Reg := Interfaces.Shift_Right (To_Unsigned_16 (Data_Reg), 4);
      Value := Volts (Shifted_Data_Reg) / 409.5 - 5.0;
      -- fail safe for when we get a bad reading
      if Value < 0.0 then
	 Value := 0.0;
      end if;
      My_Semaphore.Signal;
   end Read;
end ADC;


