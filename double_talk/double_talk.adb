with Bounded_Queue;
with Port_IO; use type Port_IO.Address_Range;
with System; use System;
with Ada.Unchecked_Conversion;
with Ada.Strings.Bounded; use Ada.Strings.Bounded;
with Ada.Exceptions;
--  with Display;
package body Double_Talk is
   -- Author : Syth Ryan
   -- Team   : The Grounded Dutchman

   Base       : constant Port_IO.Address_Range := 16#31F#;
   End_Phrase : constant Character := Character'Val (0);
   Command    : constant Character := Character'Val (1);

   type Voice_Array is array (0 .. 7) of Character;
   Voice_Numbers : constant Voice_Array :=
		     ('0', '1', '2', '3', '4', '5', '6', '7');

   type Phrase_Rec is -- Type for the Phrase and Voice Selection
      record
	 String	 : Phrase_Strings.Bounded_String;
	 Voice	 : Voice_Range;
      end record;

   -----------------------------
   type TTS_Rec is
      record
         Trash_Seven  : Boolean;
	 Sync         : Boolean;
	 Sync_Two     : Boolean;
	 Ready        : Boolean;
	 Almost_Full  : Boolean;
         Almost_Empty : Boolean;
         Trash_One    : Boolean;
         Trash_Zed    : Boolean;
      end record;

   for TTS_Rec use
      record                             -- values when True:
         Trash_Seven  at 0 range 7 .. 7;
	 Sync         at 0 range 6 .. 6; -- producing output
         Sync_Two     at 0 range 5 .. 5; -- Early, turns zero .4 seconds before
	 Ready        at 0 range 4 .. 4; -- Ready to accept a byte of data
	 Almost_Full  at 0 range 3 .. 3; -- less than 300 bytes available
         Almost_Empty at 0 range 2 .. 2; -- less than 300 bytes left in buffer
         Trash_One    at 0 range 1 .. 1; -- unneeded data made rec entry
         Trash_Zed    at 0 range 0 .. 0; -- only for unchecked conversion
      end record;

   for TTS_Rec'Size use 8;
   for TTS_Rec'Bit_Order use Low_Order_First;

   function To_Bits is new Ada.Unchecked_Conversion (Source => Port_IO.Byte,
						     Target => TTS_Rec);
   -----------------------------
   package Text_Queue is new Bounded_Queue (Element_Type => Phrase_Rec);

   protected type Bounded_Buffer (Buffer_Size : Positive) is
      procedure Put (Item :  in Phrase_Rec);
      entry Take (Item : out Phrase_Rec);
   private
      Buffer : Text_Queue.Queue_Type (Buffer_Size);
   end Bounded_Buffer;

   The_Buffer : Bounded_Buffer (Buffer_Size => Buffer_Size);
   -----------------------------------------------------------------------------
   procedure Send_Character (Char : in Character);

   -----------------------------------------------------------------------------
   protected body Bounded_Buffer is
      procedure Put (Item : in Phrase_Rec) is
	 Trash : Phrase_Rec;
      begin
	 if Text_Queue.Full (Buffer) then
	    Text_Queue.Dequeue (Queue => Buffer,
			        Item  => Trash);
	 end if;
	 Text_Queue.Enqueue (Queue => Buffer,
		             Item  => Item);
      end Put;
      -----------------------------------------------
      entry Take (Item : out Phrase_Rec)
	when not Text_Queue.Empty (Buffer) is
      begin
	 Text_Queue.Dequeue (Queue => Buffer,
		             Item  => Item);
      end Take;
   end Bounded_Buffer;

   -----------------------------------------------------------------------------
   procedure Send_Character (Char : in Character) is
      Shadow_Current  : TTS_Rec;
      Shadow_Previous : TTS_Rec;

   begin
         Shadow_Current.Ready := False;
         Shadow_Previous.Ready := False;
         -- Wait until double talk is ready to receive
         -- Each iteration, Check if Ready
         loop
            -- Check if Double talk is ready to receive
            Shadow_Current := To_Bits (Port_IO.In_Byte (Address => Base));
            delay 0.01;
            --If Double talk is ready for two pullings send the byte
            -- else assign current pulling value to previous
            exit when Shadow_Current.Ready and
              Shadow_Current.Ready = Shadow_Previous.Ready;
            Shadow_Previous.Ready := Shadow_Current.Ready;

         end loop;
         Port_IO.Out_Byte (Address => Base,
                           Data    => Character'Pos (Char));
   end Send_Character;

   -----------------------------------------------------------------------------
   procedure Set_Voice (Voice : in Voice_Range) is
   begin
      Send_Character (Char => Command);
      Send_Character (Voice_Numbers (Voice_Range'Pos (Voice)));
      Send_Character ('O');
   end Set_Voice;

   -----------------------------------------------------------------------------
   -- Consumer
   task Consumer;
   task body Consumer is
      Item  : Phrase_Rec;
      Char  : Character;
   begin
      loop
         The_Buffer.Take (Item => Item);
         Set_Voice (Voice => Item.Voice);
         -- Process each character of the string
         -- Each iteration, process one character
         for Index in 1 .. Phrase_Strings.Length (Source => Item.String) loop
            Char := Phrase_Strings.Element (Source => Item.String,
                                            Index  => Index);
            Send_Character (Char);
         end loop;
         -- we are done, speak phrase
         Send_Character (Char => End_Phrase);
         delay 0.01;
      end loop;
   exception
      when Error : others =>
--  	 Display.Put_Error ("Double talk Exception was raised:    " &
--  		       Ada.Exceptions.Exception_Name (Error) & " " &
--  		       Ada.Exceptions.Exception_Message (Error));
	 null;
   end Consumer;

   -----------------------------------------------------------------------------
   -- Speak --------------------------------------------------------------------
   -----------------------------------------------------------------------------
   procedure Speak
     (Phrase : in Phrase_Strings.Bounded_String;
      Voice  : in Voice_Range) is

      Item : Phrase_Rec;

   begin
      Item := (String => Phrase,
	       Voice  => Voice);
      The_Buffer.Put (Item => Item);
   end Speak;
end Double_Talk;
