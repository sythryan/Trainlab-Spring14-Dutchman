with Ada.Text_IO;
with Double_Talk;  use Double_Talk;
use Double_Talk.Phrase_Strings;

-- Author: Abubakar
-- Team: The Grounded Dutchman
-- Date: 3/4/2014

procedure double_talk_test is
   Phrase : Double_Talk.Phrase_Strings.Bounded_String;

begin
   Ada.Text_IO.Put_Line ("Testing Voices...");
   for Voice in Double_Talk.Voice_Range loop
      Phrase := To_Bounded_String
      ("This is " & Double_Talk.Voice_Range'Image (Voice) & "s voice");

      Double_Talk.Speak (Phrase => Phrase,
			Voice  => Voice);
   end loop;
   Ada.Text_IO.Put_Line ("Press Enter to begin testing entry queue...");
   Ada.Text_IO.Skip_Line;



   -- Should queue 7 phrases, removing the first three to make room
   -- Expected output will be phrases 4 - 7
   Ada.Text_IO.Put_Line ("Testing Entry Queue...");


   for count in 1 .. 7 loop
      Phrase := To_Bounded_String
   ("This is phrase " & Integer'Image (count));

      Speak (Phrase => Phrase,
             Voice  => Paul);
   end loop;

end double_talk_test;
