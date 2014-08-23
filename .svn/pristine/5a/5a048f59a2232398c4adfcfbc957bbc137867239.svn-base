--Author : Abubakar Audu
--Team   : The Grounded Dutchman
--Date   : 4/6/2014
with Double_Talk; use Double_Talk;
use Double_Talk.Phrase_Strings;
with Ada.Text_IO;

package body Halls_Test_Callback is
   procedure Sensor_Pointer (Sensor : in Layout.Hall_Sensor) is
      Phrase : Double_Talk.Phrase_Strings.Bounded_String;
   begin
      Phrase := To_Bounded_String
("This is hall sensor number " & Layout.Hall_Sensor'Image (Sensor));
      Speak (Phrase => Phrase,
             Voice  => Paul);
   end Sensor_Pointer;
end Halls_Test_Callback;
