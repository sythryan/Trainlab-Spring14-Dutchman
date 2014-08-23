--Author : Abubakar Audu
--Team   : The Grounded Dutchman
--Date   : 4/6/2014

with Ada.Text_IO;
with Halls;
with Halls_Test_Callback;
with Double_Talk;
use Double_Talk;
use Double_Talk.Phrase_Strings;
with Layout;
use Layout;

procedure halls_test is
   Phrase : Double_Talk.Phrase_Strings.Bounded_String;
   Enabled : Halls.Callback_Ptr;

   procedure Initialize_Sensors is
   begin
      Ada.Text_IO.Put_Line ("Setting up the Hall sensor interface electronics");
      Halls.Initialize;
   end Initialize_Sensors;

   --Test enable procedure
   --Initialize then call enable
   --Test disable procedure
   --Initialize then call disable
   procedure Sensor_Enable is
   begin
      Enabled := Halls_Test_Callback.Sensor_Pointer'Access;
      Ada.Text_IO.Put_Line ("Sensor Enabled Reached");
      Halls.Enable (Enabled);
   end Sensor_Enable;

   procedure Is_Trigger_Test is
   begin
      Ada.Text_IO.Put_Line ("Is trigger reached");
      for SensorNumber in Layout.Hall_Sensor'Range loop
         Ada.Text_IO.Put_Line
      ("Place a magnet over sensor " & Layout.Hall_Sensor'Image (SensorNumber));
         Ada.Text_IO.Put_Line ("Press Enter when ready");
         Ada.Text_IO.Skip_Line;
         if Halls.Is_Triggered (SensorNumber) then
            Phrase := To_Bounded_String
          ("There is a magnet over" & Layout.Hall_Sensor'Image (SensorNumber));
            Speak (Phrase => Phrase,
                   Voice  => Paul);
         end if;
 	 Ada.Text_IO.Put_Line
      ("Remove magnet over sensor " & Layout.Hall_Sensor'Image (SensorNumber));
         Ada.Text_IO.Put_Line ("Press Enter when ready");
         Ada.Text_IO.Skip_Line;
         if not Halls.Is_Triggered (SensorNumber) then
            Phrase := To_Bounded_String
          ("Magnet is removed over" & Layout.Hall_Sensor'Image (SensorNumber));
            Speak (Phrase => Phrase,
                   Voice  => Paul);
         end if;
      end loop;
   end Is_Trigger_Test;

   procedure Sensor_Disable is
   begin
      Ada.Text_IO.Put_Line ("Sensor disabled Reached");
      Halls.Disable;
      Is_Trigger_Test;
   end Sensor_Disable;

begin
   Ada.Text_IO.Put_Line ("******* Begin Hall Sensor Testing *******");
   Initialize_Sensors;
--     Is_Trigger_Test;
   Sensor_Enable;
   Ada.Text_IO.Put_Line ("Place magnet over a sensor");
   loop
      Ada.Text_IO.Skip_Line;
   end loop;
   Sensor_Disable;
end halls_test;
