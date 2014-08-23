with Layout;
with Layout.Search;
with Ada.Text_IO;
with Ada.Exceptions;

-- Author : Anthony,
-- Team   : The Grounded Dutchman

procedure Layout_Search_Test is
   package Block_IO is new Ada.Text_IO.Integer_IO (Layout.Block_ID);
   package Turnout_IO is new Ada.Text_IO.Integer_IO (Layout.Turnout_ID);
   package Limb_IO is
     new Ada.Text_IO.Enumeration_IO (Enum => Layout.Turn_Choice);
   package Direction_IO is
     new Ada.Text_IO.Enumeration_IO (Enum => Layout.Direction);


   Loco     : Layout.Block_ID;
   Caboose  : Layout.Block_ID;
   Blocks   : Layout.Search.Block_List (3);
   Turnouts : Layout.Search.Turnout_List (2);
   Success  : Boolean;

begin
   loop
      begin
         Ada.Text_IO.Put ("Enter location of Locomotive:");
         Block_IO.Get (Loco);
         Ada.Text_IO.Put ("Enter location of Caboose:");
         Block_IO.Get (Caboose);
	 Ada.Text_IO.New_Line;
         Layout.Search.Blocks_Beneath (Loco     => Loco,
				       Caboose  => Caboose,
				       Blocks   => Blocks,
				       Turnouts => Turnouts,
				       Success  => Success);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("-------------------------------");

         if Success then
            Ada.Text_IO.Put_Line ("Block information: ");
            for Index in 1 .. Blocks.Size loop
               Ada.Text_IO.Put ("Block ID: ");
               Block_IO.Put (Blocks.Items (Index).Block);
               Ada.Text_IO.Put (" Direction: ");
               Direction_IO.Put (Blocks.Items (Index).My_Direction);
               Ada.Text_IO.New_Line;
            end loop;
            Ada.Text_IO.New_Line;

            Ada.Text_IO.Put_Line ("Turnout information: ");
            for Index in 1 .. Turnouts.Size loop
               Ada.Text_IO.Put ("Turnout ID: ");
               Turnout_IO.Put (Turnouts.Items (Index).Turnout);
               Ada.Text_IO.Put (" Limb Direction: ");
               Limb_IO.Put (Turnouts.Items (Index).Direction);
               Ada.Text_IO.New_Line;
            end loop;
            if Turnouts.Size = 0 then
               Ada.Text_IO.Put_Line ("None");
            end if;

         else
            Ada.Text_IO.Put_Line ("Search returned unsuccessful.");
         end if;
      exception
         when Error : others =>
            Ada.Text_IO.Put ("Exception was raised:    ");
            Ada.Text_IO.Put (Ada.Exceptions.Exception_Name (Error) & " ");
            Ada.Text_IO.Put (Ada.Exceptions.Exception_Message (Error));
            Ada.Text_IO.New_Line;
      end;
      Ada.Text_IO.Put_Line ("-------------------------------");
      Ada.Text_IO.New_Line;
   end loop;

end Layout_Search_Test;
