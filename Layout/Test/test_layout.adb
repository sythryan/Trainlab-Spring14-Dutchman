with Ada.Text_IO;
with Layout; use Layout;
with Ada.IO_Exceptions;
with Ada.Integer_Text_IO;
with Ada.Exceptions;

procedure Test_Layout is
   -- Author : Syth Ryan
   -- Team   : The Grounded Dutchman

   End_OF_Block_File           : constant String :=
		             "./Layout/Test/Test_Files/testCaseForEnd_Of_Block";
   Limb_Of_Turnout_File        : constant String :=
		          "./Layout/Test/Test_Files/testCaseForLimb_Of_Turnout";
   Next_Block_File             : constant String :=
	         	    "./Layout/Test/Test_Files/testCaseForNext_Block(s)";
   Next_Turnout_File           : constant String :=
			     "./Layout/Test/Test_Files/testCaseForNext_Turnout";
   Sensor_Between_Blocks_File  : constant String :=
		    "./Layout/Test/Test_Files/testCaseForSensor_Between_Blocks";
   Sensor_Polarity_File        : constant String :=
		          "./Layout/Test/Test_Files/testCaseForSensor_Polarity";
   Blocks_Around_Sensor_File   : constant String :=
                                "./Layout/Test/Test_Files/blocks_around_sensor";
   Get_Force_Turnout_Info_File : constant String :=
                              "./Layout/Test/Test_Files/get_force_turnout_info";
   Get_Joint_Turnout_File      : constant String :=
                     "./Layout/Test/Test_Files/get_joint_turnout_test_file.txt";
   Next_Choice_Turnout_File    : constant String :=
                       "./Layout/Test/Test_Files/next_choice_turnout_test_file";

   package Block_IO is new Ada.Text_IO.Integer_IO (Layout.Block_ID);
   package Direction_IO is new Ada.Text_IO.Enumeration_IO (Layout.Direction);
   package Terminator_IO is new Ada.Text_IO.Enumeration_IO
     (Layout.Terminator_Type);
   package Turnout_IO is new Ada.Text_IO.Integer_IO (Layout.Turnout_ID);
   package Limb_IO is new Ada.Text_IO.Enumeration_IO (Layout.Limb_Type);
   package Hall_Sensor_IO is new Ada.Text_IO.Integer_IO (Layout.Hall_Sensor);

   -----------------------------------------------------------------------------
   -- End_Of_Block--
   -----------------
   procedure End_Of_Block is
      The_Block       : Layout.Block_ID;
      The_Direction   : Layout.Direction;
      The_Terminator  : Layout.Terminator_Type;
      File            : Ada.Text_IO.File_Type;
   begin

      Ada.Text_IO.Put_Line ("End_Of_Block Test Begin");
      Ada.Text_IO.Open (Name => End_OF_Block_File,
                        File => File,
                        Mode => Ada.Text_IO.In_File);
      Ada.Text_IO.Put_Line ("Testing");
      while not Ada.Text_IO.End_Of_File (File) loop
         Block_IO.Get (File => File,
                       Item => The_Block);
         Direction_IO.Get (File => File,
                           Item => The_Direction);
         Terminator_IO.Get (File => File,
                            Item => The_Terminator);
         Ada.Text_IO.Skip_Line (File => File);
         if Layout.End_Of_Block (Block => The_Block,
                                 Side  => The_Direction) /= The_Terminator then
            Ada.Text_IO.Put ("End_Of_Block failed with block");
            Block_IO.Put (The_Block);
            Ada.Text_IO.Put (" and ");
            Direction_IO.Put (The_Direction);
            Ada.Text_IO.Put (" as inputs.");
         end if;
      end loop;
      Ada.Text_IO.Close (File);
      Ada.Text_IO.Put_Line ("End of End_Of_Block Test ");
      Ada.Text_IO.New_Line;
   end End_Of_Block;

   -----------------------------------------------------------------------------
   --Limb_Of_Turnout--
   ----------------------

   procedure Limb_Of_Turnout is
      File         : Ada.Text_IO.File_Type;
      The_Turnout  : Layout.Turnout_ID;
      The_Limb	   : Layout.Limb_Type;
      The_Block    : Layout.Block_ID;
   begin

      Ada.Text_IO.Put_Line ("Limb_Turnout Begin");
      Ada.Text_IO.Open (Name => Limb_Of_Turnout_File,
                        File => File,
                        Mode => Ada.Text_IO.In_File);
      Ada.Text_IO.Put_Line ("Testing");
      while not Ada.Text_IO.End_Of_File (File) loop
         Turnout_IO.Get (File => File,
                         Item => The_Turnout);
         Limb_IO.Get (File => File,
                      Item => The_Limb);
         Block_IO.Get (File => File,
                       Item => The_Block);
         if Layout.Limb_Of_Turnout (The_Turnout, The_Limb) /= The_Block then
            Ada.Text_IO.Put ("Limb_Of_Turnout failed with Turnout ");
            Turnout_IO.Put (The_Turnout);
            Ada.Text_IO.Put ("and ");
            Limb_IO.Put (The_Limb);
            Ada.Text_IO.Put ("as inputs.");
         end if;
         Ada.Text_IO.Skip_Line (File => File);
      end loop;
      Ada.Text_IO.Close (File);
      Ada.Text_IO.Put_Line ("End of Limb_Turnout Test ");
      Ada.Text_IO.New_Line;
   end Limb_Of_Turnout;

   -----------------------------------------------------------------------------
   --Next_Block---
   ---------------

   procedure  Next_Block is
      File            : Ada.Text_IO.File_Type;
      The_Block       : Layout.Block_ID;
      The_Direction   : Layout.Direction;
      The_Next_Block  : Layout.Block_ID;
   begin

      Ada.Text_IO.Put_Line ("Next_Block Test Begin");
      Ada.Text_IO.Open (Name => Next_Block_File,
                        File => File,
                        Mode => Ada.Text_IO.In_File);
      Ada.Text_IO.Put_Line ("Testing");
      while not Ada.Text_IO.End_Of_File (File) loop
         Block_IO.Get (File => File,
                       Item => The_Block);
         Direction_IO.Get (File => File,
                           Item => The_Direction);
         Block_IO.Get (File => File,
                       Item => The_Next_Block);
         if Layout.Next_Block (The_Block, The_Direction) /= The_Next_Block then
            Ada.Text_IO.Put ("Next_Block failed with block ");
            Block_IO.Put (The_Block);
            Ada.Text_IO.Put ("and ");
            Direction_IO.Put (The_Direction);
            Ada.Text_IO.Put ("as inputs.");
         end if;
         Ada.Text_IO.Skip_Line (File => File);
      end loop;
      Ada.Text_IO.Close (File);
      Ada.Text_IO.Put_Line ("End of Next_Block Test ");
      Ada.Text_IO.New_Line;
   end Next_Block;

   -----------------------------------------------------------------------------
   --Next_Turnout--
   ----------------
   procedure Next_Turnout is
      File          : Ada.Text_IO.File_Type;
      The_Block     : Layout.Block_ID;
      The_Direction : Layout.Direction;
      The_Turnout   : Layout.Turnout_ID;
      Count	    : Natural;
   begin
      Ada.Text_IO.Put_Line ("Next_Turnout Test Begin");
      Ada.Text_IO.Open (Name => Next_Turnout_File,
                        File => File,
                        Mode => Ada.Text_IO.In_File);
      Count := 0;
      Ada.Text_IO.Put_Line ("Testing");
      while not Ada.Text_IO.End_Of_File (File) loop
         Count := Count + 1;
         begin
            Block_IO.Get (File => File,
                          Item => The_Block);
            Direction_IO.Get (File => File,
                              Item => The_Direction);
            Turnout_IO.Get (File => File,
                            Item => The_Turnout);
            if Layout.Next_Turnout
              (The_Block, The_Direction) /= The_Turnout then
               Ada.Text_IO.Put ("Next_Turnout failed with block ");
               Block_IO.Put (The_Block);
               Ada.Text_IO.Put ("and ");
               Direction_IO.Put (The_Direction);
               Ada.Text_IO.Put_Line ("as inputs.");
            end if;
            Ada.Text_IO.Skip_Line (File => File);
         exception
            when Layout.Layout_Error =>
               if Count > 10 then 	-- we expect layout error for first ten
                  Ada.Text_IO.Put_Line ("Layout_Error");
               end if;
         end;
      end loop;
      Ada.Text_IO.Put_Line ("End of Next_Turnout Test ");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Close (File);
   end Next_Turnout;

   -----------------------------------------------------------------------------
   --Sensor_Between_Blocks--
   -------------------------
   procedure Sensor_Between_Blocks is
      The_Block       : Layout.Block_ID;
      Another_Block   : Layout.Block_ID;
      The_Hall_Sensor : Layout.Hall_Sensor;
      File            : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Put_Line ("Sensor_Between_Blocks Test Begin");
      Ada.Text_IO.Open (Name => Sensor_Between_Blocks_File,
                        File => File,
                        Mode => Ada.Text_IO.In_File);
      Ada.Text_IO.Put_Line ("Testing");
      while not Ada.Text_IO.End_Of_File (File) loop
         begin
            Block_IO.Get (File => File,
                          Item => The_Block);
            Block_IO.Get (File => File,
                          Item => Another_Block);
            Hall_Sensor_IO.Get (File => File,
                                Item => The_Hall_Sensor);
            if Layout.Sensor_Between_Blocks
              (The_Block, Another_Block) /= The_Hall_Sensor then
               Ada.Text_IO.Put ("Sensor_Between_Blocks failed with block ");
               Block_IO.Put (The_Block);
               Ada.Text_IO.Put ("and ");
               Block_IO.Put (Another_Block);
               Ada.Text_IO.Put ("as inputs.");
            end if;
         exception
            when Ada.IO_Exceptions.End_Error =>
               Ada.Text_IO.Put_Line ("*******end error testing failed******");
            when Layout.Layout_Error =>
               Ada.Text_IO.Put ("**Layout_Error:  ");
               Block_IO.Put (The_Block);
               Block_IO.Put (Another_Block);
               Hall_Sensor_IO.Put (The_Hall_Sensor);
               Ada.Text_IO.New_Line;
         end;
      end loop;
      Ada.Text_IO.Put_Line ("End of Sensor between blocks test");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Close (File);
   end Sensor_Between_Blocks;

   -----------------------------------------------------------------------------
   -- Sensor_Polarity. Sensors 3,4,5,26,30 and 31 are reversing.--
   ---------------------------------------------------------------
   procedure Sensor_Polarity is
      The_Hall_Sensor : Layout.Hall_Sensor;
      File            : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Put_Line ("Sensor_Polarity Test Begin");
      Ada.Text_IO.Open (Name => Sensor_Polarity_File,
                        File => File,
                        Mode => Ada.Text_IO.In_File);
      Ada.Text_IO.Put_Line ("Testing");
      while not Ada.Text_IO.End_Of_File (File) loop
         Hall_Sensor_IO.Get (File => File,
                             Item => The_Hall_Sensor);
         if The_Hall_Sensor = 3 then
            if Layout.Sensor_Polarity (The_Hall_Sensor) then
               Ada.Text_IO.Put ("Sensor_Polarity failed with Sensor ");
               Hall_Sensor_IO.Put (The_Hall_Sensor);
               Ada.Text_IO.Put (" as input.");
               Ada.Text_IO.New_Line;
            end if;
         elsif The_Hall_Sensor = 4 then
            if Layout.Sensor_Polarity (The_Hall_Sensor) then
               Ada.Text_IO.Put ("Sensor_Polarity failed with Sensor ");
               Hall_Sensor_IO.Put (The_Hall_Sensor);
               Ada.Text_IO.Put (" as input.");
               Ada.Text_IO.New_Line;
            end if;
         elsif The_Hall_Sensor = 5 then
            if Layout.Sensor_Polarity (The_Hall_Sensor) then
               Ada.Text_IO.Put ("Sensor_Polarity failed with Sensor ");
               Hall_Sensor_IO.Put (The_Hall_Sensor);
               Ada.Text_IO.Put (" as input.");
               Ada.Text_IO.New_Line;
            end if;
         elsif The_Hall_Sensor = 26 then
            if Layout.Sensor_Polarity (The_Hall_Sensor) then
               Ada.Text_IO.Put ("Sensor_Polarity failed with Sensor ");
               Hall_Sensor_IO.Put (The_Hall_Sensor);
               Ada.Text_IO.Put (" as input.");
               Ada.Text_IO.New_Line;
            end if;
         elsif The_Hall_Sensor = 30 then
            if Layout.Sensor_Polarity (The_Hall_Sensor) then
               Ada.Text_IO.Put ("Sensor_Polarity failed with Sensor ");
               Hall_Sensor_IO.Put (The_Hall_Sensor);
               Ada.Text_IO.Put (" as input.");
               Ada.Text_IO.New_Line;
            end if;
         elsif The_Hall_Sensor = 31 then
            if Layout.Sensor_Polarity (The_Hall_Sensor) then
               Ada.Text_IO.Put ("Sensor_Polarity failed with Sensor ");
               Hall_Sensor_IO.Put (The_Hall_Sensor);
               Ada.Text_IO.Put (" as input.");
               Ada.Text_IO.New_Line;
            end if;
         else        --Checks non-reversing ones
            if not Layout.Sensor_Polarity (The_Hall_Sensor) then
               Ada.Text_IO.Put ("Sensor_Polarity failed with Sensor ");
               Hall_Sensor_IO.Put (The_Hall_Sensor);
               Ada.Text_IO.Put (" as input.");
               Ada.Text_IO.New_Line;
            end if;
         end if;
         Ada.Text_IO.Skip_Line (File => File);
      end loop;
      Ada.Text_IO.Put_Line ("Sensor Polarity Test complete");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Close (File);
   end Sensor_Polarity;

   -----------------------------------------------------------------------------
   -- Opposite Direction --
   -----------------------
   procedure Opposite_Direction is
   begin
      Ada.Text_IO.Put_Line ("Opposite Direction Test Begin");
      Ada.Text_IO.Put_Line ("Testing");
      if Layout.Opposite (A_Direction => Normal) = Normal or
	Layout.Opposite (A_Direction => Reversed) = Reversed then
	 Ada.Text_IO.Put_Line ("Failure");
      end if;
      Ada.Text_IO.Put_Line ("Opposite Direction Test Complete");
      Ada.Text_IO.New_Line;
   end Opposite_Direction;

   -----------------------------------------------------------------------------
   -- Opposite Turn --
   -------------------
   procedure Opposite_Turn is
   begin
      Ada.Text_IO.Put_Line ("Opposite Turn Test Begin");
      Ada.Text_IO.Put_Line ("Testing");
      if Layout.Opposite (Turn_Direction => Left) /= Layout.Right or
	Layout.Opposite (Turn_Direction => Right) /= Layout.Left then
	 Ada.Text_IO.Put_Line ("Failure");
      end if;
      Ada.Text_IO.Put_Line ("Opposite Turn Test Complete");
      Ada.Text_IO.New_Line;
   end Opposite_Turn;

   -----------------------------------------------------------------------------
   -- Blocks around sensor --
   --------------------------
   procedure Blocks_Around_Sensor is
      File          : Ada.Text_IO.File_Type;
      A_Block       : Block_ID;
      Another_Block : Block_ID;
      Block_1       : Block_ID;
      Block_2       : Block_ID;
      Hall_Sensor   : Layout.Hall_Sensor;
   begin
      Ada.Text_IO.Put_Line ("Blocks Around Sensor Test Begin");
      Ada.Text_IO.Put_Line ("Testing");
      Ada.Text_IO.Open (Name => Blocks_Around_Sensor_File,
                        File => File,
                        Mode => Ada.Text_IO.In_File);
      while not Ada.Text_IO.End_Of_File (File) loop
	 begin
	    Ada.Text_IO.Skip_Line (File);
            Hall_Sensor_IO.Get (File => File,
				Item => Hall_Sensor);
	    Block_IO.Get (File => File,
                          Item => A_Block);
            Block_IO.Get (File => File,
                          Item => Another_Block);
	    Layout.Blocks_Around_Sensor (A_Hall_Sensor => Hall_Sensor,
				         Block_1       => Block_1,
				         Block_2       => Block_2);
	    if not ((Block_1 = A_Block or Block_1 = Another_Block) and
	      (Block_2 = A_Block or Block_2 = Another_Block)) then

	       Ada.Text_IO.Put ("Blocks around sensor failed with sensor ");
	       Hall_Sensor_IO.Put (Hall_Sensor, 0);
	       Ada.Text_IO.New_Line;
            end if;
         exception
            when Ada.IO_Exceptions.End_Error =>
               Ada.Text_IO.Put_Line ("*******end error testing failed******");
            when Layout.Layout_Error =>
               Ada.Text_IO.Put ("**Layout_Error:  ");
               Block_IO.Put (A_Block);
               Block_IO.Put (Another_Block);
               Hall_Sensor_IO.Put (Hall_Sensor);
	       Ada.Text_IO.New_Line;
	    when others =>
	       Ada.Text_IO.Put_Line ("an unknown exception was raised");
	 end;
      end loop;
      Ada.Text_IO.Close (File);
      Ada.Text_IO.Put_Line ("Blocks around sensor test complete");
      Ada.Text_IO.New_Line;
   end Blocks_Around_Sensor;

   -----------------------------------------------------------------------------
   procedure Is_Force_Turnout is
      Checker : Boolean;
   begin
      Ada.Text_IO.Put_Line ("Is Force Turnout Test Begin");
      Ada.Text_IO.Put_Line ("Testing");

      for Index in 1 .. 40 loop
         -- Normal direction check
         Checker := Layout.Is_Force_Turnout (A_Block     => Block_ID (Index),
                                             A_Direction => Normal);
         case Index is
            when 1 | 3 | 5 | 6 | 7 | 9 | 12 | 14 | 16 | 17 | 19 | 20 | 22 | 26 |
                 28 | 25 | 27 | 29 | 30 | 34 | 35 | 38 | 39 | 40 =>
               if not Checker then
                  Ada.Text_IO.Put ("Block");
                  Ada.Integer_Text_IO.Put (Index, 3);
                  Ada.Text_IO.Put_Line (" failure normal direction");
               end if;
            when others =>
               if Checker then
                  Ada.Text_IO.Put ("Block");
                  Ada.Integer_Text_IO.Put (Index, 3);
                  Ada.Text_IO.Put_Line (" failure normal direction");
               end if;
         end case;

         -- Reverse direction check
         Checker := Layout.Is_Force_Turnout (A_Block     => Block_ID (Index),
                                             A_Direction => Reversed);
         case Index is
            when 3 | 9 | 11 | 12 | 13 | 14 | 16 | 19 |
                 22 | 23 | 25 | 27 | 28 | 36 | 32 | 39 =>
               if not Checker then
                  Ada.Text_IO.Put ("Block");
                  Ada.Integer_Text_IO.Put (Index, 3);
                  Ada.Text_IO.Put_Line (" failure reversed direction");
               end if;
            when others =>
               if Checker then
                  Ada.Text_IO.Put ("Block");
                  Ada.Integer_Text_IO.Put (Index, 3);
                  Ada.Text_IO.Put_Line (" failure reveresed direction");
               end if;
         end case;
      end loop;
      Ada.Text_IO.Put_Line ("Is_Force_Turnout test complete");
      Ada.Text_IO.New_Line;
   end Is_Force_Turnout;

   -----------------------------------------------------------------------------
   procedure Force_Turnout_Info is
      File      : Ada.Text_IO.File_Type;
      Turnout   : Layout.Turnout_ID;
      Block     : Layout.Block_ID;
      Direction : Layout.Direction;
      Set       : Layout.Turn_Choice;

      Check_Turnout : Layout.Turnout_ID;
      Check_Set     : Layout.Turn_Choice;
   begin
      Ada.Text_IO.Put_Line ("Force Turnout Info Test Begin");
      Ada.Text_IO.Put_Line ("Testing");
      Ada.Text_IO.Open (Name => Get_Force_Turnout_Info_File,
                        File => File,
                        Mode => Ada.Text_IO.In_File);
      while not Ada.Text_IO.End_Of_File (File) loop
         begin
            Ada.Text_IO.Skip_Line (File);
            Turnout_IO.Get (File  => File,
                            Item  => Turnout);
            Block_IO.Get (File  => File,
                          Item  => Block);
            Direction_IO.Get (File  => File,
                              Item  => Direction);
            Limb_IO.Get (File  => File,
                         Item  => Set);
            Layout.Get_Force_Turnout_Info (A_Block         => Block,
                                           Block_Direction => Direction,
                                           A_Turnout       => Check_Turnout,
                                           Set_Turnout     => Check_Set);
            if Check_Turnout /= Turnout then
               Ada.Text_IO.Put ("Turnouts did not equal on block ");
               Block_IO.Put (Block);
               Ada.Text_IO.Put (", ");
               Direction_IO.Put (Direction);
               Ada.Text_IO.Put_Line (" Direction");
            elsif Check_Set /= Set then
               Ada.Text_IO.Put ("Set direction did not equal on block ");
               Block_IO.Put (Block);
               Ada.Text_IO.Put (", ");
               Direction_IO.Put (Direction);
               Ada.Text_IO.Put_Line (" Direction");
            end if;
         exception
            when  others =>
               Ada.Text_IO.Put_Line ("an error occured .. ");
         end;
      end loop;
      Ada.Text_IO.Put_Line ("Force Turnout Test complete");
      Ada.Text_IO.New_Line;
   end Force_Turnout_Info;

   -----------------------------------------------------------------------------
--     procedure Is_Joint_Turnout is
--        Checker : Boolean;
--     begin
--        Ada.Text_IO.Put_Line ("Is_Joint_Turnout test begin");
--        Ada.Text_IO.Put_Line ("Testing");
--        for Index in 1 .. 40 loop
--  	 -- Normal direction blocks
--  	 Checker := Layout.Is_Joint_Turnout (A_Turnout  => Block_ID (Index),
--  				             A_Limb     => Normal);
--  	 if Index = 2 or Index = 18 or Index = 14 then
--              if not Checker then
--                 Ada.Text_IO.Put ("Block");
--  	       Ada.Integer_Text_IO.Put (Index, 3);
--  	       Ada.Text_IO.Put_Line (" failure normal direction");
--  	    end if;
--  	 else
--              if Checker then
--                 Ada.Text_IO.Put ("Block");
--  	       Ada.Integer_Text_IO.Put (Index, 3);
--  	       Ada.Text_IO.Put_Line (" failure normal direction");
--  	    end if;
--  	 end if;
--  	 -- reverse direction blocks
--  	 Checker := Layout.Is_Joint_Turnout (A_Block     => Block_ID (Index),
--  				             A_Direction => Reversed);
--  	 if Index = 2 or Index = 4 or Index = 17 then
--              if not Checker then
--                 Ada.Text_IO.Put ("Block");
--  	       Ada.Integer_Text_IO.Put (Index, 3);
--  	       Ada.Text_IO.Put_Line (" failure reversed direction");
--  	    end if;
--  	 else
--              if Checker then
--                 Ada.Text_IO.Put ("Block");
--  	       Ada.Integer_Text_IO.Put (Index, 3);
--  	       Ada.Text_IO.Put_Line (" failure reversed direction");
--  	    end if;
--  	 end if;
--        end loop;
--        Ada.Text_IO.Put_Line ("Is_Joint_Turnout test complete");
--        Ada.Text_IO.New_Line;
--     end Is_Joint_Turnout;

   -----------------------------------------------------------------------------
   procedure Get_Joint_Turnout is
      File          : Ada.Text_IO.File_Type;
      Turnout       : Layout.Turnout_ID;
      Expected      : Layout.Turnout_ID;
      Limb          : Layout.Turn_Choice;
      Turnout_Check : Layout.Turnout_ID;
   begin
      Ada.Text_IO.Put_Line ("Get Joint Turnout Test Begin");
      Ada.Text_IO.Put_Line ("Testing");
      Ada.Text_IO.Open (Name => Get_Joint_Turnout_File,
                        File => File,
                        Mode => Ada.Text_IO.In_File);
      while not Ada.Text_IO.End_Of_File (File) loop
         begin
            Ada.Text_IO.Skip_Line (File);
            Turnout_IO.Get (File  => File,
                            Item  => Turnout);
            Turnout_IO.Get (File  => File,
			    Item  => Expected);
	    Limb_IO.Get (File  => File,
		         Item  => Limb);

	    Turnout_Check := Layout.Get_Joint_Turnout (A_Turnout => Turnout,
						A_Limb    => Limb);

	    if Turnout_Check /= Expected then
	       Ada.Text_IO.Put ("Failure on Turnout: ");
	       Turnout_IO.Put (Turnout);
	       Ada.Text_IO.Put (", Expected: ");
	       Turnout_IO.Put (Expected);
	       Ada.Text_IO.New_Line;
	    end if;
	    Ada.Text_IO.Skip_Line (File);
         exception
            when Error : others =>
               Ada.Text_IO.Put ("Exception was raised:    ");
               Ada.Text_IO.Put (Ada.Exceptions.Exception_Name (Error) & " ");
               Ada.Text_IO.Put (Ada.Exceptions.Exception_Message (Error));
               Ada.Text_IO.New_Line;
         end;
      end loop;
      Ada.Text_IO.Put_Line ("Get Joint Turnout Test Complete");
      Ada.Text_IO.New_Line;
   end Get_Joint_Turnout;

   -----------------------------------------------------------------------------
   procedure Crossing_Blocks is
      Cross_Blocks : Cross_Blocks_Rec;
   begin
      Ada.Text_IO.Put_Line ("Cross blocks Test Begin");
      Ada.Text_IO.Put_Line ("Testing");
      for Index in 1 .. 40 loop
         Cross_Blocks :=
           Layout.Get_Crossing_Blocks (A_Block => Block_ID (Index));
         case Index is
            when 39 =>
               if Cross_Blocks.Length /= 1 or Cross_Blocks.List (1) /= 23 then
                  Ada.Text_IO.Put_Line ("failure on block 39");
               end if;
            when 23 =>
               if Cross_Blocks.Length /= 1 or Cross_Blocks.List (1) /= 39 then
                  Ada.Text_IO.Put_Line ("failure on block 23");
               end if;
            when 8 =>
               if Cross_Blocks.Length /= 1 or Cross_Blocks.List (1) /= 29 then
                  Ada.Text_IO.Put_Line ("failure on block 8");
               end if;
            when 29 =>
               if Cross_Blocks.Length /= 1 or Cross_Blocks.List (1) /= 8 then
                  Ada.Text_IO.Put_Line ("failure on block 29");
               end if;
            when 30 =>
               if Cross_Blocks.Length /= 1 or Cross_Blocks.List (1) /= 24 then
                  Ada.Text_IO.Put_Line ("failure on block 30");
               end if;
            when 24 =>
               if Cross_Blocks.Length /= 1 or Cross_Blocks.List (1) /= 30 then
                  Ada.Text_IO.Put_Line ("failure on block 24");
               end if;
            when others =>
               if Cross_Blocks.Length /= 0 then
                  Ada.Text_IO.Put ("failure on block ");
                  Ada.Integer_Text_IO.Put (Index);
                  Ada.Text_IO.New_Line;
               end if;
         end case;
      end loop;
      Ada.Text_IO.Put_Line ("Cross blocks test complete");
      Ada.Text_IO.New_Line;
   end Crossing_Blocks;

   -----------------------------------------------------------------------------
   procedure Next_Choice_Turnout is
      File          : Ada.Text_IO.File_Type;
      Direction     : Layout.Direction;
      Block	    : Layout.Block_ID;
      Turnout       : Layout.Turnout_ID;
      Turnout_Check : Layout.Turnout_ID;
   begin
      Ada.Text_IO.Put_Line ("Next Choice Test Begin");
      Ada.Text_IO.Put_Line ("Testing");
      Ada.Text_IO.Open (Name => Next_Choice_Turnout_File,
                        File => File,
                        Mode => Ada.Text_IO.In_File);
      while not Ada.Text_IO.End_Of_File (File) loop
         begin
            Block_IO.Get (File  => File,
                          Item  => Block);
            Direction_IO.Get (File  => File,
                              Item  => Direction);
            Turnout_IO.Get (File  => File,
                            Item  => Turnout);
            Turnout_Check := Layout.Next_Choice_Turnout (A_Block  => Block,
                                        	      A_Direction => Direction);
            if Turnout_Check /= Turnout then
               Ada.Text_IO.Put ("Block");
	       Block_IO.Put (Block, 2);
               Ada.Text_IO.Put_Line (" failed in the ");
               Direction_IO.Put (Direction);
               Ada.Text_IO.Put_Line (" Direction");
            end if;
            Ada.Text_IO.Skip_Line (File);
         exception
            when Error : others =>
               Ada.Text_IO.Put ("Exception was raised:    ");
               Ada.Text_IO.Put (Ada.Exceptions.Exception_Name (Error) & " ");
               Ada.Text_IO.Put (Ada.Exceptions.Exception_Message (Error));
               Ada.Text_IO.New_Line;
         end;
      end loop;
      Ada.Text_IO.Put_Line ("Next Choice Test Complete");
      Ada.Text_IO.New_Line;
   end Next_Choice_Turnout;


--------------------------------------------------------------------------------
begin
   Ada.Text_IO.Put_Line ("***** Begin Layout Testing  ****");
   End_Of_Block;
   Limb_Of_Turnout;
   Next_Block;
   Next_Turnout;
   Sensor_Between_Blocks;
   Sensor_Polarity;
   Opposite_Direction;
   Opposite_Turn;
   Blocks_Around_Sensor;
   Is_Force_Turnout;
   Force_Turnout_Info;
--     Is_Joint_Turnout;
   Get_Joint_Turnout;
   Crossing_Blocks;
   Next_Choice_Turnout;
   Ada.Text_IO.Put_Line ("$$$$$ TESTING COMPLETE! $$$$$$");
exception
   when Error : others =>
      Ada.Text_IO.Put ("Exception was raised:    ");
      Ada.Text_IO.Put (Ada.Exceptions.Exception_Name (Error) & " ");
      Ada.Text_IO.Put (Ada.Exceptions.Exception_Message (Error));
      Ada.Text_IO.New_Line;
end Test_Layout;
