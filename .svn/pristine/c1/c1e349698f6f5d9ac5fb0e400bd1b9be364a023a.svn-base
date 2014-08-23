with Blocks;
with Turnouts;
with Sound;
with Ada.Unchecked_Conversion;
with Cabs; use Cabs;
with Display; use Display;
with Double_Talk; use Double_Talk;
package body Trains.Functions is

   -- Assigned Cabs are the same as the Train_ID, example: train 1 = cab 1

   -- Assigned Horns are the same as the Train_ID
   function To_Sound is new Ada.Unchecked_Conversion
     (Source => Train_ID,
      Target => Sound.Installed_Range);

   -----------------------------------------------------------------------------

   protected body Train_Object is

      function Get_Status_Array return Stop_Rec is
      begin
         return Train_Stops;
      end Get_Status_Array;

      procedure Initialize_Train (Train     : in Train_ID;
                                  Loco      : in Block_ID;
                                  Caboose   : in Block_ID;
				  Direction : in Layout.Direction := Normal;
				  Min_Throttle : in Common_Units.Percent) is
         Success : Boolean;
         Turnouts : Layout.Search.Turnout_List (26);

      begin


         Number := Train;
         Min_Throttle_Num := Min_Throttle;
	 Display.Put (Train     => Number,
	       Direction => Train_Direction);
	 if Loco = Caboose then
	    Blocks.Power_Block (Block      => Loco,
			 Cab_Number => Cab_ID (Train),
			 Direction  => Direction);
	    -- add to array
	    Occupied_Blocks.Size := 1;
	    Occupied_Blocks.Items (1).Block := Loco;
	    -- set the current blocks direction
	    Occupied_Blocks.Items (1).My_Direction :=
	      Direction;
	    -- reserve
	    Blocks.Reserve_Block (Requestor => Number,
			   Block     => Occupied_Blocks.Items (1).Block,
			   Success   => Success);
	    Process_Reserve;
         else
            Layout.Search.Blocks_Beneath (Loco     => Loco,
                                          Caboose  => Caboose,
                                          Blocks   => Occupied_Blocks,
                                          Turnouts => Turnouts,
                                          Success  => Success);
            -- change the turnouts underneath the train
            for Index in Turnouts.Items'First .. Turnouts.Size loop
               Trains.Change_Turnout
                 (Requestor   => 0,
                  Turnout     => Turnouts.Items (Index).Turnout,
                  Turn_Choice => Turnouts.Items (Index).Direction);
            end loop;

            for Index in
              Occupied_Blocks.Items'First .. Occupied_Blocks.Size loop
               Blocks.Power_Block
                 (Block      => Occupied_Blocks.Items (Index).Block,
                  Cab_Number => Cab_ID (Number),
                  Direction  => Occupied_Blocks.Items (Index).My_Direction);
               Blocks.Reserve_Block
                 (Requestor => Number,
                  Block     => Occupied_Blocks.Items (Index).Block,
                  Success   => Success);
            end loop;
            Process_Reserve;
         end if;
      end Initialize_Train;

      --------------------------------------------------------------------------
      procedure Go is
         Okay_To_Go : Boolean;
      begin
         Okay_To_Go := True;
         for Index in Stop_Reason'Range loop
            if Train_Stops.Reasons (Index) then
               Okay_To_Go := False;
            end if;
         end loop;
         if Okay_To_Go then
            Halt_Status := False;
            Cabs.Set_Limit (Cab   => Cab_ID (Number),
			    Value => 100);
	    Blocks.Power_Block
	      (Block      => Occupied_Blocks.Items (Occupied_Blocks.Size).Block,
	Cab_Number => Cab_ID (Number),
	Direction  => Occupied_Blocks.Items (Occupied_Blocks.Size).My_Direction);
            Cabs.Set (Cab   => Cab_ID (Number),
		      Value => Previous_Cab_Value);
         end if;
      end Go;

      ----------------------------------------------------
      procedure Enable is
      begin
	 Train_Status := Enabled;
	 Cabs.Set_Limit (Cab   => Cab_ID (Number),
		  Value => 100);
	 Train_Stops.Reasons (Dispatcher_Request) := False;
	 for Index in
	   Occupied_Blocks.Items'First .. Occupied_Blocks.Size loop
	    Blocks.Power_Block
	      (Block      => Occupied_Blocks.Items (Index).Block,
	Cab_Number => Cab_ID (Number),
	Direction  => Occupied_Blocks.Items (Index).My_Direction);
	 end loop;
	 Go;
      end Enable;

      ---------------------------------------------------
      procedure Disable is
      begin
         Train_Status := Disabled;
         Cabs.Set_Limit (Cab   => Cab_ID (Number),
                         Value => 0);
         Train_Stops.Reasons (Dispatcher_Request) := True;
      end Disable;

      ----------------------------------------------------
      procedure Emergency_Stop is
      begin
         Train_Stops.Reasons (Dispatcher_Request) := True;
         Halt_Status := True;
         Cabs.Set_Limit (Cab   => Cab_ID (Number),
                         Value => 0);
      end Emergency_Stop;

      ----------------------------------------------------
      procedure Turnout_Failure (Turnout : in Layout.Turnout_ID) is

      begin
         Train_Stops.Reasons (Turnout_Failure) := True;
         Halt_Status := True;
         Cabs.Set_Limit (Cab   => Cab_ID (Number),
                         Value => 0);
         Train_Stops.Turnouts (Turnout) := True;
      end Turnout_Failure;

      ----------------------------------------------------
      procedure Block_Allocation_Failure (Block : Block_ID) is
      begin
	 Double_Talk.Speak
	   (Phrase => Phrase_Strings.To_Bounded_String
            ("Train " & Train_ID'Image (Number) & " failed to reserve a block"),
            Voice  => Paul);
         Train_Stops.Reasons (Reservation_Failure) := True;
         Halt_Status := True;
         Train_Stops.Block := Block;
         Cabs.Set_Limit (Cab   =>  Cab_ID (Number),
                         Value => 0);
      end Block_Allocation_Failure;

      ----------------------------------------------------
      procedure Rolling_Stock is
      begin
         Train_Stops.Reasons (Lost_Caboose) := True;
	 Emergency_Stop;
      end Rolling_Stock;

      ----------------------------------------------------
      procedure Emergency_Stop_Ready is
      begin
         Train_Stops.Reasons (Dispatcher_Request) := False;
         Go;
      end Emergency_Stop_Ready;

      ----------------------------------------------------
      procedure Turnout_Failure_Ready (Turnout : in Layout.Turnout_ID) is
         Okay_To_Go : Boolean;
      begin
	 if Train_Stops.Reasons (Turnout_Failure) then
            Okay_To_Go := True;
            Train_Stops.Turnouts (Turnout) := False;
            for Index in Train_Stops.Turnouts'Range loop
               if Train_Stops.Turnouts (Index) then
                  Okay_To_Go := False;
               end if;
            end loop;
            if Okay_To_Go then
               Train_Stops.Reasons (Turnout_Failure) := False;
               Go;
            end if;
         end if;
      end Turnout_Failure_Ready;

      ----------------------------------------------------
      procedure Block_Allocation_Ready (Block : in Layout.Block_ID) is
      begin
	 if Train_Stops.Block = Block then
	    Train_Stops.Reasons (Reservation_Failure) := False;
	    Blocks.Power_Block
	      (Block      =>
	  Occupied_Blocks.Items (Occupied_Blocks.Size).Block,
	Cab_Number => Cab_ID (Number),
	Direction  =>
	  Occupied_Blocks.Items (Occupied_Blocks.Size).My_Direction);
            Process_Reserve;
            Go;
         end if;
      end Block_Allocation_Ready;

      ----------------------------------------------------
      procedure Rolling_Stock_Ready is
      begin
         Train_Stops.Reasons (Lost_Caboose) := False;
      end Rolling_Stock_Ready;

      ----------------------------------------------------
      procedure Process_Reserve is
	 A_Turnout         : Layout.Turnout_ID;
	 Force_Turnout     : Layout.Turnout_ID;
	 Force_Direction   : Layout.Turn_Choice;
         Turnout_Direction : Layout.Turn_Choice;
         Success           : Boolean;
         Current_Block     : constant Layout.Block_ID :=
           Occupied_Blocks.Items (Occupied_Blocks.Size).Block;
         Direction         : constant Layout.Direction :=
           Occupied_Blocks.Items (Occupied_Blocks.Size).My_Direction;
      begin
         -- attempt to reserve next block
         -- if turnout then reserve limb
         if Layout.End_Of_Block (Block => Current_Block,
                                 Side  => Direction) = Turnout then
            A_Turnout := Layout.Next_Turnout (Block => Current_Block,
                                              Side  => Direction);
            Turnout_Direction :=
              Turnouts.Direction_Of (Turnout => A_Turnout);
            Train_Stops.Block :=
              Layout.Limb_Of_Turnout (Turnout => A_Turnout,
                                      Limb    => Turnout_Direction);
         else -- its a block, simply reserve block
            Train_Stops.Block := Layout.Next_Block (Block => Current_Block,
                                                    Side  => Direction);
         end if;
         Blocks.Reserve_Block (Requestor => Number,
                               Block     => Train_Stops.Block,
                               Success   => Success);
         if Success then
            -- Check for force turnouts
            if Layout.Is_Force_Turnout (A_Block     => Current_Block,
					A_Direction => Direction) then

	       Layout.Get_Force_Turnout_Info
		 (A_Block         => Current_Block,
                  Block_Direction => Direction,
                  A_Turnout       => Force_Turnout,
                  Set_Turnout     => Force_Direction);
	       if Layout.End_Of_Block (Block => Current_Block,
                                 Side  => Direction) = Turnout then
		  -- check if its from a joint turnout
		  if Layout.Is_Joint_Turnout (A_Turnout => A_Turnout,
				A_Limb    => Turnout_Direction) then

		     Change_Turnout (Requestor   => Number,
		       Turnout     => Force_Turnout,
		       Turn_Choice => Force_Direction);
		  end if;
	       else
		  Change_Turnout (Requestor   => Number,
		    Turnout     => Force_Turnout,
		       Turn_Choice => Force_Direction);
	       end if;
	    end if;
         else -- we did not reserve the block
            Block_Allocation_Failure (Block => Train_Stops.Block);
         end if;
      end Process_Reserve;

      ----------------------------------------------------
      procedure Toggle_Direction is
	 Occupied_Copy : Layout.Search.Block_List (Max_Length_Running + 1);
      begin
	 -- traverse through blocks occupied and reverse,
	 --   first until last block
	 for Index in
	   Occupied_Blocks.Items'First .. Occupied_Blocks.Size loop
	    Occupied_Blocks.Items (Index).My_Direction :=
	      Layout.Opposite (Occupied_Blocks.Items (Index).My_Direction);
	    Blocks.Power_Block
	      (Block      => Occupied_Blocks.Items (Index).Block,
	       Cab_Number => Cab_ID (Number),
	       Direction  => Occupied_Blocks.Items (Index).My_Direction);
	 end loop;

	 Blocks.Free_Block (Requestor => Number,
		     Block     => Train_Stops.Block);

	 -- reverse occupied array
	 for Index in Occupied_Blocks.Items'First .. Occupied_Blocks.Size  loop
	    Occupied_Copy.Items (Index) :=
	      Occupied_Blocks.Items (Occupied_Blocks.Size - (Index - 1));
	 end loop;
	 Occupied_Blocks.Items := Occupied_Copy.Items;
	 -- reverse train direction
	 if Train_Direction = Forward then
	    Train_Direction := Backward;
	    Sound.Bell_On (Unit => Sound.Installed_Range (Number));
	 else
	    Train_Direction := Forward;
	    Sound.Bell_Off (Unit => Sound.Installed_Range (Number));
	 end if;
	 Display.Put (Train     => Number,
	       Direction => Train_Direction);
	 -- switch reserve
	 Train_Stops.Reasons (Reservation_Failure) := False;
	 Go;
      end Toggle_Direction;

      ----------------------------------------------------
      procedure Set_Throttle (Amount : in Common_Units.Percent) is
         Smart_Throttle : Common_Units.Percent;
      begin
         if not (Train_Status = Disabled or Halt_Status) then
            Cabs.Set_Limit (Cab   => Cab_ID (Number),
                            Value => 100);
            if Amount <= 10 then
               Smart_Throttle := (Min_Throttle_Num / 10) * Amount;
            else
               Smart_Throttle :=
			Min_Throttle_Num +
                   (((((100 - Min_Throttle_Num) * 100) / 90) *
                    (Amount - 10)) / 100);
            end if;
            Cabs.Set (Cab   => Cab_ID (Number),
                      Value => Smart_Throttle);
         end if;

         Previous_Cab_Value := Smart_Throttle;
      end Set_Throttle;

      ----------------------------------------------------
      procedure Sound_Horn is
         Cab_Value : Common_Units.Percent;
      begin
         if not (Train_Status = Disabled or Halt_Status) then
            Cabs.Get (Cab   => Cab_ID (Number),
                      Value => Cab_Value);
            if Cab_Value > 0 then
               Sound.Sound_Horn (Unit   => To_Sound (Number),
                                 Signal => Sound.Approach_Highway);
            else
               Sound.Sound_Horn (Unit   => To_Sound (Number),
                                 Signal => Sound.Start);
            end if;
         end if;
      end Sound_Horn;

      ----------------------------------------------------
      procedure Process_Front_Sensor (Hall_Sensor : in Layout.Hall_Sensor;
                                      Direction   : in Layout.Direction) is
         Correct_Direction : Layout.Direction;
      begin
	 Display.Put_Error (Error_Message => "Process_Front_Sensor" &
		       Block_ID'Image (Train_Stops.Block));
         -- find direction to power
         if Layout.Sensor_Polarity (Hall_ID => Hall_Sensor) then
            Correct_Direction := Direction;
         else
            Correct_Direction := Layout.Opposite (Direction);
	 end if;
	 Blocks.Power_Block (Block      => Train_Stops.Block,
		             Cab_Number => Cab_ID (Number),
		      Direction  => Correct_Direction);
	 -- add to array
	 Occupied_Blocks.Size := Occupied_Blocks.Size + 1;
	 Occupied_Blocks.Items (Occupied_Blocks.Size).Block :=
	   Train_Stops.Block;
	 -- set the current blocks direction
	 Occupied_Blocks.Items (Occupied_Blocks.Size).My_Direction :=
           Correct_Direction;
         -- Reserve the next
         Process_Reserve;
         -- Check for Rolling Stock
         if Occupied_Blocks.Size = Trains.Max_Length_Running then
            Rolling_Stock (Train => Number);
         end if;
	 Blocks.Power_Block (Block      => Train_Stops.Block,
		             Cab_Number => 0,
		      Direction  => Correct_Direction);
      end Process_Front_Sensor;

      ----------------------------------------------------
      procedure Process_Rear_Sensor (Block_1     : in Layout.Block_ID;
                                     Block_2     : in Layout.Block_ID) is

         Block_To_Free        : Layout.Block_ID;
      begin
         Display.Put_Error (Error_Message =>
                              "Process_Rear_Sensor, from Train");

	 -- Find out which block to free
	 if Block_1 = Occupied_Blocks.Items (1).Block then
	    Block_To_Free := Block_1;
	 elsif Block_2 = Occupied_Blocks.Items (1).Block then
	    Block_To_Free := Block_2;
	 else
	    Display.Put_Error (Error_Message =>
			  "Failed to Free back of train");
	 end if;

         Blocks.Free_Block (Requestor => Number,
                            Block     => Block_To_Free);
         -- shift array back 1
         for Index in
           Occupied_Blocks.Items'First + 1 .. Occupied_Blocks.Size loop
            Occupied_Blocks.Items (Index - 1) := Occupied_Blocks.Items (Index);
         end loop;
	 Occupied_Blocks.Size := Occupied_Blocks.Size - 1;
	 if Occupied_Blocks.Size <= Trains.Max_Length_Running then
	    Rolling_Stock_Ready;
	 end if;
      end Process_Rear_Sensor;

      --------------------------------------------------------------------------
      procedure Change_Next_Choice_Turnout (Turn : in Layout.Turn_Choice) is
	 Turnout : Layout.Turnout_ID;
      begin
	 if Train_Status = Enabled then
	    Turnout := Layout.Next_Choice_Turnout
	      (A_Block     =>
	  Occupied_Blocks.Items (Occupied_Blocks.Size).Block,
	A_Direction =>
	  Occupied_Blocks.Items (Occupied_Blocks.Size).My_Direction);
	    if Turnouts.Direction_Of (Turnout => Turnout) /= Turn then
	       Change_Turnout (Requestor   => Number,
		    Turnout     => Turnout,
			Turn_Choice => Turn);
	       if Occupied_Blocks.Items (Occupied_Blocks.Size).Block =
		 Layout.Limb_Of_Turnout (Turnout => Turnout,
			   Limb    => Common) then
		  Blocks.Free_Block (Requestor => Number,
		      Block     => Train_Stops.Block);
	       end if;
	    end if;

	    Train_Stops.Reasons (Reservation_Failure) := False;
	    Go;
	 else
	    Double_Talk.Speak
	   (Phrase => Phrase_Strings.To_Bounded_String
       ("Can not change turnout train " & Train_ID'Image (Number) &
	  " is disabled "),
     Voice  => Paul);
         end if;
      end Change_Next_Choice_Turnout;

      ----------------------------------------------------
      function Get_Throttle return Common_Units.Percent is
         Value : Common_Units.Percent;
      begin
         Cabs.Get (Cab   => Cab_ID (Number),
                   Value => Value);
         return Value;
      end Get_Throttle;

      ----------------------------------------------------
      procedure Hall_Sensor_Triggered (Hall_Sensor : in Layout.Hall_Sensor) is
         Block_1     : Layout.Block_ID;
         Block_2     : Layout.Block_ID;
         Cab_1       : Cabs.Cab_ID;
         Cab_2       : Cabs.Cab_ID;
         Direction_1 : Layout.Direction;
         Direction_2 : Layout.Direction;
      begin
	 -- find front or back of the train

         Layout.Blocks_Around_Sensor (A_Hall_Sensor => Hall_Sensor,
                                      Block_1       => Block_1,
                                      Block_2       => Block_2);

         Blocks.Get_Block_Info (Block      => Block_1,
                                Cab_Number => Cab_1,
                                Direction  => Direction_1);
         Blocks.Get_Block_Info (Block      => Block_2,
                                Cab_Number => Cab_2,
				Direction  => Direction_2);

	 if Cab_1 = 0 or Cab_1 = 7 then
            Process_Front_Sensor (Hall_Sensor => Hall_Sensor,
                                  Direction   => Direction_2);
         elsif Cab_2 = 0 or Cab_2 = 7 then
            Process_Front_Sensor (Hall_Sensor => Hall_Sensor,
                                  Direction   => Direction_1);
         else
            -- back of train sensor was triggered
            Process_Rear_Sensor (Block_1     => Block_1,
                                 Block_2     => Block_2);
         end if;
      end Hall_Sensor_Triggered;

      -----------------------------------------------------------
      procedure Blocks_Occupied (Blocks : out Layout.Block_Array;
                                 Size   : out Positive) is
         New_Block_Array : Layout.Block_Array (1 .. 6);
      begin
         for Index in 1 .. Occupied_Blocks.Size loop
            New_Block_Array (Index) :=
              Occupied_Blocks.Items (Index).Block;
         end loop;
         Blocks := New_Block_Array;
         Size := Occupied_Blocks.Size;
      end Blocks_Occupied;
   end Train_Object;

end Trains.Functions;
