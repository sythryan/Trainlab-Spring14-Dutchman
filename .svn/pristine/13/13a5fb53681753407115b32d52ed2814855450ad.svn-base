with Cabs; use Cabs;
with Turnouts;
with Blocks;
with Trains.Functions;
with Halls; use Halls;
with Display; use Display;
with Double_Talk; use Double_Talk;
package body Trains is

   -- Author : Syth Ryan
   -- Team   : The Grounded Dutchman

   -----------------------------------------------------------------------------
   type Train_Array is array (Train_ID) of Trains.Functions.Train_Object;

   Trains : Train_Array;

   -----------------------------------------------------------------------------
   Blocks_Changed_Callback : Block_Change_Ptr;

   procedure Set_Blocks_Changed_Callback (To : in Block_Change_Ptr) is
   begin
      Blocks_Changed_Callback := To;
   end Set_Blocks_Changed_Callback;

   -----------------------------------------------------------------------------
   Status_Changed_Callback : Status_Change_Ptr;

   procedure Set_Status_Changed_Callback (To : in Status_Change_Ptr) is
   begin
      Status_Changed_Callback := To;
   end Set_Status_Changed_Callback;

   -----------------------------------------------------------------------------
   procedure Block_Failure_Ready (Block : in Layout.Block_ID) is
   begin
      for Index in Train_ID'Range loop
         Trains (Index).Block_Allocation_Ready (Block);
      end loop;
   end Block_Failure_Ready;

   -----------------------------------------------------------------------------
   function Get_Status_Array (Train : in Train_ID) return Stop_Rec is
   begin
      return Trains (Train).Get_Status_Array;
   end Get_Status_Array;

   ----------------------
   -- Initialize_Train --
   ----------------------
   procedure Initialize_Train (Train   : in Train_ID;
                               Loco    : in Block_ID;
			       Caboose : in Block_ID;
                               Direction : in Layout.Direction := Normal;
			       Min_Throttle : in Common_Units.Percent) is
   begin
      Display.Put_Error (Error_Message =>
                                    "Initialize_Train, from Train");
      Trains (Train).Initialize_Train (Train     => Train,
				       Loco      => Loco,
				       Caboose   => Caboose,
				       Direction => Direction,
				       Min_Throttle => Min_Throttle);
      Trains (Train).Emergency_Stop;
   end Initialize_Train;

   ------------
   -- Enable --
   ------------

   procedure Enable (Train : in Train_ID) is
   begin
      Status_Changed_Callback.all (Train,
                                   Trains (Train).Get_Status_Array);
      Trains (Train).Enable;
      Status_Changed_Callback.all (Train,
                                   Trains (Train).Get_Status_Array);
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (Train : in Train_ID) is
   begin
      Trains (Train).Disable;
      Status_Changed_Callback.all (Train,
                                   Trains (Train).Get_Status_Array);
   end Disable;

   --------------------
   -- Emergency_Stop --
   --------------------

   procedure Emergency_Stop (Train : in Train_ID) is
   begin
      Trains (Train).Emergency_Stop;
      Status_Changed_Callback.all (Train,
                                   Trains (Train).Get_Status_Array);
   end Emergency_Stop;

   ---------------------
   -- Turnout_Failure --
   ---------------------

   procedure Turnout_Failure (Requestor : in Request_ID;
			      Turnout   : in Layout.Turnout_ID) is
   begin
      if Requestor = 0 then
	 for Index in Train_ID'Range loop
	    Emergency_Stop (Train => Index);
	 end loop;
      else
	 Speak (Phrase => Phrase_Strings.To_Bounded_String
	        ("Turnout" & Layout.Turnout_ID'Image (Turnout) &
	         " failed for train " & Request_ID'Image (Requestor)),
	        Voice  => Paul);
         Trains (Requestor).Turnout_Failure (Turnout);
         Status_Changed_Callback.all (Requestor,
                                   Trains (Requestor).Get_Status_Array);
      end if;
   end Turnout_Failure;

   -------------------
   -- Rolling_Stock --
   -------------------

   procedure Rolling_Stock (Train : in Train_ID) is
   begin
      Speak (Phrase => Phrase_Strings.To_Bounded_String
	     ("Train " & Train_ID'Image (Train) & " lost caboose"),
	     Voice  => Paul);
      Trains (Train).Rolling_Stock;
      Status_Changed_Callback.all (Train,
                                   Trains (Train).Get_Status_Array);
   end Rolling_Stock;

   --------------------------
   -- Emergency_Stop_Ready --
   --------------------------

   procedure Emergency_Stop_Ready (Train : in Train_ID) is
   begin
      Speak (Phrase => Phrase_Strings.To_Bounded_String
	     ("Train " & Train_ID'Image (Train) & " ready to go"),
             Voice  => Paul);
      Trains (Train).Emergency_Stop_Ready;
      Status_Changed_Callback.all (Train,
                                   Trains (Train).Get_Status_Array);
   end Emergency_Stop_Ready;

   ---------------------------
   -- Turnout_Failure_Ready --
   ---------------------------

   procedure Turnout_Failure_Ready (Turnout : in Layout.Turnout_ID) is
   begin
      for Index in Train_ID'Range loop
         Trains (Index).Turnout_Failure_Ready (Turnout => Turnout);
         Status_Changed_Callback.all (Index,
                                   Trains (Index).Get_Status_Array);
      end loop;
   end Turnout_Failure_Ready;

   -------------------------
   -- Rolling_Stock_Ready --
   -------------------------

   procedure Rolling_Stock_Ready (Train : in Train_ID) is
   begin
      Trains (Train).Rolling_Stock_Ready;
      Status_Changed_Callback.all (Train,
                                   Trains (Train).Get_Status_Array);
   end Rolling_Stock_Ready;

   ----------------------
   -- Toggle_Direction --
   ----------------------

   procedure Toggle_Direction (Train : in Train_ID) is
   begin
      Trains (Train).Toggle_Direction;
      Status_Changed_Callback.all (Train,
                                   Trains (Train).Get_Status_Array);
   end Toggle_Direction;

   ------------------
   -- Set_Throttle --
   ------------------

   procedure Set_Throttle (Train  : in Train_ID;
			   Amount : in Common_Units.Percent) is
   begin
      Display.Put (Train    => Train,
                   Throttle => Amount);
      Trains (Train).Set_Throttle (Amount);
   end Set_Throttle;

   --------------------------------
   -- Change_Next_Choice_Turnout --
   --------------------------------
   procedure Change_Next_Choice_Turnout (Train     : in Train_ID;
					 Direction : in Layout.Turn_Choice) is
   begin
      Trains (Train).Change_Next_Choice_Turnout (Turn => Direction);
   end Change_Next_Choice_Turnout;

   --------------------
   -- Change_Turnout --
   --------------------
   procedure Change_Turnout (Requestor   : in Request_ID;
			     Turnout     : in Layout.Turnout_ID;
			     Turn_Choice : in Layout.Turn_Choice) is
   begin
      if Turnouts.Direction_Of (Turnout => Turnout) /= Turn_Choice then
	 Turnouts.Set (Requestor => Requestor,
		       Turnout   => Turnout,
		Direction => Turn_Choice);
      end if;
   end Change_Turnout;

   ----------------
   -- Sound_Horn --
   ----------------

   procedure Sound_Horn (Train : in Train_ID) is
   begin
      Trains (Train).Sound_Horn;
   end Sound_Horn;

   ----------------
   -- Blocks_On  --
   ----------------
   procedure Blocks_On (Train  : in Train_ID;
			Blocks : out Layout.Block_Array;
		        Size   : out Positive) is
   begin
      Trains (Train).Blocks_Occupied (Blocks => Blocks,
				      Size   => Size);
   end Blocks_On;

   -----------------------------------------------------------------------------
   function Get_Throttle (Train : in Train_ID) return Common_Units.Percent is
   begin
      return Trains (Train).Get_Throttle;
   end Get_Throttle;

   -----------------------------------------------------------------------------
   procedure Hall_Sensor_Triggered (Hall_Sensor : in Layout.Hall_Sensor) is
      Block_1 : Layout.Block_ID;
      Block_2 : Layout.Block_ID;
      Cab_1   : Cabs.Cab_ID;
      Cab_2   : Cabs.Cab_ID;
      Direction_1 : Layout.Direction;
      Direction_2 : Layout.Direction;
      Train       : Train_ID;
   begin
      -- Find out What Train
      Layout.Blocks_Around_Sensor (A_Hall_Sensor => Hall_Sensor,
				   Block_1       => Block_1,
				   Block_2       => Block_2);
      Blocks.Get_Block_Info (Block      => Block_1,
                             Cab_Number => Cab_1,
			     Direction  => Direction_1);
      Blocks.Get_Block_Info (Block      => Block_2,
                             Cab_Number => Cab_2,
			     Direction  => Direction_2);

      if Cab_1 = 0 and Cab_2 = 0 then
	 Double_Talk.Speak (Phrase =>
		       Double_Talk.Phrase_Strings.To_Bounded_String
			 ("Hall sensor " &
			    Layout.Hall_Sensor'Image (Hall_Sensor) &
			  " triggered without train nearby"),
		     Voice  => Paul);
	 return;
      end if;

      for Index in Train_ID'Range loop
         if Cab_1 = Cab_ID (Index) or Cab_2 = Cab_ID (Index) then
	    Train := Index;
            exit;
         end if;
      end loop;
      Trains (Train).Hall_Sensor_Triggered (Hall_Sensor);
      Blocks_Changed_Callback.all (Train);
      Status_Changed_Callback.all (Train,
                                   Trains (Train).Get_Status_Array);
   end Hall_Sensor_Triggered;

   ------------------------------------------------------------------------

begin
   Halls.Initialize;
   Blocks.Set_Block_Free_Callback (To => Block_Failure_Ready'Access);
   Turnouts.Set_Failure_Callback (To => Turnout_Failure'Access);
   Turnouts.Set_Recovery_Callback (To => Turnout_Failure_Ready'Access);
end Trains;
