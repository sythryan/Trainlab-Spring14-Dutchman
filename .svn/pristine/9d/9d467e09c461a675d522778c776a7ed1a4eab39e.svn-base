with Layout;
with Layout.Search;
package Trains.Functions is
   -- Author : Syth Ryan
   -- Team   : The Grounded Dutchman
   type Status_Type is (Enabled, Disabled);


   -----------------------------------------------------------------------------
   protected type Train_Object is
      procedure Initialize_Train (Train     : in Train_ID;
				  Loco      : in Block_ID;
				  Caboose   : in Block_ID;
				  Direction : in Layout.Direction := Normal;
				  Min_Throttle : in Percent);
      procedure Go;
      procedure Enable;
      procedure Disable;
      procedure Emergency_Stop;
      procedure Turnout_Failure (Turnout   : in Layout.Turnout_ID);
      procedure Block_Allocation_Failure (Block : Block_ID);
      procedure Rolling_Stock;
      procedure Emergency_Stop_Ready;
      procedure Turnout_Failure_Ready (Turnout : in Layout.Turnout_ID);
      procedure Block_Allocation_Ready (Block : in Layout.Block_ID);
      procedure Rolling_Stock_Ready;
      procedure Process_Reserve;
      procedure Toggle_Direction;
      procedure Set_Throttle (Amount : Common_Units.Percent);
      procedure Sound_Horn;
      procedure Change_Next_Choice_Turnout (Turn : in Layout.Turn_Choice);
      procedure Hall_Sensor_Triggered (Hall_Sensor : in Layout.Hall_Sensor);
      procedure Blocks_Occupied (Blocks : out Layout.Block_Array;
                                 Size   : out Positive);
      function Get_Status_Array return Stop_Rec;
      function Get_Throttle return Common_Units.Percent;
   private
      Train_Direction   : Trains.Direction_Type := Forward;
      Previous_Cab_Value : Common_Units.Percent;
      Number             : Train_ID;
      Min_Throttle_Num : Common_Units.Percent;
      Occupied_Blocks : Layout.Search.Block_List (Max_Length_Running + 1);
      Halt_Status     : Boolean := False;
      Train_Status    : Status_Type := Disabled;
      Train_Stops     : Stop_Rec  := (Reasons  => (others => False),
				      Block    => 40,
				      Turnouts => (others => False));
   end Train_Object;

   -----------------------------------------------------------------------------
end Trains.Functions;

