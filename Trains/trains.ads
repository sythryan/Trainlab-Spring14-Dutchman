with Common_Units; use Common_Units;
with Layout; use Layout;
with Train_Types; use Train_Types;
package Trains is
   -- Author : Syth Ryan
   -- Team   : The Grounded Dutchman

   type Direction_Type is (Backward, Forward);   -- Direction of train travel

   Max_Length_Start   : constant := 3;   -- Max number of blocks beneath a train
   Max_Length_Running : constant := 5;   -- at startup and running

   -----------------------------------------------------------------------------

   -- Types for keeping track of train stops

   type Stop_Reason is (Dispatcher_Request,  Turnout_Failure,
                        Reservation_Failure, Lost_Caboose);
   type Stop_Set    is array (Stop_Reason) of Boolean;
   type Turnout_Set is array (Layout.Turnout_ID) of Boolean;
   type Stop_Rec is
      record
         Reasons   : Stop_Set;
         Block     : Layout.Block_ID;
         Turnouts  : Turnout_Set;
      end record;

   -----------------------------------------------------------------------------
   type Block_Change_Ptr is access procedure
     (Train   : in Train_Types.Train_ID);
   procedure Set_Blocks_Changed_Callback (To : in Block_Change_Ptr);

   -- This procedure sets the procedure that the Train package will call
   --   when a blocks occupied is changed

   -----------------------------------------------------------------------------

   type Status_Change_Ptr is access procedure (Train  : in Train_Types.Train_ID;
                  			       Status : in Trains.Stop_Rec);

   procedure Set_Status_Changed_Callback (To : in Status_Change_Ptr);

   -- This procedure sets the procedure that the Train package will call
   --   when a halt Status is changed

   -----------------------------------------------------------------------------
   function Get_Status_Array (Train : in Train_ID) return Stop_Rec;
   -- returns and halts put on the train

   -----------------------------------------------------------------------------
   procedure Initialize_Train (Train   : in Train_ID;
                               Loco    : in Block_ID;
			       Caboose : in Block_ID;
                               Direction : in Layout.Direction := Normal;
                               Min_Throttle : in Percent);
   -- Initialize a Train by providing it the loco and caboose locations
   -- Direction is an optional param, only use when loco = caboose

   -----------------------------------------------------------------------------
   procedure Enable (Train : in Train_ID);
   -- Enables the specified Train

   -----------------------------------------------------------------------------
   procedure Disable (Train : in Train_ID);
   -- Disables the specified Train

   -----------------------------------------------------------------------------
   procedure Emergency_Stop (Train : in Train_ID);
   -- Halts the train using an emergency stop

   -----------------------------------------------------------------------------
   procedure Rolling_Stock (Train : in Train_ID);
   -- Halts the train from a lost or rolling stock

   -----------------------------------------------------------------------------
   procedure Emergency_Stop_Ready (Train : in Train_ID);
   -- The train is ready from an emergency stop

   -----------------------------------------------------------------------------
   procedure Rolling_Stock_Ready (Train : in Train_ID);
   -- The train is ready from a lost or rolling stock

   -----------------------------------------------------------------------------
   procedure Toggle_Direction (Train : in Train_ID);
   -- Changes the direction of the specified Train
   -- Preconditons: The Train must be stopped for 3 seconds

   -----------------------------------------------------------------------------
   procedure Set_Throttle (Train  : in Train_ID;
                           Amount : in Percent);
   -- Sets the Throttle of the Specified Train

   -----------------------------------------------------------------------------
   procedure Change_Next_Choice_Turnout (Train : in Train_ID;
					 Direction : in Layout.Turn_Choice);
   -- Changes the next choice turnout of the specified Train in
   --    the direction of Turn_Choice

   -----------------------------------------------------------------------------
   procedure Change_Turnout (Requestor   : in Request_ID;
			     Turnout     : in Layout.Turnout_ID;
                             Turn_Choice : in Layout.Turn_Choice);
   -- Changes the specified turnout to Turn_Choice

   -----------------------------------------------------------------------------
   procedure Sound_Horn (Train : in Train_ID);
   -- Sound the horn of the specified Train

   -----------------------------------------------------------------------------
   procedure Blocks_On (Train  : in Train_ID;
			Blocks : out Layout.Block_Array;
		        Size   : out Positive);
   -- Returns a block List of the blocks that the specified train is currently
   --     occupying.

   -----------------------------------------------------------------------------
   function Get_Throttle (Train : in Train_ID) return Percent;

   -- returns the throttle of the specified Train

   -----------------------------------------------------------------------------
   procedure Hall_Sensor_Triggered (Hall_Sensor : in Layout.Hall_Sensor);
   -- for hall enable

end Trains;
