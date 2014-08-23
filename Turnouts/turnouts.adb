-- Author: Nigel
-- Team  : The Grounded Dutchman

with Motors; use Motors;
with Layout; use Layout;
with Ada.Real_Time; use Ada.Real_Time;
with Double_Talk;
package body Turnouts is

   --At least need protected status.

   Change_Callback  : Change_Ptr;
   Recover_Callback : Recover_Ptr;
   Failure_Callback : Failure_Ptr;


   task type Turnout_Control_Task is
      entry Assign_Unit 	(Unit : Turnout_ID);
      entry Move_Right	(Requestor : Train_Types.Request_ID);
      entry Move_Left	(Requestor : Train_Types.Request_ID);
   end Turnout_Control_Task;

   type Turnout_Task_State is (Set_Left, Moving_Right, Set_Right, Moving_Left);


   type Task_Array is array (Turnout_ID) of Turnout_Control_Task;
   Turnout_Tasks                  : Task_Array;
   -----------------------------------------------------------------------------

   type Turnout_Array is array (Turnout_ID) of Status_Rec;
   Turnouts : Turnout_Array;

   -----------------------------------------------------------------------------
   procedure Set_Failure_Callback (To : in Failure_Ptr) is
   begin
      Failure_Callback := To;
   end Set_Failure_Callback;

   -----------------------------------------------------------------------------
   procedure Set_Recovery_Callback (To : in Recover_Ptr) is
   begin
      Recover_Callback := To;
   end Set_Recovery_Callback;

   -----------------------------------------------------------------------------
   procedure Set_Change_Callback (To : in Change_Ptr) is

   begin
      Change_Callback := To;
   end Set_Change_Callback;

   -----------------------------------------------------------------------------
   procedure Set (Requestor : in Train_Types.Request_ID;
                  Turnout   : in Layout.Turnout_ID;
                  Direction : in Layout.Turn_Choice) is

   begin
      if Direction = Left then
         Turnout_Tasks (Turnout).Move_Left (Requestor => Requestor);
      elsif Direction = Right then
         Turnout_Tasks (Turnout).Move_Right (Requestor => Requestor);
      end if;
   end Set;

   ----------------------------------------------------------------------------

   task body Turnout_Control_Task is
      My_ID             : Turnout_ID;
      My_Requestor      : Train_Types.Request_ID;
      My_Time_Delay	    : Ada.Real_Time.Time;
      Turnout_Failed    : Boolean := False;
      Turnout_State	    : Turnout_Task_State := Set_Left;


   begin

      accept Assign_Unit (Unit : Turnout_ID) do
	 My_ID := Unit;
      end Assign_Unit;

      loop    --Loop starts here because you only need to assign Unit_ID once
	 case Turnout_State is
	    when Set_Left =>
	       Turnouts (My_ID).Current := Left;
	       Turnouts (My_ID).Moving := False; --Sets Record to not moving

	       if Change_Callback /= null then
		  --Callback with not Moving
		  Change_Callback.all (Turnout   => My_ID,
			 Direction => Left,
			 Moving    => False);
	       end if;

	       select
		  accept Move_Right (Requestor : Train_Types.Request_ID) do
		     My_Requestor := Requestor;
		  end Move_Right;
		  --Sets new desired direction
		  Turnouts (My_ID).Desired := Right;
		  Turnout_State := Moving_Right; --Change State
		  --Set timeout for Moving Right
		  My_Time_Delay := Clock;
		  My_Time_Delay := My_Time_Delay + Time_Limit;
	       or
		  accept Move_Left (Requestor : Train_Types.Request_ID) do
		     My_Requestor := Requestor;
		  end Move_Left;
	       end select;

	    when Moving_Right =>


	       Motors.Set (Motor     => My_ID,
		    --Change Turnout Direction
		    Direction => Right);
	       Turnouts (My_ID).Moving := True; --Sets Record to moving
	       if Change_Callback /= null then
		  --Callback with Moving stats
		  Change_Callback.all (Turnout   => My_ID,
			 Direction => Right,
			 Moving    => True);
	       end if;

	       select
		  accept Move_Left (Requestor : Train_Types.Request_ID) do
		     My_Requestor := Requestor;
		  end Move_Left;
		  -- Do Nothing Here

	       or

		  delay until My_Time_Delay; --wait 3 for movment

		  if Motors.In_Position (Motor => My_ID) and
		    Turnouts (My_ID).Desired = Right then
		     --check if turnout is in postion and in desired state then
		     --change state to Set_Right

		     if Turnout_Failed then
			--If turnout did fail send recovery callback
			if Recover_Callback /= null then
			   Recover_Callback.all (My_ID);
			end if;
			Turnout_Failed := False;
		     end if;
		     Turnout_State := Set_Right; --Change State

		  else
		     --if not in position or not in disired state change state
		     --to Moving_Left
		     if not Turnout_Failed then
			--Callback for turnout failure
			--conditional check to call back only once
			if Failure_Callback /= null then
			   Failure_Callback.all (My_Requestor, My_ID);
			end if;
			Turnout_Failed := True;
		     end if;
		     Turnout_State := Moving_Left;
		     --Set timeout for Moving Left if Moving Right Failed
		     My_Time_Delay := Clock;
		     My_Time_Delay := My_Time_Delay + Time_Limit;
		  end if;
	       end select;

	    when Set_Right =>
	       Turnouts (My_ID).Current := Right;
	       Turnouts (My_ID).Moving := False; --Sets Record to not moving
	       --Callback with not Moving
	       if Change_Callback /= null then
		  Change_Callback.all (Turnout   => My_ID,
			 Direction => Right,
			 Moving    => False);
	       end if;
	       select
		  accept Move_Left (Requestor : Train_Types.Request_ID) do
		     My_Requestor := Requestor;
		  end Move_Left;
		  Turnouts (My_ID).Desired := Left; --Sets new desired direction
		  Turnout_State := Moving_Left; --Change State
		  --Set timeout
		  My_Time_Delay := Clock;
		  My_Time_Delay := My_Time_Delay + Time_Limit;
	       or
		  accept Move_Right (Requestor : Train_Types.Request_ID) do
		     My_Requestor := Requestor;
		  end Move_Right;
	       end select;

	    when Moving_Left =>

	       Motors.Set (Motor     => My_ID,
		    --Change Turnout Direction
		    Direction => Left);
	       Turnouts (My_ID).Moving := True; --Sets Record to moving
	       if Change_Callback /= null then
		  --Callback with Moving stats
		  Change_Callback.all (Turnout   => My_ID,
			 Direction => Left,
			 Moving    => True);
	       end if;
	       select
		  accept Move_Right (Requestor : Train_Types.Request_ID) do
		     My_Requestor := Requestor;
		  end Move_Right;
		  --Do nothing here
	       or

		  delay until My_Time_Delay; --wait 3 for movment


		  if Motors.In_Position (Motor => My_ID) and
		    Turnouts (My_ID).Desired = Left then
		     --check if turnout is in postion and in desired state then
		     --change state to Set_Left

		     if Turnout_Failed then
			--If turnout did fail send recovery callback
			if Recover_Callback /= null then
			   Recover_Callback.all (My_ID);
			end if;
			Turnout_Failed := False;
		     end if;
		     Turnout_State := Set_Left; --Change State

		  else
		     --if not in position or not in disired state then
		     --change state to Moving_Right
		     if not Turnout_Failed then
			--Callback for turnout failure
			--conditional check to call back only once
			if Failure_Callback /= null then
			   Failure_Callback.all (My_Requestor, My_ID);
			end if;

			Turnout_Failed := True;
		     end if;
		     Turnout_State := Moving_Right;
		     --Set timeout for Moving Right if Failed
		     My_Time_Delay := Clock;
		     My_Time_Delay := My_Time_Delay + Time_Limit;
		  end if;
	       end select;

	 end case;
      end loop;
   exception
      when others =>
	 Double_Talk.Speak
	   (Phrase => Double_Talk.Phrase_Strings.To_Bounded_String
       ("Turnout task Exception Raised!"),
		     Voice  => Double_Talk.Vader);
   end Turnout_Control_Task;

   -----------------------------------------------------------------------------

   function Status (Turnout : in  Layout.Turnout_ID) return Status_Rec is
   begin
      return Turnouts (Turnout);
   end Status;

   -----------------------------------------------------------------------------
   function Direction_Of (Turnout : in Layout.Turnout_ID)
                          return Layout.Turn_Choice is
   begin
      return Turnouts (Turnout).Desired;
   end Direction_Of;


   -----------------------------------------------------------------------------
   procedure Shut_Down is
      Counter : Integer := 0;

   begin
      for Turnout in Layout.Turnout_ID'Range loop
         if Counter = 4 then
            delay Duration (3);
            Counter := 0;
         end if;
         Motors.Set (Motor     => Turnout,
		     Direction => Left);
	 Counter := Counter + 1;
      end loop;
   end Shut_Down;

begin
   --Initialize Turnout record
   for Turnout in Turnouts'Range loop
      Turnouts (Turnout) := (Desired => Left,
                             Current => Left,
                             Moving  => False);
   end loop;
   --Initialize Turnout Tasks
   for Index in Task_Array'Range loop
      Turnout_Tasks (Index).Assign_Unit (Unit => Index);
   end loop;

end Turnouts;
