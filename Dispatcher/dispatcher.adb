with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Locomotives;
with Layout; use Layout;
with Layout.Search;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Display;
with Trains;
with Train_Types; use Train_Types;
with Command; use Command;
with Engineers; use Engineers;
with Engineer_Types; use Engineer_Types;
with Turnouts;
with Blocks;
with Common_Units;
with EngineerControl; use EngineerControl;
with Double_Talk; use Double_Talk;
with Ada.Exceptions;
with Halls;

-- Author : Syth Ryan
-- Team   : The Grounded Dutchman
procedure Dispatcher is
   type Train_Choice_Type is new Integer range 1 .. 11;
   type Controller_Array is array (Train_ID) of Controller_ID;
   subtype Train_Max is Integer range 0 .. Train_Types.Max_Trains;


   type Location_Rec is
      record
         Loco      : Layout.Block_ID;
         Caboose   : Layout.Block_ID;
         Direction : Layout.Direction;
      end record;

   type Location_Array is array (1 .. 3) of Location_Rec;

   package Trains_Choice_IO is new Ada.Text_IO.Integer_IO (Train_Choice_Type);
   package Block_IO is new Ada.Text_IO.Integer_IO (Layout.Block_ID);


   type State_Type is (Start, Setup, Run, Shutdown);
   State : State_Type;

   -- Track previous session on restart
   Prev_Initialized_Trains : Integer := 0;

   -----------------------------------------------------------------------------
   procedure Welcome_Screen (How_Many_Trains : out Train_Max) is
   begin
      --Welcome Screen
      Put_Line ("                     Grounded Dutchman Software");
      Ada.Text_IO.New_Line (2);
      loop
	 Put_Line ("How many trains do you wish to run? (1, 2, or 3)");
	 New_Line;
	 Put_Line ("There must be at least one unoccupied block between each " &
	      "pair of trains");
	 New_Line (5);
	 Ada.Text_IO.New_Line;
	 begin
	    Get (How_Many_Trains);
	    exit;
	 exception
	    when others =>
	       Put_Line ("Invalid number, please choose 1, 2, or 3");
	       -- Should flush the buffer in case of invalid chars
	       Skip_Line;
	 end;
      end loop;
      State := Setup;
   end Welcome_Screen;

   -----------------------------------------------------------------------------
   procedure Train_Selection (Train_Number   : in Positive;
                              Train_Choices  : in out Locomotives.Loco_Array) is
      Train       : Train_Choice_Type;
      Road_Name   : Unbounded_String;
      Model       : Unbounded_String;
      Loco_Number : Unbounded_String;

   begin
      New_Line (7);
      loop
	 --Train Setup
	 Put_Line ("The following locomotives are available");
	 Put_Line ("          Road Name           Model          Number");
	 New_Line;
	 for Index in Locomotives.Available_Locos'Range loop
	    --display avalible trains
	    Put (Index, 2);
	    Put ("      ");
	    Put (Locomotives.Available_Locos (Index).Name);
	    New_Line;
	 end loop;
	 Put_Line ("11      Other");
	 Ada.Text_IO.New_Line;
         Put ("Enter the line number from the above table of the" &
		" locomotive pulling Train #");
	 Put (Train_Number, 0);
	 New_Line (3);
         begin
            Trains_Choice_IO.Get (Train);
            case Train is
               when 11 =>
                  Put_Line ("What is the Road Name for the Train?");
                  Get_Line (Road_Name);
                  Put_Line ("What is the Model for the Train?");
                  Get_Line (Model);
                  Put_Line ("What is the Number on the Locomotive?");
                  Get_Line (Loco_Number);
                  Train_Choices (Train_Number).Name := To_String
                    (Road_Name & " " & Model & " " & Loco_Number);
                  loop
                     begin
                        Put_Line
                          ("What is the minimum throttle for the Train?");
                        Get (Train_Choices (Train_Number).Minimum_Throttle);
                        exit;
                     exception
                        when others =>
                           Put_Line
			     ("Minimum throttle must be between 1 - 100");
			   -- Should flush the buffer in case of invalid chars
			   Skip_Line;
                     end;
                  end loop;
               when others =>
                  Train_Choices (Train_Number) :=
                    Locomotives.Available_Locos (Integer (Train));
            end case;
            exit;
         exception
	    when others =>
	       New_Line (7);
	       Put_Line ("Invaild entry," & " Please select 1 - 11");
	       -- Should flush the buffer in case of invalid chars
	       Skip_Line;
	       New_Line (2);
         end;
      end loop;
   end Train_Selection;

   -----------------------------------------------------------------------------
   procedure Process_Reenter_Decision (Reenter      : out Boolean;
				       State_Change : out Boolean) is
      Decision    : Character;
   begin
      Reenter      := False;
      State_Change := False;
      New_Line;
      Put_Line ("Would you like to reenter this train's location?");
      Put_Line ("   Y = yes, I wish to reenter this train's location.");
      Put_Line
	("   N = no,  I wish to restart set up from the beginning.");
      Put_Line
	("   Q = no,  I wish to terminate this operating session.");
      New_Line (5);
      loop
	 begin
	    Get (Decision);
	    case Decision is
	       when 'y' | 'Y' =>
		  Reenter := True;
		  State_Change := False;
		  return;
	       when 'n' | 'N' =>
		  State := Start;
		  State_Change := True;
		  return;
	       when 'q' | 'Q' =>
		  State := Shutdown;
		  State_Change := True;
		  return;
	       when others =>
		  Put_Line ("Invalid choice, please select Y, N, or Q");
		  Skip_Line;
	    end case;
	 exception
	    when others =>
	       Put_Line ("Invalid choice, please select Y, N, or Q");
	       Skip_Line;
	 end;
      end loop;
   end Process_Reenter_Decision;

   -----------------------------------------------------------------------------
   procedure Process_Train_Length (Loco         : in Layout.Block_ID;
                                   Caboose      : in Layout.Block_ID;
				   Reenter      : out Boolean;
                                   State_Change : out Boolean) is
      Blocks   : Layout.Search.Block_List (3);
      Turnouts : Layout.Search.Turnout_List (3);
      Success  : Boolean;
   begin
      State_Change := False;
      Reenter      := False;
      Success      := True;
      if Loco /= Caboose then
	 Layout.Search.Blocks_Beneath (Loco     => Loco,
				       Caboose  => Caboose,
				       Blocks   => Blocks,
				       Turnouts => Turnouts,
				       Success  => Success);
      end if;
      loop
	 exit when Success or Reenter or State_Change;
	    New_Line (19);
	    Put_Line ("The maximum number of blocks beneath a train is 3");
	    Put_Line ("There are more than 3 blocks beneath your train");
	    Process_Reenter_Decision (Reenter      => Reenter,
                                      State_Change => State_Change);
      end loop;
   end Process_Train_Length;

   -----------------------------------------------------------------------------
   procedure Train_Locations (Train_Number : in     Natural;
                              Locations    : in out Location_Array;
                              State_Change :    out Boolean) is
      Direction : Character;
      Previous  : Natural;
      Reenter   : Boolean;
   begin
      Reenter := True;
      State_Change := False;
      loop
         exit when State_Change or not Reenter;
         Reenter := False;
         loop
	    begin
	       New_Line (19);
	       Put ("On which block is the locomotive pulling train ");
	       Put (Train_Number, 0);
	       Put_Line (" located ?");
	       New_Line (3);
	       Block_IO.Get (Locations (Train_Number).Loco);
	       New_Line (2);
	       Put ("On which block is the caboose (or observation car)" &
	       " for train ");
	       Put (Train_Number, 0);
	       Put_Line (" located ?");
	       New_Line (3);
	       Block_IO.Get (Locations (Train_Number).Caboose);
	       New_Line (2);
	       -- Check that new train isn't the same as a previous
	       Previous := Train_Number - 1;
	       loop
		  exit when Previous < 1;
		  if Locations (Train_Number).Caboose =
		    Locations (Previous).Caboose or
		    Locations (Train_Number).Loco =
		    Locations (Previous).Loco then
		     New_Line (19);
		     Put (" the location information for Train ");
		     Put (Train_Number, 0);
		     Put_Line ("is not valid because");
		     Put_Line ("it conflicts with a train you entered earlier");
		     Process_Reenter_Decision (Reenter      => Reenter,
                                               State_Change => State_Change);
		     New_Line;
		     exit;
		  end if;
		  Previous := Previous - 1;
	       end loop;
	       exit;
	    exception
	       when others =>
		  Put_Line ("Invalid block number, choose 1 - 40");
		  -- Should flush the buffer in case of invalid chars
		  Skip_Line;
	    end;
	 end loop;
	 New_Line;
	 if Reenter = False and State_Change = False then
	    if Locations (Train_Number).Loco =
	      Locations (Train_Number).Caboose then
	       loop
		  begin
		     Put_Line ("What is the direction of the locomotive" &
		               " on the block?");
		     Put_Line ("   N = Normal");
		     Put_Line ("   R = Reversed");
		     Get (Direction);
		     case Direction is
			when 'N' | 'n' =>
                           Locations (Train_Number).Direction := Normal;
			   exit;
			when 'R' | 'r' =>
			   Locations (Train_Number).Direction := Reversed;
			   exit;
			when others =>
			   Put_Line ("Invalid direction please choose N or R");
		     end case;
		  exception
		     when others =>
			Put_Line ("Invalid direction please choose N or R");
		  end;
	       end loop;
	    end if;
            Process_Train_Length
              (Loco         => Locations (Train_Number).Loco,
               Caboose      => Locations (Train_Number).Caboose,
               Reenter      => Reenter,
               State_Change => State_Change);
         end if;
      end loop;
   end Train_Locations;

   -----------------------------------------------------------------------------
   procedure Process_Confirm_Decision (Reentered    : out Boolean) is
      Decision : Character;
   begin
      Reentered := False;
      New_Line (2);
      Put_Line ("Is this information correct? Enter one of the following:");
      Put_Line ("   Y = yes, the information for this train is correct.");
      Put_Line
        ("   N = no, I wish to enter different information for this train.");
      Put_Line ("   R = no, I with to restart setting up from the beginning");
      Put_Line ("   Q = no, I wish to terminate this operating session. ");
      begin
	 New_Line (4);
	 Get (Decision);
	 case Decision is
	    when 'y' | 'Y' =>
               State := Run; -- information correct
	    when 'n' | 'N' =>
               Reentered := True; -- Reenter specific train
            when 'r' | 'R' =>
               State := Start;
	    when 'q' | 'Q' =>
	       State := Shutdown;
	    when others =>
	       Put_Line ("Invalid choice, please select Y, N, or Q");
	 end case;
      exception
	 when others =>
	    Put_Line ("Invalid choice, please select Y, N, or Q");
      end;
   end Process_Confirm_Decision;

   -----------------------------------------------------------------------------
   procedure Confirm_Train (Train_Number     : in Positive;
                            Train_Choices    : in Locomotives.Loco_Array;
                            Locations        : in Location_Array;
                            Reenter          : out Boolean) is
      Blocks   : Layout.Search.Block_List (3);
      Turnouts : Layout.Search.Turnout_List (3);
      Success  : Boolean;
   begin
      New_Line (19);
      Put ("                             Train #");
      Put (Train_Number, 0);
      New_Line;
      Put_Line ("               Confirmation of Information");
      New_Line;
      Put_Line (Train_Choices (Train_Number).Name);
      New_Line;
      Put ("Locomotive on block");
      Block_IO.Put (Locations (Train_Number).Loco, 3);
      New_Line;
      Put ("Caboose    on block");
      Block_IO.Put (Locations (Train_Number).Caboose, 3);
      New_Line;
      Put ("Train occupies blocks");
      if Locations (Train_Number).Loco /= Locations (Train_Number).Caboose then
         Search.Blocks_Beneath (Loco     => Locations (Train_Number).Loco,
                                Caboose  => Locations (Train_Number).Caboose,
                                Blocks   => Blocks,
                                Turnouts => Turnouts,
                                Success  => Success);
      else
         Blocks.Size := 1;
         Blocks.Items (1).Block := Locations (Train_Number).Loco;
      end if;
      for Index in 1 .. Blocks.Size - 1 loop
         Block_IO.Put (Blocks.Items (Index).Block, 3);
         Put (",");
      end loop;
      Block_IO.Put (Blocks.Items (Blocks.Size).Block, 3);
      Process_Confirm_Decision (Reentered    => Reenter);
   end Confirm_Train;

   -----------------------------------------------------------------------------
   procedure Display_Throttle (Train    : in Train_ID;
			       Throttle : in Common_Units.Percent) is
   begin
      Display.Put (Train    => Train,
		   Throttle => Throttle);
   end Display_Throttle;

   -----------------------------------------------------------------------------
   procedure Display_Direction (Train     : in Train_ID;
				Direction : in Trains.Direction_Type) is
   begin
      Display.Put (Train     => Train,
		   Direction => Direction);
   end Display_Direction;

   -----------------------------------------------------------------------------
   procedure Display_Initial_Info (Chosen_Trains    : in Locomotives.Loco_Array;
				   Number_Of_Trains : in Positive) is
      Occupied_Blocks : Layout.Block_Array (1 .. 6);
      Size : Positive;
   begin
      -- Display all possible items on the display

      -- Turnouts
      for Turnout in Turnout_ID'Range loop
	 Display.Put (Turnout   => Turnout,
	       Direction => Turnouts.Direction_Of (Turnout => Turnout),
	       Moving    => False);
      end loop;
      for Index in 1 .. Number_Of_Trains loop
	 Display.Put (Train => Train_ID (Index),
	              Name  => Chosen_Trains (Index).Name);
	 Display.Put (Train => Train_ID (Index),
               Skill => Engineers.Check_Level
		 (Engineer => Engineer_ID (Index)));

	 Display_Direction (Train     => Train_ID (Index),
		            Direction => Trains.Forward);

	 Display_Throttle (Train    => Train_ID (Index),
		 Throttle => Trains.Get_Throttle (Train => Request_ID (Index)));
	 --Get blocks_on from a train
         Trains.Blocks_On (Train  => Train_ID (Index),
			   Blocks => Occupied_Blocks,
			   Size   => Size);
         Display.Put (Train  => Train_ID (Index),
		      Blocks => Occupied_Blocks (1 .. Size));
      end loop;

   end Display_Initial_Info;

   procedure Free_Occupied_Blocks (Train_Count : in Train_ID) is
      Occupied_Blocks : Layout.Block_Array (1 .. 6);
      Count           : Positive;
   begin
      for Index in 1 .. Train_Count loop
	 Trains.Blocks_On (Train  => Index,
		    Blocks => Occupied_Blocks,
		    Size   => Count);
      end loop;

      for Index in 1 .. Count loop
	 Blocks.Free_Block (Requestor => Train_Types.Dispatcher,
		            Block     => Occupied_Blocks (Index));
      end loop;
   end Free_Occupied_Blocks;

   -----------------------------------------------------------------------------
   Number_Of_Trains : Positive;
   Locations        : Location_Array;
   Train_Choices    : Locomotives.Loco_Array (1 .. 3);
   Count            : Positive;
   Reenter          : Boolean;
   State_Change	    : Boolean;
   Dispatch_Cmd	    : Command.Command_Rec;
   Controllers	     : constant Controller_Array := (A, B, C);

begin
   State := Start;
   loop
      case State is
         when Start =>
	    Double_Talk.Speak (Phrase =>
	                 Double_Talk.Phrase_Strings.To_Bounded_String
	                 ("Grounded Dutchman is ready to begin an " &
                         "operating session."),
                          Voice  => Paul);
	    New_Line (17);
	    Welcome_Screen (How_Many_Trains => Number_Of_Trains);
	 when Setup =>
	    Halls.Disable;
            New_Line (17);
            Count := 1;
            Reenter := False;
            State_Change := False;
            loop
               exit when Count > Number_Of_Trains or
		 State = Start or State = Shutdown;
	       if Count > Prev_Initialized_Trains or Reenter then
		  Train_Selection (Train_Number  => Count,
		     Train_Choices => Train_Choices);
		  Train_Locations (Train_Number => Count,
		     Locations    => Locations,
		     State_Change => State_Change);
	       end if;

               if not State_Change then
                  Confirm_Train (Train_Number  => Count,
                                 Train_Choices => Train_Choices,
                                 Locations     => Locations,
                                 Reenter       => Reenter);
               end if;
               if State = Run and not Reenter then
		  Count := Count + 1;
               end if;
	    end loop;
	    for Index in 1 .. Number_Of_Trains loop
	       Trains.Initialize_Train (Train     => Train_ID (Index),
				 Loco      => Locations (Index).Loco,
				 Caboose   => Locations (Index).Caboose,
                                 Direction => Locations (Index).Direction,
                                   Min_Throttle =>
                                     Train_Choices (Index).Minimum_Throttle);

	    end loop;
	    Prev_Initialized_Trains := Number_Of_Trains;
	    Halls.Enable (Callback => Trains.Hall_Sensor_Triggered'Access);
	 when Run =>
            Display.Enable;

	    Display_Initial_Info (Chosen_Trains    => Train_Choices,
                                  Number_Of_Trains => Number_Of_Trains);

	    Turnouts.Set_Change_Callback (To => Display.Put'Access);
            Trains.Set_Blocks_Changed_Callback
              (To => Display.Display_Blocks'Access);
            Trains.Set_Status_Changed_Callback (To => Display.Put'Access);

	    loop
	       begin
		  for Index in 1 .. Number_Of_Trains loop
		     Display.Put (Train  => Train_ID (Index),
		    Status => Trains.Get_Status_Array
		      (Train => Train_ID (Index)));
		  end loop;

		  Command.Get (Command => Dispatch_Cmd);

		  case Dispatch_Cmd.Which is

		     when Quit =>
			Display.Put_Error (Error_Message =>
		      "Run state Quit !!!!");
			State := Shutdown;
			exit;
                     when Stop_All =>
                        Double_Talk.Speak (Phrase =>
	                        Double_Talk.Phrase_Strings.To_Bounded_String
	                         ("Emergency Stop called from Dispatcher"),
                                   Voice  => Paul);
			Display.Put_Error (Error_Message =>
		      "Run state Stop All !!!!");
			for Engineer in Engineer_ID'Range loop
			   Engineers.Disable (Engineer => Engineer);
			end loop;

		     when Stop =>
			Display.Put_Error (Error_Message =>
		      "Run state Stop Eng !!!!");
			Engineers.Disable
			  (Engineer => Engineer_ID (Dispatch_Cmd.Train));
		     when Go =>
			Display.Put_Error (Error_Message =>
		      "Run state Go Eng !!!!");
			Trains.Emergency_Stop_Ready (Dispatch_Cmd.Train);

			Engineers.Enable
			  (Engineer   => Engineer_ID (Dispatch_Cmd.Train),
                           Controller => Controllers (Dispatch_Cmd.Train),
                           Train      => Dispatch_Cmd.Train);

		     when Left =>
			Display.Put_Error (Error_Message =>
		      "Run state Turn Lft !!!!");
			Turnouts.Set (Requestor => 0,
		                      Turnout   => Dispatch_Cmd.Turnout,
		                      Direction => Left);
		     when Right =>
			Display.Put_Error (Error_Message =>
		      "Run state Turn Right !!!!");
			Turnouts.Set (Requestor => 0,
		                      Turnout   => Dispatch_Cmd.Turnout,
		                      Direction => Right);
		     when Free =>
			Display.Put_Error (Error_Message =>
		      "Run state Free !!!!");
			Blocks.Free_Block (Requestor => 0,
		                           Block     => Dispatch_Cmd.Block);
		     when Skill =>
			Display.Put_Error (Error_Message =>
		      "Run state Skill !!!!");
			Engineers.Change_Level
			  (Engineer => Dispatch_Cmd.Engineer);
		     when Restart =>
			Display.Disable;
			Ada.Text_IO.New_Line (10);

			Free_Occupied_Blocks (Train_ID (Number_Of_Trains));

			State := Start;
			exit;
		     when Error =>
			Speak (Phrase => Phrase_Strings.To_Bounded_String
	                         ("Entered invalid comand"),
	                       Voice  => Paul);
		  end case;
	       exception
                  when Error : others =>
                     Display.Put_Error
                       ("Exception was raised from dispatcher run state:    " &
			    Ada.Exceptions.Exception_Name (Error) & " " &
			    Ada.Exceptions.Exception_Message (Error));
	       end;
            end loop;

         when Shutdown =>
	    Speak (Phrase => Phrase_Strings.To_Bounded_String
	           ("Grounded Dutchman is Shuting Down"),
	           Voice  => Paul);
	    for Engineer in Engineer_ID'Range loop
	       Engineers.Disable (Engineer => Engineer);
	    end loop;

	    Turnouts.Shut_Down;
	    Halls.Disable;
	    Display.Disable;

	    Ada.Text_IO.New_Line (30);
            Ada.Text_IO.Put_Line ("System shutdown. Safe to turn power off.");
	    exit;
      end case;
   end loop;
end Dispatcher;
