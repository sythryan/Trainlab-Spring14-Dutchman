--with Dallee; use Dallee;
with Common_Units; use Common_Units;
with Layout; use Layout;
with Trains; use Trains;
with Ada.Exceptions;
with Display;
with Double_Talk; use Double_Talk;

package body Engineers is
   -- Author: Abubakar Audu
   -- Team  : The Grounded Dutchman
   -- Date  : 4/17/2014

   type Status_Rec is
      record
         State	 : State_Type := Disabled;
         Level	 : Level_Type := Novice;
      end record;

   type Status_Rec_Array is array (Engineer_ID) of Status_Rec;

   protected Engineer_Status is
      procedure Write (Engineer : in Engineer_ID;
                       State	   : in  State_Type;
                       Level	   : in Level_Type);
      procedure Read (Engineer : in Engineer_ID;
                     Eng_Status : out Status_Rec);
   private
      Engineers_Status	 : 	Status_Rec_Array;
   end Engineer_Status;


   task type An_Engineer is
      entry Enable (Controller : Controller_ID;
                    Train      : Train_ID);
      entry Disable;
      entry Change_Level;
   end An_Engineer;

   type Engineer_Rec is
      record
         Red_Button       : Button;
         Black_Button     : Button;
         Two_Way_Toggle   : Two_Way_Direction;
         Three_Way_Toggle : Three_Way_Direction;
         Throttle         : Common_Units.Percent;
      end record;

   type Task_Array is array (Engineer_ID) of An_Engineer;
   Engineer_Tasks                  : Task_Array;



   -----------------------Bodies----------------------------------------------

   protected body Engineer_Status is
      procedure Read (Engineer : in Engineer_ID;
                     Eng_Status : out Status_Rec) is
      begin
         Eng_Status := Engineers_Status (Engineer);
      end Read;

      procedure Write (Engineer : in Engineer_ID;
                       State	   : in  State_Type;
                       Level	   : in Level_Type) is
      begin
         Engineers_Status (Engineer).State := State;
         Engineers_Status (Engineer).Level := Level;
      end Write;

   end Engineer_Status;

   task body An_Engineer is
      Current_State              : State_Type := Disabled;
      My_Level                   : Level_Type := Novice;
      My_Controller              : Controller_ID;
      My_Train                   : Train_ID;
      Current                    : Engineer_Rec;
      Previous                   : Engineer_Rec;
      Delay_Time		 : constant Duration := 0.1;
      Delay_Counter_For_Change   : Duration := Duration (0);


   begin
      loop
	 case Current_State is
	    when Disabled =>
               select
                  accept Enable (Controller : in Controller_ID;
                                 Train      : in Train_ID) do
                     My_Controller := Controller;
		     My_Train      := Train;
                  end Enable;
		  Current_State := Enabled;
		  begin
		     Trains.Enable (Train => My_Train);
		  exception
		     when others =>
			Display.Put_Error (" Trains has crashed ");
		  end;
                  --Set Skill level to novice to comply with postcondition
                  My_Level := Novice;
                  Engineer_Status.Write (Engineer => Engineer_ID (My_Train),
                                         State    => Current_State,
                                         Level    => My_Level);
                  EngineerControl.Get
                    (Controller       => My_Controller,
                     Red_Button       => Previous.Red_Button,
                     Black_Button     => Previous.Black_Button,
                     Two_Way_Toggle   => Previous.Two_Way_Toggle,
                     Three_Way_Toggle => Previous.Three_Way_Toggle);
		  Current := Previous;
               or
                  accept Disable;
               or
		  accept Change_Level;
               end select;

	    when Enabled =>
               select
                  accept Enable (Controller : in Controller_ID;
                                 Train      : in Train_ID);
                  Trains.Enable (Train => My_Train);
               or
                  accept Disable;
		  Trains.Disable (Train => My_Train);
                  Current_State := Disabled;
                  Engineer_Status.Write (Engineer => Engineer_ID (My_Train),
                                         State    => Current_State,
                                         Level    => My_Level);
               or
		  accept Change_Level do
		     if My_Level = Novice then
                        My_Level := Expert;
                        Engineer_Status.Write
                          (Engineer => Engineer_ID (My_Train),
                                         State    => Current_State,
                                         Level    => My_Level);
		     else
                        My_Level := Novice;
                        Engineer_Status.Write
                          (Engineer => Engineer_ID (My_Train),
                                         State    => Current_State,
                                         Level    => My_Level);
		     end if;
                  end Change_Level;
               or delay Delay_Time;
               end select;
	       begin
		  EngineerControl.Get (Controller       => My_Controller,
			 Red_Button       => Current.Red_Button,
			 Black_Button     => Current.Black_Button,
			 Two_Way_Toggle   => Current.Two_Way_Toggle,
			 Three_Way_Toggle => Current.Three_Way_Toggle);
		  EngineerControl.Get (Controller => My_Controller,
			 Knob       => Current.Throttle);
	       exception
		  when Error : others =>
		     Display.Put_Error ("Exception was raised:    " &
			    Ada.Exceptions.Exception_Name (Error) & " " &
			    Ada.Exceptions.Exception_Message (Error));
	       end;

               -----------  Emergency Stop -----------------------------------
               if Previous.Red_Button = Up and
		 Current.Red_Button = Down then
		  Double_Talk.Speak (Phrase =>
			 Double_Talk.Phrase_Strings.To_Bounded_String
			   ("Engineer " & Train_ID'Image (My_Train) &
			    " requested emergency stop"),
		                     Voice  => Paul);
                  Trains.Emergency_Stop (Train   => My_Train);
                  Current_State := Disabled;
               end if;
               ------------ Horn ----------------------------------------------
               if Previous.Black_Button = Up and
                 Current.Black_Button = Down then
                  Trains.Sound_Horn (Train => My_Train);
               end if;
	       ------------ Throttle -----------------------------------------
               if Current.Throttle < 5 then
                  --increments the counter if the train is "stopped"
                  --else resets to zero
                  Delay_Counter_For_Change :=
                    Delay_Counter_For_Change + Delay_Time;
               else
                  Delay_Counter_For_Change := 0.0;
               end if;

               if Previous.Throttle /= Current.Throttle then
                  Trains.Set_Throttle (Train  => My_Train,
                                       Amount => Current.Throttle);
               end if;
               ------------- Train Direction ----------------------------------
	       if Previous.Two_Way_Toggle /= Current.Two_Way_Toggle and
		 Current.Throttle < 50 and
		 Delay_Counter_For_Change >= 3.0 then
		  Trains.Toggle_Direction (Train => My_Train);
	       elsif Delay_Counter_For_Change < 3.0 and
		 Previous.Two_Way_Toggle /= Current.Two_Way_Toggle then
		  Double_Talk.Speak (Phrase =>
			 Double_Talk.Phrase_Strings.To_Bounded_String
			   ("Must stop for three " &
			      "seconds to change direction."),
		       Voice  => Paul);
		  null;
	       end if;
               --------------Turnout Direction ---------------------------------
	       if Current.Three_Way_Toggle /= Centered then
		  if Previous.Three_Way_Toggle /= Current.Three_Way_Toggle then
		     --Check that turnout toggle has just been moved to the left or
		     -- right.
		     Display.Put_Error ("Toggle Turnout from Eng");
		     if My_Level = Expert then
			--If the Engineer level is expert then toggle without
			--checking "stopped" for 3 seconds condition
			if Current.Three_Way_Toggle = Left then
			   Trains.Change_Next_Choice_Turnout
			     (Train     => My_Train,
	                      Direction => Left);
			elsif Current.Three_Way_Toggle = Right then
			   Trains.Change_Next_Choice_Turnout
			     (Train     => My_Train,
	                      Direction => Right);
			end if;
		     else
			if Delay_Counter_For_Change >= Duration (3) then
			   if Current.Three_Way_Toggle = Left then
			      Trains.Change_Next_Choice_Turnout
				(Train     => My_Train,
                                 Direction => Left);
			   elsif Current.Three_Way_Toggle = Right then
			      Trains.Change_Next_Choice_Turnout
				(Train     => My_Train,
                                 Direction => Right);
			   end if;
			else
			   Double_Talk.Speak (Phrase =>
			   Double_Talk.Phrase_Strings.To_Bounded_String
			     ("Novice must stop for three " &
				"seconds to change turnouts."),
			 Voice  => Paul);
			end if;
		     end if;
		  end if;
	       end if;
	 end case;
	 Previous := Current;
      end loop;
   exception
      when Error : others =>
	 Display.Put_Error ("Exception was raised:    " &
		       Ada.Exceptions.Exception_Name (Error) & " " &
		       Ada.Exceptions.Exception_Message (Error));
	 Double_Talk.Speak (Phrase => Phrase_Strings.To_Bounded_String (
		     "Engineer Exception Raised!"),
		     Voice  => Vader);
   end An_Engineer;


   procedure Enable (Engineer   : Engineer_ID;
                     Controller : Controller_ID;
                     Train      : Train_ID) is

   begin
      Engineer_Tasks (Engineer).Enable (Controller => Controller,
                                        Train      => Train);

   end Enable;

   procedure Disable (Engineer : Engineer_ID) is
      -- Disables an engineer
      -- If already disabled, the request is ignored
   begin
      Engineer_Tasks (Engineer).Disable;
   end Disable;

   procedure Change_Level (Engineer : Engineer_ID) is
      -- Sets the Engineer's level to Opposite level
   begin

      Engineer_Tasks (Engineer).Change_Level;
   end Change_Level;

   function Check_Status (Engineer : Engineer_ID) return State_Type is
      --Must wait .1 sec after setting status before checking
      Eng_Status : Status_Rec;
   begin
      Engineer_Status.Read (Engineer   => Engineer,
                            Eng_Status => Eng_Status);
      return Eng_Status.State;
   end Check_Status;

   function Check_Level (Engineer : Engineer_ID) return Level_Type is
      Eng_Status : Status_Rec;
   begin
      Engineer_Status.Read (Engineer   => Engineer,
                            Eng_Status => Eng_Status);
      return Eng_Status.Level;
   end Check_Level;

end Engineers;
