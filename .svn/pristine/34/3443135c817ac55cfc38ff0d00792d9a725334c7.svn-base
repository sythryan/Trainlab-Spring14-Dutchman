--Author: Nigel Ploof
--Team  :  The Grounded Dutchman
with Ada.Text_IO;
with Ada.Strings.Bounded;
with Ada.Characters.Handling;

package body Command is

   type State_Type is
     (Wait_for_1st_Input, Wait_for_2nd_Input, Wait_for_3rd_Input);

   package Input_String is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Max => 3);

   type Command_Array is array (Command_Type) of Command_Rec;

   Commands : constant Command_Array :=
                (Stop_All => (Which => Stop_All),
                 Stop     => (Which => Stop, Train => 3),
                 Go       => (Which => Go, Train => 3),
                 Left     => (Which => Left, Turnout => 26),
                 Right    => (Which => Right, Turnout => 26),
                 Free     => (Which => Free, Block => 40),
                 Skill    => (Which => Skill, Engineer => 3),
                 Restart  => (Which => Restart),
                 Quit     => (Which => Quit),
                 Error    => (Which => Error));


   procedure Get (Command : out Command_Rec) is
      Input          : Character;
      Current_State  : State_Type := Wait_for_1st_Input;
      Command_String : Input_String.Bounded_String
        := Input_String.To_Bounded_String ("");

   begin
      loop
         case Current_State is
            when Wait_for_1st_Input =>
               Ada.Text_IO.Get (Input);
               if Ada.Characters.Handling.Is_Digit (Input) then
                  Current_State := Wait_for_2nd_Input;
                  Command_String :=
                    Input_String.Append (Left  => Command_String,
                                         Right => Input);
               else
                  if Input = ' ' then
                     Command := Commands (Stop_All);
                  elsif Input = 'r' or Input = 'R' then
                     Command := Commands (Restart);
                  elsif Input = 'q' or Input  = 'Q' then
                     Command := Commands (Quit);
                  else
                     Command := Commands (Error);
                  end if;
                  exit;
               end if;

            when Wait_for_2nd_Input =>
               Ada.Text_IO.Get (Input);
               if Ada.Characters.Handling.Is_Digit (Input) then
                  Current_State := Wait_for_3rd_Input;
                  Command_String :=
                    Input_String.Append (Left  => Command_String,
                                         Right => Input);
               else
                  if Input = 's' or Input  = 'S' then
                     Command := Commands (Stop);
                     Command.Train := Train_Types.Train_ID'Value
                       (Input_String.To_String (Command_String));
                  elsif Input = 'g' or Input  = 'G' then
                     Command := Commands (Go);
                     Command.Train := Train_Types.Train_ID'Value
                       (Input_String.To_String (Command_String));
                  elsif Input = 'r' or Input  = 'R' then
                     Command := Commands (Right);
                     Command.Turnout := Layout.Turnout_ID'Value
                       (Input_String.To_String (Command_String));
                  elsif Input = 'l' or Input  = 'L' then
                     Command := Commands (Left);
                     Command.Turnout := Layout.Turnout_ID'Value
                       (Input_String.To_String (Command_String));
                  elsif Input = 'f' or Input  = 'F' then
                     Command := Commands (Free);
                     Command.Block := Layout.Block_ID'Value
                       (Input_String.To_String (Command_String));
                  elsif Input = 'e' or Input  = 'E' then
                     Command := Commands (Skill);
                     Command.Engineer := Engineer_ID'Value
                       (Input_String.To_String (Command_String));
                  else
                     Command := Commands (Error);
                  end if;
                  exit;
               end if;
            when Wait_for_3rd_Input =>
               Ada.Text_IO.Get (Input);
               if Ada.Characters.Handling.Is_Digit (Input) then
                  Command := Commands (Error);
               else
                  if Input = 'r' or Input  = 'R' then
                     Command := Commands (Right);
                     Command.Turnout := Layout.Turnout_ID'Value
                       (Input_String.To_String (Command_String));
                  elsif Input = 'l' or Input  = 'L' then
                     Command := Commands (Left);
                     Command.Turnout := Layout.Turnout_ID'Value
                       (Input_String.To_String (Command_String));
                  elsif Input = 'f' or Input  = 'F' then
                     Command := Commands (Free);
                     Command.Block := Layout.Block_ID'Value
                       (Input_String.To_String (Command_String));
                  else
                     Command := Commands (Error);
                  end if;
               end if;
               exit;
         end case;
      end loop;
   exception
      when Constraint_Error =>
	 Command := Commands (Error);

   end Get;


end Command;
