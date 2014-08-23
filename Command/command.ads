with Layout;      -- For block and turnout ID numbers
with Engineer_Types; use Engineer_Types;
with Train_Types;      -- For train ID numbers
package Command is

   type Command_Type is (Stop_All, Stop, Go, Left, Right,
                         Free, Skill, Restart, Quit, Error);
   type Command_Rec (Which : Command_Type := Stop_All) is
      record
         case Which is
            when Stop_All | Restart | Quit | Error =>
               null;
            when Stop | Go =>
               Train : Train_Types.Train_ID;
            when Left | Right =>
               Turnout : Layout.Turnout_ID;
            when Free =>
               Block   : Layout.Block_ID;
            when Skill =>
               Engineer : Engineer_ID;
         end case;
      end record;


   procedure Get (Command : out Command_Rec);
   -- Returns a dispatcher command entered at the keyboard
   -- Preconditions  : none
   -- Postconditions : Returns one of nine possible keyboard commands
   --                  along with the associated data
   --                      or
   --                  Returns Error when the command entered is invalid.

end Command;
