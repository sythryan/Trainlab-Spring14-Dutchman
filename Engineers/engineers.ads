with EngineerControl; use EngineerControl;
with Engineer_Types; use Engineer_Types;
with Train_Types; use Train_Types;

with Ada.Real_Time; use Ada.Real_Time;
package Engineers is
   -- Author: Syth Ryan
   -- Team  : The Grounded Dutchman

   Time_Limit : constant Ada.Real_Time.Time_Span :=
		  Ada.Real_Time.To_Time_Span (3.0);  -- seconds

   -----------------------------------------------------------------------------

   procedure Enable (Engineer   : Engineer_ID;
                     Controller : Controller_ID;
                     Train : Train_Types.Train_ID);
   -- Enables an engineer
   -- If already enabled, the request is ignored
   -- When enabled from a disabled state, reset to novice.

   procedure Disable (Engineer : Engineer_ID);
   -- Disables an engineer
   -- If already disabled, the request is ignored

   procedure Change_Level (Engineer : Engineer_ID);
   -- Sets the Engineer's level to Opposite level

   function Check_Status (Engineer : Engineer_ID) return State_Type;
   --Must wait .1 sec after setting status before checking

   function Check_Level (Engineer : Engineer_ID) return Level_Type;

end Engineers;
