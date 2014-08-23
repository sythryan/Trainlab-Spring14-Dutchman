package Engineers_Stub is
   -- Author: Syth Ryan
   -- Team  : The Grounded Dutchman

   type Engineer_ID is new Integer range 1 .. 3;
   type Level_Type is (Novice, Expert);

   -----------------------------------------------------------------------------

   procedure Enable (Engineer : Engineer_ID);
   -- Enables an engineer
   -- If already enabled, the request is ignored
   -- When enabled from a disabled state, reset to novice.

   procedure Disable (Engineer : Engineer_ID);
   -- Disables an engineer
   -- If already disabled, the request is ignored

   procedure Change_Level (Engineer : Engineer_ID);
   -- Sets the Engineer's level to Opposite level

   --function Check_Status (Engineer : Engineer_ID) return Status_Type;

   --function Check_Level (Engineer : Engineer_ID) return Level_Type;

end Engineers_Stub;
