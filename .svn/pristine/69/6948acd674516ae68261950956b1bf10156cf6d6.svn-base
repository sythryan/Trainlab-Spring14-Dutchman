-- Author: Anthony Spoerl
-- Date:   4/8/2014

with Trains;
with Layout;

package Turnout_Test_Callbacks is
   procedure Turnout_Failure (Requestor : in Train_Types.Request_ID;
			      Turnout   : in Layout.Turnout_ID);

   procedure Turnout_Recovered (Turnout   : in Layout.Turnout_ID);

   procedure Turnout_Changed (Turnout   : in Layout.Turnout_ID;
			      Direction : in Layout.Turn_Choice;
			      Moving    : in Boolean);
end Turnout_Test_Callbacks;
