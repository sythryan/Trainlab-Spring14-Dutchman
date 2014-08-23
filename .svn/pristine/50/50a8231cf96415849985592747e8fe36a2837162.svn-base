-- Author: Syth Ryan
-- Team  : The Grounded Dutchman
with Common_Units;
package EngineerControl is

   type Two_Way_Direction is (Backward, Forward);
   type Three_Way_Direction is (Left, Right, Centered);
   type Button is (Down, Up);
   type Controller_ID is (A, B, C);

   -----------------------------------------------------------------------------
   procedure Get (Controller      : in Controller_ID;
		  Red_Button       : out Button;
		  Black_Button     : out Button;
		  Two_Way_Toggle   : out Two_Way_Direction;
		  Three_Way_Toggle : out Three_Way_Direction);
   -- Checks the state of the hand controller

   -----------------------------------------------------------------------------
   procedure Get (Controller	  : in Controller_ID;
		  Knob	          : out Common_Units.Percent);
   -- Checks the percent of the knob

   -----------------------------------------------------------------------------
private
   for Button use (Down => 2#0#,
		   Up   => 2#1#);

   for Two_Way_Direction use (Backward => 2#0#,
			      Forward  => 2#1#);

   for Three_Way_Direction use (Left     => 2#01#,
				Right    => 2#10#,
				Centered => 2#11#);
end EngineerControl;
