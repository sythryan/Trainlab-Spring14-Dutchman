-- Author    : Syth Ryan
-- Team Name : The Grounded Dutchman

package Layout is
   pragma Pure (Layout);

   type Block_ID    is range 1 .. 40;
   type Turnout_ID  is range 1 .. 26;

   type Terminator_Type is (Block, Turnout, Dead_End);
   type Hall_Sensor is range 1 .. 51;

   type Direction       is (Normal, Reversed);
   type Limb_Type       is (Left, Right, Common);

   type Block_Array is array (Integer range <>) of Block_ID;

   -- Change this Variable for more Crossblocks
   Max_Cross_Block_Size : constant Block_ID := 1;
   type Block_List is array (1 .. Integer (Max_Cross_Block_Size)) of Block_ID;

   type Cross_Blocks_Rec is
      record
	 Length : Natural;
	 List   : Block_List;
      end record;

   subtype Turn_Choice is Limb_Type range Left .. Right;

   Layout_Error : exception;

   ----------------------------------------------------------------------------
   function End_Of_Block (Block : in Block_ID;
                          Side  : in Direction) return Terminator_Type;
   -- Preconditions : none
   -- Returns whats located on a specified side of the block

   ----------------------------------------------------------------------------
   function Next_Block (Block      : in Block_ID;
                        Side       : in Direction) return Block_ID;
   -- Preconditions : none
   -- exception : Layout_Error is raised when there is no block on the Side
   --		     of Block
   -- Returns The block number on the Side of Block

   -----------------------------------------------------------------------------
   function Next_Turnout (Block      : in Block_ID;
                          Side       : in Direction) return Turnout_ID;
   -- Preconditions : none
   -- exception : Layout_Error is raised when there is no turnout on the Side
   --		  of Block
   -- Returns The turnout number on the side of Block

   -----------------------------------------------------------------------------
   function Limb_Of_Turnout (Turnout : in Turnout_ID;
                             Limb    : in Limb_Type) return Block_ID;
   -- Preconditions : none
   -- Returns the Block_ID located on the Limb of Turnout

   ----------------------------------------------------------------------------
   function Sensor_Between_Blocks (Block_One : in Block_ID;
                                   Block_Two : in Block_ID) return Hall_Sensor;
   -- Preconditions : none
   -- Exceptions : Layout_Error is raised when Block_One and Block_Two are not
   -- 		   adjacent
   -- Returns the Hall sensor between Block_One and Block_Two

   ----------------------------------------------------------------------------
   function Sensor_Polarity (Hall_ID : in Hall_Sensor) return Boolean;
   -- Returns if the sensor is a reversing point:
   --           True => Normal, False => Reversing
   ----------------------------------------------------------------------------
   function Opposite (A_Direction : in Direction) return Direction;
   -- Returns the opposite direction of A_Direction

   ----------------------------------------------------------------------------
   function Opposite (Turn_Direction : Limb_Type) return Limb_Type;
   --Returns the opposite turn direction of a turnout.

   ----------------------------------------------------------------------------
   procedure Blocks_Around_Sensor (A_Hall_Sensor : in Hall_Sensor;
                                   Block_1       : out Block_ID;
                                   Block_2	 : out Block_ID);
   --Returns a record contain-0ing two Block_IDs givin a Hall_Sensor ID

   ----------------------------------------------------------------------------
   function Is_Force_Turnout (A_Block : Block_ID; A_Direction : Direction)
                              return Boolean;
   --Returns True if there is a forced turnout at the given end of a Block_ID

   ----------------------------------------------------------------------------
   procedure Get_Force_Turnout_Info (A_Block         : in Block_ID;
                                     Block_Direction : in Direction;
                                     A_Turnout       : out Turnout_ID;
                                     Set_Turnout     : out Limb_Type);
   --Returns a record with the ID of a turnout and the direction it must be set

   -----------------------------------------------------------------------------
   function Is_Joint_Turnout (A_Turnout : Turnout_ID; A_Limb : Limb_Type)
                              return Boolean;
   --Returns True if there is a Joint Turnout at the given Limb of a Turnout

   -----------------------------------------------------------------------------
   function Get_Joint_Turnout (A_Turnout : Turnout_ID; A_Limb : Limb_Type)
                               return Turnout_ID;
   --Returns the ID of the turnout the given turnout is connected with
   --to form a joint turnout

   -----------------------------------------------------------------------------
   function Get_Crossing_Blocks (A_Block : Block_ID) return Cross_Blocks_Rec;
   --Returns a list of Blocks that cross the given Block

   -----------------------------------------------------------------------------
   function Next_Choice_Turnout (A_Block : Block_ID; A_Direction : Direction)
                               return Turnout_ID;
   --Returns the next choice Turnout_ID of a Block with a given direction


end Layout;
