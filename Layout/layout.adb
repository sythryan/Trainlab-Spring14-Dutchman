-- Author : Nigel Ploof
-- Team   : Grounded Dutchman
package body Layout is

   ---------Record Type for Terminators and IDs --------------------------------
   type Terminator_Rec (Which : Terminator_Type := Block) is
      record
         case Which is
            when Block =>
               A_Block_ID       : Block_ID;
            when Turnout =>
               A_Turnout_ID     : Turnout_ID;
            when Dead_End =>
               null;
         end case;
      end record;


   -----2D Array to hold the sensor ID given two block numbers-----------------
   type Sensor_ID_Array_Type is array (Hall_Sensor, Integer range 0 .. 1)
     of Block_ID;
   Sensor_ID_Array : constant Sensor_ID_Array_Type :=
                       (1  => (1, 11),
                        2  => (13, 31),
                        3  => (12, 31),
                        4  => (31, 36),
                        5  => (31, 32),
                        6  => (10, 11),
                        7  => (10, 12),
                        8  => (13, 14),
                        9  => (2, 13),
                        10 => (1, 2),
                        11 => (14, 15),
                        12 => (15, 26),
                        13 => (36, 37),
                        14 => (32, 33),
                        15 => (15, 16),
                        16 => (15, 28),
                        17 => (25, 26),
                        18 => (26, 27),
                        19 => (9, 10),
                        20 => (10, 29),
                        21 => (16, 17),
                        22 => (2, 17),
                        23 => (2, 3),
                        24 => (33, 34),
                        25 => (37, 38),
                        26 => (24, 28),
                        27 => (24, 25),
                        28 => (8, 27),
                        29 => (8, 9),
                        30 => (29, 30),
                        31 => (8, 39),
                        32 => (7, 8),
                        33 => (17, 18),
                        34 => (18, 30),
                        35 => (35, 38),
                        36 => (34, 35),
                        37 => (23, 24),
                        38 => (3, 4),
                        39 => (4, 18),
                        40 => (18, 19),
                        41 => (19, 20),
                        42 => (20, 35),
                        43 => (20, 21),
                        44 => (21, 39),
                        45 => (4, 5),
                        46 => (21, 23),
                        47 => (21, 22),
                        48 => (6, 22),
                        49 => (5, 6),
                        50 => (6, 7),
                        51 => (7, 40));


   ----- Create 2D Array holding Terminator Values for Block_IDs Direction------
   --   type Terminator_Type is (Block, Turnout, Dead_End);

   type Terminator_Array_Type is array (Block_ID, Direction) of Terminator_Rec;

   Terminator_Array : constant Terminator_Array_Type :=
                        (1  => (Normal   => (Which   => Block,
                                             A_Block_ID      => 2),
                                Reversed => (Which           => Block,
                                             A_Block_ID      => 11)), --Block 1
                         2  => (Normal   => (Which     => Turnout,
                                             A_Turnout_ID => 6),
                                Reversed => (Which        => Turnout,
                                             A_Turnout_ID => 3)),
                         3  => (Normal   => (Which   => Block,
                                             A_Block_ID      => 4),
                                Reversed => (Which           => Block,
                                             A_Block_ID      => 2)),
                         4  => (Normal   => (Which   => Block,
                                             A_Block_ID      => 5),
                                Reversed => (Which             => Turnout,
                                             A_Turnout_ID      => 18)),
                         5  => (Normal   => (Which   => Block,
                                             A_Block_ID      => 6),
                                Reversed => (Which           => Block,
                                             A_Block_ID      => 4)),  --Block 5
                         6  => (Normal   => (Which   => Block,
                                             A_Block_ID      => 7),
                                Reversed => (Which             => Turnout,
                                             A_Turnout_ID      => 22)),
                         7  => (Normal   => (Which   => Block,
                                             A_Block_ID      => 8),
                                Reversed => (Which             => Turnout,
                                             A_Turnout_ID      => 23)),
                         8  => (Normal   => (Which   => Turnout,
                                             A_Turnout_ID      => 12),
                                Reversed => (Which             => Turnout,
                                             A_Turnout_ID      => 10)),
                         9  => (Normal   => (Which   => Block,
                                             A_Block_ID      => 10),
                                Reversed => (Which           => Block,
                                             A_Block_ID      => 8)),
                         10 => (Normal   => (Which   => Turnout,
                                             A_Turnout_ID     => 15),
                                Reversed => (Which            => Turnout,
                                             A_Turnout_ID     => 13)), --Block10
                         11 => (Normal   => (Which   => Block,
                                             A_Block_ID      => 1),
                                Reversed => (Which           => Block,
                                             A_Block_ID      => 10)),
                         12 => (Normal   => (Which   => Block,
                                             A_Block_ID      => 31),
                                Reversed => (Which           => Block,
                                             A_Block_ID      => 10)),
                         13 => (Normal   => (Which   => Turnout,
                                             A_Turnout_ID      => 2),
                                Reversed => (Which           => Block,
                                             A_Block_ID      => 31)),
                         14 => (Normal   => (Which   => Block,
                                             A_Block_ID      => 15),
                                Reversed => (Which           => Block,
                                             A_Block_ID      => 13)),
                         15 => (Normal   => (Which   => Turnout,
                                             A_Turnout_ID      => 5),
                                Reversed => (Which             => Turnout,
                                             A_Turnout_ID      => 4)), --Block15
                         16 => (Normal   => (Which   => Block,
                                             A_Block_ID      => 17),
                                Reversed => (Which           => Block,
                                             A_Block_ID      => 15)),
                         17 => (Normal   => (Which   => Block,
                                             A_Block_ID      => 18),
                                Reversed => (Which             => Turnout,
                                             A_Turnout_ID      => 7)),
                         18 => (Normal   => (Which   => Turnout,
                                             A_Turnout_ID      => 17),
                                Reversed => (Which             => Turnout,
                                             A_Turnout_ID      => 8)),
                         19 => (Normal   => (Which   => Block,
                                             A_Block_ID      => 20),
                                Reversed => (Which           => Block,
                                             A_Block_ID      => 18)),
                         20 => (Normal   => (Which   => Block,
                                             A_Block_ID      => 21),
                                Reversed => (Which           => Turnout,
                                             A_Turnout_ID    => 19)), --Block20
                         21 => (Normal   => (Which   => Turnout,
                                             A_Turnout_ID      => 21),
                                Reversed => (Which             => Turnout,
                                             A_Turnout_ID      => 20)),
                         22 => (Normal   => (Which   => Block,
                                             A_Block_ID      => 6),
                                Reversed => (Which           => Block,
                                             A_Block_ID      => 21)),
                         23 => (Normal   => (Which   => Block,
                                             A_Block_ID      => 24),
                                Reversed => (Which           => Block,
                                             A_Block_ID      => 21)),
                         24 => (Normal   => (Which   => Turnout,
                                             A_Turnout_ID      => 11),
                                Reversed => (Which           => Block,
                                             A_Block_ID      => 23)),
                         25 => (Normal   => (Which   => Block,
                                             A_Block_ID      => 26),
                                Reversed => (Which           => Block,
                                             A_Block_ID      => 24)), --Block 25
                         26 => (Normal   => (Which   => Block,
                                             A_Block_ID      => 15),
                                Reversed => (Which             => Turnout,
                                             A_Turnout_ID      => 14)),
                         27 => (Normal   => (Which   => Block,
                                             A_Block_ID      => 26),
                                Reversed => (Which           => Block,
                                             A_Block_ID      => 8)),
                         28 => (Normal   => (Which   => Block,
                                             A_Block_ID      => 24),
                                Reversed => (Which           => Block,
                                             A_Block_ID      => 15)),
                         29 => (Normal   => (Which   => Block,
                                             A_Block_ID      => 10),
                                Reversed => (Which           => Block,
                                             A_Block_ID      => 30)),
                         30 => (Normal   => (Which   => Block,
                                             A_Block_ID      => 18),
                                Reversed => (Which           => Block,
                                             A_Block_ID      => 29)), --Block 30
                         31 => (Normal   => (Which   => Turnout,
                                             A_Turnout_ID      => 1),
                                Reversed => (Which             => Turnout,
                                             A_Turnout_ID      => 16)),
                         32 => (Normal   => (Which   => Block,
                                             A_Block_ID      => 33),
                                Reversed => (Which           => Block,
                                             A_Block_ID      => 31)),
                         33 => (Normal   => (Which   => Block,
                                             A_Block_ID      => 34),
                                Reversed => (Which           => Block,
                                             A_Block_ID      => 32)),
                         34 => (Normal   => (Which   => Block,
                                             A_Block_ID      => 35),
                                Reversed => (Which           => Block,
                                             A_Block_ID      => 33)),
                         35 => (Normal   => (Which   => Block,
                                             A_Block_ID      => 20),
                                Reversed => (Which           => Turnout,
                                             A_Turnout_ID    => 9)), --Block 35
                         36 => (Normal   => (Which   => Block,
                                             A_Block_ID      => 37),
                                Reversed => (Which           => Block,
                                             A_Block_ID      => 31)),
                         37 => (Normal   => (Which   => Block,
                                             A_Block_ID      => 38),
                                Reversed => (Which           => Block,
                                             A_Block_ID      => 36)),
                         38 => (Normal   => (Which   => Block,
                                             A_Block_ID      => 35),
                                Reversed => (Which           => Block,
                                             A_Block_ID      => 37)),
                         39 => (Normal   => (Which   => Block,
                                             A_Block_ID      => 21),
                                Reversed => (Which           => Block,
                                             A_Block_ID      => 8)),
                         40 => (Normal   => (Which           => Block,
                                             A_Block_ID      => 7),  --Block 40
                                Reversed => (Which           => Dead_End)));


   -------------Create a 2D array to hold what is on a Turnout Limb-------------

   type Turnout_Array_Type is array (Turnout_ID, Limb_Type) of Block_ID;
   Turnout_Array  :  constant  Turnout_Array_Type :=
   -----Left,Right,Common
                      ((13, 12, 31), --Turnout 1
                       (2, 14,  13),
                       (13, 1, 2),
                       (26, 14, 15),
                       (16, 28, 15), --Turnout 5
                       (3, 17, 2),
                       (16, 2, 17),
                       (30, 17, 18),
                       (34, 38, 35),
                       (39, 7, 8), --Turnout 10
                       (25, 28, 24),
                       (9, 27, 8),
                       (9, 29, 10),
                       (25, 27, 26),
                       (11, 12, 10), --Turnout 15
                       (36, 32, 31),
                       (4, 19, 18),
                       (18, 3, 4),
                       (35, 19, 20),
                       (39, 20, 21), --Turnout 20
                       (22, 23, 21),
                       (22, 5, 6),
                       (6, 40, 7),
                       (40, 40, 40),
                       (40, 40, 40), --Turnout 25
                       (40, 40, 40));

   ----------------------------------------------------------------------------
   type Forced_Turnout_Rec is
      record
         Block    : Block_ID;
         Polarity : Direction;
      end record;


   -- Array to hold Forced Turnouts -- Dirction to set turnout is represented by
   -- nested array where idx 0 = Right 1 = Left
   type Forced_Turnout_Array is array (Turnout_ID, 0 .. 1)
     of Forced_Turnout_Rec;
   Forced_Turnouts  :  constant  Forced_Turnout_Array :=
                        (1  => (0 => (Block => 12, Polarity => Normal),
                                1 => (Block => 13, Polarity => Reversed)), --jnt
                         2  => (0 => (Block => 14, Polarity => Reversed),
                                1 => (Block => 2, Polarity => Reversed)), --jnt
                         3  => (0 => (Block => 1, Polarity => Normal),
                                1 => (Block => 13, Polarity => Normal)),
                         4  => (0 => (Block => 14, Polarity => Normal),
                                1 => (Block => 26, Polarity => Normal)),
                         5  => (0 => (Block => 28, Polarity => Reversed),
                                1 => (Block => 16, Polarity => Reversed)),
                         6  => (0 => (Block => 17, Polarity => Reversed), --jnt
                                1 => (Block => 3, Polarity => Reversed)),
                         7  => (0 => (Block => 2, Polarity => Normal), --jnt
                                1 => (Block => 16, Polarity => Normal)),
                         8  => (0 => (Block => 17, Polarity => Normal),
                                1 => (Block => 30, Polarity => Normal)),
                         9  => (0 => (Block => 38, Polarity => Normal),
                                1 => (Block => 34, Polarity => Normal)),
                         10 => (0 => (Block => 7, Polarity => Normal),
                                1 => (Block => 39, Polarity => Reversed)),
                         11 => (0 => (Block => 28, Polarity => Normal),
                                1 => (Block => 25, Polarity => Reversed)),
                         12 => (0 => (Block => 27, Polarity => Reversed),
                                1 => (Block => 9, Polarity => Reversed)),
                         13 => (0 => (Block => 29, Polarity => Normal),
                                1 => (Block => 9, Polarity => Normal)),
                         14 => (0 => (Block => 27, Polarity => Normal),
                                1 => (Block => 25, Polarity => Normal)),
                         15 => (0 => (Block => 12, Polarity => Reversed),
                                1 => (Block => 11, Polarity => Reversed)),
                         16 => (0 => (Block => 32, Polarity => Reversed),
                                1 => (Block => 36, Polarity => Reversed)),
                         17 => (0 => (Block => 19, Polarity => Reversed),
                                1 => (Block => 4, Polarity => Reversed)), --jnt
                         18 => (0 => (Block => 3, Polarity => Normal),
                                1 => (Block => 18, Polarity => Normal)), --jnt
                         19 => (0 => (Block => 19, Polarity => Normal),
                                1 => (Block => 35, Polarity => Normal)),
                         20 => (0 => (Block => 20, Polarity => Normal),
                                1 => (Block => 39, Polarity => Normal)),
                         21 => (0 => (Block => 23, Polarity => Reversed),
                                1 => (Block => 22, Polarity => Reversed)),
                         22 => (0 => (Block => 5, Polarity => Normal),
                                1 => (Block => 22, Polarity => Normal)),
                         23 => (0 => (Block => 40, Polarity => Normal),
                                1 => (Block => 6, Polarity => Normal)),
                         24 => (0 => (Block => 40, Polarity => Normal),
                                1 => (Block => 40, Polarity => Normal)), -- ??
                         25 => (0 => (Block => 40, Polarity => Normal),
                                1 => (Block => 40, Polarity => Normal)),
                         26 => (0 => (Block => 40, Polarity => Normal),
                                1 => (Block => 40, Polarity => Normal))  --??
                        );


   ----------------------------------------------------------------------------
   function End_Of_Block (Block : in Block_ID;
                          Side  : in Direction) return Terminator_Type is
   begin
      return Terminator_Array (Block, Side).Which;
   end End_Of_Block;

   ----------------------------------------------------------------------------
   function Next_Block (Block      : in Block_ID;
                        Side       : in Direction) return Block_ID is
   begin
      return Terminator_Array (Block, Side).A_Block_ID;
   exception
      when Constraint_Error =>
         raise Layout_Error;
   end Next_Block;

   -----------------------------------------------------------------------------
   function Next_Turnout (Block      : in Block_ID;
                          Side       : in Direction) return Turnout_ID is
   begin
      return Terminator_Array (Block, Side).A_Turnout_ID;
   exception
      when Constraint_Error =>
         raise Layout_Error;
   end Next_Turnout;

   -----------------------------------------------------------------------------
   function Limb_Of_Turnout (Turnout : in Turnout_ID;
                             Limb    : in Limb_Type) return Block_ID is
   begin
      return Turnout_Array (Turnout, Limb);
   end Limb_Of_Turnout;


   ----------------------------------------------------------------------------
   function Sensor_Between_Blocks (Block_One : in Block_ID;
                                   Block_Two : in Block_ID)
                                   return Hall_Sensor is
   begin
      for Hall_Sensor in Sensor_ID_Array'Range loop
         if (Sensor_ID_Array (Hall_Sensor, 0) = Block_One and
               Sensor_ID_Array (Hall_Sensor, 1) = Block_Two) or
           (Sensor_ID_Array (Hall_Sensor, 1) = Block_One and
              Sensor_ID_Array (Hall_Sensor, 0) = Block_Two) then
            return Hall_Sensor;
         end if;
      end loop;
      raise Layout_Error;
   end Sensor_Between_Blocks;

   ----------------------------------------------------------------------------
   function Sensor_Polarity (Hall_ID : in Hall_Sensor) return Boolean is
   begin
      if Hall_ID = 3 or Hall_ID = 4 or Hall_ID = 5 or Hall_ID = 26 or
        Hall_ID = 30 or Hall_ID = 31 then
         return False;
      else
         return True;
      end if;
   end Sensor_Polarity;
   ----------------------------------------------------------------------------
   function Opposite (A_Direction : in Direction) return Direction is
   begin
      if A_Direction = Normal then
         return Reversed;
      else
         return Normal;
      end if;
   end Opposite;

   ----------------------------------------------------------------------------
   function Opposite (Turn_Direction : Limb_Type) return Limb_Type is
   begin
      if Turn_Direction = Right then
         return Left;
      elsif Turn_Direction = Left then
         return Right;
      else
         raise Layout_Error;
      end if;
   end Opposite;

   ----------------------------------------------------------------------------

   procedure Blocks_Around_Sensor (A_Hall_Sensor : in Hall_Sensor;
                                   Block_1       : out Block_ID;
                                   Block_2	      : out Block_ID) is
   begin
      Block_1 :=  Sensor_ID_Array (A_Hall_Sensor, 0);
      Block_2 :=  Sensor_ID_Array (A_Hall_Sensor, 1);
   end Blocks_Around_Sensor;

   ----------------------------------------------------------------------------

   function Is_Force_Turnout (A_Block : Block_ID; A_Direction : Direction)
                                 return Boolean is
   begin
      for Turnout in Forced_Turnouts'Range loop
         if (Forced_Turnouts (Turnout, 0).Block = A_Block and
               Forced_Turnouts (Turnout, 0).Polarity = A_Direction) or
           (Forced_Turnouts (Turnout, 1).Block = A_Block and
              Forced_Turnouts (Turnout, 1).Polarity = A_Direction) then
            return True;
         end if;
      end loop;
      return False;
   end Is_Force_Turnout;

   ---------------------------------------------------------------------------

   procedure Get_Force_Turnout_Info (A_Block         : in Block_ID;
                                     Block_Direction : in Direction;
                                     A_Turnout       : out Turnout_ID;
                                     Set_Turnout     : out Limb_Type) is
   begin
      for Turnout in Forced_Turnouts'Range loop
         if Forced_Turnouts (Turnout, 0).Block = A_Block and
               Forced_Turnouts (Turnout, 0).Polarity = Block_Direction then
            A_Turnout := Turnout;
            Set_Turnout := Right;
            return;
         elsif  Forced_Turnouts (Turnout, 1).Block = A_Block and
                   Forced_Turnouts (Turnout, 1).Polarity = Block_Direction then
            A_Turnout := Turnout;
            Set_Turnout := Left;
            return;
         end if;
      end loop;
   end Get_Force_Turnout_Info;

   ---------------------------------------------------------------------------

   function Is_Joint_Turnout (A_Turnout : Turnout_ID; A_Limb : Limb_Type)
                              return Boolean is
   begin
      if (A_Turnout = 2 and A_Limb = Left) or
        (A_Turnout = 3 and A_Limb = Left) or
        (A_Turnout = 6 and A_Limb = Right) or
        (A_Turnout = 7 and A_Limb = Right) or
        (A_Turnout = 17 and A_Limb = Left) or
        (A_Turnout = 18 and A_Limb = Left) then
         return True;
      else
         return False;
      end if;

   end Is_Joint_Turnout;

   ----------------------------------------------------------------------------

   function Get_Joint_Turnout (A_Turnout : Turnout_ID; A_Limb : Limb_Type)
                               return Turnout_ID is
      --Returns the ID of the turnout the given turnout is connected with
      --to form a joint turnout
   begin
      if A_Turnout = 2 and A_Limb = Left then
         return 3;
      elsif A_Turnout = 3 and A_Limb = Left then
         return 2;
      elsif A_Turnout = 6 and A_Limb = Right then
         return 7;
      elsif A_Turnout = 7 and A_Limb = Right then
         return 6;
      elsif A_Turnout = 17 and A_Limb = Left then
         return 18;
      elsif A_Turnout = 18 and A_Limb = Left then
         return 17;
      else
         raise Layout_Error;
      end if;

   end Get_Joint_Turnout;

   ----------------------------------------------------------------------------

   function Get_Crossing_Blocks (A_Block : Block_ID) return Cross_Blocks_Rec is
      --Returns a list of Blocks that cross the given Block

      Cross_Blocks : Cross_Blocks_Rec;
   begin
      case A_Block is
	 when 8 =>
	    Cross_Blocks.Length := 1;
            Cross_Blocks.List   := (Block_List'First => 29);
	 when 23 =>
	    Cross_Blocks.Length := 1;
            Cross_Blocks.List   := (Block_List'First => 39);
	 when 24 =>
	    Cross_Blocks.Length := 1;
            Cross_Blocks.List   := (Block_List'First => 30);
	 when 29 =>
	    Cross_Blocks.Length := 1;
            Cross_Blocks.List   := (Block_List'First => 8);
	 when 30 =>
	    Cross_Blocks.Length := 1;
            Cross_Blocks.List   := (Block_List'First => 24);
	 when 39 =>
	    Cross_Blocks.Length := 1;
            Cross_Blocks.List   := (Block_List'First => 23);
	 when others =>
	    Cross_Blocks.Length := 0;
      end case;
      return Cross_Blocks;
   end Get_Crossing_Blocks;

   ----------------------------------------------------------------------------

   function Next_Choice_Turnout (A_Block : Block_ID; A_Direction : Direction)
                                 return Turnout_ID is
      --Returns the next choice Turnout_ID of a Block with a given direction
      Next_Block : Block_ID;
      Sensor     : Hall_Sensor;
   begin
      if End_Of_Block (Block => A_Block,
                       Side  => A_Direction) = Dead_End then
         --Undesired Base Case
         raise Layout_Error;

      elsif End_Of_Block (Block => A_Block,
                          Side  => A_Direction) = Turnout then
         --Desired Base Case
         return Next_Turnout (Block => A_Block,
                              Side  => A_Direction);

      elsif End_Of_Block (Block => A_Block,
                          Side  => A_Direction) = Block then
         Next_Block := Layout.Next_Block (Block => A_Block,
                                          Side  => A_Direction);
         Sensor := Sensor_Between_Blocks (A_Block, Next_Block);
         if Sensor_Polarity (Sensor) then --Returns False if reversing
            --recursive call
            return Next_Choice_Turnout (A_Block     => Next_Block,
                                        A_Direction => A_Direction);
         else -- if reversing node --recursive call
            return Next_Choice_Turnout (A_Block     => Next_Block,
                                        A_Direction => Opposite (A_Direction));
         end if;

      else
         raise Layout_Error;
      end if;

   end Next_Choice_Turnout;

end Layout;
