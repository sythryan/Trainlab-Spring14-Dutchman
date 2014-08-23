with Layout;
package body Layout.Search is
-- Author: Syth Ryan
-- Team  : The Grounded Dutchman

   --------------------
   -- Blocks_Beneath --
   --------------------

   procedure Blocks_Beneath (Loco     : in  Block_ID;
                             Caboose  : in  Block_ID;
                             Blocks   : out Block_List;
                             Turnouts : out Turnout_List;
                             Success  : out Boolean) is

      --------------------------------------------------------------------------
      procedure Insert_To_Block_List (Current : in Block_ID;
				      Value   : in Block_ID;
				      A_Direction : in Direction;
                                      A_List  : in out Block_List) is
      begin
         A_List.Size := A_List.Size + 1;
	 A_List.Items (A_List.Size).Block := Value;
	 -- change direction if needed
	 if Sensor_Polarity (Sensor_Between_Blocks (Block_One => Current,
					            Block_Two => Value)) then
	    A_List.Items (A_List.Size).My_Direction := A_Direction;
	 else
	    A_List.Items (A_List.Size).My_Direction :=
	      Layout.Opposite (A_Direction);
	 end if;
      end Insert_To_Block_List;

      --------------------------------------------------------------------------
      procedure Remove_From_Block_List (A_List : in out Block_List) is
      begin
         A_List.Size := A_List.Size - 1;
      end Remove_From_Block_List;

      --------------------------------------------------------------------------
      procedure Insert_To_Turnout_List (Value  : in Turnout_ID;
					Switch : in Turn_Choice;
                                        A_List : in out Turnout_List) is
      begin
	 A_List.Size := A_List.Size + 1;
	 A_List.Items (A_List.Size).Turnout := Value;
	 A_List.Items (A_List.Size).Direction := Switch;
      end Insert_To_Turnout_List;

      --------------------------------------------------------------------------
      procedure Recursive_Blocks_Beneath (Loco        : in Block_ID;
                                          Current     : in out Block_ID;
                                          Blocks      : in out Block_List;
                                          Turnouts    : in out Turnout_List;
                                          Success     : out Boolean;
                                          A_Direction : in Direction) is

	 Current_Turnout : Turnout_ID;
	 Terminator      : Layout.Terminator_Type;
	 Left_Limb       : Layout.Block_ID;
	 Right_Limb      : Layout.Block_ID;
      begin

	 if Blocks.Size = Blocks.Max_Size then
	    -- Base Case: We've Reached our size limit
	    Success := False;
	 else
	    Terminator := End_Of_Block (Block  => Current,
				 Side   => A_Direction);
	    case Terminator is
		  -- General Case: Keep searching
	       when Block =>
		  -- Next Terminator is a block
		  if Next_Block (Block => Current,
		   Side  => A_Direction) = Loco then
		     -- Base Case: Loco was found
		     Insert_To_Block_List (Current     => Current,
			     Value       => Loco,
			     A_Direction => A_Direction,
			     A_List      => Blocks);
		     Success := True;
		  else
		     Insert_To_Block_List
		       (Current     => Current,
	                Value       => Next_Block (Block => Current,
			                           Side  => A_Direction),
	                A_Direction => A_Direction,
	                A_List      => Blocks);
		     Current := Blocks.Items (Blocks.Size).Block;
		     Recursive_Blocks_Beneath (Loco        => Loco,
				 Current     => Current,
				 Blocks	     => Blocks,
				 Turnouts    => Turnouts,
				 Success     => Success,
				 A_Direction => A_Direction);
		     if not Success then
			Remove_From_Block_List (A_List => Blocks);
		     end if;
		  end if;
	       when Turnout =>
		  -- Next Terminator is a Turnout
		  Current_Turnout := Next_Turnout (Block => Current,
				     Side  => A_Direction);
		  -- Persue left limb
		  Left_Limb := Limb_Of_Turnout (Turnout => Current_Turnout,
			                   Limb    => Left);
		  if Left_Limb /= 40 then
		     Insert_To_Block_List    -- Adds block on left limb
		       (Current     => Current,
	                Value       => Left_Limb,
	                A_Direction => A_Direction,
	                A_List      => Blocks);
		     if Left_Limb = Loco then
			-- Base Case: We've found Loco
			Success := True;
			-- Left Limb was successful, add Turnout to List
			Insert_To_Turnout_List (Value  => Current_Turnout,
			   Switch => Left,
			   A_List => Turnouts);
		     else
			Current  := Left_Limb;
			Recursive_Blocks_Beneath (Loco        => Loco,
			     Current     => Current,
			     Blocks	 => Blocks,
			     Turnouts    => Turnouts,
			     Success     => Success,
			     A_Direction => A_Direction);

		     end if;
		  else
		     Success := False;
		  end if;
		  if not Success then
		     Right_Limb := Limb_Of_Turnout (Turnout => Current_Turnout,
				    Limb    => Right);
		     if Right_Limb /= 40 then
			-- Loco was not found on left limb, persue Right limb
			-- Take off left limb that was added
			Remove_From_Block_List (A_List => Blocks);
			Current := Blocks.Items (Blocks.Size).Block;
			Insert_To_Block_List    -- Adds block on right limb
			  (Current     => Current,
                           Value       => Right_Limb,
                           A_Direction => A_Direction,
                           A_List      => Blocks);
			if Right_Limb = Loco then
			   -- Base Case: We've found Loco
			   Success := True;
			   -- Right limb was successful, add Turnout to List
			   Insert_To_Turnout_List (Value  => Current_Turnout,
			      Switch => Right,
			      A_List => Turnouts);
			else
			   Current  := Right_Limb;
			   Recursive_Blocks_Beneath (Loco        => Loco,
				Current     => Current,
				Blocks	     => Blocks,
				Turnouts    => Turnouts,
				Success     => Success,
				A_Direction => A_Direction);
			   if Success then
			      -- Right limb was successful, add Turnout to List
			      Insert_To_Turnout_List (Value  => Current_Turnout,
				 Switch => Right,
				 A_List => Turnouts);
			   else
			      if Right_Limb /= 40 then
				 -- Take off right limb that was added
				 Remove_From_Block_List (A_List => Blocks);
			      end if;
			   end if;
			end if;
		     else
			Success := False;
		     end if;
		  else
		     -- Left Limb was successful, add Turnout to List
		     Insert_To_Turnout_List (Value  => Current_Turnout,
			       Switch => Left,
			       A_List => Turnouts);
		  end if;
	       when Dead_End =>
		  -- Base Case: We've Reached a Dead end
		  Success := False;
	    end case;
	 end if;
      end Recursive_Blocks_Beneath;
   -----------------------------------------------------------------------------
      Current      : Block_ID;

   begin
      Current  := Caboose;
      Success  := False;
      Blocks.Size := 1;
      Blocks.Items (1).Block := Caboose;
      Blocks.Items (1).My_Direction := Normal;

      Recursive_Blocks_Beneath (Loco        => Loco,
                                Current     => Current,
                                Blocks      => Blocks,
                                Turnouts    => Turnouts,
                                Success     => Success,
                                A_Direction => Normal);

      if not Success then
	 -- Loco still not found, persue reversed direction of caboose
	 Current       := Caboose;
	 Blocks.Size   := 1;
         Turnouts.Size := 0;
         Blocks.Items (1).My_Direction := Reversed;

         Recursive_Blocks_Beneath (Loco        => Loco,
                                   Current     => Current,
                                   Blocks      => Blocks,
                                   Turnouts    => Turnouts,
                                   Success     => Success,
                                   A_Direction => Reversed);
      end if;
   end Blocks_Beneath;
end Layout.Search;
