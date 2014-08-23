with Layout; use Layout;
with Port_IO; use type Port_IO.Address_Range;
with Ada.Unchecked_Conversion;

package body Blocks is
   -- Author : Nigel
   -- Team   : The Grounded Dutchman

   --Block status
   type Status_Rec is
      record
         Reserved 	       :  Natural range 0 .. 2 := 0;
         -- Counter for reserved block need more range??
         Reserved_By 	    : Train_Types.Request_ID := 0;
         --What train is holding the reservation
         Polarity 	       : Layout.Direction := Normal;
         --Is the Block powered Normal or Reversed
      end record;

   type Block_Rec_Array is array (Layout.Block_ID) of Status_Rec;

   type Bit_Array is array (0 .. 7) of Boolean;
   for Bit_Array'Component_Size use 1;

   --------------------------------------------------------------------------

   function To_Bit_Array is new Ada.Unchecked_Conversion
     (Source => Port_IO.Byte,
      Target => Bit_Array);

   function To_Port_IO_Byte is new Ada.Unchecked_Conversion
     (Source => Bit_Array,
      Target => Port_IO.Byte);

   --------------------------------------------------------------------------
   Block_Free_Callback : Block_Free_Ptr;

   procedure Set_Block_Free_Callback (To : in Block_Free_Ptr) is
   begin
      Block_Free_Callback := To;
   end Set_Block_Free_Callback;

   --------------------------------------------------------------------------

   protected Blocks_Manager is
      procedure Reserve (Requestor : in Train_Types.Request_ID;
                         Block     : in Layout.Block_ID;
                         Success   : out Boolean);
      procedure Free (Requestor : in Train_Types.Request_ID;
                      Block     : in Layout.Block_ID);
      procedure Power (Block     :  Layout.Block_ID;
                       Port_IO_Address     : Port_IO.Address_Range;
                       Cab_Number          : Cabs.Cab_ID;
                       Direction           : Layout.Direction);
      procedure Block_Info (Block     : in Layout.Block_ID;
                            Port_IO_Address     : Port_IO.Address_Range;
                            Cab_Number          : out Cabs.Cab_ID;
                            Direction           : out Layout.Direction);
   private
      Block_Statuses : Block_Rec_Array;
   end Blocks_Manager;

   ----------------------------------------------------------------------------
   protected body Blocks_Manager is

      ----------------------------------------------------
      procedure Reserve (Requestor : in Train_Types.Request_ID;
                         Block     : in Layout.Block_ID;
                         Success   : out Boolean) is
         --if a block is currently reserved by the requestor Success := True is
         --passed back, else the Reservation must be false in order to set the
         --reservation and Success := True
         Cross_Blocks_List    : Layout.Cross_Blocks_Rec;
      begin
         if Block_Statuses (Block).Reserved > 0 then
	    if Block_Statuses (Block).Reserved_By = Requestor then
	       if Block_Statuses (Block).Reserved = 2 then
		  Success := False;
	       else
		  Block_Statuses (Block).Reserved :=
		    Block_Statuses (Block).Reserved + 1;
		  Success := True;
	       end if;
            else
	       Success := False;
            end if;
	 else
            Block_Statuses (Block).Reserved :=
              Block_Statuses (Block).Reserved + 1;
            Block_Statuses (Block).Reserved_By := Requestor;
            Success := True;
         end if;

         if Success then
            -- check for cross blocks
            Cross_Blocks_List :=
              Layout.Get_Crossing_Blocks (A_Block => Block);
            if Cross_Blocks_List.Length > 0 then
               for Index in 1 .. Cross_Blocks_List.Length loop
		  if Block_Statuses (Cross_Blocks_List.List (Index))
		    .Reserved > 0 then
                     if Block_Statuses (Cross_Blocks_List.List (Index))
                       .Reserved_By = Requestor then
                        if Block_Statuses (Cross_Blocks_List.List (Index))
                          .Reserved = 2 then
                           Success := False;
                        else
			   Block_Statuses
			     (Cross_Blocks_List.List (Index)).Reserved :=
			     Block_Statuses
			       (Cross_Blocks_List.List (Index)).Reserved + 1;
                           Success := True;
                        end if;
                     else
                        Success := False;
                     end if;
                  else
                     Block_Statuses (Cross_Blocks_List.List (Index)).Reserved :=
		       Block_Statuses
			 (Cross_Blocks_List.List (Index)).Reserved + 1;
		     Block_Statuses (Cross_Blocks_List.List (Index))
		       .Reserved_By := Requestor;
                     Success := True;
                  end if;
               end loop;
            end if;
	 end if;
      end Reserve;

      -------------------------------------------------------------
      procedure Free (Requestor : in Train_Types.Request_ID;
                      Block     : in Layout.Block_ID) is
         --Block can be freed by Dispactcher or train that had reserved block in
         --the first place

         Cross_Blocks_List    : Layout.Cross_Blocks_Rec;
      begin
         if Requestor = 0 then
            Block_Statuses (Block).Reserved := 0;
	 else
	    if Block_Statuses (Block).Reserved_By = Requestor then
	       if Block_Statuses (Block).Reserved > 0 then
		  Block_Statuses (Block).Reserved :=
		    Block_Statuses (Block).Reserved - 1;
	       end if;
	    end if;
	 end if;
	 -- if block is finally free send call back
	 if Block_Statuses (Block).Reserved = 0 then
	    Block_Free_Callback.all (Block);
         end if;

         -- Check for Cross Blocks
         Cross_Blocks_List :=
	Layout.Get_Crossing_Blocks (A_Block => Block);
         if Cross_Blocks_List.Length > 0 then
            for Index in 1 .. Cross_Blocks_List.Length loop
	       if Block_Statuses (Cross_Blocks_List.List (Index))
		 .Reserved_By = Requestor then
		  if Block_Statuses (Cross_Blocks_List.List (Index))
		    .Reserved > 0 then
		     Block_Statuses (Cross_Blocks_List.List (Index)).Reserved :=
		       Block_Statuses
			 (Cross_Blocks_List.List (Index)).Reserved - 1;
                  end if;
               end if;
               -- if block is finally free send call back
	       if Block_Statuses
		 (Cross_Blocks_List.List (Index)).Reserved = 0 then
                  Block_Free_Callback.all (Block);
               end if;
            end loop;
	 end if;
      end Free;

      ---------------------------------------------------------------
      procedure Power (Block     :  Layout.Block_ID;
                       Port_IO_Address     : Port_IO.Address_Range;
                       Cab_Number          : Cabs.Cab_ID;
                       Direction           : Layout.Direction) is

         Data_Byte	   : Bit_Array;
      begin

         --Read Current Register Setting in
         Data_Byte := To_Bit_Array (Port_IO.In_Byte (Port_IO_Address));

         --Set nibble
         if Block rem 2 = 0 then
            --check if low nibble
            if Cab_Number = 0 then
               Data_Byte (0) := False;
               Data_Byte (1) := False;
               Data_Byte (2) := False;
            elsif Cab_Number = 1 then
               Data_Byte (0) := True;
               Data_Byte (1) := False;
               Data_Byte (2) := False;
            elsif Cab_Number = 2 then
               Data_Byte (0) := False;
               Data_Byte (1) := True;
               Data_Byte (2) := False;
            elsif Cab_Number = 3 then
               Data_Byte (0) := True;
               Data_Byte (1) := True;
               Data_Byte (2) := False;
            elsif Cab_Number = 4 then
               Data_Byte (0) := False;
               Data_Byte (1) := False;
               Data_Byte (2) := True;
            elsif Cab_Number = 5 then
               Data_Byte (0) := True;
               Data_Byte (1) := False;
               Data_Byte (2) := True;
            elsif Cab_Number = 6 then
               Data_Byte (0) := False;
               Data_Byte (1) := True;
               Data_Byte (2) := True;
            elsif Cab_Number = 7 then
               Data_Byte (0) := True;
               Data_Byte (1) := True;
               Data_Byte (2) := True;
            end if;
            --Set Polarity for low nibble
            if Direction = Normal then
               Data_Byte (3) := False;
            elsif Direction = Reversed then
               Data_Byte (3) := True;
            end if;
         elsif Block rem 2 = 1 then
            --check if high nibble
            if Cab_Number = 0 then
               Data_Byte (4) := False;
               Data_Byte (5) := False;
               Data_Byte (6) := False;
            elsif Cab_Number = 1 then
               Data_Byte (4) := True;
               Data_Byte (5) := False;
               Data_Byte (6) := False;
            elsif Cab_Number = 2 then
               Data_Byte (4) := False;
               Data_Byte (5) := True;
               Data_Byte (6) := False;
            elsif Cab_Number = 3 then
               Data_Byte (4) := True;
               Data_Byte (5) := True;
               Data_Byte (6) := False;
            elsif Cab_Number = 4 then
               Data_Byte (4) := False;
               Data_Byte (5) := False;
               Data_Byte (6) := True;
            elsif Cab_Number = 5 then
               Data_Byte (4) := True;
               Data_Byte (5) := False;
               Data_Byte (6) := True;
            elsif Cab_Number = 6 then
               Data_Byte (4) := False;
               Data_Byte (5) := True;
               Data_Byte (6) := True;
            elsif Cab_Number = 7 then
               Data_Byte (4) := True;
               Data_Byte (5) := True;
               Data_Byte (6) := True;
            end if;
            --Set Polarity for high nibble
            if Direction = Normal then
               Data_Byte (7) := False;
            elsif Direction = Reversed then
               Data_Byte (7) := True;
            end if;
         else
            raise Block_Error;
         end if;

         --Write out Byte
         Port_IO.Out_Byte (Address => Port_IO_Address,
			   Data    => To_Port_IO_Byte (Data_Byte));
      end Power;

      ------------------------------------------------------------------------

      procedure Block_Info (Block               : in Layout.Block_ID;
                            Port_IO_Address     : in Port_IO.Address_Range;
                            Cab_Number          : out Cabs.Cab_ID;
                            Direction           : out Layout.Direction) is
         Data_Byte	    : Bit_Array;
         -- Hard Coded Values for the diffrent Cabs to compare against
         --the Data_Byte
         Cab_0_Comp : constant Bit_Array :=
		(False, False, False, False, False, False, False, False);
         Cab_1_Comp	   : constant Bit_Array :=
			(True, False, False, False, False, False, False, False);
         Cab_2_Comp	   : constant Bit_Array :=
                        (False, True, False, False, False, False, False, False);
         Cab_3_Comp	   : constant Bit_Array :=
                        (True, True, False, False, False, False, False, False);
         Cab_4_Comp	   : constant Bit_Array :=
                        (False, False, True, False, False, False, False, False);
         Cab_5_Comp	   : constant Bit_Array :=
                         (True, False, True, False, False, False, False, False);
         Cab_6_Comp	   : constant Bit_Array :=
                         (False, True, True, False, False, False, False, False);
         Cab_7_Comp	   : constant Bit_Array :=
                          (True, True, True, False, False, False, False, False);

      begin

         --Read Current Register Setting in
         Data_Byte := To_Bit_Array (Port_IO.In_Byte (Port_IO_Address));
         --Compare Cab Values
         if Block rem 2 = 0 then
            -- Set Cab Number to return
            if Data_Byte (0 .. 2) = Cab_0_Comp (0 .. 2) then
               Cab_Number := 0;
            elsif Data_Byte (0 .. 2) = Cab_1_Comp (0 .. 2) then
               Cab_Number := 1;
            elsif  Data_Byte (0 .. 2) = Cab_2_Comp (0 .. 2) then
               Cab_Number := 2;
            elsif Data_Byte (0 .. 2) = Cab_3_Comp (0 .. 2) then
               Cab_Number := 3;
            elsif Data_Byte (0 .. 2) = Cab_4_Comp (0 .. 2) then
               Cab_Number := 4;
            elsif Data_Byte (0 .. 2) = Cab_5_Comp (0 .. 2) then
               Cab_Number := 5;
            elsif Data_Byte (0 .. 2) = Cab_6_Comp (0 .. 2) then
               Cab_Number := 6;
            elsif Data_Byte (0 .. 2) = Cab_7_Comp (0 .. 2) then
               Cab_Number := 7;
            else
               raise Block_Error;
            end if;

            --Set Direction to Return
            if Data_Byte (3) = False then
               Direction := Normal;
            else
               Direction := Reversed;
            end if;

         else
            -- Set Cab Number to return
            if Data_Byte (4 .. 6) = Cab_0_Comp (0 .. 2) then
               Cab_Number := 0;
            elsif Data_Byte (4 .. 6) = Cab_1_Comp (0 .. 2) then
               Cab_Number := 1;
            elsif Data_Byte (4 .. 6) = Cab_2_Comp (0 .. 2) then
               Cab_Number := 2;
            elsif Data_Byte (4 .. 6) = Cab_3_Comp (0 .. 2) then
               Cab_Number := 3;
            elsif Data_Byte (4 .. 6) = Cab_4_Comp (0 .. 2) then
               Cab_Number := 4;
            elsif Data_Byte (4 .. 6) = Cab_5_Comp (0 .. 2) then
               Cab_Number := 5;
            elsif Data_Byte (4 .. 6) = Cab_6_Comp (0 .. 2) then
               Cab_Number := 6;
            elsif Data_Byte (4 .. 6) = Cab_7_Comp (0 .. 2) then
               Cab_Number := 7;
            else
               raise Block_Error;
            end if;

            --Set Direction to Return
            if Data_Byte (7) = False then
               Direction := Normal;
            else
               Direction := Reversed;
            end if;
	 end if;
      end Block_Info;
      ---------------------------------------------------------------------

   end Blocks_Manager;

   -----------------------------------------------------------------------------
   --ALL PROCEDURES BELOW HERE ARE PASSED TO BLOCK_MANAGER FOR PROTECTION

   procedure Reserve_Block (Requestor : in Train_Types.Request_ID;
                            Block     : in Layout.Block_ID;
			    Success   : out Boolean) is
   begin
      Blocks_Manager.Reserve (Requestor => Requestor,
                              Block     => Block,
			      Success   => Success);
   end Reserve_Block;

   --------------------------------------------------------------------------
   procedure Free_Block (Requestor : in Train_Types.Request_ID;
                         Block     : in Layout.Block_ID) is

      Base : Port_IO.Address_Range;
   begin
      --Display.Put_Error (Error_Message => "Attempt to Free Block");
      --Set Base Address
      if Block <= 12 then
         Base := 16#200#;
      elsif Block <= 24 then
         Base := 16#208#;
      elsif Block <= 36 then
         Base := 16#210#;
      else
         Base := 16#218#;
      end if;

      --Set Port Offset
      if Block rem 12 = 3 or Block rem 12 = 4 then
         --skip +0
         Base := Base + Port_IO.Address_Range (1);
      elsif Block rem 12 = 5 or Block rem 12 = 6 then
         Base := Base + Port_IO.Address_Range (2);
      elsif Block rem 12 = 7 or Block rem 12 = 8 then
         --Skip +3 offset due to config port
         Base := Base + Port_IO.Address_Range (4);
      elsif Block rem 12 = 9 or Block rem 12 = 10 then
         Base := Base + Port_IO.Address_Range (5);
      elsif Block rem 12 = 11 or Block rem 12 = 0 then
         Base := Base + Port_IO.Address_Range (6);
      end if;

      Power_Block (Block      => Block,
                   Cab_Number => 0,
                   Direction  => Normal);

      Blocks_Manager.Free (Requestor => Requestor,
			   Block     => Block);
   end Free_Block;

   --------------------------------------------------------------------------
   procedure Power_Block (Block	     : Layout.Block_ID;
                          Cab_Number : Cabs.Cab_ID;
                          Direction  : Layout.Direction) is
      Base	        : Port_IO.Address_Range;
   begin
      --Set Base Address
      if Block <= 12 then
         Base := 16#200#;
      elsif Block <= 24 then
         Base := 16#208#;
      elsif Block <= 36 then
         Base := 16#210#;
      else
         Base := 16#218#;
      end if;

      --Set Port Offset
      if Block rem 12 = 3 or Block rem 12 = 4 then
         --skip +0
         Base := Base + Port_IO.Address_Range (1);
      elsif Block rem 12 = 5 or Block rem 12 = 6 then
         Base := Base + Port_IO.Address_Range (2);
      elsif Block rem 12 = 7 or Block rem 12 = 8 then
         --Skip +3 offset due to config port
         Base := Base + Port_IO.Address_Range (4);
      elsif Block rem 12 = 9 or Block rem 12 = 10 then
         Base := Base + Port_IO.Address_Range (5);
      elsif Block rem 12 = 11 or Block rem 12 = 0 then
         Base := Base + Port_IO.Address_Range (6);
      end if;

      Blocks_Manager.Power (Block     => Block,
                            Port_IO_Address => Base,
                            Cab_Number      => Cab_Number,
                            Direction       => Direction);
   end Power_Block;
   ----------------------------------------------------------------------------
   procedure Get_Block_Info (Block      : in Layout.Block_ID;
                             Cab_Number : out Cabs.Cab_ID;
                             Direction  : out Layout.Direction) is
      Base	        : Port_IO.Address_Range;
   begin
      --Set Base Address
      if Block <= 12 then
         Base := 16#200#;
      elsif Block <= 24 then
         Base := 16#208#;
      elsif Block <= 36 then
         Base := 16#210#;
      else
         Base := 16#218#;
      end if;

      --Set Port Offset
      if Block rem 12 = 3 or Block rem 12 = 4 then
         --skip +0
         Base := Base + Port_IO.Address_Range (1);
      elsif Block rem 12 = 5 or Block rem 12 = 6 then
         Base := Base + Port_IO.Address_Range (2);
      elsif Block rem 12 = 7 or Block rem 12 = 8 then
         --Skip +3 offset due to config port
         Base := Base + Port_IO.Address_Range (4);
      elsif Block rem 12 = 9 or Block rem 12 = 10 then
         Base := Base + Port_IO.Address_Range (5);
      elsif Block rem 12 = 11 or Block rem 12 = 0 then
         Base := Base + Port_IO.Address_Range (6);
      end if;

      Blocks_Manager.Block_Info (Block           => Block,
                                 Port_IO_Address => Base,
                                 Cab_Number      => Cab_Number,
                                 Direction       => Direction);

   end Get_Block_Info;
   ----------------------------------------------------------------------------
   procedure init is
      --initializes the ports to output
      type Address_Array is array (0 .. 3) of Port_IO.Address_Range;
      --addresses of config ports
      Addresses_To_Set_For_Output : constant Address_Array :=
                                      (16#200#, 16#208#, 16#210#, 16#218#);

   begin
      for Address in Addresses_To_Set_For_Output'Range loop
         Port_IO.Out_Byte (Address => Addresses_To_Set_For_Output (Address) +
                             Port_IO.Address_Range (3),
                           Data    => 128); --this number sets all ports to out
         Port_IO.Out_Byte (Address => Addresses_To_Set_For_Output (Address) +
                             Port_IO.Address_Range (7),
                           Data    => 128);
      end loop;
   end init;
   ----------------------------------------------------------------------------
begin
   init;
   --Init programs chips so that ports are all set as output ports
end Blocks;
