-- Author: Anthony Spoerl
-- Team:   Grounded Dutchman
-- Date:   4/15/2014

-- *** Comment these out if not running under MaRTE
with MaRTE_OS;
pragma Warnings (Off, MaRTE_OS);
-- ***

with Ada.Text_IO;
with Trains;
with Layout; use Layout;
with Blocks; use Blocks;
with Cabs;

procedure blocks_test is
   package Block_IO is new Ada.Text_IO.Integer_IO (Layout.Block_ID);

   procedure Test_1 is
      -- Test successfully reserving blocks
      Passed     : Boolean;
      All_Passed : Boolean := True;
   begin
      Ada.Text_IO.Put_Line ("Beginning Test 1:");

      for Index in Layout.Block_ID loop
         Reserve_Block (Requestor => 0,
                        Block     => Index,
                        Success   => Passed);

         if Passed then
            Ada.Text_IO.Put ("Block ");
            Block_IO.Put (Index);
            Ada.Text_IO.Put_Line (" has been reserved.");
         else
            Ada.Text_IO.Put ("*** Block ");
            Block_IO.Put (Index);
            Ada.Text_IO.Put_Line (" failed reservation ***");

            All_Passed := False;
         end if;
      end loop;

      if All_Passed then
         Ada.Text_IO.Put_Line ("Test 1: Passed.");
      else
         Ada.Text_IO.Put_Line ("Test 1: ERROR: One or more tests failed.");
      end if;
   end Test_1;

   procedure Test_2 is
      -- Test reserving blocks that are already reserved fails
      -- Precondition: All blocks are reserved by Requestor 0
      Passed     : Boolean;
      All_Passed : Boolean := True;
   begin
      Ada.Text_IO.Put_Line ("Beginning Test 2:");
      for Index in Layout.Block_ID loop
         Reserve_Block (Requestor => 1,
                        Block     => Index,
                        Success   => Passed);

         if not Passed then
            Ada.Text_IO.Put ("Block ");
            Block_IO.Put (Index);
            Ada.Text_IO.Put_Line (" could not be reserved.");
         else
            Ada.Text_IO.Put ("*** Block ");
            Block_IO.Put (Index);
            Ada.Text_IO.Put_Line (" was reserved again ***");

            All_Passed := False;
	 end if;
      end loop;

      if All_Passed then
         Ada.Text_IO.Put_Line ("Test 2: Passed.");
      else
         Ada.Text_IO.Put_Line ("Test 2: ERROR: One or more tests failed.");
      end if;
   end Test_2;

   procedure Test_3 is
      -- Test powering blocks
   begin
      Ada.Text_IO.Put_Line ("Beginning Test 3:");

      for Test_Block in Layout.Block_ID loop


	    Power_Block (Block      => Test_Block,
		  Cab_Number => 1,
		  Direction  => Reversed);

	    Ada.Text_IO.Put ("Block ");
	    Block_IO.Put (Test_Block);
	    Ada.Text_IO.Put_Line (" is being powered by cab 1 (Reversed)");
	    Ada.Text_IO.Skip_Line;

	    Power_Block (Block      => Test_Block,
		  Cab_Number => 2,
		  Direction  => Normal);
	    Ada.Text_IO.Put ("Block ");
	    Block_IO.Put (Test_Block);
	    Ada.Text_IO.Put_Line (" is being powered by cab 2");
	    Ada.Text_IO.Skip_Line;

	    Power_Block (Block      => Test_Block,
		  Cab_Number => 3,
		  Direction  => Normal);
	    Ada.Text_IO.Put ("Block ");
	    Block_IO.Put (Test_Block);
	    Ada.Text_IO.Put_Line (" is being powered by cab 3");
	    Ada.Text_IO.Skip_Line;

	    Power_Block (Block      => Test_Block,
		  Cab_Number => 4,
		  Direction  => Normal);
	    Ada.Text_IO.Put ("Block ");
	    Block_IO.Put (Test_Block);
	    Ada.Text_IO.Put_Line (" is being powered by cab 4");
	    Ada.Text_IO.Skip_Line;

	    Power_Block (Block      => Test_Block,
		  Cab_Number => 5,
		  Direction  => Normal);
	    Ada.Text_IO.Put ("Block ");
	    Block_IO.Put (Test_Block);
	    Ada.Text_IO.Put_Line (" is being powered by cab 5");
	    Ada.Text_IO.Skip_Line;

	    Power_Block (Block      => Test_Block,
		  Cab_Number => 6,
		  Direction  => Normal);
	    Ada.Text_IO.Put ("Block ");
	    Block_IO.Put (Test_Block);
	    Ada.Text_IO.Put_Line (" is being powered by cab 6");
	    Ada.Text_IO.Skip_Line;

  	    Free_Block (Requestor  => 0,
  		 Block      => Test_Block);

	    Ada.Text_IO.Put ("Block ");
	    Block_IO.Put (Test_Block);
	    Ada.Text_IO.Put_Line (" freed. Should no longer be powered.");
	    Ada.Text_IO.Skip_Line;


      end loop;

      Ada.Text_IO.Put_Line ("End Test 3.");
   end Test_3;

   procedure Test_4 is
      -- Test freeing all blocks (Succesfully)
      -- Precodition: All blocks have been reserved
      First_Requestor  : constant Train_Types.Request_ID := 0;
      Second_Requestor : constant Train_Types.Request_ID := 1;
      All_Passed       : Boolean := True;
      Passed           : Boolean;
   begin
      Ada.Text_IO.Put_Line ("Beginning Test 4:");

      for Index in Layout.Block_ID loop
	 -- Reserve a block by some initial requestor
	 Reserve_Block (Requestor => First_Requestor,
		        Block     => Index,
		        Success   => Passed);

         if not Passed then
            Ada.Text_IO.Put_Line ("Failed initial reservation");
         end if;

	 Free_Block (Requestor => First_Requestor,
	             Block     => Index);

	 -- Check that another requestor can now reserve the block
	 Reserve_Block (Requestor => Second_Requestor,
		        Block     => Index,
		        Success   => Passed);

	 if Passed then
	    Ada.Text_IO.Put ("Block ");
	    Block_IO.Put (Index);
	    Ada.Text_IO.Put_Line (" has been reserved.");
	 else
	    Ada.Text_IO.Put ("*** Block ");
	    Block_IO.Put (Index);
	    Ada.Text_IO.Put_Line (" failed reservation ***");

	    All_Passed := False;
	 end if;
      end loop;

      if All_Passed then
         Ada.Text_IO.Put_Line ("Test 4: Passed.");
      else
         Ada.Text_IO.Put_Line ("Test 4: ERROR: One or more tests failed.");
      end if;
   end Test_4;


   --###################### This test is not needed now McCormick thinks a check
   -- to see if what reserved the block is the one trying to power it will
   --Unduely slow down the response time so this requirement has been removed
   --#####################################################################

--     procedure Test_5 is
--        -- Test attempting to power an unreserved block
--        Test_Block : constant Layout.Block_ID := 2;
--        Failed     : Boolean := True;
--     begin
--        begin
--  	 Ada.Text_IO.Put_Line ("Beginning Test 5:");
--
--  	 Free_Block (Requestor => 0,
--  	      Block     => Test_Block);
--
--  	 Power_Block (Block      => Test_Block,
--  	       Cab_Number => 1,
--  	       Direction  => Reversed);
--
--        exception
--  	 when Block_Error =>
--  	    Ada.Text_IO.Put ("Block ");
--  	    Block_IO.Put (Test_Block);
--  	    Ada.Text_IO.Put_Line
--  	      (" should not be powered. Check that powered LED is off.");
--  	    Failed := False;
--        end;
--
--        if Failed then
--  	 Ada.Text_IO.Put_Line ("*** Expected block exception ***");
--        end if;
--
--        Ada.Text_IO.Put_Line ("End Test 5.");
--     end Test_5;

   procedure Test_6 is
      -- Test freeing blocks reserved multiple times by dispatcher and caller
      Test_Block  : constant Layout.Block_ID := 23;
      Dispatcher  : constant Train_Types.Request_ID := 0;
      Train       : constant Train_Types.Request_ID := 1;
      Passed      : Boolean;
   begin
      -- Reserve Test_Block twice
      Reserve_Block (Requestor => Train,
		     Block     => Test_Block,
		     Success   => Passed);
      Reserve_Block (Requestor => Train,
		     Block     => Test_Block,
		     Success   => Passed);

      Free_Block (Requestor => Train,
		  Block     => Test_Block);

      Reserve_Block (Requestor => Dispatcher,
		     Block     => Test_Block,
		     Success   => Passed);
      -- Test_Block should still be reserved by Train, so this call should fail
      if Passed then
	 Ada.Text_IO.Put_Line ("*** Failed multiple reserveations. ***");
      end if;

      -- Reserve Test_Block twice again
      Reserve_Block (Requestor => Train,
		     Block     => Test_Block,
		     Success   => Passed);

      Free_Block (Requestor => Dispatcher,
		  Block     => Test_Block);

      Reserve_Block (Requestor => Dispatcher,
		     Block     => Test_Block,
		     Success   => Passed);

      if not Passed then
	 Ada.Text_IO.Put_Line
	   ("*** Dispatcher failed to free all reservations. ***");
      end if;

      Ada.Text_IO.Put_Line ("Test 6 Complete.");
   end Test_6;

begin
   Ada.Text_IO.Put_Line ("Block Tests:");

   -- Iniialize cabs for powering blocks
   Cabs.Set_Limit (Cab   => 1,
		   Value => 100);
   Cabs.Set_Limit (Cab   => 2,
		   Value => 100);
   Cabs.Set_Limit (Cab   => 3,
		   Value => 100);
   Cabs.Set_Limit (Cab   => 4,
		   Value => 100);
   Cabs.Set_Limit (Cab   => 5,
		   Value => 100);
   Cabs.Set_Limit (Cab   => 6,
		   Value => 100);
   Cabs.Set (Cab   => 1,
	     Value => 100);
   Cabs.Set (Cab   => 2,
	     Value => 100);
   Cabs.Set (Cab   => 3,
	     Value => 80);
   Cabs.Set (Cab   => 4,
	     Value => 60);
   Cabs.Set (Cab   => 5,
	     Value => 40);
   Cabs.Set (Cab   => 6,
	     Value => 20);

   Test_1;
   Ada.Text_IO.Skip_Line;
   Test_2;
   Ada.Text_IO.Skip_Line;
   Test_3;
   Ada.Text_IO.Skip_Line;
   Test_4;
   Ada.Text_IO.Skip_Line;
   --Test_5;
   --Ada.Text_IO.Skip_Line;
   Test_6;

   Ada.Text_IO.Put_Line ("Block testing complete.");
end blocks_test;
