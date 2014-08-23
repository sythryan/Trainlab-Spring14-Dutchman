with Layout;
with Train_Types; use Train_Types;
with Cabs; use Cabs;

package Blocks is

   -- This package provides operations for the blocks

   -- Written by Nigel Ploof, April 2014


   ----------------------------------------------------------------------------
   Block_Error : exception;

   type Block_Free_Ptr is access procedure (Block  : in Layout.Block_ID);
   procedure Set_Block_Free_Callback (To : in Block_Free_Ptr);

   -- This procedure sets the procedure that the Blocks package will call
   --   when a Block fails to Reserve
   -- If no callback is set, no call is made when a Reservation fails
   -- Calling this procedure with a null value for To removes any previous
   --   setting of the callback procedure for a failed reservation

   -----------------------------------------------------------------------------
   procedure Reserve_Block (Requestor : in Train_Types.Request_ID;
                            Block     : in Layout.Block_ID;
                            Success   : out Boolean);
   -- Reserves a Block
   --
   -- Potentially blocking (rendezvous with a Block task or protected item)
   --
   -- Preconditions  : None
   --
   -- Postconditions : An attempt is made to Reserve a block and
   --						set the polarity.
   --
   --                  If Reserve is successful the callback
   --                  procedure set by Set_Reserve_Callback is called with True
   --
   --                  If a block is not able to be Reserved a False is sent to
   --					the Set_Reserve_Call back.

   -----------------------------------------------------------------------------


   procedure Free_Block (Requestor : in Train_Types.Request_ID;
                         Block     : in Layout.Block_ID);
   -- Frees a Block
   --
   -- Potentially blocking (rendezvous with a Block task or protected item)
   --
   -- Preconditions  : None
   --
   -- Postconditions : An attempt is made to free a Block, block record will
   --			show block free.


   procedure Power_Block (Block	: Layout.Block_ID;
                          Cab_Number : Cabs.Cab_ID;
                          Direction  : Layout.Direction);

   -- Powers an already reserved block
   --
   -- Preconditions  : None
   --
   -- Postconditions : If successful the Block will be powered in the requested
   --			direction.

   procedure Get_Block_Info (Block : in Layout.Block_ID;
                             Cab_Number : out Cabs.Cab_ID;
                             Direction  : out Layout.Direction);

   -- Retrives Cab and direction info from block hardware
   --
   -- Preconditions : None
   --
   -- Postcondtions : No change is implemented, bytes are only read
   --


end Blocks;
