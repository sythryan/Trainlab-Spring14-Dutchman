package Train_Types is
   -- This package provides the basic types for trains.
   --
   -- Written by John McCormick, April 2002 as a single package.
   -- Broken into parent and child packages to separate the basic types
   -- from the operations in February 2008.

   -- IDs for the trains.
   -- Zero is reserved for dispatcher making requests for an engineer
   Max_Trains : constant := 3;
   type    Request_ID is            range 0 .. Max_Trains;
   subtype Train_ID   is Request_ID range 1 .. Max_Trains;

   Dispatcher : constant Request_ID := 0;
end Train_Types;
