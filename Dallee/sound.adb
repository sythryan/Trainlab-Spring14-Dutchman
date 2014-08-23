-- Author: Nigel Ploof
-- Team:   Grounded Dutchman
with Dallee; use Dallee;
with Double_Talk; use Double_Talk;
package body Sound is

   type Pattern_Array is array (0 .. 4) of Integer range 0 .. 6;
   type Horn_Signals_Array is array (Horn_Signal) of Pattern_Array;
   Horn_Signals : constant Horn_Signals_Array :=
                    (Stop              => (1, others => 0),
                     Start             => (3, 3, others => 0),
                     Approach_Highway  => (3, 3, 1, 3, others => 0),
                     Approach_Crossing => (3, 3, 1, others => 0),
                     Warning           => (1, 1, 1, 1, others => 0)
                    );

   task type Dallee_Unit_Control_Task is
      entry Assign_Unit (Unit : Installed_Range);
      entry Sound_Horn	(Horn_Sequence : Pattern_Array);
      entry Bell_Toggle (Bell : Sound_Status);
   end Dallee_Unit_Control_Task;

   type Task_Array is array (Installed_Range) of Dallee_Unit_Control_Task;
   Dallee_Units                  : Task_Array;

   ----------------------------------------------------------------------------

   procedure Sound_Horn (Unit   : in Installed_Range;
                         Signal : in Horn_Signal) is

   begin

      Dallee_Units (Unit).Sound_Horn (Horn_Signals (Signal));

   end Sound_Horn;

   ----------------------------------------------------------------------------
   procedure Bell_On (Unit : in Installed_Range) is
   begin
      Dallee_Units (Unit).Bell_Toggle (On);
   end Bell_On;

   ----------------------------------------------------------------------------
   procedure Bell_Off (Unit : in Installed_Range) is
   begin
      Dallee_Units (Unit).Bell_Toggle (Off);
   end Bell_Off;

   ----------------------------------------------------------------------------

   task body Dallee_Unit_Control_Task is
      My_ID             : Installed_Range;
      My_Horn_Sequence  : Pattern_Array;
      My_Bell           : Sound_Status;
   begin

      accept Assign_Unit (Unit : Installed_Range) do
         My_ID := Unit;
      end Assign_Unit;

      loop    --?Loop here because you only need to assign Unit_UD once?
         select

            accept Sound_Horn (Horn_Sequence : Pattern_Array) do
               My_Horn_Sequence := Horn_Sequence;
            end Sound_Horn;

            for Seconds in My_Horn_Sequence'Range loop
               Dallee.Sound_Horn (My_ID, On);
               delay Duration (My_Horn_Sequence (Seconds));
               Dallee.Sound_Horn (My_ID, Off);
               delay Duration (1);
            end loop;

         or

            accept Bell_Toggle (Bell : Sound_Status) do
               My_Bell := Bell;
            end Bell_Toggle;

            Dallee.Sound_Bell (My_ID, My_Bell);

         end select;
      end loop;
   exception
      when others =>
	 Double_Talk.Speak (Phrase => Phrase_Strings.To_Bounded_String
		     ("Sound Task Exception was raised"),
		     Voice  => Paul);
   end Dallee_Unit_Control_Task;

----------------------------------------------------------------------------

begin
   --Rendezvous with all the tasks in Array of Run Horn Tasks
      for Index in Task_Array'Range loop
         --Give same ID as its Index
         Dallee_Units (Index).Assign_Unit (Unit => Index);
      end loop;
end Sound;

