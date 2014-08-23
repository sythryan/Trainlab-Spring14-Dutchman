--Author : Abubakar Audu
--Team   : The Grounded Dutchman

-- *** Comment these out if not running under MaRTE
-- with MaRTE_OS;
-- pragma Warnings (Off, MaRTE_OS);
-- ***

with Ada.Text_IO;
with Dallee;
use Dallee;

procedure dallee_test is
   package Installed_Unit_IO is new Ada.Text_IO.Integer_IO
     (dallee.Installed_Unit);

   procedure Sound_Horn_Test is
   begin
      Ada.Text_IO.Put_Line ("Begin individual horn test:");
      for Unit in Dallee.Installed_Unit loop
	 Ada.Text_IO.Put ("Sounding horn for unit ");
	 Installed_Unit_IO.Put (Unit);
	 Ada.Text_IO.New_Line;

	 Dallee.Sound_Horn (Number => Unit,
		     Set_To => On);
	 delay 1.0;
	 Dallee.Sound_Horn (Number => Unit,
		     Set_To => Off);

	 Ada.Text_IO.Put ("Press enter for next test...");
	 Ada.Text_IO.Skip_Line;
      end loop;
   end Sound_Horn_Test;

   procedure Sound_Bell_Test is
   begin
      Ada.Text_IO.Put_Line ("Begin individual bell test:");
      for Unit in Dallee.Installed_Unit loop
	 Ada.Text_IO.Put ("Sounding bell for unit ");
	 Installed_Unit_IO.Put (Unit);
	 Ada.Text_IO.New_Line;

	 Dallee.Sound_Bell (Number => Unit,
		     Set_To => On);
	 delay 1.0;
	 Dallee.Sound_Bell (Number => Unit,
		     Set_To => Off);

	 Ada.Text_IO.Put ("Press enter for next test...");
	 Ada.Text_IO.Skip_Line;
      end loop;
   end Sound_Bell_Test;

   procedure Sound_Multiple_Unit_Test is

      procedure Signal_Type_Output (Unit_1_Signal : in String;
                              Unit_2_Signal : in String;
                              Unit_3_Signal : in String) is
      begin
         Ada.Text_IO.Put ("Unit 1 sounding: ");
         Ada.Text_IO.Put_Line (Unit_1_Signal);
         Ada.Text_IO.Put ("Unit 2 sounding: ");
         Ada.Text_IO.Put_Line (Unit_2_Signal);
         Ada.Text_IO.Put ("Unit 3 sounding: ");
         Ada.Text_IO.Put_Line (Unit_3_Signal);
      end Signal_Type_Output;

   begin
      Ada.Text_IO.Put_Line ("Begin testing multiple units at same time:");

      Signal_Type_Output (Unit_1_Signal => "Bell",
			  Unit_2_Signal => "Bell",
                          Unit_3_Signal => "Bell");
      Dallee.Sound_Bell (Number => 1,
                         Set_To => On);
      Dallee.Sound_Bell (Number => 2,
                         Set_To => On);
      Dallee.Sound_Bell (Number => 3,
                         Set_To => On);
      delay 3.0;
      Dallee.Sound_Bell (Number => 1,
                         Set_To => Off);
      Dallee.Sound_Bell (Number => 2,
                         Set_To => Off);
      Dallee.Sound_Bell (Number => 3,
			 Set_To => Off);
      Ada.Text_IO.Put_Line ("Press Enter to begin next test.");
      Ada.Text_IO.Skip_Line;


      Signal_Type_Output (Unit_1_Signal => "Horn",
			  Unit_2_Signal => "Horn",
                          Unit_3_Signal => "Horn");
      Dallee.Sound_Horn (Number => 1,
                         Set_To => On);
      Dallee.Sound_Horn (Number => 2,
                         Set_To => On);
      Dallee.Sound_Horn (Number => 3,
                         Set_To => On);
      delay 3.0;
      Dallee.Sound_Horn (Number => 1,
                         Set_To => Off);
      Dallee.Sound_Horn (Number => 2,
                         Set_To => Off);
      Dallee.Sound_Horn (Number => 3,
			 Set_To => Off);
      Ada.Text_IO.Put_Line ("Press Enter to begin next test.");
      Ada.Text_IO.Skip_Line;


      Signal_Type_Output (Unit_1_Signal => "Bell",
			  Unit_2_Signal => "Horn",
			  Unit_3_Signal => "Bell");
      Dallee.Sound_Bell (Number => 1,
                         Set_To => On);
      Dallee.Sound_Horn (Number => 2,
                         Set_To => On);
      Dallee.Sound_Bell (Number => 3,
                         Set_To => On);
      delay 3.0;
      Dallee.Sound_Bell (Number => 1,
                         Set_To => Off);
      Dallee.Sound_Horn (Number => 2,
                         Set_To => Off);
      Dallee.Sound_Bell (Number => 3,
			 Set_To => Off);
      Ada.Text_IO.Put_Line ("Press Enter to begin next test.");
      Ada.Text_IO.Skip_Line;


      Signal_Type_Output (Unit_1_Signal => "Horn",
			  Unit_2_Signal => "Bell",
                          Unit_3_Signal => "Horn");
      Dallee.Sound_Horn (Number => 1,
                         Set_To => On);
      Dallee.Sound_Bell (Number => 2,
                         Set_To => On);
      Dallee.Sound_Horn (Number => 3,
                         Set_To => On);
      delay 3.0;
      Dallee.Sound_Horn (Number => 1,
                         Set_To => Off);
      Dallee.Sound_Bell (Number => 2,
                         Set_To => Off);
      Dallee.Sound_Horn (Number => 3,
			 Set_To => Off);

      Ada.Text_IO.Put_Line ("Multiple unit tests complete");
   end Sound_Multiple_Unit_Test;

begin
   Sound_Horn_Test;
   Sound_Bell_Test;
   Sound_Multiple_Unit_Test;
end dallee_test;
