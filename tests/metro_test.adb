-- ***************************************************************************
--                      Metro Test - Test Suite
--
--           Copyright (C) 2026 By Ulrik Hørlyk Hjort
--
-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
-- LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
-- OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
-- WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-- ***************************************************************************
with Ada.Text_IO; use Ada.Text_IO;
with Metro_Network;

procedure Metro_Test is
   M : Metro_Network.Metro;
   Route : Metro_Network.Route_Vectors.Vector;
   Test_Count : Natural := 0;
   Pass_Count : Natural := 0;
   
   procedure Test (Name : String; Start : String; Dest : String; 
                   Expected_Stations : Natural; Expected_Transfers : Natural) is
   begin
      Test_Count := Test_Count + 1;
      Put (Name & "... ");
      
      Route := Metro_Network.Find_Route (M, Start, Dest);
      
      if Natural (Route.Length) = Expected_Stations then
         -- Count transfers
         declare
            use Metro_Network;  -- Make Line_ID operations visible
            Current_Line : Line_ID := Route.First_Element.Line;
            Transfer_Count : Natural := 0;
         begin
            for I in Route.First_Index + 1 .. Route.Last_Index loop
               if Route.Element (I).Line /= Current_Line then
                  Transfer_Count := Transfer_Count + 1;
                  Current_Line := Route.Element (I).Line;
               end if;
            end loop;
            
            if Transfer_Count = Expected_Transfers then
               Put_Line ("PASS");
               Pass_Count := Pass_Count + 1;
            else
               Put_Line ("FAIL (expected" & Expected_Transfers'Image & 
                        " transfers, got" & Transfer_Count'Image & ")");
            end if;
         end;
      else
         Put_Line ("FAIL (expected" & Expected_Stations'Image & 
                  " stations, got" & Route.Length'Image & ")");
      end if;
   end Test;
   
begin
   Put_Line ("Paris Metro Route Finder - Test Suite");
   Put_Line ("======================================");
   Put_Line ("");
   
   -- Load metro data
   Put_Line ("Loading metro data...");
   Metro_Network.Load_Metro_Data (M, "../data");
   Put_Line ("");
   
   Put_Line ("Running tests...");
   Put_Line ("");
   
   -- Test 1: Same line, no transfers
   Test ("Same line (no transfers)", 
         "Porte de Vincennes", "Bastille", 5, 0);
   
   -- Test 2: Adjacent stations
   Test ("Adjacent stations",
         "Bastille", "Gare de Lyon", 2, 0);
   
   -- Test 3: One transfer
   Test ("One transfer",
         "Châtelet", "République", 5, 1);
   
   -- Test 4: Multiple transfers  
   Test ("Two transfers",
         "Concorde", "Bastille", 6, 2);
   
   -- Test 5: Long route with transfers
   Test ("Long route (3 transfers)",
         "Porte de Vincennes", "Montparnasse-Bienvenue", 12, 2);
   
   -- Test 6: Same station
   Test ("Same station",
         "Concorde", "Concorde", 1, 0);
   
   -- Test 7: Transfer station route
   Test ("Via transfer hub",
         "Nation", "Concorde", 7, 2);
   
   Put_Line ("");
   Put_Line ("======================================");
   Put_Line ("Results:" & Pass_Count'Image & " /" & Test_Count'Image & " tests passed");
   
   if Pass_Count = Test_Count then
      Put_Line ("All tests PASSED!");
   else
      Put_Line ("Some tests FAILED!");
   end if;
   
end Metro_Test;
