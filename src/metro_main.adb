-- ***************************************************************************
--            Metro Network - Application Entry Point
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
with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Metro_Network;

procedure Metro_Main is
   M : Metro_Network.Metro;
   Route : Metro_Network.Route_Vectors.Vector;
   
   -- Get station name with autocomplete support
   function Get_Station_With_Autocomplete (Prompt : String) return String is
      Input : String (1 .. 200);
      Last : Natural;
      Matches : Metro_Network.Station_Vectors.Vector;
   begin
      loop
         Put (Prompt);
         Get_Line (Input, Last);
         
         if Last = 0 then
            Put_Line ("Please enter a station name.");
            goto Continue;
         end if;
         
         declare
            Input_Str : constant String := Input (1 .. Last);
         begin
            -- Check for autocomplete request (ending with '?')
            if Last > 0 and then Input (Last) = '?' then
               declare
                  Prefix : constant String := Input (1 .. Last - 1);
               begin
                  Matches := Metro_Network.Find_Matching_Stations (M, Prefix);
                  
                  if Matches.Is_Empty then
                     Put_Line ("No stations found matching '" & Prefix & "'");
                  elsif Matches.Length = 1 then
                     declare
                        Station : constant String := To_String (Matches.First_Element);
                     begin
                        Put_Line ("Auto-completing to: " & Station);
                        return Station;
                     end;
                  else
                     Put_Line ("Found" & Matches.Length'Image & " matching stations:");
                     for Station of Matches loop
                        Put_Line ("  - " & To_String (Station));
                     end loop;
                     Put_Line ("Please be more specific or select one from above.");
                  end if;
               end;
               goto Continue;
            end if;
            
            -- Check if station exists
            Matches := Metro_Network.Find_Matching_Stations (M, Input_Str);
            
            if Matches.Is_Empty then
               Put_Line ("Station '" & Input_Str & "' not found.");
               Put_Line ("Tip: Add '?' after partial name for suggestions (e.g., 'bast?')");
            elsif Matches.Length = 1 then
               -- Exact or unique prefix match
               return To_String (Matches.First_Element);
            else
               -- Multiple matches - check for exact match
               for Station of Matches loop
                  if To_String (Station) = Input_Str then
                     return Input_Str;
                  end if;
               end loop;
               
               -- No exact match, show suggestions
               Put_Line ("Multiple stations match. Did you mean:");
               for Station of Matches loop
                  Put_Line ("  - " & To_String (Station));
               end loop;
               Put_Line ("Please enter complete name or use '?' for autocomplete.");
            end if;
         end;
         
         <<Continue>>
      end loop;
   end Get_Station_With_Autocomplete;
   
begin
   Put_Line ("Paris Metro Route Finder");
   Put_Line ("========================");
   Put_Line ("");

   -- Load metro data (check for enhanced data first)
   declare
      use Ada.Directories;
   begin
      if Exists ("data_enhanced") and then Kind ("data_enhanced") = Directory then
         Put_Line ("Loading enhanced metro data with distance and time information...");
         Metro_Network.Load_Metro_Data (M, "data_enhanced", Use_Enhanced => True);
      else
         Put_Line ("Loading basic metro data...");
         Metro_Network.Load_Metro_Data (M, "data");
      end if;
   end;
   Put_Line ("");

   -- Get start and destination from command line or prompt
   declare
      Start, Destination : Unbounded_String;
   begin
      if Ada.Command_Line.Argument_Count >= 2 then
         -- Use command line arguments
         Start := To_Unbounded_String (Ada.Command_Line.Argument (1));
         Destination := To_Unbounded_String (Ada.Command_Line.Argument (2));
      else
         -- Interactive mode with autocomplete
         Put_Line ("Autocomplete tip: Add '?' after partial name for suggestions");
         Put_Line ("Example: 'bast?' will show all stations starting with 'bast'");
         Put_Line ("");
         
         Start := To_Unbounded_String (Get_Station_With_Autocomplete ("Enter start station: "));
         Destination := To_Unbounded_String (Get_Station_With_Autocomplete ("Enter destination station: "));
      end if;

      -- Find and display route
      Route := Metro_Network.Find_Route (M, To_String (Start), To_String (Destination));
      Metro_Network.Display_Route (M, Route, To_String (Start), To_String (Destination));
   end;

end Metro_Main;
