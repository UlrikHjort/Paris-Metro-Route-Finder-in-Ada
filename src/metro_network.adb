-- ***************************************************************************
--            Metro Network - Package Body
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
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Containers; use Ada.Containers;
with Ada.Numerics.Elementary_Functions;

package body Metro_Network is

   function Hash_Line_ID (Key : Line_ID) return Ada.Containers.Hash_Type is
   begin
      return Hash (Unbounded_String (Key));
   end Hash_Line_ID;
   
   function Hash_Station (Key : Station_Name) return Ada.Containers.Hash_Type is
   begin
      return Hash (Key);
   end Hash_Station;
   
   -- Normalize station name (trim, lowercase for comparison)
   function Normalize (Name : String) return String is
      use Ada.Characters.Handling;
   begin
      return To_Lower (Ada.Strings.Fixed.Trim (Name, Ada.Strings.Both));
   end Normalize;
   
   -- Calculate distance between two positions using Haversine formula
   function Calculate_Distance (From, To : Position) return Float is
      use Ada.Numerics.Elementary_Functions;
      
      Earth_Radius : constant Float := 6371.0; -- km
      Pi : constant Float := Ada.Numerics.Pi;
      
      -- Convert degrees to radians
      Lat1_Rad : constant Float := From.Latitude * Pi / 180.0;
      Lat2_Rad : constant Float := To.Latitude * Pi / 180.0;
      Delta_Lat : constant Float := (To.Latitude - From.Latitude) * Pi / 180.0;
      Delta_Lon : constant Float := (To.Longitude - From.Longitude) * Pi / 180.0;
      
      A : Float;
      C : Float;
   begin
      A := Sin (Delta_Lat / 2.0) * Sin (Delta_Lat / 2.0) +
           Cos (Lat1_Rad) * Cos (Lat2_Rad) *
           Sin (Delta_Lon / 2.0) * Sin (Delta_Lon / 2.0);
      
      C := 2.0 * Arctan (Sqrt (A), Sqrt (1.0 - A));
      
      return Earth_Radius * C * 1000.0; -- Convert to meters
   end Calculate_Distance;
   
   -- Parse CSV line into components
   procedure Parse_CSV_Line (Line : String; 
                             Station : out Unbounded_String;
                             Lat : out Float;
                             Lon : out Float;
                             Time : out Natural;
                             Success : out Boolean) is
      use Ada.Strings.Fixed;
      Comma1, Comma2, Comma3 : Natural;
   begin
      Success := False;
      
      -- Find comma positions
      Comma1 := Ada.Strings.Fixed.Index (Line, ",");
      if Comma1 = 0 then
         return;
      end if;
      
      Comma2 := Ada.Strings.Fixed.Index (Line, ",", Comma1 + 1);
      if Comma2 = 0 then
         return;
      end if;
      
      Comma3 := Ada.Strings.Fixed.Index (Line, ",", Comma2 + 1);
      if Comma3 = 0 then
         return;
      end if;
      
      -- Extract fields
      Station := To_Unbounded_String (Trim (Line (Line'First .. Comma1 - 1), Ada.Strings.Both));
      
      Lat := Float'Value (Line (Comma1 + 1 .. Comma2 - 1));
      Lon := Float'Value (Line (Comma2 + 1 .. Comma3 - 1));
      
      -- Trim the time string to remove any CR/LF and whitespace
      declare
         Time_Str_Raw : constant String := Line (Comma3 + 1 .. Line'Last);
         Time_Str : constant String := Trim (Time_Str_Raw, Ada.Strings.Both);
         Time_Str_Clean : String (1 .. Time_Str'Length);
         Pos : Natural := 0;
      begin
         -- Remove CR/LF characters manually
         for I in Time_Str'Range loop
            if Time_Str (I) /= Character'Val (13) and Time_Str (I) /= Character'Val (10) then
               Pos := Pos + 1;
               Time_Str_Clean (Pos) := Time_Str (I);
            end if;
         end loop;
         
         Time := Natural'Value (Time_Str_Clean (1 .. Pos));
      end;
      
      Success := True;
   exception
      when others =>
         Success := False;
   end Parse_CSV_Line;
   
   -- Load a single metro line from file (supports both .txt and .csv formats)
   procedure Load_Line (M : in out Metro; Filename : String; Line_Name : Line_ID) is
      File : File_Type;
      
      -- Store station data as we read
      type Station_Data is record
         Name : Station_Name;
         Time_To_Next : Natural;  -- Time to reach next station in seconds
      end record;
      
      package Station_Data_Vectors is new Ada.Containers.Vectors
        (Index_Type => Positive, Element_Type => Station_Data);
      
      Stations_Data : Station_Data_Vectors.Vector;
      Is_CSV : constant Boolean := Filename'Length > 4 and then
                                    Filename (Filename'Last - 3 .. Filename'Last) = ".csv";
      First_Line : Boolean := True;
   begin
      Open (File, In_File, Filename);
      
      -- Read all stations from file
      while not End_Of_File (File) loop
         declare
            Line : constant String := Get_Line (File);
            Station : Station_Name;
            Trimmed : constant String := Ada.Strings.Fixed.Trim (Line, Ada.Strings.Both);
            Lat, Lon : Float;
            Time_To_Next : Natural := 0;
            Parse_Success : Boolean;
         begin
            if Trimmed'Length = 0 then
               null; -- Skip empty lines
            elsif Is_CSV and First_Line and Ada.Strings.Fixed.Index (Trimmed, "StationName") > 0 then
               First_Line := False; -- Skip CSV header
            elsif Is_CSV then
               First_Line := False;
               -- Parse CSV format
               Parse_CSV_Line (Trimmed, Station, Lat, Lon, Time_To_Next, Parse_Success);
               
               if Parse_Success then
                  -- Store position and station data
                  M.Station_Positions.Include (Station, (Latitude => Lat, Longitude => Lon));
                  Stations_Data.Append ((Name => Station, Time_To_Next => Time_To_Next));
            
                  -- Add to all stations list
                  if not M.All_Stations.Contains (Station) then
                     M.All_Stations.Append (Station);
                  end if;
            
                  -- Add line to station's line list
                  if M.Station_To_Lines.Contains (Station) then
                     declare
                        Lines : Line_Vectors.Vector := M.Station_To_Lines.Element (Station);
                     begin
                        if not Lines.Contains (Line_Name) then
                           Lines.Append (Line_Name);
                           M.Station_To_Lines.Replace (Station, Lines);
                        end if;
                     end;
                  else
                     declare
                        Lines : Line_Vectors.Vector;
                     begin
                        Lines.Append (Line_Name);
                        M.Station_To_Lines.Insert (Station, Lines);
                     end;
                  end if;
               end if;
            else
               -- Plain text format
               First_Line := False;
               Station := To_Unbounded_String (Trimmed);
               Time_To_Next := 120; -- Default 2 minutes
               
               Stations_Data.Append ((Name => Station, Time_To_Next => Time_To_Next));
         
               -- Add to all stations list
               if not M.All_Stations.Contains (Station) then
                  M.All_Stations.Append (Station);
               end if;
         
               -- Add line to station's line list
               if M.Station_To_Lines.Contains (Station) then
                  declare
                     Lines : Line_Vectors.Vector := M.Station_To_Lines.Element (Station);
                  begin
                     if not Lines.Contains (Line_Name) then
                        Lines.Append (Line_Name);
                        M.Station_To_Lines.Replace (Station, Lines);
                     end if;
                  end;
               else
                  declare
                     Lines : Line_Vectors.Vector;
                  begin
                     Lines.Append (Line_Name);
                     M.Station_To_Lines.Insert (Station, Lines);
                  end;
               end if;
            end if;
         end;
      end loop;
      
      Close (File);
      
      -- Build graph connections for this line
      for I in Stations_Data.First_Index .. Stations_Data.Last_Index loop
         declare
            Station_Info : constant Station_Data := Stations_Data.Element (I);
            Station : constant Station_Name := Station_Info.Name;
         begin
            -- Initialize connection vector if needed
            if not M.Graph.Contains (Station) then
               M.Graph.Insert (Station, Connection_Vectors.Empty_Vector);
            end if;
            
            -- Connect to next station (forward)
            if I < Stations_Data.Last_Index then
               declare
                  Next_Station : constant Station_Name := Stations_Data.Element (I + 1).Name;
                  Distance : Float := 0.0;
                  Time_Sec : constant Natural := Station_Info.Time_To_Next;
                  Conn : Connection;
                  Connections : Connection_Vectors.Vector := M.Graph.Element (Station);
               begin
                  -- Calculate distance if positions available
                  if M.Station_Positions.Contains (Station) and 
                     M.Station_Positions.Contains (Next_Station) then
                     Distance := Calculate_Distance (
                        M.Station_Positions.Element (Station),
                        M.Station_Positions.Element (Next_Station));
                  end if;
                  
                  Conn := (To_Station => Next_Station, 
                          Via_Line => Line_Name,
                          Distance_Meters => Distance,
                          Time_Seconds => Time_Sec);
                  
                  Connections.Append (Conn);
                  M.Graph.Replace (Station, Connections);
               end;
            end if;
            
            -- Connect to previous station (backward)
            if I > Stations_Data.First_Index then
               declare
                  Prev_Station : constant Station_Name := Stations_Data.Element (I - 1).Name;
                  Prev_Info : constant Station_Data := Stations_Data.Element (I - 1);
                  Distance : Float := 0.0;
                  Time_Sec : constant Natural := Prev_Info.Time_To_Next; -- Use previous station's time
                  Conn : Connection;
                  Connections : Connection_Vectors.Vector := M.Graph.Element (Station);
               begin
                  -- Calculate distance if positions available
                  if M.Station_Positions.Contains (Station) and 
                     M.Station_Positions.Contains (Prev_Station) then
                     Distance := Calculate_Distance (
                        M.Station_Positions.Element (Station),
                        M.Station_Positions.Element (Prev_Station));
                  end if;
                  
                  Conn := (To_Station => Prev_Station, 
                          Via_Line => Line_Name,
                          Distance_Meters => Distance,
                          Time_Seconds => Time_Sec);
                  
                  Connections.Append (Conn);
                  M.Graph.Replace (Station, Connections);
               end;
            end if;
         end;
      end loop;
      
   exception
      when Name_Error =>
         Put_Line ("Warning: Could not open file " & Filename);
   end Load_Line;
   
   procedure Load_Metro_Data (M : in out Metro; Lines_Dir : String; Use_Enhanced : Boolean := False) is
      use Ada.Directories;
      Search : Search_Type;
      Dir_Ent : Directory_Entry_Type;
      Pattern : constant String := (if Use_Enhanced then "M*.csv" else "M*.txt");
   begin
      Start_Search (Search, Lines_Dir, Pattern);
      
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Dir_Ent);
         
         declare
            Filename : constant String := Full_Name (Dir_Ent);
            Simple : constant String := Simple_Name (Dir_Ent);
            Line_Name : Line_ID;
            Ext_Len : constant Natural := (if Use_Enhanced then 4 else 4); -- Both .csv and .txt are 4 chars
         begin
            -- Extract line ID from filename (e.g., "M1.txt" -> "M1" or "M1.csv" -> "M1")
            declare
               Line_Str : constant Unbounded_String := 
                 To_Unbounded_String (Simple (Simple'First .. Simple'Last - Ext_Len));
            begin
               Line_Name := Line_ID (Line_Str);
            end;
            
            Put_Line ("Loading line " & To_String (Unbounded_String (Line_Name)) & "...");
            Load_Line (M, Filename, Line_Name);
         end;
      end loop;
      
      End_Search (Search);
      
      Put_Line ("Loaded" & M.All_Stations.Length'Image & " stations on" & 
                Natural (M.Station_To_Lines.Length)'Image & " unique locations");
   end Load_Metro_Data;
   
   -- Find route between two stations
   function Find_Route (M : Metro; Start : String; Destination : String) 
                       return Route_Vectors.Vector is
      
      Start_Station : constant Station_Name := 
        To_Unbounded_String (Ada.Strings.Fixed.Trim (Start, Ada.Strings.Both));
      Dest_Station : constant Station_Name := 
        To_Unbounded_String (Ada.Strings.Fixed.Trim (Destination, Ada.Strings.Both));
      
      -- Queue element for BFS
      type Queue_Element is record
         Station : Station_Name;
         Line    : Line_ID;
      end record;
      
      package Queue_Vectors is new Ada.Containers.Vectors
        (Index_Type => Positive, Element_Type => Queue_Element);
      
      -- Visited set
      package Visited_Maps is new Ada.Containers.Hashed_Maps
        (Key_Type        => Station_Name,
         Element_Type    => Boolean,
         Hash            => Hash_Station,
         Equivalent_Keys => "=");
      
      -- Parent tracking for path reconstruction
      package Parent_Maps is new Ada.Containers.Hashed_Maps
        (Key_Type        => Station_Name,
         Element_Type    => Route_Step,
         Hash            => Hash_Station,
         Equivalent_Keys => "=");
      
      Queue : Queue_Vectors.Vector;
      Visited : Visited_Maps.Map;
      Parents : Parent_Maps.Map;
      Route : Route_Vectors.Vector;
      
   begin
      -- Check if stations exist
      if not M.Graph.Contains (Start_Station) then
         Put_Line ("Error: Station '" & Start & "' not found");
         return Route;
      end if;
      
      if not M.Graph.Contains (Dest_Station) then
         Put_Line ("Error: Station '" & Destination & "' not found");
         return Route;
      end if;
      
      -- Same station
      if Start_Station = Dest_Station then
         Route.Append ((Station => Start_Station, Line => Line_ID (Null_Unbounded_String)));
         return Route;
      end if;
      
      -- Initialize BFS
      declare
         Initial_Lines : constant Line_Vectors.Vector := M.Station_To_Lines.Element (Start_Station);
      begin
         for Line of Initial_Lines loop
            Queue.Append ((Station => Start_Station, Line => Line));
         end loop;
      end;
      
      Visited.Insert (Start_Station, True);
      
      -- BFS main loop
      while not Queue.Is_Empty loop
         declare
            Current : constant Queue_Element := Queue.First_Element;
            Curr_Station : constant Station_Name := Current.Station;
            Curr_Line : constant Line_ID := Current.Line;
         begin
            Queue.Delete_First;
            
            -- Check if we reached destination
            if Curr_Station = Dest_Station then
               -- Reconstruct path
               declare
                  Step : Station_Name := Dest_Station;
               begin
                  while Parents.Contains (Step) loop
                     declare
                        Parent_Step : constant Route_Step := Parents.Element (Step);
                     begin
                        Route.Prepend (Parent_Step);
                        Step := Parent_Step.Station;
                     end;
                  end loop;
                  
                  -- Add destination
                  if not Route.Is_Empty then
                     Route.Append ((Station => Dest_Station, 
                                   Line => Route.Last_Element.Line));
                  end if;
               end;
               
               return Route;
            end if;
            
            -- Explore neighbors
            if M.Graph.Contains (Curr_Station) then
               declare
                  Connections : constant Connection_Vectors.Vector := 
                    M.Graph.Element (Curr_Station);
               begin
                  for Conn of Connections loop
                     if not Visited.Contains (Conn.To_Station) then
                        Visited.Insert (Conn.To_Station, True);
                        Queue.Append ((Station => Conn.To_Station, Line => Conn.Via_Line));
                        Parents.Include (Conn.To_Station, 
                                       (Station => Curr_Station, Line => Conn.Via_Line));
                     end if;
                  end loop;
               end;
            end if;
         end;
      end loop;
      
      Put_Line ("No route found");
      return Route;
   end Find_Route;
   
   procedure Display_Route (M : Metro;
                           Route : Route_Vectors.Vector; 
                           Start : String; 
                           Destination : String) is
      
      function Format_Distance (Meters : Float) return String is
      begin
         if Meters < 1000.0 then
            return Natural (Meters)'Image & " m";
         else
            declare
               Km : constant Float := Meters / 1000.0;
               Km_Str : constant String := Float'Image (Km);
               Dot_Pos : Natural := 0;
            begin
               -- Find decimal point
               for I in Km_Str'Range loop
                  if Km_Str (I) = '.' then
                     Dot_Pos := I;
                     exit;
                  end if;
               end loop;
               
               -- Return with 2 decimal places
               if Dot_Pos > 0 and then Dot_Pos + 2 <= Km_Str'Last then
                  return Ada.Strings.Fixed.Trim (Km_Str (Km_Str'First .. Dot_Pos + 2), Ada.Strings.Both) & " km";
               else
                  return Ada.Strings.Fixed.Trim (Km_Str, Ada.Strings.Both) & " km";
               end if;
            end;
         end if;
      end Format_Distance;
      
      function Format_Time (Seconds : Natural) return String is
         Minutes : constant Natural := Seconds / 60;
         Remaining_Secs : constant Natural := Seconds mod 60;
      begin
         if Minutes = 0 then
            return Remaining_Secs'Image & " sec";
         elsif Remaining_Secs = 0 then
            return Minutes'Image & " min";
         else
            return Minutes'Image & " min" & Remaining_Secs'Image & " sec";
         end if;
      end Format_Time;
      
      function Get_Connection_Info (From, To_St : Station_Name; Via : Line_ID) 
        return Connection is
         Conns : Connection_Vectors.Vector;
      begin
         if M.Graph.Contains (From) then
            Conns := M.Graph.Element (From);
            for Conn of Conns loop
               if Conn.To_Station = To_St and Conn.Via_Line = Via then
                  return Conn;
               end if;
            end loop;
         end if;
         -- Return default if not found
         return (To_Station => To_St, Via_Line => Via, Distance_Meters => 0.0, Time_Seconds => 0);
      end Get_Connection_Info;
      
   begin
      if Route.Is_Empty then
         Put_Line ("No route available");
         return;
      end if;
      
      if Route.Length = 1 then
         Put_Line ("You are already at " & Start);
         return;
      end if;
      
      Put_Line ("");
      Put_Line ("Route from " & Start & " to " & Destination & ":");
      Put_Line ("========================================");
      
      declare
         Current_Line : Line_ID := Route.First_Element.Line;
         Segment_Start : Positive := Route.First_Index;
         Transfer_Count : Natural := 0;
         Total_Distance : Float := 0.0;
         Total_Time : Natural := 0;
         Segment_Distance : Float;
         Segment_Time : Natural;
         Transfer_Penalty : constant Natural := 120; -- 2 minutes per transfer
      begin
         -- Process route and print segments
         for I in Route.First_Index + 1 .. Route.Last_Index loop
            if Route.Element (I).Line /= Current_Line then
               -- Line change detected - calculate segment totals
               Segment_Distance := 0.0;
               Segment_Time := 0;
               
               for J in Segment_Start .. I - 2 loop
                  declare
                     Conn : constant Connection := Get_Connection_Info (
                        Route.Element (J).Station,
                        Route.Element (J + 1).Station,
                        Current_Line);
                  begin
                     Segment_Distance := Segment_Distance + Conn.Distance_Meters;
                     Segment_Time := Segment_Time + Conn.Time_Seconds;
                  end;
               end loop;
               
               -- Print previous segment (if it has more than 1 station)
               if I - 1 > Segment_Start then
                  Put ("  Take line " & To_String (Unbounded_String (Current_Line)) &
                       " from " & To_String (Route.Element (Segment_Start).Station) &
                       " to " & To_String (Route.Element (I - 1).Station));
                  
                  if Segment_Distance > 0.0 then
                     Put (" (" & Format_Distance (Segment_Distance));
                     if Segment_Time > 0 then
                        Put (", " & Format_Time (Segment_Time));
                     end if;
                     Put (")");
                  end if;
                  New_Line;
                  
                  Total_Distance := Total_Distance + Segment_Distance;
                  Total_Time := Total_Time + Segment_Time;
               end if;
               
               -- Print transfer info
               Put_Line ("  >> Transfer to line " & 
                        To_String (Unbounded_String (Route.Element (I).Line)) &
                        " at " & To_String (Route.Element (I - 1).Station));
               
               Transfer_Count := Transfer_Count + 1;
               Total_Time := Total_Time + Transfer_Penalty;
               Current_Line := Route.Element (I).Line;
               Segment_Start := I - 1;  -- Start next segment from transfer station
            end if;
         end loop;
         
         -- Calculate final segment
         Segment_Distance := 0.0;
         Segment_Time := 0;
         
         for J in Segment_Start .. Route.Last_Index - 1 loop
            declare
               Conn : constant Connection := Get_Connection_Info (
                  Route.Element (J).Station,
                  Route.Element (J + 1).Station,
                  Current_Line);
            begin
               Segment_Distance := Segment_Distance + Conn.Distance_Meters;
               Segment_Time := Segment_Time + Conn.Time_Seconds;
            end;
         end loop;
         
         -- Print final segment
         Put ("  Take line " & To_String (Unbounded_String (Current_Line)) &
              " from " & To_String (Route.Element (Segment_Start).Station) &
              " to " & To_String (Route.Last_Element.Station));
         
         if Segment_Distance > 0.0 then
            Put (" (" & Format_Distance (Segment_Distance));
            if Segment_Time > 0 then
               Put (", " & Format_Time (Segment_Time));
            end if;
            Put (")");
         end if;
         New_Line;
         
         Total_Distance := Total_Distance + Segment_Distance;
         Total_Time := Total_Time + Segment_Time;
         
         Put_Line ("");
         Put_Line ("Total stations:" & Route.Length'Image);
         Put_Line ("Transfers:" & Transfer_Count'Image);
         
         if Total_Distance > 0.0 then
            Put_Line ("Total distance: " & Format_Distance (Total_Distance));
         end if;
         
         if Total_Time > 0 then
            Put_Line ("Estimated time: " & Format_Time (Total_Time));
         end if;
      end;
   end Display_Route;
   
   -- Find stations matching a prefix (case-insensitive)
   function Find_Matching_Stations (M : Metro; Prefix : String) return Station_Vectors.Vector is
      Result : Station_Vectors.Vector;
      Normalized_Prefix : constant String := Normalize (Prefix);
   begin
      if Normalized_Prefix'Length = 0 then
         return Result;
      end if;
      
      for Station of M.All_Stations loop
         declare
            Station_Str : constant String := To_String (Station);
            Normalized_Station : constant String := Normalize (Station_Str);
         begin
            -- Check if station starts with prefix
            if Normalized_Station'Length >= Normalized_Prefix'Length and then
               Normalized_Station (Normalized_Station'First .. 
                                  Normalized_Station'First + Normalized_Prefix'Length - 1) = 
               Normalized_Prefix then
               Result.Append (Station);
            end if;
         end;
      end loop;
      
      return Result;
   end Find_Matching_Stations;

end Metro_Network;
