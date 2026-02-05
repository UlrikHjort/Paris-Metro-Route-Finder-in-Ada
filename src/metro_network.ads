-- ***************************************************************************
--            Metro Network - Package Specification
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

with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

package Metro_Network is

   type Line_ID is new Unbounded_String;
   
   -- Hash function for Line_ID
   function Hash_Line_ID (Key : Line_ID) return Ada.Containers.Hash_Type;
   
   -- Station name type
   subtype Station_Name is Unbounded_String;
   
   -- Hash function for Station_Name
   function Hash_Station (Key : Station_Name) return Ada.Containers.Hash_Type;
   
   -- Geographic position
   type Position is record
      Latitude  : Float;
      Longitude : Float;
   end record;
   
   -- Station information with position
   type Station_Info is record
      Name : Station_Name;
      Pos  : Position;
   end record;
   
   -- Vector of station names
   package Station_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Station_Name);
   
   -- Vector of line IDs
   package Line_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Line_ID);
   
   -- Map: Station_Name -> Position
   package Station_Position_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Station_Name,
      Element_Type    => Position,
      Hash            => Hash_Station,
      Equivalent_Keys => "=");
   
   -- Map: Station_Name -> Vector of Line_IDs (which lines serve this station)
   package Station_Lines_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Station_Name,
      Element_Type    => Line_Vectors.Vector,
      Hash            => Hash_Station,
      Equivalent_Keys => "=",
      "="             => Line_Vectors."=");
   
   -- Connection: represents an edge in the graph with distance and time
   type Connection is record
      To_Station      : Station_Name;
      Via_Line        : Line_ID;
      Distance_Meters : Float := 0.0;  -- Distance in meters
      Time_Seconds    : Natural := 0;  -- Travel time in seconds
   end record;
   
   -- Vector of connections
   package Connection_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Connection);
   
   -- Map: Station_Name -> Vector of Connections (adjacency list)
   package Graph_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Station_Name,
      Element_Type    => Connection_Vectors.Vector,
      Hash            => Hash_Station,
      Equivalent_Keys => "=",
      "="             => Connection_Vectors."=");
   
   -- Path step in a route
   type Route_Step is record
      Station : Station_Name;
      Line    : Line_ID;
   end record;
   
   -- Vector of route steps
   package Route_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Route_Step);
   
   -- Metro network type
   type Metro is record
      All_Stations     : Station_Vectors.Vector;
      Station_Positions : Station_Position_Maps.Map;  -- NEW: positions
      Station_To_Lines  : Station_Lines_Maps.Map;
      Graph            : Graph_Maps.Map;
   end record;
   
   -- Calculate distance between two positions using Haversine formula
   function Calculate_Distance (From, To : Position) return Float;
   
   -- Initialize and load metro data
   procedure Load_Metro_Data (M : in out Metro; Lines_Dir : String; Use_Enhanced : Boolean := False);
   
   -- Find route between two stations
   function Find_Route (M : Metro; Start : String; Destination : String) 
                       return Route_Vectors.Vector;
   
   -- Display a route
   procedure Display_Route (M : Metro;
                           Route : Route_Vectors.Vector; 
                           Start : String; 
                           Destination : String);
   
   -- Find stations matching a prefix (case-insensitive)
   function Find_Matching_Stations (M : Metro; Prefix : String) return Station_Vectors.Vector;

end Metro_Network;
