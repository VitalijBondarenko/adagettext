------------------------------------------------------------------------------
--                                                                          --
-- Copyright (c) 2014-2015 Vitalij Bondarenko <vibondare@gmail.com>         --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- The MIT License (MIT)                                                    --
--                                                                          --
-- Permission is hereby granted, free of charge, to any person obtaining a  --
-- copy of this software and associated documentation files (the            --
-- "Software"), to deal in the Software without restriction, including      --
-- without limitation the rights to use, copy, modify, merge, publish,      --
-- distribute, sublicense, and/or sell copies of the Software, and to       --
-- permit persons to whom the Software is furnished to do so, subject to    --
-- the following conditions:                                                --
--                                                                          --
-- The above copyright notice and this permission notice shall be included  --
-- in all copies or substantial portions of the Software.                   --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS  --
-- OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF               --
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.   --
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY     --
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,     --
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE        --
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                   --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded.Hash;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body L10n_Table is

   ---------------------
   -- Get_L10n_String --
   ---------------------

   function Get_L10n_String
     (Pattern : Pattern_Matcher; Source_Line : String) return String
   is
      Bracket_Index : Natural;
      Minus_Index   : Natural;
      First         : Positive;
      Last          : Positive;
      Found         : Boolean;
      Result        : Match_Array (0 .. 0);

   begin
      Match (Pattern, Source_Line, Result);
      Found := not (Result (0) = No_Match);

      if Found then
         First := Result (0).First;
         Last := Result (0).Last;
         Bracket_Index := Index (Source_Line (First .. Last), "(");
         Minus_Index := Index (Source_Line (First .. Last), "-");

         if Bracket_Index > 0 and (Bracket_Index - Minus_Index) in 2 .. 3 then
            First := Bracket_Index + 1;
         elsif Minus_Index > 0 then
            First := Minus_Index + 1;
         end if;

         return Source_Line (First .. Last);
      end if;

      return "";
   end Get_L10n_String;

   function Get_L10n_String
     (Pattern     : Pattern_Matcher;
      Source_Line : String) return Unbounded_String is
   begin
      return To_Unbounded_String (Get_L10n_String (Pattern, Source_Line));
   end Get_L10n_String;

   function Get_L10n_String
     (Pattern     : Pattern_Matcher;
      Source_Line : Unbounded_String) return Unbounded_String is
   begin
      return To_Unbounded_String
        (Get_L10n_String (Pattern, To_String (Source_Line)));
   end Get_L10n_String;

   function Get_L10n_String
     (Pattern     : String;
      Source_Line : String) return String is
   begin
      return Get_L10n_String (Compile (Pattern), Source_Line);
   end Get_L10n_String;

end L10n_Table;
