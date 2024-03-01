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

   function Get_L10n_String (Source_Line : String) return String
   is
      Paren_Left_Idx : Natural;
      Minus_Idx      : Natural;
      Quotedbl_Idx   : Natural;
      Comma_Idx      : Natural;
      First          : Positive;
      Last           : Positive;
      Found          : Boolean;
      Result         : Match_Array (0 .. 0);
   begin
      --  "-"
      Match (Minus_Pattern, Source_Line, Result);

      if Result (0) /= No_Match then
         Found := True;
         First := Result (0).First;
         Last := Result (0).Last;
         Paren_Left_Idx := Index (Source_Line (First .. Last), "(");
         Minus_Idx := Index (Source_Line (First .. Last), "-");

         if Paren_Left_Idx > 0 and (Paren_Left_Idx - Minus_Idx) in 2 .. 3 then
            First := Paren_Left_Idx + 1;
         elsif Minus_Idx > 0 then
            First := Minus_Idx + 1;
         end if;
      end if;

      --  "Gettext"
      if not Found then
         Match (Gettext_Pattern, Source_Line, Result);

         if Result (0) /= No_Match then
            Found := True;
            First := Result (0).First;
            Last := Result (0).Last;
            Paren_Left_Idx := Index (Source_Line (First .. Last), "(");
            Quotedbl_Idx := Index (Source_Line (First .. Last), """");

            if Paren_Left_Idx > 0 and Quotedbl_Idx > 0 then
               First := Quotedbl_Idx;
               Quotedbl_Idx := Index (Source_Line (First + 1 .. Last), """");
               Last := Quotedbl_Idx;
            end if;
         end if;
      end if;

      --  "Dgettext"
      if not Found then
         Match (Dgettext_Pattern, Source_Line, Result);

         if Result (0) /= No_Match then
            Found := True;
            First := Result (0).First;
            Last := Result (0).Last;
            Paren_Left_Idx := Index (Source_Line (First .. Last), "(");
            Comma_Idx := Index (Source_Line (First .. Last), ",");

            if Paren_Left_Idx > 0 and Comma_Idx > 0 then
               Quotedbl_Idx := Index (Source_Line (Comma_Idx .. Last), """");
               First := Quotedbl_Idx;
               Quotedbl_Idx := Index (Source_Line (First + 1 .. Last), """");
               Last := Quotedbl_Idx;
            end if;
         end if;
      end if;

      return Source_Line (First .. Last);

   exception
      when others => return "";
end Get_L10n_String;

   ---------------------
   -- Get_L10n_String --
   ---------------------

   function Get_L10n_String (Source_Line : String) return Unbounded_String
   is
   begin
      return To_Unbounded_String (Get_L10n_String (Source_Line));
   end Get_L10n_String;

   ---------------------
   -- Get_L10n_String --
   ---------------------

   function Get_L10n_String
     (Source_Line : Unbounded_String) return String
   is
   begin
      return Get_L10n_String (To_String (Source_Line));
   end Get_L10n_String;

   ---------------------
   -- Get_L10n_String --
   ---------------------

   function Get_L10n_String
     (Source_Line : Unbounded_String) return Unbounded_String
   is
   begin
      return Get_L10n_String (To_String (Source_Line));
   end Get_L10n_String;

end L10n_Table;
