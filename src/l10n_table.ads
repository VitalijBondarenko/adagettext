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

with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with GNAT.Regpat;                use GNAT.Regpat;

package L10n_Table is

   subtype L10n_Key is Unbounded_String;

   package L10n_String_Table is new Ada.Containers.Ordered_Maps
     (Key_Type        => L10n_Key,
      Element_Type    => Unbounded_String);

   L10n_Pattern_Default : constant String := "-"".*""";
   L10n_Matcher_Default : constant Pattern_Matcher :=
     Compile (L10n_Pattern_Default);

   function Get_L10n_String
     (Pattern     : Pattern_Matcher;
      Source_Line : String) return String;
   function Get_L10n_String
     (Pattern     : Pattern_Matcher;
      Source_Line : String) return Unbounded_String;
   function Get_L10n_String
     (Pattern     : Pattern_Matcher;
      Source_Line : Unbounded_String) return Unbounded_String;
   function Get_L10n_String
     (Pattern     : String;
      Source_Line : String) return String;
   --  Return a string matched to the Pattern

   Pot_File_Header : constant String :=
     "# SOME DESCRIPTIVE TITLE." & ASCII.LF &
     "# Copyright (C) YEAR THE PACKAGE'S COPYRIGHT HOLDER" & ASCII.LF &
     "# This file is distributed under the same license as the PACKAGE package." & ASCII.LF &
     "# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR." & ASCII.LF &
     "#" & ASCII.LF &
     "#, fuzzy" & ASCII.LF &
     "msgid """ & ASCII.LF &
     "msgstr """ & ASCII.LF &
     """Project-Id-Version: PACKAGE VERSION\n""" & ASCII.LF &
     """Report-Msgid-Bugs-To: \n""" & ASCII.LF &
     """POT-Creation-Date: YEAR-MO-DA HO:MI+ZONE\n""" & ASCII.LF &
     """PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\n""" & ASCII.LF &
     """Last-Translator: FULL NAME <EMAIL@ADDRESS>\n""" & ASCII.LF &
     """Language-Team: LANGUAGE <LL@li.org>\n""" & ASCII.LF &
     """Language: \n""" & ASCII.LF &
     """MIME-Version: 1.0\n""" & ASCII.LF &
     """Content-Type: text/plain; charset=CHARSET\n""" & ASCII.LF &
     """Content-Transfer-Encoding: 8bit\n""" & ASCII.LF;
end L10n_Table;
