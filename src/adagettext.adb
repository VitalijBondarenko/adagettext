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

with Ada.Text_IO;                   use Ada.Text_IO;
with GNAT.Regpat;                   use GNAT.Regpat;
with Ada.Strings.Fixed;             use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Command_Line;              use Ada.Command_Line;
with GNAT.Command_Line;             use GNAT.Command_Line;
with Ada.Directories;               use Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Exceptions;                use Ada.Exceptions;

with L10n_Table;                    use L10n_Table;

procedure AdaGettext is

   package AIOE renames Ada.IO_Exceptions;
   package L10nt renames L10n_Table.L10n_String_Table;
   use L10nt;

   Prog            : constant String := Command_Name;
   Program_Name    : constant String := Base_Name (Prog);
   Program_Version : constant String := "1.2";

   PO_Dirname   : constant String := "po";
   POTFILES_in  : constant String := "POTFILES.in";
   Pot_File_In  : File_Type;
   Write_Header : Boolean := True;

   L10n_Strings : L10n_String_Table.Map;
   Str_Cursor   : L10n_String_Table.Cursor;
   Src_File     : File_Type;
   Src_Filename : Unbounded_String;
   Src_Line     : Unbounded_String;
   Src_Line_No  : Natural := 0;
   L10n_Line    : Unbounded_String;
   Temp_Line    : Unbounded_String;
   Pot_File     : File_Type := Standard_Output;
   Pot_Filename : Unbounded_String := Null_Unbounded_String;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      Put_Line (Program_Name & " version " & Program_Version);
      New_Line;
      Put_Line ("Usage : " & Program_Name & " [options]");
      New_Line;
      Put_Line
        ("Extract translatable strings from given input Ada source files");
      Put_Line ("and generate po template (.pot) only.");
      Put_Line ("The result is printed to outfile, if it is given.");
      Put_Line ("Otherwise standard output is used.");
      New_Line;
      Put_Line ("The following options are available:");
      Put_Line ("   -h, --help  print this help and exit");
      Put_Line ("   -o outfile  write output to specified file");
      Put_Line ("   -n          don't write header to output file");
   end Usage;

begin  --  Start AdaGettext
   loop
      case Getopt ("h -help n o:") is
         when ASCII.NUL =>
            exit;
         when 'h' =>
            Usage;
            return;
         when 'n' =>
            Write_Header := False;
         when 'o' =>
            Pot_Filename := To_Unbounded_String (Parameter);
         when '-' =>
            if Full_Switch = "-help" then
               Usage;
               return;
            end if;
         when others =>
            raise Program_Error;
      end case;
   end loop;

   if Base_Name (Current_Directory) /= PO_Dirname then
      Put_Line
        (Program_Name & ": Unable to proceed." & ASCII.LF
         & "Make sure to run this program inside the po directory." & ASCII.LF
         & "Try 'adagettext --help' for more information.");
      return;
   end if;

   Open (Pot_File_In, In_File, POTFILES_in);

   if Pot_Filename /= Null_Unbounded_String then
      Close (Pot_File);
      Create (Pot_File, Out_File, To_String (Pot_Filename));
   end if;

   while not End_Of_File (Pot_File_In) loop
      Get_Line (Pot_File_In, Src_Filename);

      if Src_Filename /= Null_Unbounded_String then
         Src_Filename := "../" & Src_Filename;
         Src_Line_No := 0;

         begin
            Open (Src_File, In_File, To_String (Src_Filename));

            while not End_Of_File (Src_File) loop
               Get_Line (Src_File, Src_Line);
               Src_Line_No := Src_Line_No + 1;
               L10n_Line := Get_L10n_String (Src_Line);

               if L10n_Line /= Null_Unbounded_String then
                  Str_Cursor := L10n_Strings.Find (L10n_Line);

                  if Str_Cursor = L10n_String_Table.No_Element then
                     L10n_Strings.Insert
                       (L10n_Line,
                        "#: " & Src_Filename & ":" & Src_Line_No'Img);
                  else
                     Temp_Line := L10n_Strings.Element (L10n_Line);
                     L10n_Strings.Replace_Element
                       (Str_Cursor,
                        Temp_Line & ASCII.LF
                        & "#: " & Src_Filename & ":" & Src_Line_No'Img);
                  end if;
               end if;
            end loop;

            Close (Src_File);
         exception
            when others =>
               if Is_Open (Src_File) then
                  Close (Src_File);
               end if;
         end;
      end if;
   end loop;

   Close (Pot_File_In);

   if Write_Header then
      Put_Line (Pot_File, Pot_File_Header);
   end if;

   Str_Cursor := L10n_Strings.First;

   while L10n_String_Table.Has_Element (Str_Cursor) loop
      Put_Line
        (Pot_File, To_String (L10n_String_Table.Element (Str_Cursor)));
      Put_Line
        (Pot_File, "msgid " & To_String (L10n_String_Table.Key (Str_Cursor)));
      Put_Line (Pot_File, "msgstr """"" & ASCII.LF);
      L10n_String_Table.Next (Str_Cursor);
   end loop;

exception
   when Invalid_Switch    =>
      Put_Line (Program_Name & ": " & "Invalid switch " & Full_Switch);
   when Invalid_Parameter =>
      Put_Line (Program_Name & ": "
                & "Invalid or missing parameter for -" & Full_Switch);
   when E : others        =>
      if Is_Open (Pot_File_In) then
         Close (Pot_File_In);
      end if;

      if Is_Open (Src_File) then
         Close (Src_File);
      end if;

      Put_Line (Program_Name & ": " & Exception_Message (E));
end AdaGettext;
