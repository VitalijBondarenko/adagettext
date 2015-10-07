------------------------------------------------------------------------------
--                                                                          --
-- Copyright (c) 2014 Vitalij Bondarenko <vibondare@gmail.com>              --
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

with Gtk.Dialog;         use Gtk.Dialog;
with Gtk.Message_Dialog; use Gtk.Message_Dialog;
with Gtk.Expander;       use Gtk.Expander;
with Gtk.Label;          use Gtk.Label;
with Gtk.Box;            use Gtk.Box;
with Pango.Enums;

package body Message_Dialogs is

   ------------------
   -- Dialog_Error --
   ------------------

   function Message_Box
     (Box_Type : Gtk_Message_Type;
      Buttons  : Gtk_Buttons_Type;
      Message  : UTF8_String;
      Message2 : UTF8_String := "";
      Detail   : UTF8_String := "";
      Title    : UTF8_String := "";
      Parent   : Gtk_Window) return Gtk_Message_Dialog
   is
      MD       : Gtk_Message_Dialog;
      Expander : Gtk_Expander;
      Label    : Gtk_Label;

   begin
      Gtk.Message_Dialog.Gtk_New
        (Dialog   => MD,
         Parent   => Parent,
         Flags    => Modal,
         Typ      => Box_Type,
         Buttons  => Buttons,
         Message  => Message);
      Set_Title (MD, Title);

      if Message2 /= "" then
         Format_Secondary_Text (MD, Message2 & ASCII.NUL);
         -- ASCII.NUL added for the GtkAda 2.x error workaround
      end if;

      if Detail /= "" then
         Set_Markup (MD, "<b>" & Message & "</b>");
         Gtk_New (Expander, -"Details");
         Gtk_New (Label, Detail);
         Set_Line_Wrap (Label, True);
         Add (Expander, Label);
         Pack_Start (Gtk_Box (Get_Content_Area (MD)), Expander);
      end if;

      Show_All (MD);
      return MD;
   end Message_Box;

   -------------------------
   -- Dialog_Confirmation --
   -------------------------

   function Dialog_Confirmation
     (Parent : Gtk_Window;
      Msg    : UTF8_String;
      Msg_2  : UTF8_String := "";
      Detail : UTF8_String := "";
      Title  : UTF8_String := -"Confirmation") return Boolean
   is
      MD : Gtk_Message_Dialog;
      R  : Gtk_Response_Type;

   begin
      MD := Message_Box
        (Box_Type => Message_Question,
         Buttons  => Buttons_Yes_No,
         Message  => Msg,
         Message2 => Msg_2,
         Detail   => Detail,
         Title    => Title,
         Parent   => Parent);
      R := Run (MD);
      Destroy (MD);

      if R = Gtk_Response_Yes then
         return True;
      else
         return False;
      end if;
   end Dialog_Confirmation;

   ------------------------
   -- Dialog_Information --
   ------------------------

   procedure Dialog_Information
     (Parent : Gtk_Window;
      Msg    : UTF8_String;
      Msg_2  : UTF8_String := "";
      Detail : UTF8_String := "";
      Title  : UTF8_String := -"Information")
   is
      MD : Gtk_Message_Dialog;
      R  : Gtk_Response_Type;

   begin
      MD := Message_Box
        (Box_Type => Message_Error,
         Buttons  => Buttons_Ok,
         Message  => Msg,
         Message2 => Msg_2,
         Detail   => Detail,
         Title    => Title,
         Parent   => Parent);
      R := Run (MD);
      Destroy (MD);
   end Dialog_Information;

   ------------------
   -- Dialog_Error --
   ------------------

   procedure Dialog_Error
     (Parent : Gtk_Window;
      Msg    : UTF8_String;
      Msg_2  : UTF8_String := "";
      Detail : UTF8_String := "";
      Title  : UTF8_String := -"Error")
   is
      MD : Gtk_Message_Dialog;
      R  : Gtk_Response_Type;

   begin
      MD := Message_Box
        (Box_Type => Message_Error,
         Buttons  => Buttons_Close,
         Message  => Msg,
         Message2 => Msg_2,
         Detail   => Detail,
         Title    => Title,
         Parent   => Parent);
      R := Run (MD);
      Destroy (MD);
   end Dialog_Error;

   --------------------
   -- Dialog_Warning --
   --------------------

   procedure Dialog_Warning
     (Parent : Gtk_Window;
      Msg    : UTF8_String;
      Msg_2  : UTF8_String := "";
      Detail : UTF8_String := "";
      Title  : UTF8_String := -"Warning")
   is
      MD : Gtk_Message_Dialog;
      R  : Gtk_Response_Type;

   begin
      MD := Message_Box
        (Box_Type => Message_Warning,
         Buttons  => Buttons_Close,
         Message  => Msg,
         Message2 => Msg_2,
         Detail   => Detail,
         Title    => Title,
         Parent   => Parent);
      R := Run (MD);
      Destroy (MD);
   end Dialog_Warning;

end Message_Dialogs;
