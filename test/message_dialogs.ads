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

with Glib;        use Glib;
with Gtkada.Intl; use Gtkada.Intl;
with Gtk.Window;  use Gtk.Window;

package Message_Dialogs is

   function Dialog_Confirmation
     (Parent : Gtk_Window;
      Msg    : UTF8_String;
      Msg_2  : UTF8_String := "";
      Detail : UTF8_String := "";
      Title  : UTF8_String := -"Confirmation") return Boolean;

   procedure Dialog_Information
     (Parent : Gtk_Window;
      Msg    : UTF8_String;
      Msg_2  : UTF8_String := "";
      Detail : UTF8_String := "";
      Title  : UTF8_String := -"Information");

   procedure Dialog_Error
     (Parent : Gtk_Window;
      Msg    : UTF8_String;
      Msg_2  : UTF8_String := "";
      Detail : UTF8_String := "";
      Title  : UTF8_String := -"Error");

   procedure Dialog_Warning
     (Parent : Gtk_Window;
      Msg    : UTF8_String;
      Msg_2  : UTF8_String := "";
      Detail : UTF8_String := "";
      Title  : UTF8_String := -"Warning");

end Message_Dialogs;
