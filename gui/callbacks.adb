---
--- Copyright (c) 2025, Benjamin Mordaunt
---

with Ada.Text_IO;     use Ada.Text_IO;

with Gtk.Dialog;
with Gtk.File_Chooser;
with Gtk.File_Chooser_Dialog;
with Gtk.Main;

package body Callbacks is
   use Gtk.Dialog;
   use Gtk.File_Chooser;
   use Gtk.File_Chooser_Dialog;
   use Gtk.Menu_Item;

   procedure On_Open (Self : access Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);

      RARC_Archive_Dialog : Gtk_File_Chooser_Dialog;
      Resp                : Gtk_Response_Type;
      Discard             : Gtk.Widget.Gtk_Widget;
   begin
      RARC_Archive_Dialog := new Gtk_File_Chooser_Dialog_Record;
      RARC_Archive_Dialog.Initialize
         (Title   => "Select a RARC/U8 archive...",
          Parent  => null,
          Action  => Action_Open);

      -- TODO: File filter.

      Discard := Gtk.Dialog.Add_Button (
         Gtk_Dialog (RARC_Archive_Dialog),
         Text => "_Cancel",
         Response_Id => Gtk.Dialog.Gtk_Response_Cancel
      );

      Discard := Gtk.Dialog.Add_Button (
         Gtk_Dialog (RARC_Archive_Dialog),
         Text => "_Open",
         Response_Id => Gtk.Dialog.Gtk_Response_Accept
      );

      Resp := RARC_Archive_Dialog.Run;

      if Resp = Gtk_Response_Accept then
         Put_Line ("Selected archive """ & RARC_Archive_Dialog.Get_Filename & """.");
      else
         Put_Line ("Cancelled...");
      end if;

      RARC_Archive_Dialog.Destroy;
   end On_Open;

   procedure On_Destroy (Self : access Gtk.Widget.Gtk_Widget_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Gtk.Main.Main_Quit;
   end On_Destroy;
end Callbacks;