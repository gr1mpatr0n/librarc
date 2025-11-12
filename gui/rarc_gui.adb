---
--- Copyright (c) 2025, Benjamin Mordaunt
---

with Gtk.Main;         use Gtk.Main;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Window;       use Gtk.Window;
with Gtk.Box;          use Gtk.Box;
with Gtk.Label;        use Gtk.Label;
with Gtk.Menu;         use Gtk.Menu;
with Gtk.Menu_Bar;     use Gtk.Menu_Bar;
with Gtk.Menu_Item;    use Gtk.Menu_Item;

with Callbacks;        use Callbacks;

procedure RARC_GUI is
   Main_Window  : Gtk_Window;
   Menu_Bar     : Gtk_Menu_Bar;
   File_Menu    : Gtk_Menu;
   Open_Item    : Gtk_Menu_Item;
   Contents     : Gtk_Box;
   Copyright    : Gtk_Label;
begin
   Gtk.Main.Init;

   -- Main window
   Main_Window := new Gtk_Window_Record;
   Main_Window.Initialize (Window_Toplevel);
   Main_Window.Set_Title ("RARC Archive Manager");
   Main_Window.Set_Default_Size (400, 300);
   Main_Window.On_Destroy (On_Destroy'Access);

   -- Box
   Contents := new Gtk_Box_Record;
   Contents.Initialize (Orientation_Vertical, 0);
   Main_Window.Add (Contents);

   -- Menu bar
   Menu_Bar := new Gtk_Menu_Bar_Record;
   Gtk.Menu_Bar.Initialize (Menu_Bar);
   Contents.Pack_Start (Menu_Bar, Expand => False);

   Copyright := new Gtk_Label_Record;
   Copyright.Initialize ("Stand with Ukraine 🇺🇦 and the Palestinian people 🇵🇸!");
   Copyright.Set_Alignment (1.0, 0.5);
   Copyright.Set_Margin_Bottom (5);
   Copyright.Set_Margin_Right (5);
   Copyright.Set_Margin_Left (5);
   Contents.Pack_End (Copyright, Expand => False);

   -- File menu
   declare
      File_Item : Gtk_Menu_Item;
   begin
      File_Item := new Gtk_Menu_Item_Record;
      File_Item.Initialize_With_Mnemonic ("_File");
      Menu_Bar.Append (File_Item);

      File_Menu := new Gtk_Menu_Record;
      Gtk.Menu.Initialize (File_Menu);
      File_Item.Set_Submenu (File_Menu);

      Open_Item := new Gtk_Menu_Item_Record;
      Open_Item.Initialize_With_Mnemonic ("_Open...");
      Open_Item.On_Activate (On_Open'Access);
      File_Menu.Append (Open_Item);
   end;

   Main_Window.Show_All;

   Main;
end RARC_GUI;
