---
--- Copyright (c) 2025, Benjamin Mordaunt
---

with Gtk.Menu_Item;
with Gtk.Widget;

package Callbacks is
   procedure On_Open (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class);
   procedure On_Destroy (Self : access Gtk.Widget.Gtk_Widget_Record'Class);
end Callbacks;