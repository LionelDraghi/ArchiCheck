
# GtkAda test suite



##  GtkAda test suite / File Identification

  Checking that

  > archicheck -q -lf -r -I gtkada-master

  is identifying the same files (once sorted) than

  > find gtkada-master -name *.ad[sb]


 GtkAda test suite / File Identification [Successful]

##  GtkAda test suite / Unit Identification


  ```
  archicheck -ld -r -I gtkada-master
  ```

  4785 unit found, not detailed here

 GtkAda test suite / Unit Identification [Successful]

##  GtkAda test suite / A realistic GtkAda description file


  ![](gtk.png)

  Checking those rules over GtkAda:

  ```
Interfaces use is allowed
System     use is allowed 
Ada        use is allowed
GNAT       use is allowed

-- Most of the error are due to this obsolete package.
-- to uncomment when '.' will be taken into account :
-- Gtk.Arguments use is allowed. --  This package is obsolete and replaced by Glib.Values

Pango use Cairo
Pango use Glib

Gdk use Cairo

Gtk use Glib
Gtk use Pango
Gtk use Gdk
Gtk use Cairo

-- GtkAda use Gtk 
-- GtkAda.Bindings use is allowed
-- '.' is not yet taken into account in rules file,
-- but Gtk is heavily using Bindings, so to uncomment 
-- the first line, implement the second!
  ```

  Expected output:

  ```
Error : Gtk is over Gdk, so Gdk.Dnd shall not use Gtk
Error : Gtk is over Gdk, so Gdk.Dnd shall not use Gtk.Target_List
Error : Gtk is over Gdk, so Gdk.Dnd shall not use Gtk
Error : Gtk is over Glib, so Glib.Simple_Action shall not use Gtk.Arguments
Error : Gtk is over Glib, so Glib.Menu_Model shall not use Gtk.Arguments
Error : Gtk is over Gdk, so Gdk.Frame_Clock shall not use Gtk.Arguments
Error : Gtk is over Gdk, so Gdk.Display shall not use Gtk.Arguments
Error : Gtk is over Glib, so Glib.Action_Group shall not use Gtk.Arguments
Error : Gtk is over Glib, so Glib.Application shall not use Gtk.Arguments
Error : Gtk is over Glib, so Glib.Cancellable shall not use Gtk.Arguments
  ```


 GtkAda test suite / A realistic GtkAda description file [Successful]
