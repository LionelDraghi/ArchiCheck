
# GtkAda test suite



##  GtkAda test suite / File Identification

  Checking that  

  > acc -q -lf -r -I gtkada-master  

  is identifying the same 862 files (once sorted) than  

  > find gtkada-master -name *.ad[sb] | sort  
```  
gtkada-master/src/cairo.adb
gtkada-master/src/cairo.ads
gtkada-master/src/cairo-font_face.ads
...
gtkada-master/testgtk/testcairo_drawing.ads
gtkada-master/testgtk/testgtk.adb
gtkada-master/testgtk/test_rtree.adb
```  


GtkAda test suite / File Identification [Successful](gtkada.md#gtkada-test-suite--file-identification)

##  GtkAda test suite / Unit Identification


  > acc -ld -r -I gtkada-master | sort  

  4785 dependencies expected :  

```  
Cairo.Font_Face package spec depends on Interfaces.C.Strings
Cairo.Font_Face package spec depends on System
Cairo package spec depends on Ada.Unchecked_Deallocation
Cairo package spec depends on Glib
Cairo package spec depends on Glib.Values
Cairo package spec depends on Interfaces.C.Strings
Cairo package spec depends on System
Cairo.Pattern package spec depends on System
Cairo.Scaled_Font package spec depends on Interfaces.C.Strings
Cairo.Surface package spec depends on System
...
Testgtk procedure body depends on Gtk.Enums
Testgtk procedure body depends on Gtk.Main
Testgtk procedure body depends on Gtk.Style_Provider
Testgtk procedure body depends on Main_Windows
Test_Rtree procedure body depends on Ada.Text_IO
Test_Rtree procedure body depends on Gtkada.Canvas_View
Test_Rtree procedure body depends on Gtkada.Canvas_View.Rtrees
Test_Rtree procedure body depends on Gtkada.Style
Trackball package body depends on Ada.Numerics.Aux
View_Gl package spec depends on Gtk.Frame
```  


GtkAda test suite / Unit Identification [Successful](gtkada.md#gtkada-test-suite--unit-identification)

##  GtkAda test suite / A realistic GtkAda description file


  ![](gtk.png)  

  Checking those rules over GtkAda:  

```  
Interfaces use is allowed
System     use is allowed 
Ada        use is allowed
GNAT       use is allowed

Pango may use Cairo
Pango may use Glib

Gdk may use Cairo

GtkAda may use Gtk 

Gtk may use Glib
Gtk may use Pango
Gtk may use Gdk
Gtk may use Cairo

-- Exceptions to previous declarations
GtkAda.Bindings use is allowed
GtkAda.Types    use is allowed
GtkAda.C        use is allowed
Gtk.Arguments   use is allowed -- This package is obsolete and replaced by Glib.Values
```  

  Expected output:  

```  
Error : gtkada-master/src/gdk-dnd.adb:29: Gtk may use Gdk, so Gdk.Dnd shall not use Gtk
Error : gtkada-master/src/gdk-dnd.ads:29: Gtk may use Gdk, so Gdk.Dnd shall not use Gtk
Error : gtkada-master/src/gdk-dnd.ads:30: Gtk may use Gdk, so Gdk.Dnd shall not use Gtk.Target_List
Error : gtkada-master/src/opengl/gtk-glarea.adb:26: GtkAda may use Gtk, so Gtk.GLArea shall not use Gtkada.Handlers
```  


GtkAda test suite / A realistic GtkAda description file [Successful](gtkada.md#gtkada-test-suite--a-realistic-gtkada-description-file)

##  GtkAda test suite / Another realistic GtkAda description file


  Checking those rules over GtkAda:  

```  
Interfaces use is allowed
System     use is allowed 
Ada        use is allowed
GNAT       use is allowed

Low_Level contains Cairo and Glib -- arbitrary name
Utilities contains Pango, Gdk     -- arbitrary name
Utilities is a layer over Low_Level
Gtk is a layer over Utilities

GtkAda may use Gtk 

Gtk may use Low_Level
Gtk may use Utilities

GtkAda may use Low_Level
GtkAda may use Utilities

-- Exceptions to previous declarations
GtkAda.Bindings use is allowed
GtkAda.Types    use is allowed
GtkAda.C        use is allowed
Gtk.Arguments   use is allowed -- This package is obsolete and replaced by Glib.Values
```  

  Expected output:  

```  
Error : gtkada-master/src/gdk-dnd.adb:29: Gdk.Dnd is in Utilities layer, and so shall not use Gtk in the upper Gtk layer
Error : gtkada-master/src/gdk-dnd.adb:29: Gtk may use Utilities, so Gdk.Dnd shall not use Gtk
Error : gtkada-master/src/gdk-dnd.ads:29: Gdk.Dnd is in Utilities layer, and so shall not use Gtk in the upper Gtk layer
Error : gtkada-master/src/gdk-dnd.ads:29: Gtk may use Utilities, so Gdk.Dnd shall not use Gtk
Error : gtkada-master/src/gdk-dnd.ads:30: Gdk.Dnd is in Utilities layer, and so shall not use Gtk.Target_List in the upper Gtk layer
Error : gtkada-master/src/gdk-dnd.ads:30: Gtk may use Utilities, so Gdk.Dnd shall not use Gtk.Target_List
Error : gtkada-master/src/opengl/gtk-glarea.adb:26: GtkAda may use Gtk, so Gtk.GLArea shall not use Gtkada.Handlers
```  


GtkAda test suite / Another realistic GtkAda description file [Successful](gtkada.md#gtkada-test-suite--another-realistic-gtkada-description-file)
