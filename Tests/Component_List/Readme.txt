File: --list_components option tests

Those tests check the component list extraction from various rule's files.

The command line is :
> archicheck --list_components RULES_FILE_NAME

The format of the rules may be of various form :

(start code)

GUI contains Gtk
GuI cOnTaInS gtK;
GUI contains Gtk, Glib and Pango
DB contains DB.IO
DB contains also DB.Query

(end)
