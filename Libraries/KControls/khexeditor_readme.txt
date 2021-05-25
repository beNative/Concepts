Software: KhexEditor component for Delphi and Lazarus
Original authorship: Tomas Krysl
-------------------

NOTE:
-------------------
This readme only covers the KHexEditor component. 
Refer to kcontrols_readme.txt for installation requirements and other information.


PLANNED:
-------------------
- improve performance for non-Win32 widget sets in Lazarus
- byte swaps (Little and Big Endian)
- considering: visual control of modified text


KNOWN PROBLEMS:
-------------------
Delphi common:
 -none, best performance

Lazarus common:
 -slow performance on non-Win32 widget sets 

VERSION HISTORY - NEW KCONTROLS PACKAGE
-------------------

VERSION HISTORY - OLD KHEXEDITOR PACKAGE ONLY
-------------------
Version 1.5 (November 2010)
  Added:
    -Append method to append data at a position
  Modified:
    -packages for Delphi XE

Version 1.4 (October 2009)
  Modified:
    -printing and previewing to comply with kcontrols.pas
    -update to Delphi 2010
    -port to Lazarus

Version 1.3 (October 2008)
  Modified:
    -update to Delphi 2009

Version 1.22 (January 2008)
  Modified:
    -packages included for newer Delphi

Version 1.21 (June 2006)
  Modified:
    -bugs fixed when no printer installed    

Version 1.2 (June 2006):
  Added:
    -runtime package
    -UpdateCharMetrics method
    -keyboard features into TKHexEditorPrintPreview
  Modified:
    -UpdateScrollRange modified to avoid
     design-time exceptions in the IDE
    -minor bugfixes

Version 1.1 (May 2006):
  Added:
    -print preview - new component (KHexEditorPreview.pas)
    -PaintTo method to paint the outline to another canvas
  Modified:
    -little modifications

Version 1.0 (April 2006): Initial release