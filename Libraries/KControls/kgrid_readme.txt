Software: KGrid component for Delphi and Lazarus
Original authorship: Tomas Krysl
-------------------

NOTE:
-------------------
This readme only covers the KGrid component. 
Refer to kcontrols_readme.txt for installation requirements and other information.

PLANNED:
-------------------
- filters (can be implemented via editable fixed rows)
- tree columns (can be implemented with a short custom code now)

KNOWN PROBLEMS:
-------------------
Delphi VCL:
 -none, best performance

Lazarus LCL:
 -KDBGrid demo might not draw unicode fields correctly for certain databases. 
  applies for all data aware controls, not just TKDBgrid (was LCL problem, might be fixed already)
 -Win32/Win64: 
   Transparent editor underpainting incorrect if TKGrid is placed onto TPageControl (was widgetset problem, might be fixed already)
 -GTK2: 
   Slightly slow inplace editor performance with huge grids (was widgetset problem, might be fixed already)
 -Carbon: 
   Print and preview not fully working because of missing implementation of affine transformations for device contexts.
 -QT: 
   Slightly slow inplace editor performance with huge grids (was widgetset problem, might be fixed already)
   Checkbox not transparent (was widgetset problem, might be fixed already)
   Scrollbar arrows behave differently (was widgetset problem, might be fixed already)
 -WinCE: 
   Slow inplace editor performance (was widgetset problem, might be fixed already)
   Printer4Lazarus package not supported, you have to remove it from dependency.
   This also means printer setup (via TPrinterSetupDialog) is not supported in TKPrintSetupForm.

CONTRIBUTORS:
-------------------
Gianluca Culot: idea for TKCustomGrid.OnChanged event
JR: some useful functions and ideas
aki: selectable fixed cells


VERSION HISTORY - NEW KCONTROLS PACKAGE
-------------------
Version 1.8 (date not set, to be released): 
  Added:
    -Multiple disjunct selections

Version 1.7 (August 2015):
  Modified:
    -TKDBGrid empty dataset exception fixed
  
Version 1.6 (July 2014):
  Added:
    -Columns property to TKDBGrid (not 100% compatible with TDBGrid)

Version 1.5 (July 2014):
  Added:
    -2 modes of mouse wheel scrolling in TKCustomGrid


VERSION HISTORY - OLD KGRID PACKAGE ONLY
-------------------
Version 1.7 (November 2010): 
  Added:
    -Windows Vista/7 style selection,
    -selectable and editable fixed cells (modified contibutions by aki)
    -packages for Delphi XE
  Modified:
    -removed some obsolete methods, several bugfixes

Version 1.6 (October 2010): 
  Added:
    -column/row/grid autosizing,
    -automatic data type recognition and images in TKDBGrid,
    -improvements in TKGridCellPainter (images, button shapes etc.),
	-cell hints
	-OnMouseDblClickCell event
    -new features based on contributions by JR (OptionsEx property)
	-PaintCell method
  Modified:
    -several bugfixes

Version 1.5 (October 2009): 
  Added:
    -printing/previewing/on the fly previewing (TKPrintPreview, TKPrintPageSetup classes etc.),
     in Lazarus works only for Win32(suppose Win64 too) widget set
    -OnMouseClickCell and OnMouseLeaveCell, OnMouseClickCell events
  Modified:
    -painting and inplace editor performance for GTK2, QT yet slightly improved

Version 1.4 (October 2009): 
  Added:
    -full Lazarus support (all official or beta state widget sets, tested on Win32/Win64, GTK, GTK2, QT)
    -cell merging and splitting (CellSpan property & TKGridCell ColSpan and RowSpan properties)
    -data aware control (TKDBGrid class)
    -column/row individual maximum and minimum extent (TKGridAxisItem MinExtent & MaxExtent properties)
    -smooth scrolling (ScrollModeHorz & ScrollModeVert properties)
    -OnMouseEnterCell and OnMouseLeaveCell events
    -KDBGrid demo for Delphi/Lazarus
  Modified:
    -HotFix 3.10: painting performance optimized for GTK2 
    -major modifications due to platform independency in Lazarus
    -some generous functions moved from KGrids.pas to KGraphics.pas or KFunctions.pas
    -(very) few incompatibilities with previous versions
    -KGrid demo extended
    -no more InnoSetup installation but generous zip package due to platform independency
    -lower case introduced for unit names etc. due to platform independency  
    -documentation completed

Version 1.3 (August 2009): 
  Added:
    -ported to Lazarus (Windows widgetset only)
    -TKCustomGrid.ThroughClick property (clicking a cell will click the inplace editor as well)
    -TKGridTextAttributes - text attributes (multiline text, end ellipsis, path ellipsis, word break)
    -keyboard behavior extended
  Modified:
    -JCL not needed anymore (mainly because of the Lazarus support)
    -inplace editor rendering
    -documentation

Version 1.3 beta (July 2009): 
  Added:
    -TKGridAxisItem.Visible property
    -optional visual indication of hidden columns or rows
    -goIndicateHiddenCells style in TKCustomGrid.Options
    -goMouseCanHideCells style in TKCustomGrid.Options
    -goHeaderAlignment style in TKCustomGrid.Options 
    -TKCustomGrid.SortStyle property
    -TKCustomGrid.UpdateSortMode method 
  Modified:
    -moving columns/rows via OnExchangeCols/OnExchangeRows (both normal and virtual mode)
    -inplace editor rendering
    -documentation
 
Version 1.2 (October 2008): 
  Added:
    -OnChanged event handler
  Modified:
    -update to Delphi 2009
    -painting of the themed header cells fixed
    -painting of some inplace editors fixed (e.g. TRichEdit)

Version 1.1 (April 2008): 
  Added:
    -sorting interface 
    -cell clipping and double buffering
    -TKGridCellPainter class, 
    -improved compatibility with TStringGrid
    -another small improvements and fixes
  Modified:
    -demo has been extended
    -documentation   

Version 1.0 (January 2008):
  Added:
    -index mapping
    -small demo, 
    -documentation
    -many other improvements and bug fixes

Version 0.9 (July 2007): Initial release