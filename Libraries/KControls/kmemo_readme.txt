Software: KMemo component for Delphi and Lazarus
Original authorship: Tomas Krysl
-------------------

NOTE:
-------------------
This readme only covers the KMemo component.
Refer to kcontrols_readme.txt for installation requirements and other information.

PLANNED/STILL MISSING:
-------------------
- fill justify paragraph alignment (now defaults to left justify)
- readers/writers from/into other formats (HTML, ODT, PDF?)
- search/replace commands
- style sheets
- tab stops
- tools for tables
- undo/redo commands

KNOWN PROBLEMS:
-------------------
- The KMemo word processor may still wrap words incorrectly around relative or absolute shapes in certain cases. 
  This is because of missing repositioning of whole line after it has been completely measured
  and its height increased (e.g. due to bigger font or in-line embedded object).
- Print/Preview does not break pages at line ends
- TKMemo has no page/section/column support
- Loading/Saving RTF documents orignally created in different text processors may result in substantial differences in produced documents,
  as only a subset of RTF tags from the RTF specification v 1.9 is supported. RTF compatibility has been primarilly tested 
  with Microsoft Word which seems to comply with the RTF specification the most. Other text processors (such as LibreOffice Writer)
  may show completely different results for RTF documents created by KMemo.
- Microsoft Word will ignore the starting value for numbered lists explicitly set by KMemo. This is because Word creates for every such case 
  a completely new list which is IMO a smut and I won't follow this. Starting value in KMemo is always bound to the paragraph 
  and a new RTF control word has been introduced for this: \lsstartat. 
  Starting value given by list level (\levelstartat) is only used as default starting value.
- Metafiles won't be loaded to KMemo under Linux because of the missing TMetafile support in Lazarus under Linux.


CONTRIBUTORS:
-------------------


VERSION HISTORY - NEW KCONTROLS PACKAGE
-------------------
Version 1.8 (date not set, to be released): 
  Added:
    -tools for relative objects (images, text boxes)
    -OnBlockClick, OnBlockDblClick events
    -TKCustomMemo.BlockAt method
    -TKMemoBlocks.LoadFromStream, TKMemoBlocks.SaveToStream

Version 1.7 (August 2015): 
  Added:
    -many new paragraph and text formatting styles
    -TKMemoContainer class to support nested blocks, embedded objects and floating shapes
    -Advanced table support through TKMemoTable, TKMemoTableRow and TKMemoTableCell classes
    -Advanced list style support through TKMemoListTable, TKMemoList and TKMemoListLevel classes
    -Hyperlinks through TKMemoHyperlink class
    -Rich Text Format reader and writer (KMemoRTF.pas)
    -clipboard operations (copy, cut, paste), also with full Rich Text Format support
    -print/preview support 
    -TKMemoFrame and formatting dialogs that form a simple rich text editor, which you can place to your projects without coding
  Modified:
    -Reworked text and paragraph formatting through TKMemoTextStyle, TKMemoParaStyle classes
    -Reworked TKMemoBlocks, TKMemoTextBlock and TKMemoImageBlock

Version 1.5 (July 2014): 
  Added:
    -TKMemo component first introduced (early alpha state)
