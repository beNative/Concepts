{
  Copyright (C) 2013-2017 Tim Sinaeve tim.sinaeve@gmail.com

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}

{
  The Original Code is part of 'Extended components library, Version 1.3.16'.
  The Initial Developer of the Original Code (Ex_Grid.pas) is Roman M. Mochalov
  (roman@tersy.ru). Portions created by the Initial Developer are Copyright
  (C) 1997-2007. All Rights Reserved. You may obtain a copy of the original code
  at http://www.tersy.ru/~roman/download/

  Changes by Tim Sinaeve:
    - The original comments were translated from Russian to English using
       machine translation.
    - If <RETURN> is used to navigate through cells, it does not force the cell
      in Edit mode anymore before we can go to the next cell. So we only have
      to press <RETURN> once to go to the next cell.
    - Added <LastSelectableCol> function that returns the index of the last
      visible column where it is possible to establish a cursor.
    - Made IsCellHighlighted virtual to use in TCustomDBGridView
    - Added <SHIFT>-<CTRL>-<+> shortcut to auto resize all columns for all
      visible rows.
    - Bugfix : HeaderClick is only fired when AllowClicking returned from
      HeaderClicking is True.
    - Clicking on flat headers is allowed now. The drawing of the pressed down
      section needs to be adjusted (TODO)
    - Bugfix : Focused returned in some cases False if the inplace editor of
      the grid was focused.
    - Added AppendRow event to allow the creation of new rows when moving the
      cursor through the grid.
        [goMouseWheelUp, goMouseWheelDown] were added to TGridCursorOffset to
        distinguish mouse wheel movements from keyboard cursor movements.
    - Bugfix: Added overridden GetDisplayName to TGridHeaderSection to return
      the header caption as the DisplayName.
    - Bugfix: Call InvalidateCell in CheckClick event dispatch method to show
      updated check status.
    - Removed compiler directives to keep compatibility with older compiler
      versions.
    - Code ported to support unicode and later versions of Delphi.
    - Added theming support
    - Removed following properties:
          - Ctl3D
          - ParentCtl3D
    - Renamed property ThemeXPEnabled to ThemingEnabled
    - Renamed property TCustomGridColumn.Title to HeaderSection
    - Added property EditAlighnment to TGridColumn

  TODO:
    * Columns (or sections?) :
        - Add Hint property
        - Add Font property

    * Sections
        - Add Height property so that height can vary per section level.

    * AutoSizeColumns for all rows (extra parameter) does not work correctly.

    * An event that is triggered before user tries to leave an edited cell. This
      event should only be triggered when the content of the cell was changed.
      Other criteria that should be met:
        - Edit mode (Editing property is True)
        - Cell content was changed (cell editor text differs from cell text)
        - The cell is not readonly
        - ...

    * The sort arrows should be drawn instead of using a bitmap, so that the
      display can be scaled for larger headers.

  KNOWN BUGS:
    * RETURN in edit mode on last cell resulted in unwanted saving of #13#10
      sequences.


  TODO
    - ColumnsFullDrag property does not work
    - DefaultEditMenu property does not work

}

unit DDuce.Components.GridView;

{$I ..\DDuce.inc}

interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.CommCtrl,
  System.SysUtils, System.Classes, System.Math,
  Vcl.Themes, Vcl.Forms, Vcl.Controls, Vcl.Graphics, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ImgList, Vcl.Mask;

type
  TBitmap = Vcl.Graphics.TBitmap;

const
  MAX_COLUMN_WIDTH = MaxInt div 2;

type
  TGridHeaderSections = class;
  TCustomGridHeader   = class;
  TCustomGridColumn   = class;
  TGridColumns        = class;
  TCustomGridRows     = class;
  TCustomGridFixed    = class;
  TCustomGridView     = class;

  TGridEditStyle = (
    geSimple,
    geEllipsis,
    gePickList,
    geDataList,
    geUserDefine
  );

  TGridCheckKind  = (
    gcNone,
    gcCheckBox,
    gcRadioButton,
    gcUserDefine
  );

  TGridCheckStyle = (
    csFlat,
    cs3D,
    csWin95
  );

  { TGridCell }

  TGridCell = record
    Col : Integer;
    Row : Integer;
  end;

{ TGridHeaderSection }
{
  Header section.

  Properties:
    ColumnIndex -       The index of the corresponding column. For a header
                        based on multiple columns or sections, this is the index
                        of the first section/column in that section.
    BoundsRect -        The rectangular area of the section. It does not
                        include the boundary of any sub sections.
    FirstColumnIndex -  The index of the left most column in the current header
                        section.
    FixedColumn -       True if the header (or one of its contained columns)
                        is fixed.
    Header -            Reference to the header collection object.
    Level -             The level of the header section. Uppermost sections
                        have level 0.
    Parent -            Reference to the section's parent section.
    ParentSections -    Reference to the list of parent sections, to which the
                        section belongs.
    ResizeColumnIndex - Index of the column which will be resized when the
                        section is resized using the mouse.
                        For a section with subheaders, this is the index of the
                        last sub header.
    Visible -           Controls visibility of the section (and its sub
                        sections).
    Alignment -         The alignment of the header's title caption.
    Caption -           The header's caption.
    Sections -          List of all sub sections.
    Width -             The section's width.
    WordWrap -          Wraps the header's caption.
}

  TGridHeaderSection = class(TCollectionItem)
  private
    FSections    : TGridHeaderSections;
    FCaption     : string;
    FWidth       : Integer;
    FAlignment   : TAlignment;
    FWordWrap    : Boolean;
    FBoundsRect  : TRect;
    FColumnIndex : Integer;

    function IsSectionsStored: Boolean;
    function IsWidthStored: Boolean;
    function GetAllowClick: Boolean;
    function GetBoundsRect: TRect;
    function GetFirstColumnIndex: Integer;
    function GetFixedColumn: Boolean;
    function GetHeader: TCustomGridHeader;
    function GetLevel: Integer;
    function GetParent: TGridHeaderSection;
    function GetParentSections: TGridHeaderSections;
    function GetResizeColumnIndex: Integer;
    function GetSections: TGridHeaderSections;
    function GetVisible: Boolean;
    function GetWidth: Integer;
    procedure SetAlignment(Value: TAlignment);
    procedure SetCaption(const Value: string);
    procedure SetSections(Value: TGridHeaderSections);
    procedure SetWidth(Value: Integer);
    procedure SetWordWrap(Value: Boolean);

  protected
    function GetDisplayName: string; override;

  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    property AllowClick : Boolean
      read GetAllowClick;

    property BoundsRect : TRect
      read GetBoundsRect;

    property ColumnIndex: Integer
      read FColumnIndex;

    property FirstColumnIndex: Integer
      read GetFirstColumnIndex;

    property FixedColumn: Boolean
      read GetFixedColumn;

    property Header: TCustomGridHeader
      read GetHeader;

    property Level: Integer
      read GetLevel;

    property Parent: TGridHeaderSection
      read GetParent;

    property ParentSections: TGridHeaderSections
      read GetParentSections;

    property ResizeColumnIndex: Integer
      read GetResizeColumnIndex;

    property Visible: Boolean
      read GetVisible;

  published
    property Alignment: TAlignment
      read FAlignment write SetAlignment default taLeftJustify;

    property Caption: string
      read FCaption write SetCaption;

    property Width: Integer
      read GetWidth write SetWidth stored IsWidthStored default 64;

    property WordWrap: Boolean
      read FWordWrap write SetWordWrap default False;

    property Sections: TGridHeaderSections
      read GetSections write SetSections stored IsSectionsStored;
  end;

{ TGridHeaderSections }
{
  Collection holding the header sections of the grid view.

  Procedures:
    Add -         Adds a new section.

  Properties:
    Header -      Reference to the header collection object.
    MaxColumn -   The upper most column index.
    MaxLevel -    The maximum level of sub sections.
    Owner -       Reference to the section's owner.
    Sections -    List of sub sections.
}

  TGridHeaderSections = class(TCollection)
  private
    FHeader       : TCustomGridHeader;
    FOwnerSection : TGridHeaderSection;

    function GetMaxColumn: Integer;
    function GetMaxLevel: Integer;
    function GetSection(Index: Integer): TGridHeaderSection;
    procedure SetSection(Index: Integer; Value: TGridHeaderSection);

  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;

  public
    constructor Create(
      AHeader       : TCustomGridHeader;
      AOwnerSection : TGridHeaderSection
    ); virtual;

    function Add: TGridHeaderSection;

    property Header: TCustomGridHeader
      read FHeader;

    property MaxColumn: Integer
      read GetMaxColumn;

    property MaxLevel: Integer
      read GetMaxLevel;

    property OwnerSection: TGridHeaderSection
      read FOwnerSection;

    property Sections[Index: Integer]: TGridHeaderSection
      read GetSection write SetSection; default;
  end;

{ TCustomGridHeader }
 {
  Class representing the GridView's header.

  Procedures:
    SynchronizeSections - Equalizes a quantity of lower sections with the
                          quantity the columns indicated.
    UpdateSections      - Draw of the internal parameters of sections
                          (BoundsRect and ColumnIndex). The given parameters
                          initialize one time with a change in the title for the
                          acceleration work with the sections.

  Properties:
    Grid -              reference to the grid view object.
    Height -            The GridView's height
    Images -            of the picture of the sections of title.
    MaxColumn -         the maximum index of column.
    MaxLevel -          the maximum level of subtitles.
    Width -             the width
    AutoHeight -        to automatically select the height of sections.
    AutoSynchronize -   Automatically synchronize the sections with the columns.

    Color -             Background color.
    Flat -              the form of mapping the sections of title. If Flat = True,
                        to the sections of zagolovoka are reflected by flat, in
                        contrary slechaye - type of buttons.
    Font -              type.
    FullSynchronizing - to completely synchronize the sections of title s
                        by columns, including the text of title and the alignment
                        text. Otherwise it is synchronized only a quantity of
                        sections with a quantity of columns.
    GridColor -         to take as the color of title the color of table.
                        NOTE: if GridColor = True, then separating
                        the lines of the sections of title are drawn by single lines,
                        as grid in usual cells. Otherwise
                        separating lines are drawn by dual.
    GridFont -          to take as type of title type of table.
    SectionHeight -     the height of one section (subtitle). With that
                        published property AutoHeight it is selected taking into
                        account the height pictures, 3D effect, the value of
                        property GridColor and the height of type.
 }

  TCustomGridHeader = class(TPersistent)
  private
    FGrid              : TCustomGridView;
    FSections          : TGridHeaderSections;
    FSectionHeight     : Integer;
    FAutoHeight        : Boolean;
    FSynchronized      : Boolean;
    FAutoSynchronize   : Boolean;
    FFullSynchronizing : Boolean;
    FColor             : TColor;
    FGridColor         : Boolean;
    FFont              : TFont;
    FGridFont          : Boolean;
    FImages            : TImageList;
    FImagesLink        : TChangeLink;
    FFlat              : Boolean;
    FOnChange          : TNotifyEvent;

    procedure ImagesChange(Sender: TObject);
    function IsColorStored: Boolean;
    function IsFontStored: Boolean;
    function IsSectionsStored: Boolean;
    procedure FontChange(Sender: TObject);
    function GetHeight: Integer;
    function GetMaxColumn: Integer;
    function GetMaxLevel: Integer;
    function GetWidth: Integer;
    procedure SetAutoHeight(Value: Boolean);
    procedure SetAutoSynchronize(Value: Boolean);
    procedure SetColor(Value: TColor);
    procedure SetFlat(Value: Boolean);
    procedure SetFont(Value: TFont);
    procedure SetFullSynchronizing(Value: Boolean);
    procedure SetGridColor(Value: Boolean);
    procedure SetGridFont(Value: Boolean);
    procedure SetImages(Value: TImageList);
    procedure SetSections(Value: TGridHeaderSections);
    procedure SetSectionHeight(Value: Integer);
    procedure SetSynchronized(Value: Boolean);

  protected
    procedure Change; virtual;
    procedure GridColorChanged(NewColor: TColor); virtual;
    procedure GridFontChanged(NewFont: TFont); virtual;

  public
    constructor Create(AGrid: TCustomGridView); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SynchronizeSections;
    procedure UpdateSections; virtual;

    property Grid: TCustomGridView
      read FGrid;

    property Height: Integer
      read GetHeight;

    property MaxColumn: Integer
      read GetMaxColumn;

    property MaxLevel: Integer
      read GetMaxLevel;

    property Width: Integer
      read GetWidth;

    property OnChange: TNotifyEvent
      read FOnChange write FOnChange;

    property AutoHeight: Boolean
      read FAutoHeight write SetAutoHeight default False;

    property AutoSynchronize: Boolean
      read FAutoSynchronize write SetAutoSynchronize default True;

    property Color: TColor
      read FColor write SetColor stored IsColorStored default clBtnFace;

    property Images: TImageList
      read FImages write SetImages;

    property Flat: Boolean
      read FFlat write SetFlat default True;

    property Font: TFont
      read FFont write SetFont stored IsFontStored;

    { For each created column, a corresponding section is created automatically. }
    property FullSynchronizing: Boolean
      read FFullSynchronizing write SetFullSynchronizing default False;

    property GridColor: Boolean
      read FGridColor write SetGridColor default False;

    property GridFont: Boolean
      read FGridFont write SetGridFont default True;

    property Sections: TGridHeaderSections
      read FSections write SetSections stored IsSectionsStored;

    property SectionHeight: Integer
      read FSectionHeight write SetSectionHeight default 17;

    { If enabled the header sections synchronized with the columns. }
    property Synchronized: Boolean
      read FSynchronized write SetSynchronized stored True;
  end;

  TGridHeader = class(TCustomGridHeader)
  published
    property AutoHeight;
    property AutoSynchronize;
    property Color;
    property Images;
    property Flat;
    property Font;
    property FullSynchronizing;
    property GridColor;
    property GridFont;
    property Sections;
    property SectionHeight;
    property Synchronized;
  end;

{ TCustomGridColumn }
{
  Column of table.

  Properties:
    Columns -     Reference to the list of columns.
    AlignEdit -   it is necessary whether to equalize text in the line of
                  introduction into the correspondence with the offset of the
                  text of column.
    Alignment -   the offset of the text of column.
    AllowClick -  it is possible or not to press on the title of column.
    Caption -     the text of title.
    CheckKind -   the type of the flag of column.
    EditMask -    the mask of the line of the editing of column.
    EditStyle -   the style of the line of the introduction of column.
    FixedSize -   the width of column is constant (it is not possible to change
                  with mouse in RunTime).
    MaxLength -   the maximum length of the edited text. If the value
                  it is established in -1, then line will be ReadOnly (at that time,
                  as cell itself is not).
    MaxWidth -    Maximum column width.
    MinWidth -    Minimum column width.
    PickList -    Contents of the dropdown list.
    Tag -         Tag property from TComponent.
    WantReturns - can be text in the cells to contain the symbols of the transfer
                  line. If WantReturns is advanced in False, then the pressure
                  key ENTER in the line of introduction it will be been ignored, simovly
                  transfer they will be reflected as usual symbols.
    ReadOnly  -   Specifies if the cells of this column can be edited.
    TabStop -     Determinates if it is possible to establish a cursor in this
                  column. (This behaviour can be overridden in the grid's
                  <OnCellAcceptCursor> event).
            -       reference in the section of the title of column.
    Visible -     Toggles column visibility.
    Width -       The physical width of the column. If the column is set
                  invisible this property will return zero.
    DefWidth -    The default width of column. It does not depend on the
                  visibility of the column.
}

  TGridEditWordWrap = (
    ewAuto,
    ewEnabled,
    ewDisabled
  );

  TGridColumnClass = class of TCustomGridColumn;

  TCustomGridColumn = class(TCollectionItem)
  private
    FAlignEdit      : Boolean;
    FAlignment      : TAlignment;
    FAllowClick     : Boolean;
    FAllowEdit      : Boolean;
    FCaption        : string;
    FCheckAlignment : TAlignment;
    FCheckKind      : TGridCheckKind;
    FColumns        : TGridColumns;
    FEditMask       : string;
    FEditStyle      : TGridEditStyle;
    FEditWordWrap   : TGridEditWordWrap;
    FFixedSize      : Boolean;
    FMaxLength      : Integer;
    FMaxWidth       : Integer;
    FMinWidth       : Integer;
    FPickList       : TStrings;
    FReadOnly       : Boolean;
    FTabStop        : Boolean;
    FTag            : Integer;
    FVisible        : Boolean;
    FWantReturns    : Boolean;
    FWidth          : Integer;
    FWordWrap       : Boolean;

    function GetEditAlignment: TAlignment;
    function GetGrid: TCustomGridView;
    function GetPickList: TStrings;
    function GetPickListCount: Integer;
    function GetHeaderSection: TGridHeaderSection;
    function GetWidth: Integer;
    function IsPickListStored: Boolean;
    procedure SetAlignEdit(Value: Boolean);
    procedure SetAllowEdit(Value: Boolean);
    procedure SetCheckAlignment(Value: TAlignment);
    procedure SetCheckKind(Value: TGridCheckKind);
    procedure SetEditWordWrap(Value: TGridEditWordWrap);
    procedure SetMaxWidth(Value: Integer);
    procedure SetMinWidth(Value: Integer);
    procedure SetPickList(Value: TStrings);
    procedure SetTabStop(Value: Boolean);
    procedure SetWantReturns(Value: Boolean);
    procedure SetWordWrap(Value: Boolean);

  protected
    function GetDisplayName: string; override;
    procedure SetAlignment(Value: TAlignment); virtual;
    procedure SetCaption(const Value: string); virtual;
    procedure SetEditMask(const Value: string); virtual;
    procedure SetEditStyle(Value: TGRidEditStyle); virtual;
    procedure SetMaxLength(Value: Integer); virtual;
    procedure SetReadOnly(Value: Boolean); virtual;
    procedure SetVisible(Value: Boolean); virtual;
    procedure SetWidth(Value: Integer); virtual;

  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    property AlignEdit: Boolean
      read FAlignEdit write SetAlignEdit default False;

    property Alignment: TAlignment
      read FAlignment write SetAlignment default taLeftJustify;

    property AllowClick: Boolean
      read FAllowClick write FAllowClick default True;

    property AllowEdit: Boolean
      read FAllowEdit write SetAllowEdit default True;

    property Caption: string
      read FCaption write SetCaption;

    property CheckAlignment: TAlignment
      read FCheckAlignment write SetCheckAlignment default taLeftJustify;

    property CheckKind: TGridCheckKind
      read FCheckKind write SetCheckKind default gcNone;

    property Columns: TGridColumns
      read FColumns;

    property DefWidth: Integer
      read FWidth write SetWidth default 64;

    property EditAlignment: TAlignment
      read GetEditAlignment;

    property EditMask: string
      read FEditMask write SetEditMask;

    property EditStyle: TGridEditStyle
      read FEditStyle write SetEditStyle default geSimple;

    property EditWordWrap: TGridEditWordWrap
      read FEditWordWrap write SetEditWordWrap default ewAuto;

    property FixedSize: Boolean
      read FFixedSize write FFixedSize default False;

    property Grid: TCustomGridView
      read GetGrid;

    property MaxLength: Integer read
      FMaxLength write SetMaxLength default 0;

    property MaxWidth: Integer
      read FMaxWidth write SetMaxWidth default 10000;

    property MinWidth: Integer
      read FMinWidth write SetMinWidth default 0;

    property PickList: TStrings
      read GetPickList write SetPickList stored IsPickListStored;

    property PickListCount: Integer
      read GetPickListCount;

    property ReadOnly: Boolean read
      FReadOnly write SetReadOnly default False;

    property TabStop: Boolean
      read FTabStop write SetTabStop default True;

    property Tag: Integer
      read FTag write FTag default 0;

    property HeaderSection: TGridHeaderSection
      read GetHeaderSection;

    property Visible: Boolean
      read FVisible write SetVisible default True;

    property WantReturns: Boolean
      read FWantReturns write SetWantReturns default False;

    property Width: Integer
      read GetWidth write SetWidth stored False default 64;

    property WordWrap: Boolean
      read FWordWrap write SetWordWrap default False;
  end;

  TGridColumn = class(TCustomGridColumn)
  published
    property AlignEdit;
    property Alignment;
    property AllowClick;
    property AllowEdit;
    property Caption;
    property CheckAlignment;
    property CheckKind;
    property DefWidth;
    property EditAlignment;
    property EditMask;
    property EditStyle;
    property EditWordWrap;
    property FixedSize;
    property HeaderSection;
    property MaxLength;
    property MaxWidth;
    property MinWidth;
    property PickList;
    property ReadOnly;
    property TabStop;
    property Tag;
    property Visible;
    property WantReturns;
    property Width;
    property WordWrap;
  end;

{ TGridColumns }

  TGridColumns = class(TCollection)
  private
    FGrid     : TCustomGridView;
    FOnChange : TNotifyEvent;

    function GetColumn(Index: Integer): TGridColumn;
    function GetLayout: string;
    procedure SetColumn(Index: Integer; Value: TGridColumn);
    procedure SetLayout(const Value: string);

  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;

    property OnChange: TNotifyEvent
      read FOnChange write FOnChange;

  public
    constructor Create(AGrid: TCustomGridView); virtual;

    function Add: TGridColumn;

    property Columns[Index: Integer]: TGridColumn
      read GetColumn write SetColumn; default;

    property Grid: TCustomGridView
      read FGrid;

    { A comma seperated list of column widths. The values are automatically
      syncronized with the column properties }
    property Layout: string
      read GetLayout write SetLayout;
  end;

{ TCustomGridRows }
{
  Table rows.

  Properties:
    MaxCount -    the maximum allowed quantity of rows in the grid. It depends
                  on the height of line.
    AutoHeight -  Automatically select the height of lines.
    Count -       Quantity of rows in the grid.
    Grid -        Reference to the grid.
    Height -      the height of one line. With the established property AutoHeight
                  it is selected taking into account the presence of flags, height of pictures,
                  the height of type of table, height of type, 3D form, the value
                  property Fixed.GridColor, the vertical displacement of the text
                  cell, the presence of grid.
                  NOTE: If the height of line is less than the height of the text
                  of yacheyki, are possible Glucks from the otrisovkoy button of the line of the introduction
                  table. Sootvestviye of the height of line and type of the table
                  one should track by hand or establish AutoHeight in
                  True.

  Events:
    OnChange -    Event to track property changes.
}

  TCustomGridRows = class(TPersistent)
  private
    FGrid       : TCustomGridView;
    FCount      : Integer;
    FHeight     : Integer;
    FAutoHeight : Boolean;
    FOnChange   : TNotifyEvent;

    function GetMaxCount: Integer;
    procedure SetAutoHeight(Value: Boolean);
    procedure SetHeight(Value: Integer);

  protected
    procedure Change; virtual;
    procedure SetCount(Value: Integer); virtual;

  public
    constructor Create(AGrid: TCustomGridView); virtual;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    property AutoHeight: Boolean
      read FAutoHeight write SetAutoHeight default False;

    property Count: Integer
      read FCount write SetCount default 0;

    property Grid: TCustomGridView
      read FGrid;

    property Height: Integer
      read FHeight write SetHeight default 17;

    property MaxCount: Integer
      read GetMaxCount;

    property OnChange: TNotifyEvent
      read FOnChange write FOnChange;
  end;

  TGridRows = class(TCustomGridRows)
  published
    property AutoHeight;
    property Count;
    property Height;
    property OnChange;
  end;

{ TCustomGridFixed }
{
  Parameters of fixed columns in the grid view.

  Properties:
    Count -     a quantity of fixed columns. It cannot be more than
                a quantity of columns of table minus 1.
    Color -     the color of the fixed part.
    Flat -      the form of mapping the fixed cells. If Flat = True, then
                cells are reflected by flat, otherwise - in the form of buttons.
    Font -      type of the text of cells.
    GridColor - to take as the color of those fixed the color of table.
                NOTE: if GridColor = True, then the grid of those fixed
                cells is drawn by single linimya, as in usual cells. In
                the contrary case grid - dual separating line as
                in the title
    GridFont -  to take as type of those fixed type of table.
    ShowDivider - Forces a vertical divider line to be shown between the fixed
                  and the normal columns if the property <Gridlines> of the grid
                  is set to False.

  Events:
    OnChange -  event to a change in the parameters.
}

  TCustomGridFixed = class(TPersistent)
  private
    FGrid        : TCustomGridView;
    FCount       : Integer;
    FColor       : TColor;
    FGridColor   : Boolean;
    FFont        : TFont;
    FGridFont    : Boolean;
    FFlat        : Boolean;
    FShowDivider : Boolean;
    FOnChange    : TNotifyEvent;

    function IsColorStored: Boolean;
    function IsFontStored: Boolean;

    procedure FontChange(Sender: TObject);

    procedure SetColor(Value: TColor);
    procedure SetFlat(Value: Boolean);
    procedure SetFont(Value: TFont);
    procedure SetGridColor(Value: Boolean);
    procedure SetGridFont(Value: Boolean);
    procedure SetShowDivider(Value: Boolean);

  protected
    procedure Change; virtual;
    procedure GridColorChanged(NewColor: TColor); virtual;
    procedure GridFontChanged(NewFont: TFont); virtual;
    procedure SetCount(Value: Integer); virtual;

  public
    constructor Create(AGrid: TCustomGridView); virtual;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    property Color: TColor
      read FColor write SetColor stored IsColorStored default clBtnFace;

    property Count: Integer
      read FCount write SetCount default 0;

    property Flat: Boolean
      read FFlat write SetFlat default True;

    property Font: TFont
      read FFont write SetFont stored IsFontStored;

    property Grid: TCustomGridView
      read FGrid;

    property GridColor: Boolean
      read FGridColor write SetGridColor default False;

    property GridFont: Boolean
      read FGridFont write SetGridFont default True;

    property ShowDivider: Boolean
      read FShowDivider write SetShowDivider default True;

    property OnChange: TNotifyEvent
      read FOnChange write FOnChange;
  end;

  TGridFixed = class(TCustomGridFixed)
  published
    property Color;
    property Count;
    property Flat;
    property Font;
    property GridColor;
    property GridFont;
    property ShowDivider;
  end;

{ TGridScrollBar }
{
  Scroll bar properties of the grid view.

  Procedures:
    Change -        it is caused to paste after a change in the position.
    Scroll -        it is caused directly before a change in the position for
                    the correction of new position.
    ScrollGrid -    To skrollirovat' the image of grid. It is caused with the change
                    the position of cursor.
    ScrollMessage - to process communication Windows about the pressure on the scroller.
    SetParams -     to establish limits.
    SetPosition -   to establish position.
    SetPositionEx - to establish position.

  Properties:
    Grid -           Reference to the grid component.
    Kind -           the type of scroller (horizontal or vertical).
    LineStep -       low pitch.
    LineSize -       the size of the skrolliruyemoy part of the window during the displacement to 1
                     position.
    Min -            the minimum.
    max of -         maximum.
    PageStep -       steep pitch.
    Position -       the current position.
    Tracking -       the sign of a synchronous change in the position with the displacement
                     myshkoy central cursor.
    Visible -        the visibility of scroller.
    Events:
    OnChange -       occurred changes in the position.
    OnChangeParams - occurred changes in the parameters (step, maximum).
    OnScroll -       it is caused directly before a change in the position
                     scroller.
  }

  TGridScrollEvent = procedure(
    Sender        : TObject;
    ScrollCode    : Integer;
    var ScrollPos : Integer
  ) of object;

  TGridScrollBar = class(TPersistent)
  private
    FGrid           : TCustomGridView;
    FKind           : TScrollBarKind;
    FPosition       : Integer;
    FMin            : Integer;
    FMax            : Integer;
    FPageStep       : Integer;
    FLineStep       : Integer;
    FLineSize       : Integer;
    FTracking       : Boolean;
    FVisible        : Boolean;
    FUpdateLock     : Integer;
    FOnScroll       : TGridScrollEvent;
    FOnChange       : TNotifyEvent;
    FOnChangeParams : TNotifyEvent;

    function GetRange: Integer;
    procedure SetLineSize(Value: Integer);
    procedure SetLineStep(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetPageStep(Value: Integer);
    procedure SetVisible(Value: Boolean);

  protected
    procedure Change; virtual;
    procedure ChangeParams; virtual;
    procedure Scroll(ScrollCode: Integer; var ScrollPos: Integer); virtual;
    procedure ScrollMessage(var Message: TWMScroll); virtual;
    procedure SetParams(AMin, AMax, APageStep, ALineStep: Integer); virtual;
    procedure SetPosition(Value: Integer);
    procedure SetPositionEx(Value: Integer; ScrollCode: Integer); virtual;
    procedure Update; virtual;

  public
    constructor Create(AGrid: TCustomGridView; AKind: TScrollBarKind); virtual;

    procedure Assign(Source: TPersistent); override;
    procedure LockUpdate;
    procedure UnLockUpdate;

    property Grid: TCustomGridView
      read FGrid;

    property Kind: TScrollBarKind
      read FKind;

    property LineStep: Integer
      read FLineStep write SetLineStep;

    property LineSize: Integer
      read FLineStep write SetLineSize;

    property Max: Integer
      read FMax write SetMax;

    property Min: Integer
      read FMin write SetMin;

    property PageStep: Integer
      read FPageStep write SetPageStep;

    property Position: Integer
      read FPosition write SetPosition;

    property Range: Integer
      read GetRange;

    property UpdateLock: Integer
      read FUpdateLock;

    property OnChange: TNotifyEvent
      read FOnChange write FOnChange;

    property OnChangeParams: TNotifyEvent
      read FOnChangeParams write FOnChangeParams;

    property OnScroll: TGridScrollEvent
      read FOnScroll write FOnScroll;

  published
    property Tracking: Boolean
      read FTracking write FTracking default True;

    property Visible: Boolean
      read FVisible write SetVisible default True;

  end;

{ TGridListBox }
{
   Dropdown inplace editor.
}

  TGridListBox = class(TCustomListBox)
  private
    FGrid       : TCustomGridView;
    FSearchText : String;
    FSearchTime : Integer;

  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseUp(
      Button : TMouseButton;
      Shift  : TShiftState;
      X, Y   : Integer
    ); override;

  public
    constructor Create(AOwner: TComponent); override;

    property Grid: TCustomGridView
      read FGrid;
  end;

{ TCustomGridEdit }
  {
    Storka of the introduction of table. It can contain button with the dots or button s
    falling out of spisokm.
    Procedures:
    GetDropList -      to obtain the falling out list of the line of editing in
                       sootvestvii with the type of line. Function should be overlapped
                       for obtaining creating the list of another class,
                       different from TGridListBox. For example, the heir
                       TDBGridView vovzrashchayet list TDBLookupListBox for
                       line of the type TDataList.
    PaintButton -      to draw button.
    UpdateBounds -     the determination of position and size of line with that following
                       by its demonstration (it is caused directly from the method
                       Show).
    UpdateColors -     the determination of the color of cell and type.
    UpdateContents -   the determination of the text of line, maximum length of the line
                       and the possibility of editing.
    UpdateList -       tuning exterior view vypadashchengo of list.
    UpdateListBounds - determination of position and size of the falling out list.
    UpdateListItems -  the filling of the current falling out list with values.
    UpdateListValue -  the installation of the selected value of the falling out list.
                       It is caused from method CloseUp with the selection of value.
    UpdateStyle -      determination of the type of line.
    CloseUp -          to shut the falling out list.
    DropDown -         to open the falling out list (only for the line with the type
                       gePickList, geDataList).
    Press -            pressure on the button with the dots (by mouse or they will gain
                       Ctrl+.Enter with WantReturns = False).
    SelectNext -       should be selected the following value from the list (they will gain
                       Ctrl+.Enter with WantReturns = False).
    Properties:
    ButtonRect -       the rectangle of button.
    ButtonWidth -      the width of button.
    DropDownCount -    a quantity of lines in the dropdown list.
    DropList -         the current falling out list of line. It can not coincide
                       with DropListBox dl yanaslednikov (for example, for Lookup
                       cells in TDBGridView).
    DropListBox -      its own falling out list of line.
    DropListVisible -  the visibility of the falling out list.
    EditStyle -        the type of line. It can take the following values:
                         geSimple -         the simple line of editing.
                         geEllipsis -       line with the button... (with the dots)
                         gePickList -       line with the button of the falling out list
                         geDataList -       to use in combination with Lookup fields
                         geUserDefine -     line with the button of user.
    Grid -             reference to the button.
    LineCount -        a quantity of lines in the line.
    WantReturns -      can text in the line contain the symbols of transfer.
    WordWrap -         is permitted the automatic transfer of words.
  }

  TGridEditClass = class of TCustomGridEdit;

  TCustomGridEdit = class(TCustomMaskEdit)
  private
    FGrid            : TCustomGridView;
    FClickTime       : Integer;
    FEditStyle       : TGridEditStyle;
    FWantReturns     : Boolean;
    FWordWrap        : Boolean;
    FDropDownCount   : Integer;
    FDropListVisible : Boolean;
    FPickList        : TGridListBox;
    FActiveList      : TWinControl;
    FButtonWidth     : Integer;
    FButtonTracking  : Boolean;
    FButtonPressed   : Boolean;
    FDefocusing      : Boolean;
    FAlignment       : TAlignment;

    function GetButtonRect: TRect;
    function GetLineCount: Integer;
    function GetVisible: Boolean;
    procedure ListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetAlignment(Value: TAlignment);
    procedure SetButtonWidth(Value: Integer);
    procedure SetDropListVisible(Value: Boolean);
    procedure SetEditStyle(Value: TGridEditStyle);
    procedure SetWordWrap(Value: Boolean);

    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMPaste(var Message); message WM_PASTE;
    procedure WMCut(var Message); message WM_CUT;
    procedure WMClear(var Message); message WM_CLEAR;
    procedure WMUndo(var Message); message WM_UNDO;
    procedure WMCancelMode(var Message: TMessage); message WM_CANCELMODE;
    procedure WMKillFocus(var Message: TMessage); message WM_KILLFOCUS;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMContextMenu(var Message: TMessage); message WM_CONTEXTMENU;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;

  protected
    procedure Change; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DblClick; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    function EditCanModify: Boolean; override;
    function GetDropList: TWinControl; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure PaintButton(DC: HDC); virtual;
    procedure PaintWindow(DC: HDC); override;
    procedure StartButtonTracking(X, Y: Integer);
    procedure StepButtonTracking(X, Y: Integer);
    procedure StopButtonTracking;
    procedure UpdateBounds(ScrollCaret: Boolean); virtual;
    procedure UpdateColors; virtual;
    procedure UpdateContents; virtual;
    procedure UpdateList; virtual;
    procedure UpdateListBounds; virtual;
    procedure UpdateListItems; virtual;
    procedure UpdateListValue(Accept: Boolean); virtual;
    procedure UpdateStyle; virtual;
    procedure WndProc(var Message: TMessage); override;

  public
    constructor Create(AOwner: TComponent); override;

    procedure CloseUp(Accept: Boolean); virtual;
    procedure Deselect;
    procedure DropDown;
    procedure Invalidate; override;
    procedure Hide;
    procedure Press;
    procedure SelectNext;
    procedure SetFocus; override;
    procedure Show;

    property ActiveList: TWinControl
      read FActiveList write FActiveList;

    property Alignment: TAlignment
      read FAlignment write SetAlignment default taLeftJustify;

    property ButtonRect: TRect
      read GetButtonRect;

    property ButtonWidth: Integer
      read FButtonWidth write SetButtonWidth;

    property Color;

    property DropDownCount: Integer
      read FDropDownCount write FDropDownCount;

    property DropListVisible: Boolean
      read FDropListVisible write SetDropListVisible;

    property EditStyle: TGridEditStyle
      read FEditStyle write SetEditStyle;

    property Font;

    property Grid: TCustomGridView
      read FGrid;

    property LineCount: Integer
      read GetLineCount;

    property MaxLength;

    property PickList: TGridListBox
      read FPickList;

    property Visible: Boolean
      read GetVisible;

    property WantReturns: Boolean
      read FWantReturns write FWantReturns;

    property WordWrap: Boolean
      read FWordWrap write SetWordWrap;

  end;

  TGridEdit = class(TCustomGridEdit);

{ TGridTipsWindow }

  TGridTipsWindowClass = class of TGridTipsWindow;

  TGridTipsWindow = class(THintWindow)
  private
    FGrid : TCustomGridView;

    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;

  protected
    procedure Paint; override;

  public
    procedure ActivateHint(Rect: TRect; const AHint: string); override;
    procedure ActivateHintData(Rect: TRect; const AHint: string;
      AData: Pointer); override;
    function CalcHintRect(MaxWidth: Integer; const AHint: string;
      AData: Pointer): TRect; override;

  end;

{ TCustomGridView }

{$REGION 'Documentation'}
{
  Procedures:
    AcquireFocus -         the installation of focus to talitsu or line of introduction.
                           Returns False, if on the the kaim or to reasons the focus
                           it is not established.
    CellClick -            the pressure of mashkoy on the cell.
    Change -               the cell of vydlena.
    Changing -             cell is separated.
    CheckClick -           the pressure of mashkoy on the flag cell.
    ColumnAutoSize -       Of ishirina column changes to the width, calculated
                           it is automatic on the width of text.
    ColumnResizing -       changes the width of column.
    ColumnResize -         is changed the width of column.
    CreateColumn -         to create column. This function is used
                           for creating the columns of another class, different from
                           TGridColumn.
    CreateColumns -        to create the list of the columns of table. Function it follows
                           to use for creating the columns of another class,
                           different from TGridColumns.
    CreateEdit -           to create the line of editing of the type indicated or
                           the standard class TGridEdit, if class not
                           it is indicated. For the indication of the type of the line of the introduction of table,
                           different from the standard, should be overlapped
                           function GetEditClass.
                           NOTE: By function of compiling the line of the introduction
                           one should use only for only for
                           the fulfillment of the additional actions, necessary
                           for the correct compiling of line (T.e.
                           immediately after the call of designer). If
                           it is necessary to only change the class of line, it follows
                           to overlap function GetEditClass.
                           ATTENTION: Can still at the moment of creation EditCell
                           ne indicates the cell of editing. For
                           the determination of the future cell of editing it follows
                           to use CellFocused.
    CreateFixed -          to create the list of the fixed columns. Function
                           one should use for creating fixed
                           another class, different from TGridFixed.
    CreateHeader -         to create title. A function should be used
                           for creating the title of another class, outstanding
                           from TGridHeader.
    CreateHeaderSection -   to create the section of title. This function it follows
                           to use for creating the sections of the title of other
                           klassa, different from TGridHeaderSection.
    CreateRows -           to create the list of lines. This function it follows
                           to use for compiling of the lines of another class,
                           different from TGridRows.
    CreateScrollBar -      to create the strip of warming up. Function it follows
                           to use for creating the strip of the warming up of other
                           the class, different from TGridScrollBar.
    EditButtonPress -      occurred pressure on the button of the line of editing.
    EditCanAcceptKey -     is it possible to introduce the symbol into the cell indicated.
    EditCanceled -         the line of editing is extinguished as a result
                           the pressure of key Escape.
    EditCanModify -        is it possible to edit text in the cell. It is caused
                           directly before the change produced in
                           to the line of introduction (pressure of key and t.p.).
    EditCanShow -          can be shown the line of introduction.
    EditCanUndo -          can the line of introduction perform operation Undo.
    EditChange -           changed text in the line of introduction.
    EditCloseUp -          closing the falling out list with the selection of value.
    EditSelectNext -       Nazhat Ctrl+.Enter on the button with the unexposed list.
                           To parameter Value should be regained the new importance
                           for the line of introduction.
    GetCellColors -        the installation of the colors of cell depending on focus,
                           vydeleniya and t.p.
    GetCellImage -         to return the number of the picture of cell. If there are no pictures,
                           compulsorily should be in returned -1.
    GetCellImageRect -     the rectangle of the picture of cell relative to the leftist
                           the verkhnego angle of table.
    GetCellHintRect -      to return the rectangle of the boundary of the prompt of cell.
                           If text not pomeshchyaetsya into these boundaries, then
                           shows the prompt of yacheykt (CellTips).
    GetCellText -          to return the text of cell.
    GetCellTextBounds -    to calculate the rectangle of the text of cell. For
                           calculations are used GetTextRect and GetCellText.
    GetCellTextIndent -    to return the shift of the text of cell along the horizontal
                           relative to the leavogo and upper edge of the rectangle
                           editing. Displacement to the left on silence is equal
                           2 pixels, if there is a flag or picture, and
                           .TekhtLeftIndent of pixels, if in the cell only
                           text. Displacement on top to silence is equal
                           TextTopIndent plus two pixels, if fixed
                           cells take the form of button (not Flat).

    GetCheckAlignment -    to return the alignment of the flag of cell. They are allowed
                           the following values:
                           taLeftJustify - flag to the left.
                           .taChenter of -      flag on to center cells.
    GetCheckImage -        To venrut' the picture of cell. Picture must be
                           with sizes CheckWidth of X CheckHeight pixels
                           (T.e. y'khy'), is desirable with the transparent background.
    GetCheckKind -         to return the type of the flag of cell. Rezal'tat can
                           to take the following values:
                             gcNone -        cell not sozherzhit flag.
                             gcCheckBox -    cell contains flag as u
                                             component TCheckBox.
                             gcRadioButton - cell contains flag as
                                             in component TRadioButton.
     to                      gcUserDefine -  cell contains flag, form
                                             which it is determined
                                             by user. For the determination
                                             form is used the event
                                             OnGetCheckImage.
    GetCheckRect -         to obtain the rectangle of the flag of cell in the coordinates
                           relative to table.
    GetCheckState -        to return the state of vlazhka of cell.
    GetColumnClass -       to return the class of the columns of table. It is used for
                           redefining the class of columns in the table- heirs
                           (for example TDBGridView) for the correct work
                           redaktorov Delphi IDE.
    GetColumnLeftRight     Returns the horizontal bounderies of a given column
    GetCursorCell -        to find the cell, to which it is possible to establish cursor.
                           The cell and the displacement is transferred as the parameters
                           from this cell. If obtained after the displacement
                           cell cannot assume focus, in that indicated
                           napravlenii  searches for the following accessible cell.
                           The parameter of displacement can assume the following
                           the value:
                             goLeft -     to be displaced from that indicated to one
                                          column to the left.
                             goRight -    to be displaced from that indicated to one
                                          column to the right.
                             goUp -       to be displaced from that indicated to one
                                          column upward.
                             goDown -     to be displaced from that indicated to one
                                          column downward.
                             goPageUp -   to be displaced from that indicated on
                                          page upward.
                             goPageDown - to be displaced from that indicated on
                                          page downward.
                             goHome -     to find the first accessible cell on
                                          to the line indicated.
                             goEnd -      to Nayti the last accessible cell
                                          on the line indicated.
                             goGridHome - to be displaced into the upper left angle
                                          table.
                             goGridEnd -  to be displaced into the lower right angle
                                          table.
                             goSelect -   to isolate the cell indicated. If
                                          cell is not accessible, to find
                                          podkhodyashchuyu on the same line or in
                                          to the same column.
                             goFirst -    to find the first-encountered accessible
                                          cell.
                             goNext -     to find the following cell. Search goes
                                          to the right from the cell to the edge indicated,
                                          it then passes to the line downward and
                                          t.d.
                             goPrev -     to find the previous cell. Search
                                          it goes from the cell indicated to the left to
                                          edge, then it passes to the line
                                          vverkh and t.d.
    GetEditClass -         to return the class of the line of editing for that indicated
                           cell. If the class of line does not coincide with the fact that
                           it was earlier, then the new line of that indicated is created
                           class (by analogy with the fact, as is done in
                           TApplication.ActivateHint). T.o. to each cell
                           it is possible to make its line of editing.
    GetEditList -          to Zapolnit' the falling out list of the line of editing.
    GetEditListBounds -    to touch up the clause of the falling out list of the line
                           editing.
    GetEditMask -          to return the mask of introduction for the editing. On
                           to umolchaniyu there is no mask.
    GetEditStyle -         to return style the periods of editing.
    GetEditText -          to return the text of cell for the editing. On
                           the text of cell returns to silence.
    GetGridLineColor -     to obtain the color of grid line.
    GetHeaderColors -      the installation of the colors of the section of title.
    GetHeaderImage -       to return the number of the picture of the section of title. If
                           there are no pictures, compulsorily should be in returned -1.
    GetRowTopBottom        Returns the vertical bounderies for a given row.
    GetSortDirection -     to determine the direction of sorting for that indicated
                           column. Result can assume the following
                           the value:
                             gsNone -         there is no sorting.
                             gsAscending  -   sorting on the growth.
                             gsDescending -   sorting on the decrease.
    GetSortImage -         to return the picture of sorting for the column indicated.
    GetTextRect -          the universal procedure of the calculation of the rectangle
                           text for the cell. It is not attached to the fabric of the table
                           and to cell. Always it must be identical
                           to the universal procedure of painting of text.
    GetTipsRect -          to calculate rectangle for the demonstration of prompt s
                           by the text of cell. He uses for the calculation
                           procedure GetTextRect.
    GetTipsText -          to return the text of prompt for the cell indicated. On
                           to umolchaniyu returns the text of cell.
    GetTipsWindowClass -   to return the class of the window of the prompt of cell.
    HeaderClick -          is pressed the section of title.
    HeaderClicking -       begins pressure in the section of title.
    HideCursor -           to extinguish the cursor.
    HideEdit -             to hide the line of editing.
    HideFocus -            to extinguish the rectangle of focus.
    PaintCell -            to draw cell.
    PaintCells -           to draw cells.
    PaintFixed -           to Risovat' the fixed cells.
    PaintFixedGrid -       to draw the network of the fixed cells.
    PaintFocus -           to draw the rectangle of focus.
    PaintFreeField -       to draw the region, free from the cells.
    PaintGridLines -       to draw grid lines.
    PaintHeader -          to draw the section of title.
    PaintHeaders -         to draw title.
    PaintHeaderSections -  to draw the sections of title.
    PaintResizeLine -      to draw line with a change in the size of column.
    PaintText -            the universal procedure of painting of the text of cell.
                           It is not attached to the fabric of table and the cell.
                           It is used also for painting CellTips. In
                           the case of overlapping the procedure it is necessary to consider
                           change with the calculation of the rectangle of the prompt
                           (T.e. inside function GetTextRect).
    SetEditText -          To ustanovt' text into the cell from the line of introduction.
                           it Vyzyvayetsya with the change of cell, if only the cell
                           not ReadOnly. If text does not start, it follows
                           to cause exception.
    SetCursor -            To ustanovt' cursor into the cell indicated.
    ShowCursor -           to show cursor.
    ShowEdit -             to show the line of editing.
    ShowEditChar -         to show the line of editing and to transmit to it
                           symbol.
    ShowFocus -            to show the rectangle of focus.
    StartColResize -       to begin a change in the width of column.
    StartHeaderClick -     to begin pressure on the title.
    StepColResize -        to continue a change in the width of column.
    StepHeaderClick -      to continue pressure on the title.
    StopColResize -        to complete a change in the width of column.
    StopHeaderClick -      to complete pressure on the title.
    ApplyEdit -            the completion of editing with the application of the text
                           cell. On silence it causes SetEditText.
    CancelEdit -           the cancellation of the editing of cell. In zvisimosti from
                           value AlwaysEdit establish the text of the line
                           introduction to the text of cell or it dissipates the line of introduction without
                           conclusion OnSetEditText.
    DrawDragRect -         to draw the framework of focus on the cell. It is used
                           for opmetki of cell with operation DragDrop.
    FindText -             Searches for a given text. Returns True if a matching
                           cell is found.
    GetCellAt -            to find cell from the point. It returns (-1, -1), if
                           cell is not found.
    GetCellRect -          the rectangle of cell relative to the left upper
                           the angle of table.
    GetCellsRect -         to obtain the rectangle of cells.
    GetColumnAt -          to find column from the point. It returns -1, if
                           column is not found.
    GetColumnMaxWidth -    Returns the maximal cell content width for a given
                           column in all visible rows. Used to calculate the
                           optimal column size for a given column.
    GetColumnRect -        the rectangle of column.
    GetColumnsRect -       the rectangle of columns indicated on that indicated.
    GetColumnsWidth -      the summary width of columns with that indicated on that indicated.
    GetEditRect -          to return rectangle for the line of editing.
                           On silence it is equal to the rectangle of cell after
                           vychetom of flag and picture to the left.
    GetFirstImageColumn -  to obtain the number of picture for first that seen
                           the unfixed column. On silence such
                           column has a picture, rest do not have.
    GetFixedRect -         the rectangle of the fixed columns.
    GetFixedWidth -        the summary width of the fixed columns.
    GetFocusRect -         the rectangle of focus. From silence it is calculated s
                           by the calculation of the line-by-line isolation and presence of pictures.
    GetGridHeight -        the height of the visible part of the cells.
    GetGridOrigin -        the shift of cells relative to the left upper angle
                           the visible rectangle of table (T.e. excluding
                           title) in the pixels. It is determined by the position
                           the cursors of scrollers.
    GetGridRect -          the rectangle of the visible part of the cells (in the the client
                           the coordinates of table).
    GetHeaderHeight -      the height of title. It is equal to 0, if we do not see.
    GetHeaderRect -        the rectangle of title.
    GetHeaderSection -     to find the section of the title of the level indicated for
                           the column indicated. If it is indicated as the level
                           -1, then lowest section returns.
    GetResizeSectionAt -    to return the section of title, above the right boundary
                           by which is located the point indicated.
    GetRowAt -             to find line from the point.
    GetRowRect -           the rectangle of line.
    GetRowsRect -          the rectangle of lines indicated on that indicated.
    GetRowsHeight -        to return the summary height of lines from that indicated to
                           indicated.
    GetSectionAt -         to obtain the section of title, which contains
                           the point indicated.
    InvalidateCell -       to draw again cell.
    InvalidateCheck -      to draw again the flag of cell.
    InvalidateColumn -     to draw again columns.
    InvalidateColumns -    to draw again columns with that indicated on that indicated.
    InvalidateEdit -       to draw again the line of introduction.
    InvalidateFixed -      to draw again the fixed columns.
    InvalidateFocus -      to draw again focus (cell or line).
    InvalidateGrid -       to draw again table (all acheyki).
    InvalidateHeader -     to draw again title.
    InvalidateRect -       to renew the rectangle of table.
    InvalidateRow -        to draw again line.
    InvalidateRows -       to draw again lines with that indicated on that indicated.
    IsActiveControl -      is table the current active constituent
                           form.
    IsCellAcceptCursor -   is it possible to establish cursor into the cell.
    IsCellHasCheck -       checking to the presence of flag in cell.
    IsCellHasImage -       checking to the presence of picture in cell.
    IsCellFocused -        checking for the entry of cell into the focus, and with
                           line-by-line isolation (RowSelect = True) - in
                           display line.
    IsCellReadOnly -       it is possible or not to change text in the line of the introduction
                           the cell indicated.
    IsCellValid -          checking the correctness of cell (nontrivial width,
                           visibility, output beyond the boundaries of columns and lines).
    IsCellVisible -        checking the visibility of yacheki on the screen.
    IsColumnVisible -      checking the visibility of column not screen.
    IsFocusAllowed -       the sign of the visibility of focus. Focus is visible, if
                           the isolation of line is permitted or it is forbidden
                           redaktirvoaniye. Otherwise on the spot
                           focus is located the line of introduction.
    IsHeaderHasImage -     Proverka to the presence of picture in the section of title.
    IsHeaderPressed -      checking to the pressure in the section of title. It returns
                           True, if pressure with that indicated now proceeds
                           the section of title.
    IsRowVisible -         checking the visibility of line.
    LockUpdate -           to block copying.
    MakeCellVisible -      To sdulat' of that seen the cell indicated.
    UndoEdit -             Vypoleniye of operation Undo for the built-in line
                           introduction. It is caused with the pressure of key ESC in
                           the case, if the line of introduction is not removed (T.e.
                           with AlwaysEdit = True).
    UnLockUpdate -         to unblock copying.
    UpdateCursor -         the renovation of the position of cursor. If the cursor
                           it is located on the cell beyond the limits of table or there,
                           where the focus is not permitted - first fallen searches for
                           cell.
    UpdateColors -         the renovation of the colors of title and fixed with
                           a change in the color of table.
    UpdateEdit -           the renovation of position, text of the line of introduction and it
                           visibility in sootvestvii with the position of cursor.
    UpdateEditContents -   the renovation of exterior view and text of the visible line
                           introduction. This method should be used in the case,
                           when in the course of editing the renovation occurs
                           dannykh of the table, parameters of columns and t.p. and
                           is required sootvestvenno to renew the line of introduction.
    UpdateEditText -       the renovation of data of table from the line of introduction.
    UpdateFixed -          the renovation of the parameters of those fixed (for example,
                           quantity depending on a quantity of the columns
                           table).
    UpdateFocus -          to establish focus to talitsu, if this is possible.
    UpdateFonts -          the renovation of types of title and fixed with
                           a change in type of table.
    UpdateHeader -         the renovation of title (bringing in sootvestvii s
                           by columns, if stands flag AutoSynchronize or
                           Synchronized).
    UpdateRows -           the renovation of the parameters of lines (for example, the height
                           line depending on the size of pictures and
                           the size of type of table).
    UpdateScrollBars -     the renovation of tuning the strips of warming up (maximum,
                           steep pitch, low pitch).
                           NOTE: Vertical scroller is tuned
                           to the number of lines, horizontal - to the width
                           the nefiksirovannykh columns.
    UpdateScrollPos -      the installation of the position of the cursors of the strips of warming up in
                           correspondence with the current chosen cell.
    UpdateSelection -      Of proveka of the possibility of the selection of the cell indicated.
                           If cell cannot be isolated, then it will search for
                           the nearest accessible for the selection.
    UpdateText -           the same as UpdateEditText.
    UpdateVisOriginSize -  the renovation of tuning the visible range of cells on
                           to the position of the cursors of the strips of warming up.
    Properties:
    AllowEdit -            can be seen the line of editing.
                           NOTE: Although the line modzhet to be seen, it is possible
                           tyuere is no ili to edit the text of cell it is determined
                           with the value of properties ReadOnly of column and entire table.
    AllowSelect -          is permitted the isolation of cells (lines) by color (T.e.
                           the presence of focus on the current cell).
    AlwaysSelected -       to always show focus to those isolated.
    BorderStyle -
    CellFocused -          the current position of cursor.
    Cells -                access to the contents of cell.
    CellSelected -         the sign of the isolation of cursor.
    CheckBoxes -           mapping the flags of cells.
    CheckHeight -          the height of the pictures of flags. It is always equal to 16 pixels,
                           it is not subject to change.
    CheckLeftIndent -      the shift of the flag of cell from the left edge. It influences
                           to otrisovku of the flag of cell. It does not follow without the the special
                           the need for changing this value (and also
                           value CheckTopIndent).
    CheckStyle -           the style of the flags of cells. It supports one
                             of these values:
                               csFlat -   flat flags.
                               cs3D -     convex flags.
                               csWin95 -  flags in style Windows 95.
    CheckTopIndent -       the displacement of flag from the upper edge of cell.
    CheckWidth -           the width of the pictures of flags. It is always equal to 16 pixels,
                           it is not subject to change.
    Col -                  the current column of the cell of focus. Is equal CellFocused.Col.
    ColumnClick -          it is possible or not to press the title of column.
    Columns -              the list of the columns of table.
    ColumnsFullDrag -      Updates the columnwidh of a column immediately as
                             you resize it. Otherwise a vertical line is shown
                             while dragging the column.
    CursorKeys -           of the key for movement of cursor over the cells. It can be
                           kombinatsey from the following values:
                             gkArrows -     navigation on the cells by pointers.
                             gkTabs -       navigation on the cells with pomosh'yu.
                                            key TAB and Shift-tab
                             gkReturn -     passage to the following cell afterward
                                            vvoda of text into the line of the introduction
                                            by the pressure of key Return.
                             gkMouse -      the isolation of cell by mouse.
                             gkMouseMove -  the synchronous isolation of cell with
                                            the displacement of mouse with that pressed
                                            by button.
                             gkMouseWheel - navigation on the cells with the aid of
                                            the little wheel of mouse.
    DefaultEditMenu -      to show standard Popup menu (on the the right
                           to the button of mouse) for the line of introduction. If the property
                           it is advanced in False, then it will be shown for the line
                           the menu of table.
    Edit -                 the line of editing.
    EditCell -             the cell of editing.
    EditColumn -           the column, in which is located the cell of editing.
    EditDropDown -         control of the visibility of the falling out list. With
                           the establishment of value into True is transferred cell in
                           the regime of editing shows list (if
                           this is possible).
    Editing -              sign is the editing of cell. It makes it possible to begin
                           or to complete the process of the editing of cell.
                           NOTE: With Editing:= True is placed focus on
                           table row.
    EndEllipsis -          to derive or not nepomeshchayushchiysya in the cell text s
                           ... (by dots) at the end. It works only for
                           single line text s we equalize to the left. It is strong
                           it influences the speed of painting.
    Fixed -                of Parametry of the fixed columns.
    FlatBorder -           to reflect the "flat" border (as Bevel).
    FocusOnScroll -        to take away to itself focus or not with skrollirovanii
                           table. If FocusOnScroll = False, then
                           skrollirovaniye by mouse for the slider will not bring k
                           to the focusing of table. If we make FocusOnScroll
                           = True, then with the installation of active cell
                           skrollirovniyem from without (for example, during control
                           by another component), will occur the focusing
                           table.
    GridColor -            the color of grid lines.
    GridLines -            to draw or there is no grid line.
    GridStyle -            the style of grid. It can be kombinatsey from the following
                           the values:
                             gsHorzLine -     to draw horizontal lines.
                             gsVertLine -     to draw vertical lines.
                             gsFullHorzLine - horizontal lines to the the right
                                              kraya of table, but not to the latter
                                              column.
                             gsFullVertLine - vertical lines to the the lower
                                              the edge of table, but not to the latter
                                              line (if Rows.Copunt &.gt; 0).
                             gsListViewLike - to draw the horizontal lines
                                              to entire table independent of
                                              togo, how many lines in the table.
                             gsDotLines -     to draw grid lines by the smallen point
                                              (beautifully, but for long).
    Header -               title.
    HideSelection -        to  Gasit' or not cursor with the disappearance of focus.
    ImageLeftIndent -      the shift of the picture of cell from the left edge.
                           It is used for tuning of painting of the picture
                           cell. It does not follow without the special need
                           to change this value (and also ImageTopIndent).
    Images -               the list of pictures.
    ImageIndexDef -        the number of picture on silence. This picture will be
                           it is assigned to the first visible column.
    ImageHighlight -       to illuminate or not kartiku for that isolated
                           cell.
    ImageTopIndent -       the shift of the picture of cell from the upper edge.
    LeftCol -              left seen (m.b. partially seen) the column of table.
                           Is equal VisOrigin.Col.
    RightClickSelect -     is allowed or not isolation of the cell of the right
                           by the key for mouse.
    Row -                  the Tekushchaya line of the cell of focus. Is equal CellFocused.Row.
    Rows -                 the parameters of the lines of cell.
    RowSelect -            isolation in the entire line or on the cell.
    ShowCellTips -         to show or not Hint with the text of cell, if
                           it tekst it is not placed by pillar into the cell (for Delphi
                           versions 3 are above).
                           ATTENTION: Hint cell it is based on the the standard
                           Hint- e, for mapping of prompts is necessary
                           to establish ShowHint in True. If ShowCellTips
                           it is established, then usual Hint - to be reflected not
                           there will be (T.e. either Hint or CellTips).
    ShowFocusRect -        to show the framework of focus.
    ShowHeader -           To pokazyvayet' title.
    SortLeftIndent -       the displacement of the picture of the direction of sorting from
                           the right edge of text. It is used for the tuning
                           the conclusion of the picture of the direction of sorting. It does not follow
                           without the special need for changing this value (A
                           also value SortTopIndent).
    SortTopIndent -        the displacement of the picture of the direction of the sorting
                           relative to upper it is hazel text on the vertical line.
    TextLeftIndent -       the shift of the text of cell from the left edge, in the case,
                           when the cell does not have picture. It is used for
                           nastroyki of the conclusion of the text of cell. It does not follow without
                           the special need for changing this value (and also
                           TextRightIndent and TextTopIndent).
    TextRightIndent -      Tyuey otsut the text of cell from the right edge.
    TextTopIndent -        the shift of the text of cell on top.
    TipsCell -             the cell, for which pokazyvayetssya prompt.
    TipsText -             of the lines of prompt for cell TipsCell.
    TopRow -               the upper visible table row. Is equal VesOrigin.Row.
    VertScrollBar -        vertical scroller.
    VisibleColCount -      a quantity of visible columns. Is equal VisSize.Col.
    VisibleRowCount -      a quantity of visible lines. Is equal VisSize.Row.
    VisOrigin -            the first visible cell.
    VisSize -              a quantity of visible cells.

  Events:
    OnCellAcceptCursor -   is it possible to establish cursor on the cell indicated.
    OnCellClick -          the flick by mouse on the cell.
    OnCellTips -           it is necessary whether to show prompt on the cell indicated.
    OnChange -             Changes the chosen cell. It is caused immediately
                           afterward a change in yacheyeyki of cursor.
    OnChangeColumns -      occurred a change in the columns of table.
    OnChangeEditing -      occurred a change in the visibility of the line of introduction.
    OnChangeEditMode -     occurred a change in the regime of editing.
    OnChangeFixed -        occurred a change in the fixed columns of table.
    OnChangeRows -         Gets called if the RowCount or row height changes.
    OnChanging -           Occurs just before the position of the focused cell
                           in the grid changes.
    OnCheckClick -         the flick by mouse on the flag of cell.
    OnColumnAutoSize -     the width of column changes by the width, selected
                           it is automatic.
    OnColumnResize -       the width of column changed.
    OnColumnResizing -     changes the width of column.
    OnDraw -               Of drawing of table.
    OnDrawCell -           Of drawing of cell. If it is necessary to change
                           the color of cell, but to leave standard otrisovku,
                           should be redefining colors carried out in
                           event OnGetCellColors.
    OnDrawHeader -         Of drawing of the section of cell. If it is necessary to change
                           the color of section, but to leave standard otrisovku,
                           should be redefining colors carried out in
                           event OnGetHeaderColors.
    OnEditAcceptKey -      checking priyemlimosti of the symbol of cell.
    OnEditButtonPress -    is pressed button with the dots in the line of vvola.
    OnEditCanShow -
    OnEditCanceled -       of text entry it is abolished, the line of the editing
                           it is extinguished as a result of the pressure of key Escape.
    OnEditCanModify -      can change text in the line of introduction.
    OnEditChange -         changed text in the line of introduction.
    OnEditCloseUp -        occurred closing the falling out list with the selection
                           znacheniya.
    OnEditSelectNext -     will gain Ctrl+.Enter on the button with the unexposed list
                           (according to the idea should be into the cell put the following
                           value from the list).
    OnGetCellColors -      To opredeleit' the color of cell. Event influences
                           to otrisovku of cell, form CellTips and so forth, etc.
    OnGetCellImage -       to return the number of the picture of cell.
    OnGetCellImageIndent - to obtain the shift of the picture of cell.
    OnGetCellReadOnly -    to obtain flag "only for reading" for indicated
                           cell.
    OnGetCellText -        to obtain the text of cell.
    OnGetCellTextIndent -  to obtain the shift of the text of cell.
    OnGetCheckAlignment -  to obtain the levelling off of the flag of cell.
    OnGetCheckImage -      to obtain the picture of the flag of cell.
    OnGetCheckIndent -     to obtain the displacement of the picture of flag.
    OnGetCheckKind -       to obtain the type of the flag of cell.
    OnGetCheckState -      to obtain the state of the flag of cell.
    OnGetEditList -        to fill the falling out list of the line of editing.
    OnGetEditListBounds -  to touch up position and dimensions of the falling out list
                           the line of editing.
    OnGetEditMask -        to return the mask of cell for the editing.
    OnGetEditStyle -       to determine the style of the line of editing.
    OnGetEditText -        to return the text of cell for the editing. On
                           to silence is taken the text of cell.
    OnGetHeaderColors -    To opredeleit' the color of the section of title.
    OnGetHeaderImage -     To opredelet' the number of the picture of the section of title.
    OnGetSortDirection -   to determine the direction of sorting for that indicated
                           column. It is caused directly with otrisovke
                           sektsii of title for determining the mark of sorting.
    OnGetSortImage -       to return the picture of sorting for the column indicated.
    OnHeaderClick -        Called after clicking a header section. This event is
                           only fired if AllowClicking in the OnheaderClicking
                           event was True.
    OnHeaderClicking -     Called before clicking a header section.
    OnResize -             a change in the size of table.
    OnSetEditText -        to establish the text of cell. It is caused with
                           movement of cursor. If text does not start,
                           should be caused exception.
  }
{$ENDREGION}

  TGridStyle = (
    gsHorzLine,
    gsVertLine,
    gsFullHorzLine,
    gsFullVertLine,
    gsListViewLike,
    gsDotLines,
    gsFullRowPaint
  );
  TGridStyles = set of TGridStyle;

  TGridCursorKey = (
    gkArrows,
    gkTabs,
    gkReturn,
    gkMouse,
    gkMouseMove,
    gkMouseWheel
  );
  TGridCursorKeys = set of TGridCursorKey;

  TGridCursorOffset = (
    goLeft,
    goRight,
    goUp,
    goDown,
    goPageUp,
    goPageDown,
    goMouseWheelUp,
    goMouseWheelDown,
    goHome,
    goEnd,
    goGridHome,
    goGridEnd,
    goSelect,
    goFirst,
    goNext,
    goPrev
  );

  TGridSortDirection = (
    gsNone,
    gsAscending,
    gsDescending
  );

  TGridAppendRowEvent = procedure(
    Sender     : TObject;
    var Append : Boolean
  ) of object;
  TGridTextEvent = procedure(
    Sender    : TObject;
    Cell      : TGridCell;
    var Value : string
  ) of object;
  TGridRectEvent = procedure(
    Sender   : TObject;
    Cell     : TGridCell;
    var Rect : TRect
  ) of object;
  TGridCellColorsEvent = procedure(
    Sender : TObject;
    Cell   : TGridCell;
    Canvas : TCanvas
  ) of object;
  TGridCellImageEvent = procedure(
    Sender         : TObject;
    Cell           : TGridCell;
    var ImageIndex : Integer
  ) of object;
  TGridCellClickEvent = procedure(
    Sender : TObject;
    Cell   : TGridCell;
    Shift  : TShiftState;
    X, Y   : Integer
  ) of object;
  TGridCellAcceptCursorEvent = procedure(
    Sender     : TObject;
    Cell       : TGridCell;
    var Accept : Boolean
  ) of object;
  TGridCellNotifyEvent = procedure(
    Sender : TObject;
    Cell   : TGridCell
  ) of object;
  TGridCellIndentEvent = procedure(
    Sender     : TObject;
    Cell       : TGridCell;
    var Indent : TPoint
  ) of object;
  TGridCellTipsEvent = procedure(
    Sender        : TObject;
    Cell          : TGridCell;
    var AllowTips : Boolean
  ) of object;
  TGridCellReadOnlyEvent = procedure(
    Sender           : TObject;
    Cell             : TGridCell;
    var CellReadOnly : Boolean
  ) of object;
  TGridHeaderColorsEvent = procedure(
    Sender  : TObject;
    Section : TGridHeaderSection;
    Canvas  : TCanvas
  ) of object;
  TGridHeaderImageEvent = procedure(
    Sender         : TObject;
    Section        : TGridHeaderSection;
    var ImageIndex : Integer) of object;
  TGridDrawEvent = procedure(
    Sender             : TObject;
    var DefaultDrawing : Boolean
  ) of object;
  TGridDrawCellEvent = procedure(
    Sender             : TObject;
    Cell               : TGridCell;
    var Rect           : TRect;
    var DefaultDrawing : Boolean
  ) of object;
  TGridDrawHeaderEvent = procedure(
    Sender             : TObject;
    Section            : TGridHeaderSection;
    Rect               : TRect;
    var DefaultDrawing : Boolean
  ) of object;
  TGridColumnResizeEvent = procedure(
    Sender    : TObject;
    Column    : Integer;
    var Width : Integer
  ) of object;
  TGridHeaderClickEvent = procedure(
    Sender  : TObject;
    Section : TGridHeaderSection
  ) of object;
  TGridHeaderClickingEvent = procedure(
    Sender         : TObject;
    Section        : TGridHeaderSection;
    var AllowClick : Boolean
  ) of object;
  TGridChangingEvent = procedure(
    Sender       : TObject;
    var Cell     : TGridCell;
    var Selected : Boolean
  ) of object;
  TGridChangedEvent = procedure(
    Sender   : TObject;
    Cell     : TGridCell;
    Selected : Boolean
  ) of object;
  TGridEditStyleEvent = procedure(
    Sender    : TObject;
    Cell      : TGridCell;
    var Style : TGridEditStyle
  ) of object;
  TGridEditListEvent = procedure(
    Sender : TObject;
    Cell   : TGridCell;
    Items  : TStrings
  ) of object;
  TGridEditCloseUpEvent = procedure(
    Sender     : TObject;
    Cell       : TGridCell;
    ItemIndex  : Integer;
    var Accept : Boolean
  ) of object;
  TGridEditCanModifyEvent = procedure(
    Sender        : TObject;
    Cell          : TGridCell;
    var CanModify : Boolean
  ) of object;
  TGridEditCanShowEvent = procedure(
    Sender      : TObject;
    Cell        : TGridCell;
    var CanShow : Boolean
  ) of object;
  TGridAcceptKeyEvent = procedure(
    Sender     : TObject;
    Cell       : TGridCell;
    Key        : Char;
    var Accept : Boolean
  ) of object;
  TGridCheckKindEvent = procedure(
    Sender        : TObject;
    Cell          : TGridCell;
    var CheckKind : TGridCheckKind
  ) of object;
  TGridCheckStateEvent = procedure(
    Sender         : TObject;
    Cell           : TGridCell;
    var CheckState : TCheckBoxState
  ) of object;
  TGridCheckImageEvent = procedure(
    Sender     : TObject;
    Cell       : TGridCell;
    CheckImage : TBitmap
  ) of object;
  TGridCheckAlignmentEvent = procedure(
    Sender             : TObject;
    Cell               : TGridCell;
    var CheckAlignment : TAlignment
  ) of object;
  TGridSortDirectionEvent = procedure(
    Sender            : TObject;
    Section           : TGridHeaderSection;
    var SortDirection : TGridSortDirection
  ) of object;
  TGridSortImageEvent = procedure(
    Sender    : TObject;
    Section   : TGridHeaderSection;
    SortImage : TBitmap
  ) of object;

  TCustomGridView = class(TCustomControl)
  private
    FAllowEdit            : Boolean;
    FAllowSelect          : Boolean;
    FAlwaysEdit           : Boolean;
    FAlwaysSelected       : Boolean;
    FBorderStyle          : TBorderStyle;
    FCancelOnExit         : Boolean;
    FCellFocused          : TGridCell;
    FCellSelected         : Boolean;
    FCheckBitmapCB        : TBitmap;
    FCheckBitmapRB        : TBitmap;
    FCheckBoxes           : Boolean;
    FCheckBuffer          : TBitmap;
    FCheckHeight          : Integer;
    FCheckLeftIndent      : Integer;
    FCheckStyle           : TGridCheckStyle;
    FCheckTopIndent       : Integer;
    FCheckWidth           : Integer;
    FClickPos             : TGridCell;
    FColResizeCount       : Integer;
    FColResizeIndex       : Integer;
    FColResizeMaxWidth    : Integer;
    FColResizeMinWidth    : Integer;
    FColResizeOffset      : Integer;
    FColResizePos         : Integer;
    FColResizeRect        : TRect;
    FColResizeSection     : TGridHeaderSection;
    FColResizing          : Boolean;
    FColumnClick          : Boolean;
    FColumns              : TGridColumns;
    FColumnsFullDrag      : Boolean;
    FColumnsSizing        : Boolean;
    FCursorKeys           : TGridCursorKeys;
    FDefaultEditMenu      : Boolean;
    FDoubleBuffered       : Boolean;
    FEdit                 : TCustomGridEdit;
    FEditCell             : TGridCell;
    FEditing              : Boolean;
    FEndEllipsis          : Boolean;
    FFixed                : TCustomGridFixed;
    FFlatBorder           : Boolean;
    FFocusOnScroll        : Boolean;
    FGridColor            : TColor;
    FGridLines            : Boolean;
    FGridLineWidth        : Integer;
    FGridStyle            : TGridStyles;
    FHeader               : TCustomGridHeader;
    FHeaderClicking       : Boolean;
    FHeaderClickRect      : TRect;
    FHeaderClickSection   : TGridHeaderSection;
    FHeaderClickState     : Boolean;
    FHideSelection        : Boolean;
    FHitTest              : TPoint;
    FHorzScrollBar        : TGridScrollBar;
    FHotHeaderSection     : TGridHeaderSection;
    FImageHighlight       : Boolean;
    FImageIndexDef        : Integer;
    FImageLeftIndent      : Integer;
    FImages               : TImageList;
    FImagesLink           : TChangeLink;
    FImageTopIndent       : Integer;
    FOnAppendRow          : TGridAppendRowEvent;
    FOnCellAcceptCursor   : TGridCellAcceptCursorEvent;
    FOnCellClick          : TGridCellClickEvent;
    FOnChange             : TGridChangedEvent;
    FOnChangeColumns      : TNotifyEvent;
    FOnChangeEditing      : TNotifyEvent;
    FOnChangeEditMode     : TNotifyEvent;
    FOnChangeFixed        : TNotifyEvent;
    FOnChangeRows         : TNotifyEvent;
    FOnMouseEnter         : TNotifyEvent;
    FOnMouseLeave         : TNotifyEvent;
    FOnChanging           : TGridChangingEvent;
    FOnCheckClick         : TGridCellNotifyEvent;
    FOnColumnAutoSize     : TGridColumnResizeEvent;
    FOnColumnResize       : TGridColumnResizeEvent;
    FOnColumnResizing     : TGridColumnResizeEvent;
    FOnDraw               : TGridDrawEvent;
    FOnDrawCell           : TGridDrawCellEvent;
    FOnDrawHeader         : TGridDrawHeaderEvent;
    FOnEditAcceptKey      : TGridAcceptKeyEvent;
    FOnEditButtonPress    : TGridCellNotifyEvent;
    FOnEditCanceled       : TGridCellNotifyEvent;
    FOnEditCanModify      : TGridEditCanModifyEvent;
    FOnEditCanShow        : TGridEditCanShowEvent;
    FOnEditChange         : TGridCellNotifyEvent;
    FOnEditCloseUp        : TGridEditCloseUpEvent;
    FOnEditSelectNext     : TGridTextEvent;
    FOnGetCellColors      : TGridCellColorsEvent;
    FOnGetCellImage       : TGridCellImageEvent;
    FOnGetCellImageIndent : TGridCellIndentEvent;
    FOnGetCellReadOnly    : TGridCellreadOnlyEvent;
    FOnGetCellText        : TGridTextEvent;
    FOnGetCellTextIndent  : TGridCellIndentEvent;
    FOnGetCheckAlignment  : TGridCheckAlignmentEvent;
    FOnGetCheckImage      : TGridCheckImageEvent;
    FOnGetCheckIndent     : TGridCellIndentEvent;
    FOnGetCheckKind       : TGridCheckKindEvent;
    FOnGetCheckState      : TGridCheckStateEvent;
    FOnGetEditList        : TGridEditListEvent;
    FOnGetEditListBounds  : TGridRectEvent;
    FOnGetEditMask        : TGridTextEvent;
    FOnGetEditStyle       : TGridEditStyleEvent;
    FOnGetEditText        : TGridTextEvent;
    FOnGetHeaderColors    : TGridHeaderColorsEvent;
    FOnGetHeaderImage     : TGridHeaderImageEvent;
    FOnGetSortDirection   : TGridSortDirectionEvent;
    FOnGetSortImage       : TGridSortImageEvent;
    FOnHeaderClick        : TGridHeaderClickEvent;
    FOnHeaderClicking     : TGridHeaderClickingEvent;
    FOnSetEditText        : TGridTextEvent;
    FPatternBitmap        : TBitmap;
    FReadOnly             : Boolean;
    FRightClickSelect     : Boolean;
    FRows                 : TCustomGridRows;
    FRowSelect            : Boolean;
    FShowCellTips         : Boolean;
    FShowFocusRect        : Boolean;
    FShowHeader           : Boolean;
    FSortBitmapA          : TBitmap;
    FSortBitmapD          : TBitmap;
    FSortBuffer           : TBitmap;
    FSortLeftIndent       : Integer;
    FSortTopIndent        : Integer;
    FTextLeftIndent       : Integer;
    FTextRightIndent      : Integer;
    FTextTopIndent        : Integer;
    FThemingEnabled       : Boolean;
    FTipsCell             : TGridCell;
    FTipsText             : string;
    FUpdateLock           : Integer;
    FVertScrollBar        : TGridScrollBar;
    FVisOrigin            : TGridCell;
    FVisSize              : TGridCell;
    FOnGetCellHintRect    : TGridRectEvent;
    FOnCellTips           : TGridCellTipsEvent;
    FOnGetTipsRect        : TGridRectEvent;
    FOnGetTipsText        : TGridTextEvent;
    FFitColsToClient      : Boolean;
    FVclStyleEnabled      : Boolean;

    function GetCell(Col, Row: Integer): string;
    function GetChecked(Col, Row: Integer): Boolean;
    function GetCheckBoxState(Col, Row: Longint): TCheckBoxState;
    function GetCol: Integer;
    function GetFixed: TGridFixed;
    function GetEdit: TGridEdit;
    function GetEditColumn: TGridColumn;
    function GetEditDropDown: Boolean;
    function GetEditing: Boolean;
    function GetHeader: TGridHeader;
    function GetLeftCol: Integer;
    function GetRow: Integer;
    function GetRows: TGridRows;
    function GetTopRow: Integer;
    function GetVisibleColCount: Integer;
    function GetVisibleRowCount: Integer;
    procedure ColumnsChange(Sender: TObject);
    procedure FixedChange(Sender: TObject);
    procedure HeaderChange(Sender: TObject);
    procedure HorzScroll(Sender: TObject; ScrollCode: Integer; var ScrollPos: Integer);
    procedure HorzScrollChange(Sender: TObject);
    procedure ImagesChange(Sender: TObject);
    procedure RowsChange(Sender: TObject);
    procedure SetAllowEdit(Value: Boolean);
    procedure SetAllowSelect(Value: Boolean);
    procedure SetAlwaysEdit(Value: Boolean);
    procedure SetAlwaysSelected(Value: Boolean);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetCell(Col, Row: Integer; Value: string);
    procedure SetCellFocused(Value: TGridCell);
    procedure SetCellSelected(Value: Boolean);
    procedure SetCheckBoxes(Value: Boolean);
    procedure SetCheckLeftIndent(Value: Integer);
    procedure SetCheckStyle(Value: TGridCheckStyle);
    procedure SetCheckTopIndent(Value: Integer);
    procedure SetCol(Value: Integer);
    procedure SetColumns(Value: TGridColumns);
    procedure SetCursorKeys(Value: TGridCursorKeys);
    procedure SetEditDropDown(Value: Boolean);
    procedure SetEditing(Value: Boolean);
    procedure SetEndEllipsis(Value: Boolean);
    procedure SetFlatBorder(Value: Boolean);
    procedure SetFixed(Value: TGridFixed);
    procedure SetFitColsToClient(const Value: Boolean);
    procedure SetGridColor(Value: TColor);
    procedure SetGridLines(Value: Boolean);
    procedure SetGridStyle(Value: TGridStyles);
    procedure SetHeader(Value: TGridHeader);
    procedure SetHideSelection(Value: Boolean);
    procedure SetHorzScrollBar(Value: TGridScrollBar);
    procedure SetImageIndexDef(Value: Integer);
    procedure SetImageHighlight(Value: Boolean);
    procedure SetImageLeftIndent(Value: Integer);
    procedure SetImages(Value: TImageList);
    procedure SetImageTopIndent(Value: Integer);
    procedure SetLeftCol(Value: Integer);
    procedure SetReadOnly(Value: Boolean);
    procedure SetRows(Value: TGridRows);
    procedure SetRowSelect(Value: Boolean);
    procedure SetShowCellTips(Value: Boolean);
    procedure SetShowFocusRect(Value: Boolean);
    procedure SetShowHeader(Value: Boolean);
    procedure SetSortLeftIndent(Value: Integer);
    procedure SetSortTopIndent(Value: Integer);
    procedure SetTextLeftIndent(Value: Integer);
    procedure SetTextRightIndent(Value: Integer);
    procedure SetTextTopIndent(Value: Integer);
    procedure SetTopRow(Value: Integer);
    procedure SetThemingEnabled(const Value: Boolean);
    procedure SetVertScrollBar(Value: TGridScrollBar);
    procedure SetVisOrigin(Value: TGridCell);

    procedure VertScroll(Sender: TObject; ScrollCode: Integer; var ScrollPos: Integer);
    procedure VertScrollChange(Sender: TObject);

    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMLButtonDown(var Message: TMessage); message WM_LBUTTONDOWN;
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;

    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMShowHintChanged(var Message: TMessage); message CM_SHOWHINTCHANGED;
    procedure CMHintShow(var AMessage: TMessage); message CM_HINTSHOW;
    procedure CMMouseLeave(var AMessage: TMessage); message CM_MOUSELEAVE;
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;

  protected
    function AcquireFocus: Boolean;
    procedure CancelCellTips; virtual;
    procedure CellClick(Cell: TGridCell; Shift: TShiftState; X, Y: Integer); virtual;
    procedure CellTips(Cell: TGridCell; var AllowTips: Boolean); virtual;
    procedure Change(var Cell: TGridCell; var Selected: Boolean); virtual;
    procedure ChangeColumns; virtual;
    procedure ChangeEditing; virtual;
    procedure ChangeEditMode; virtual;
    procedure ChangeFixed; virtual;
    procedure ChangeRows; virtual;
    procedure ChangeScale(M, D: Integer); override;
    procedure Changing(var Cell: TGridCell; var Selected: Boolean); virtual;
    procedure CheckClick(Cell: TGridCell); virtual;
    procedure ColumnAutoSize(Column: Integer; var Width: Integer); virtual;
    procedure ColumnResize(Column: Integer; var Width: Integer); virtual;
    procedure ColumnResizing(Column: Integer; var Width: Integer); virtual;
    function CreateColumn(Columns: TGridColumns): TCustomGridColumn; virtual;
    function CreateColumns: TGridColumns; virtual;
    function CreateEdit(EditClass: TGridEditClass): TCustomGridEdit; virtual;
    function CreateFixed: TCustomGridFixed; virtual;
    function CreateHeader: TCustomGridHeader; virtual;
    function CreateHeaderSection(Sections: TGridHeaderSections): TGridHeaderSection; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    function CreateRows: TCustomGridRows; virtual;
    function CreateScrollBar(Kind: TScrollBarKind): TGridScrollBar; virtual;
    procedure CreateWnd; override;

    procedure DoAppendRow(var Append : Boolean); dynamic;
    procedure DoExit; override;
    procedure DoMouseEnter; virtual;
    procedure DoMouseLeave; virtual;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint)
      : Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint)
      : Boolean; override;

    procedure EditButtonPress(Cell: TGridCell); virtual;
    function EditCanAcceptKey(Cell: TGridCell; Key: Char): Boolean; virtual;
    procedure EditCanceled(Cell: TGridCell); virtual;
    function EditCanModify(Cell: TGridCell): Boolean; virtual;
    function EditCanShow(Cell: TGridCell): Boolean; virtual;
    function EditCanUndo(Cell: TGridCell): Boolean; virtual;
    procedure EditChange(Cell: TGridCell); virtual;
    procedure EditCloseUp(Cell: TGridCell; ItemIndex: Integer;
      var Accept: Boolean); virtual;
    procedure EditSelectNext(Cell: TGridCell; var Value: string); virtual;
    procedure GetCellColors(Cell: TGridCell; Canvas: TCanvas); virtual;
    function GetCellImage(Cell: TGridCell): Integer; virtual;
    function GetCellImageIndent(Cell: TGridCell): TPoint; virtual;
    function GetCellImageRect(Cell: TGridCell): TRect; virtual;
    function GetCellHintRect(Cell: TGridCell): TRect; virtual;
    function GetCellText(Cell: TGridCell): string; virtual;
    function GetCellTextBounds(Cell: TGridCell): TRect; virtual;
    function GetCellTextIndent(Cell: TGridCell): TPoint; virtual;
    function GetCheckAlignment(Cell: TGridCell): TAlignment; virtual;
    procedure GetCheckImage(Cell: TGridCell; CheckImage: TBitmap); virtual;
    function GetCheckIndent(Cell: TGridCell): TPoint; virtual;
    function GetCheckKind(Cell: TGridCell): TGridCheckKind; virtual;
    function GetCheckRect(Cell: TGridCell): TRect; virtual;
    function GetCheckState(Cell: TGridCell): TCheckBoxState; virtual;
    function GetClientOrigin: TPoint; override;
    function GetClientRect: TRect; override;
    function GetColumnClass: TGridColumnClass; virtual;
    function GetCursorCell(Cell: TGridCell; Offset: TGridCursorOffset)
      : TGridCell; virtual;
    function GetEditClass(Cell: TGridCell): TGridEditClass; virtual;
    procedure GetEditList(Cell: TGridCell; Items: TStrings); virtual;
    procedure GetEditListBounds(Cell: TGridCell; var Rect: TRect); virtual;
    function GetEditMask(Cell: TGridCell): string; virtual;
    function GetEditStyle(Cell: TGridCell): TGridEditStyle; virtual;
    function GetEditText(Cell: TGridCell): string; virtual;
    function GetGridLineColor(BkColor: TColor): TColor; virtual;
    function GetHeaderImage(Section: TGridHeaderSection): Integer; virtual;
    procedure GetHeaderColors(Section: TGridHeaderSection;
      Canvas: TCanvas); virtual;
    function GetSortDirection(Section: TGridHeaderSection)
      : TGridSortDirection; virtual;
    procedure GetSortImage(Section: TGridHeaderSection;
      SortImage: TBitmap); virtual;
    function GetTextRect(Canvas: TCanvas; Rect: TRect;
      LeftIndent, TopIndent: Integer; Alignment: TAlignment;
      WantReturns, WordWrap: Boolean; const Text: string): TRect; virtual;
    function GetTipsRect(Cell: TGridCell): TRect; virtual;
    function GetTipsText(Cell: TGridCell): string; virtual;
    function GetTipsWindowClass: TGridTipsWindowClass; virtual;
    procedure HeaderClick(Section: TGridHeaderSection); virtual;
    procedure HeaderClicking(Section: TGridHeaderSection;
      var AllowClick: Boolean); virtual;
    procedure HideCursor; virtual;
    procedure HideEdit; virtual;
    procedure HideFocus; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Paint; override;
    procedure Paint3DFrame(Rect: TRect; SideFlags: Integer); virtual;
    procedure PaintCell(Cell: TGridCell; Rect: TRect); virtual;
    procedure PaintCells; virtual;
    procedure PaintDotGridLines(Points: Pointer; Count: Integer); virtual;
    procedure PaintFixed; virtual;
    procedure PaintFixedGrid; virtual;
    procedure PaintFreeField; virtual;
    procedure PaintFocus; virtual;
    procedure PaintGridLines; virtual;
    procedure PaintHeader(Section: TGridHeaderSection; Rect: TRect); virtual;
    procedure PaintHeaders(DrawFixed: Boolean); virtual;
    procedure PaintHeaderSections(Sections: TGridHeaderSections;
      AllowFixed: Boolean); virtual;
    procedure PaintResizeLine;
    procedure PaintText(Canvas: TCanvas; Rect: TRect;
      LeftIndent, TopIndent: Integer; Alignment: TAlignment;
      WantReturns, WordWrap: Boolean; const Text: string); virtual;
    procedure PreparePatternBitmap(Canvas: TCanvas; FillColor: TColor;
      Remove: Boolean); virtual;
    procedure ResetClickPos; virtual;
    procedure Resize; override;
    procedure SetEditText(Cell: TGridCell; var Value: string); virtual;
    procedure SetRow(Value: Integer); virtual;
    procedure ShowCursor; virtual;
    procedure ShowEdit; virtual;
    procedure ShowEditChar(C: Char); virtual;
    procedure ShowFocus; virtual;
    procedure StartColResize(Section: TGridHeaderSection; X, Y: Integer);
    procedure StartHeaderClick(Section: TGridHeaderSection; X, Y: Integer);
    procedure StepColResize(X, Y: Integer);
    procedure StepHeaderClick(X, Y: Integer);
    procedure StopColResize(Abort: Boolean);
    procedure StopHeaderClick(Abort: Boolean);
    procedure UpdateHotHeader(Section: TGridHeaderSection);
    function CanStretchCol(ACol: Integer) : Boolean;
    function CanShrinkCol(ACol: Integer) : Boolean;
    function LimitColumnWidth(ACol, AWidth: Integer): Integer;
    procedure VclStyleChanged; virtual;

    property VclStyleEnabled: Boolean
      read FVclStyleEnabled;

  public
    class constructor Create;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ApplyEdit; virtual;
    procedure CancelEdit; virtual;
    procedure DefaultDrawCell(Cell: TGridCell; Rect: TRect); virtual;
    procedure DefaultDrawHeader(Section: TGridHeaderSection; Rect: TRect); virtual;
    procedure DrawDragRect(Cell: TGridCell); virtual;
    function FindText(const SearchStr: string; StartCell: TGridCell;
      Options: TFindOptions; var ResultCell: TGridCell): Boolean; virtual;
    function Focused: Boolean; override;
    function LastSelectableCol(ARow : Integer) : Integer; virtual;
    function GetCellAt(X, Y: Integer): TGridCell; virtual;
    function GetCellRect(Cell: TGridCell): TRect;
    function GetCellsRect(Cell1, Cell2: TGridCell): TRect;
    function GetColumnAt(X, Y: Integer): Integer; virtual;
    function GetColumnLeftRight(Column: Integer): TRect;
    function GetColumnMaxWidth(
      Column          : Integer;
      IncludeTitles   : Boolean = False;
      OnlyVisibleRows : Boolean = True
    ) : Integer; virtual;
    function GetColumnRect(Column: Integer): TRect;
    function GetColumnsRect(Column1, Column2: Integer): TRect;
    function GetColumnsWidth(Column1, Column2: Integer): Integer;
    function GetEditRect(Cell: TGridCell): TRect; virtual;
    function GetFirstImageColumn: Integer;
    function GetFixedRect: TRect; virtual;
    function GetFixedWidth: Integer;
    function GetFocusRect: TRect; virtual;
    function GetGridHeight: Integer;
    function GetGridOrigin: TPoint;
    function GetGridRect: TRect; virtual;
    function GetHeaderHeight: Integer;
    function GetHeaderRect: TRect; virtual;
    function GetHeaderSection(ColumnIndex, Level: Integer): TGridHeaderSection;
    function GetResizeSectionAt(X, Y: Integer): TGridHeaderSection;
    function GetRowAt(X, Y: Integer): Integer; virtual;
    function GetRowRect(Row: Integer): TRect;
    function GetRowsRect(Row1, Row2: Integer): TRect;
    function GetRowsHeight(Row1, Row2: Integer): Integer;
    function GetRowTopBottom(Row: Integer): TRect;
    function GetSectionAt(X, Y: Integer): TGridHeaderSection;

    procedure Invalidate; override;
    procedure InvalidateCell(Cell: TGridCell);
    procedure InvalidateCheck(Cell: TGridCell);
    procedure InvalidateColumn(Column: Integer);
    procedure InvalidateColumns(Column1, Column2: Integer);
    procedure InvalidateEdit;
    procedure InvalidateFixed;
    procedure InvalidateFocus; virtual;
    procedure InvalidateGrid;
    procedure InvalidateHeader;
    procedure InvalidateRect(Rect: TRect);
    procedure InvalidateRow(Row: Integer);
    procedure InvalidateRows(Row1, Row2: Integer);

    function IsActiveControl: Boolean;
    function IsCellAcceptCursor(Cell: TGridCell): Boolean; virtual;
    function IsCellHighlighted(Cell: TGridCell): Boolean; virtual;
    function IsCellHasCheck(Cell: TGridCell): Boolean; virtual;
    function IsCellHasImage(Cell: TGridCell): Boolean; virtual;
    function IsCellFocused(Cell: TGridCell): Boolean;
    function IsCellFixed(Cell: TGridCell) : Boolean;
    function IsCellReadOnly(Cell: TGridCell): Boolean; virtual;
    function IsCellValid(Cell: TGridCell): Boolean;
    function IsCellValidEx(Cell: TGridCell;
      CheckPosition, CheckVisible: Boolean): Boolean;
    function IsCellVisible(Cell: TGridCell; PartialOK: Boolean): Boolean;
    function IsColumnVisible(Column: Integer): Boolean;
    function IsFocusAllowed: Boolean;
    function IsHeaderHasImage(Section: TGridHeaderSection): Boolean; virtual;
    function IsHeaderPressed(Section: TGridHeaderSection): Boolean; virtual;
    function IsRowVisible(Row: Integer): Boolean;
    procedure LockUpdate;
    procedure MakeCellVisible(Cell: TGridCell; PartialOK: Boolean); virtual;
    procedure SetCursor(Cell: TGridCell; Selected, Visible: Boolean); virtual;
    procedure UndoEdit; virtual;
    procedure UnLockUpdate(Redraw: Boolean);
    procedure UpdateCursor; virtual;
    procedure UpdateColors; virtual;
    procedure UpdateEdit(Activate: Boolean); virtual;
    procedure UpdateEditContents(SaveText: Boolean); virtual;
    procedure UpdateEditText; virtual;
    procedure UpdateFixed; virtual;
    procedure UpdateFocus; virtual;
    procedure UpdateFonts; virtual;
    procedure UpdateHeader; virtual;
    procedure UpdateRows; virtual;
    procedure UpdateScrollBars; virtual;
    procedure UpdateScrollPos; virtual;
    procedure UpdateSelection(var Cell: TGridCell;
      var Selected: Boolean); virtual;
    procedure UpdateText; virtual;
    procedure UpdateVisOriginSize; virtual;

    procedure AutoSizeCols(
      AIncludeTitles  : Boolean = True;
      OnlyVisibleRows : Boolean = True
    ); virtual;
    procedure SizeColumnsToClient;

    property AllowEdit: Boolean
      read FAllowEdit write SetAllowEdit default False;

    property AllowSelect: Boolean
      read FAllowSelect write SetAllowSelect default True;

    property AlwaysEdit: Boolean
      read FAlwaysEdit write SetAlwaysEdit default False;

    property AlwaysSelected: Boolean
      read FAlwaysSelected write SetAlwaysSelected default False;

    property BorderStyle: TBorderStyle
      read FBorderStyle write SetBorderStyle default bsSingle;

    property CancelOnExit: Boolean
      read FCancelOnExit write FCancelOnExit default True;

    property Canvas;

    property CellFocused: TGridCell
      read FCellFocused write SetCellFocused;

    property Cells[Col, Row: Integer]: string
      read GetCell write SetCell;

    property CellSelected: Boolean
      read FCellSelected write SetCellSelected;

    property CheckBoxes: Boolean
      read FCheckBoxes write SetCheckBoxes default False;

    property Checked[Col, Row: Integer]: Boolean
      read GetChecked;

    property CheckState[Col, Row: Longint]: TCheckBoxState
      read GetCheckBoxState;

    property CheckHeight: Integer
      read FCheckHeight;

    property CheckLeftIndent: Integer
      read FCheckLeftIndent write SetCheckLeftIndent default 0;

    property CheckStyle: TGridCheckStyle
      read FCheckStyle write SetCheckStyle default csWin95;

    property CheckTopIndent: Integer
      read FCheckTopIndent write SetCheckTopIndent default 0;

    property CheckWidth: Integer
      read FCheckWidth;

    property Col: Integer
      read GetCol write SetCol;

    property ColResizing: Boolean
      read FColResizing;

    property ColumnClick: Boolean
      read FColumnClick write FColumnClick default True;

    property Columns: TGridColumns
      read FColumns write SetColumns;

    property ColumnsFullDrag: Boolean
      read FColumnsFullDrag write FColumnsFullDrag default False;

    property ColumnsSizing: Boolean
      read FColumnsSizing write FColumnsSizing default True;

    property CursorKeys: TGridCursorKeys
      read FCursorKeys write SetCursorKeys default [gkArrows, gkMouse, gkMouseWheel];

    property DefaultEditMenu: Boolean
      read FDefaultEditMenu write FDefaultEditMenu default False;

    property DoubleBuffered: Boolean
      read FDoubleBuffered write FDoubleBuffered default False;

    property Edit: TGridEdit
      read GetEdit;

    property EditCell: TGridCell
      read FEditCell;

    property EditColumn: TGridColumn
      read GetEditColumn;

    property EditDropDown: Boolean
      read GetEditDropDown write SetEditDropDown;

    property Editing: Boolean
      read GetEditing write SetEditing;

    property EndEllipsis: Boolean
      read FEndEllipsis write SetEndEllipsis default True;

    property Fixed: TGridFixed
      read GetFixed write SetFixed;

    property FitColsToClient: Boolean
      read FFitColsToClient write SetFitColsToClient;

    property FlatBorder: Boolean
      read FFlatBorder write SetFlatBorder default False;

    property FocusOnScroll: Boolean
      read FFocusOnScroll write FFocusOnScroll default False;

    property GridColor: TColor
      read FGridColor write SetGridColor default clSilver;

    property GridLines: Boolean
      read FGridLines write SetGridLines default True;

    property GridLineWidth: Integer
      read FGridLineWidth;

    property GridStyle: TGridStyles
      read FGridStyle write SetGridStyle default [gsHorzLine, gsVertLine];

    property Header: TGridHeader
      read GetHeader write SetHeader;

    property HideSelection: Boolean
      read FHideSelection write SetHideSelection default False;

    property HorzScrollBar: TGridScrollBar
      read FHorzScrollBar write SetHorzScrollBar;

    property ImageHighlight: Boolean
      read FImageHighlight write SetImageHighlight default True;

    property ImageIndexDef: Integer
      read FImageIndexDef write SetImageIndexDef default 0;

    property ImageLeftIndent: Integer
      read FImageLeftIndent write SetImageLeftIndent default 2;

    property Images: TImageList
      read FImages write SetImages;

    property ImageTopIndent: Integer
      read FImageTopIndent write SetImageTopIndent default 1;

    property LeftCol: Integer
      read GetLeftCol write SetLeftCol;

    property ReadOnly: Boolean
      read FReadOnly write SetReadOnly default False;

    property RightClickSelect: Boolean
      read FRightClickSelect write FRightClickSelect default True;

    property Row: Integer
      read GetRow write SetRow;

    property Rows: TGridRows
      read GetRows write SetRows;

    property RowSelect: Boolean
      read FRowSelect write SetRowSelect default False;

    property ShowCellTips: Boolean
      read FShowCellTips write SetShowCellTips;

    property ShowFocusRect: Boolean
      read FShowFocusRect write SetShowFocusRect default True;

    property ShowHeader: Boolean
      read FShowHeader write SetShowHeader default True;

    property SortLeftIndent: Integer
      read FSortLeftIndent write SetSortLeftIndent default 0;  // was 5

    property SortTopIndent: Integer
      read FSortTopIndent write SetSortTopIndent default 0;

    property TextLeftIndent: Integer
      read FTextLeftIndent write SetTextLeftIndent default 6;

    property TextRightIndent: Integer
      read FTextRightIndent write SetTextRightIndent default 6;

    property TextTopIndent: Integer
      read FTextTopIndent write SetTextTopIndent default 2;

    property ThemingEnabled: Boolean
      read FThemingEnabled write SetThemingEnabled default True;

    property TipsCell: TGridCell
      read FTipsCell;

    property TipsText: string
      read FTipsText;

    property TopRow: Integer
      read GetTopRow write SetTopRow;

    property UpdateLock: Integer
      read FUpdateLock;

    property VertScrollBar: TGridScrollBar
      read FVertScrollBar write SetVertScrollBar;

    property VisibleColCount: Integer
      read GetVisibleColCount;

    property VisibleRowCount: Integer
      read GetVisibleRowCount;

    property VisOrigin: TGridCell
      read FVisOrigin write SetVisOrigin;

    property VisSize: TGridCell
      read FVisSize;

    property OnAppendRow: TGridAppendRowEvent
      read FOnAppendRow write FOnAppendRow;

    property OnCellAcceptCursor: TGridCellAcceptCursorEvent
      read FOnCellAcceptCursor write FOnCellAcceptCursor;

    property OnCellClick: TGridCellClickEvent
      read FOnCellClick write FOnCellClick;

    property OnCellTips: TGridCellTipsEvent
      read FOnCellTips write FOnCellTips;

    property OnChange: TGridChangedEvent
      read FOnChange write FOnChange;

    property OnChangeColumns: TNotifyEvent
      read FOnChangeColumns write FOnChangeColumns;

    property OnChangeEditing: TNotifyEvent
      read FOnChangeEditing write FOnChangeEditing;

    property OnChangeEditMode: TNotifyEvent
      read FOnChangeEditMode write FOnChangeEditMode;

    property OnChangeFixed: TNotifyEvent
      read FOnChangeFixed write FOnChangeFixed;

    property OnChangeRows: TNotifyEvent
      read FOnChangeRows write FOnChangeRows;

    property OnChanging: TGridChangingEvent
      read FOnChanging write FOnChanging;

    property OnCheckClick: TGridCellNotifyEvent
      read FOnCheckClick write FOnCheckClick;

    property OnColumnAutoSize: TGridColumnResizeEvent
      read FOnColumnAutoSize write FOnColumnAutoSize;

    property OnColumnResize: TGridColumnResizeEvent
      read FOnColumnResize write FOnColumnResize;

    property OnColumnResizing: TGridColumnResizeEvent
      read FOnColumnResizing write FOnColumnResizing;

    property OnDraw: TGridDrawEvent
      read FOnDraw write FOnDraw;

    property OnDrawCell: TGridDrawCellEvent
      read FOnDrawCell write FOnDrawCell;

    property OnDrawHeader: TGridDrawHeaderEvent
      read FOnDrawHeader write FOnDrawHeader;

    property OnEditAcceptKey: TGridAcceptKeyEvent
      read FOnEditAcceptKey write FOnEditAcceptKey;

    property OnEditButtonPress: TGridCellNotifyEvent
      read FOnEditButtonPress write FOnEditButtonPress;

    property OnEditCanceled: TGridCellNotifyEvent
      read FOnEditCanceled write FOnEditCanceled;

    property OnEditCanModify: TGridEditCanModifyEvent
      read FOnEditCanModify write FOnEditCanModify;

    property OnEditCanShow: TGridEditCanShowEvent
      read FOnEditCanShow write FOnEditCanShow;

    property OnEditChange: TGridCellNotifyEvent
      read FOnEditChange write FOnEditChange;

    property OnEditCloseUp: TGridEditCloseUpEvent
      read FOnEditCloseUp write FOnEditCloseUp;

    property OnEditSelectNext: TGridTextEvent
      read FOnEditSelectNext write FOnEditSelectNext;

    property OnGetCellColors: TGridCellColorsEvent
      read FOnGetCellColors write FOnGetCellColors;

    property OnGetCellHintRect: TGridRectEvent
      read FOnGetCellHintRect write FOnGetCellHintRect;

    property OnGetCellImage: TGridCellImageEvent
      read FOnGetCellImage write FOnGetCellImage;

    property OnGetCellImageIndent: TGridCellIndentEvent
      read FOnGetCellImageIndent write FOnGetCellImageIndent;

    property OnGetCellReadOnly: TGridCellreadOnlyEvent
      read FOnGetCellReadOnly write FOnGetCellReadOnly;

    property OnGetCellText: TGridTextEvent
      read FOnGetCellText write FOnGetCellText;

    property OnGetCellTextIndent: TGridCellIndentEvent
      read FOnGetCellTextIndent write FOnGetCellTextIndent;

    property OnGetCheckAlignment: TGridCheckAlignmentEvent
      read FOnGetCheckAlignment write FOnGetCheckAlignment;

    property OnGetCheckImage: TGridCheckImageEvent
      read FOnGetCheckImage write FOnGetCheckImage;

    property OnGetCheckIndent: TGridCellIndentEvent
      read FOnGetCheckIndent write FOnGetCheckIndent;

    property OnGetCheckKind: TGridCheckKindEvent
      read FOnGetCheckKind write FOnGetCheckKind;

    property OnGetCheckState: TGridCheckStateEvent
      read FOnGetCheckState write FOnGetCheckState;

    property OnGetEditList: TGridEditListEvent
      read FOnGetEditList write FOnGetEditList;

    property OnGetEditListBounds: TGridRectEvent
      read FOnGetEditListBounds write FOnGetEditListBounds;

    property OnGetEditMask: TGridTextEvent
      read FOnGetEditMask write FOnGetEditMask;

    property OnGetEditStyle: TGridEditStyleEvent
      read FOnGetEditStyle write FOnGetEditStyle;

    property OnGetEditText: TGridTextEvent
      read FOnGetEditText write FOnGetEditText;

    property OnGetHeaderColors: TGridHeaderColorsEvent
      read FOnGetHeaderColors write FOnGetHeaderColors;

    property OnGetHeaderImage: TGridHeaderImageEvent
      read FOnGetHeaderImage write FOnGetHeaderImage;

    property OnGetSortDirection: TGridSortDirectionEvent
      read FOnGetSortDirection write FOnGetSortDirection;

    property OnGetSortImage: TGridSortImageEvent
      read FOnGetSortImage write FOnGetSortImage;

    property OnGetTipsRect: TGridRectEvent
      read FOnGetTipsRect write FOnGetTipsRect;

    property OnGetTipsText: TGridTextEvent
      read FOnGetTipsText write FOnGetTipsText;

    property OnHeaderClick: TGridHeaderClickEvent
      read FOnHeaderClick write FOnHeaderClick;

    property OnHeaderClicking: TGridHeaderClickingEvent
      read FOnHeaderClicking write FOnHeaderClicking;

    property OnSetEditText: TGridTextEvent
      read FOnSetEditText write FOnSetEditText;
  end;

{ TGridView }

  TGridView = class(TCustomGridView)
  published
    property Align;
    property AllowEdit;
    property AllowSelect;
    property AlwaysEdit;
    property AlwaysSelected;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind default bkNone;
    property BevelWidth;
    property BorderStyle;
    property CancelOnExit;
    property CheckBoxes;
    property CheckStyle;
    property Color;
    property ColumnClick;
    property Columns;
    property ColumnsFullDrag;
    property Constraints;
    property CursorKeys;
    property DefaultEditMenu;
    property DragCursor;
    property DragMode;
    property DoubleBuffered;
    property Enabled;
    property EndEllipsis;
    property FitColsToClient;
    property Fixed;
    property FlatBorder;
    property FocusOnScroll;
    property Font;
    property GridColor;
    property GridLines;
    property GridStyle;
    property Header;
    property HideSelection;
    property Hint;
    property HorzScrollBar;
    property ImageIndexDef;
    property ImageHighlight;
    property Images;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RightClickSelect;
    property Rows;
    property RowSelect;
    property ShowCellTips;
    property ShowFocusRect;
    property ShowHeader;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property ThemingEnabled;
    property VertScrollBar;
    property Visible;

    property OnAppendRow;
    property OnCellAcceptCursor;
    property OnCellClick;
    property OnCellTips;
    property OnChange;
    property OnChangeColumns;
    property OnChangeEditing;
    property OnChangeEditMode;
    property OnChangeFixed;
    property OnChangeRows;
    property OnChanging;
    property OnCheckClick;
    property OnClick;
    property OnColumnAutoSize;
    property OnColumnResizing;
    property OnColumnResize;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDraw;
    property OnDrawCell;
    property OnDrawHeader;
    property OnEditAcceptKey;
    property OnEditButtonPress;
    property OnEditCanceled;
    property OnEditCanModify;
    property OnEditCanShow;
    property OnEditChange;
    property OnEditCloseUp;
    property OnEditSelectNext;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetCellColors;
    property OnGetCellHintRect;
    property OnGetCellImage;
    property OnGetCellImageIndent;
    property OnGetCellReadOnly;
    property OnGetCellText;
    property OnGetCellTextIndent;
    property OnGetCheckAlignment;
    property OnGetCheckImage;
    property OnGetCheckIndent;
    property OnGetCheckKind;
    property OnGetCheckState;
    property OnGetEditList;
    property OnGetEditListBounds;
    property OnGetEditMask;
    property OnGetEditStyle;
    property OnGetEditText;
    property OnGetHeaderColors;
    property OnGetHeaderImage;
    property OnGetSortDirection;
    property OnGetSortImage;
    property OnGetTipsRect;
    property OnGetTipsText;
    property OnHeaderClick;
    property OnHeaderClicking;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnResize;
    property OnSetEditText;
    property OnStartDrag;
  end;

  TGridViewVclStyleScrollBarsHook = class(TMouseTrackControlStyleHook)
  strict private type
  {$REGION 'TVclStyleScrollBarWindow'}
    TVclStyleScrollBarWindow = class(TWinControl)
    private
      FScrollBarWindowOwner : TGridViewVclStyleScrollBarsHook;
      FScrollBarVertical    : Boolean;
      FScrollBarVisible     : Boolean;
      FScrollBarEnabled     : Boolean;

      procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
      procedure WMEraseBkgnd(var Msg: TMessage); message WM_ERASEBKGND;
      procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;

    protected
      procedure CreateParams(var Params: TCreateParams); override;

    public
      constructor Create(AOwner: TComponent); override;

      property ScrollBarWindowOwner: TGridViewVclStyleScrollBarsHook
        read FScrollBarWindowOwner write FScrollBarWindowOwner;

      property ScrollBarVertical: Boolean
        read FScrollBarVertical write FScrollBarVertical;

      property ScrollBarVisible: Boolean
        read FScrollBarVisible write FScrollBarVisible;

      property ScrollBarEnabled: Boolean
        read FScrollBarEnabled write FScrollBarEnabled;
    end;
  {$ENDREGION}
  private
    FHorzScrollBarDownButtonRect  : TRect;
    FHorzScrollBarDownButtonState : TThemedScrollBar;
    FHorzScrollBarRect            : TRect;
    FHorzScrollBarSliderState     : TThemedScrollBar;
    FHorzScrollBarSliderTrackRect : TRect;
    FHorzScrollBarUpButtonRect    : TRect;
    FHorzScrollBarUpButtonState   : TThemedScrollBar;
    FHorzScrollBarWindow          : TVclStyleScrollBarWindow;
    FLeftMouseButtonDown          : Boolean;
    FPrevScrollPos                : Integer;
    FScrollPos                    : Single;
    FVertScrollBarDownButtonRect  : TRect;
    FVertScrollBarDownButtonState : TThemedScrollBar;
    FVertScrollBarRect            : TRect;
    FVertScrollBarSliderState     : TThemedScrollBar;
    FVertScrollBarSliderTrackRect : TRect;
    FVertScrollBarUpButtonRect    : TRect;
    FVertScrollBarUpButtonState   : TThemedScrollBar;
    FVertScrollBarWindow          : TVclStyleScrollBarWindow;

    procedure WMKeyDown(var Msg: TMessage); message WM_KEYDOWN;
    procedure WMKeyUp(var Msg: TMessage); message WM_KEYUP;
    procedure WMLButtonDown(var Msg: TWMMouse); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Msg: TWMMouse); message WM_LBUTTONUP;
    procedure WMNCLButtonDown(var Msg: TWMMouse); message WM_NCLBUTTONDOWN;
    procedure WMNCMouseMove(var Msg: TWMMouse); message WM_NCMOUSEMOVE;
    procedure WMNCLButtonUp(var Msg: TWMMouse); message WM_NCLBUTTONUP;
    procedure WMNCPaint(var Msg: TMessage); message WM_NCPAINT;
    procedure WMMouseMove(var Msg: TWMMouse); message WM_MOUSEMOVE;
    procedure WMMouseWheel(var Msg: TMessage); message WM_MOUSEWHEEL;
    procedure WMVScroll(var Msg: TMessage); message WM_VSCROLL;
    procedure WMHScroll(var Msg: TMessage); message WM_HSCROLL;
    procedure WMCaptureChanged(var Msg: TMessage); message WM_CAPTURECHANGED;
    procedure WMNCLButtonDblClk(var Msg: TWMMouse); message WM_NCLBUTTONDBLCLK;
    procedure WMSize(var Msg: TMessage); message WM_SIZE;

  protected
    procedure CalcScrollBarsRect; virtual;
    procedure DrawHorzScrollBar(DC: HDC); virtual;
    procedure DrawVertScrollBar(DC: HDC); virtual;
    function GetHorzScrollBarSliderRect: TRect;
    function GetVertScrollBarSliderRect: TRect;
    procedure MouseLeave; override;
    procedure PaintScrollBars; virtual;
    function PointInTreeHeader(const P: TPoint): Boolean;
    procedure UpdateScrollBarWindow;

  public
    constructor Create(AControl: TWinControl); override;
    destructor Destroy; override;
  end;

{ Several utility routines }

function GridCell(Col, Row: Integer): TGridCell;
function IsCellEqual(Cell1, Cell2: TGridCell): Boolean;
function IsCellEmpty(Cell: TGridCell): Boolean;
function OffsetCell(Cell: TGridCell; C, R: Integer): TGridCell;

{ The standard Windows scrollbar has limitation to the the maximum position
  (32768, it can not be under WinNT), but in the table of records it can be
  it is more, then for the installation of the parameters of their scroller one
  should scale }

const
  MaxWinPos = 10000;

function WinPosToScrollPos(WinPos, ScrollMin, ScrollMax: Integer): Integer;
function ScrollPosToWinPos(ScrollPos, ScrollMin, ScrollMax: Integer): Integer;

function GetFontWidth(Font: TFont; TextLength: Integer): Integer;

procedure FillDWord(var Dest; Count, Value: Integer);

implementation

uses
  System.UITypes, System.Types;

{$R *.res}

// non interfaced routines
type
  PIntArray = ^TIntArray;
  TIntArray = array[0..MaxInt div 16 - 1] of Integer;

function CasePos(Substr: string; S: string; MatchCase: Boolean): Integer;
begin
  if not MatchCase then
  begin
    Substr := AnsiUpperCase(Substr);
    S      := AnsiUpperCase(S);
  end;
  Result := AnsiPos(Substr, S);
end;

function CompareStrEx(const S1, S2: string;
  WholeWord, MatchCase: Boolean): Boolean;
begin
  if WholeWord then
    if MatchCase then
      Result := AnsiCompareStr(S1, S2) = 0
    else
      Result := AnsiCompareText(S1, S2) = 0
  else
    Result := CasePos(S1, S2, MatchCase) <> 0;
end;

procedure PaintToolBarBtnFrameEx(DC: HDC; Rect: TRect; Pressed: Boolean; SideFlags: Integer);
begin
  if StyleServices.Enabled then
  begin

  end
  else
  begin
    if not Pressed then
    begin
      DrawEdge(DC, Rect, BDR_RAISEDOUTER, SideFlags and (not BF_TOPLEFT));
      if SideFlags and BF_BOTTOM <> 0 then
        Dec(Rect.Bottom);
      if SideFlags and BF_RIGHT <> 0 then
        Dec(Rect.Right);
      DrawEdge(DC, Rect, BDR_RAISEDINNER, SideFlags and (not BF_BOTTOMRIGHT));
      if SideFlags and BF_TOP <> 0 then
        Inc(Rect.Top);
      if SideFlags and BF_LEFT <> 0 then
        Inc(Rect.Left);
      DrawEdge(DC, Rect, BDR_RAISEDINNER, SideFlags and (not BF_TOPLEFT));
      if SideFlags and BF_BOTTOM <> 0 then
        Dec(Rect.Bottom);
      if SideFlags and BF_RIGHT <> 0 then
        Dec(Rect.Right);
      DrawEdge(DC, Rect, BDR_RAISEDOUTER, SideFlags and (not BF_BOTTOMRIGHT));
    end
    else
    begin
      DrawEdge(DC, Rect, BDR_SUNKENOUTER, SideFlags and (not BF_TOPLEFT));
      if SideFlags and BF_BOTTOM <> 0 then
        Dec(Rect.Bottom);
      if SideFlags and BF_RIGHT <> 0 then
        Dec(Rect.Right);
      DrawEdge(DC, Rect, BDR_SUNKENINNER, SideFlags and (not BF_BOTTOMRIGHT));
      if SideFlags and BF_TOP <> 0 then
        Inc(Rect.Top);
      if SideFlags and BF_LEFT <> 0 then
        Inc(Rect.Left);
      DrawEdge(DC, Rect, BDR_SUNKENINNER, SideFlags and (not BF_TOPLEFT));
      if SideFlags and BF_BOTTOM <> 0 then
        Dec(Rect.Bottom);
      if SideFlags and BF_RIGHT <> 0 then
        Dec(Rect.Right);
      DrawEdge(DC, Rect, BDR_SUNKENOUTER, SideFlags and (not BF_BOTTOMRIGHT));
    end
  end;
end;

procedure PaintScrollBtnFrameEx(DC: HDC; Rect: TRect; Pressed: Boolean; SideFlags: Integer);
begin
  if StyleServices.Enabled then
  begin
    ///

  end
  else
  begin
    if not Pressed then
      PaintToolBarBtnFrameEx(DC, Rect, Pressed, SideFlags)
    else
      DrawEdge(DC, Rect, BDR_SUNKENOUTER, SideFlags or BF_FLAT);
  end;
end;

function GetTextWidth(const AText: string; AFont: TFont): Integer;
var
  Bitmap: Vcl.Graphics.TBitmap;
begin
  Bitmap := Vcl.Graphics.TBitmap.Create;
  try
    Bitmap.Canvas.Font.Assign(AFont);
    Result := Bitmap.Canvas.TextExtent(AText).cx;
 finally
    Bitmap.Free;
  end;
end;

procedure PaintBtnComboBox2(DC: HDC; Rect: TRect; Pressed: Boolean);
var
  Flags, DX: Integer;
  Details: TThemedElementDetails;
begin
  if StyleServices.Enabled then
  begin
    if Pressed then
      Details := StyleServices.GetElementDetails(tcDropDownButtonPressed)
    else
      Details := StyleServices.GetElementDetails(tcDropDownButtonNormal);
    StyleServices.DrawElement(DC, Details, Rect);
  end
  else
  begin
    Flags := 0;
    if Pressed then Flags := DFCS_FLAT;
    DrawEdge(DC, Rect, EDGE_RAISED, BF_RECT or BF_MIDDLE or Flags);
    Flags := (Rect.Right - Rect.Left) div 2 - 1 + Ord(Pressed);
    DX := (Rect.Right - Rect.Left) mod 2 - 1;
    PatBlt(DC, Rect.Left + Flags - 2 + DX, Rect.Top + Flags - 1, 7, 1, BLACKNESS);
    PatBlt(DC, Rect.Left + Flags - 1 + DX, Rect.Top + Flags + 0, 5, 1, BLACKNESS);
    PatBlt(DC, Rect.Left + Flags - 0 + DX, Rect.Top + Flags + 1, 3, 1, BLACKNESS);
    PatBlt(DC, Rect.Left + Flags + 1 + DX, Rect.Top + Flags + 2, 1, 1, BLACKNESS);
  end;
end;

procedure PaintBtnEllipsis(DC: HDC; Rect: TRect; Pressed: Boolean);
var
  Details: TThemedElementDetails;
  Flags: Integer;
begin
  if StyleServices.Enabled then
  begin
    if Pressed then
      Details := StyleServices.GetElementDetails(tbPushButtonPressed)
    else
      Details := StyleServices.GetElementDetails(tbPushButtonNormal);
    StyleServices.DrawElement(DC, Details, Rect);
  end
  else
  begin
    Flags := 0;
    if Pressed then
      Flags := BF_FLAT;
    DrawEdge(DC, Rect, EDGE_RAISED, BF_RECT or BF_MIDDLE or Flags);
    Flags := (Rect.Right - Rect.Left) div 2 - 1 + Ord(Pressed);
    PatBlt(DC, Rect.Left + Flags, Rect.Top + Flags, 2, 2, BLACKNESS);
    PatBlt(DC, Rect.Left + Flags - 3, Rect.Top + Flags, 2, 2, BLACKNESS);
    PatBlt(DC, Rect.Left + Flags + 3, Rect.Top + Flags, 2, 2, BLACKNESS);
  end;
end;

function GetTextLineCount(const S: string): Integer;
var
  P: PChar;
begin
  Result := 0;
  P      := Pointer(S);
  while P^ <> #0 do
  begin
    while not CharInSet(P^, [#0, #10, #13]) do
      Inc(P);
    Inc(Result);
    if P^ = #13 then
      Inc(P);
    if P^ = #10 then
      Inc(P);
  end;
end;

procedure FillDword(var Dest; Count, Value: Integer);
var
  I : Integer;
  E : Integer;
type
  PV = ^Integer;
begin
  I := Integer(@Dest);
  E := I + Count * 4;
  while I < E do
  begin
    PV(I)^ := Value;
    Inc(I, 4);
  end;
end;

function ExpandStrings(Strings: TStrings; const Separator: string): string;
var
  I: Integer;
begin
  Result := '';
  if Strings.Count > 0 then
  begin
    Result := Strings[0];
    for I := 1 to Strings.Count - 1 do
      Result := Result + Separator + Strings[I];
  end;
end;

function GetFontHeight(Font: TFont): Integer;
var
  DC: HDC;
  Canvas: TCanvas;
begin
  DC := GetDC(0);
  try
    Canvas := TCanvas.Create;
    try
      Canvas.Handle := DC;
      Canvas.Font := Font;
      Result := Canvas.TextHeight('^j');
    finally
      Canvas.Free;
    end;
  finally
    ReleaseDC(0, DC);
  end;
end;

{ Calculation (provisional) width of a line from the specified quantity of symbols. }

function GetFontWidth(Font: TFont; TextLength: Integer): Integer;
var
  DC: HDC;
  Canvas: TCanvas;
  TM: TTextMetric;
begin
  DC := GetDC(0);
  try
    Canvas := TCanvas.Create;
    try
      Canvas.Handle := DC;
      Canvas.Font := Font;
      GetTextMetrics(DC, TM);
      Result := TextLength * (Canvas.TextWidth('0') - TM.tmOverhang) + TM.tmOverhang + 4;
    finally
      Canvas.Free;
    end;
  finally
    ReleaseDC(0, DC);
  end;
end;

{ Utility routines }

function GridCell(Col, Row: Integer): TGridCell;
begin
  Result.Col := Col;
  Result.Row := Row;
end;

function IsCellEqual(Cell1, Cell2: TGridCell): Boolean;
begin
  Result := (Cell1.Col = Cell2.Col) and (Cell1.Row = Cell2.Row);
end;

function IsCellEmpty(Cell: TGridCell): Boolean;
begin
  Result := (Cell.Col = -1) or (Cell.Row = -1);
end;

function OffsetCell(Cell: TGridCell; C, R: Integer): TGridCell;
begin
  Result.Col := Cell.Col + C;
  Result.Row := Cell.Row + R;
end;

function WinPosToScrollPos(WinPos, ScrollMin, ScrollMax: Integer): Integer;
begin
  Result := ScrollMin + MulDiv(WinPos, ScrollMax - ScrollMin, MaxWinPos);
end;

function ScrollPosToWinPos(ScrollPos, ScrollMin, ScrollMax: Integer): Integer;
begin
  if ScrollMax <> ScrollMin then
  begin
    Result := MulDiv(ScrollPos - ScrollMin, MaxWinPos, ScrollMax - ScrollMin);
    Exit;
  end;
  Result := 0;
end;

{ TGridHeaderSection }

constructor TGridHeaderSection.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FWidth := 64;
end;

destructor TGridHeaderSection.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FSections);
end;

function TGridHeaderSection.IsSectionsStored: Boolean;
begin
  Result := (FSections <> nil) and (FSections.Count > 0);
end;

function TGridHeaderSection.IsWidthStored: Boolean;
begin
  Result := ((FSections = nil) or (FSections.Count = 0)) and (Width <> 64);
end;

function TGridHeaderSection.GetAllowClick: Boolean;
var
  I: Integer;
begin
  Result := False;
  if (Header <> nil) and (Header.Grid <> nil) then
  begin
    I := ColumnIndex;
    if I < Header.Grid.Columns.Count then
      Result := Header.Grid.Columns[I].AllowClick;
  end;
end;

function TGridHeaderSection.GetBoundsRect: TRect;
begin
  if (Header = nil) or (Header.Grid = nil) then
  begin
    Result := Rect(0, 0, 0, 0);
    Exit;
  end;
  {absolute boundaries}
  Result := FBoundsRect;
  {if this is the not fixed title - we displace it by the value of the shift
    table by scroller}
  if not FixedColumn then
    OffsetRect(Result, Header.Grid.GetGridOrigin.X, 0);
end;

function TGridHeaderSection.GetDisplayName: string;
begin
  Result := Caption;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

function TGridHeaderSection.GetFirstColumnIndex: Integer;
begin
  if Sections.Count > 0 then
  begin
    Result := Sections[0].FirstColumnIndex;
    Exit;
  end;
  Result := ColumnIndex;
end;

function TGridHeaderSection.GetFixedColumn: Boolean;
begin
  if Sections.Count > 0 then
  begin
    Result := Sections[0].FixedColumn;
    Exit;
  end;
  if (Header = nil) or (Header.Grid = nil) then
  begin
    Result := False;
    Exit;
  end;
  Result := ColumnIndex < Header.Grid.Fixed.Count;
end;

function TGridHeaderSection.GetHeader: TCustomGridHeader;
begin
  if ParentSections <> nil then
  begin
    Result := ParentSections.Header;
    Exit;
  end;
  Result := nil;
end;

function TGridHeaderSection.GetLevel: Integer;
begin
  if Parent <> nil then
  begin
    Result := Parent.Level + 1;
    Exit;
  end;
  Result := 0
end;

function TGridHeaderSection.GetParent: TGridHeaderSection;
begin
  if ParentSections <> nil then
  begin
    Result := ParentSections.OwnerSection;
    Exit;
  end;
  Result := nil;
end;

function TGridHeaderSection.GetParentSections: TGridHeaderSections;
begin
  if Collection <> nil then
  begin
    Result := TGridHeaderSections(Collection);
    Exit;
  end;
  Result := nil;
end;

function TGridHeaderSection.GetSections: TGridHeaderSections;
begin
  if FSections = nil then
    FSections := TGridHeaderSections.Create(Header, Self);
  Result := FSections;
end;

function TGridHeaderSection.GetResizeColumnIndex: Integer;
var
  I: Integer;
begin
  {if there are subtitles we return the column of the latter from them}
  for I := Sections.Count - 1 downto 0 do
    if Sections[I].Visible then
    begin
      Result := Sections[I].ResizeColumnIndex;
      Exit;
    end;
  { return the calculated index of column}
  Result := FColumnIndex;
end;

function TGridHeaderSection.GetVisible: Boolean;
var
  I: Integer;
begin
  {if there are subtitles, then we look their visibility}
  if Sections.Count > 0 then
    for I := 0 to Sections.Count - 1 do
      if Sections[I].Visible then
      begin
        Result := True;
        Exit;
      end;
  { otherwise look the visibility of column}
  if (Header <> nil) and (Header.Grid <> nil) then
  begin
    I := ColumnIndex;
    if I < Header.Grid.Columns.Count then
    begin
      Result := Header.Grid.Columns[I].Visible;
      Exit;
    end;
  end;
  {there is no column - section is visible}
  Result := True;
end;

function TGridHeaderSection.GetWidth: Integer;
var
  I: Integer;
  S: TGridHeaderSection;
begin
  {if there are subtitles, then width is a sum of the widths of subtitles}
  if Sections.Count > 0 then
  begin
    Result := 0;
    for I := 0 to Sections.Count - 1 do
    begin
      S := Sections[I];
      Result := Result + S.Width;
    end;
    Exit;
  end;
  { otherwise return the width of the corresponding column}
  if (Header <> nil) and (Header.Grid <> nil) then
  begin
    I := ColumnIndex;
    if I < Header.Grid.Columns.Count then
    begin
      Result := Header.Grid.Columns[I].Width;
      Exit;
    end;
  end;
  {there is no column - its width}
  Result := FWidth;
end;

procedure TGridHeaderSection.SetAlignment(Value: TAlignment);
begin
  if Alignment <> Value then
  begin
    FAlignment := Value;
    Changed(False);
  end;
end;

procedure TGridHeaderSection.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;

procedure TGridHeaderSection.SetSections(Value: TGridHeaderSections);
begin
  Sections.Assign(Value);
end;

procedure TGridHeaderSection.SetWidth(Value: Integer);
begin
  if (Value >= 0) and (Width <> Value) then
  begin
    if (Header <> nil) and (Header.Grid <> nil) then
      if ColumnIndex > Header.Grid.Columns.Count - 1 then
        if Sections.Count > 0 then
        begin
          with Sections[Sections.Count - 1] do
            SetWidth(Width + (Value - Self.Width));
          Exit;
        end;
    FWidth := Value;
    Changed(False);
  end;
end;

procedure TGridHeaderSection.SetWordWrap(Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    Changed(False);
  end;
end;

procedure TGridHeaderSection.Assign(Source: TPersistent);
begin
  if Source is TGridHeaderSection then
  begin
    Sections  := TGridHeaderSection(Source).Sections;
    Caption   := TGridHeaderSection(Source).Caption;
    Width     := TGridHeaderSection(Source).Width;
    Alignment := TGridHeaderSection(Source).Alignment;
    WordWrap  := TGridHeaderSection(Source).WordWrap;
    Exit;
  end;
  inherited Assign(Source);
end;

{ TGridHeaderSections }

constructor TGridHeaderSections.Create(AHeader: TCustomGridHeader; AOwnerSection: TGridHeaderSection);
begin
  inherited Create(TGridHeaderSection);
  FHeader := AHeader;
  FOwnerSection := AOwnerSection;
end;

function TGridHeaderSections.GetMaxColumn: Integer;
begin
  if Count > 0 then
  begin
    Result := Sections[Count - 1].ColumnIndex;
    Exit;
  end;
  Result := 0;
end;

function TGridHeaderSections.GetMaxLevel: Integer;

  procedure DoGetMaxLevel(Sections: TGridHeaderSections);
  var
    I: Integer;
    S: TGridHeaderSection;
  begin
    for I := 0 to Sections.Count - 1 do
    begin
      S := Sections[I];
      if Result < S.Level then
        Result := S.Level;
      DoGetMaxLevel(S.Sections);
    end;
  end;

begin
  Result := 0;
  DoGetMaxLevel(Self);
end;

function TGridHeaderSections.GetSection(Index: Integer): TGridHeaderSection;
begin
  Result := TGridHeaderSection(inherited GetItem(Index));
end;

procedure TGridHeaderSections.SetSection(Index: Integer; Value: TGridHeaderSection);
begin
  inherited SetItem(Index, Value);
end;

function TGridHeaderSections.GetOwner: TPersistent;
begin
  Result := Header;
end;

procedure TGridHeaderSections.Update(Item: TCollectionItem);
begin
  if Header <> nil then
    Header.Change;
end;

function TGridHeaderSections.Add: TGridHeaderSection;
begin
  if (Header = nil) or (Header.Grid = nil) then
  begin
    Result := TGridHeaderSection(inherited Add);
    Exit;
  end;
  Result := Header.Grid.CreateHeaderSection(Self);
end;

{ TCustomGridHeader }

constructor TCustomGridHeader.Create(AGrid: TCustomGridView);
begin
  inherited Create;
  FGrid := AGrid;
  FColor := clBtnFace;
  FSections := TGridHeaderSections.Create(Self, nil);
  FSectionHeight := 17;
  FSynchronized := True;
  FAutoSynchronize := True;
  FColor := clBtnFace;
  FFont := TFont.Create;
  FFont.OnChange := FontChange;
  FGridFont := True;
  FImagesLink := TChangeLink.Create;
  FImagesLink.OnChange := ImagesChange;
  FFLat := True;
end;

destructor TCustomGridHeader.Destroy;
begin
  FOnChange := nil;
  FreeAndNil(FImagesLink);
  inherited Destroy;
  FreeAndNil(FSections);
  FreeAndNil(FFont);
end;

function TCustomGridHeader.IsColorStored: Boolean;
begin
  Result := not GridColor;
end;

function TCustomGridHeader.IsFontStored: Boolean;
begin
  Result := not GridFont;
end;

function TCustomGridHeader.IsSectionsStored: Boolean;
begin
  Result := not ((GetMaxLevel = 0) and FullSynchronizing and Synchronized);
end;

procedure TCustomGridHeader.ImagesChange(Sender: TObject);
begin
  Change;
end;

procedure TCustomGridHeader.FontChange(Sender: TObject);
begin
  FGridFont := False;
  SetSectionHeight(SectionHeight);
  Change;
end;

function TCustomGridHeader.GetHeight: Integer;
begin
  Result := (GetMaxLevel + 1) * SectionHeight;
end;

function TCustomGridHeader.GetMaxColumn: Integer;
begin
  Result := Sections.MaxColumn;
end;

function TCustomGridHeader.GetMaxLevel: Integer;
begin
  Result := Sections.MaxLevel;
end;

function TCustomGridHeader.GetWidth: Integer;
var
  I : Integer;
  S : TGridHeaderSection;
begin
  Result := 0;
  for I := 0 to Sections.Count - 1 do
  begin
    S := Sections[I];
    Result := Result + S.Width;
  end;
end;

procedure TCustomGridHeader.SetAutoHeight(Value: Boolean);
begin
  if FAutoHeight <> Value then
  begin
    FAutoHeight := Value;
    if Value then
      SetSectionHeight(SectionHeight);
  end;
end;

procedure TCustomGridHeader.SetAutoSynchronize(Value: Boolean);
begin
  if FAutoSynchronize <> Value then
  begin
    FAutoSynchronize := Value;
    if Value then
      Synchronized := True;
  end;
end;

procedure TCustomGridHeader.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    FGridColor := False;
    Change;
  end;
end;

procedure TCustomGridHeader.SetImages(Value: TImageList);
begin
  if FImages <> Value then
  begin
    if Assigned(FImages) then
      FImages.UnRegisterChanges(FImagesLink);
    FImages := Value;
    if Assigned(FImages) then
    begin
      FImages.RegisterChanges(FImagesLink);
      if Grid <> nil then
        FImages.FreeNotification(Grid);
    end;
    SetSectionHeight(SectionHeight);
    Change;
  end;
end;

procedure TCustomGridHeader.SetFlat(Value: Boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    if Value and (Grid <> nil) then
      Grid.Fixed.Flat := True;
    SetSectionHeight(SectionHeight);
    Change;
  end;
end;

procedure TCustomGridHeader.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TCustomGridHeader.SetFullSynchronizing(Value: Boolean);
begin
  if FFullSynchronizing <> Value then
  begin
    FFullSynchronizing := Value;
    if Value then
      Synchronized := False;
  end;
end;

procedure TCustomGridHeader.SetGridColor(Value: Boolean);
begin
  if FGridColor <> Value then
  begin
    FGridColor := Value;
    if Grid <> nil then
      GridColorChanged(Grid.Color);
    SetSectionHeight(SectionHeight);
    Change;
  end;
end;

procedure TCustomGridHeader.SetGridFont(Value: Boolean);
begin
  if FGridFont <> Value then
  begin
    FGridFont := Value;
    if Grid <> nil then
      GridFontChanged(Grid.Font);
    Change;
  end;
end;

procedure TCustomGridHeader.SetSections(Value: TGridHeaderSections);
begin
  FSections.Assign(Value);
  SetSynchronized(False);
end;

procedure TCustomGridHeader.SetSectionHeight(Value: Integer);
var
  TH, IH: Integer;
begin
  if AutoHeight then
  begin
    TH := GetFontHeight(Font) + 2 * 2;
    IH := 0;
    if Images <> nil then
    begin
      IH := Images.Height + 2{+ 1};
      if not GridColor then
        Inc(IH, 1);
      if not Flat then
        Inc(IH, 1);
    end;
    Value := MaxIntValue([0, TH, IH]);
  end;
  if Value < 0 then
    Value := 0;
  if FSectionHeight <> Value then
  begin
    FSectionHeight := Value;
    Change;
  end;
end;

procedure TCustomGridHeader.SetSynchronized(Value: Boolean);
begin
  if FSynchronized <> Value then
  begin
    FSynchronized := Value;
    if (Value or FAutoSynchronize) and (Grid <> nil) then
    begin
      FSynchronized := True;
      SynchronizeSections;
    end;
  end;
end;

procedure TCustomGridHeader.Change;
begin
  UpdateSections;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCustomGridHeader.GridColorChanged(NewColor: TColor);
begin
  if FGridColor then
  begin
    SetColor(NewColor);
    FGridColor := True;
  end;
end;

procedure TCustomGridHeader.GridFontChanged(NewFont: Tfont);
begin
  if FGridFont then
  begin
    SetFont(NewFont);
    FGridFont := True;
  end;
end;

procedure TCustomGridHeader.Assign(Source: TPersistent);
begin
  if Source is TCustomGridHeader then
  begin
    Sections        := TCustomGridHeader(Source).Sections;
    SectionHeight   := TCustomGridHeader(Source).SectionHeight;
    Synchronized    := TCustomGridHeader(Source).Synchronized;
    AutoSynchronize := TCustomGridHeader(Source).AutoSynchronize;
    Color           := TCustomGridHeader(Source).Color;
    GridColor       := TCustomGridHeader(Source).GridColor;
    Font            := TCustomGridHeader(Source).Font;
    GridFont        := TCustomGridHeader(Source).GridFont;
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TCustomGridHeader.SynchronizeSections;
var
  C: Integer;

  procedure DoAddSections(Column: Integer; SynchronizeCaption: Boolean);
  var
    R: TRect;
  begin
    R.Left := Grid.GetColumnLeftRight(Column).Left;
    R.Right := R.Left;
    R.Top := Grid.ClientRect.Top;
    R.Bottom := R.Top + Height;
    while Column < Grid.Columns.Count do
    begin
      R.Left := R.Right;
      R.Right := R.Left + Grid.Columns[Column].Width;
      with Sections.Add do
      begin
        FColumnIndex := Column;
        FBoundsRect := R;
        Width := Grid.Columns[Column].Width;
        if SynchronizeCaption then
        begin
          Caption := Grid.Columns[Column].Caption;
          Alignment := Grid.Columns[Column].Alignment
        end;
      end;
      Inc(Column);
    end;
  end;

  procedure DoDeleteSections(Sections: TGridHeaderSections);
  var
    I: Integer;
    S: TGridHeaderSection;
  begin
    for I := Sections.Count - 1 downto 0 do
    begin
      S := Sections[I];
      DoDeleteSections(S.Sections);
      if (S.Sections.Count = 0) and (S.ColumnIndex > Grid.Columns.Count - 1) then
        S.Free;
    end;
  end;

  procedure DoSynchronizeSections(Sections: TGridHeaderSections);
  var
    I: Integer;
    S: TGridHeaderSection;
  begin
    for I := Sections.Count - 1 downto 0 do
    begin
      S := Sections[I];
      if S.Sections.Count = 0 then
      begin
        C := S.ColumnIndex;
        S.Width := Grid.Columns[C].Width;
        if FullSynchronizing then
        begin
          S.Caption := Grid.Columns[C].Caption;
          S.Alignment := Grid.Columns[C].Alignment;
        end;
      end
      else
        DoSynchronizeSections(S.Sections);
    end;
  end;

begin
  {before the synchronization it is necessary to renew the internal parameters of sections}
  UpdateSections;
  { synchronize sections}
  if (Grid <> nil) and (Grid.ComponentState * [csReading, csLoading] = [])
    and (Grid.Columns <> nil) then
  begin
    Sections.BeginUpdate;
    try
      {title is empty - we add all columns}
      if Sections.Count = 0 then
      begin
        DoAddSections(0, True);
        Exit;
      end;
      {if sections it is less - we add, otherwise move away excess}
      C := Sections[Sections.Count - 1].ColumnIndex;
      if C < Grid.Columns.Count - 1 then
        DoAddSections(C + 1, False)
      else if C > Grid.Columns.Count - 1 then
        DoDeleteSections(Sections);
      {in lower sections we synchronize title, levelling off and shinrinu}
      DoSynchronizeSections(Sections);
    finally
      Sections.EndUpdate;
    end;
  end;
end;

procedure TCustomGridHeader.UpdateSections;
var
  R: TRect;
  C: Integer;

  procedure DoUpdateColumnIndex(Sections: TGridHeaderSections);
  var
    I: Integer;
    S: TGridHeaderSection;
  begin
    for I := 0 to Sections.Count - 1 do
    begin
      S := Sections[I];
      {there are whether subtitles}
      if S.Sections.Count = 0 then
      begin
        {this is lower section}
        S.FColumnIndex := C;
        Inc(C);
      end
      else
      begin
        {recursion to all subtitles from below}
        DoUpdateColumnIndex(S.Sections);
        {index is an index of the latter}
        S.FColumnIndex := S.Sections[S.Sections.Count - 1].FColumnIndex;
      end;
    end;
  end;

  procedure DoUpdateSecionsBounds(Sections: TGridHeaderSections; Rect: TRect);
  var
    I: Integer;
    S: TGridHeaderSection;
    R, SR: TRect;
  begin
    R := Rect;
    R.Right := R.Left;
    { sort out subtitles}
    for I := 0 to Sections.Count - 1 do
    begin
      S := Sections[I];
      R.Left := R.Right;
      R.Right := R.Left + S.Width;
      {rectangle}
      SR := R;
      if S.Sections.Count > 0 then
        SR.Bottom := R.Top + SectionHeight;
      { memorize}
      S.FBoundsRect := SR;
      {subtitles}
      if S.Sections.Count > 0 then
      begin
        {it is subtracted line from above}
        SR.Top := SR.Bottom;
        SR.Bottom := R.Bottom;
        {subtitles from below}
        DoUpdateSecionsBounds(S.Sections, SR);
      end;
    end;
  end;

begin
  if (Grid <> nil) and (Grid.ComponentState * [csReading, csLoading] = [])
    and (Grid.Columns <> nil) then
  begin
    {opredelyaei the indices of columns}
    C := 0;
    DoUpdateColumnIndex(Sections);
    {the absolute boundaries of title}
    R.Left := Grid.ClientRect.Left;
    R.Right := R.Left + Grid.GetColumnsWidth(0, Grid.Columns.Count - 1);
    R.Top := Grid.ClientRect.Top;
    R.Bottom := R.Top + Height;
    { define the boundaries of sections}
    DoUpdateSecionsBounds(Sections, R);
  end;
end;

{ TCustomGridColumn }

constructor TCustomGridColumn.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FColumns := TGridColumns(Collection);
  FWidth := 64;
  FMinWidth := 0;
  FMaxWidth := 10000;
  FAlignment := taLeftJustify;
  FTabStop := True;
  FVisible := True;
  FAllowClick := True;
  FAllowEdit := True;
  FCheckAlignment := taLeftJustify;
end;

destructor TCustomGridColumn.Destroy;
begin
  inherited;
  FreeAndNil(FPickList);
end;

function TCustomGridColumn.GetWidth: Integer;
begin
  if not FVisible then
  begin
    Result := 0;
    Exit;
  end;
  Result := FWidth;
end;

function TCustomGridColumn.GetEditAlignment: TAlignment;
begin
  if AlignEdit then
    Result := Alignment
  else
    Result := taLeftJustify;
end;

function TCustomGridColumn.GetGrid: TCustomGridView;
begin
  Result := nil;
  if Columns <> nil then
    Result := TCustomGridView(Columns.Grid);
end;

function TCustomGridColumn.GetPickList: TStrings;
begin
  if FPickList = nil then
    FPickList := TStringList.Create;
  Result := FPickList;
end;

function TCustomGridColumn.GetPickListCount: Integer;
begin
  Result := 0;
  if FPickList <> nil then
    Result := FPickList.Count;
end;

function TCustomGridColumn.GetHeaderSection: TGridHeaderSection;
begin
  Result := nil;
  if Grid <> nil then
    Result := Grid.GetHeaderSection(Index, -1);
end;

function TCustomGridColumn.IsPickListStored: Boolean;
begin
  Result := GetPickListCount <> 0;
end;

procedure TCustomGridColumn.SetAlignEdit(Value: Boolean);
begin
  if FAlignEdit <> Value then
  begin
    FAlignEdit := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetAllowEdit(Value: Boolean);
begin
  if AllowEdit <> Value then
  begin
    FAllowEdit := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetCheckAlignment(Value: TAlignment);
begin
  if FCheckAlignment <> Value then
  begin
    FCheckAlignment := Value;
    Changed(False);
  end;
end;
procedure TCustomGridColumn.SetCheckKind(Value: TGridCheckKind);
begin
  if FCheckKind <> Value then
  begin
    FCheckKind := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetEditWordWrap(Value: TGridEditWordWrap);
begin
  if FEditWordWrap <> Value then
  begin
    FEditWordWrap := Value;
    Changed(False);
  end;
end;
procedure TCustomGridColumn.SetMaxWidth(Value: Integer);
begin
  if Value < FMinWidth then
    Value := FMinWidth;
  if Value > 10000 then
    Value := 10000;
  FMaxWidth := Value;
  SetWidth(FWidth);
end;

procedure TCustomGridColumn.SetMinWidth(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value > FMaxWidth then
    Value := FMaxWidth;
  FMinWidth := Value;
  SetWidth(FWidth);
end;

procedure TCustomGridColumn.SetPickList(Value: TStrings);
begin
  if Value = nil then
  begin
    FreeAndNil(FPickList);
    Exit;
  end;
  PickList.Assign(Value);
end;

procedure TCustomGridColumn.SetTabStop(Value: Boolean);
begin
  if FTabStop <> Value then
  begin
    FTabStop := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetWantReturns(Value: Boolean);
begin
  if FWantReturns <> Value then
  begin
    FWantReturns := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetWordWrap(Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    Changed(False);
  end;
end;

function TCustomGridColumn.GetDisplayName: string;
begin
  Result := FCaption;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TCustomGridColumn.SetAlignment(Value: TAlignment);
begin
  if Alignment <> Value then
  begin
    FAlignment := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetEditMask(const Value: string);
begin
  if FEditMask <> Value then
  begin
    FEditMask := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetEditStyle(Value: TGridEditStyle);
begin
  if FEditStyle <> Value then
  begin
    FEditStyle := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetMaxLength(Value: Integer);
begin
  if FMaxLength <> Value then
  begin
    FMaxLength := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetReadOnly(Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed(True);
  end;
end;

procedure TCustomGridColumn.SetWidth(Value: Integer);
begin
  if Value < FMinWidth then
    Value := FMinWidth;
  if Value > FMaxWidth then
    Value := FMaxWidth;
  if FWidth <> Value then
  begin
    FWidth := Value;
    Changed(True);
  end;
end;

procedure TCustomGridColumn.Assign(Source: TPersistent);
begin
  if Source is TCustomGridColumn then
  begin
    Caption        := TCustomGridColumn(Source).Caption;
    DefWidth       := TCustomGridColumn(Source).DefWidth;
    MinWidth       := TCustomGridColumn(Source).MinWidth;
    MaxWidth       := TCustomGridColumn(Source).MaxWidth;
    FixedSize      := TCustomGridColumn(Source).FixedSize;
    MaxLength      := TCustomGridColumn(Source).MaxLength;
    Alignment      := TCustomGridColumn(Source).Alignment;
    ReadOnly       := TCustomGridColumn(Source).ReadOnly;
    EditStyle      := TCustomGridColumn(Source).EditStyle;
    EditMask       := TCustomGridColumn(Source).EditMask;
    CheckKind      := TCustomGridColumn(Source).CheckKind;
    CheckAlignment := TCustomGridColumn(Source).CheckAlignment;
    WantReturns    := TCustomGridColumn(Source).WantReturns;
    WordWrap       := TCustomGridColumn(Source).WordWrap;
    TabStop        := TCustomGridColumn(Source).TabStop;
    Visible        := TCustomGridColumn(Source).Visible;
    PickList       := TCustomGridColumn(Source).FPickList;
    Tag            := TCustomGridColumn(Source).Tag;
    AllowClick     := TCustomGridColumn(Source).AllowClick;
    AllowEdit      := TCustomGridColumn(Source).AllowEdit;
    Exit;
  end;
  inherited Assign(Source);
end;

{ TGridColumns }

constructor TGridColumns.Create(AGrid: TCustomGridView);
var
  AClass: TGridColumnClass;
begin
  AClass := TGridColumn;
  if AGrid <> nil then
    AClass := AGrid.GetColumnClass;
  inherited Create(AClass);
  FGrid := AGrid;
end;

function TGridColumns.GetColumn(Index: Integer): TGridColumn;
begin
  Result := TGridColumn(inherited GetItem(Index));
end;

function TGridColumns.GetLayout: string;
var
  I, W: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    { add in the list of the width of columns}
    for I := 0 to Count - 1 do
    begin
      {the negative value of width sootvestvuyet to invisible column}
      W := Columns[I].DefWidth;
      if not Columns[I].Visible then
        W := W * (-1);
      { add in the list}
      Strings.Add(IntToStr(W));
    end;
    {result - widths of columns, divided by comma}
    Result := ExpandStrings(Strings, ',')
  finally
    Strings.Free;
  end;
end;

procedure TGridColumns.SetColumn(Index: Integer; Value: TGridColumn);
begin
  inherited SetItem(Index, Value);
end;

procedure TGridColumns.SetLayout(const Value: string);
var
  I, W: Integer;
  Strings: TStringList;
begin
  BeginUpdate;
  try
    Strings := TStringList.Create;
    try
      { divide the widths of columns, divided by comma, into the lines}
      ExtractStrings([','], [' '], PChar(Value), Strings);
      { change the widths of columns}
      for I := 0 to Strings.Count - 1 do
      begin
        { check a quantity of columns}
        if I > Count - 1 then
          Break;
        { obtain width}
        W := StrToIntDef(Strings[I], Columns[I].DefWidth);
        {the negative value of width sootvestvuyet to invisible column}
        Columns[I].DefWidth := Abs(W);
        Columns[I].Visible := W > 0;
      end;
    finally
      Strings.Free;
    end;
  finally
    EndUpdate;
  end;
end;

function TGridColumns.GetOwner: TPersistent;
begin
  Result := FGrid;
end;

procedure TGridColumns.Update(Item: TCollectionItem);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TGridColumns.Add: TGridColumn;
begin
  if Grid = nil then
  begin
    Result := TGridColumn(inherited Add);
    Exit;
  end;
  Result := TGridColumn(Grid.CreateColumn(Self));
end;

{ TCustomGridRows }

constructor TCustomGridRows.Create(AGrid: TCustomGridView);
begin
  inherited Create;
  FGrid := AGrid;
  FHeight := 17;
end;

destructor TCustomGridRows.Destroy;
begin
  FOnChange := nil;
  SetCount(0);
  inherited Destroy;
end;

function TCustomGridRows.GetMaxCount: Integer;
begin
  Result := MaxInt - 2;
  if Height > 0 then
    Result := Result div Height - 2;
end;

procedure TCustomGridRows.SetAutoHeight(Value: Boolean);
begin
  if FAutoHeight <> Value then
  begin
    FAutoHeight := Value;
    if Value then
      SetHeight(Height);
  end;
end;

procedure TCustomGridRows.SetHeight(Value: Integer);
var
  TH, FH, CH, IH, GH: Integer;
begin
  if AutoHeight and (Grid <> nil) then
  begin
    {the height of text}
    TH := GetFontHeight(Grid.Font) + Grid.TextTopIndent * 2;
    FH := GetFontHeight(Grid.Fixed.Font) + Grid.TextTopIndent * 2;
    CH := 0;
    if Grid.CheckBoxes then
    begin
      CH := Grid.CheckHeight + Grid.CheckTopIndent{+ 1};
      if Grid.Fixed.Count > 0 then
      begin
        if not Grid.Fixed.Flat then
          Inc(CH, 3)
        else if (not Grid.Fixed.GridColor) and Grid.GridLines and (gsHorzLine in Grid.GridStyle) then
          Inc(CH, 1);
      end
      else if Grid.GridLines and (gsHorzLine in Grid.GridStyle) then
        Inc(CH, 1);
    end;
    {the height of picture}
    IH := 0;
    if Grid.Images <> nil then
    begin
      IH := Grid.Images.Height + Grid.ImageTopIndent{+ 1};
      if Grid.Fixed.Count > 0 then
      begin
        if not Grid.Fixed.Flat then
          Inc(IH, 3)
        else if (not Grid.Fixed.GridColor) and Grid.GridLines and (gsHorzLine in Grid.GridStyle) then
          Inc(IH, 1);
      end
      else if Grid.GridLines and (gsHorzLine in Grid.GridStyle) then
        Inc(IH, 1);
    end;
    GH := (Grid.FGridLineWidth * 2) * Ord(Grid.GridLines and (gsHorzLine in Grid.GridStyle));
    Value := MaxIntValue([0, TH, FH, CH, IH, GH]);
  end;
  if Value < 0 then
    Value := 0;
  if FHeight <> Value then
  begin
    FHeight := Value;
    if Count > MaxCount then
      SetCount(Count)
    else
      Change;
  end;
end;

procedure TCustomGridRows.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCustomGridRows.SetCount(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value > MaxCount then
    Value := MaxCount;
  if FCount <> Value then
  begin
    FCount := Value;
    Change;
  end;
end;

procedure TCustomGridRows.Assign(Source: TPersistent);
begin
  if Source is TCustomGridRows then
  begin
    Count  := TCustomGridRows(Source).Count;
    Height := TCustomGridRows(Source).Height;
    Exit;
  end;
  inherited Assign(Source);
end;

{ TCustomGridFixed }

constructor TCustomGridFixed.Create(AGrid: TCustomGridView);
begin
  inherited Create;
  FGrid := AGrid;
  FColor := clBtnFace;
  FFont := TFont.Create;
  FFont.OnChange := FontChange;
  FGridFont := True;
  FFLat := True;
  FShowDivider := True;
end;

destructor TCustomGridFixed.Destroy;
begin
  FOnChange := nil;
  inherited Destroy;
  FreeandNil(FFont);
end;

function TCustomGridFixed.IsColorStored: Boolean;
begin
  Result := not GridColor;
end;

function TCustomGridFixed.IsFontStored: Boolean;
begin
  Result := not GridFont;
end;

procedure TCustomGridFixed.FontChange(Sender: TObject);
begin
  FGridFont := False;
  Change;
end;

procedure TCustomGridFixed.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    FGridColor := False;
    Change;
  end;
end;

procedure TCustomGridFixed.SetFlat(Value: Boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    if (not Value) and (Grid <> nil) then
      Grid.Header.Flat := False;
    Change;
  end;
end;

procedure TCustomGridFixed.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TCustomGridFixed.SetGridColor(Value: Boolean);
begin
  if FGridColor <> Value then
  begin
    FGridColor := Value;
    if Grid <> nil then
      GridColorChanged(Grid.Color);
    Change;
  end;
end;

procedure TCustomGridFixed.SetGridFont(Value: Boolean);
begin
  if FGridFont <> Value then
  begin
    FGridFont := Value;
    if Grid <> nil then
      GridFontChanged(Grid.Font);
    Change;
  end;
end;

procedure TCustomGridFixed.SetShowDivider(Value: Boolean);
begin
  if FShowDivider <> Value then
  begin
    FShowDivider := Value;
    Change;
  end;
end;

procedure TCustomGridFixed.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCustomGridFixed.GridColorChanged(NewColor: TColor);
begin
  if FGridColor then
  begin
    SetColor(NewColor);
    FGridColor := True;
  end;
end;

procedure TCustomGridFixed.GridFontChanged(NewFont: TFont);
begin
  if FGridFont then
  begin
    SetFont(NewFont);
    FGridFont := True;
  end;
end;

procedure TCustomGridFixed.SetCount(Value: Integer);
begin
  if (Grid <> nil) and (Value > Grid.Columns.Count - 1) then
    Value := Grid.Columns.Count - 1;
  if Value < 0 then
    Value := 0;
  if FCount <> Value then
  begin
    FCount := Value;
    Change;
  end;
end;

procedure TCustomGridFixed.Assign(Source: TPersistent);
begin
  if Source is TCustomGridFixed then
  begin
    Count     := TCustomGridFixed(Source).Count;
    Color     := TCustomGridFixed(Source).Color;
    GridColor := TCustomGridFixed(Source).GridColor;
    Font      := TCustomGridFixed(Source).Font;
    GridFont  := TCustomGridFixed(Source).GridFont;
  end
  else
    inherited Assign(Source);
end;

{ TGridScrollBar }

constructor TGridScrollBar.Create(AGrid: TCustomGridView; AKind: TScrollBarKind);
begin
  inherited Create;
  FGrid     := AGrid;
  FKind     := AKind;
  FPageStep := 100;
  FLineStep := 8;
  FLineSize := 1;
  FTracking := True;
  FVisible  := True;
end;

function TGridScrollBar.GetRange: Integer;
begin
  Result := Max - Min;
end;

procedure TGridScrollBar.SetLineSize(Value: Integer);
begin
  if Value < 1 then
    FLineSize := 1
  else
    FLineSize := Value;
end;

procedure TGridScrollBar.SetLineStep(Value: Integer);
begin
  SetParams(Min, Max, PageStep, Value);
end;

procedure TGridScrollBar.SetMax(Value: Integer);
begin
  SetParams(Min, Value, PageStep, LineStep);
end;

procedure TGridScrollBar.SetMin(Value: Integer);
begin
  SetParams(Value, Max, PageStep, LineStep);
end;

procedure TGridScrollBar.SetPageStep(Value: Integer);
begin
  SetParams(Min, Max, Value, LineStep);
end;

procedure TGridScrollBar.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Update;
  end;
end;

procedure TGridScrollBar.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TGridScrollBar.ChangeParams;
begin
  if Assigned(FOnChangeParams) then
    FOnChangeParams(Self);
end;

procedure TGridScrollBar.Scroll(ScrollCode: Integer; var ScrollPos: Integer);
begin
  if Assigned(FOnScroll) then
    FOnScroll(Self, ScrollCode, ScrollPos);
end;

procedure TGridScrollBar.ScrollMessage(var Message: TWMScroll);
begin
  with Message do
  begin
    LockUpdate;
    try
      case ScrollCode of
        SB_LINELEFT      : SetPositionEx(Position - LineStep, ScrollCode);
        SB_LINERIGHT     : SetPositionEx(Position + LineStep, ScrollCode);
        SB_PAGELEFT      : SetPositionEx(Position - PageStep, ScrollCode);
        SB_PAGERIGHT     : SetPositionEx(Position + PageStep, ScrollCode);
        SB_THUMBPOSITION : SetPositionEx(WinPosToScrollPos(Pos, Min, Max),
                                         ScrollCode);
        SB_THUMBTRACK    : if Tracking then
          SetPositionEx(WinPosToScrollPos(Pos, Min, Max), ScrollCode);
        SB_ENDSCROLL     : SetPositionEx(Position, ScrollCode);
      end;
    finally
      UnLockUpdate;
    end;
  end;
end;

procedure TGridScrollBar.SetParams(AMin, AMax, APageStep, ALineStep: Integer);
begin
  if APageStep < 0 then
    APageStep := 0;
  if ALineStep < 0 then
    ALineStep := 0;
  if AMax < AMin then
    AMax := AMin;
  if (FMin <> AMin) or (FMax <> AMax) or (FPageStep <> APageStep) or (FLineStep <> ALineStep) then
  begin
    FMin := AMin;
    FMax := AMax;
    FPageStep := APageStep;
    FLineStep := ALineStep;
    if FPosition > Range - FPageStep then
      FPosition := Range - FPageStep;
    if FPosition < 0 then
      FPosition := 0;
    Update;
    ChangeParams;
  end;
end;

procedure TGridScrollBar.SetPosition(Value: Integer);
begin
  SetPositionEx(Value, SB_ENDSCROLL);
end;

procedure TGridScrollBar.SetPositionEx(Value: Integer; ScrollCode: Integer);
var
  R: TRect;

  procedure UpdatePosition;
  begin
    if Value > Max - PageStep then
      Value := Max - PageStep;
    if Value < Min then
      Value := Min;
  end;

begin
  UpdatePosition;
  if Value <> FPosition then
  begin
    Scroll(ScrollCode, Value);
    UpdatePosition;
  end;
  if Value <> FPosition then
  begin
    with FGrid do
    begin
      HideFocus;
      if FKind = sbHorizontal then
      begin
        R := GetClientRect;
        R.Left := GetFixedRect.Right;
        ScrollWindowEx(Handle, (FPosition - Value) * FLineSize, 0, @R, @R, 0, nil, SW_INVALIDATE);
      end
      else
      begin
        R := GetGridRect;
        ScrollWindowEx(Handle, 0, (FPosition - Value) * FLineSize, @R, @R, 0, nil, SW_INVALIDATE);
      end;
      FPosition :=  Value;
      ShowFocus;
    end;
    Update;
    Change;
  end;
end;

procedure TGridScrollBar.Update;
var
  ScrollInfo: TScrollInfo;
  ScrollCode: Integer;
begin
  if FGrid.HandleAllocated and (FUpdateLock = 0) then
  begin
    FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.fMask := SIF_ALL;
    ScrollInfo.nMin := 0;
    ScrollInfo.nMax := MaxWinPos * Ord(Visible and (Range > PageStep));
    ScrollInfo.nPage := ScrollPosToWinPos(Min + PageStep, Min, Max);
    ScrollInfo.nPos := ScrollPosToWinPos(Position, Min, Max);
    ScrollCode := SB_VERT;
    if Kind = sbHorizontal then
      ScrollCode := SB_HORZ;
    SetScrollInfo(FGrid.Handle, ScrollCode, ScrollInfo, True);
  end;
end;

procedure TGridScrollBar.Assign(Source: TPersistent);
begin
  if Source is TGridScrollBar then
  begin
    LockUpdate;
    try
      PageStep := TGridScrollBar(Source).PageStep;
      LineStep := TGridScrollBar(Source).LineStep;
      Min      := TGridScrollBar(Source).Min;
      Max      := TGridScrollBar(Source).Max;
      Position := TGridScrollBar(Source).Position;
      Tracking := TGridScrollBar(Source).Tracking;
      Visible  := TGridScrollBar(Source).Visible;
      Exit;
    finally
      UnLockUpdate;
    end;
  end;
  inherited Assign(Source);
end;

procedure TGridScrollBar.LockUpdate;
begin
  Inc(FUpdateLock);
end;

procedure TGridScrollBar.UnLockUpdate;
begin
  Dec(FUpdateLock);
  if FUpdateLock = 0 then
    Update;
end;

{ TGridListBox }

constructor TGridListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ParentShowHint := False;
  ShowHint := False;
end;

procedure TGridListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_BORDER;
    ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
    WindowClass.Style := CS_SAVEBITS;
  end;
end;

procedure TGridListBox.CreateWnd;
begin
  inherited CreateWnd;
  Winapi.Windows.SetParent(Handle, 0);
  CallWindowProc(DefWndProc, Handle, WM_SETFOCUS, 0, 0);
end;

procedure TGridListBox.Keypress(var Key: Char);
var
  TickCount: Integer;
begin
  case Key of
    #8, #27:
      { discard the text of search}
      FSearchText := '';
    #32..#255:
      { initiate search}
      begin
        TickCount := Longint(GetTickCount);
        if Abs(TickCount - FSearchTime) > 2000 then
          FSearchText := '';
        FSearchTime := TickCount;
        if Length(FSearchText) < 32 then
          FSearchText := FSearchText + Key;
        SendMessage(Handle, LB_SELECTSTRING, WORD(-1), Longint(PChar(FSearchText)));
        Key := #0;
      end;
  end;
  inherited Keypress(Key);
end;

procedure TGridListBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if (Grid <> nil) and (Grid.Edit <> nil) then
    Grid.Edit.CloseUp((X >= 0) and (Y >= 0) and (X < Width) and (Y < Height));
end;

{ TCustomGridEdit }

constructor TCustomGridEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {internal variables}
  FEditStyle := geSimple;
  FDropDownCount := 8;
  FButtonWidth := GetSystemMetrics(SM_CXVSCROLL);
  {the parameters of exterior view}
  ParentCtl3D := False;
  Ctl3D := False;
  TabStop := False;
  BorderStyle := bsNone;
  ParentShowHint := False;
  ShowHint := False;
end;

function TCustomGridEdit.GetButtonRect: TRect;
begin
  Result := Rect(Width - FButtonWidth, 0, Width, Height);
end;

function TCustomGridEdit.GetLineCount: Integer;
begin
  Result := GetTextLineCount(Text);
end;

function TCustomGridEdit.GetVisible: Boolean;
begin
  Result := IsWindowVisible(Handle);
end;

procedure TCustomGridEdit.ListMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (FActiveList <> nil) then
    CloseUp(PtInRect(FActiveList.ClientRect, Point(X, Y)));
end;

procedure TCustomGridEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

procedure TCustomGridEdit.SetButtonWidth(Value: Integer);
begin
  if FButtonWidth <> Value then
  begin
    FButtonWidth := Value;
    Repaint;
  end;
end;

procedure TCustomGridEdit.SetDropListVisible(Value: Boolean);
begin
  if Value then
    DropDown
  else
    CloseUp(False);
end;

procedure TCustomGridEdit.SetEditStyle(Value: TGridEditStyle);
begin
  if FEditStyle <> Value then
  begin
    FEditStyle := Value;
    Repaint;
  end;
end;

procedure TCustomGridEdit.SetWordWrap(Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    RecreateWnd;
  end;
end;

procedure TCustomGridEdit.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  with Message do
  begin
    Result := Result or DLGC_WANTARROWS or DLGC_WANTCHARS;
    if (Grid <> nil) and (gkTabs in Grid.CursorKeys) then
      Result := Result or DLGC_WANTTAB;
  end;
end;

procedure TCustomGridEdit.WMCancelMode(var Message: TMessage);
begin
  StopButtonTracking;
  inherited;
end;

procedure TCustomGridEdit.WMKillFocus(var Message: TMessage);
begin
  inherited;
  CloseUp(False);
end;

procedure TCustomGridEdit.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  Invalidate;
end;

procedure TCustomGridEdit.WMPaint(var Message: TWMPaint);
begin
  PaintHandler(Message);
end;

procedure TCustomGridEdit.WMLButtonDown(var Message: TWMLButtonDown);
begin
  SendCancelMode(Self);
  with Message do
    {in order to the isolation of text not gaslo with the pressure on the button, it is processed
      pressure themselves}
    if (EditStyle <> geSimple) and PtInrect(ButtonRect, Point(XPos, YPos)) then
    begin
      if csCaptureMouse in ControlStyle then
        MouseCapture := True;
      MouseDown(mbLeft, KeysToShiftState(Keys), XPos, YPos);
    end
    else
    begin
      CloseUp(False);
      inherited;
    end;
end;

procedure TCustomGridEdit.WMLButtonDblClk(var Message: TWMLButtonDblClk);
var
  P: TPoint;
begin
  with Message do
  begin
    P := Point(XPos, YPos);
    if (FEditStyle <> geSimple) and PtInRect(GetButtonRect, P) then
      Exit;
  end;
  inherited;
end;

procedure TCustomGridEdit.WMSetCursor(var Message: TWMSetCursor);
var
  P: TPoint;
begin
  GetCursorPos(P);
  if (FEditStyle <> geSimple) and PtInRect(GetButtonRect, ScreenToClient(P)) then
  begin
    Winapi.Windows.SetCursor(LoadCursor(0, IDC_ARROW));
    Exit;
  end;
  inherited;
end;

procedure TCustomGridEdit.WMPaste(var Message);
begin
  if EditCanModify then
    inherited;
end;

procedure TCustomGridEdit.WMCut(var Message);
begin
  if EditCanModify then
    inherited;
end;

procedure TCustomGridEdit.WMClear(var Message);
begin
  if EditCanModify then
    inherited;
end;

procedure TCustomGridEdit.WMUndo(var Message);
begin
  if (Grid = nil) or Grid.EditCanUndo(Grid.EditCell) then
    inherited;
end;

procedure TCustomGridEdit.CMCancelMode(var Message: TCMCancelMode);
begin
  if (Message.Sender <> Self) and (Message.Sender <> FActiveList) then
    CloseUp(False);
end;

procedure TCustomGridEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TCustomGridEdit.CMShowingChanged(var Message: TMessage);
begin
  { ignore a change in the visibility through a change in property Visible}
end;

procedure TCustomGridEdit.WMContextMenu(var Message: TMessage);
begin
  {if property DefaultPopupMenu of table is established in True, then on the the right
    to button it is necessary to show the standard popup menu of the line of introduction, but not Popup
    the menu of table}
  if (Grid <> nil) and Grid.DefaultEditMenu and Assigned(Grid.PopupMenu) then
    with Message do
      Result := CallWindowProc(DefWndProc, Handle, Msg, WParam, LParam)
  else
    inherited;
end;

procedure TCustomGridEdit.Change;
begin
  if Grid <> nil then
    Grid.EditChange(Grid.EditCell);
end;

procedure TCustomGridEdit.CreateParams(var Params: TCreateParams);
const
  WordWraps: array[Boolean] of DWORD = (0, ES_AUTOHSCROLL);
  Aligns: array[TAlignment] of DWORD = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);
  with Params do
    Style := Style and (not WordWraps[FWordWrap]) or ES_MULTILINE or
      Aligns[FAlignment];
end;

procedure TCustomGridEdit.DblClick;
begin
  if Grid <> nil then
    Grid.DblClick;

  case EditStyle of
    geEllipsis: Press;
    gePickList, geDataList:
      if not FDropListVisible then
        SelectNext;
  end;
end;

function TCustomGridEdit.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := (Grid <> nil) and Grid.DoMouseWheel(Shift, WheelDelta, MousePos);
end;

function TCustomGridEdit.EditCanModify: Boolean;
begin
  Result := (Grid <> nil) and Grid.EditCanModify(Grid.EditCell);
end;

function TCustomGridEdit.GetDropList: TWinControl;
begin
  if FPickList = nil then
  begin
    FPickList := TGridListBox.Create(Self);
    FPickList.IntegralHeight := True;
    FPickList.FGrid := Grid;
  end;
  Result := FPickList;
end;

procedure TCustomGridEdit.KeyDown(var Key: Word; Shift: TShiftState);

  procedure SendToParent;
  begin
    if Grid <> nil then
    begin
      Grid.KeyDown(Key, Shift);
      Key := 0;
    end;
  end;

  procedure ParentEvent;
  var
    GridKeyDown: TKeyEvent;
  begin
    if Grid <> nil then
    begin
      GridKeyDown := Grid.OnKeyDown;
      if Assigned(GridKeyDown) then
        GridKeyDown(Grid, Key, Shift);
    end;
  end;

begin
  {it is processed pressure}
  case Key of
    VK_UP, VK_DOWN:
      {the displacement of focus}
      if (Shift = [ssCtrl]) or ((Shift = []) and (not (WantReturns or WordWrap))) then
        SendToParent;
    VK_PRIOR, VK_NEXT:
      {the displacement of focus}
      if Shift = [ssCtrl] then
        SendToParent;
    VK_ESCAPE:
      {cancellation}
      SendToParent;
    VK_DELETE:
      {removal}
      if not EditCanModify then
        SendToParent;
    VK_INSERT:
      {insert}
      if (not EditCanModify) or (Shift = []) then
        SendToParent;
(*
    VK_LEFT, VK_RIGHT, VK_HOME, VK_END:
      {the displacement of focus with that pressed Ctrl}
      if Shift = [ssCtrl] then
        SendToParent;
*)

    VK_TAB:
      {tabulation}
      if not (ssAlt in Shift) then
        SendToParent;
  end;
  {button is not processed - event}
  if Key <> 0 then
  begin
    ParentEvent;
    inherited KeyDown(Key, Shift);
  end;
end;

procedure TCustomGridEdit.KeyPress(var Key: Char);
begin
  if Grid <> nil then
  begin
    Grid.KeyPress(Key);
    if CharInSet(Key, [#32..#255])
      and not Grid.EditCanAcceptKey(Grid.EditCell, Key) then
    begin
      Key := #0;
      MessageBeep(0);
    end;
    case Key of
      #9, #27, #13: // bugfix TS : added #13
        {TAB, ESC we remove }
        Key := #0;
      ^H, ^V, ^X, #32..#255:
        {BACKSPACE, usual symbols we remove, if it cannot be edited}
        if not EditCanModify then
          Key := #0;
    end;
  end;
  if Key <> #0 then
    inherited KeyPress(Key);
end;

procedure TCustomGridEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if Grid <> nil then
    Grid.KeyUp(Key, Shift);
end;

procedure TCustomGridEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (EditStyle <> geSimple) and PtInrect(ButtonRect, Point(X, Y)) then
  begin
    if FDropListVisible then
      CloseUp(False)
    else
    begin
      StartButtonTracking(X, Y);
      if EditStyle <> geEllipsis then
        DropDown;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomGridEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  M: TSmallPoint;
begin
  if FButtonTracking then
  begin
    StepButtonTracking(X, Y);
    if FDropListVisible then
    begin
      P := FActiveList.ScreenToClient(ClientToScreen(Point(X, Y)));
      if PtInRect(FActiveList.ClientRect, P) then
      begin
        StopButtonTracking;
        M := PointToSmallPoint(P);
        SendMessage(FActiveList.Handle, WM_LBUTTONDOWN, 0, Integer(M));
        Exit;
      end;
    end;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TCustomGridEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: Boolean;
begin
  P := FButtonPressed;
  StopButtonTracking;
  if (Button = mbLeft) and (EditStyle = geEllipsis) and P then
    Press;
  inherited MouseUp(Button, Shift, X, Y);
end;

{TODO: Theming support}

procedure TCustomGridEdit.PaintButton(DC: HDC);
var
  R: TRect;
begin
  if EditStyle <> geSimple then
  begin
    R := GetButtonRect;
    case EditStyle of
      geEllipsis:
        PaintBtnEllipsis(DC, R, FButtonPressed);
      gePickList, geDataList:
        PaintBtnComboBox2(DC, R, FButtonPressed);
    end;
  end;
end;

procedure TCustomGridEdit.PaintWindow(DC: HDC);
var
  R: TRect;
begin
  PaintButton(DC);
  { remove the rectangle of button from the region of painting}
  if (EditStyle <> geSimple) then
  begin
    R := GetButtonRect;
    ExcludeClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
  end;
  inherited;
end;

procedure TCustomGridEdit.StartButtonTracking(X, Y: Integer);
begin
  MouseCapture := True;
  FButtonTracking := True;
  StepButtonTracking(X, Y);
end;

procedure TCustomGridEdit.StepButtonTracking(X, Y: Integer);
var
  R: TRect;
  P: Boolean;
begin
  R := GetButtonRect;
  P := PtInRect(R, Point(X, Y));
  if FButtonPressed <> P then
  begin
    FButtonPressed := P;
    InvalidateRect(Handle, @R, False);
  end;
end;

procedure TCustomGridEdit.StopButtonTracking;
begin
  if FButtonTracking then
  begin
    StepButtonTracking(-1, -1);
    FButtonTracking := False;
    MouseCapture := False;
  end;
end;

type
  THackWinControl = class(TWinControl);

procedure TCustomGridEdit.UpdateBounds(ScrollCaret: Boolean);
const
  Flags = SWP_SHOWWINDOW or SWP_NOREDRAW;
var
  R, F: TRect;
  L, T, W, H: Integer;
  TI: TPoint;
begin
  if Grid <> nil then
  begin
    R := Grid.GetEditRect(Grid.EditCell);
    F := R;
    with Grid.GetFixedRect do
    begin
      if R.Left < Right then
        R.Left := Right;
      if R.Right < Right then
        R.Right := Right;
    end;
    with Grid.GetHeaderRect do
    begin
      if R.Top < Bottom then
        R.Top := Bottom;
      if R.Bottom < Bottom then
        R.Bottom := Bottom;
    end;
    W := R.Right - R.Left;
    H := R.Bottom - R.Top;
    SetWindowPos(Handle, HWND_TOP, R.Left, R.Top, W, H, Flags);
    L := F.Left - R.Left;
    T := F.Top - R.Top;
    W := F.Right - F.Left;
    H := F.Bottom - F.Top;
    TI := Grid.GetCellTextIndent(Grid.EditCell);
    if EditStyle <> geSimple then
      Dec(W, ButtonWidth + 1)
    else
      Dec(W, Grid.TextRightIndent);
    R := Bounds(L + TI.X, T + TI.Y, W - TI.X + Ord(Alignment = taRightJustify), H);
    SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@R));
    if ScrollCaret then
      SendMessage(Handle, EM_SCROLLCARET, 0, 0);
  end
end;

procedure TCustomGridEdit.UpdateColors;
var
  Canvas: TCanvas;
begin
  if Grid <> nil then
  begin
    Canvas := TCanvas.Create;
    try
      Grid.GetCellColors(Grid.EditCell, Canvas);
      Color := Canvas.Brush.Color;
      Font := Canvas.Font;
    finally
      Canvas.Free;
    end;
  end;
end;

procedure TCustomGridEdit.UpdateContents;
begin
  if (Grid = nil) or (not Grid.IsCellValid(Grid.EditCell)) then
    Exit;
  with Grid do
  begin
    Self.MaxLength := Columns[EditCell.Col].MaxLength;
    Self.ReadOnly := IsCellReadOnly(EditCell) or (Self.MaxLength = -1);
    Self.WantReturns := Columns[EditCell.Col].WantReturns;
    case Columns[EditCell.Col].EditWordWrap of
      ewAuto: Self.WordWrap := Columns[EditCell.Col].WordWrap;
      ewEnabled: Self.WordWrap := True;
    else
      Self.WordWrap := False;
    end;
    Self.Alignment := Columns[EditCell.Col].EditAlignment;
    Self.EditMask := GetEditMask(EditCell);
    Self.Text := GetEditText(EditCell);
  end;
end;

procedure TCustomGridEdit.UpdateList;
begin
  if FActiveList <> nil then
  begin
    FActiveList.Visible := False;
    FActiveList.Parent := Self;
    THackWinControl(FActiveList).OnMouseUp := ListMouseUp;
    THackWinControl(FActiveList).Font := Font;
  end;
end;

procedure TCustomGridEdit.UpdateListBounds;
var
  I, X, W: Integer;
  R: TRect;
begin
  if (Grid = nil) or (FActiveList = nil) then
    Exit;
  {for the standard list we determine width and height in the lines}
  if FActiveList is TGridListBox then
    with TGridListBox(FActiveList) do
    begin
      Canvas.Font := Font;
      if Items.Count > 0 then
      begin
        W := 0;
        for I := 0 to Items.Count - 1 do
        begin
          X := Canvas.TextWidth(Items[I]);
          if W < X then
            W := X;
        end;
        ClientWidth := W + 6;
      end
      else
        ClientWidth := 100;
      { determine height}
      if (FDropDownCount < 1) or (Items.Count = 0) then
        ClientHeight := ItemHeight
      else if Items.Count < FDropDownCount then
        ClientHeight := Items.Count * ItemHeight
      else
        ClientHeight := FDropDownCount * ItemHeight;
    end;
  { to podpravlyam the dimensions of list depending on the dimensions of column and it
    position on the screen}
  with FActiveList do
  begin
    { correct on the width of column}
    R := Grid.GetCellRect(Grid.EditCell);
    Width := MaxIntValue([Width, R.Right - R.Left]);
    {position}
    Left := Self.ClientOrigin.X + Self.Width - Width;
    Top := Self.ClientOrigin.Y + Self.Height;
    if Top + Height > Screen.Height then
      Top := Self.ClientOrigin.Y - Height;
    { correct in the the sootvestviyem with the wish of user}
    R := BoundsRect;
    Grid.GetEditListBounds(Grid.EditCell, R);
    BoundsRect := R;
  end;
end;

procedure TCustomGridEdit.UpdateListItems;
begin
  if (Grid = nil) or (FActiveList = nil) or (not (FActiveList is TGridListBox)) then
    Exit;
  with TGridListBox(FActiveList) do
  begin
    Items.Clear;
    Grid.GetEditList(Grid.EditCell, Items);
    SendMessage(Handle, LB_SELECTSTRING, WORD(-1), Longint(PChar(Self.Text)));
  end;
end;

procedure TCustomGridEdit.UpdateListValue(Accept: Boolean);
var
  I: Integer;
begin
  if (FActiveList <> nil) and (FActiveList is TGridListBox) then
  begin
    I := TGridListBox(FActiveList).ItemIndex;
    if (Grid <> nil) then
      Grid.EditCloseUp(Grid.EditCell, I, Accept);
    if Accept and (I <> -1) then
    begin
      Text := TGridListBox(FActiveList).Items[I];
      SendMessage(Handle, EM_SETSEL, 0, -1);
    end;
  end;
end;

procedure TCustomGridEdit.UpdateStyle;
var
  Style: TGridEditStyle;
begin
  { obtain the style of line}
  Style := geSimple;
  if (Grid <> nil) and (not Grid.ReadOnly) then
    Style := Grid.GetEditStyle(Grid.EditCell);
  EditStyle := Style;
end;

{
  Delete the requested message from the queue, but throw back
  any WM_QUIT msgs that PeekMessage may also return.
}

procedure KillMessage(Wnd: HWND; Msg: Integer);
var
  M: TMsg;
begin
  M.Message := 0;
  if PeekMessage(M, Wnd, Msg, Msg, PM_REMOVE) and (M.Message = WM_QUIT) then
    PostQuitMessage(M.wParam);
end;

procedure TCustomGridEdit.WndProc(var Message: TMessage);

  procedure DoDropDownKeys(var Key: Word; Shift: TShiftState);
  begin
    case Key of
      VK_UP, VK_DOWN:
        if ssAlt in Shift then
        begin
          if FDropListVisible then
            CloseUp(True)
          else
            DropDown;
          Key := 0;
        end;
      VK_RETURN, VK_ESCAPE:
        if (not (ssAlt in Shift)) and FDropListVisible then
        begin
          KillMessage(Handle, WM_CHAR);
          CloseUp(Key = VK_RETURN);
          Key := 0;
        end;
    end;
  end;

  procedure DoButtonKeys(var Key: Word; Shift: TShiftState);
  begin
    if (Key = VK_RETURN) and (Shift = [ssCtrl]) then
    begin
      KillMessage(Handle, WM_CHAR);
      Key := 0;
      case EditStyle of
        geEllipsis: Press;
        gePickList, geDataList: if not FDropListVisible then
            SelectNext;
      end;
    end;
  end;

begin
  case Message.Msg of
    WM_KEYDOWN,
      WM_SYSKEYDOWN,
      WM_CHAR:
      with TWMKey(Message) do
      begin
        if EditStyle in [gePickList, geDataList] then
        begin
          DoDropDownKeys(CharCode, KeyDataToShiftState(KeyData));
          if (CharCode <> 0) and FDropListVisible then
          begin
            with TMessage(Message) do
              SendMessage(FActiveList.Handle, Msg, WParam, LParam);
            Exit;
          end;
        end;
        if not WantReturns then
        begin
          DoButtonKeys(CharCode, KeyDataToShiftState(KeyData));
          if CharCode = 0 then
            Exit;
        end;
      end;
    WM_SETFOCUS:
      begin
        if (GetParentForm(Self) = nil) or GetParentForm(Self).SetFocusedControl(Grid) then
          Dispatch(Message);
        Exit;
      end;
    WM_LBUTTONDOWN:
      with TWMLButtonDown(Message) do
      begin
        if (EditStyle = geSimple) or (not PtInrect(ButtonRect, Point(XPos, YPos))) then
          if UINT(GetMessageTime - FClickTime) < GetDoubleClickTime then
            Message.Msg := WM_LBUTTONDBLCLK;
        FClickTime := 0;
      end;
  end;
  inherited WndProc(Message);
end;

procedure TCustomGridEdit.CloseUp(Accept: Boolean);
const
  Flags = SWP_NOZORDER or SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW;
begin
  if FDropListVisible then
  begin
    if GetCapture <> 0 then
      SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    { hide list }
    SetWindowPos(FActiveList.Handle, 0, 0, 0, 0, 0, Flags);
    FDropListVisible := False;
    Invalidate;
    UpdateListValue(Accept);
  end;
end;

procedure TCustomGridEdit.Deselect;
begin
  SendMessage(Handle, EM_SETSEL, $7FFFFFFF, Integer($FFFFFFFF));
end;

procedure TCustomGridEdit.DropDown;
const
  Flags = SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW;
begin
  if (not FDropListVisible) and (Grid <> nil) and (EditStyle in [gePickList, geDataList]) then
  begin
    FActiveList := GetDropList;
    if FActiveList <> nil then
    begin
      UpdateList;
      UpdateListItems;
      UpdateListBounds;
      SetWindowPos(FActiveList.Handle, HWND_TOP, FActiveList.Left, FActiveList.Top, 0, 0, Flags);
      FDropListVisible := True;
      Invalidate;
      Winapi.Windows.SetFocus(Handle);
    end;
  end;
end;

procedure TCustomGridEdit.Invalidate;
var
  Cur: TRect;
begin
  if Grid = nil then
  begin
    inherited Invalidate;
    Exit;
  end;
  ValidateRect(Handle, nil);
  InvalidateRect(Handle, nil, True);
  Winapi.Windows.GetClientRect(Handle, Cur);
  MapWindowPoints(Handle, Grid.Handle, Cur, 2);
  ValidateRect(Grid.Handle, @Cur);
  InvalidateRect(Grid.Handle, @Cur, False);
end;

procedure TCustomGridEdit.Hide;
const
  Flags = SWP_HIDEWINDOW or SWP_NOZORDER or SWP_NOREDRAW;
begin
  if (Grid <> nil) and HandleAllocated and Visible then
  begin
    Grid.FEditing := False;
    Invalidate;
    SetWindowPos(Handle, 0, 0, 0, 0, 0, Flags);
    if Focused then
    begin
      FDefocusing := True;
      try
        Winapi.Windows.SetFocus(Grid.Handle);
      finally
        FDefocusing := False;
      end;
    end;
  end;
end;

procedure TCustomGridEdit.Press;
begin
  if Grid <> nil then
    Grid.EditButtonPress(Grid.EditCell);
end;

procedure TCustomGridEdit.SelectNext;
var
  OldText, NewText: string;
begin
  if Grid <> nil then
  begin
    OldText := Text;
    NewText := OldText;
    Grid.EditSelectNext(Grid.EditCell, NewText);
    if NewText <> OldText then
    begin
      Text := NewText;
      SendMessage(Handle, EM_SETSEL, 0, -1);
    end;
  end;
end;

procedure TCustomGridEdit.SetFocus;
begin
  if IsWindowVisible(Handle) then
    Winapi.Windows.SetFocus(Handle);
end;

procedure TCustomGridEdit.Show;
var
  ScrollCaret: Boolean;
begin
  if Grid <> nil then
  begin
    ScrollCaret := not Grid.FEditing;
    Grid.FEditing := True;
    Grid.FCellSelected := True;
    { correct colors (one should make before the installation of boundaries, since
      they are advanced depending on the size of type)}
    UpdateColors;
    UpdateBounds(ScrollCaret);
    if Grid.Focused then
      Winapi.Windows.SetFocus(Handle);
  end;
end;

{ TGridTipsWindow }

procedure TGridTipsWindow.WMNCPaint(var Message: TMessage);
begin
  Canvas.Handle := GetWindowDC(Handle);
  try
    Canvas.Pen.Color := clBlack;
    Canvas.Brush.Color := Color;
    Canvas.Rectangle(0, 0, Width, Height);
  finally
    Canvas.Handle := 0;
  end;
end;

procedure TGridTipsWindow.CMTextChanged(var Message: TMessage);
begin
  { ignore event, otherwise "jumps" the size of window }
end;

procedure TGridTipsWindow.Paint;
var
  TI: TPoint;
  A: TAlignment;
  WR, WW: Boolean;
  T: string;
begin
  if FGrid = nil then
  begin
    inherited Paint;
    Exit;
  end;
  FGrid.GetCellColors(FGrid.FTipsCell, Canvas);
  Canvas.Brush.Color := Color;
  Canvas.Font.Color := clInfoText;
  with FGrid do
  begin
    TI := GetCellTextIndent(FTipsCell);
    A := Columns[FTipsCell.Col].Alignment;
    WR := Columns[FTipsCell.Col].WantReturns;
    WW := Columns[FTipsCell.Col].WordWrap;
    T := FTipsText;
  end;
  FGrid.PaintText(Canvas, ClientRect, TI.X, TI.Y, A, WR, WW, T);
end;

procedure TGridTipsWindow.ActivateHint(Rect: TRect; const AHint: string);
const
  Flags = SWP_SHOWWINDOW or SWP_NOACTIVATE;
begin
  Caption := AHint;
  UpdateBoundsRect(Rect);
  SetWindowPos(Handle, HWND_TOPMOST, Rect.Left, Rect.Top, Width, Height, Flags);
  Invalidate;
end;

procedure TGridTipsWindow.ActivateHintData(Rect: TRect; const AHint: string; AData: Pointer);
begin
  FGrid := AData;
  inherited ActivateHintData(Rect, AHint, AData);
end;

function TGridTipsWindow.CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect;
var
  R: TRect;
begin
  FGrid := AData;
  if FGrid = nil then
  begin
    Result := inherited CalcHintRect(MaxWidth, AHint, AData);
    Exit;
  end;
  R := FGrid.GetTipsRect(FGrid.FTipsCell);
  OffsetRect(R, -R.Left, -R.Top);
  Result := R;
end;

{ TCustomGridView }

{  Initializes support for VCL styles.  }
class constructor TCustomGridView.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(
    TCustomGridView,
    TGridViewVclStyleScrollBarsHook
  );
end;

constructor TCustomGridView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csNeedsBorderPaint];
  Width                   := 185;
  Height                  := 105;
  Color                   := clWindow;
  ParentColor             := False;
  TabStop                 := True;
  FThemingEnabled         := True;
  FHorzScrollBar          := CreateScrollBar(sbHorizontal);
  FHorzScrollBar.OnScroll := HorzScroll;
  FHorzScrollBar.OnChange := HorzScrollChange;
  FVertScrollBar          := CreateScrollBar(sbVertical);
  FVertScrollBar.LineSize := 17;
  FVertScrollBar.OnScroll := VertScroll;
  FVertScrollBar.OnChange := VertScrollChange;
  FHeader                 := CreateHeader;
  FHeader.OnChange        := HeaderChange;
  FColumns                := CreateColumns;
  FColumns.OnChange       := ColumnsChange;
  FRows                   := CreateRows;
  FRows.OnChange          := RowsChange;
  FFixed                  := CreateFixed;
  FFixed.OnChange         := FixedChange;
  FImagesLink             := TChangeLink.Create;
  FImagesLink.OnChange    := ImagesChange;
  FBorderStyle            := bsSingle;
  FShowHeader             := True;
  FGridLines              := True;
  FGridLineWidth          := 1; { <- do not change !!! }
  FGridStyle              := [gsHorzLine, gsVertLine];
  FGridColor              := clSilver;
  FEndEllipsis            := True;
  FImageLeftIndent        := 2;
  FImageTopIndent         := 1;
  FImageHighlight         := True;
  FTextLeftIndent         := 6;
  FTextRightIndent        := 6;
  FTextTopIndent          := 2;
  FShowFocusRect          := True;
  FRightClickSelect       := True;
  FAllowSelect            := True;
  FCursorKeys             := [gkArrows, gkMouse, gkMouseWheel];
  FColumnsSizing          := True;
  FColumnClick            := True;
  FEditCell               := GridCell(-1, -1);
  FCheckStyle             := csWin95;
  FCheckWidth             := 16;
  FCheckHeight            := 16;
  FCheckLeftIndent        := 0;
  FCheckTopIndent         := 0;
  FCheckBitmapCB          := TBitmap.Create;
  FCheckBitmapCB.LoadFromResourceName(HInstance, 'BM_GRIDVIEW_CB');
  FCheckBitmapRB          := TBitmap.Create;
  FCheckBitmapRB.LoadFromResourceName(HInstance, 'BM_GRIDVIEW_RB');
  FCheckBuffer            := TBitmap.Create;
  FSortLeftIndent         := 0;
  FSortTopIndent          := 0;
  FSortBitmapA            := TBitmap.Create;
  FSortBitmapA.LoadFromResourceName(HInstance, 'BM_GRIDVIEW_SA');
  FSortBitmapD            := TBitmap.Create;
  FSortBitmapD.LoadFromResourceName(HInstance, 'BM_GRIDVIEW_SD');
  FSortBuffer             := TBitmap.Create;
  FPatternBitmap          := TBitmap.Create;
  FPatternBitmap.Width    := 2;
  FPatternBitmap.Height   := 2;
  FCancelOnExit           := True;
  VclStyleChanged;
end;

destructor TCustomGridView.Destroy;
begin
  FreeandNil(FPatternBitmap);
  FreeandNil(FSortBuffer);
  FreeandNil(FSortBitmapD);
  FreeandNil(FSortBitmapA);
  FreeandNil(FCheckBuffer);
  FreeandNil(FCheckBitmapRB);
  FreeandNil(FCheckBitmapCB);
  FreeandNil(FImagesLink);
  FreeandNil(FFixed);
  FreeandNil(FRows);
  FreeandNil(FColumns);
  FreeandNil(FHeader);
  FreeandNil(FHorzScrollBar);
  FreeandNil(FVertScrollBar);
  inherited Destroy;
end;

function TCustomGridView.GetCell(Col, Row: Integer): string;
begin
  Result := GetCellText(GridCell(Col, Row));
end;

function TCustomGridView.GetChecked(Col, Row: Integer): Boolean;
begin
  Result := GetCheckState(GridCell(Col, Row)) in [cbChecked, cbGrayed];
end;

function TCustomGridView.GetCheckBoxState(Col, Row: Longint): TCheckBoxState;
begin
  Result := GetCheckState(GridCell(Col, Row));
end;
function TCustomGridView.GetCol: Integer;
begin
  Result := CellFocused.Col;
end;

function TCustomGridView.GetFixed: TGridFixed;
begin
  Result := TGridFixed(FFixed);
end;

function TCustomGridView.GetEdit: TGridEdit;
begin
  Result := TGridEdit(FEdit);
end;

function TCustomGridView.GetEditColumn: TGridColumn;
begin
  Result := nil;
  if (EditCell.Col >= 0) and (EditCell.Col < Columns.Count) then
    Result := Columns[EditCell.Col];
end;

function TCustomGridView.GetEditDropDown: Boolean;
begin
  Result := (Edit <> nil) and Edit.DropListVisible;
end;

function TCustomGridView.GetEditing: Boolean;
begin
  Result := FEditing and (FEdit <> nil);
end;

function TCustomGridView.GetHeader: TGridHeader;
begin
  Result := TGridHeader(FHeader);
end;

function TCustomGridView.GetLeftCol: Integer;
begin
  Result := VisOrigin.Col;
end;

function TCustomGridView.GetRow: Integer;
begin
  Result := CellFocused.Row;
end;

function TCustomGridView.GetRows: TGridRows;
begin
  Result := TGridRows(FRows);
end;

function TCustomGridView.GetTopRow: Integer;
begin
  Result := VisOrigin.Row;
end;

function TCustomGridView.GetVisibleColCount: Integer;
begin
  Result := VisSize.Col;
end;

function TCustomGridView.GetVisibleRowCount: Integer;
begin
  Result := VisSize.Row;
end;

procedure TCustomGridView.ColumnsChange(Sender: TObject);
begin
  if [csReading, csLoading] * ComponentState = [] then
  begin
    UpdateFixed;
    UpdateHeader;
    if not Header.AutoSynchronize then
      Header.Synchronized := False;
  end;
  UpdateScrollBars;
  UpdateVisOriginSize;
  UpdateCursor;
  UpdateEdit(Editing);
  Invalidate;
  ChangeColumns;
end;

procedure TCustomGridView.FixedChange(Sender: TObject);
begin
  UpdateRows;
  UpdateScrollBars;
  UpdateVisOriginSize;
  UpdateCursor;
  UpdateEdit(Editing);
  Invalidate;
  ChangeFixed;
end;

function TCustomGridView.Focused: Boolean;
begin
  Result := inherited Focused or (Assigned(Edit) and Edit.Focused);
end;

procedure TCustomGridView.HeaderChange(Sender: TObject);
begin
  UpdateScrollBars;
  UpdateVisOriginSize;
  UpdateEdit(Editing);
  Invalidate;
end;

procedure TCustomGridView.HorzScroll(Sender: TObject; ScrollCode: Integer;
  var ScrollPos: Integer);
begin
  CancelCellTips;
  if FocusOnScroll then
    UpdateFocus;
end;

procedure TCustomGridView.HorzScrollChange(Sender: TObject);
begin
  UpdateVisOriginSize;
  UpdateEdit(Editing);
end;

procedure TCustomGridView.ImagesChange(Sender: TObject);
begin
  InvalidateGrid;
  UpdateRows;
end;

procedure TCustomGridView.RowsChange(Sender: TObject);
begin
  CancelCellTips;
  UpdateScrollBars;
  UpdateVisOriginSize;
  UpdateCursor;
  UpdateEdit(Editing);
  Invalidate;
  ChangeRows;
end;

procedure TCustomGridView.SetAllowEdit(Value: Boolean);
begin
  if FAllowEdit <> Value then
  begin
    FAllowEdit := Value;
    if not Value then
    begin
      AlwaysEdit := False;
      HideEdit;
    end
    else
      RowSelect := False;
    ChangeEditMode;
  end;
end;

procedure TCustomGridView.SetAllowSelect(Value: Boolean);
begin
  if FAllowSelect <> Value then
  begin
    FAllowSelect := Value;
    RowSelect := FRowSelect or (not Value);
    InvalidateFocus;
  end;
end;

procedure TCustomGridView.SetAlwaysEdit(Value: Boolean);
begin
  if FAlwaysEdit <> Value then
  begin
    FAlwaysEdit := Value;
    if Value then
    begin
      AllowEdit := True;
      Editing := True;
    end
    else
      HideEdit;
  end;
end;

procedure TCustomGridView.SetAlwaysSelected(Value: Boolean);
begin
  if FAlwaysSelected <> Value then
  begin
    FAlwaysSelected := Value;
    FCellSelected := FCellSelected or Value;
    InvalidateFocus;
  end;
end;

// TODO : extend implementation!!

procedure TCustomGridView.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TCustomGridView.SetCell(Col, Row: Integer; Value: string);
begin
  SetEditText(GridCell(Col, Row), Value);
end;

procedure TCustomGridView.SetCellFocused(Value: TGridCell);
begin
  SetCursor(Value, CellSelected, True);
end;

procedure TCustomGridView.SetCellSelected(Value: Boolean);
begin
  SetCursor(CellFocused, Value, True);
end;

procedure TCustomGridView.SetCheckBoxes(Value: Boolean);
begin
  if FCheckBoxes <> Value then
  begin
    FCheckBoxes := Value;
    UpdateRows;
    UpdateEdit(Editing);
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetCheckLeftIndent(Value: Integer);
begin
  if FCheckLeftIndent <> Value then
  begin
    FCheckLeftIndent := Value;
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetCheckStyle(Value: TGridCheckStyle);
begin
  if FCheckStyle <> Value then
  begin
    FCheckStyle := Value;
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetCheckTopIndent(Value: Integer);
begin
  if FCheckTopIndent <> Value then
  begin
    FCheckTopIndent := Value;
    UpdateRows;
    UpdateEdit(Editing);
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetCol(Value: Integer);
begin
  CellFocused := GridCell(Value, CellFocused.Row);
end;

procedure TCustomGridView.SetColumns(Value: TGridColumns);
begin
  FColumns.Assign(Value);
end;

procedure TCustomGridView.SetCursorKeys(Value: TGridCursorKeys);
begin
  { check for incompatible flags}
  if gkMouseMove in Value then
    Include(Value, gkMouse);
  if not (gkMouse in Value) then
    Exclude(Value, gkMouseMove);
  FCursorKeys := Value;
end;

procedure TCustomGridView.SetEditDropDown(Value: Boolean);
begin
  Editing := True;
  if Edit <> nil then
    Edit.DropListvisible := True;
end;

procedure TCustomGridView.SetEditing(Value: Boolean);
var
  WasEditing: Boolean;
begin
  WasEditing := Editing;
  if Value and AllowEdit then
  begin
    if AcquireFocus then
      ShowEdit;
  end
  else if (not Value) and FEditing then
  begin
    UpdateEditText;
    if not AlwaysEdit then
      HideEdit;
  end;
  if WasEditing <> Editing then
    ChangeEditing;
end;

procedure TCustomGridView.SetEndEllipsis(Value: Boolean);
begin
  if FEndEllipsis <> Value then
  begin
    FEndEllipsis := Value;
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetFlatBorder(Value: Boolean);
begin
  if FFlatBorder <> Value then
  begin
    FFlatBorder := Value;
    RecreateWnd;
  end;
end;

procedure TCustomGridView.SetFitColsToClient(const Value: Boolean);
begin
  FFitColsToClient := Value;
  if Value then
    SizeColumnsToClient;
end;
procedure TCustomGridView.SetFixed(Value: TGridFixed);
begin
  FFixed.Assign(Value);
end;

procedure TCustomGridView.SetHeader(Value: TGridHeader);
begin
  FHeader.Assign(Value);
end;

procedure TCustomGridView.SetHideSelection(Value: Boolean);
begin
  if FHideSelection <> Value then
  begin
    FHideSelection := Value;
    InvalidateFocus;
  end;
end;

procedure TCustomGridView.SetHorzScrollBar(Value: TGridScrollBar);
begin
  FHorzScrollBar.Assign(Value);
end;

procedure TCustomGridView.SetGridColor(Value: TColor);
begin
  if FGridColor <> Value then
  begin
    FGridColor := Value;
    Invalidate;
  end;
end;

procedure TCustomGridView.SetGridLines(Value: Boolean);
begin
  if FGridLines <> Value then
  begin
    FGridLines := Value;
    UpdateRows;
    UpdateEdit(Editing);
    Invalidate;
  end;
end;

procedure TCustomGridView.SetGridStyle(Value: TGridStyles);
begin
  if FGridStyle <> Value then
  begin
    FGridStyle := Value;
    UpdateRows;
    UpdateEdit(Editing);
    Invalidate;
  end;
end;

procedure TCustomGridView.SetImageIndexDef(Value: Integer);
begin
  if FImageIndexDef <> Value then
  begin
    FImageIndexDef := Value;
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetImageHighlight(Value: Boolean);
begin
  if FImageHighlight <> Value then
  begin
    FImageHighlight := Value;
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetImageLeftIndent(Value: Integer);
begin
  if FImageLeftIndent <> Value then
  begin
    FImageLeftIndent := Value;
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetImages(Value: TImageList);
begin
  if FImages <> Value then
  begin
    if Assigned(FImages) then
      FImages.UnRegisterChanges(FImagesLink);
    FImages := Value;
    if Assigned(FImages) then
    begin
      FImages.RegisterChanges(FImagesLink);
      FImages.FreeNotification(Self);
    end;
    UpdateRows;
    UpdateEdit(Editing);
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetImageTopIndent(Value: Integer);
begin
  if FImageTopIndent <> Value then
  begin
    FImageTopIndent := Value;
    UpdateRows;
    UpdateEdit(Editing);
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetLeftCol(Value: Integer);
begin
  VisOrigin := GridCell(Value, VisOrigin.Row);
end;

procedure TCustomGridView.SetReadOnly(Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    UpdateEditContents(True);
  end;
end;

procedure TCustomGridView.SetRow(Value: Integer);
begin
  CellFocused := GridCell(CellFocused.Col, Value);
end;

procedure TCustomGridView.SetRows(Value: TGridRows);
begin
  FRows.Assign(Value);
end;

procedure TCustomGridView.SetRowSelect(Value: Boolean);
begin
  if FRowSelect <> Value then
  begin
    FRowSelect := Value;
    if Value then
      AllowEdit := False;
    AllowSelect := AllowSelect or (not Value);
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetShowCellTips(Value: Boolean);
begin
  if FShowCellTips <> Value then
  begin
    FShowCellTips := Value;
    ShowHint := ShowHint or Value;
  end;
end;

procedure TCustomGridView.SetShowFocusRect(Value: Boolean);
begin
  if FShowFocusRect <> Value then
  begin
    FShowFocusRect := Value;
    InvalidateFocus;
  end;
end;

procedure TCustomGridView.SetShowHeader(Value: Boolean);
begin
  if FShowHeader <> Value then
  begin
    FShowHeader := Value;
    UpdateScrollBars;
    UpdateVisOriginSize;
    UpdateEdit(Editing);
    UpdateCursor;
    Invalidate;
  end;
end;

procedure TCustomGridView.SetSortLeftIndent(Value: Integer);
begin
  if FSortLeftIndent <> Value then
  begin
    FSortLeftIndent := Value;
    UpdateHeader;
    InvalidateHeader;
  end;
end;

procedure TCustomGridView.SetSortTopIndent(Value: Integer);
begin
  if FSortTopIndent <> Value then
  begin
    FSortTopIndent := Value;
    UpdateHeader;
    InvalidateHeader;
  end;
end;

procedure TCustomGridView.SetTextLeftIndent(Value: Integer);
begin
  if FTextLeftIndent <> Value then
  begin
    FTextLeftIndent := Value;
    UpdateEdit(Editing);
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetTextRightIndent(Value: Integer);
begin
  if FTextRightIndent <> Value then
  begin
    FTextRightIndent := Value;
    UpdateEdit(Editing);
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetTextTopIndent(Value: Integer);
begin
  if FTextTopIndent <> Value then
  begin
    FTextTopIndent := Value;
    UpdateRows;
    UpdateEdit(Editing);
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetTopRow(Value: Integer);
begin
  VisOrigin := GridCell(VisOrigin.Col, Value);
end;

procedure TCustomGridView.SetVertScrollBar(Value: TGridScrollBar);
begin
  FVertScrollBar.Assign(Value);
end;

procedure TCustomGridView.SetVisOrigin(Value: TGridCell);
begin
  if (FVisOrigin.Col <> Value.Col) or (FVisOrigin.Row <> Value.Row) then
  begin
    FVisOrigin := Value;
    UpdateScrollPos;
    UpdateVisOriginSize;
    Invalidate;
  end;
end;

procedure TCustomGridView.SetThemingEnabled(const Value: Boolean);
begin
  if Value <> ThemingEnabled then
  begin
    FThemingEnabled := Value;
    Invalidate;
  end;
end;

procedure TCustomGridView.VclStyleChanged;
begin
  FVclStyleEnabled := ThemingEnabled and StyleServices.Enabled
    and not StyleServices.IsSystemStyle;
end;

procedure TCustomGridView.VertScroll(Sender: TObject; ScrollCode: Integer;
  var ScrollPos: Integer);
begin
  CancelCellTips;
  if FocusOnScroll then
    UpdateFocus;
end;

procedure TCustomGridView.VertScrollChange(Sender: TObject);
begin
  UpdateVisOriginSize;
  UpdateEdit(Editing);
end;

procedure TCustomGridView.WMPaint(var Message: TWMPaint);
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
begin
  if DoubleBuffered and (Message.DC = 0) then
  begin
    DC := GetDC(0);
    MemBitmap := CreateCompatibleBitmap(DC, ClientRect.Right, ClientRect.Bottom);
    ReleaseDC(0, DC);
    MemDC := CreateCompatibleDC(0);
    OldBitmap := SelectObject(MemDC, MemBitmap);
    try
      DC := BeginPaint(Handle, PS);
      Perform(WM_ERASEBKGND, MemDC, 0);
      Message.DC := MemDC;
      WMPaint(Message);
      Message.DC := 0;
      if Editing then
        with GetEditRect(EditCell) do
          ExcludeClipRect(DC, Left, Top, Right, Bottom);
      BitBlt(DC, 0, 0, ClientRect.Right, ClientRect.Bottom, MemDC, 0, 0, SRCCOPY);
      EndPaint(Handle, PS);
    finally
      SelectObject(MemDC, OldBitmap);
      DeleteDC(MemDC);
      DeleteObject(MemBitmap);
    end;
  end
  else
    inherited;
end;

procedure TCustomGridView.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  with Message do
  begin
    Result := DLGC_WANTARROWS;
    if not RowSelect then
    begin
      if gkTabs in CursorKeys then
        Result := Result or DLGC_WANTTAB;
      if AllowEdit then
        Result := Result or DLGC_WANTCHARS;
    end;
  end;
end;

procedure TCustomGridView.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  if not (csDestroying in ComponentState) then
  begin
    if Rows.Count > 0 then
    begin
      InvalidateFocus;
      if (FEdit <> nil) and (Message.FocusedWnd <> FEdit.Handle) then
        HideCursor;
    end;
  end;
end;

procedure TCustomGridView.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if Rows.Count > 0 then
  begin
    InvalidateFocus;
    if (FEdit = nil) or ((Message.FocusedWnd <> FEdit.Handle) or (not FEdit.FDefocusing)) then
      ShowCursor;
  end;
end;

procedure TCustomGridView.WMLButtonDown(var Message: TMessage);
begin
  inherited;
  if FEdit <> nil then
    FEdit.FClickTime := GetMessageTime;
end;

procedure TCustomGridView.WMChar(var Msg: TWMChar);
begin
  if AllowEdit and CharInSet(Char(Msg.CharCode), [^H, #32..#255]) then
  begin
    ShowEditChar(Char(Msg.CharCode));
    Exit;
  end;
  inherited;
end;

procedure TCustomGridView.WMSize(var Message: TWMSize);
begin
  inherited;
  Resize;
end;

procedure TCustomGridView.WMHScroll(var Message: TWMHScroll);
begin
  if Message.ScrollBar = 0 then
    FHorzScrollBar.ScrollMessage(Message)
  else
    inherited;
end;

procedure TCustomGridView.WMVScroll(var Message: TWMVScroll);
begin
  if Message.ScrollBar = 0 then
    FVertScrollBar.ScrollMessage(Message)
  else
    inherited;
end;

procedure TCustomGridView.WMNCHitTest(var Message: TWMNCHitTest);
begin
  inherited;
  FHitTest := ScreenToClient(SmallPointToPoint(Message.Pos));
end;

procedure TCustomGridView.WMSetCursor(var Message: TWMSetCursor);
begin
  with Message, FHitTest do
    if not (csDesigning in ComponentState) then
    begin
      if FColResizing then
      begin
        Winapi.Windows.SetCursor(Screen.Cursors[crHSplit]);
        Exit;
      end;
      if (HitTest = HTCLIENT) and ShowHeader then
        if PtInRect(GetHeaderRect, FHitTest)
          and (GetResizeSectionAt(X, Y) <> nil) then
        begin
          Winapi.Windows.SetCursor(Screen.Cursors[crHSplit]);
          Exit;
        end;
    end;
  inherited;
end;

procedure TCustomGridView.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TCustomGridView.CMCancelMode(var Message: TCMCancelMode);
begin
  if FEdit <> nil then
    FEdit.WndProc(TMessage(Message));
  inherited;
end;

procedure TCustomGridView.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TCustomGridView.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) then
    RecreateWnd;
  inherited;
end;

procedure TCustomGridView.CMFontChanged(var Message: TMessage);
begin
  Canvas.Font := Font;
  UpdateFonts;
  UpdateRows;
  inherited;
end;

procedure TCustomGridView.CMColorChanged(var Message: TMessage);
begin
  Brush.Color := Color;
  UpdateColors;
  inherited;
end;

procedure TCustomGridView.CMShowHintChanged(var Message: TMessage);
begin
  ShowCellTips := ShowCellTips and ShowHint;
end;

procedure TCustomGridView.CMStyleChanged(var Message: TMessage);
begin
  RecreateWnd;
end;

procedure TCustomGridView.CMHintShow(var AMessage: TMessage);
var
  AllowTips: Boolean;
  R, TR: TRect;
  W: Integer;
begin
  with AMessage, PHintInfo(LParam)^ do
  begin
    if not ShowCellTips then
    begin
      inherited;
      Exit;
    end;
    if not PtInRect(GetGridRect, CursorPos) then
    begin
      Result := 1;
      Exit;
    end;
    FTipsCell := GetCellAt(CursorPos.X, CursorPos.Y);
    if IsCellEmpty(FTipsCell) then
    begin
      Result := 1;
      Exit;
    end;
    if IsCellEqual(EditCell, FTipsCell) and Editing then
    begin
      Result := 1;
      Exit;
    end;
    CellTips(FTipsCell, AllowTips);
    if not AllowTips then
    begin
      Result := 1;
      Exit;
    end;
    { obtain the rectangle of cell (without the picture)}
    R := GetCellHintRect(FTipsCell);
    { obtain the rectangle of the text of cell}
    TR := GetCellTextBounds(FTipsCell);
    W := TR.Right - TR.Left;
    case Columns[FTipsCell.Col].Alignment of
      taCenter:
        begin
          TR.Left := R.Left - (W - (R.Right - R.Left)) div 2;
          TR.Right := TR.Left + W;
        end;
      taRightJustify:
        begin
          TR.Right := R.Right;
          TR.Left := TR.Right - W;
        end;
    else
      TR.Left := R.Left;
      TR.Right := TR.Left + W;
    end;
    { consider the visible part of the table}
    IntersectRect(R, R, ClientRect);
    if ShowHeader then
      SubtractRect(R, R, GetHeaderRect);
    if FTipsCell.Col >= Fixed.Count then
      SubtractRect(R, R, GetFixedRect);
    if (TR.Left >= R.Left) and (TR.Right <= R.Right) and
      (TR.Bottom - TR.Top <= R.Bottom - R.Top) then
    begin
      Result := 1;
      Exit;
    end;
    FTipsText := GetTipsText(FTipsCell);
    R := GetTipsRect(FTipsCell);
    HintPos := ClientToScreen(R.TopLeft);
    HintStr := FTipsText;
    R := GetCellRect(FTipsCell);
    if FTipsCell.Col < Fixed.Count then
    begin
      R.Left := MaxIntValue([R.Left, GetFixedRect.Left]);
      R.Right := MinIntValue([R.Right, GetFixedRect.Right]);
    end
    else
    begin
      R.Left := MaxIntValue([R.Left, GetFixedRect.Right]);
      R.Right := MinIntValue([R.Right, GetClientRect.Right]);
    end;
    InflateRect(R, 1, 1);
    CursorRect := R;
    HintWindowClass := GetTipsWindowClass;
    HintData := Self;
    Result := 0;
  end;
end;

procedure TCustomGridView.CMMouseLeave(var AMessage: TMessage);
begin
  inherited;
  UpdateHotHeader(Nil);
  DoMouseLeave;
end;

function TCustomGridView.AcquireFocus: Boolean;
begin
  Result := True;
  if not (csDesigning in ComponentState) and CanFocus then
  begin
    UpdateFocus;
    Result := IsActiveControl;
  end;
end;

procedure TCustomGridView.CancelCellTips;
var
  P: TPoint;
  HintControl: TControl;

  function GetHintControl(Control: TControl): TControl;
  begin
    Result := Control;
    while (Result <> nil) and not Result.ShowHint do
      Result := Result.Parent;
    if (Result <> nil) and (csDesigning in Result.ComponentState) then
      Result := nil;
  end;

begin
  if ShowCellTips then
  begin
    Winapi.Windows.GetCursorPos(P);
    HintControl := GetHintControl(FindDragTarget(P, False));
    if HintControl = Self then
      Application.CancelHint;
  end;
end;
procedure TCustomGridView.CellClick(Cell: TGridCell; Shift: TShiftState;
  X, Y: Integer);
begin
  if Assigned(FOnCellClick) then
    FOnCellClick(Self, Cell, Shift, X, Y);
end;

procedure TCustomGridView.CellTips(Cell: TGridCell; var AllowTips: Boolean);
begin
  AllowTips := True;
  if Assigned(FOnCellTips) then
    FOnCellTips(self, Cell, AllowTips);
end;

procedure TCustomGridView.Change(var Cell: TGridCell; var Selected: Boolean);
begin
  if Assigned(FOnChange) then
    FOnChange(Self, Cell, Selected);
end;

procedure TCustomGridView.ChangeColumns;
begin
  if Assigned(FOnChangeColumns) then
    FOnChangeColumns(Self);
end;

procedure TCustomGridView.ChangeEditing;
begin
  if Assigned(FOnChangeEditing) then
    FOnChangeEditing(Self);
end;

procedure TCustomGridView.ChangeEditMode;
begin
  if Assigned(FOnChangeEditMode) then
    FOnChangeEditMode(Self);
end;

procedure TCustomGridView.ChangeFixed;
begin
  if Assigned(FOnChangeFixed) then
    FOnChangeFixed(Self);
end;

procedure TCustomGridView.ChangeRows;
begin
  if Assigned(FOnChangeRows) then
    FOnChangeRows(Self);
end;

procedure TCustomGridView.ChangeScale(M, D: Integer);
var
  S: Boolean;
  I: Integer;
begin
  inherited ChangeScale(M, D);
  if M <> D then
  begin
    S := Header.Synchronized;
    try
      with Columns do
      begin
        BeginUpdate;
        try
          for I := 0 to Count - 1 do
          begin
            Columns[I].FMaxWidth := MulDiv(Columns[I].FMaxWidth, M, D);
            Columns[I].FMinWidth := MulDiv(Columns[I].FMinWidth, M, D);
            Columns[I].DefWidth := MulDiv(Columns[I].DefWidth, M, D);
          end;
        finally
          EndUpdate;
        end;
      end;
      with Rows do
        Height := MulDiv(Height, M, D);
      with Header do
      begin
        SectionHeight := MulDiv(SectionHeight, M, D);
        if not GridFont then
          Font.Size := MulDiv(Font.Size, M, D);
      end;
      with Fixed do
        if not GridFont then
          Font.Size := MulDiv(Font.Size, M, D);
    finally
      Header.Synchronized := S;
    end;
  end;
end;

procedure TCustomGridView.Changing(var Cell: TGridCell; var Selected: Boolean);
begin
  if Assigned(FOnChanging) then
    FOnChanging(Self, Cell, Selected);
end;

procedure TCustomGridView.CheckClick(Cell: TGridCell);
begin
  if Assigned(FOnCheckClick) then
  begin
    FOnCheckClick(Self, Cell);
    InvalidateCell(Cell);
  end;
end;

procedure TCustomGridView.ColumnAutoSize(Column: Integer; var Width: Integer);
begin
  if Assigned(FOnColumnAutoSize) then
    FOnColumnAutoSize(Self, Column, Width);
end;

procedure TCustomGridView.ColumnResize(Column: Integer; var Width: Integer);
begin
  if Assigned(FOnColumnResize) then
    FOnColumnResize(Self, Column, Width);
end;

procedure TCustomGridView.ColumnResizing(Column: Integer; var Width: Integer);
begin
  if Assigned(FOnColumnResizing) then
    FOnColumnResizing(Self, Column, Width);
end;

function TCustomGridView.CreateColumn(Columns: TGridColumns): TCustomGridColumn;
begin
  Result := GetColumnClass.Create(Columns);
end;

function TCustomGridView.CreateColumns: TGridColumns;
begin
  Result := TGridColumns.Create(Self);
end;

function TCustomGridView.CreateEdit(EditClass: TGridEditClass): TCustomGridEdit;
begin
  if EditClass = nil then
    EditClass := TGridEdit;
  Result := EditClass.Create(Self);
end;

function TCustomGridView.CreateFixed: TCustomGridFixed;
begin
  Result := TGridFixed.Create(Self);
end;

function TCustomGridView.CreateHeader: TCustomGridHeader;
begin
  Result := TGridHeader.Create(Self);
end;

function TCustomGridView.CreateHeaderSection(Sections: TGridHeaderSections):
  TGridHeaderSection;
begin
  Result := TGridHeaderSection.Create(Sections);
end;

procedure TCustomGridView.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
  FlatBorders: array[Boolean] of DWORD = (WS_EX_CLIENTEDGE, WS_EX_STATICEDGE);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_TABSTOP;
    Style := Style or BorderStyles[FBorderStyle];
    if Ctl3D and NewStyleControls and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or FlatBorders[FFlatBorder];
    end;
  end;
end;

function TCustomGridView.CreateRows: TCustomGridRows;
begin
  Result := TGridRows.Create(Self);
end;

function TCustomGridView.CreateScrollBar(Kind: TScrollBarKind): TGridScrollBar;
begin
  Result := TGridScrollBar.Create(Self, Kind);
end;

procedure TCustomGridView.CreateWnd;
begin
  inherited;
  FHorzScrollBar.Update;
  FVertScrollBar.Update;
end;

procedure TCustomGridView.DoAppendRow(var Append: Boolean);
begin
  if Assigned(FOnAppendRow) then
    FOnAppendRow(Self, Append);
end;

procedure TCustomGridView.DoExit;
begin
  ResetClickPos;
  if CancelOnExit then
    Editing := False;
  inherited DoExit;
end;

procedure TCustomGridView.DoMouseEnter;
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TCustomGridView.DoMouseLeave;
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

function TCustomGridView.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint):
  Boolean;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  if (not Result) and (gkMouseWheel in CursorKeys) then
  begin
    SetCursor(GetCursorCell(CellFocused, goMouseWheelDown), True, True);
    Result := True;
  end;
end;

function TCustomGridView.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint):
  Boolean;
begin
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  if (not Result) and (gkMouseWheel in CursorKeys) then
  begin
    SetCursor(GetCursorCell(CellFocused, goMouseWheelUp), True, True);
    Result := True;
  end;
end;

procedure TCustomGridView.EditButtonPress(Cell: TGridCell);
begin
  if Assigned(FOnEditButtonPress) then
    FOnEditButtonPress(Self, Cell);
end;

procedure TCustomGridView.EditCanceled(Cell: TGridCell);
begin
  if Assigned(FOnEditCanceled) then
    FOnEditCanceled(Self, Cell);
end;

function TCustomGridView.EditCanModify(Cell: TGridCell): Boolean;
begin
  Result := not IsCellReadOnly(Cell);
  if Assigned(FOnEditCanModify) then
    FOnEditCanModify(Self, Cell, Result);
end;

function TCustomGridView.EditCanAcceptKey(Cell: TGridCell; Key: Char): Boolean;
begin
  Result := IsCellValid(Cell);
  if Assigned(FOnEditAcceptKey) then
    FOnEditAcceptKey(Self, Cell, Key, Result);
end;

function TCustomGridView.EditCanShow(Cell: TGridCell): Boolean;
begin
  if [csReading, csLoading, csDesigning, csDestroying] * ComponentState <> [] then
  begin
    Result := False;
    Exit;
  end;
  if (Columns.Count - Fixed.Count = 0) or (Rows.Count = 0) then
  begin
    Result := False;
    Exit;
  end;
  Result := HandleAllocated and AllowEdit and (AlwaysEdit or IsActiveControl);
  if (Cell.Col >= Fixed.Count) and (Cell.Col < Columns.Count) then
    Result := Result and Columns[Cell.Col].AllowEdit;
  if Result and Assigned(FOnEditCanShow) then
    FOnEditCanShow(Self, Cell, Result);
end;

function TCustomGridView.EditCanUndo(Cell: TGridCell): Boolean;
begin
  Result := EditCanModify(Cell);
end;

procedure TCustomGridView.EditChange(Cell: TGridCell);
begin
  if Assigned(FOnEditChange) then
    FOnEditChange(Self, Cell);
end;

procedure TCustomGridView.EditCloseUp(Cell: TGridCell; ItemIndex: Integer;
  var Accept: Boolean);
begin
  if Assigned(FOnEditCloseUp) then
    FOnEditCloseUp(Self, Cell, ItemIndex, Accept);
end;

procedure TCustomGridView.EditSelectNext(Cell: TGridCell; var Value: string);
begin
  if Assigned(FOnEditSelectNext) then
    FOnEditSelectNext(Self, Cell, Value);
end;

procedure TCustomGridView.GetCellColors(Cell: TGridCell; Canvas: TCanvas);
begin
  if (Cell.Col >= 0) and (Cell.Col < Fixed.Count) then
  begin
    Canvas.Brush.Color := Fixed.Color;
    Canvas.Font := Fixed.Font;
  end
  else
  begin
    Canvas.Brush.Color := Self.Color;
    Canvas.Font := Self.Font;

    if not Enabled then
      Canvas.Font.Color := clGrayText;

    if Enabled and IsFocusAllowed and IsCellHighlighted(Cell) then
      if Focused then
      begin
        Canvas.Brush.Color := clHighlight;
        Canvas.Font.Color := clHighlightText;
      end
      else if not HideSelection then
      begin
        Canvas.Brush.Color := clBtnFace;
        Canvas.Font.Color := Font.Color;
      end;
  end;
  if Assigned(FOnGetCellColors) then
    FOnGetCellColors(Self, Cell, Canvas);
end;

function TCustomGridView.GetCellImage(Cell: TGridCell): Integer;
begin
  if not Assigned(Images) then
  begin
    Result := -1;
    Exit;
  end;
  if Cell.Col = GetFirstImageColumn then
    Result := ImageIndexDef
  else
    Result := -1;
  if Assigned(FOnGetCellImage) then
    FOnGetCellImage(Self, Cell, Result);
end;

function TCustomGridView.GetCellImageIndent(Cell: TGridCell): TPoint;
begin
  Result.X := ImageLeftIndent;
  Result.Y := ImageTopIndent;
  if (Fixed.Count > 0) and (not Fixed.Flat) then
    Inc(Result.Y, 1);
  if Assigned(FOnGetCellImageIndent) then
    FOnGetCellImageIndent(Self, Cell, Result);
end;

function TCustomGridView.GetCellImageRect(Cell: TGridCell): TRect;
var
  R: TRect;
begin
  if not IsCellHasImage(Cell) then
  begin
    Result := System.Classes.Rect(0, 0, 0, 0);
    Exit;
  end;
  R := GetCellRect(Cell);
  if IsCellHasCheck(Cell) then
    Inc(R.Left, CheckWidth + GetCheckIndent(Cell).X);
  with Result do
  begin
    Left := R.Left + GetCellImageIndent(Cell).X;
    Right := MinIntValue([Left + Images.Width, R.Right]);
    Top := R.Top + GetCellImageIndent(Cell).Y;
    Bottom := R.Top + Images.Height;
  end;
end;

function TCustomGridView.GetCellHintRect(Cell: TGridCell): TRect;
begin
  Result := GetEditRect(Cell);
  if Assigned(FOnGetCellHintRect) then
    FOnGetCellHintRect(Self, Cell, Result);
end;

function TCustomGridView.GetCellText(Cell: TGridCell): string;
begin
  Result := '';
  if Assigned(FOnGetCellText) then
    FOnGetCellText(Self, Cell, Result);
end;

function TCustomGridView.GetCellTextBounds(Cell: TGridCell): TRect;
var
  R: TRect;
  TI: TPoint;
  A: TAlignment;
  WR, WW: Boolean;
  T: string;
begin
  if (Cell.Col < 0) or (Cell.Col > Columns.Count - 1) then
  begin
    Result := System.Classes.Rect(0, 0, 0, 0);
    Exit;
  end;
  if (Cell.Row >= 0) and (Cell.Row < Rows.Count) then
  begin
    GetCellColors(Cell, Canvas);
    TI := GetCellTextIndent(Cell);
    T := GetCellText(Cell);
  end;
  R := System.Classes.Rect(0, 0, 0, 0);
  if Columns[Cell.Col].WordWrap then
  begin
    R := GetEditRect(Cell);
    OffsetRect(R, -R.Left, -R.Top);
    R.Bottom := R.Top;
  end;
  A := Columns[Cell.Col].Alignment;
  WR := Columns[Cell.Col].WantReturns;
  WW := Columns[Cell.Col].WordWrap;
  Result := GetTextRect(Canvas, R, TI.X, TI.Y, A, WR, WW, T);
  OffsetRect(Result, -Result.Left, -Result.Top);
end;

function TCustomGridView.GetCellTextIndent(Cell: TGridCell): TPoint;
begin
  Result.X := TextLeftIndent;
  Result.Y := TextTopIndent;
  if IsCellHasCheck(Cell) or IsCellHasImage(Cell) then
  begin
    Result.X := 2;
    if (Fixed.Count > 0) and (not Fixed.Flat) then
      Inc(Result.Y, 1);
  end;
  if Assigned(FOnGetCellTextIndent) then
    FOnGetCellTextIndent(Self, Cell, Result);
end;

function TCustomGridView.GetCheckAlignment(Cell: TGridCell): TAlignment;
begin
  Result := taLeftJustify;
  if CheckBoxes and (Cell.Col >= 0) and (Cell.Col < Columns.Count) then
  begin
    Result := Columns[Cell.Col].CheckAlignment;
    if Assigned(FOnGetCheckAlignment) then
      FOnGetCheckAlignment(Self, Cell, Result);
  end;
end;

procedure TCustomGridView.GetCheckImage(Cell: TGridCell; CheckImage: TBitmap);
begin
  if Assigned(FOnGetCheckImage) then
    FOnGetCheckImage(Self, Cell, CheckImage);
end;

function TCustomGridView.GetCheckIndent(Cell: TGridCell): TPoint;
begin
  Result.X := CheckLeftIndent;
  Result.Y := CheckTopIndent;
  if (Fixed.Count > 0) and (not Fixed.Flat) then
    Inc(Result.Y, 1);
  if GetCheckAlignment(Cell) = taCenter then
    Result.X := (Columns[Cell.Col].Width - CheckWidth) div 2 - 1;
  if Assigned(FOnGetCheckIndent) then
    FOnGetCheckIndent(Self, Cell, Result);
end;

function TCustomGridView.GetCheckKind(Cell: TGridCell): TGridCheckKind;
begin
  Result := gcNone;
  if CheckBoxes and (Cell.Col >= 0) and (Cell.Col < Columns.Count) then
  begin
    Result := Columns[Cell.Col].CheckKind;
    if Assigned(FOnGetCheckKind) then
      FOnGetCheckKind(Self, Cell, Result);
  end;
end;

function TCustomGridView.GetCheckRect(Cell: TGridCell): TRect;
var
  R: TRect;
begin
  if not IsCellHasCheck(Cell) then
  begin
    Result := System.Classes.Rect(0, 0, 0, 0);
    Exit;
  end;
  R := GetCellRect(Cell);
  with Result do
  begin
    Left := R.Left + GetCheckIndent(Cell).X;
    Right := MinIntValue([Left + CheckWidth, R.Right]);
    Top := R.Top + GetCheckIndent(Cell).Y;
    Bottom := R.Top + CheckHeight;
  end;
end;

function TCustomGridView.GetCheckState(Cell: TGridCell): TCheckBoxState;
begin
  Result := cbUnchecked;
  if Assigned(FOnGetCheckState) then
    FOnGetCheckState(Self, Cell, Result);
end;

function TCustomGridView.GetClientOrigin: TPoint;
begin
  if Parent = nil then
    Result := GetClientRect.TopLeft
  else
    Result := inherited GetClientOrigin;
end;

function TCustomGridView.GetClientRect: TRect;
begin
  if Parent = nil then
    Result := Bounds(0, 0, Width, Height)
  else
    Result := inherited GetClientRect;
end;

function TCustomGridView.GetColumnClass: TGridColumnClass;
begin
  Result := TGridColumn;
end;

function TCustomGridView.GetCursorCell(Cell: TGridCell;
  Offset: TGridCursorOffset): TGridCell;

  function DoMoveLeft(O: Integer): TGridCell;
  var
    I: Integer;
    C: TGridCell;
  begin
    I := MaxIntValue([Cell.Col - O, Fixed.Count]);
    while I >= Fixed.Count do
    begin
      C := GridCell(I, Cell.Row);
      if IsCellAcceptCursor(C) then
      begin
        Result := C;
        Exit;
      end;
      Dec(I);
    end;
    Result := Cell;
  end;

  function DoMoveRight(O: Integer): TGridCell;
  var
    I: Integer;
    C: TGridCell;
  begin
    I := MinIntValue([Cell.Col + O, Columns.Count - 1]);
    while I <= Columns.Count - 1 do
    begin
      C := GridCell(I, Cell.Row);
      if IsCellAcceptCursor(C) then
      begin
        Result := C;
        Exit;
      end;
      Inc(I);
    end;
    Result := Cell;
  end;

  function DoMoveUp(O: Integer): TGridCell;
  var
    J : Integer;
    C : TGridCell;
  begin
    J := MaxIntValue([Cell.Row - O, 0]);
    while J >= 0 do
    begin
      C := GridCell(Cell.Col, J);
      if IsCellAcceptCursor(C) then
      begin
        Result := C;
        Exit;
      end;
      Dec(J);
    end;
    Result := Cell;
  end;

  function DoMoveDown(O: Integer; Append : Boolean = False): TGridCell;
  var
    B : Boolean;
    J : Integer;
    C : TGridCell;
  begin
    if Append and ((Cell.Row + O) - (Rows.Count - 1) = 1) then
    begin
      B := False;
      DoAppendRow(B);
      if B then
        Rows.Count := Rows.Count + 1;
    end;

    J := MinIntValue([Cell.Row + O, Rows.Count - 1]);
    while J <= Rows.Count - 1 do
    begin
      C := GridCell(Cell.Col, J);
      if IsCellAcceptCursor(C) then
      begin
        Result := C;
        Exit;
      end;
      Inc(J);
    end;
    Result := Cell;
  end;

  function DoMoveHome: TGridCell;
  var
    C : TGridCell;
  begin
    C := Cell;
    try
      Cell.Col := Fixed.Count;
      Result := DoMoveRight(0);
    finally
      Cell := C;
    end;
  end;

  function DoMoveEnd: TGridCell;
  var
    C : TGridCell;
  begin
    C := Cell;
    try
      Cell.Col := Columns.Count - 1;
      Result := DoMoveLeft(0);
    finally
      Cell := C;
    end;
  end;

  function DoMoveGridHome: TGridCell;
  var
    I : Integer;
    J : Integer;
    C : TGridCell;
  begin
    I := Fixed.Count;
    while I <= Cell.Col do
    begin
      J := 0;
      while J <= Cell.Row do
      begin
        C := GridCell(I, J);
        if IsCellAcceptCursor(C) then
        begin
          Result := C;
          Exit;
        end;
        Inc(J);
      end;
      Inc(I);
    end;
    Result := Cell;
  end;

  function DoMoveGridEnd: TGridCell;
  var
    I : Integer;
    J : Integer;
    C : TGridCell;
  begin
    I := Columns.Count - 1;
    while I >= Cell.Col do
    begin
      J := Rows.Count - 1;
      while J >= Cell.Row do
      begin
        C := GridCell(I, J);
        if IsCellAcceptCursor(C) then
        begin
          Result := C;
          Exit;
        end;
        Dec(J);
      end;
      Dec(I);
    end;
    Result := Cell;
  end;

  function DoSelect: TGridCell;

    function DoSelectLeft: TGridCell;
    var
      I : Integer;
      C : TGridCell;
    begin
      I := MaxIntValue([Cell.Col, Fixed.Count]);
      while I <= CellFocused.Col do
      begin
        C := GridCell(I, Cell.Row);
        if IsCellAcceptCursor(C) then
        begin
          Result := C;
          Exit;
        end;
        Inc(I);
      end;
      Result := Cell;
    end;

    function DoSelectRight: TGridCell;
    var
      I : Integer;
      C : TGridCell;
    begin
      I := MinIntValue([Cell.Col, Columns.Count - 1]);
      while I >= CellFocused.Col do
      begin
        C := GridCell(I, Cell.Row);
        if IsCellAcceptCursor(C) then
        begin
          Result := C;
          Exit;
        end;
        Dec(I);
      end;
      Result := Cell;
    end;

    function DoSelectUp: TGridCell;
    var
      J : Integer;
      C : TGridCell;
    begin
      J := MaxIntValue([Cell.Row, 0]);
      while J <= CellFocused.Row do
      begin
        C := GridCell(Cell.Col, J);
        if IsCellAcceptCursor(C) then
        begin
          Result := C;
          Exit;
        end;
        Inc(J);
      end;
      Result := Cell;
    end;

    function DoSelectDown: TGridCell;
    var
      J : Integer;
      C : TGridCell;
    begin
      J := MinIntValue([Cell.Row, Rows.Count - 1]);
      while J >= CellFocused.Row do
      begin
        C := GridCell(Cell.Col, J);
        if IsCellAcceptCursor(C) then
        begin
          Result := C;
          Exit;
        end;
        Dec(J);
      end;
      Result := Cell;
    end;
  begin
    if IsCellAcceptCursor(Cell) then
    begin
      Result := Cell;
      Exit;
    end;
    if Cell.Col < CellFocused.Col then
    begin
      Result := DoSelectLeft;
      if IsCellAcceptCursor(Result) then
        Exit;
    end;
    if Cell.Col > CellFocused.Col then
    begin
      Result := DoSelectRight;
      if IsCellAcceptCursor(Result) then
        Exit;
    end;
    if Cell.Row < CellFocused.Row then
    begin
      Result := DoSelectUp;
      if IsCellAcceptCursor(Result) then
        Exit;
    end;
    if Cell.Row > CellFocused.Row then
    begin
      Result := DoSelectDown;
      if IsCellAcceptCursor(Result) then
        Exit;
    end;
    Result := CellFocused;
  end;

  function DoFirst: TGridCell;
  var
    C : TGridCell;
    I : Integer;
    J : Integer;
  begin
    J := 0;
    while J <= Rows.Count - 1 do
    begin
      I := Fixed.Count;
      while I <= Columns.Count - 1 do
      begin
        C := GridCell(I, J);
        if IsCellAcceptCursor(C) then
        begin
          Result := C;
          Exit;
        end;
        Inc(I);
      end;
      Inc(J);
    end;
    Result := CellFocused;
  end;

  function DoNext: TGridCell;
  var
    C : TGridCell;
    I : Integer;
    J : Integer;
    B : Boolean;
  begin
    I := Cell.Col + 1;
    J := Cell.Row;
    while J <= Rows.Count - 1 do
    begin
      while I <= Columns.Count - 1 do
      begin
        C := GridCell(I, J);
        if IsCellAcceptCursor(C) and ((not RowSelect) or (C.Row <> Cell.Row)) then
        begin
          Result := C;
          Exit;
        end;
        Inc(I);
      end;
      I := Fixed.Count;
      Inc(J);
      if J = Rows.Count then
      begin
        B := False;
        DoAppendRow(B);
        if B then
          Rows.Count := Rows.Count + 1;
      end;
    end;
    Result := CellFocused;
  end;

  function DoPrev: TGridCell;
  var
    C : TGridCell;
    I : Integer;
    J : Integer;
  begin
    I := Cell.Col - 1;
    J := Cell.Row;
    while J >= 0 do
    begin
      while I >= Fixed.Count do
      begin
        C := GridCell(I, J);
        if IsCellAcceptCursor(C) and ((not RowSelect) or (C.Row <> Cell.Row)) then
        begin
          Result := C;
          Exit;
        end;
        Dec(I);
      end;
      I := Columns.Count - 1;
      Dec(J);
    end;
    Result := CellFocused;
  end;

begin
  case Offset of
    goLeft:
      {displacement to the column to the left}
      Result := DoMoveLeft(1);
    goRight:
      {displacement to the right to one column}
      Result := DoMoveRight(1);
    goUp:
      {upward bias to one column}
      Result := DoMoveUp(1);
    goDown:
      {downward bias to one column}
      Result := DoMoveDown(1, True);
    goMouseWheelUp:
      Result := DoMoveUp(1);
    goMouseWheelDown:
      Result := DoMoveDown(1);
    goPageUp:
      {displacement to the page upward}
      Result := DoMoveUp(VisSize.Row - 1);
    goPageDown:
      {displacement to the page downward}
      Result := DoMoveDown(VisSize.Row - 1);
    goHome:
      {into the beginning of line}
      Result := DoMoveHome;
    goEnd:
      {into the end of the line}
      Result := DoMoveEnd;
    goGridHome:
      {into the beginning of table}
      Result := DoMoveGridHome;
    goGridEnd:
      {into the end of the table}
      Result := DoMoveGridEnd;
    goSelect:
      {checking cell}
      Result := DoSelect;
    goFirst:
      {to select the first possible cell}
      Result := DoFirst;
    goNext:
      {to select the following cell}
      Result := DoNext;
    goPrev:
      {to select the previous possible cell}
      Result := DoPrev;
  else
    Result := Cell;
  end;
end;

function TCustomGridView.GetEditClass(Cell: TGridCell): TGridEditClass;
begin
  Result := TGridEdit;
end;

procedure TCustomGridView.GetEditList(Cell: TGridCell; Items: TStrings);
begin
  if (Cell.Col >= 0) and (Cell.Col < Columns.Count) then
  begin
    with Columns[Cell.Col] do
      if (EditStyle = gePickList) and (FPickList <> nil) then
        Items.Assign(FPickList);
    if Assigned(FOnGetEditList) then
      FOnGetEditList(Self, Cell, Items);
  end;
end;

procedure TCustomGridView.GetEditListBounds(Cell: TGridCell; var Rect: TRect);
begin
  if Assigned(FOnGetEditListBounds) then
    FOnGetEditListBounds(Self, Cell, Rect);
end;

function TCustomGridView.GetEditMask(Cell: TGridCell): string;
begin
  Result := '';
  if (Cell.Col >= 0) and (Cell.Col < Columns.Count) then
  begin
    Result := Columns[Cell.Col].EditMask;
    if Assigned(FOnGetEditMask) then
      FOnGetEditMask(Self, Cell, Result);
  end;
end;

function TCustomGridView.GetEditStyle(Cell: TGridCell): TGridEditStyle;
begin
  Result := geSimple;
  if (Cell.Col >= 0) and (Cell.Col < Columns.Count) then
  begin
    Result := Columns[Cell.Col].EditStyle;
    if Assigned(FOnGetEditStyle) then
      FOnGetEditStyle(Self, Cell, Result);
  end;
end;

function TCustomGridView.GetEditText(Cell: TGridCell): string;
begin
  Result := GetCellText(Cell);
  if Assigned(FOnGetEditText) then
    FOnGetEditText(Self, Cell, Result);
end;

procedure TCustomGridView.GetHeaderColors(Section: TGridHeaderSection;
  Canvas: TCanvas);
begin
  Canvas.Brush.Color := Header.Color;
  Canvas.Font := Header.Font;
  if Assigned(FOnGetHeaderColors) then
    FOnGetHeaderColors(Self, Section, Canvas);
end;

function TCustomGridView.GetGridLineColor(BkColor: TColor): TColor;
const
  LineColors: array[Boolean] of TColor = (clSilver, clGray);
begin
  Result := FGridColor;
  if ColorToRGB(Result) = ColorToRGB(BkColor) then
    Result := Result xor clGray;
end;

function TCustomGridView.GetHeaderImage(Section: TGridHeaderSection): Integer;
begin
  if not Assigned(Header.Images) then
  begin
    Result := -1;
    Exit;
  end;
  Result := Section.ColumnIndex;
  if Assigned(FOnGetHeaderImage) then
    FOnGetHeaderImage(Self, Section, Result);
end;

function TCustomGridView.GetSortDirection(Section: TGridHeaderSection):
  TGridSortDirection;
begin
  Result := gsNone;
  if Assigned(FOnGetSortDirection) then
    FOnGetSortDirection(Self, Section, Result);
end;

procedure TCustomGridView.GetSortImage(Section: TGridHeaderSection;
  SortImage: TBitmap);
begin
  if Assigned(FOnGetSortImage) then
    FOnGetSortImage(Self, Section, SortImage);
end;

function TCustomGridView.GetTextRect(Canvas: TCanvas; Rect: TRect;
  LeftIndent, TopIndent: Integer; Alignment: TAlignment;
  WantReturns, WordWrap: Boolean; const Text: string): TRect;
var
  R: TRect;
  P: TDrawTextParams;
  F, W, H, I: Integer;
begin
  { check how the text is derived: with the aid of DrawTextEx or TextOut}
  if WantReturns or WordWrap or EndEllipsis then
  begin
    {the parameters of the conclusion of text}
    FillChar(P, SizeOf(P), 0);
    P.cbSize := SizeOf(P);
    P.iLeftMargin := LeftIndent;
    P.iRightMargin := TextRightIndent;
    {the attributes of text}
    F := DT_NOPREFIX;
    {horizontal levelling off}
    case Alignment of
      taLeftJustify:
        F := F or DT_LEFT;
      taCenter:
        F := F or DT_CENTER;
      taRightJustify:
        F := F or DT_RIGHT;
    end;
    {vertical levelling off}
    if not (WantReturns or WordWrap) then
    begin
      {the automatic flare function}
      F := F or DT_SINGLELINE or DT_VCENTER;
      {dots at the end we do not consider}
    end;
    {the transfer of words}
    if WordWrap then
      F := F or DT_WORDBREAK;
    {the rectangle of text}
    R := Rect;
    { draw text}
    DrawTextEx(Canvas.Handle, PChar(Text), Length(Text), R, F or DT_CALCRECT, @P);
    {the dimensions of text}
    W := MaxIntValue([Rect.Right - Rect.Left, R.Right - R.Left]);
    H := MaxIntValue([Rect.Bottom - Rect.Top, R.Bottom - R.Top]);
  end
  else
  begin
    {the displacement of text to the left}
    I := LeftIndent;
    {height and the width of text}
    W := MaxIntValue([Rect.Right - Rect.Left, I + Canvas.TextWidth(Text) + TextRightIndent]);
    H := MaxIntValue([Rect.Bottom - Rect.Top, Canvas.TextHeight(Text)]);
  end;
  { form rectangle}
  case Alignment of
    taCenter:
      begin
        R.Left := Rect.Left - (W - (Rect.Right - Rect.Left)) div 2;
        R.Right := R.Left + W;
      end;
    taRightJustify:
      begin
        R.Right := Rect.Right;
        R.Left := R.Right - W;
      end;
  else
    R.Left := Rect.Left;
    R.Right := R.Left + W;
  end;
  R.Top := Rect.Top;
  R.Bottom := R.Top + H;
  Result := R;
end;

function TCustomGridView.GetTipsRect(Cell: TGridCell): TRect;
var
  R: TRect;
  TI: TPoint;
  A: TAlignment;
  WR, WW: Boolean;
  T: string;
begin
  { check cell}
  if not IsCellValid(Cell) then
  begin
    Result := Rect(0, 0, 0, 0);
    Exit;
  end;
  { calculate rectangle}
  with GetTipsWindowClass.Create(Self) do
  try
    GetCellColors(Cell, Canvas);
    {the parameters of painting}
    R := GetEditRect(Cell);
    TI := GetCellTextIndent(Cell);
    A := Columns[Cell.Col].Alignment;
    WR := Columns[Cell.Col].WantReturns;
    WW := Columns[Cell.Col].WordWrap;
    T := GetTipsText(Cell);
    { count rectangle}
    R := GetTextRect(Canvas, R, TI.X, TI.Y, A, WR, WW, T);
  finally
    Free;
  end;
  {for the text, it is more than the height of line - correction}
  if R.Bottom - R.Top > Rows.Height then
    Inc(R.Bottom, TextTopIndent * 2); {!}
  { consider border}
  InflateRect(R, 1, 1);
  Result := R;
  if Assigned(FOnGetTipsRect) then
    FOnGetTipsRect(Self, Cell, Result);
end;

function TCustomGridView.GetTipsText(Cell: TGridCell): string;
begin
  Result := GetCellText(Cell);
  if Assigned(FOnGetTipsText) then
    FOnGetTipsText(Self, Cell, Result);
end;

function TCustomGridView.GetTipsWindowClass: TGridTipsWindowClass;
begin
  Result := TGridTipsWindow;
end;

procedure TCustomGridView.HeaderClick(Section: TGridHeaderSection);
begin
  if Assigned(FOnHeaderClick) then
    FOnHeaderClick(Self, Section);
end;

procedure TCustomGridView.HeaderClicking(Section: TGridHeaderSection;
  var AllowClick: Boolean);
begin
  {  by default you can click only on the lower sections }
  AllowClick := ColumnClick and Section.AllowClick and (Section.Sections.Count = 0);
  if Assigned(FOnHeaderClicking) then
    FOnHeaderClicking(Self, Section, AllowClick);
end;

procedure TCustomGridView.HideCursor;
begin
  if IsFocusAllowed then
    InvalidateFocus
  else
    HideEdit;
end;

procedure TCustomGridView.HideEdit;
begin
  if FEdit <> nil then
  begin
    FEditCell := GridCell(-1, -1);
    FEdit.Hide;
  end;
end;

procedure TCustomGridView.HideFocus;
begin
  if IsFocusAllowed then
    PaintFocus;
end;

procedure TCustomGridView.KeyDown(var Key: Word; Shift: TShiftState);
const
  HomeOffsets: array[Boolean] of TGridCursorOffset = (goHome, goGridHome);
  EndOffsets: array[Boolean] of TGridCursorOffset = (goEnd, goGridEnd);
  TabOffsets: array[Boolean] of TGridCursorOffset = (goNext, goPrev);
var
  Cell: TGridCell;
begin
  inherited KeyDown(Key, Shift);
  if gkArrows in CursorKeys then
    case Key of
      VK_LEFT:
        begin
          SetCursor(GetCursorCell(CellFocused, goLeft), True, True);
          if RowSelect then
            with HorzScrollBar do
              SetPosition(Position - LineStep);
        end;
      VK_RIGHT:
        begin
          SetCursor(GetCursorCell(CellFocused, goRight), True, True);
          if RowSelect then
            with HorzScrollBar do
              SetPosition(Position + LineStep);
        end;
      VK_UP:
        begin
          {if there is no focus, then we displace entire table}
          if not AllowSelect then
            Cell := VisOrigin
          else
            Cell := CellFocused;
          { change that isolated}
          SetCursor(GetCursorCell(Cell, goUp), True, True);
        end;
      VK_DOWN:
        begin
          {if there is no focus, then we displace entire table}
          if not AllowSelect then
          begin
            Cell := GridCell(VisOrigin.Col, VisOrigin.Row + VisSize.Row - 1);
            if not IsCellVisible(Cell, False) then
              Dec(Cell.Row);
          end
          else
            Cell := CellFocused;
          { change that isolated}
          SetCursor(GetCursorCell(Cell, goDown), True, True)
        end;
      VK_PRIOR:
        {cursor to the page upward}
        SetCursor(GetCursorCell(CellFocused, goPageUp), True, True);
      VK_NEXT:
        {cursor to the page downward}
        SetCursor(GetCursorCell(CellFocused, goPageDown), True, True);
      VK_HOME:
        {cursor into the beginning of line or table}
        begin
          Cell := GetCursorCell(CellFocused, HomeOffsets[ssCtrl in Shift]);
          SetCursor(Cell, True, True);
        end;
      VK_END:
        {cursor into the end of the line or table}
        begin
          Cell := GetCursorCell(CellFocused, EndOffsets[ssCtrl in Shift]);
          SetCursor(Cell, True, True);
        end;
    end;
  { ADDED TS: use F2 to force edit mode }
  if Key = VK_F2 then
    Editing := True;

  {cursor to the following or previous cell with pressure TAB}
  if (gkTabs in CursorKeys) and (Key = VK_TAB) then
    SetCursor(GetCursorCell(CellFocused, TabOffsets[ssShift in Shift]), True, True);

  if (Key = VK_SPACE) and (not EditCanShow(CellFocused)) and CheckBoxes then
    if GetCheckKind(CellFocused) <> gcNone then
      CheckClick(CellFocused);

  { ADDED TS : autosize columns }
  if ([ssCtrl, ssShift] <= Shift) and (Key = VK_ADD) then
  begin
    AutoSizeCols;
    Key := 0;
  end;
end;

procedure TCustomGridView.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  { Handle <RETURN> }
  if Key = #13 then
  begin
    { Edit cell }
    if Editing then
    begin
      ApplyEdit;
    end;
    { Position cursor to the next (selectable) cell }
    if gkReturn in CursorKeys then
    begin
      SetCursor(GetCursorCell(CellFocused, goNext), True, True);
    end;
  end
  { Handle <ESCAPE> key (terminates edit mode if applicable). }
  else if Key = #27 then
  begin
    if Editing then
    begin
      { Cancel input in text editor and restore cell value. }
      if not AlwaysEdit then
        CancelEdit
      else
        UndoEdit;
    end
  end;
end;

function TCustomGridView.LastSelectableCol(ARow: Integer): Integer;
var
  I : Integer;
  C : TGridCell;
  B : Boolean;
begin
  Result := -1;
  C.Row := ARow;
  for I := 0 to Columns.Count - 1 do
  begin
    C.Col := I;
    B := Columns[I].Visible and Columns[I].TabStop and
         (not Columns[I].ReadOnly) and (not IsCellReadOnly(C)) and
         IsCellAcceptCursor(C);
    if B then
      Result := I;
  end;
end;

function TCustomGridView.LimitColumnWidth(ACol, AWidth: Integer): Integer;
var
  N : Integer;
begin
  if (Columns[ACol].MaxWidth > 0) and (AWidth > Columns[ACol].MaxWidth) then
    N := Columns[ACol].MaxWidth
  else if (Columns[ACol].MinWidth > 0) and (AWidth < Columns[ACol].MinWidth)
  then
    N := Columns[ACol].MinWidth
  else if Columns[ACol].Caption <> '' then
    N := Max(GetTextWidth(Columns[ACol].Caption, Font) + 8,  AWidth)
  else
    N := AWidth;
  Result := N;
end;

procedure TCustomGridView.Loaded;
begin
  inherited Loaded;
  UpdateFixed;
  UpdateHeader;
  UpdateColors;
  UpdateFonts;
  UpdateEdit(AlwaysEdit);
  FCellSelected := AlwaysSelected;
  UpdateCursor;
end;

procedure TCustomGridView.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  S: TGridHeaderSection;
  I, W: Integer;
  C, P: TGridCell;
  AllowClicking: Boolean;
begin
  if not AcquireFocus then
  begin
    MouseCapture := False;
    Exit;
  end;
  if Button = mbLeft then
    if ShowHeader and PtInRect(GetHeaderRect, Point(X, Y)) then
    begin
      S := GetResizeSectionAt(X, Y);
      if S <> nil then
      begin
        if ssDouble in Shift then
        begin
          I := S.ResizeColumnIndex;
          if I < Columns.Count then
          begin
            W := MinIntValue([Columns[I].MaxWidth, GetColumnMaxWidth(I)]);
            ColumnAutoSize(I, W);
            ColumnResize(I, W);
            FColResizing := True;
            try
              Columns[I].Width := W;
            finally
              FColResizing := False;
            end;
          end;
        end
        else
          StartColResize(S, X, Y);
      end
      else if not (ssDouble in Shift) then
      begin
        S := GetSectionAt(X, Y);
        if S <> nil then
        begin
          AllowClicking := True;
          HeaderClicking(S, AllowClicking);
          if AllowClicking then
            StartHeaderClick(S, X, Y);
        end;
      end;
      Exit;
    end;
  if (Button = mbLeft) or ((Button = mbRight) and RightClickSelect) then
    if (gkMouse in CursorKeys) and (PtInRect(GetGridRect, Point(X, Y))) then
    begin
      if RowSelect and (gsFullRowPaint in GridStyle) then
      begin
        C.Row := GetRowAt(X, Y);
        C.Col := 0;
      end
      else
        C := GetCellAt(X, Y);
      P := FClickPos;
      FClickPos := GridCell(-1, -1);
      if IsCellEmpty(C) then
      begin
        Editing := False;
        SetCursor(CellFocused, False, False);
      end
      else
      begin
        SetCursor(C, True, True);
        CellClick(C, Shift, X, Y);
        if PtInRect(GetCheckRect(C), Point(X, Y)) then
        begin
          CheckClick(C);
          Exit;
        end;
        if (Button = mbLeft) and IsCellEqual(C, CellFocused) and AllowEdit then
          if (ssDouble in Shift) or IsCellEqual(C, P) then
          begin
            Editing := True;
            if Editing then
              Exit;
          end;
      end;
      FClickPos := C;
    end;
  if Button = mbRight then
  begin
    if FColResizing then
    begin
      StopColResize(True);
      Exit;
    end;
    if FHeaderClicking then
    begin
      StopHeaderClick(True);
      Exit;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomGridView.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  C: TGridCell;
begin
  if FColResizing then
  begin
    StepColResize(X, Y);
    Exit;
  end;
  if FHeaderClicking then
  begin
    StepHeaderClick(X, Y);
    Exit;
  end;
  if (ssLeft in Shift) or ((ssRight in Shift) and RightClickSelect) then
    if gkMouseMove in CursorKeys then
    begin
      C := GetCellAt(X, Y);
      if (not IsCellEmpty(C)) and (not IsCellEqual(C, CellFocused)) then
      begin
        SetCursor(C, True, True);
        if IsCellEqual(C, CellFocused) and AlwaysEdit then
        begin
          Editing := True;
          if Editing then
            Exit;
        end;
      end;
    end;
  if FThemingEnabled and StyleServices.Enabled then
  begin
    UpdateHotHeader(GetSectionAt(X, Y));
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TCustomGridView.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if FColResizing then
  begin
    StopColResize(False);
    Exit;
  end;
  if FHeaderClicking then
  begin
    StopHeaderClick(False);
    Exit;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCustomGridView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = Images then
      Images := nil;
    if (Header <> nil) and (AComponent = Header.Images) then
      Header.Images := nil;
    if AComponent = FEdit then
    begin
      FEdit := nil;
      FEditCell := GridCell(-1, -1);
      FEditing := False;
    end;
  end;
end;

procedure TCustomGridView.Paint;
var
  DefDraw: Boolean;
  R: TRect;
begin
  DefDraw := True;
  try
    if Assigned(FOnDraw) then
      FOnDraw(Self, DefDraw);
  except
    Application.HandleException(Self);
  end;
  if not DefDraw then
    Exit;
  with GetClientRect do
  begin
    ExcludeClipRect(Canvas.Handle, 0, Top, Left, Bottom);
    ExcludeClipRect(Canvas.Handle, Left, 0, Right, Top);
    ExcludeClipRect(Canvas.Handle, Right, Top, Width, Bottom);
    ExcludeClipRect(Canvas.Handle, Left, Bottom, Right, Height);
  end;
  if ShowHeader and RectVisible(Canvas.Handle, GetHeaderRect) then
  begin
    PaintHeaders(True);
    with GetHeaderRect do
    begin
      R := GetFixedRect;
      ExcludeClipRect(Canvas.Handle, R.Left, Top, R.Right, Bottom);
    end;
    PaintHeaders(False);
    with GetHeaderRect do
      ExcludeClipRect(Canvas.Handle, Left, Top, Right, Bottom);
  end;
  PaintFreeField;
  if (Fixed.Count > 0) and RectVisible(Canvas.Handle, GetFixedRect) then
  begin
    PaintFixed;
    if GridLines then
      PaintFixedGrid;
    with GetFixedRect do
      ExcludeClipRect(Canvas.Handle, Left, Top, Right, Bottom);
  end;
  if (VisSize.Col > 0) and (VisSize.Row > 0) then
  begin
    if Editing then
      with GetEditRect(EditCell) do
        ExcludeClipRect(Canvas.Handle, Left, Top, Right, Bottom);
    PaintCells;
    if IsFocusAllowed then
      PaintFocus;
  end;
  if GridLines then
    PaintGridLines;

  if FColResizing and (FColResizeCount > 0) and not FColumnsFullDrag then
    PaintResizeLine;
end;

function GetRGBColor(Value: TColor): DWORD;
begin
  Result := ColorToRGB(Value);
  case Result of
    clNone:
      Result := CLR_NONE;
    clDefault:
      Result := CLR_DEFAULT;
  end;
end;

procedure TCustomGridView.Paint3DFrame(Rect: TRect; SideFlags: Integer);
begin
  PaintScrollBtnFrameEx(Canvas.Handle, Rect, False, SideFlags);
end;

procedure TCustomGridView.PaintCell(Cell: TGridCell; Rect: TRect);
var
  DefDraw: Boolean;
begin
// TS TEST (required?)
  GetCellColors(Cell, Canvas);
  DefDraw := True;
  try
    if Assigned(FOnDrawCell) then
      FOnDrawCell(Self, Cell, Rect, DefDraw);
  except
    Application.HandleException(Self);
  end;
  if DefDraw then
    DefaultDrawCell(Cell, Rect);
end;

procedure TCustomGridView.PaintCells;
var
  I, J    : Integer;
  L, T, W : Integer;
  R       : TRect;
  C       : TGridCell;
begin
  L := GetColumnRect(VisOrigin.Col).Left;
  T := GetRowRect(VisOrigin.Row).Top;
  R.Bottom := T;
  for J := 0 to FVisSize.Row - 1 do
  begin
    R.Top := R.Bottom;
    R.Bottom := R.Bottom + Rows.Height;
    R.Right := L;
    for I := 0 to FVisSize.Col - 1 do
    begin
      C := GridCell(VisOrigin.Col + I, VisOrigin.Row + J);
      W := Columns[C.Col].Width;
      if W > 0 then
      begin
        R.Left := R.Right;
        R.Right := R.Right + W;
        if RectVisible(Canvas.Handle, R) then
          PaintCell(C, R);
      end;
    end;
  end;
end;

procedure TCustomGridView.PaintDotGridLines(Points: Pointer; Count: Integer);
var
  P: PIntArray absolute Points;
  I: Integer;
  R: TRect;
begin
  PreparePatternBitmap(Canvas, GetGridLineColor(Color), False);
  try
    I := 0;
    while I < Count * 2 do
    begin
      R.Left := P^[I];
      Inc(I);
      R.Top := P^[I];
      Inc(I);
      R.Right := P^[I];
      Inc(I);
      R.Bottom := P^[I];
      Inc(I);
      if (R.Left = R.Right) and (R.Top <> R.Bottom) then
        Inc(R.Right)
      else if (R.Left <> R.Right) and (R.Top = R.Bottom) then
        Inc(R.Bottom);
      Canvas.FillRect(R);
    end;
  finally
    PreparePatternBitmap(Canvas, GetGridLineColor(Color), True);
  end;
end;

{TODO -oTS -cGeneral : Add theming support}

procedure TCustomGridView.PaintFixed;
var
  I, J, W: Integer;
  R: TRect;
  C: TGridCell;
begin
  R.Bottom := GetRowRect(VisOrigin.Row).Top;
  for J := 0 to FVisSize.Row - 1 do
  begin
    R.Top := R.Bottom;
    R.Bottom := R.Bottom + Rows.Height;
    R.Right := GetGridRect.Left;
    for I := 0 to Fixed.Count - 1 do
    begin
      C := GridCell(I, VisOrigin.Row + J);
      W := Columns[C.Col].Width;
      if W > 0 then
      begin
        R.Left := R.Right;
        R.Right := R.Right + W;
        if RectVisible(Canvas.Handle, R) then
          PaintCell(C, R);
      end;
    end;
  end;
  if Fixed.Flat and (Fixed.ShowDivider or (gsFullVertLine in GridStyle)) then
  begin
    R := GetFixedRect;
    if Fixed.GridColor then
    begin
      if not (gsDotLines in GridStyle) then
      begin
        Canvas.Pen.Color := GetGridLineColor(Color);
        Canvas.Pen.Width := FGridLineWidth;
        Canvas.MoveTo(R.Right - 1, R.Bottom - 1);
        Canvas.LineTo(R.Right - 1, R.Top - 1);
      end
      else
      begin
        R.Left := R.Right - 1;
        PaintDotGridLines(@R, 2);
      end;
    end
    else
      with Canvas do
      begin
        Pen.Color := clBtnShadow;
        Pen.Width := 1;
        MoveTo(R.Right - 2, R.Top - 1);
        LineTo(R.Right - 2, R.Bottom);
        Pen.Color := clBtnHighlight;
        MoveTo(R.Right - 1, R.Bottom - 1);
        LineTo(R.Right - 1, R.Top - 1);
      end;
  end;
  if FColResizing and (FColResizeCount > 0) and not FColumnsFullDrag then
    PaintResizeLine;
end;

procedure TCustomGridView.PaintFixedGrid;
var
  Points: PIntArray;
  PointCount: Integer;
  StrokeList: PIntArray;
  StrokeCount: Integer;
  I, L, R, T, B, X, Y, C, W: Integer;
  Index: Integer;
  Rect: TRect;

  procedure ShiftGridPoints(DX, DY: Integer);
  var
    I: Integer;
  begin
    I := 0;
    while I < Fixed.Count * Ord(gsVertLine in GridStyle) * 4 do
    begin
      if I mod 2 = 0 then
        Points^[I] := Points^[I] + DX;
      Inc(I);
    end;
    while I < PointCount * 2 do
    begin
      if I mod 2 = 1 then
        Points^[I] := Points^[I] + DY;
      Inc(I);
    end;
  end;

  procedure Paint3DCells(Rect: TRect);
  var
    I: Integer;
    R: TRect;
  begin
    R := Rect;
    R.Bottom := R.Top;
    while R.Bottom < Rect.Bottom do
    begin
      R.Top := R.Bottom;
      R.Bottom := R.Bottom + Rows.Height;
      R.Right := GetFixedRect.Left;
      for I := 0 to Fixed.Count - 1 do
      begin
        W := Columns[I].Width;
        if W > 0 then
        begin
          R.Left := R.Right;
          R.Right := R.Right + W;
          if RectVisible(Canvas.Handle, R) then
            Paint3DFrame(R, BF_RECT);
        end;
      end;
    end;
  end;

  procedure PaintHorz3DLines(Rect: TRect);
  var
    R: TRect;
  begin
    R := Rect;
    R.Bottom := R.Top;
    repeat
      R.Top := R.Bottom;
      R.Bottom := R.Bottom + Rows.Height;
      if RectVisible(Canvas.Handle, R) then
        Paint3DFrame(R, BF_RECT);
    until R.Bottom >= Rect.Bottom;
  end;

  procedure PaintVert3DLines(Rect: TRect; DrawBottomLine: Boolean);
  const
    Flags: array[Boolean] of Integer = (BF_TOPLEFT or BF_RIGHT, BF_RECT);
  var
    I: Integer;
    R: TRect;
  begin
    R := Rect;
    R.Right := R.Left;
    for I := 0 to Fixed.Count - 1 do
    begin
      W := Columns[I].Width;
      if W > 0 then
      begin
        R.Left := R.Right;
        R.Right := R.Right + W;
        if RectVisible(Canvas.Handle, R) then
          Paint3DFrame(R, Flags[DrawBottomLine]);
      end;
    end
  end;

  procedure PaintBottom3DMargin(Rect: TRect);
  begin
    if RectVisible(Canvas.Handle, Rect) then
      Paint3DFrame(Rect, BF_LEFT or BF_TOP or BF_RIGHT);
  end;

begin
  if Fixed.Flat then
  begin
    StrokeCount := 0;
    if gsHorzLine in GridStyle then
    begin
      StrokeCount := FVisSize.Row;
      if gsListViewLike in GridStyle then
        StrokeCount := GetGridHeight div Rows.Height;
    end;
    if gsVertLine in GridStyle then
      StrokeCount := StrokeCount + Fixed.Count;
    if StrokeCount > 0 then
    begin
      PointCount := StrokeCount * 2;
      StrokeList := AllocMem(StrokeCount * SizeOf(Integer));
      Points := AllocMem(PointCount * SizeOf(TPoint));
      FillDWord(StrokeList^, StrokeCount, 2);
      Rect := GetFixedRect;
      if gsVertLine in GridStyle then
      begin
        T := Rect.Top;
        B := Rect.Bottom;
        if [gsFullVertLine, gsListViewLike] * GridStyle = [] then
          B := GetRowRect(VisOrigin.Row + VisSize.Row).Top;
        X := Rect.Left;
        for I := 0 to Fixed.Count - 1 do
        begin
          X := X + Columns[I].Width;
          Index := I * 4;
          Points^[Index + 0] := X - 2;
          Points^[Index + 1] := T;
          Points^[Index + 2] := X - 2;
          Points^[Index + 3] := B;
        end;
      end;
      if gsHorzLine in GridStyle then
      begin
        L := Rect.Left;
        R := Rect.Right;
        Y := GetRowRect(VisOrigin.Row).Top;
        C := FVisSize.Row;
        if gsListViewLike in GridStyle then
          C := GetGridHeight div Rows.Height;
        for I := 0 to C - 1 do
        begin
          Y := Y + Rows.Height;
          Index := Fixed.Count * Ord(gsVertLine in GridStyle) * 4 + I * 4;
          Points^[Index + 0] := L;
          Points^[Index + 1] := Y - 2;
          Points^[Index + 2] := R;
          Points^[Index + 3] := Y - 2;
        end;
      end;
      if Fixed.GridColor then
        with Canvas do
        begin
          ShiftGridPoints(1, 1);
          if not (gsDotLines in GridStyle) then
          begin
            Pen.Color := GetGridLineColor(Color);
            Pen.Width := FGridLineWidth;
            PolyPolyLine(Handle, Points^, StrokeList^, StrokeCount);
          end
          else
            PaintDotGridLines(Points, PointCount);
        end
      else
        with Canvas do
        begin
          Pen.Color := clBtnShadow;
          Pen.Width := 1;
          PolyPolyLine(Handle, Points^, StrokeList^, StrokeCount);
          ShiftGridPoints(1, 1);
          Pen.Color := clBtnHighlight;
          PolyPolyLine(Handle, Points^, StrokeList^, StrokeCount);
        end;
      FreeMem(Points);
      FreeMem(StrokeList);
    end;
  end
  else if (gsHorzLine in GridStyle) and (gsVertLine in GridStyle) then
  begin
    Rect := GetFixedRect;
    if not (gsListViewLike in GridStyle) then
      Rect.Bottom := Rect.Top + FVisSize.Row * Rows.Height;
    Paint3DCells(Rect);
    if not (gsListViewLike in GridStyle) then
    begin
      Rect.Top := Rect.Bottom;
      Rect.Bottom := GetFixedRect.Bottom;
      if gsFullVertLine in GridStyle then
        PaintVert3DLines(Rect, False)
      else
        PaintBottom3DMargin(Rect);
    end;
  end
  else if (gsHorzLine in GridStyle) and (not (gsVertLine in GridStyle)) then
  begin
    Rect := GetFixedRect;
    if not (gsListViewLike in GridStyle) then
      Rect.Bottom := Rect.Top + FVisSize.Row * Rows.Height;
    PaintHorz3DLines(Rect);
    if not (gsListViewLike in GridStyle) then
    begin
      Rect.Top := Rect.Bottom;
      Rect.Bottom := GetFixedRect.Bottom;
      PaintBottom3DMargin(Rect);
    end;
  end
  else if (not (gsHorzLine in GridStyle)) and (gsVertLine in GridStyle) then
  begin
    Rect := GetFixedRect;
    PaintVert3DLines(Rect, False);
  end
  else
  begin
    Rect := GetFixedRect;
    PaintBottom3DMargin(Rect);
  end;
end;

procedure TCustomGridView.PaintFocus;
var
  R: TRect;
begin
  if ShowFocusRect and Focused and (VisSize.Row > 0) and (not Editing) then
  begin
    R := GetFocusRect;
    if GridLines then
    begin
      if gsVertLine in GridStyle then
        Dec(R.Right, FGridLineWidth);
      if gsHorzLine in GridStyle then
        Dec(R.Bottom, FGridLineWidth);
    end;
    with Canvas do
    begin
      SetTextColor(Handle, ColorToRGB(clWhite));
      SetBkColor(Handle, ColorToRGB(clBlack));
      SetBkMode(Handle, OPAQUE);
      SetRop2(Handle, R2_COPYPEN);
      { intercept place under the title and fixed}
      with GetGridRect do
        IntersectClipRect(Handle, GetFixedRect.Right, Top, Right, Bottom);
      {focus}
      DrawFocusRect(R);
    end;
  end;
end;

procedure TCustomGridView.PaintFreeField;
var
  X, Y: Integer;
  R, RowRect: TRect;
  LastRow   : Integer;
  Cell      : TGridCell;
begin
  {field to the right of table}
  X := GetColumnRect(VisOrigin.Col + VisSize.Col).Left;
  R := GetGridRect;
  if X < R.Right then
  begin
    R.Left := X;
    with Canvas do
      if gsFullRowPaint in GridStyle then
    begin
        Cell.Row := VisOrigin.Row;
        Cell.Col := -1;
        LastRow := Cell.Row + VisSize.Row - 1;
        while Cell.Row <= LastRow do
        begin
          RowRect := GetRowRect(Cell.Row);
          RowRect.Left := R.Left;
          RowRect.Right := R.Right;
          GetCellColors(Cell, Canvas);
          FillRect(RowRect);
          Inc(Cell.Row);
        end;
      end
      else
      begin
      Brush.Color := Color;
      FillRect(R);
    end;
  end;
  {field from below from the table}
  Y := GetRowRect(VisOrigin.Row + VisSize.Row).Top;
  R := GetGridRect;
  if Y < R.Bottom then
  begin
    R.Left := GetFixedRect.Right;
    R.Top := Y;
    with Canvas do
    begin
      Brush.Color := Color;
      FillRect(R);
    end;
    {field under those fixed}
    R.Right := R.Left;
    R.Left := GetFixedRect.Left;
    with Canvas do
    begin
      Brush.Color := Fixed.Color;
      FillRect(R);
    end;
  end;
  {the line of a change in the width of column}
  if FColResizing and (FColResizeCount > 0) and not FColumnsFullDrag then
    PaintResizeLine;
end;

procedure TCustomGridView.PaintGridLines;
var
  Points              : PIntArray;
  PointCount          : Integer;
  StrokeList          : PIntArray;
  StrokeCount         : Integer;
  I                   : Integer;
  L, R, T, B, X, Y, C : Integer;
  Index               : Integer;
  Rect                : TRect;
begin
  {opreleyaem a quantity of lines of grid}
  StrokeCount := 0;
  if gsHorzLine in GridStyle then
  begin
    StrokeCount := FVisSize.Row;
    if gsListViewLike in GridStyle then
      StrokeCount := GetGridHeight div Rows.Height;
  end;
  if gsVertLine in GridStyle then
    StrokeCount := StrokeCount + FVisSize.Col;
  {but there is whether grid}
  if StrokeCount > 0 then
  begin
    {opreleyaem a quantity of points of grid}
    PointCount := StrokeCount * 2;
    { separate memory under the lines}
    StrokeList := AllocMem(StrokeCount * SizeOf(Integer));
    Points := AllocMem(PointCount * SizeOf(TPoint));
    {the initialization of the massif of a quantity of points of poly-lines}
    FillDWord(StrokeList^, StrokeCount, 2);
    Rect := GetGridRect;
    {the point of vertical lines}
    if gsVertLine in GridStyle then
    begin
      T := Rect.Top;
      B := Rect.Bottom;
      if [gsFullVertLine, gsListViewLike] * GridStyle = [] then
        B := GetRowRect(VisOrigin.Row + VisSize.Row).Top;
      X := GetColumnRect(VisOrigin.Col).Left;
      for I := 0 to FVisSize.Col - 1 do
      begin
        X := X + Columns[VisOrigin.Col + I].Width;
        Index := I * 4;
        Points^[Index + 0] := X - 1;
        Points^[Index + 1] := T;
        Points^[Index + 2] := X - 1;
        Points^[Index + 3] := B;
      end;
    end;
    {the point of horizontal lines}
    if gsHorzLine in GridStyle then
    begin
      L := Rect.Left + GetFixedWidth;
      R := Rect.Right;
      if [gsFullHorzLine, gsListViewLike] * GridStyle = [] then
        R := GetColumnRect(VisOrigin.Col + VisSize.Col).Left;
      Y := GetRowRect(VisOrigin.Row).Top;
      C := FVisSize.Row;
      if gsListViewLike in GridStyle then
        C := GetGridHeight div Rows.Height;
      for I := 0 to C - 1 do
      begin
        Y := Y + Rows.Height;
        Index := FVisSize.Col * Ord(gsVertLine in GridStyle) * 4 + I * 4;
        Points^[Index + 0] := L;
        Points^[Index + 1] := Y - 1;
        Points^[Index + 2] := R;
        Points^[Index + 3] := Y - 1;
      end;
    end;
    if not (gsDotLines in GridStyle) then
    begin
      Canvas.Pen.Color := GetGridLineColor(Color);
      Canvas.Pen.Width := FGridLineWidth;
      PolyPolyLine(Canvas.Handle, Points^, StrokeList^, StrokeCount);
    end
    else
      PaintDotGridLines(Points, PointCount);
    FreeMem(Points);
    FreeMem(StrokeList);
  end;
end;

procedure TCustomGridView.PaintHeader(Section: TGridHeaderSection; Rect: TRect);
var
  DefDraw: Boolean;
begin
  { establish color and type of section}
  GetHeaderColors(Section, Canvas);
  {drawing of user}
  DefDraw := True;
  try
    if Assigned(FOnDrawHeader) then
      FOnDrawHeader(Self, Section, Rect, DefDraw);
  except
    Application.HandleException(Self);
  end;
  {is necessary drawing on silence}
  if DefDraw then
    DefaultDrawHeader(Section, Rect);
end;

procedure TCustomGridView.PaintHeaders(DrawFixed: Boolean);
var
  R            : TRect;
  DC           : HDC;
  Details      : TThemedElementDetails;
  ThemedHeader : TThemedHeader;
begin
  PaintHeaderSections(Header.Sections, DrawFixed);
  R := GetHeaderRect;
  R.Left := GetClientRect.Left + Header.GetWidth + GetGridOrigin.X;
  if R.Left < R.Right then
  begin
    if FThemingEnabled and StyleServices.Enabled then
    begin
      ThemedHeader := thHeaderItemRightNormal;
      Details := StyleServices.GetElementDetails(ThemedHeader);
      DC := Canvas.Handle;
      StyleServices.DrawElement(DC, Details, R);
    end
    else
    begin
      Canvas.Brush.Color := Header.Color;
      Canvas.FillRect(R);
      if not Header.Flat then
        Paint3DFrame(R, BF_LEFT or BF_TOP or BF_BOTTOM);
    end;
  end;
  if Header.Flat then
  begin
    { correct the edge of rectangle}
    if DrawFixed then
    begin
      R.Left := GetClientRect.Left;
      R.Right := GetFixedRect.Right;
    end
    else
    begin
      R.Left := GetFixedRect.Right;
      R.Right := GetClientRect.Right;
    end;
    { draw}
    with Canvas do
      {if the colors of those fixed i  of table coincide - it drawes usual strip}
      if Header.GridColor then
      begin
        Pen.Color := GetGridLineColor(Color);
        Pen.Width := FGridLineWidth;
        MoveTo(R.Left, R.Bottom - 1);
        LineTo(R.Right, R.Bottom - 1);
      end
      else
      { otherwise draw dual strip}
      begin
      with Canvas do
      begin
        Pen.Color := clBtnShadow;
        Pen.Width := 1;
        MoveTo(R.Left, R.Bottom - 2);
        LineTo(R.Right, R.Bottom - 2);
        Pen.Color := clBtnHighlight;
        MoveTo(R.Left, R.Bottom - 1);
        LineTo(R.Right, R.Bottom - 1);
      end;
    end;
  end;
  {the line of a change in the width of column}
  if FColResizing and (FColResizeCount > 0) and not FColumnsFullDrag then
    PaintResizeLine;
end;

procedure TCustomGridView.PaintHeaderSections(Sections: TGridHeaderSections;
  AllowFixed: Boolean);
var
  I: Integer;
  S: TGridHeaderSection;
  R, SR: TRect;
begin
  for I := 0 to Sections.Count - 1 do
  begin
    S := Sections[I];
    { draw only the sections "of the fixedness indicated"}
    if AllowFixed = S.FixedColumn then
    begin
      R := S.BoundsRect;
      { do not draw, the section of nontrivial width}
      if R.Right > R.Left then
      begin
        { calculate the rectangle of section}
        SR := R;
        if S.Sections.Count > 0 then
          SR.Bottom := GetHeaderRect.Bottom;
        { draw only those sections and pozzagolovki, which must be drawn again}
        if RectVisible(Canvas.Handle, SR) then
        begin
          PaintHeader(S, R);
          PaintHeaderSections(S.Sections, AllowFixed);
        end;
      end;
    end
    else
      {nektorye general titles can have simultaneously that fixed and
        the unfixed subtitles (although this is incorrect); therefore
        let us try to otrasovat' them also}
      PaintHeaderSections(S.Sections, AllowFixed);
  end;
end;

procedure TCustomGridView.PaintResizeLine;
var
  OldPen: TPen;
begin
  OldPen := TPen.Create;
  try
    with Canvas do
    begin
      OldPen.Assign(Pen);
      try
        Pen.Color := clWhite;
        Pen.Style := psSolid;
        Pen.Mode := pmXor;
        Pen.Width := 1;
        with FColResizeRect do
        begin
          MoveTo(FColResizePos, Top);
          LineTo(FColResizePos, Bottom);
        end;
      finally
        Pen := OldPen;
      end;
    end;
  finally
    OldPen.Free;
  end;
end;

procedure TCustomGridView.PaintText(Canvas: TCanvas; Rect: TRect;
  LeftIndent, TopIndent: Integer; Alignment: TAlignment;
  WantReturns, WordWrap: Boolean; const Text: string);
var
  P: TDrawTextParams;
  F, DX: Integer;
  A: UINT;
begin
  {is concluded text}
  if WantReturns or WordWrap or EndEllipsis then
  begin
    {the parameters of the conclusion of text}
    FillChar(P, SizeOf(P), 0);
    P.cbSize := SizeOf(P);
    P.iLeftMargin := LeftIndent;
    P.iRightMargin := TextRightIndent;
    {the attributes of text}
    F := DT_NOPREFIX;
    { horizontal offset }
    case Alignment of
      taLeftJustify:
        F := F or DT_LEFT;
      taCenter:
        F := F or DT_CENTER;
      taRightJustify:
        F := F or DT_RIGHT;
    end;
    { vertical offset }
    if not (WantReturns or WordWrap) then
    begin
      {the automatic flare function}
      F := F or DT_SINGLELINE;
      {dots at the end}
      if Alignment = taLeftJustify then
        F := F or DT_END_ELLIPSIS
    end;
    {the transfer of words}
    if WordWrap then
      F := F or DT_WORDBREAK;
    {the displacement of text from above}
    Inc(Rect.Top, TopIndent);
    {is concluded text}
    with Canvas do
    begin
      SetBkMode(Handle, TRANSPARENT);
      DrawTextEx(Handle, PChar(Text), Length(Text), Rect, F, @P);
    end;
  end
  else
  begin
    {displacement along horizontal}
    case Alignment of
      taCenter:
        begin
          DX := LeftIndent + (Rect.Right - Rect.Left) div 2;
          A := TA_CENTER;
        end;
      taRightJustify:
        begin
          DX := (Rect.Right - Rect.Left) - TextRightIndent;
          A := TA_RIGHT;
        end;
    else
      DX := LeftIndent;
      A := TA_LEFT;
    end;
    {the standard conclusion of text}
    with Canvas do
    begin
      SetBkMode(Handle, TRANSPARENT);
      SetTextAlign(Handle, A);
      ExtTextOut(Handle, Rect.Left + DX, Rect.Top + TopIndent, ETO_CLIPPED, @Rect, PChar(Text), Length(Text), nil);
      SetTextAlign(Handle, TA_LEFT);
    end;
  end;
end;

procedure TCustomGridView.PreparePatternBitmap(Canvas: TCanvas;
  FillColor: TColor; Remove: Boolean);
begin
  if Remove then
  begin
    if Canvas.Brush.Bitmap = nil then
      Exit;
    Canvas.Brush.Bitmap := nil;
  end
  else
  begin
    if Canvas.Brush.Bitmap = FPatternBitmap then
      Exit;
    {lines by smallen point are drawn with the use of point filling. Size
      filling is established immediately on entire Canvas relative to the the upper
      the left angle of component. Therefore, even pouring the line, shifted by 1
      pixel from previous, we nevertheless will obtain the alternating filling.
      The displacement of table to 1 pixel old grid displaces with the horizontal,
      and so that the new grid would be drawn by that also by alternating with the old, it is necessary
      to displace the size of filling. }
    if HorzScrollBar.Position mod 2 = 0 then
    begin
      FPatternBitmap.Canvas.Pixels[0, 0] := Color;
      FPatternBitmap.Canvas.Pixels[1, 1] := Color;
      FPatternBitmap.Canvas.Pixels[0, 1] := FillColor;
      FPatternBitmap.Canvas.Pixels[1, 0] := FillColor;
    end
    else
    begin
      FPatternBitmap.Canvas.Pixels[0, 0] := FillColor;
      FPatternBitmap.Canvas.Pixels[1, 1] := FillColor;
      FPatternBitmap.Canvas.Pixels[0, 1] := Color;
      FPatternBitmap.Canvas.Pixels[1, 0] := Color;
    end;
    Canvas.Brush.Bitmap := FPatternBitmap;
  end;
  Canvas.Refresh;
end;

procedure TCustomGridView.ResetClickPos;
begin
  FClickPos := GridCell(-1, -1);
end;

procedure TCustomGridView.Resize;
begin
  if UpdateLock = 0 then
  begin
    if FitColsToClient then
      SizeColumnsToClient;
    UpdateScrollBars;
  end;
  UpdateVisOriginSize;
  UpdateEdit(Editing);
  inherited Resize;
end;

procedure TCustomGridView.SetEditText(Cell: TGridCell; var Value: string);
begin
  if Assigned(FOnSetEditText) then
    FOnSetEditText(Self, Cell, Value);
end;

procedure TCustomGridView.ShowCursor;
begin
  if IsFocusAllowed then
    InvalidateFocus
  else
    ShowEdit;
end;

procedure TCustomGridView.ShowEdit;
begin
  UpdateEdit(True);
end;

procedure TCustomGridView.ShowEditChar(C: Char);
begin
  Editing := True;
  if (Edit <> nil) and Editing then
    PostMessage(Edit.Handle, WM_CHAR, Word(C), 0);
end;

procedure TCustomGridView.ShowFocus;
begin
  if IsFocusAllowed then
    PaintFocus;
end;


procedure TCustomGridView.SizeColumnsToClient;
const
  MAX_RUNCOUNT = 20;
var
  iTotColW     : Integer; // total column width
  iVisColCount : Integer; // visible column count
  iStrColCount : Integer; // stretchable column count
  iShrColCount : Integer; // shrinkable column count
  iClientW     : Integer; // clientwidth of the grid
  iOldW        : Integer; // old column width
  iFiller      : Integer; // redistributable space
  I, J         : Integer;
  K            : Integer; // resizing cycle counter
  bCanResize   : Boolean;
begin
  LockUpdate;
  try
    // should not be done if we want to take GUI-resizings into account
  //  if (gsColSizing <> FGridState) and (ColumnResizeMode = crmDynamic) then
//    AutoSizeCols(False);

    // determine iVisColCount and iTotColW
    iVisColCount := 0;
    iTotColW     := 0;
    for I := 0 to Pred(Columns.Count) do
    begin
      if Columns[I].Visible then
      begin
        Inc(iTotColW, Columns[I].Width);
        Inc(iVisColCount);
      end;
    end;

    if iVisColCount <> 0 then
    begin
      // measure grid client width by excluding vertical scroll bar, grid
      // borders and column lines
      // ClientWidth does take scrollbar and border settings into account
      iClientW := ClientWidth + 6;

      if GridLines then
        Dec(iClientW, GridLineWidth * (iVisColCount + 1));

      bCanResize := iClientW > 0;
      // TODO 1 -oTim Sinaeve: now the maximum amount of iterations is limited
      // by the variable <K>. A more elegant solution should be found to limit
      // the amount of resizing cycles.
      K := 0;
      while bCanResize and (iTotColW < iClientW) and (K < MAX_RUNCOUNT) do // stretch columns
      begin
        iStrColCount := 0;
        for I := 0 to Pred(Columns.Count) do
          if CanStretchCol(I) then
            Inc(iStrColCount);

        if iStrColCount > 0 then
        begin
          iFiller := iClientW - iTotColW;
          J := 0;
          iTotColW := 0; // needed for second pass
          for I := 0 to Pred(Columns.Count) do
          begin
            if CanStretchCol(I) then
            begin
              iOldW := Columns[I].Width;
              if iFiller > (iStrColCount - J) then
                Columns[I].Width := LimitColumnWidth(I, Columns[I].Width +
                  (iFiller div (iStrColCount - J)))
              else
                Columns[I].Width := LimitColumnWidth(I, Columns[I].Width + 1);

              Dec(iFiller, Columns[I].Width - iOldW);
              Inc(J);
            end; // if CanStretchCol(I) then
            if Columns[I].Visible then
              Inc(iTotColW, Columns[I].Width);
          end; // for I := 0 to Pred(Columns.Count) do
        end
        else
          bCanResize := False;
        Inc(K);
      end; // while bCanResize and (iTotColW < iClientW) do

      K := 0;
      while bCanResize and (iTotColW > iClientW) and (K < MAX_RUNCOUNT) do // shrink columns
      begin
        iShrColCount := 0;
        for I := 0 to Pred(Columns.Count) do
          if CanShrinkCol(I) then
            Inc(iShrColCount);

        if iShrColCount > 0 then
        begin
          iFiller := iTotColW - iClientW;
          J := 0; // counter for the visible columns
          iTotColW := 0;
          for I := 0 to Pred(Columns.Count) do
          begin
            if CanShrinkCol(I) then
            begin
              iOldW := Columns[I].Width;

              if iFiller > (iShrColCount - J) then
                Columns[I].Width := LimitColumnWidth(I, Columns[I].Width -
                 (iFiller div (iShrColCount - J)))
              else
                Columns[I].Width := LimitColumnWidth(I, Columns[I].Width - 1);
              if Columns[I].Width < 0 then
                Columns[I].Width := 0;
              Dec(iFiller, iOldW - Columns[I].Width);
              Inc(J);
            end;
            if Columns[I].Visible then
              Inc(iTotColW, Columns[I].Width);
          end // for I := 0 to Pred(Columns.Count) do
        end
        else
          bCanResize := False;
        Inc(K);
      end; // while bCanResize and (iTotColW > iClientW) do
    end; // if iVisColCount <> 0 then
  finally
    UnLockUpdate(True);
  end;
end;

procedure TCustomGridView.StartColResize(Section: TGridHeaderSection;
  X, Y: Integer);
begin
  FColResizeSection := Section;
  FColResizeIndex := Section.ResizeColumnIndex;
  { calculate boundary rectangle for changing the size}
  with FColResizeSection do
  begin
    if FColResizeIndex <= Columns.Count - 1 then
    begin
      FColResizeRect := GetColumnRect(FColResizeIndex);
      FColResizeRect.Bottom := GetClientRect.Bottom;
      FColResizeMinWidth := Columns[FColResizeIndex].MinWidth;
      FColResizeMaxWidth := Columns[FColResizeIndex].MaxWidth;
    end
    else
    begin
      FColResizeRect := BoundsRect;
      FColResizeRect.Bottom := GetClientRect.Bottom;
      FColResizeMinWidth := 0;
      FColResizeMaxWidth := MAX_COLUMN_WIDTH;
    end;

    FColResizeRect.Top := Level * Header.SectionHeight;
    FColResizeRect.Bottom := Height;
  end;
  {the position of dimension line}
  FColResizePos := FColResizeRect.Right;
  FColResizeOffset := FColResizePos - X;
  {it is possible to change the size of column}
  FColResizeCount := 0;
  FColResizing := True;
  { seize mouse}
  MouseCapture := True;
end;

procedure TCustomGridView.StepColResize(X, Y: Integer);
var
  W: Integer;
  R: TRect;
  S: TGridHeaderSection;
begin
  {but will go a change in the size of column}
  if FColResizing then
  begin
    {the current position of line}
    X := X + FColResizeOffset;
    {the current width}
    W := X - FColResizeRect.Left;
    { correct width in sootvestvii with the boundaries}
    if W < FColResizeMinWidth then
      W := FColResizeMinWidth;
    if W > FColResizeMaxWidth then
      W := FColResizeMaxWidth;
    ColumnResizing(FColResizeIndex, W);
    { again correct shshchirinu}
    if W < FColResizeMinWidth then
      W := FColResizeMinWidth;
    if W > FColResizeMaxWidth then
      W := FColResizeMaxWidth;
    {the new position of line}
    X := FColResizeRect.Left + W;
    { draw a line}
    if FColResizePos <> X then
    begin
      { paint old line}
      if (FColResizeCount > 0) and not FColumnsFullDrag then
        PaintResizeLine;
      Inc(FColResizeCount);
      {the new position of line}
      FColResizePos := X;
      { establish width}
      if FColumnsFullDrag and (FColResizeIndex < Columns.Count) then
      begin
        {before a change in the width of column we calculate and it is renovated changing
          the part of the cells of table}
        R := GetClientRect;
        R.Left := GetColumnRect(FColResizeIndex).Left;
        if FColResizeIndex >= Fixed.Count then
          {if the unfixed column is partially closed fixed,
            that this closed part drawn again must not be}
          R.Left := MaxIntValue([R.Left, GetFixedRect.Right]);
        if W < Columns[FColResizeIndex].Width then
          with HorzScrollBar do
            if (Range > PageStep) and (Position = Range - PageStep) then
              if FColResizeIndex <= Columns.Count - 1 then
              begin
                {the borderline case: if horizontal scroller in the the very
                  with right position, the decrease of the width of column gives k
                  to the right-shift of all unfixed columns, located
                  to the left of that flowing}
                R.Left := GetFixedRect.Right;
                R.Right := GetColumnRect(FColResizeIndex + 1).Left;
              end;
        InvalidateRect(R);
        {if the column has multilevel title, then additionally
          it is renovated uppermost section}
        S := GetHeaderSection(FColResizeIndex, 0);
        if S <> nil then
        begin
          R := S.BoundsRect;
          R.Bottom := GetHeaderRect.Bottom;
          InvalidateRect(R);
        end;
        { forbid the copying of entire table, we establish the new width
          stolbtsa }
        LockUpdate;
        try
          Columns[FColResizeIndex].Width := W;
        finally
          UnlockUpdate(False);
        end;
        {now we draw again (better immediately - so less it blinks)}
        Update;
      end
      else
        { draw new line}
        PaintResizeLine;
    end
    else
    begin
      { draw line for the first time}
      if (FColResizeCount = 0) and not FColumnsFullDrag then
        PaintResizeLine;
      Inc(FColResizeCount);
    end;
  end;
end;

procedure TCustomGridView.StopColResize(Abort: Boolean);
var
  W: Integer;
begin
  if FColResizing then
  try
    { free mouse}
    MouseCapture := False;
    {there was khotyaby one displacement}
    if FColResizeCount > 0 then
    begin
      { paint line}
      if not FColumnsFullDrag then
        PaintResizeLine;
      {but is not interrupted change}
      if Abort then
        Exit;
      { establish the size of column}
      with FColResizeSection do
      begin
        {new width}
        W := FColResizePos - FColResizeRect.Left;
        { correct width in sootvestvii with the boundaries}
        if W < FColResizeMinWidth then
          W := FColResizeMinWidth;
        if W > FColResizeMaxWidth then
          W := FColResizeMaxWidth;
        {the event of user}
        ColumnResize(FColResizeIndex, W);
        { again correct shshchirinu}
        if W < FColResizeMinWidth then
          W := FColResizeMinWidth;
        if W > FColResizeMaxWidth then
          W := FColResizeMaxWidth;
        { establish width}
        if FColResizeIndex < Columns.Count then
          Columns[FColResizeIndex].Width := W;
        Width := W;
      end;
    end;
  finally
    FColResizing := False;
  end;
end;

procedure TCustomGridView.StartHeaderClick(Section: TGridHeaderSection;
  X, Y: Integer);
var
  AllowClick: Boolean;
begin
  AllowClick := True;
  HeaderClicking(Section, AllowClick);
  if AllowClick then
  begin
    FHeaderClickSection := Section;
    FHeaderClickRect := Section.BoundsRect;
    FHeaderClickState := False;
    FHeaderClicking := True;
    MouseCapture := True;
    StepHeaderClick(X, Y);
  end;
end;

procedure TCustomGridView.StepHeaderClick(X, Y: Integer);
var
  P: Boolean;
begin
  if FHeaderClicking then
  begin
    P := PtInRect(FHeaderClickRect, Point(X, Y));
    if FHeaderClickState <> P then
    begin
      FHeaderClickState := P;
      InvalidateRect(FHeaderClickRect);
    end;
  end;
end;

procedure TCustomGridView.StopHeaderClick(Abort: Boolean);
var
  P: Boolean;
begin
  if FHeaderClicking then
  begin
    P := FHeaderClickState;
    StepHeaderClick(-1, -1);
    FHeaderClicking := False;
    MouseCapture := False;
    if (not Abort) and P then
      HeaderClick(FHeaderClickSection);
  end;
end;

procedure TCustomGridView.UpdateHotHeader(Section: TGridHeaderSection);
var
  AllowClick: Boolean;
begin
  if FHotHeaderSection = Section then
    Exit;
  if Assigned(Section) then
  begin
    AllowClick := true;
    HeaderClicking(Section, AllowClick);
    if not AllowClick then
      Section := nil;
  end;
  if Assigned(FHotHeaderSection) then
    InvalidateRect(FHotHeaderSection.GetBoundsRect);
  FHotHeaderSection := Section;
  if Assigned(FHotHeaderSection) then
    InvalidateRect(FHotHeaderSection.GetBoundsRect);
end;

procedure TCustomGridView.ApplyEdit;
begin
  Editing := False;
end;

procedure TCustomGridView.AutoSizeCols(AIncludeTitles : Boolean = True;
  OnlyVisibleRows : Boolean = True);
var
  I, W: Integer;
begin
  LockUpdate;
  try
    for I := 0 to Columns.Count -1 do
    begin
      if not Columns[I].FixedSize then
      begin
        W := MinIntValue([Columns[I].MaxWidth,
                          GetColumnMaxWidth(I, AIncludeTitles, OnlyVisibleRows)]);
        FColResizing := True;
        try
          ColumnAutoSize(I, W);
          Columns[I].Width := W;
        finally
          FColResizing := False;
          UpdateVisOriginSize;
        end;
      end;
    end;
  finally
    UnLockUpdate(True);
  end;
end;

procedure TCustomGridView.CancelEdit;
var
  Cell: TGridCell;
begin
  if Editing then
  begin
    Cell := EditCell;
    if not AlwaysEdit then
    begin
      HideEdit;
      ChangeEditing;
    end
    else
      UpdateEditContents(False);
    EditCanceled(Cell);
  end;
end;

function TCustomGridView.CanShrinkCol(ACol: Integer): Boolean;
begin
  Result := Columns[ACol].Visible and (Columns[ACol].Width > 0) and
    ((Columns[ACol].MinWidth = 0) or
     (Columns[ACol].Width > Columns[ACol].MinWidth));
end;

function TCustomGridView.CanStretchCol(ACol: Integer): Boolean;
begin
  Result := Columns[ACol].Visible and
    ((Columns[ACol].MaxWidth = 0) or
     (Columns[ACol].Width < Columns[ACol].MaxWidth));
end;

{TODO -oTS -cGeneral : Add theming support}

procedure TCustomGridView.DefaultDrawCell(Cell: TGridCell; Rect: TRect);
const
  DS: array[Boolean] of Integer = (ILD_NORMAL, ILD_SELECTED);
var
  DefRect        : TRect;
  CK             : TGridCheckKind;
  CB             : TBitmap;
  CR             : TRect;
  CI, X, Y, W, H: Integer;
  IDS            : Integer;
  BKC, BLC       : DWORD;
  R              : TRect;
  RH, CH, IH, SH : Boolean;
  TI             : TPoint;
  A              : TAlignment;
  WR, WW         : Boolean;
  T              : string;
begin
  DefRect := Rect;
  if GridLines then
  begin
    if gsVertLine in GridStyle then
      Dec(Rect.Right, FGridLineWidth);
    if gsHorzLine in GridStyle then
      Dec(Rect.Bottom, FGridLineWidth);
  end;
  CK := GetCheckKind(Cell);
  CI := GetCellImage(Cell);
  { determine the sign of the illumination of the picture: picture is not illuminated,
    if cell is first chosen, the flag of the illumination is not advanced
    and the color of the background of cell is a color of that isolated}
  RH := RowSelect and (Cell.Col = Fixed.Count) and (Cell.Row = CellFocused.Row);
  CH := (not RowSelect) and IsCellEqual(Cell, CellFocused);
  SH := (Canvas.Brush.Color = clHighlight) or (Canvas.Brush.Color = clBtnFace); {?}
  IH := (not ImageHighlight) and (RH or CH) and SH;
  { draw checkbox }
  if CK <> gcNone then
  begin
    { for the illuminated flag we restore the color of background}
    if IH then
      Canvas.Brush.Color := Color;
    { obtain the rectangle of flag}
    R := Rect;
    R.Right := MinIntValue([R.Left + CheckWidth + GetCheckIndent(Cell).X, Rect.Right]);
    {but is visible flag}
    if R.Left < DefRect.Right then
    begin
      { draw}
      with Canvas do
      begin
        {the position of flag}
        X := R.Left + GetCheckIndent(Cell).X;
        Y := R.Top + GetCheckIndent(Cell).Y;
        {the size of the flag (necessarily for the cutting off of the flags of narrow columns)}
        W := CheckWidth;
        if X + W > R.Right then
          W := R.Right - X;
        H := CheckHeight;
        if Y + H > R.Bottom then
          H := R.Bottom - Y;
        { define the type of flag}
        if CK <> gcUserDefine then
        begin
          { determine a series of pictures according to the type of flag}
          CB := FCheckBitmapCB;
          if CK = gcRadioButton then
            CB := FCheckBitmapRB;
          { determine the picture of flag}
          CR := Bounds(CheckWidth * Ord(GetCheckState(Cell)), 0, W, H);
          OffsetRect(CR, CheckWidth * 4 * Ord(CheckStyle), 0);
          { draw vlazhok}
          FillRect(R);
          BrushCopy(Bounds(X, Y, W, H), CB, CR, CB.TransparentColor);
        end
        else
        begin
          CB := FCheckBuffer;
          CB.Width := CheckWidth;
          CB.Height := CheckHeight;
          GetCheckImage(Cell, CB);
          FillRect(R);
          if not (CB.Empty or (CB.Width < 1) or (CB.Height < 1)) then
          begin
            CR := Bounds(0, 0, W, H);
            BrushCopy(Bounds(X, Y, W, H), CB, CR, CB.TransparentColor);
          end;
        end;
      end;
      { displace the left edge of initial rectangle}
      Rect.Left := R.Right;
    end;
  end;
  { draw picture}
  if CI <> -1 then
  begin
    {for the illuminated picture we restore the color of background}
    if IH then
      Canvas.Brush.Color := Color;
    { obtain the rectangle of picture}
    R := Rect;
    R.Right := MinIntValue([R.Left + Images.Width + GetCellImageIndent(Cell).X, Rect.Right]);
    if R.Left < DefRect.Right then
    begin
      X := R.Left + GetCellImageIndent(Cell).X;
      Y := R.Top + GetCellImageIndent(Cell).Y;
      {the size of the picture (necessarily for the cutting off of the pictures of narrow columns)}
      W := Images.Width;
      if X + W > R.Right then
        W := R.Right - X;
      H := Images.Height;
      if Y + H > R.Bottom then
        H := R.Bottom - Y;
      {style and the background colors of picture}
      IDS := DS[IsCellHighlighted(Cell) and Focused and IH];
      BKC := GetRGBColor(Images.BkColor);
      BLC := GetRGBColor(Images.BlendColor);
      Canvas.FillRect(R);
      ImageList_DrawEx(Images.Handle, CI, Canvas.Handle, X, Y, W, H, BKC, BLC, IDS);
      Rect.Left := R.Right;
    end;
  end;
  { restore the color of background}
  GetCellColors(Cell, Canvas);
  if not(IsCellEqual(Cell, FEditCell) and (not IsFocusAllowed)) then
  begin
    R := Rect;
    if R.Left < DefRect.Right then
      with Canvas do
      begin
        TI := GetCellTextIndent(Cell);
        A := Columns[Cell.Col].Alignment;
        WR := Columns[Cell.Col].WantReturns;
        WW := Columns[Cell.Col].WordWrap;
        T := GetCellText(Cell);
        FillRect(R);
        PaintText(Canvas, R, TI.X, TI.Y, A, WR, WW, T);
      end;
  end;
end;

procedure TCustomGridView.DefaultDrawHeader(Section: TGridHeaderSection;
  Rect: TRect);
var
  DefRect       : TRect;
  I, X, Y, W, H : Integer;
  BKC, BLC      : DWORD;
  P             : TDrawTextParams;
  F             : Integer;
  T             : string;
  TL            : Integer;
  R             : TRect;
  SD            : TGridSortDirection;
  SB            : TBitmap;
  SR            : TRect;
  DC            : HDC;
  Details       : TThemedElementDetails;
  ThemedHeader  : TThemedHeader;
  IsPressed     : Boolean;
begin
  DefRect := Rect;
  IsPressed := IsHeaderPressed(Section);
  if FThemingEnabled and StyleServices.Enabled then
  begin
    if IsPressed then
      ThemedHeader := thHeaderItemPressed
    else if FHotHeaderSection = Section then
      ThemedHeader := thHeaderItemHot
    else
      ThemedHeader := thHeaderItemNormal;
    Details := StyleServices.GetElementDetails(ThemedHeader);
    DC := Canvas.Handle;
    // Header shape
    StyleServices.DrawElement(DC, Details, Rect);
  end
  else
  begin
    Canvas.FillRect(Rect);
  end;
  {if section is pressed - we displace picture and text}
  if IsHeaderPressed(Section) then
    OffsetRect(Rect, 1, 1);
  I := GetHeaderImage(Section);
  if I <> -1 then
  begin
    R := Rect;
    R.Right := MinIntValue([R.Left + Header.Images.Width + 2, Rect.Right]);
    if R.Left < Rect.Right then
    begin
      with Canvas do
      begin
        X := R.Left + 2;
        Y := R.Top + 1 + Ord(not Header.Flat);
        {the size of the picture (necessarily for the cutting off of the pictures of narrow sections)}
        W := Header.Images.Width;
        if X + W > R.Right then
          W := R.Right - X;
        H := Header.Images.Height;
        if Y + H > R.Bottom then
          H := R.Bottom - Y;
        {the background colors of picture}
        BKC := GetRGBColor(Header.Images.BkColor);
        BLC := GetRGBColor(Header.Images.BlendColor);
        { draw picture }
        ImageList_DrawEx(Header.Images.Handle, I, Canvas.Handle, X, Y, W, H, BKC, BLC, ILD_NORMAL);
      end;
      { displace the left edge of initial rectangle }
      Rect.Left := R.Right;
    end;
  end;
  if Rect.Left < Rect.Right then
  begin
    { obtain the text of title}
    T := Section.Caption;
    TL := Length(T);
    {for the quite lower section we obtain the direction of sorting}
    SD := gsNone;
    if Section.Level = Header.MaxLevel then
      SD := GetSortDirection(Section);
    with Canvas do
    begin
      {the parameters of the conclusion of text}
      FillChar(P, SizeOf(P), 0);
      P.cbSize := SizeOf(P);
      P.iLeftMargin := 2 + 4 * Ord(I = -1);
      P.iRightMargin := 6;
      F := DT_END_ELLIPSIS or DT_NOPREFIX;
      case Section.Alignment of
        taLeftJustify:
          F := F or DT_LEFT;
        taRightJustify:
          F := F or DT_RIGHT;
        taCenter:
          F := F or DT_CENTER;
      end;
      if Section.WordWrap then
        F := F or DT_WORDBREAK;
      {if there is a picture of sorting - we draw first it}
      if SD <> gsNone then
      begin
        SB := FSortBuffer;
        { assign picture on silence }
        case SD of
          gsAscending:
            SB.Assign(FSortBitmapA);
          gsDescending:
            SB.Assign(FSortBitmapD);
        end;
        { if there is a user-defined picture, we take it }
        GetSortImage(Section, SB);
        W := SB.Width;
        H := SB.Height;
        { determine the width of text }
        R := Rect;
        if TL > 0 then
        begin
          DrawTextEx(Handle, PChar(T), Length(T), R, F or DT_CALCRECT, @P);
          R.Top := Rect.Top +
            ((Rect.Bottom - Rect.Top) - (R.Bottom - R.Top)) div 2;
        end
        else
        begin
          R.Right := R.Left;
          R.Top := Rect.Top + 2 * Ord(not Header.Flat);
        end;
        { calculate rectangle }
        SR.Left := MinIntValue([Rect.Right - W - 6, R.Right + SortLeftIndent]);
        SR.Left := MaxIntValue([Rect.Left + P.iLeftMargin, SR.Left]);
        SR.Right := MinIntValue([Rect.Right - 6, SR.Left + W]);
        SR.Top := R.Top + SortTopIndent;
        SR.Bottom := MinIntValue([Rect.Bottom, SR.Top + H]);
        W := SR.Right - SR.Left;
        H := SR.Bottom - SR.Top;
        { draw picture }
        BrushCopy(SR, SB, Bounds(0, 0, W, H), clSilver);
        { correct the rectangle of text}
        Rect.Right := SR.Left - SortLeftIndent;
      end;
      { draw text}
      if (TL > 0) and (Rect.Left < Rect.Right) then
      begin
        { again determine the rectangle of text}
        R := Rect;
        DrawTextEx(Handle, PChar(T), Length(T), R, F or DT_CALCRECT, @P);
        {for the sections without the pictures we equalize text on the vertical line}
        if I = -1 then
          OffsetRect(R, 0,
            ((Rect.Bottom - Rect.Top) - (R.Bottom - R.Top)) div 2)
        else
          OffsetRect(R, 0, 2 + 2 * Ord(not Header.Flat));
        { correct left and right side of edge}
        R.Right := Rect.Right;
        R.Left := Rect.Left;
        {is concluded text}
        SetBkMode(Handle, TRANSPARENT);
        DrawTextEx(Handle, PChar(T), Length(T), R, F, @P);
      end;
    end;
  end;
  if not FThemingEnabled or not StyleServices.Enabled then
  { draw separator}
  with Canvas do
  begin
    Rect := DefRect;
    { determine what to draw - button or strip}
    if Header.Flat then
    begin
      {if the color of title and table coincides - it drawes single line}
      if Header.GridColor then
      begin
        Pen.Color := GetGridLineColor(Color);
        Pen.Width := FGridLineWidth;
        {strip from below}
        MoveTo(Rect.Left, Rect.Bottom - 1);
        LineTo(Rect.Right - 1, Rect.Bottom - 1);
        {strip to the right}
        MoveTo(Rect.Right - 1, Rect.Top);
        LineTo(Rect.Right - 1, Rect.Bottom);
      end
      else
      begin
        Pen.Width := 1;
        {strip from below}
        Pen.Color := clBtnShadow;
        MoveTo(Rect.Left, Rect.Bottom - 2);
        LineTo(Rect.Right - 1, Rect.Bottom - 2);
        Pen.Color := clBtnHighlight;
        MoveTo(Rect.Left, Rect.Bottom - 1);
        LineTo(Rect.Right - 1, Rect.Bottom - 1);
        {strip to the right}
        Pen.Color := clBtnShadow;
        MoveTo(Rect.Right - 2, Rect.Top);
        LineTo(Rect.Right - 2, Rect.Bottom - 1);
        Pen.Color := clBtnHighlight;
        MoveTo(Rect.Right - 1, Rect.Top);
        LineTo(Rect.Right - 1, Rect.Bottom);
      end;
    end
    else
    begin
      { draw the framework of button}
      if IsHeaderPressed(Section) then
        DrawEdge(Handle, Rect, BDR_SUNKENOUTER, BF_RECT or BF_FLAT)
      else
        Paint3DFrame(Rect, BF_RECT);
      { correct the rectangle of section}
      InflateRect(Rect, -2, -2);
    end;
  end;
end;

procedure TCustomGridView.DrawDragRect(Cell: TGridCell);
var
  R: TRect;
begin
  if IsCellVisible(Cell, True) then
  begin
    R := GetEditRect(Cell);
    GetCellColors(CellFocused, Canvas);
    with Canvas do
    begin
      { intercept place under the title and fixed }
      with GetGridRect do
        IntersectClipRect(Handle, GetFixedRect.Right, Top, Right, Bottom);
      DrawFocusRect(R);
    end;
  end;
end;

function TCustomGridView.FindText(const SearchStr: string; StartCell: TGridCell;
  Options: TFindOptions; var ResultCell: TGridCell): Boolean;
var
  I, J, D, C: Integer;
  S: string;
begin
  {the direction of search, the current line and a quantity of lines for the search}
  if frDown in Options then
  begin
    D := 1;
    J := StartCell.Row + 1;
    C := Rows.Count - J;
  end
  else
  begin
    D := -1;
    J := StartCell.Row - 1;
    C := J + 1;
  end;
  { search for}
  while C > 0 do
  begin
    { sort out columns}
    for I := 0 to Columns.Count - 1 do
      { compare values (only for the visible columns)}
      if Columns[I].Width > 0 then
      begin
        S := Cells[I, J];
        {in the empty line we do not search for}
        if Length(S) = 0 then
          Continue;
        { compare lines taking into account conditions}
        if CompareStrEx(SearchStr, S, frWholeWord in Options,
          frMatchCase in Options) then
        begin
          ResultCell := GridCell(I, J);
          Result := True;
          Exit;
        end;
      end;
    {the following line}
    Inc(J, D);
    Dec(C);
  end;
  ResultCell := GridCell(-1, -1);
  Result := False;
end;

function TCustomGridView.GetCellAt(X, Y: Integer): TGridCell;
var
  C, R: Integer;
begin
  C := GetColumnAt(X, Y);
  R := GetRowAt(X, Y);
  if (C <> -1) and (R <> -1) then
  begin
    Result.Col := C;
    Result.Row := R;
  end
  else
  begin
    Result.Col := -1;
    Result.Row := -1;
  end;
end;

function TCustomGridView.GetCellRect(Cell: TGridCell): TRect;
begin
  IntersectRect(Result, GetColumnRect(Cell.Col), GetRowRect(Cell.Row));
end;

function TCustomGridView.GetCellsRect(Cell1, Cell2: TGridCell): TRect;
var
  CR, RR: TRect;
begin
  if (Cell2.Col < Cell1.Col) or (Cell2.Row < Cell1.Row) then
  begin
    Result := Rect(0, 0, 0, 0);
    Exit;
  end;
  CR := GetColumnRect(Cell1.Col);
  if Cell2.Col > Cell1.Col then
    CR.Right := GetColumnRect(Cell2.Col).Right;
  {upper and lower boundaries}
  RR := GetRowRect(Cell1.Row);
  if Cell2.Row > Cell1.Row then
    RR.Bottom := GetRowRect(Cell2.Row).Bottom;
  Result.Left := CR.Left;
  Result.Right := CR.Right;
  Result.Top := CR.Top;
  Result.Bottom := CR.Bottom;
end;

function TCustomGridView.GetColumnAt(X, Y: Integer): Integer;
var
  L, R: Integer;
begin
  Result := 0;
  L := GetClientRect.Left;
  while Result <= Fixed.Count - 1 do
  begin
    R := L + Columns[Result].Width;
    if (R <> L) and (X >= L) and (X < R) then
      Exit;
    L := R;
    Inc(Result);
  end;
  L := L + GetGridOrigin.X;
  while Result <= Columns.Count - 1 do
  begin
    R := L + Columns[Result].Width;
    if (R <> L) and (X >= L) and (X < R) then
      Exit;
    L := R;
    Inc(Result);
  end;
  Result := -1;
end;

function TCustomGridView.GetColumnLeftRight(Column: Integer): TRect;
begin
  if Columns.Count = 0 then
  begin
    Result.Left := GetGridRect.Left;
    Result.Right := Result.Left;
  end
  else if Column < 0 then
  begin
    Result := GetColumnLeftRight(0);
    Result.Right := Result.Left;
  end
  else if Column > Columns.Count - 1 then
  begin
    Result := GetColumnLeftRight(Columns.Count - 1);
    Result.Left := Result.Right;
  end
  else
  begin
    Result.Left := GetClientRect.Left + GetColumnsWidth(0, Column - 1);
    if Column >= Fixed.Count then
      Inc(Result.Left, GetGridOrigin.X);
    Result.Right := Result.Left + Columns[Column].Width;
  end;
end;

function TCustomGridView.GetColumnMaxWidth(Column: Integer;
  IncludeTitles: Boolean; OnlyVisibleRows : Boolean): Integer;
var
  I, W: Integer;
  C: TGridCell;
  R: TRect;
  MinRow, MaxRow: Integer;
begin
  if (Column < 0) or (Column > Columns.Count - 1) then
  begin
    Result := 0;
    Exit;
  end;
  if IncludeTitles then
    // we reserve a margin of 6 pixels for to draw a sorting arrow in the header
    Result := GetTextWidth(Columns[Column].Caption, Font) + 26
  else
    Result := 0;

  if OnlyVisibleRows then
  begin
    MinRow := VisOrigin.Row;
    MaxRow := FVisSize.Row - 1;
  end
  else
  begin
    MinRow := 0;
    MaxRow := Rows.Count -1;
  end;

  if MaxRow >= MinRow then
  begin
    { calculate the optimal column width }
    for I := MinRow to MaxRow do
    begin
      { column and celltext }
      C := GridCell(Column, I);
      { determine the text rectangle }
      R := GetCellTextBounds(C);
      { determine text width }
      W := R.Right - R.Left;
      { include cell checkbox }
      if IsCellHasCheck(C) then
        Inc(W, CheckWidth + GetCheckIndent(C).X);
      { include cell image }
      if IsCellHasImage(C) then
        Inc(W, Images.Width + GetCellImageIndent(C).X);
      { include grid lines }
      if GridLines and (gsVertLine in GridStyle) then
        Inc(W, FGridLineWidth);
      { if the column title is wider, return the title width }
      if Result < W then
        Result := W;
    end;
  end;
end;

function TCustomGridView.GetColumnRect(Column: Integer): TRect;
begin
  Result := GetColumnLeftRight(Column);
  {the upper and bottom edge of line is determined respectively by the the upper
    to the edge of the first line and on the bottom edge of last line}
  Result.Top := GetRowTopBottom(0).Top;
  Result.Bottom := GetRowTopBottom(Rows.Count - 1).Bottom;
end;

function TCustomGridView.GetColumnsRect(Column1, Column2: Integer): TRect;
var
  R1, R2: TRect;
begin
  R1 := GetColumnRect(Column1);
  R2 := GetColumnRect(Column2);
  UnionRect(Result, R1, R2); {!}
end;

function TCustomGridView.GetColumnsWidth(Column1, Column2: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  Column1 := MaxIntValue([Column1, 0]);
  Column2 := MinIntValue([Column2, Columns.Count - 1]);
  for I := Column1 to Column2 do
    Inc(Result, Columns[I].Width);
end;

function TCustomGridView.GetEditRect(Cell: TGridCell): TRect;
begin
  Result := GetCellRect(Cell);
  if IsCellHasCheck(Cell) then
    Inc(Result.Left, CheckWidth + GetCheckIndent(Cell).X);
  if IsCellHasImage(Cell) then
    Inc(Result.Left, Images.Width + GetCellImageIndent(Cell).X);
  if GridLines then
  begin
    if gsVertLine in GridStyle then
      Dec(Result.Right, FGridLineWidth);
    if gsHorzLine in GridStyle then
      Dec(Result.Bottom, FGridLineWidth);
  end;
  { check right edge}
  if Result.Left > Result.Right then
    Result.Left := Result.Right;
end;

function TCustomGridView.GetHeaderHeight: Integer;
begin
  Result := Header.Height;
end;

function TCustomGridView.GetHeaderRect: TRect;
begin
  Result := ClientRect;
  Result.Bottom := Result.Top;
  if ShowHeader then
    Inc(Result.Bottom, GetHeaderHeight);
end;

function TCustomGridView.GetFirstImageColumn: Integer;
var
  I: Integer;
begin
  for I := Fixed.Count to Columns.Count - 1 do
    if Columns[I].Visible then
    begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

function TCustomGridView.GetFixedRect: TRect;
begin
  Result := GetGridRect;
  Result.Right := Result.Left + GetColumnsWidth(0, Fixed.Count - 1);
end;

function TCustomGridView.GetFixedWidth: Integer;
begin
  with GetFixedRect do
    Result := Right - Left;
end;

function TCustomGridView.GetFocusRect: TRect;
var
  C: TGridCell;
  L: Integer;
begin
  if RowSelect then
  begin
    C := GridCell(Fixed.Count, CellFocused.Row);
    Result := GetRowRect(CellFocused.Row);
    Result.Left := GetColumnLeftRight(Fixed.Count).Left;
  end
  else
  begin
    C := CellFocused;
    Result := GetCellRect(CellFocused);
  end;
  if not ImageHighlight then
  begin
    if IsCellHasCheck(C) then
      Inc(Result.Left, CheckWidth + GetCheckIndent(C).X);
    if IsCellHasImage(C) then
      Inc(Result.Left, Images.Width + GetCellImageIndent(C).X);
  end;
  L := GetCellRect(C).Right;
  if Result.Left > L then
    Result.Left := L;
end;

function TCustomGridView.GetGridHeight: Integer;
begin
  with GetGridRect do
    Result := Bottom - Top;
end;

function TCustomGridView.GetGridOrigin: TPoint;
begin
  Result.X := - HorzScrollBar.Position;
  Result.Y := - VertScrollBar.Position * Rows.Height;
end;

function TCustomGridView.GetGridRect: TRect;
begin
  Result := ClientRect;
  Result.Top := GetHeaderRect.Bottom;
end;

function TCustomGridView.GetHeaderSection(ColumnIndex, Level: Integer):
  TGridHeaderSection;

  function DoGetSection(Sections: TGridHeaderSections): TGridHeaderSection;
  var
    I, L: Integer;
    S: TGridHeaderSection;
  begin
    for I := 0 to Sections.Count - 1 do
    begin
      S := Sections[I];
      L := S.Level;
      { compare column and level}
      if (S.ColumnIndex >= ColumnIndex) and
        (((Level = -1) and (S.Sections.Count = 0)) or (L = Level)) then
      begin
        Result := S;
        Exit;
      end;
      {recursion to all subtitles from below}
      S := DoGetSection(S.Sections);
      {they found or not}
      if S <> nil then
      begin
        Result := S;
        Exit;
      end;
    end;
    {there is no section}
    Result := nil;
  end;

begin
  Result := DoGetSection(Header.Sections);
end;

function TCustomGridView.GetResizeSectionAt(X, Y: Integer): TGridHeaderSection;

  function FindSection(Sections: TGridHeaderSections;
    var Section: TGridHeaderSection): Boolean;
  var
    I, C, DL, DR: Integer;
    R: TRect;
    S: TGridHeaderSection;
  begin
    for I := Sections.Count - 1 downto 0 do
    begin
      { obtain cell and its column}
      S := Sections[I];
      { search for only for the visible columns}
      if S.Visible then
      begin
        C := S.ResizeColumnIndex;
        { obtain the rectangle of the range of change in the size}
        R := S.BoundsRect;
        with R do
        begin
          { determine an error in the entry}
          DL := 7;
          if R.Right - R.Left < 20 then
            DL := 3;
          if R.Right - R.Left < 10 then
            DL := 1;
          DR := 5;
          if C < Columns.Count - 1 then
          begin
            if Columns[C + 1].DefWidth < 20 then
              DR := 3;
            if Columns[C + 1].DefWidth < 10 then
              DR := 1;
          end;
          { correct the rectangle of entry}
          if R.Right > R.Left then
            Left := Right - DL;
          Right := Right + DR;
        end;
        if PtInRect(R, Point(X, Y)) then
        begin
          if (C < Columns.Count) and
            (Columns[C].FixedSize or (not ColumnsSizing)) then
          begin
            Section := nil;
            Result := False;
          end
          else
          begin
            Section := S;
            Result := True;
          end;
          Exit;
        end;
        { search for section in the subtitles}
        if FindSection(S.Sections, Section) then
        begin
          Result := True;
          Exit;
        end;
      end;
    end;
    Section := nil;
    Result := False;
  end;

begin
  FindSection(Header.Sections, Result);
end;

function TCustomGridView.GetRowAt(X, Y: Integer): Integer;
var
  Row: Integer;
  GRT, GOY: Integer;
begin
  Result := -1;
  GRT := GetGridRect.Top;
  GOY := GetGridOrigin.Y;
  if Y - GRT - GOY < 0 then
    Exit;
  if Rows.Height > 0 then
  begin
    Row := (Y - GRT - GOY) div Rows.Height;
    if (Row >= 0) and (Row < Rows.Count) then
      Result := Row;
  end;
end;

function TCustomGridView.GetRowRect(Row: Integer): TRect;
begin
  Result := GetRowTopBottom(Row);
  {the left and right edge of line is determined respectively by the leftist
    to the edge of the first unfixed column and on the right edge of the latter
    column}
  Result.Left := MinIntValue([GetClientRect.Left,
    GetColumnLeftRight(Fixed.Count).Left]);
  Result.Right := GetColumnLeftRight(Columns.Count - 1).Right;
  if (gsFullRowPaint in GridStyle) then
    with ClientRect do
      if Right > Result.Right then
        Result.Right := Right;
end;

function TCustomGridView.GetRowsRect(Row1, Row2: Integer): TRect;
var
  R1, R2: TRect;
begin
  R1 := GetRowRect(Row1);
  R2 := GetRowRect(Row2);
  UnionRect(Result, R1, R2);
end;

function TCustomGridView.GetRowsHeight(Row1, Row2: Integer): Integer;
begin
  Result := 0;
  if Row2 >= Row1 then
    Result := (Row2 - Row1 + 1) * Rows.Height;
end;

function TCustomGridView.GetRowTopBottom(Row: Integer): TRect;
begin
  {the top and the bottom of rectangle is calculated from number and height of line}
  Result.Top := GetGridRect.Top + GetRowsHeight(0, Row - 1) + GetGridOrigin.Y;
  Result.Bottom := Result.Top + Rows.Height;
end;

function TCustomGridView.GetSectionAt(X, Y: Integer): TGridHeaderSection;

  function FindSection(Sections: TGridHeaderSections;
    var Section: TGridHeaderSection): Boolean;
  var
    I: Integer;
    S: TGridHeaderSection;
    R: TRect;
  begin
    for I := 0 to Sections.Count - 1 do
    begin
      S := Sections[I];
      if S.Visible then
      begin
        R := S.BoundsRect;
        if PtInRect(R, Point(X, Y)) then
        begin
          Section := S;
          Result := True;
          Exit;
        end;
      end;
      { search for section in the sub headers}
      if FindSection(S.Sections, Section) then
      begin
        Result := True;
        Exit;
      end;
    end;
    Section := nil;
    Result := False;
  end;

begin
  FindSection(Header.Sections, Result);
end;

procedure TCustomGridView.Invalidate;
begin
  if (Parent <> nil) and (FUpdateLock = 0) then
    inherited;
end;

procedure TCustomGridView.InvalidateCell(Cell: TGridCell);
begin
  if (Cell.Row = CellFocused.Row) or (Cell.Col = CellFocused.Col) then
    HideFocus;
  InvalidateRect(GetCellRect(Cell));
  if (Cell.Row = CellFocused.Row) or (Cell.Col = CellFocused.Col) then
    ShowFocus;
end;

procedure TCustomGridView.InvalidateCheck(Cell: TGridCell);
begin
  InvalidateRect(GetCheckRect(Cell));
end;

procedure TCustomGridView.InvalidateColumn(Column: Integer);
begin
  HideFocus;
  InvalidateRect(GetColumnRect(Column));
  ShowFocus;
end;

procedure TCustomGridView.InvalidateColumns(Column1, Column2: Integer);
begin
  HideFocus;
  InvalidateRect(GetColumnsRect(Column1, Column2));
  ShowFocus;
end;

procedure TCustomGridView.InvalidateEdit;
begin
  if Editing then
    Edit.Invalidate;
end;

procedure TCustomGridView.InvalidateFixed;
begin
  InvalidateRect(GetFixedRect);
end;

procedure TCustomGridView.InvalidateFocus;
var
  Rect: TRect;
begin
  Rect := GetFocusRect;
  if RowSelect then
    UnionRect(Rect, Rect, GetCellRect(GridCell(Fixed.Count, CellFocused.Row)))
  else
    UnionRect(Rect, Rect, GetCellRect(CellFocused));
  InvalidateRect(Rect);
end;

procedure TCustomGridView.InvalidateGrid;
begin
  InvalidateEdit;
  InvalidateRect(GetGridRect);
end;

procedure TCustomGridView.InvalidateHeader;
begin
  if ShowHeader then
    InvalidateRect(GetHeaderRect);
end;

procedure TCustomGridView.InvalidateRect(Rect: TRect);
begin
  if (FUpdateLock = 0) and HandleAllocated and Visible then
    Winapi.Windows.InvalidateRect(Handle, @Rect, False);
end;

procedure TCustomGridView.InvalidateRow(Row: Integer);
begin
  if Row = CellFocused.Row then
    HideFocus;
  InvalidateRect(GetRowRect(Row));
  if Row = CellFocused.Row then
    ShowFocus;
end;

procedure TCustomGridView.InvalidateRows(Row1, Row2: Integer);
begin
  HideFocus;
  InvalidateRect(GetRowsRect(Row1, Row2));
  ShowFocus;
end;

function TCustomGridView.IsActiveControl: Boolean;
var
  H: HWND;
begin
  if (GetParentForm(Self) <> nil) and
    (GetParentForm(Self).ActiveControl = Self) then
  begin
    Result := True;
    Exit;
  end;
  H := GetFocus;
  while IsWindow(H) do
  begin
    if H = WindowHandle then
    begin
      Result := True;
      Exit;
    end;
    H := GetParent(H);
  end;
  Result := False;
end;

function TCustomGridView.IsCellAcceptCursor(Cell: TGridCell): Boolean;
begin
  if not IsCellValid(Cell) then
  begin
    Result := False;
    Exit;
  end;
  Result := (Cell.Col >= Fixed.Count) and Columns[Cell.Col].TabStop;
  if Assigned(FOnCellAcceptCursor) then
    FOnCellAcceptCursor(Self, Cell, Result);
end;

function TCustomGridView.IsCellHighlighted(Cell: TGridCell): Boolean;
begin
  Result := CellSelected and IsCellFocused(Cell) and
    ((not HideSelection) or Focused);
end;

function TCustomGridView.IsCellHasCheck(Cell: TGridCell): Boolean;
begin
  Result := IsCellValid(Cell) and CheckBoxes and (GetCheckKind(Cell) <> gcNone);
end;

function TCustomGridView.IsCellHasImage(Cell: TGridCell): Boolean;
begin
  Result := IsCellValid(Cell) and Assigned(Images) and
    (GetCellImage(Cell) <> -1);
end;

function TCustomGridView.IsCellFixed(Cell: TGridCell): Boolean;
begin
  Result := (Fixed.Count > 0) and (Cell.Col < Fixed.Count);
end;

function TCustomGridView.IsCellFocused(Cell: TGridCell): Boolean;
begin
  Result := ((Cell.Col = CellFocused.Col) or RowSelect) and
    (Cell.Row = CellFocused.Row) and
    ((Cell.Col >= Fixed.Count) or
    ((Cell.Col = -1) and (gsFullRowPaint in GridStyle)));
end;

function TCustomGridView.IsCellReadOnly(Cell: TGridCell): Boolean;
begin
  Result := True;
  if IsCellValid(Cell) then
  begin
    Result := ReadOnly or (Cell.Col < Fixed.Count) or Columns[Cell.Col].ReadOnly;
    if Assigned(FOnGetCellReadOnly) then
      FOnGetCellReadOnly(Self, Cell, Result);
  end;
end;

function TCustomGridView.IsCellValid(Cell: TGridCell): Boolean;
begin
  Result := IsCellValidEx(Cell, True, True);
end;

function TCustomGridView.IsCellValidEx(Cell: TGridCell; CheckPosition,
  CheckVisible: Boolean): Boolean;
var
  C, R, V: Boolean;
begin
  { determine visibility, width of column and correctness of cell}
  with Cell do
  begin
    C := (Col >= 0) and (Col < Columns.Count);
    R := (Row >= 0) and (Row < Rows.Count);
    V := C and Columns[Col].Visible and (Columns[Col].Width > 0);
  end;
  Result := ((not CheckPosition) or (C and R)) and ((not CheckVisible) or V);
end;

function TCustomGridView.IsCellVisible(Cell: TGridCell; PartialOK: Boolean):
  Boolean;
var
  CR, GR, R: TRect;
begin
  { obtain the boundaries of cell and grid}
  CR := GetCellRect(Cell);
  GR := GetGridRect;
  {if there are those fixed and cell is not fixed, then left boundary exists
    the boundary of those fixed, but not the boundary of table}
  if (Fixed.Count > 0) and (Cell.Col >- Fixed.Count) then
    GR.Left := GetFixedRect.Right;
  Result := IntersectRect(R, CR, GR);
  { complete visibility }
  if not PartialOK then
    Result := EqualRect(R, CR);
end;

function TCustomGridView.IsColumnVisible(Column: Integer): Boolean;
var
  R: TRect;
begin
  Result := IntersectRect(R, GetColumnRect(Column), GetGridRect);
end;

function TCustomGridView.IsFocusAllowed: Boolean;
begin
  Result := (RowSelect or (not (Editing or AlwaysEdit))) and AllowSelect;
end;

function TCustomGridView.IsHeaderHasImage(Section: TGridHeaderSection): Boolean;
begin
  Result := Assigned(Header.Images) and (GetHeaderImage(Section) <> -1);
end;

function TCustomGridView.IsHeaderPressed(Section: TGridHeaderSection): Boolean;
begin
  Result := ((Section = nil) or (Section = FHeaderClickSection)) and
    FHeaderClickState;
end;

function TCustomGridView.IsRowVisible(Row: Integer): Boolean;
var
  R: TRect;
begin
  Result := IntersectRect(R, GetRowRect(Row), GetGridRect);
end;

procedure TCustomGridView.LockUpdate;
begin
  Inc(FUpdateLock);
end;

procedure TCustomGridView.MakeCellVisible(Cell: TGridCell; PartialOK: Boolean);
var
  DX, DY, X, Y: Integer;
  R: TRect;
begin
  if not IsCellVisible(Cell, PartialOK) then
  begin
    DX := 0;
    DY := 0;
    with GetGridRect do
    begin
      {displacement along horizontal}
      if not RowSelect then
      begin
        R := GetColumnRect(Cell.Col);
        X := Left + GetFixedWidth;
        if R.Right > Right then
          DX := Right - R.Right;
        if R.Left < X then
          DX := X - R.Left;
        if R.Right - R.Left > Right - X then
          DX := X - R.Left;
      end;
      {displacement on the vertical line}
      if Rows.Height > 0 then
      begin
        R := GetRowRect(Cell.Row);
        if R.Bottom > Bottom then
          DY := Bottom - R.Bottom;
        if R.Top < Top then
          DY := Top - R.Top;
        if R.Bottom - R.Top > Bottom - Top then
          DY := Top - R.Top;
        Y := DY div Rows.Height;
        if (FVisSize.Row > 1) and (DY mod Rows.Height <> 0) then
          Dec(Y);
        DY := Y;
      end;
    end;
    { change position}
    with VertScrollBar do
      Position := Position - DY;
    with HorzScrollBar do
      Position := Position - DX;
  end;
end;

procedure TCustomGridView.SetCursor(Cell: TGridCell;
  Selected, Visible: Boolean);
begin
  { Validate position and selection }
  UpdateSelection(Cell, Selected);
  { If selection or focus was changed by UpdateSelection, then update the grid }
  if (not IsCellEqual(FCellFocused, Cell)) or (FCellSelected <> Selected) then
  begin
    { Dispatch <OnChanging> event }
    Changing(Cell, Selected);
    { Set focus to the active cell. }
    if not IsCellEqual(FCellFocused, Cell) then
    begin
      { Hide cell tips. }
      CancelCellTips;
      Editing := False;
      HideCursor;
      FCellFocused := Cell;
      FCellSelected := Selected;
      if Visible then
        MakeCellVisible(CellFocused, False);
      ShowCursor;
    end
    { Update selection on the active cell. }
    else if FCellSelected <> Selected then
    begin
      if Editing then
        ShowEdit;
      if not Editing then
      begin
        HideCursor;
        FCellSelected := Selected;
        if Visible then
          MakeCellVisible(CellFocused, False);
        ShowCursor;
      end;
    end;
    { Dispatch <OnChange> event }
    Change(FCellFocused, FCellSelected);
  end
  else
    { Cell did not change - we update visibility }
    if Visible then
      MakeCellVisible(CellFocused, False);
end;

procedure TCustomGridView.UndoEdit;
begin
  if (FEdit <> nil) and EditCanUndo(EditCell) then
    FEdit.Perform(WM_UNDO, 0, 0);
end;

procedure TCustomGridView.UnLockUpdate(Redraw: Boolean);
begin
  Dec(FUpdateLock);
  if (FUpdateLock = 0) and Redraw then
    Invalidate;
end;

procedure TCustomGridView.UpdateCursor;
var
  Cell: TGridCell;
  IsValidCell, Dummy: Boolean;
begin
  Cell := CellFocused;
  IsValidCell := IsCellValid(Cell) and IsCellAcceptCursor(Cell);
  { if the current cell is inaccessible, then we search for accessible cell all
    around or first fallen, if there is no such}
  if not IsValidCell then
  begin
    UpdateSelection(Cell, Dummy);
    if IsCellEqual(Cell, CellFocused) then
      Cell := GetCursorCell(Cell, goFirst);
  end;
  { correct the isolation of cell}
  SetCursor(Cell, CellSelected, not IsValidCell);
end;

procedure TCustomGridView.UpdateColors;
begin
  Header.GridColorChanged(Color);
  Fixed.GridColorChanged(Color);
end;

procedure TCustomGridView.UpdateEdit(Activate: Boolean);

  procedure DoValidateEdit;
  var
    EditClass: TGridEditClass;
  begin
    EditClass := GetEditClass(FCellFocused);
    if (FEdit = nil) or (FEdit.ClassType <> EditClass) then
    begin
      FreeAndNil(FEdit);
      FEdit := CreateEdit(EditClass);
      FEdit.Parent := Self;
      FEdit.FGrid := Self;
    end;
  end;

  procedure DoUpdateEdit;
  begin
    FEditCell := FCellFocused;
    FEdit.Updating;
    FEdit.UpdateContents;
    FEdit.UpdateStyle;
    FEdit.Updated;
    FEdit.SelectAll;
  end;

begin
  if EditCanShow(FCellFocused) then
  begin
    if FEdit = nil then
    begin
      DoValidateEdit;
      DoUpdateEdit;
    end
    else if not (IsCellEqual(FEditCell, FCellFocused) and Editing) then
    begin
      Activate := Activate or Editing or AlwaysEdit;
      HideEdit;
      DoValidateEdit;
      DoUpdateEdit;
    end;
    if Activate then
      FEdit.Show;
  end
  else
    HideEdit;
end;

procedure TCustomGridView.UpdateEditContents(SaveText: Boolean);
var
  EditText: string;
begin
  if Editing then
  begin
    EditText := Edit.Text;
    HideEdit;
    UpdateEdit(True);
    if SaveText then
      Edit.Text := EditText;
  end;
end;

procedure TCustomGridView.UpdateEditText;
var
  EditFocused: Boolean;
  EditText: string;
begin
  if (not ReadOnly) and (Edit <> nil) and (not IsCellReadOnly(EditCell)) then
  begin
    EditFocused := Editing;
    try
      EditText := Edit.Text;
      try
        SetEditText(EditCell, EditText);
      finally
        Edit.Text := EditText;
      end;
    except
      on E: Exception do
      begin
        { do not give to be shifted to the cursor of scroller}
        MakeCellVisible(CellFocused, False);
        {if line is visible - focus to it, otherwise it will be extinguished afterward
          opening window with the communication about the error}
        if EditFocused then
          Edit.SetFocus;
        {error}
        raise;
      end;
    end;
  end;
end;

procedure TCustomGridView.UpdateFixed;
begin
  if Assigned(Fixed) then
  Fixed.SetCount(Fixed.Count);
end;

procedure TCustomGridView.UpdateFocus;
begin
  if csDesigning in ComponentState then
    Exit;
  if IsActiveControl then
  begin
    Winapi.Windows.SetFocus(Handle);
    if GetFocus = Handle then
      Perform(CM_UIACTIVATE, 0, 0);
  end
  else
    if IsWindowVisible(Handle) and TabStop and
    (CanFocus or (GetParentForm(Self) = nil)) then
    begin
      SetFocus;
      if AlwaysEdit and (Edit <> nil) then
        UpdateEdit(True);
    end;
end;

procedure TCustomGridView.UpdateFonts;
begin
  Header.GridFontChanged(Font);
  Fixed.GridFontChanged(Font);
end;

procedure TCustomGridView.UpdateHeader;
begin
  with Header do
  begin
    if AutoSynchronize or Synchronized then
      SynchronizeSections
    else
      UpdateSections;
    SetSectionHeight(SectionHeight);
  end;
end;

procedure TCustomGridView.UpdateRows;
begin
  Rows.SetHeight(Rows.Height);
end;

procedure TCustomGridView.UpdateScrollBars;
var
  R, P, L: Integer;
begin
  { block repeated updates when dimensions of the window change, which
    compulsorily it will occur with the demonstration of scroller or with its extinction}
  LockUpdate;
  try
    {the parameters of vertical scroller}
    if (Rows.Count > 0) and (Rows.Height > 0) then
    begin
      R := Rows.Count - 1;
      with GetGridRect do
        P := (Bottom - Top) div Rows.Height - 1;
      L := 1;
    end
    else
    begin
      R := 0;
      P := 0;
      L := 0;
    end;
    VertScrollBar.SetLineSize(Rows.Height);
    VertScrollBar.SetParams(0, R, P, L);
    if Columns.Count > 0 then
    begin
      R := GetColumnsWidth(0, Columns.Count - 1) - GetFixedWidth;
      with GetGridRect do
        P := (Right - Left) - GetFixedWidth;
      L := 8;
    end
    else
    begin
      R := 0;
      P := 0;
      L := 0;
    end;
    HorzScrollBar.SetLineSize(1);
    HorzScrollBar.SetParams(0, R, P, L);
  finally
    UnLockUpdate(False);
  end;
end;

procedure TCustomGridView.UpdateScrollPos;
begin
  VertScrollBar.Position := FVisOrigin.Row;
  HorzScrollBar.Position := GetColumnsWidth(Fixed.Count, FVisOrigin.Col - 1);
end;

procedure TCustomGridView.UpdateSelection(var Cell: TGridCell;
  var Selected: Boolean);
begin
  Selected := Selected or FAlwaysSelected;
  Selected := Selected and (Rows.Count > 0) and (Columns.Count > 0);
  { Check if the cell position is valid and update it if needed. }
  with Cell do
  begin
    if Col < Fixed.Count then
      Col := Fixed.Count;
    if Col < 0 then
      Col := 0;
    if Col > Columns.Count - 1 then
      Col := Columns.Count - 1;

    if Row < 0 then
      Row := 0;
    if Row > Rows.Count - 1 then
      Row := Rows.Count - 1;
  end;
  Cell := GetCursorCell(Cell, goSelect);
end;

procedure TCustomGridView.UpdateText;
begin
  UpdateEditText;
end;

procedure TCustomGridView.UpdateVisOriginSize;
var
  R       : TRect;
  I, X, H : Integer;
begin
  if FColResizing then
    Exit;
  if Columns.Count > 0 then
  begin
    { search for the first-encountered visible unfixed column}
    X := GetClientRect.Left + GetFixedWidth - HorzScrollBar.Position;
    R := GetFixedRect;
    I := Fixed.Count;
    while I < Columns.Count - 1 do
    begin
      X := X + Columns[I].Width;
      if X >= R.Right then
        Break;
      Inc(I);
    end;
    FVisOrigin.Col := I;
    { count a quantity of visible columns}
    R := GetGridRect;
    while I < Columns.Count - 1 do
    begin
      if X >= R.Right then
        Break;
      Inc(I);
      X := X + Columns[I].Width;
    end;
    FVisSize.Col := I - FVisOrigin.Col + 1;
  end
  else
  begin
    FVisOrigin.Col := 0;
    FVisSize.Col := 0;
  end;
  if (Rows.Count > 0) and (Rows.Height > 0) then
  begin
    {vertical cursor is determined the number of the first visible line}
    FVisOrigin.Row := VertScrollBar.Position;
    { count a quantity of visible (even partially) lines}
    H := GetGridHeight;
    FVisSize.Row := H div Rows.Height + Ord(H mod Rows.Height > 0);
    if FVisSize.Row + FVisOrigin.Row  > Rows.Count then
      FVisSize.Row := Rows.Count - FVisOrigin.Row;
    if FVisSize.Row < 0 then
      FVisSize.Row := 0;
  end
  else
  begin
    FVisOrigin.Row := 0;
    FVisSize.Row := 0;
  end;
end;

{$REGION 'TGridViewVclStyleScrollBarsHook'}
{$if CompilerVersion >= 23}
procedure TGridViewVclStyleScrollBarsHook.CalcScrollBarsRect;
var
  P: TPoint;
  BorderValue: TSize;
  BarInfo: TScrollBarInfo;
  I: Integer;

  procedure CalcVerticalRects;
  begin
    BarInfo.cbSize := SizeOf(BarInfo);
    GetScrollBarInfo(Handle, Integer(OBJID_VSCROLL), BarInfo);
    FVertScrollBarWindow.Visible :=
      not(STATE_SYSTEM_INVISIBLE and BarInfo.rgstate[0] <> 0);
    FVertScrollBarWindow.Enabled :=
      not(STATE_SYSTEM_UNAVAILABLE and BarInfo.rgstate[0] <> 0);
    if FVertScrollBarWindow.Visible then
    begin
      // ScrollBar Rect
      P := BarInfo.rcScrollBar.TopLeft;
      ScreenToClient(Handle, P);
      FVertScrollBarRect.TopLeft := P;
      P := BarInfo.rcScrollBar.BottomRight;
      ScreenToClient(Handle, P);
      FVertScrollBarRect.BottomRight := P;
      OffsetRect(FVertScrollBarRect, BorderValue.cx, BorderValue.cy);

      I := GetSystemMetrics(SM_CYVTHUMB);
      // Down Button
      FVertScrollBarDownButtonRect := FVertScrollBarRect;
      FVertScrollBarDownButtonRect.Top := FVertScrollBarDownButtonRect.Bottom - I;

      // UP Button
      FVertScrollBarUpButtonRect := FVertScrollBarRect;
      FVertScrollBarUpButtonRect.Bottom := FVertScrollBarUpButtonRect.Top + I;

      FVertScrollBarSliderTrackRect := FVertScrollBarRect;
      Inc(FVertScrollBarSliderTrackRect.Top, I);
      Dec(FVertScrollBarSliderTrackRect.Bottom, I);
    end;
  end;

  procedure CalcHorizontalRects;
  begin
    BarInfo.cbSize := SizeOf(BarInfo);
    GetScrollBarInfo(Handle, Integer(OBJID_HSCROLL), BarInfo);
    FHorzScrollBarWindow.Visible :=
      not(STATE_SYSTEM_INVISIBLE and BarInfo.rgstate[0] <> 0);
    FHorzScrollBarWindow.Enabled :=
      not(STATE_SYSTEM_UNAVAILABLE and BarInfo.rgstate[0] <> 0);
    if FHorzScrollBarWindow.Visible then
    begin
      // ScrollBar Rect
      P := BarInfo.rcScrollBar.TopLeft;
      ScreenToClient(Handle, P);
      FHorzScrollBarRect.TopLeft := P;
      P := BarInfo.rcScrollBar.BottomRight;
      ScreenToClient(Handle, P);
      FHorzScrollBarRect.BottomRight := P;
      OffsetRect(FHorzScrollBarRect, BorderValue.cx, BorderValue.cy);

      I := GetSystemMetrics(SM_CXHTHUMB);
      // Down Button
      FHorzScrollBarDownButtonRect := FHorzScrollBarRect;
      FHorzScrollBarDownButtonRect.Left := FHorzScrollBarDownButtonRect.Right - I;

      // UP Button
      FHorzScrollBarUpButtonRect := FHorzScrollBarRect;
      FHorzScrollBarUpButtonRect.Right := FHorzScrollBarUpButtonRect.Left + I;

      FHorzScrollBarSliderTrackRect := FHorzScrollBarRect;
      Inc(FHorzScrollBarSliderTrackRect.Left, I);
      Dec(FHorzScrollBarSliderTrackRect.Right, I);
    end;
  end;

begin
  BorderValue.cx := 0;
  BorderValue.cy := 0;
  if HasBorder then
    if HasClientEdge then
    begin
      BorderValue.cx := GetSystemMetrics(SM_CXEDGE);
      BorderValue.cy := GetSystemMetrics(SM_CYEDGE);
    end;
  CalcVerticalRects;
  CalcHorizontalRects;
end;

constructor TGridViewVclStyleScrollBarsHook.Create(AControl: TWinControl);
begin
  inherited;
  FVertScrollBarWindow :=
    TVclStyleScrollBarWindow.CreateParented(GetParent(Control.Handle));
  FVertScrollBarWindow.ScrollBarWindowOwner := Self;
  FVertScrollBarWindow.ScrollBarVertical := True;

  FHorzScrollBarWindow :=
    TVclStyleScrollBarWindow.CreateParented(GetParent(Control.Handle));
  FHorzScrollBarWindow.ScrollBarWindowOwner := Self;

  FVertScrollBarSliderState := tsThumbBtnVertNormal;
  FVertScrollBarUpButtonState := tsArrowBtnUpNormal;
  FVertScrollBarDownButtonState := tsArrowBtnDownNormal;
  FHorzScrollBarSliderState := tsThumbBtnHorzNormal;
  FHorzScrollBarUpButtonState := tsArrowBtnLeftNormal;
  FHorzScrollBarDownButtonState := tsArrowBtnRightNormal;
end;

destructor TGridViewVclStyleScrollBarsHook.Destroy;
begin
  FVertScrollBarWindow.ScrollBarWindowOwner := nil;
  FreeAndNil(FVertScrollBarWindow);
  FHorzScrollBarWindow.ScrollBarWindowOwner := nil;
  FreeAndNil(FHorzScrollBarWindow);
  inherited;
end;

procedure TGridViewVclStyleScrollBarsHook.DrawHorzScrollBar(DC: HDC);
var
  B: TBitmap;
  Details: TThemedElementDetails;
  R: TRect;
begin
  if ((Handle = 0) or (DC = 0)) then
    Exit;
  if FHorzScrollBarWindow.Visible and StyleServices.Available then
  begin
    B := TBitmap.Create;
    try
      B.Width := FHorzScrollBarRect.Width;
      B.Height := FHorzScrollBarRect.Height;
      MoveWindowOrg(B.Canvas.Handle, -FHorzScrollBarRect.Left, -FHorzScrollBarRect.Top);
      R := FHorzScrollBarRect;
      R.Left := FHorzScrollBarUpButtonRect.Right;
      R.Right := FHorzScrollBarDownButtonRect.Left;

      Details := StyleServices.GetElementDetails(tsUpperTrackHorzNormal);
      StyleServices.DrawElement(B.Canvas.Handle, Details, R);

      if FHorzScrollBarWindow.Enabled then
        Details := StyleServices.GetElementDetails(FHorzScrollBarSliderState);
      StyleServices.DrawElement(B.Canvas.Handle, Details, GetHorzScrollBarSliderRect);

      if FHorzScrollBarWindow.Enabled then
        Details := StyleServices.GetElementDetails(FHorzScrollBarUpButtonState)
      else
        Details := StyleServices.GetElementDetails(tsArrowBtnLeftDisabled);
      StyleServices.DrawElement(B.Canvas.Handle, Details, FHorzScrollBarUpButtonRect);

      if FHorzScrollBarWindow.Enabled then
        Details := StyleServices.GetElementDetails(FHorzScrollBarDownButtonState)
      else
        Details := StyleServices.GetElementDetails(tsArrowBtnRightDisabled);
      StyleServices.DrawElement(B.Canvas.Handle, Details, FHorzScrollBarDownButtonRect);

      MoveWindowOrg(B.Canvas.Handle, FHorzScrollBarRect.Left, FHorzScrollBarRect.Top);
      with FHorzScrollBarRect do
        BitBlt(DC, Left, Top, B.Width, B.Height, B.Canvas.Handle, 0, 0, SRCCOPY);
    finally
      B.Free;
    end;
  end;
end;

procedure TGridViewVclStyleScrollBarsHook.DrawVertScrollBar(DC: HDC);
var
  B: TBitmap;
  Details: TThemedElementDetails;
  R: TRect;
begin
  if ((Handle = 0) or (DC = 0)) then
    Exit;
  if FVertScrollBarWindow.Visible and StyleServices.Available then
  begin
    B := TBitmap.Create;
    try
      B.Width := FVertScrollBarRect.Width;
      B.Height := FVertScrollBarWindow.Height;
      MoveWindowOrg(B.Canvas.Handle, -FVertScrollBarRect.Left, -FVertScrollBarRect.Top);
      R := FVertScrollBarRect;
      R.Bottom := B.Height + FVertScrollBarRect.Top;
      Details := StyleServices.GetElementDetails(tsUpperTrackVertNormal);
      StyleServices.DrawElement(B.Canvas.Handle, Details, R);
      R.Top := FVertScrollBarUpButtonRect.Bottom;
      R.Bottom := FVertScrollBarDownButtonRect.Top;

      Details := StyleServices.GetElementDetails(tsUpperTrackVertNormal);
      StyleServices.DrawElement(B.Canvas.Handle, Details, R);

      if FVertScrollBarWindow.Enabled then
        Details := StyleServices.GetElementDetails(FVertScrollBarSliderState);
      StyleServices.DrawElement(B.Canvas.Handle, Details, GetVertScrollBarSliderRect);

      if FVertScrollBarWindow.Enabled then
        Details := StyleServices.GetElementDetails(FVertScrollBarUpButtonState)
      else
        Details := StyleServices.GetElementDetails(tsArrowBtnUpDisabled);
      StyleServices.DrawElement(B.Canvas.Handle, Details, FVertScrollBarUpButtonRect);

      if FVertScrollBarWindow.Enabled then
        Details := StyleServices.GetElementDetails(FVertScrollBarDownButtonState)
      else
        Details := StyleServices.GetElementDetails(tsArrowBtnDownDisabled);
      StyleServices.DrawElement(B.Canvas.Handle, Details, FVertScrollBarDownButtonRect);

      MoveWindowOrg(B.Canvas.Handle, FVertScrollBarRect.Left, FVertScrollBarRect.Top);
      with FVertScrollBarRect do
        BitBlt(DC, Left, Top, B.Width, B.Height, B.Canvas.Handle, 0, 0, SRCCOPY);
    finally
      B.Free;
    end;
  end;
end;

function TGridViewVclStyleScrollBarsHook.GetHorzScrollBarSliderRect: TRect;
var
  P: TPoint;
  BarInfo: TScrollBarInfo;
begin
  if FHorzScrollBarWindow.Visible and FHorzScrollBarWindow.Enabled then
  begin
    BarInfo.cbSize := SizeOf(BarInfo);
    GetScrollBarInfo(Handle, Integer(OBJID_HSCROLL), BarInfo);
    P := BarInfo.rcScrollBar.TopLeft;
    ScreenToClient(Handle, P);
    Result.TopLeft := P;
    P := BarInfo.rcScrollBar.BottomRight;
    ScreenToClient(Handle, P);
    Result.BottomRight := P;
    Result.Left := BarInfo.xyThumbTop;
    Result.Right := BarInfo.xyThumbBottom;
    if HasBorder then
      if HasClientEdge then
        OffsetRect(Result, 2, 2)
      else
        OffsetRect(Result, 1, 1);
  end;
end;

function TGridViewVclStyleScrollBarsHook.GetVertScrollBarSliderRect: TRect;
var
  P: TPoint;
  BarInfo: TScrollBarInfo;
begin
  if FVertScrollBarWindow.Visible and FVertScrollBarWindow.Enabled then
  begin
    BarInfo.cbSize := SizeOf(BarInfo);
    GetScrollBarInfo(Handle, Integer(OBJID_VSCROLL), BarInfo);
    P := BarInfo.rcScrollBar.TopLeft;
    ScreenToClient(Handle, P);
    Result.TopLeft := P;
    P := BarInfo.rcScrollBar.BottomRight;
    ScreenToClient(Handle, P);
    Result.BottomRight := P;
    Result.Top := BarInfo.xyThumbTop;
    Result.Bottom := BarInfo.xyThumbBottom;
    if HasBorder then
      if HasClientEdge then
        OffsetRect(Result, 2, 2)
      else
        OffsetRect(Result, 1, 1);
  end;
end;

procedure TGridViewVclStyleScrollBarsHook.MouseLeave;
begin
   inherited MouseLeave;
  if FVertScrollBarSliderState = tsThumbBtnVertHot then
    FVertScrollBarSliderState := tsThumbBtnVertNormal;

  if FHorzScrollBarSliderState = tsThumbBtnHorzHot then
    FHorzScrollBarSliderState := tsThumbBtnHorzNormal;

  if FVertScrollBarUpButtonState = tsArrowBtnUpHot then
    FVertScrollBarUpButtonState := tsArrowBtnUpNormal;

  if FVertScrollBarDownButtonState = tsArrowBtnDownHot then
    FVertScrollBarDownButtonState := tsArrowBtnDownNormal;

  if FHorzScrollBarUpButtonState = tsArrowBtnLeftHot then
    FHorzScrollBarUpButtonState := tsArrowBtnLeftNormal;

  if FHorzScrollBarDownButtonState = tsArrowBtnRightHot then
    FHorzScrollBarDownButtonState := tsArrowBtnRightNormal;

  PaintScrollBars;
end;

procedure TGridViewVclStyleScrollBarsHook.PaintScrollBars;
begin
  FVertScrollBarWindow.Repaint;
  FHorzScrollBarWindow.Repaint;
end;

function TGridViewVclStyleScrollBarsHook.PointInTreeHeader(const P: TPoint): Boolean;
begin
  Result := PtInRect(TCustomGridView(Control).GetHeaderRect, P);
end;

procedure TGridViewVclStyleScrollBarsHook.UpdateScrollBarWindow;
var
  R            : TRect;
  HeaderHeight : Integer;
  BorderWidth  : Integer;
begin
  // TS: probably the headerheight never needs to be determined. We need to test
  // this for multiple-section headers.
  HeaderHeight := 0;
  BorderWidth := 0;
  // VertScrollBarWindow
  if FVertScrollBarWindow.Visible then
  begin
    R := FVertScrollBarRect;
    if Control.BidiMode = bdRightToLeft then
    begin
      OffsetRect(R, -R.Left, 0);
      if HasBorder then
        OffsetRect(R, GetSystemMetrics(SM_CXEDGE), 0);
    end;
    if HasBorder then
      BorderWidth := GetSystemMetrics(SM_CYEDGE) * 2;
    ShowWindow(FVertScrollBarWindow.Handle, SW_SHOW);
    SetWindowPos(
      FVertScrollBarWindow.Handle,
      HWND_TOP,
      Control.Left + R.Left,
      Control.Top + R.Top + HeaderHeight,
      R.Right - R.Left,
      Control.Height - HeaderHeight  - BorderWidth,
      SWP_SHOWWINDOW
    );
  end
  else
    ShowWindow(FVertScrollBarWindow.Handle, SW_HIDE);

  // HorzScrollBarWindow
  if FHorzScrollBarWindow.Visible then
  begin
    R := FHorzScrollBarRect;
    if Control.BidiMode = bdRightToLeft then
      OffsetRect(R, FVertScrollBarRect.Width, 0);
    ShowWindow(FHorzScrollBarWindow.Handle, SW_SHOW);
    SetWindowPos(
      FHorzScrollBarWindow.Handle,
      HWND_TOP,
      Control.Left + R.Left,
      Control.Top + R.Top + HeaderHeight,
      R.Right - R.Left,
      R.Bottom - R.Top,
      SWP_SHOWWINDOW
    );
  end
  else
    ShowWindow(FHorzScrollBarWindow.Handle, SW_HIDE);
end;

procedure TGridViewVclStyleScrollBarsHook.WMCaptureChanged(var Msg: TMessage);
begin
   if FVertScrollBarWindow.Visible and FVertScrollBarWindow.Enabled then
  begin
    if FVertScrollBarUpButtonState = tsArrowBtnUpPressed then
    begin
      FVertScrollBarUpButtonState := tsArrowBtnUpNormal;
      PaintScrollBars;
    end;

    if FVertScrollBarDownButtonState = tsArrowBtnDownPressed then
    begin
      FVertScrollBarDownButtonState := tsArrowBtnDownNormal;
      PaintScrollBars;
    end;
  end;

  if FHorzScrollBarWindow.Visible and FHorzScrollBarWindow.Enabled then
  begin
    if FHorzScrollBarUpButtonState = tsArrowBtnLeftPressed then
    begin
      FHorzScrollBarUpButtonState := tsArrowBtnLeftNormal;
      PaintScrollBars;
    end;

    if FHorzScrollBarDownButtonState = tsArrowBtnRightPressed then
    begin
      FHorzScrollBarDownButtonState := tsArrowBtnRightNormal;
      PaintScrollBars;
    end;
  end;

  CallDefaultProc(TMessage(Msg));
  Handled := True;
end;

procedure TGridViewVclStyleScrollBarsHook.WMHScroll(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScrollBars;
  Handled := True;
end;

procedure TGridViewVclStyleScrollBarsHook.WMKeyDown(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScrollBars;
  Handled := True;
end;

procedure TGridViewVclStyleScrollBarsHook.WMKeyUp(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScrollBars;
  Handled := True;
end;

procedure TGridViewVclStyleScrollBarsHook.WMLButtonDown(var Msg: TWMMouse);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScrollBars;
  Handled := True;
end;

procedure TGridViewVclStyleScrollBarsHook.WMLButtonUp(var Msg: TWMMouse);
var
  P: TPoint;
begin
  P := Point(Msg.XPos, Msg.YPos);
  ScreenToClient(Handle, P);
  if not PointInTreeHeader(P) then
  begin
    if FVertScrollBarWindow.Visible then
    begin
      if FVertScrollBarSliderState = tsThumbBtnVertPressed then
      begin
        PostMessage(Handle, WM_VSCROLL, Integer(SmallPoint(SB_ENDSCROLL, 0)), 0);
        FLeftMouseButtonDown := False;
        FVertScrollBarSliderState := tsThumbBtnVertNormal;
        PaintScrollBars;
        Handled := True;
        ReleaseCapture;
        Exit;
      end;

      if FVertScrollBarUpButtonState = tsArrowBtnUpPressed then
        FVertScrollBarUpButtonState := tsArrowBtnUpNormal;

      if FVertScrollBarDownButtonState = tsArrowBtnDownPressed then
        FVertScrollBarDownButtonState := tsArrowBtnDownNormal;
    end;

    if FHorzScrollBarWindow.Visible then
    begin
      if FHorzScrollBarSliderState = tsThumbBtnHorzPressed then
      begin
        PostMessage(Handle, WM_HSCROLL, Integer(SmallPoint(SB_ENDSCROLL, 0)), 0);
        FLeftMouseButtonDown := False;
        FHorzScrollBarSliderState := tsThumbBtnHorzNormal;
        PaintScrollBars;
        Handled := True;
        ReleaseCapture;
        Exit;
      end;

      if FHorzScrollBarUpButtonState = tsArrowBtnLeftPressed then
        FHorzScrollBarUpButtonState := tsArrowBtnLeftNormal;

      if FHorzScrollBarDownButtonState = tsArrowBtnRightPressed then
        FHorzScrollBarDownButtonState := tsArrowBtnRightNormal;
    end;
    PaintScrollBars;
  end;
  FLeftMouseButtonDown := False;
end;

procedure TGridViewVclStyleScrollBarsHook.WMMouseMove(var Msg: TWMMouse);
var
  SF: TScrollInfo;
begin
  inherited;
  if FVertScrollBarSliderState = tsThumbBtnVertPressed then
  begin
    SF.fMask := SIF_ALL;
    SF.cbSize := SizeOf(SF);
    GetScrollInfo(Handle, SB_VERT, SF);
    if SF.nPos <> Round(FScrollPos) then
      FScrollPos := SF.nPos;

    FScrollPos := FScrollPos + (SF.nMax - SF.nMin) *
      ((Mouse.CursorPos.Y - FPrevScrollPos) /
        FVertScrollBarSliderTrackRect.Height);
    if FScrollPos < SF.nMin then
      FScrollPos := SF.nMin;
    if FScrollPos > SF.nMax then
      FScrollPos := SF.nMax;
    if SF.nPage <> 0 then
      if Round(FScrollPos) > SF.nMax - Integer(SF.nPage) + 1 then
        FScrollPos := SF.nMax - Integer(SF.nPage) + 1;
    FPrevScrollPos := Mouse.CursorPos.Y;
    SF.nPos := Round(FScrollPos);

    SetScrollInfo(Handle, SB_VERT, SF, False);
    PostMessage(
      Handle,
      WM_VSCROLL,
      Integer(SmallPoint(SB_THUMBPOSITION, Round(FScrollPos))),
      0
    );
    PaintScrollBars;
    Handled := True;
    Exit;
  end;

  if FHorzScrollBarSliderState = tsThumbBtnHorzPressed then
  begin
    SF.fMask := SIF_ALL;
    SF.cbSize := SizeOf(SF);
    GetScrollInfo(Handle, SB_HORZ, SF);
    if SF.nPos <> Round(FScrollPos) then
      FScrollPos := SF.nPos;

    FScrollPos := FScrollPos + (SF.nMax - SF.nMin) *
      ((Mouse.CursorPos.X - FPrevScrollPos) /
        FHorzScrollBarSliderTrackRect.Width);
    if FScrollPos < SF.nMin then
      FScrollPos := SF.nMin;
    if FScrollPos > SF.nMax then
      FScrollPos := SF.nMax;
    if SF.nPage <> 0 then
      if Round(FScrollPos) > SF.nMax - Integer(SF.nPage) + 1 then
        FScrollPos := SF.nMax - Integer(SF.nPage) + 1;
    FPrevScrollPos := Mouse.CursorPos.X;
    SF.nPos := Round(FScrollPos);

    SetScrollInfo(Handle, SB_HORZ, SF, False);
    PostMessage(
      Handle,
      WM_HSCROLL,
      Integer(SmallPoint(SB_THUMBPOSITION,
      Round(FScrollPos))),
      0
    );
    PaintScrollBars;
    Handled := True;
    Exit;
  end;

  if (FHorzScrollBarSliderState <> tsThumbBtnHorzPressed)
    and (FHorzScrollBarSliderState = tsThumbBtnHorzHot) then
  begin
    FHorzScrollBarSliderState := tsThumbBtnHorzNormal;
    PaintScrollBars;
  end;

  if (FVertScrollBarSliderState <> tsThumbBtnVertPressed)
    and (FVertScrollBarSliderState = tsThumbBtnVertHot) then
  begin
    FVertScrollBarSliderState := tsThumbBtnVertNormal;
    PaintScrollBars;
  end;

  if (FHorzScrollBarUpButtonState <> tsArrowBtnLeftPressed)
    and (FHorzScrollBarUpButtonState = tsArrowBtnLeftHot) then
  begin
    FHorzScrollBarUpButtonState := tsArrowBtnLeftNormal;
    PaintScrollBars;
  end;

  if (FHorzScrollBarDownButtonState <> tsArrowBtnRightPressed)
    and (FHorzScrollBarDownButtonState = tsArrowBtnRightHot) then
  begin
    FHorzScrollBarDownButtonState := tsArrowBtnRightNormal;
    PaintScrollBars;
  end;

  if (FVertScrollBarUpButtonState <> tsArrowBtnUpPressed)
    and (FVertScrollBarUpButtonState = tsArrowBtnUpHot) then
  begin
    FVertScrollBarUpButtonState := tsArrowBtnUpNormal;
    PaintScrollBars;
  end;

  if (FVertScrollBarDownButtonState <> tsArrowBtnDownPressed)
    and (FVertScrollBarDownButtonState = tsArrowBtnDownHot) then
  begin
    FVertScrollBarDownButtonState := tsArrowBtnDownNormal;
    PaintScrollBars;
  end;

  CallDefaultProc(TMessage(Msg));
  if FLeftMouseButtonDown then
    PaintScrollBars;
  Handled := True;
end;

procedure TGridViewVclStyleScrollBarsHook.WMMouseWheel(var Msg: TMessage);
begin
   CallDefaultProc(TMessage(Msg));
  PaintScrollBars;
  Handled := True;
end;

procedure TGridViewVclStyleScrollBarsHook.WMNCLButtonDblClk(var Msg: TWMMouse);
begin
  WMNCLButtonDown(Msg);
end;

procedure TGridViewVclStyleScrollBarsHook.WMNCLButtonDown(var Msg: TWMMouse);
var
  P: TPoint;
  SF: TScrollInfo;
begin
  P := Point(Msg.XPos, Msg.YPos);
  ScreenToClient(Handle, P);

  if HasBorder then
    if HasClientEdge then
    begin
      P.X := P.X + 2;
      P.Y := P.Y + 2;
    end
    else
    begin
      P.X := P.X + 1;
      P.Y := P.Y + 1;
    end;

  if not PointInTreeHeader(P) then
  begin
    if FVertScrollBarWindow.Visible then
    begin
      if PtInRect(GetVertScrollBarSliderRect, P) then
      begin
        FLeftMouseButtonDown := True;
        SF.fMask := SIF_ALL;
        SF.cbSize := SizeOf(SF);
        GetScrollInfo(Handle, SB_VERT, SF);
        FScrollPos := SF.nPos;
        FPrevScrollPos := Mouse.CursorPos.Y;
        FVertScrollBarSliderState := tsThumbBtnVertPressed;
        PaintScrollBars;
        SetCapture(Handle);
        Handled := True;
        Exit;
      end;

      if FVertScrollBarWindow.Enabled then
      begin
        if PtInRect(FVertScrollBarDownButtonRect, P) then
          FVertScrollBarDownButtonState := tsArrowBtnDownPressed;
        if PtInRect(FVertScrollBarUpButtonRect, P) then
          FVertScrollBarUpButtonState := tsArrowBtnUpPressed;
      end;
    end;

    if FHorzScrollBarWindow.Visible then
    begin
      if PtInRect(GetHorzScrollBarSliderRect, P) then
      begin
        FLeftMouseButtonDown := True;
        SF.fMask := SIF_ALL;
        SF.cbSize := SizeOf(SF);
        GetScrollInfo(Handle, SB_HORZ, SF);
        // FListPos := SF.nPos;
        FScrollPos := SF.nPos;
        FPrevScrollPos := Mouse.CursorPos.X;
        FHorzScrollBarSliderState := tsThumbBtnHorzPressed;
        PaintScrollBars;
        SetCapture(Handle);
        Handled := True;
        Exit;
      end;

      if FHorzScrollBarWindow.Enabled then
      begin
        if PtInRect(FHorzScrollBarDownButtonRect, P) then
          FHorzScrollBarDownButtonState := tsArrowBtnRightPressed;
        if PtInRect(FHorzScrollBarUpButtonRect, P) then
          FHorzScrollBarUpButtonState := tsArrowBtnLeftPressed;
      end;
    end;
    FLeftMouseButtonDown := True;
    PaintScrollBars;
  end;
end;

procedure TGridViewVclStyleScrollBarsHook.WMNCLButtonUp(var Msg: TWMMouse);
var
  P: TPoint;
  B: Boolean;
begin
  P := Point(Msg.XPos, Msg.YPos);
  ScreenToClient(Handle, P);

  if HasBorder then
    if HasClientEdge then
    begin
      P.X := P.X + 2;
      P.Y := P.Y + 2;
    end
    else
    begin
      P.X := P.X + 1;
      P.Y := P.Y + 1;
    end;

  B := PointInTreeHeader(P);

  if not B then
  begin
    if FVertScrollBarWindow.Visible then
      if FVertScrollBarWindow.Enabled then
      begin
        if FVertScrollBarSliderState = tsThumbBtnVertPressed then
        begin
          FLeftMouseButtonDown := False;
          FVertScrollBarSliderState := tsThumbBtnVertNormal;
          PaintScrollBars;
          Handled := True;
          Exit;
        end;

        if PtInRect(FVertScrollBarDownButtonRect, P) then
          FVertScrollBarDownButtonState := tsArrowBtnDownHot
        else
          FVertScrollBarDownButtonState := tsArrowBtnDownNormal;

        if PtInRect(FVertScrollBarUpButtonRect, P) then
          FVertScrollBarUpButtonState := tsArrowBtnUpHot
        else
          FVertScrollBarUpButtonState := tsArrowBtnUpNormal;
      end;

    if FHorzScrollBarWindow.Visible then
      if FHorzScrollBarWindow.Enabled then
      begin
        if FHorzScrollBarSliderState = tsThumbBtnHorzPressed then
        begin
          FLeftMouseButtonDown := False;
          FHorzScrollBarSliderState := tsThumbBtnHorzNormal;
          PaintScrollBars;
          Handled := True;
          Exit;
        end;

        if PtInRect(FHorzScrollBarDownButtonRect, P) then
          FHorzScrollBarDownButtonState := tsArrowBtnRightHot
        else
          FHorzScrollBarDownButtonState := tsArrowBtnRightNormal;

        if PtInRect(FHorzScrollBarUpButtonRect, P) then
          FHorzScrollBarUpButtonState := tsArrowBtnLeftHot
        else
          FHorzScrollBarUpButtonState := tsArrowBtnLeftNormal;
      end;

  end;
  CallDefaultProc(TMessage(Msg));
  if not B and (FHorzScrollBarWindow.Visible) or (FVertScrollBarWindow.Visible) then
    PaintScrollBars;
  Handled := True;
end;

procedure TGridViewVclStyleScrollBarsHook.WMNCMouseMove(var Msg: TWMMouse);
var
  P: TPoint;
  MustUpdateScroll: Boolean;
  B: Boolean;
begin
  inherited;
  P := Point(Msg.XPos, Msg.YPos);
  ScreenToClient(Handle, P);

  if PointInTreeHeader(P) then
  begin
    CallDefaultProc(TMessage(Msg));
    PaintScrollBars;
    Handled := True;
    Exit;
  end;

  if HasBorder then
    if HasClientEdge then
    begin
      P.X := P.X + 2;
      P.Y := P.Y + 2;
    end
    else
    begin
      P.X := P.X + 1;
      P.Y := P.Y + 1;
    end;

  MustUpdateScroll := False;
  if FVertScrollBarWindow.Enabled then
  begin
    B := PtInRect(GetVertScrollBarSliderRect, P);
    if B and (FVertScrollBarSliderState = tsThumbBtnVertNormal) then
    begin
      FVertScrollBarSliderState := tsThumbBtnVertHot;
      MustUpdateScroll := True;
    end
    else if not B and (FVertScrollBarSliderState = tsThumbBtnVertHot) then
    begin
      FVertScrollBarSliderState := tsThumbBtnVertNormal;
      MustUpdateScroll := True;
    end;

    B := PtInRect(FVertScrollBarDownButtonRect, P);
    if B and (FVertScrollBarDownButtonState = tsArrowBtnDownNormal) then
    begin
      FVertScrollBarDownButtonState := tsArrowBtnDownHot;
      MustUpdateScroll := True;
    end
    else if not B and (FVertScrollBarDownButtonState = tsArrowBtnDownHot) then
    begin
      FVertScrollBarDownButtonState := tsArrowBtnDownNormal;
      MustUpdateScroll := True;
    end;
    B := PtInRect(FVertScrollBarUpButtonRect, P);
    if B and (FVertScrollBarUpButtonState = tsArrowBtnUpNormal) then
    begin
      FVertScrollBarUpButtonState := tsArrowBtnUpHot;
      MustUpdateScroll := True;
    end
    else if not B and (FVertScrollBarUpButtonState = tsArrowBtnUpHot) then
    begin
      FVertScrollBarUpButtonState := tsArrowBtnUpNormal;
      MustUpdateScroll := True;
    end;
  end;

  if FHorzScrollBarWindow.Enabled then
  begin
    B := PtInRect(GetHorzScrollBarSliderRect, P);
    if B and (FHorzScrollBarSliderState = tsThumbBtnHorzNormal) then
    begin
      FHorzScrollBarSliderState := tsThumbBtnHorzHot;
      MustUpdateScroll := True;
    end
    else if not B and (FHorzScrollBarSliderState = tsThumbBtnHorzHot) then
    begin
      FHorzScrollBarSliderState := tsThumbBtnHorzNormal;
      MustUpdateScroll := True;
    end;

    B := PtInRect(FHorzScrollBarDownButtonRect, P);
    if B and (FHorzScrollBarDownButtonState = tsArrowBtnRightNormal) then
    begin
      FHorzScrollBarDownButtonState := tsArrowBtnRightHot;
      MustUpdateScroll := True;
    end
    else if not B and (FHorzScrollBarDownButtonState = tsArrowBtnRightHot) then
    begin
      FHorzScrollBarDownButtonState := tsArrowBtnRightNormal;
      MustUpdateScroll := True;
    end;

    B := PtInRect(FHorzScrollBarUpButtonRect, P);
    if B and (FHorzScrollBarUpButtonState = tsArrowBtnLeftNormal) then
    begin
      FHorzScrollBarUpButtonState := tsArrowBtnLeftHot;
      MustUpdateScroll := True;
    end
    else if not B and (FHorzScrollBarUpButtonState = tsArrowBtnLeftHot) then
    begin
      FHorzScrollBarUpButtonState := tsArrowBtnLeftNormal;
      MustUpdateScroll := True;
    end;
  end;

  if MustUpdateScroll then
    PaintScrollBars;
end;

procedure TGridViewVclStyleScrollBarsHook.WMNCPaint(var Msg: TMessage);
begin
  CalcScrollBarsRect;
  UpdateScrollBarWindow;
//PaintScrollBars;
// Handled := True;
end;

procedure TGridViewVclStyleScrollBarsHook.WMSize(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  CalcScrollBarsRect;
  UpdateScrollBarWindow;
  PaintScrollBars;
  Handled := True;
end;

procedure TGridViewVclStyleScrollBarsHook.WMVScroll(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScrollBars;
  Handled := True;
end;

{ TVclStyleScrollBarsHook.TVclStyleScrollBarWindow }

constructor TGridViewVclStyleScrollBarsHook.TVclStyleScrollBarWindow.Create
  (AOwner: TComponent);
begin
  inherited;
  ControlStyle          := ControlStyle + [csOverrideStylePaint];
  FScrollBarWindowOwner := nil;
  FScrollBarVertical    := False;
  FScrollBarVisible     := False;
  FScrollBarEnabled     := False;
end;

procedure TGridViewVclStyleScrollBarsHook.TVclStyleScrollBarWindow.CreateParams
  (var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or WS_CHILDWINDOW or WS_CLIPCHILDREN or
    WS_CLIPSIBLINGS;
  Params.ExStyle := Params.ExStyle or WS_EX_NOPARENTNOTIFY;
end;

procedure TGridViewVclStyleScrollBarsHook.TVclStyleScrollBarWindow.WMEraseBkgnd
  (var Msg: TMessage);
begin
  Msg.Result := 1;
end;

procedure TGridViewVclStyleScrollBarsHook.TVclStyleScrollBarWindow.WMNCHitTest
  (var Msg: TWMNCHitTest);
begin
  Msg.Result := HTTRANSPARENT;
end;

procedure TGridViewVclStyleScrollBarsHook.TVclStyleScrollBarWindow.WMPaint
  (var Msg: TWMPaint);
var
  PS: TPaintStruct;
  DC: HDC;
begin
  BeginPaint(Handle, PS);
  try
    if FScrollBarWindowOwner <> nil then
    begin
      DC := GetWindowDC(Handle);
      try
        if FScrollBarVertical then
        begin
          MoveWindowOrg(DC, -FScrollBarWindowOwner.FVertScrollBarRect.Left,
            -FScrollBarWindowOwner.FVertScrollBarRect.Top);
          FScrollBarWindowOwner.DrawVertScrollBar(DC);
        end
        else
        begin
          MoveWindowOrg(DC, -FScrollBarWindowOwner.FHorzScrollBarRect.Left,
            -FScrollBarWindowOwner.FHorzScrollBarRect.Top);
          FScrollBarWindowOwner.DrawHorzScrollBar(DC);
        end;
      finally
        ReleaseDC(Handle, DC);
      end;
    end;
  finally
    EndPaint(Handle, PS);
  end;
end;
{$IFEND}
{$ENDREGION}

{$IF COMPILERVERSION <= 21}
{$REGION 'StyleServices'}
class procedure StyleServices.DrawElement(DC: HDC;
  Details: TThemedElementDetails; const R: TRect);
begin
  ThemeServices.DrawElement(DC, Details, R);
end;

class function StyleServices.GetElementDetails(
  Detail: TThemedHeader): TThemedElementDetails;
begin
  Result := ThemeServices.GetElementDetails(Detail);
end;

class function StyleServices.GetElementDetails(Detail: TThemedComboBox):
  TThemedElementDetails;
begin
  Result := ThemeServices.GetElementDetails(Detail);
end;

class function StyleServices.GetElementDetails(Detail: TThemedButton):
  TThemedElementDetails;
begin
  Result := ThemeServices.GetElementDetails(Detail);
end;

class function StyleServices.GetEnabled: Boolean;
begin
  Result := ThemeServices.ThemesEnabled;
end;
{$IFEND}
{$ENDREGION}

end.
