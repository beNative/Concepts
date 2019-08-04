{
  Copyright (C) 2013-2019 Tim Sinaeve tim.sinaeve@gmail.com

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
  The Initial Developer of the Original Code (Ex_DBGrid.pas) is Roman M.
  Mochalov (roman@tersy.ru). Portions created by the Initial Developer are
  Copyright (C) 1997-2007. All Rights Reserved. You may obtain a copy of the
  original code at http://www.tersy.ru/~roman/download/

  Changes by Tim Sinaeve
     - The original comments were translated from Russian to English using
       machine translation.
     - If the column does not have a fieldname associated, then don't make the
        column ReadOnly (changed <IsReadOnlyField>). This enables us to use
        such columns as editable non-databound columns.
      - [fkInternalCalc, fkAggregate] added to readonly fields in function
         <IsReadOnlyField>
      - Added new event OnDataLayoutChanged. This is a very useful event when
        the dataset columns need to be resized automatically after a dataset
        refresh.
     - bugfix : take <Visible> property of the DataSet's Field into account in
       <RestoreDefaults>
     - bugfix : always call <SetCursor(CellFocused, True, True);> after moving
       the cursor in the datalink in <KeyDown>
     - changed default CursorKeys
     - for string fields (ftString, ftWideString) the property WantReturns is
       True by default.
     - published event property OnEditCanShow added to TDBGridView
     - Bugfixes in DoMouseWheelUp and DoMouseWheelDown : the Handled parameter
       was not taken into account.
     - Code ported to support unicode and later versions of Delphi.

     * Multiselection support
       - IsRowMultiSelected
       - SelectAll/DeselectAll
       - TBookmarkList (-> DBGrids) => modified to support user defined bookmarks
       - override IsCellHighlighted
       - added FSelecting field (set when multiselection is in progress)
       - added ClearSelection method
       - SPACE can be used to toggle the selection of the active row when
         MultiSelect and RowSelect is True.

  KNOWN ISSUES:
     - AutoSizeCols(True, False) should size the columns for all records in the
       DataSet. For the moment only the visible rows are considered because
       Rows.Count equals FDataLink.RecordCount (= recordcount in the record
       buffer). The real recordcount is FDataLink.DataSet.RecordCount but we
       can't assign this to Rows.Count because that would increase the record
       buffer too.
     - FindText works only to search all VISIBLE cells.
     - the Row value seems to be the offset from the first VISIBLE row in the
       grid. Maybe this holds true for TGridView as well.
     - when posting the value of a field in the last column, a carriage return
       is inserted in the field value before the value is posted. Needs further
       investigation.

  TODO:
    - Add component editor to automatically create persistent columns for the
      connected fields.
    - CTRL-A during MultiSelect selects all records
}

{$I DDuce.inc}

unit DDuce.Components.DBGridView;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Math,
  Winapi.Windows, Winapi.Messages, Winapi.CommCtrl,
  Vcl.Controls, Vcl.Graphics, Vcl.Forms, Vcl.DBCtrls, Vcl.StdCtrls,
  Vcl.ImgList,
  Data.DB,

  DDuce.Components.GridView;

type
  TCustomDBGridView = class;

{ TDBGridHeader }

  TDBGridHeader = class(TCustomGridHeader)
  public
    constructor Create(AGrid: TCustomGridView); override;

  published
    property AutoHeight;
    property AutoSynchronize;
    property Color;
    property Images;
    property Flat default False;
    property Font;
    property FullSynchronizing default False;
    property GridColor;
    property GridFont;
    property Sections;
    property SectionHeight;
    property Synchronized;
  end;

{ TDBGridColumn }

  {
    Column of table with the additional property: by reference in the field of
    the source of data. Knows how to automatically determine name, levelling off,
    mask and type of the line of editing, the maximum length of line, width and
    the sign of editing from the field indicated.

    Methods:

    RestoreDefaults - To restore the values of column on silence.

    Properties:
    DefaultColumn - sign of use by a column of the parameters of the field of
                    the source of data connected with it.
    Field - reference to the field of the source of data, mapped into this column.
    FieldName - name of the field of column.
  }

  TDBGridColumn = class(TCustomGridColumn)
  private
    FField         : TField;
    FFieldName     : string;
    FDefaultColumn : Boolean;

    function GetGrid: TCustomDBGridView;
    function IsNondefaultColumn: Boolean;
    function GetField: TField;
    procedure SetDefaultColumn(Value: Boolean);
    procedure SetField(Value: TField);
    procedure SetFieldName(const Value: string);

  protected
    function GetDisplayName: string; override;
    procedure SetAlignment(Value: TAlignment); override;
    procedure SetCaption(const Value: string); override;
    procedure SetEditMask(const Value: string); override;
    procedure SetEditStyle(Value: TGridEditStyle); override;
    procedure SetMaxLength(Value: Integer); override;
    procedure SetReadOnly(Value: Boolean); override;
    procedure SetWidth(Value: Integer); override;

  public
    constructor Create(Collection: TCollection); override;

    procedure RestoreDefaults; virtual;

    property Field: TField
      read GetField write SetField;

    property Grid: TCustomDBGridView
      read GetGrid;

  published
    property AlignEdit;
    property Alignment stored IsNondefaultColumn;
    property AllowClick;
    property AllowEdit;
    property Caption stored IsNondefaultColumn;
    property CheckKind;
    property CheckAlignment;
    property EditAlignment;
    property EditMask stored IsNondefaultColumn;
    property EditStyle stored IsNondefaultColumn;
    property EditWordWrap;
    property FixedSize;
    property MaxLength stored IsNondefaultColumn;
    property MaxWidth;
    property MinWidth;
    property PickList;
    property ReadOnly stored IsNondefaultColumn;
    property TabStop;
    property Tag;
    property Visible;
    property WantReturns;
    property Width;
    property WordWrap;
    property DefWidth stored IsNondefaultColumn;
    property FieldName: string
      read FFieldName write SetFieldName stored True;
    property DefaultColumn: Boolean
      read FDefaultColumn write SetDefaultColumn default True;
  end;

{ TDBGridColumn }

  TDBGridColumns = class(TGridColumns)
  private
    function GetColumn(Index: Integer): TDBGridColumn;
    function GetGrid: TCustomDBGridView;
    procedure SetColumn(Index: Integer; Value: TDBGridColumn);

  protected
    procedure Update(Item: TCollectionItem); override;

  public
    function Find(AFieldName: string): TDBGridColumn;
    function Add: TDBGridColumn;

    property Columns[Index: Integer]: TDBGridColumn
      read GetColumn write SetColumn; default;
    property Grid: TCustomDBGridView
      read GetGrid;
  end;

{ TDBGridRows }

  {
    Table rows. Are differed from the lines of the standard TGridView only
    in terms of the fact that they do not have published property Count, since
    a quantity of lines is established by table automatically and does not be
    subject to change by hand.
  }

  TDBGridRows = class(TCustomGridRows)
  private
    FRowsFromGrid: Integer;

    function GetGrid: TCustomDBGridView;

  protected
    procedure Change; override;
    procedure SetCount(Value: Integer); override;

  public
    property Grid: TCustomDBGridView
      read GetGrid;

  published
    property AutoHeight;
    property Height;
    property OnChange;
  end;

{ TDBGridFixed }

  {
    Parameters of the fixed columns of table. They have additionally a property
    DefCount - a quantity of fixed columns, since property Count depends on a
    quantity of columns and it can be discarded in 0 with the automatic creation
    of columns by table on the fields of the source of data.
  }

  TDBGridFixed = class(TCustomGridFixed)
  private
    FDefCount : Integer;

    function GetGrid: TCustomDBGridView;
    procedure SetDefCount(Value: Integer);

  public
    constructor Create(AGrid: TCustomGridView); override;
    property Grid: TCustomDBGridView read GetGrid;

  published
    property Color;
    property Count: Integer
      read FDefCount write SetDefCount default 0;
    property Flat default False;
    property Font;
    property GridColor;
    property GridFont;
    property ShowDivider;
  end;

{ TDBGridScrollBar }

  {
     Scrollbar for the grid. It has a position always 0 in order not to
     skrollirovat' line in TGridView. Itself worries about the installation of
     the position of the cursor of strollera in the dependence on the current
     record of source. It catches the events of scroller and it displaces the
     current record of the source of data.
  }

  TDBGridScrollBar = class(TGridScrollBar)
  private
    FRowMin : Integer;
    FRowMax : Integer;

    function GetGrid: TCustomDBGridView;

  protected
    procedure ScrollMessage(var Message: TWMScroll); override;
    procedure SetParams(AMin, AMax, APageStep, ALineStep: Integer); override;
    procedure SetPositionEx(Value: Integer; ScrollCode: Integer); override;
    procedure Update; override;

  public
    property Grid: TCustomDBGridView
      read GetGrid;

  end;

{ TDBGridEdit }

  {
   Line of the editing of table. Knows how to show the falling out list for
   Lookup pour on. It removes the buttons of list or "...", if the field of
   column cannot be edited (ReadOnly, computed, BLOB field, etc.).
  }

  TDBGridListBox = class(TPopupDataList)
  private
    FLookupSource : TDataSource;

  public
    constructor Create(AOwner: TComponent); override;

    property LookupSource: TDataSource
      read FLookupSource;
  end;

  TDBGridEdit = class(TCustomGridEdit)
  private
    FDataList : TDBGridListBox;

    function GetGrid: TCustomDBGridView;

  protected
    function GetDropList: TWinControl; override;
    procedure UpdateList; override;
    procedure UpdateListItems; override;
    procedure UpdateListValue(Accept: Boolean); override;
    procedure UpdateStyle; override;

  public
    property Grid: TCustomDBGridView
      read GetGrid;
  end;

{ TDBGridDataLink }

  {
     Links the grid with the datasource.
     An object of this class is used by TDBGridView to coordinate the actions
     of TDataSource and TDataSet, and to respond to data events.
  }

  TDBGridDataLink = class(TDataLink)
  private
    FGrid         : TCustomDBGridView;
    FModified     : Boolean;
    FInUpdateData : Integer;

  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure EditingChanged; override;
    procedure FocusControl(Field: TFieldRef); override;
    function GetActiveRecord: Integer; override;
    procedure LayoutChanged; override;
    function MoveBy(Distance: Integer): Integer; override;
    procedure RecordChanged(Field: TField); override;
    procedure SetActiveRecord(Value: Integer); override;
    procedure UpdateData; override;
    property Grid: TCustomDBGridView read FGrid;
    property InUpdateData: Integer read FInUpdateData;

  public
    constructor Create(AGrid: TCustomDBGridView);
    procedure Modified;
    procedure Reset;

  end;

{ TCustomDBGridView }

{
   Table for the representation of the contained source of data indicated.
   Realizes the possibilities, analogous DBGrid: mapping data from that indicated
   DataSet, automatic rasladka of columns on the fields of source, the editing
   of records, the falling out list of zacheniy for lookup pour on, indicator,
   insert and the removal of records.

    Methods:

    ChangeEditText - to change the instantaneous value of the edited field with
                     the renovation of the line of editing. The possibility of
                     the editing of field is checked before the installation
                     of new value. It is used, when field value is required
                     to change from without (for example, after pushing of
                     knob... (with the dots)).

    Properties:

    AllowInsertRecord - is it possible to put new records in the table with the
                        pressure of key INSERT or on reaching of the end of the
                        table.
    AllowDeleteRecord - is it possible to move away records from the table with
                        the pressure of key DELETE; DataSource - bond with the
                        source of data.
    DefaultLayout - Columns are automatically created for each corresponding
                    field in the dataset.
    EditColumn - current edited column. It sootvestvuyet to the column, in which
                is located the line of introduction. EditField - current edited
                field of source. It sootvestvuyet to the field of the column of
                the current edited cell.
    IndicatorImages - reference to the list of the pictures of indicator.
                   On silence sotvetstviye between the state of indicator
                   and the index of picture is the following:
                    -1 - no indicator
                     0 - current record
                     1 - edit mode
                     2 - new record
                     3 - multiselect
                     4 - reserved
    IndicatorWidth - width of the column of indicator.
    SelectedField - current chosen field of source. It sootvestvuyet to the
                   field of the current column of the chosen cell.
    ShowIndicator - sign of mapping indicator. Events: All inherited events are
                    analogous to events TGridView with one exception: the value
                    of the line of cell, indicated in the event (i.e. Cell.Row),
                    always varies from 0 to a quantity of visible columns.
                    For obtaining the field (field value) of the source of data,
                    sootvestvuyushchego to the cell indicated, it is necessary
                    to use property Columns[.Cell.Col]..Field.

    OnDataDeleteRecord - demand to the removal of the record of the record of
                         source with the pressure of key Delete.
    OnDataEditError - processor of the error of the beginning of the editing
                      of the record of source.
    OnDataInsertRecord - demand to the insert of record into the source with
                        the pressure of key Insert.
    OnDataUpdateError - processor of the error of the renovation of field
                        value of source with the completion of editing.
    OnDataUpdateField - event to a change in the value of the current field.
                        It is caused after record in the field of new value from
                        the line of introduction (falling out list).
    OnDataLayoutChanged - Event that is called when the dataset layout changes.
                          If columns need to be resized automatically after
                          a refresh you need to handle this event.
    OnDataActiveChanged - Event that is called on changes in the Active propety
                          of the DataLink (and thus the corresponding DataSet).
    OnGetIndicatorImage - Obtains the index of the indicatorimage in the
                          imagelist.

  }

  TDBGridDataAction = (gdaFail, gdaAbort);

  TDBGridDataErrorEvent = procedure(Sender: TObject; E: Exception;
    var Action: TDBGridDataAction) of object;
  TDBGridDataInsertEvent = procedure(Sender: TObject; var AllowInsert: Boolean)
    of object;
  TDBGridDataDeleteEvent = procedure(Sender: TObject; var AllowDelete: Boolean)
    of object;
  TDBGridDataUpdateEvent = procedure(Sender: TObject; Field: TField) of object;
  TDBGridIndicatorImageEvent = procedure(Sender: TObject; DataRow: Integer;
    var ImageIndex: Integer) of object;
  TDBGridRowMultiSelectEvent = procedure(Sender: TObject; Row: Integer;
    var Select : Boolean) of object;
  TDBGridClearMultiSelectEvent = procedure(Sender: TObject) of object;

  TBookmarkList = class;

  TCustomDBGridView = class(TCustomGridView)
  private
    FDataLink            : TDBGridDataLink;
    FDefaultLayout       : Boolean;
    FShowIndicator       : Boolean;
    FIndicatorImages     : TImageList;
    FIndicatorsLink      : TChangeLink;
    FIndicatorsDef       : TImageList;
    FIndicatorWidth      : Integer;
    FAllowInsertRecord   : Boolean;
    FAllowDeleteRecord   : Boolean;
    FLayoutLock          : Integer;
    FScrollLock          : Integer;
    FScrollCell          : TGridCell;
    FScrollSelected      : Boolean;
    FCursorFromDataSet   : Integer;
    FFieldText           : string;
    FContextPopup        : Integer;
    FOnDataEditError     : TDBGridDataErrorEvent;
    FOnDataUpdateError   : TDBGridDataErrorEvent;
    FOnDataInsertRecord  : TDBGridDataInsertEvent;
    FOnDataDeleteRecord  : TDBGridDataDeleteEvent;
    FOnDataUpdateField   : TDBGridDataUpdateEvent;
    FOnDataLayoutChanged : TNotifyEvent;
    FOnGetIndicatorImage : TDBGridIndicatorImageEvent;
    FOnRowMultiSelect    : TDBGridRowMultiSelectEvent;
    FOnClearMultiSelect  : TDBGridClearMultiSelectEvent;
    FMultiSelect         : Boolean;
    FBookmarks           : TBookmarkList;
    FOnDataActiveChanged : TNotifyEvent;
    FOnDataChanged       : TNotifyEvent;

    function GetCol: Longint;
    function GetColumns: TDBGridColumns;
    function GetDataSource: TDataSource;
    function GetEditColumn: TDBGridColumn;
    function GetEditField: TField;
    function GetFixed: TDBGridFixed;
    function GetHeader: TDBGridHeader;
    function GetRows: TDBGridRows;
    function GetSelectedField: TField;
    procedure SetCol(Value: Longint);
    procedure SetColumns(Value: TDBGridColumns);
    procedure SetDataSource(Value: TDataSource);
    procedure SetDefaultLayout(Value: Boolean);
    procedure SetFixed(Value: TDBGridFixed);
    procedure SetHeader(Value: TDBGridHeader);
    procedure SetIndicatorImages(Value: TImageList);
    procedure SetIndicatorWidth(Value: Integer);
    procedure SetRows(Value: TDBGridRows);
    procedure SetSelectedField(Value: TField);
    procedure SetShowIndicator(Value: Boolean);
    function GetMultiSelect: Boolean;
    function GetRowSelected(ARow: Integer): Boolean;
    procedure SetMultiSelect(const Value: Boolean);
    procedure SetRowSelected(ARow: Integer; const Value: Boolean);

    procedure IndicatorsChange(Sender: TObject);
    function IsColumnsStored: Boolean;
    procedure ReadColumns(Reader: TReader);
    procedure WriteColumns(Writer: TWriter);

    procedure CMExit(var Message: TMessage); message CM_EXIT;
    procedure WMContextMenu(var Message: TMessage); message WM_CONTEXTMENU;

  protected
    function AcquireLockLayout: Boolean;
    procedure ChangeIndicator; virtual;
    procedure ChangeScale(M, D: Integer); override;
    function CreateColumns: TGridColumns; override;
    function CreateFixed: TCustomGridFixed; override;
    function CreateHeader: TCustomGridHeader; override;
    function CreateRows: TCustomGridRows; override;
    function CreateScrollBar(Kind: TScrollBarKind): TGridScrollBar; override;
    procedure DataEditError(E: Exception; var Action: TDBGridDataAction); virtual;
    procedure DataFieldUpdated(Field: TField); virtual;
    procedure DataLayoutChanged; virtual;
    procedure DataLinkActivate(Active: Boolean); virtual;
    procedure DataRecordChanged(Field: TField); virtual;
    procedure DataSetChanged; virtual;
    procedure DataSetDeleteRecord; virtual;
    procedure DataSetInsertRecord(AppendMode: Boolean); virtual;
    procedure DataSetScrolled(Distance: Integer); virtual;
    procedure DataUpdateError(E: Exception;
      var Action: TDBGridDataAction); virtual;

    procedure DefineProperties(Filer: TFiler); override;
    procedure ClearSelection;

    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint)
      : Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint)
      : Boolean; override;
    procedure DoSelection(Select: Boolean; Direction: Integer); virtual;
    procedure DoRowMultiSelect(var Select: Boolean); dynamic;
    function EditCanAcceptKey(Cell: TGridCell; Key: Char): Boolean; override;
    function EditCanModify(Cell: TGridCell): Boolean; override;
    function EditCanShow(Cell: TGridCell): Boolean; override;
    procedure GetCellColors(Cell: TGridCell; Canvas: TCanvas); override;
    function GetCellText(Cell: TGridCell): string; override;
    function GetColumnClass: TGridColumnClass; override;
    function GetEditClass(Cell: TGridCell): TGridEditClass; override;
    function GetEditText(Cell: TGridCell): string; override;
    procedure HideCursor; override;
    procedure InvalidateIndicator;
    procedure InvalidateIndicatorImage(DataRow: Integer);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MoveRow(Distance: Integer; Select : Boolean = False); virtual;
    procedure MoveRowByMouse(Shift : TShiftState;
      X, Y : Integer;
      Select : Boolean = False;
      SelectRange : Boolean = False); virtual;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Paint; override;
    procedure PaintCell(Cell: TGridCell; Rect: TRect); override;
    procedure PaintIndicatorFixed; virtual;
    procedure PaintIndicatorGrid; virtual;
    procedure PaintIndicatorHeader; virtual;
    procedure PaintIndicatorImage(Rect: TRect; DataRow: Integer); virtual;
    procedure SetEditText(Cell: TGridCell; var Value: string); override;
    procedure SetRow(Value: Longint); override;
    procedure Resize; override;
    procedure UpdateData; virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CancelEdit; override;
    procedure ChangeEditText(const S: string); virtual;

    function IsCellReadOnly(Cell: TGridCell): Boolean; override;
    function IsRowMultiSelected: Boolean; overload;
    function IsRowMultiSelected(ARow : Integer): Boolean; overload;
    function IsFixedVisible: Boolean;

    procedure SelectAll; virtual;
    procedure DeselectAll; virtual;
    function GetClientRect: TRect; override;
    function GetIndicatorHeaderRect: TRect; virtual;
    function GetIndicatorFixedRect: TRect; virtual;
    function GetIndicatorImage(DataRow: Integer): Integer; virtual;
    function GetIndicatorImageRect(DataRow: Integer): TRect; virtual;
    function GetIndicatorRect: TRect; virtual;
    procedure LockLayout;
    procedure LockScroll;
    procedure MakeCellVisible(Cell: TGridCell; PartialOK: Boolean); override;
    procedure SetCursor(Cell: TGridCell; Selected, Visible: Boolean); override;
    procedure UndoEdit; override;
    procedure UnLockLayout(CancelChanges: Boolean);
    procedure UnLockScroll(CancelScroll: Boolean);
    procedure UpdateCursorPos; virtual;
    procedure UpdateLayout; virtual;
    procedure UpdateRowCount; virtual;
    procedure UpdateSelection(var Cell: TGridCell;
      var Selected: Boolean); override;
    procedure DefaultDrawCell(Cell: TGridCell; Rect: TRect); override;
    procedure AutoSizeCols(AIncludeTitles: Boolean = True;
      OnlyVisibleRows: Boolean = True); override;

    property AllowDeleteRecord: Boolean
      read FAllowDeleteRecord write FAllowDeleteRecord default True;

    property AllowEdit
      default True;

    property AllowInsertRecord: Boolean
      read FAllowInsertRecord write FAllowInsertRecord default True;

    property Col: Longint
      read GetCol write SetCol;

    property ColumnClick
      default False;

    property Columns: TDBGridColumns
      read GetColumns write SetColumns stored IsColumnsStored;

    property CursorKeys default [gkArrows, gkMouse, gkTabs, gkMouseWheel];

    property CursorLock: Integer
      read FScrollLock;

    property DataLink: TDBGridDataLink
      read FDataLink;

    property DataSource: TDataSource
      read GetDataSource write SetDataSource;

    property DefaultLayout: Boolean
      read FDefaultLayout write SetDefaultLayout default True;

    property Header: TDBGridHeader
      read GetHeader write SetHeader;

    property Fixed: TDBGridFixed
      read GetFixed write SetFixed;

    property LayoutLock: Integer
      read FLayoutLock;

    property EditColumn: TDBGridColumn
      read GetEditColumn;

    property EditField: TField
      read GetEditField;

    property IndicatorImages: TImageList
      read FIndicatorImages write SetIndicatorImages;

    property IndicatorWidth: Integer
      read FIndicatorWidth write SetIndicatorWidth default 12;

    property Rows: TDBGridRows
      read GetRows write SetRows;

    property SelectedField: TField read
      GetSelectedField write SetSelectedField;

    property ShowIndicator: Boolean
      read FShowIndicator write SetShowIndicator default True;

    property SelectedRows: TBookmarkList
      read FBookmarks;

    property MultiSelect: Boolean
      read GetMultiSelect write SetMultiSelect;

    { Alters the selection status for a given row when goMultiselect is enabled
      in Options. When MultiSelect is not enabled RowSelected will ALWAYS return
      False. }
    property RowSelected[ARow: Integer]: Boolean
      read GetRowSelected write SetRowSelected;

    // TDataLink related events
    property OnDataActiveChanged: TNotifyEvent
      read FOnDataActiveChanged write FOnDataActiveChanged;

    property OnDataChanged: TNotifyEvent
      read FOnDataChanged write FOnDataChanged;

    property OnDataDeleteRecord: TDBGridDataDeleteEvent
      read FOnDataDeleteRecord write FOnDataDeleteRecord;

    property OnDataInsertRecord: TDBGridDataInsertEvent
      read FOnDataInsertRecord write FOnDataInsertRecord;

    property OnDataEditError: TDBGridDataErrorEvent
      read FOnDataEditError write FOnDataEditError;

    property OnDataUpdateError: TDBGridDataErrorEvent
      read FOnDataUpdateError write FOnDataUpdateError;

    property OnDataUpdateField: TDBGridDataUpdateEvent
      read FOnDataUpdateField write FOnDataUpdateField;

    property OnDataLayoutChanged: TNotifyEvent
      read FOnDataLayoutChanged write FOnDataLayoutChanged;

    property OnGetIndicatorImage: TDBGridIndicatorImageEvent
      read FOnGetIndicatorImage write FOnGetIndicatorImage;

    // MultiSelect support
    property OnRowMultiSelect: TDBGridRowMultiSelectEvent
      read FOnRowMultiSelect write FOnRowMultiSelect;

    property OnClearMultiSelect: TDBGridClearMultiSelectEvent
      read FOnClearMultiSelect write FOnClearMultiSelect;
  end;

  TDBGridView = class(TCustomDBGridView)
  published
    property Align;
    property AllowDeleteRecord;
    property AllowEdit;
    property AllowInsertRecord;
    property AllowSelect;
    property AlwaysEdit;
    property AlwaysSelected;
    property Anchors;
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
    property DataSource;
    property DefaultEditMenu;
    property DefaultLayout;
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
    property GridLines;
    property GridStyle;
    property Header;
    property HideSelection;
    property Hint;
    property HorzScrollBar;
    property ImageIndexDef;
    property ImageHighlight;
    property Images;
    property IndicatorImages;
    property IndicatorWidth;
    property MultiSelect;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RightClickSelect;
    property Rows;
    property RowSelect;
    property ShowCellTips;
    property ShowIndicator;
    property ShowFocusRect;
    property ShowHeader;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property ThemingEnabled;
    property VertScrollBar;
    property Visible;

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
    property OnClearMultiSelect;
    property OnClick;
    property OnColumnAutoSize;
    property OnColumnResize;
    property OnColumnResizing;
    property OnDataActiveChanged;
    property OnDataChanged;
    property OnDataDeleteRecord;
    property OnDataEditError;
    property OnDataInsertRecord;
    property OnDataLayoutChanged;
    property OnDataUpdateError;
    property OnDataUpdateField;
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
    property OnGetIndicatorImage;
    property OnGetSortDirection;
    property OnGetSortImage;
    property OnGetTipsRect;
    property OnGetTipsText;
    property OnHeaderClick;
    property OnHeaderClicking;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnRowMultiSelect;
    property OnSetEditText;
    property OnStartDrag;

  end;

  TBookmarkList = class
  private
    FList       : TList<TBookmark>;
    FGrid       : TCustomDBGridView;
    FCache      : TBookmark;
    FCacheIndex : Integer;
    FCacheFind  : Boolean;
    FLinkActive : Boolean;

    function GetCount: Integer;
    function GetCurrentRowSelected: Boolean;
    function GetItem(Index: Integer): TBookmark;
    procedure SetCurrentRowSelected(Value: Boolean);

    procedure BookmarksChanged(
      Sender     : TObject;
      const Item : TBookmark;
      Action     : TCollectionNotification
    );

  protected
    function CurrentRow: TBookmark;
    function Compare(const Item1, Item2: TBookmark): Integer;
    procedure LinkActive(Value: Boolean);

  public
    constructor Create(AGrid: TCustomDBGridView);
    destructor Destroy; override;


    procedure Clear; // free all bookmarks
    function Find(const Item: TBookmark; var Index: Integer): Boolean;
    function IndexOf(const Item: TBookmark): Integer;
    function Refresh: Boolean; // drop orphaned bookmarks; True = orphans found

    property Count: Integer
      read GetCount;

    property CurrentRowSelected: Boolean
      read GetCurrentRowSelected write SetCurrentRowSelected;

    property Items[Index: Integer]: TBookmark
      read GetItem; default;

    property Bookmarks : TList<TBookmark>
      read FList write FList;
  end;

implementation

uses
  System.Types,
  Vcl.Themes,
  Data.DBConsts;

{$R *.RES}

type
  PIntArray = ^TIntArray;
  TIntArray = array[0..MaxInt div 16 - 1] of Integer;

function IsLookupField(Field: TField): Boolean;
var
  MasterField: TField;
begin
  Result := False;
  if (Field <> nil) and (Field.FieldKind = fkLookup) and
    (Field.DataSet <> nil) then
  begin
    MasterField := Field.DataSet.FieldByName(Field.KeyFields);
    if (MasterField <> nil) and MasterField.CanModify then
      Result := True;
  end
end;

function IsReadOnlyField(Field: TField): Boolean;
const
  fkReadOnly = [fkLookup, fkCalculated, fkInternalCalc, fkAggregate];
begin
  Result := (Field = nil) or Field.ReadOnly or (Field.FieldKind in fkReadOnly) or
    ((Field.DataType in ftNonTextTypes) and (not Assigned(Field.OnSetText)));
end;

{ TDBGridHeader }

constructor TDBGridHeader.Create(AGrid: TCustomGridView);
begin
  inherited;
  Flat := False;
  FullSynchronizing := False;
end;

{ TDBGridColumn }

constructor TDBGridColumn.Create(Collection: TCollection);
begin
  inherited;
  FDefaultColumn := True;
end;

function TDBGridColumn.IsNondefaultColumn: Boolean;
begin
  Result := not DefaultColumn;
end;

function TDBGridColumn.GetField: TField;
var
  DS : TDataSet;
begin
  if (FField = nil) and (Length(FFieldName) > 0) then
  begin
    if Assigned(Grid) and Assigned(Grid.DataLink.DataSet) then
    begin
      DS := Grid.DataLink.DataSet;
      {$IFDEF DELPHIXE6_UP}
      if DS.Active or not (lcAutomatic in DS.Fields.LifeCycles) then
      {$ELSE}
      if DS.Active or not DS.DefaultFields then
      {$ENDIF}
      begin
        SetField(DS.FindField(FFieldName));
      end;
    end;
  end;
  Result := FField;
end;

function TDBGridColumn.GetGrid: TCustomDBGridView;
begin
  Result := TCustomDBGridView(inherited Grid);
end;

procedure TDBGridColumn.SetDefaultColumn(Value: Boolean);
begin
  if FDefaultColumn <> Value then
  begin
    if Value then
      RestoreDefaults;
    FDefaultColumn := Value;
  end;
end;

procedure TDBGridColumn.SetField(Value: TField);
begin
  if FField <> Value then
  begin
    if (FField <> nil) and (Grid <> nil) then
      FField.RemoveFreeNotification(Grid);
    FField := Value;
    if FField <> nil then
    begin
      if Grid <> nil then
        FField.FreeNotification(Grid);
      FFieldName := FField.FullName;
    end;
  end;
end;

procedure TDBGridColumn.SetFieldName(const Value: string);
var
  F: TField;
begin
  F := nil;
  if Length(Value) > 0 then
    if Assigned(Grid) and (not (csLoading in Grid.ComponentState)) then
      if Assigned(Grid.DataLink.DataSet) then
        F := Grid.DataLink.DataSet.FindField(Value);
  FFieldName := Value;
  SetField(F);
  if FDefaultColumn then
  begin
    RestoreDefaults;
    FDefaultColumn := True;
  end;
  Changed(False);
end;

function TDBGridColumn.GetDisplayName: string;
begin
  Result := FFieldName;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TDBGridColumn.SetAlignment(Value: TAlignment);
begin
  FDefaultColumn := False;
  inherited;
end;

procedure TDBGridColumn.SetCaption(const Value: string);
begin
  FDefaultColumn := False;
  inherited;
end;

procedure TDBGridColumn.SetEditMask(const Value: string);
begin
  FDefaultColumn := False;
  inherited;
end;

procedure TDBGridColumn.SetEditStyle(Value: TGridEditStyle);
begin
  FDefaultColumn := False;
  inherited;
end;

procedure TDBGridColumn.SetMaxLength(Value: Integer);
begin
  FDefaultColumn := False;
  inherited;
end;

procedure TDBGridColumn.SetReadOnly(Value: Boolean);
begin
  FDefaultColumn := False;
  inherited;
end;

procedure TDBGridColumn.SetWidth(Value: Integer);
begin
  FDefaultColumn := False;
  inherited;
end;

procedure TDBGridColumn.RestoreDefaults;
var
  R: TRect;

  function AllowLookup: Boolean;
  begin
    Result := IsLookupField(Field) and (Grid <> nil) and
      (Grid.DataLink.Active) and (not Grid.Datalink.ReadOnly);
  end;

begin
  if Field <> nil then
  begin
    Alignment := Field.Alignment;
    Caption   := Field.DisplayLabel;
    EditMask  := Field.EditMask;
    Visible   := Field.Visible;

    if AllowLookup then
      EditStyle := geDataList
    else if PickListCount > 0 then
      EditStyle := gePickList

      // !!! edited TS (was just else)
    else if EditStyle <> geEllipsis then
      EditStyle := geSimple;
    ReadOnly := IsReadOnlyField(Field);
    MaxLength := 0;

    if Field.DataType in [ftString , ftWideString] then
    begin
      MaxLength := Field.Size;
      WantReturns := True;
    end;

    if Grid <> nil then
    begin
      Grid.GetCellColors(GridCell(Self.Index, 0), Grid.Canvas);
      Width := GetFontWidth(Grid.Canvas.Font, Field.DisplayWidth);
      with Grid do
        R := GetTextRect(Canvas, Rect(0, 0, 0, 0), TextLeftIndent, TextTopIndent,
          Self.Alignment, False, False, Self.Caption);
      Width := MaxIntValue([DefWidth, R.Right - R.Left]);
    end;
  end
end;

{ TDBGridColumns }

function TDBGridColumns.GetColumn(Index: Integer): TDBGridColumn;
begin
  Result := TDBGridColumn(inherited GetItem(Index));
end;

function TDBGridColumns.GetGrid: TCustomDBGridView;
begin
  Result := TCustomDBGridView(inherited Grid);
end;

procedure TDBGridColumns.SetColumn(Index: Integer; Value: TDBGridColumn);
begin
  inherited SetItem(Index, Value);
end;

procedure TDBGridColumns.Update(Item: TCollectionItem);
begin
  { If any column property is changed, DefaultLayout is disabled which means
    that columns are not automatically in sync anymore with the dataset. This
    property by definition means that columns are not created automatically for
    every field anymore when the dataset is opened. If DefaultLayout is false,
    columns are not automatically cleared when the dataset closes. }
  if (Grid <> nil) and (Grid.LayoutLock = 0) then
    Grid.DefaultLayout := False;
  inherited Update(Item);
end;

function TDBGridColumns.Add: TDBGridColumn;
begin
  Result := TDBGridColumn(inherited Add);
end;

function TDBGridColumns.Find(AFieldName: string): TDBGridColumn;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Pred(Count) do
  begin
    if Assigned(Columns[i].Field) and
    (UpperCase(AFieldName) = UpperCase(Columns[i].Field.FieldName)) then
    begin
      Result := Columns[i];
      Exit;
    end
    else if AFieldName = Columns[i].FieldName then
    begin
      Result := Columns[i];
      Exit;
    end;
  end;
end;

{ TDBGridListBox }

constructor TDBGridListBox.Create(AOwner: TComponent);
begin
  inherited;
  FLookupSource := TDataSource.Create(Self);
end;

{ TDBGridEdit }

function TDBGridEdit.GetGrid: TCustomDBGridView;
begin
  Result := TCustomDBGridView( inherited Grid);
end;

function TDBGridEdit.GetDropList: TWinControl;
begin
  if (EditStyle = geDataList) and (Grid <> nil) and
    IsLookupField(Grid.EditField) then
  begin
    if FDataList = nil then
      FDataList := TDBGridListBox.Create(Self);
    Result := FDataList;
  end
  else
    Result := inherited GetDropList;
end;

procedure TDBGridEdit.UpdateList;
begin
  inherited;
  if (ActiveList = nil) or (not (ActiveList is TDBGridListBox)) then
    Exit;
  TDBGridListBox(ActiveList).RowCount := Self.DropDownCount;
end;

procedure TDBGridEdit.UpdateListItems;
begin
  if (ActiveList = nil) or (not (ActiveList is TDBGridListBox)) then
  begin
    inherited;
    Exit;
  end;
  if (Grid = nil) or (Grid.EditField = nil) then
    Exit;
  with Grid.EditField, TDBGridListBox(ActiveList) do
  begin
    LookupSource.DataSet := LookupDataSet;
    KeyField := LookupKeyFields;
    ListField := LookupResultField;
    ListSource := LookupSource;
    KeyValue := DataSet.FieldByName(KeyFields).Value;
  end;
end;

procedure TDBGridEdit.UpdateListValue(Accept: Boolean);
var
  ListValue: Variant;
  MasterField: TField;
begin
  if (ActiveList <> nil) and Accept and (Grid <> nil) then
  begin
    if ActiveList is TDBGridListBox then
      with TDBGridListBox(ActiveList) do
      begin
        ListValue := KeyValue;
        ListSource := nil;
        LookupSource.DataSet := nil;
        if (Grid.EditField <> nil) and (Grid.EditField.DataSet <> nil) then
          with Grid.EditField do
          begin
            MasterField := DataSet.FindField(KeyFields);
            if (MasterField <> nil) and MasterField.CanModify and
              Grid.DataLink.Edit then
              MasterField.Value := ListValue;
          end;
      end
    else if ActiveList is TGridListBox then
      if EditCanModify then
      begin
        inherited;
        Grid.DataLink.Modified;
      end;
  end
  else
    inherited;
end;

procedure TDBGridEdit.UpdateStyle;
var
  MasterField: TField;
begin
  inherited UpdateStyle;
  if (EditStyle <> geSimple) and (Grid <> nil) then
    if (not Grid.DataLink.Active) or Grid.DataLink.ReadOnly then
      EditStyle := geSimple
    else if EditStyle = geDataList then
    begin
      MasterField := nil;
      if (Grid.EditField <> nil) and (Grid.EditField.DataSet <> nil) then
        with Grid.EditField do
          MasterField := DataSet.FindField(KeyFields);
      if (MasterField = nil) or (not MasterField.CanModify) then
        EditStyle := geSimple;
    end
    else if Grid.IsCellReadOnly(Grid.EditCell) then
      EditStyle := geSimple;
end;

{ TDBGridRows }

function TDBGridRows.GetGrid: TCustomDBGridView;
begin
  Result := TCustomDBGridView(inherited Grid);
end;

procedure TDBGridRows.Change;
begin
  if Grid <> nil then
    Grid.UpdateRowCount;
  inherited;
end;

procedure TDBGridRows.SetCount(Value: Integer);
begin
  if FRowsFromGrid <> 0 then
    inherited;
end;

{ TDBGridFixed }

constructor TDBGridFixed.Create(AGrid: TCustomGridView);
begin
  inherited;
  Flat := False;
end;

function TDBGridFixed.GetGrid: TCustomDBGridView;
begin
  Result := TCustomDBGridView(inherited Grid);
end;

procedure TDBGridFixed.SetDefCount(Value: Integer);
begin
  FDefCount := Value;
  SetCount(Value);
end;

{ TDBGridScrollBar }

function TDBGridScrollBar.GetGrid: TCustomDBGridView;
begin
  Result := TCustomDBGridView(inherited Grid);
end;

procedure TDBGridScrollBar.ScrollMessage(var Message: TWMScroll);
var
  DataSet: TDataSet;
  PageStep: Integer;

  {
    REMARK : First, Last, and RecNo assignment on the dataset cause the
             DataSetUpdate method to be called in the DataLink!
             DataLink.MoveBy is used this does not occur!
  }

  procedure DoThumbPos(Pos: Integer);
  begin
    Pos := WinPosToScrollPos(Pos, FRowMin, FRowMax);
    if DataSet.IsSequenced then
    begin
      if Pos <= 1 then
        DataSet.First
      else if Pos >= DataSet.RecordCount then
        DataSet.Last
      else
        Grid.DataLink.MoveBy(Pos - DataSet.RecNo);
    end
    else
      case Pos of
        0:
          DataSet.First;
        1:
          Grid.DataLink.MoveBy(-PageStep);
        2:
          Exit;
        3:
          Grid.DataLink.MoveBy(PageStep);
        4:
          DataSet.Last;
      end;
  end;

begin
  if (Grid = nil) or (not Grid.DataLink.Active) or
    (Grid.DataLink.DataSet = nil) then
    Exit;
  DataSet := Grid.DataLink.DataSet;
  PageStep := Grid.VisSize.Row;
  case Message.ScrollCode of
    SB_LINEUP:
      Grid.DataLink.MoveBy(-1);
    SB_LINEDOWN:
      Grid.DataLink.MoveBy(1);
    SB_PAGEUP:
      Grid.DataLink.MoveBy(-PageStep);
    SB_PAGEDOWN:
      Grid.DataLink.MoveBy(PageStep);
    SB_THUMBPOSITION:
      DoThumbPos(Message.Pos);
    SB_THUMBTRACK:
      if Tracking and DataSet.IsSequenced then
        DoThumbPos(Message.Pos);
    SB_BOTTOM:
      DataSet.Last;
    SB_TOP:
      DataSet.First;
  end;
end;

procedure TDBGridScrollBar.SetParams(AMin, AMax, APageStep,
  ALineStep: Integer);
begin
  inherited SetParams(0, 0, 0, 0);
  Update;
end;

procedure TDBGridScrollBar.SetPositionEx(Value: Integer; ScrollCode: Integer);
begin
  inherited SetPositionEx(0, ScrollCode);
  Update;
end;

procedure TDBGridScrollBar.Update;
var
  NewPage, NewPos: Integer;
  DataSet: TDataSet;
  SI: TScrollInfo;
begin
  if (Grid <> nil) and (Grid.HandleAllocated) and (UpdateLock = 0) then
  begin
    NewPage := 0;
    NewPos := 0;
    DataSet := Grid.DataLink.DataSet;
    if Grid.DataLink.Active and (DataSet <> nil) then
    begin
      if DataSet.IsSequenced then
      begin
        if not(DataSet.State in [dsInactive, dsBrowse, dsEdit]) then
        begin
          SI.cbSize := SizeOf(SI);
          SI.fMask := SIF_ALL;
          GetScrollInfo(Grid.Handle, SB_VERT, SI);
          NewPos := WinPosToScrollPos(SI.nPos, FRowMin, FRowMax);
        end
        else
          NewPos := DataSet.RecNo;
        FRowMin := 1;
        NewPage := Grid.VisSize.Row;
        FRowMax := DataSet.RecordCount + NewPage; // - 1;
      end
      else
      begin
        FRowMin := 0;
        FRowMax := 4;
        NewPage := 0;
        if DataSet.BOF then
          NewPos := 0
        else if DataSet.EOF then
          NewPos := 4
        else
          NewPos := 2;
      end;
    end
    else
    begin
      FRowMin := 0;
      FRowMax := 0;
    end;
    FillChar(SI, SizeOf(SI), 0);
    SI.cbSize := SizeOf(SI);
    SI.fMask := SIF_ALL;
    SI.nMin := 0;
    SI.nMax := MaxWinPos * Ord(Visible and (FRowMax - FRowMin > NewPage));
    SI.nPage := ScrollPosToWinPos(FRowMin + NewPage, FRowMin, FRowMax);
    SI.nPos := ScrollPosToWinPos(NewPos, FRowMin, FRowMax);
    SetScrollInfo(Grid.Handle, SB_VERT, SI, True);
  end;
end;

{ TDBGridDataLink }

constructor TDBGridDataLink.Create(AGrid: TCustomDBGridView);
begin
  inherited Create;
  VisualControl := True;
  FGrid := AGrid;
end;

{ Responds to changes in the Active property of this TDataLink (and thus the
  Active property of the TDataSet it manages). }

procedure TDBGridDataLink.ActiveChanged;
begin
  Grid.DataLinkActivate(Active);
  FModified := False;
end;

{  DataSetChanged responds to changes to the contents of the dataset. Anything
   that changes the contents of the dataset, whether it is editing the data,
   inserting or deleting records, or changing the key triggers this method.
   Changes specific to the representation of the data within the data-aware
   object, such as scrolling the dataset or changing the layout of data elements
   within the object, also trigger this method. DataSetChanged simply calls
   RecordChanged. Derived classes can override this procedure to make additional
   adjustments to changes in the dataset. }

procedure TDBGridDataLink.DataSetChanged;
begin
  FGrid.DataSetChanged;
  FModified := False;
end;

procedure TDBGridDataLink.DataSetScrolled(Distance: Integer);
begin
  FGrid.DataSetScrolled(Distance);
end;

function TDBGridDataLink.GetActiveRecord: Integer;
begin
  Result := 0;
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    Result := inherited GetActiveRecord;
end;

{  The LayoutChanged method simply calls the DataSetChanged method.
   LayoutChanged is intended to provide an interface for a method that can
   respond after changes in the representation of the data by the data-aware
   object.
   An example of such a change would be changing the number or order of the
   columns in a TDBGridView. }

procedure TDBGridDataLink.LayoutChanged;
begin
  FGrid.DataLayoutChanged;
  inherited LayoutChanged;
end;

{  Responds to changes in the editing state of the data source. }

procedure TDBGridDataLink.EditingChanged;
begin
  FGrid.InvalidateIndicatorImage(FGrid.Row);
  if not Editing then
    FGrid.HideEdit;
end;

procedure TDBGridDataLink.FocusControl(Field: TFieldRef);
begin
  if Assigned(Field) and Assigned(Field^) then
  begin
    FGrid.SelectedField := Field^;
    if (FGrid.SelectedField = Field^) and FGrid.AcquireFocus then
    begin
      Field^ := nil;
      FGrid.ShowEdit;
    end;
  end;
end;

function TDBGridDataLink.MoveBy(Distance: Integer): Integer;
begin
  Result := Distance;
  if Result <> 0 then
    Result := inherited MoveBy(Distance);
end;

procedure TDBGridDataLink.RecordChanged(Field: TField);
begin
  FGrid.DataRecordChanged(Field);
  FModified := False;
end;

procedure TDBGridDataLink.SetActiveRecord(Value: Integer);
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    inherited;
end;

procedure TDBGridDataLink.UpdateData;
begin
  if FModified and (FInUpdateData = 0) then
  begin
    Inc(FInUpdateData);
    try
      FGrid.UpdateData;
    finally
      Dec(FInUpdateData);
    end;
    FModified := False;
  end;
end;

procedure TDBGridDataLink.Modified;
begin
  FModified := True;
end;

procedure TDBGridDataLink.Reset;
begin
  if FModified then
    RecordChanged(nil)
  else
    DataSet.Cancel;
end;

{ TCustomDBGridView }

constructor TCustomDBGridView.Create(AOwner: TComponent);
begin
  FDataLink                := TDBGridDataLink.Create(Self);
  FDefaultLayout           := True;
  FShowIndicator           := True;
  FBookmarks               := TBookmarkList.Create(Self);
  FIndicatorWidth          := 12;
  FAllowDeleteRecord       := True;
  FAllowInsertRecord       := True;
  FIndicatorsLink          := TChangeLink.Create;
  FIndicatorsLink.OnChange := IndicatorsChange;
  FIndicatorsDef           := TImageList.CreateSize(16, 16);
  FIndicatorsDef.BkColor   := clFuchsia;
  FIndicatorsDef.ResInstLoad(HInstance, rtBitmap, 'BM_GRIDVIEW_DB', clFuchsia);
  inherited Create(AOwner);
  AllowEdit := True;
  ColumnClick := False;
  CursorKeys := [gkArrows, gkMouse, gkTabs, gkMouseWheel];
end;

procedure TCustomDBGridView.DeselectAll;
begin
  SelectedRows.Clear;
end;

destructor TCustomDBGridView.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FIndicatorsLink);
  FreeAndNil(FIndicatorsDef);
  FreeAndNil(FDataLink);
  FreeAndNil(FBookmarks);
end;

procedure TCustomDBGridView.DoRowMultiSelect(var Select: Boolean);
begin
  if Assigned(FOnRowMultiSelect) then
    FOnRowMultiSelect(Self, Row, Select);
end;

procedure TCustomDBGridView.DoSelection(Select: Boolean; Direction: Integer);
var
  DoSelect : Boolean;
begin
  if MultiSelect and FDatalink.Active and Select then
  begin
    if Direction = 0 then
    begin
      DoSelect := not FBookmarks.CurrentRowSelected;
      DoRowMultiSelect(DoSelect);
      FBookmarks.CurrentRowSelected := DoSelect;
    end
    else
    begin
      DoSelect := True;
      DoRowMultiSelect(DoSelect);
      FBookmarks.CurrentRowSelected := DoSelect;
      while Direction > 0 do
      begin
        FDatalink.MoveBy(1);
        DoSelect := True;
        DoRowMultiSelect(DoSelect);
        FBookmarks.CurrentRowSelected := DoSelect;
        Dec(Direction);
      end;
      while Direction < 0 do
      begin
        FDatalink.MoveBy(-1);
        DoSelect := True;
        DoRowMultiSelect(DoSelect);
        FBookmarks.CurrentRowSelected := DoSelect;
        Inc(Direction);
      end;
      DoSelect := True;
      DoRowMultiSelect(DoSelect);
      FBookmarks.CurrentRowSelected := DoSelect;
    end;
  end
  else
    FDatalink.MoveBy(Direction);
end;

function TCustomDBGridView.GetCol: Longint;
begin
  Result := inherited Col;
end;

function TCustomDBGridView.GetColumns: TDBGridColumns;
begin
  Result := TDBGridColumns(inherited Columns);
end;

function TCustomDBGridView.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TCustomDBGridView.GetEditColumn: TDBGridColumn;
begin
  Result := TDBGridColumn(inherited EditColumn);
end;

function TCustomDBGridView.GetEditField: TField;
begin
  Result := nil;
  if EditColumn <> nil then
    Result := EditColumn.Field;
end;

function TCustomDBGridView.GetFixed: TDBGridFixed;
begin
  Result := TDBGridFixed(inherited Fixed);
end;

function TCustomDBGridView.GetHeader: TDBGridHeader;
begin
  Result := TDBGridHeader(inherited Header);
end;

function TCustomDBGridView.GetRows: TDBGridRows;
begin
  Result := TDBGridRows(inherited Rows);
end;

function TCustomDBGridView.GetRowSelected(ARow: Integer): Boolean;
begin
  Result := IsRowMultiSelected(ARow);
end;

function TCustomDBGridView.GetSelectedField: TField;
begin
  Result := nil;
  if (Col >= Fixed.Count) and (Col < Columns.Count) then
    Result := Columns[Col].Field;
end;

procedure TCustomDBGridView.IndicatorsChange(Sender: TObject);
begin
  if FShowIndicator then
    InvalidateIndicator;
end;

function TCustomDBGridView.IsColumnsStored: Boolean;
begin
  Result := not DefaultLayout;
end;

{ Does not work for filtered datasets }

procedure TCustomDBGridView.SelectAll;
var
  DS : TDataSet;
begin
  if MultiSelect then
  begin
    DS := DataLink.DataSet;
    DS.DisableControls;
    LockUpdate;
    DeselectAll;
    Screen.Cursor := crHourGlass;
    try
      DS.First;
      while not DS.Eof do
      begin
        DoSelection(True, 0);
        DS.Next;
      end;
    finally
      UnLockUpdate(False);
      DS.EnableControls;
      Refresh;
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TCustomDBGridView.SetCol(Value: Longint);
begin
  inherited Col := Value;
end;

procedure TCustomDBGridView.SetColumns(Value: TDBGridColumns);
begin
  Columns.Assign(Value);
end;

procedure TCustomDBGridView.SetDataSource(Value: TDataSource);
begin
  if FDataLink.DataSource <> Value then
  begin
    with FDataLink do
    begin
      DataSource := Value;
      if DataSource <> nil then
        DataSource.FreeNotification(Self);
    end;
    DataLayoutChanged;
  end;
end;

procedure TCustomDBGridView.SetDefaultLayout(Value: Boolean);
begin
  if FDefaultLayout <> Value then
  begin
    FDefaultLayout := Value;
    DataLayoutChanged;
    Invalidate;
  end;
end;

procedure TCustomDBGridView.SetFixed(Value: TDBGridFixed);
begin
  Fixed.Assign(Value);
end;

procedure TCustomDBGridView.SetHeader(Value: TDBGridHeader);
begin
  Header.Assign(Value);
end;

procedure TCustomDBGridView.SetIndicatorImages(Value: TImageList);
begin
  if FIndicatorImages <> Value then
  begin
    if Assigned(FIndicatorImages) then
      FIndicatorImages.UnRegisterChanges(FIndicatorsLink);
    FIndicatorImages := Value;
    if Assigned(FIndicatorImages) then
    begin
      FIndicatorImages.RegisterChanges(FIndicatorsLink);
      FIndicatorImages.FreeNotification(Self);
    end;
    ChangeIndicator;
  end;
end;

procedure TCustomDBGridView.SetIndicatorWidth(Value: Integer);
begin
  if FIndicatorWidth <> Value then
  begin
    FIndicatorWidth := Value;
    ChangeIndicator;
  end;
end;

procedure TCustomDBGridView.SetMultiSelect(const Value: Boolean);
begin
  if Value <> MultiSelect then
  begin
    FMultiSelect := Value;
    if not Value then
      ClearSelection;
  end;
end;

procedure TCustomDBGridView.SetRow(Value: Integer);
begin
  MoveRow(Value - Row);
end;

procedure TCustomDBGridView.SetRows(Value: TDBGridRows);
begin
  Rows.Assign(Value);
end;

procedure TCustomDBGridView.SetRowSelected(ARow: Integer; const Value: Boolean);
var
  OldActive: Integer;
begin
  if ARow >= 0 then
  begin
    OldActive := FDataLink.ActiveRecord;
    try
      FDatalink.ActiveRecord := ARow;
      DoSelection(True, 0);
    finally
      FDatalink.ActiveRecord := OldActive;
    end;
  end;
end;

procedure TCustomDBGridView.SetSelectedField(Value: TField);
var
  I: Integer;
begin
  if Value <> nil then
    for I := 0 to Columns.Count - 1 do
      if Columns[I].Field = Value then
      begin
        Col := I;
        Break;
      end;
end;

procedure TCustomDBGridView.SetShowIndicator(Value: Boolean);
begin
  if FShowIndicator <> Value then
  begin
    FShowIndicator := Value;
    ChangeIndicator;
  end;
end;

procedure TCustomDBGridView.ReadColumns(Reader: TReader);
begin
  Columns.Clear;
  Reader.ReadValue;
  Reader.ReadCollection(Columns);
end;

procedure TCustomDBGridView.WriteColumns(Writer: TWriter);
begin
  Writer.WriteCollection(Columns);
end;

procedure TCustomDBGridView.CMExit(var Message: TMessage);
begin
  if CancelOnExit and FDataLink.Active then
    try
      with FDataLink.DataSet do
        if (State = dsInsert) and (not Modified) and
          (not FDataLink.FModified) then
          Cancel
        else
          FDataLink.UpdateData;
    except
      AcquireFocus;
      raise;
  end;
  inherited;
end;

procedure TCustomDBGridView.WMContextMenu(var Message: TMessage);
begin
  Inc(FContextPopup);
  try
    inherited;
  finally
    Dec(FContextPopup);
  end;
end;

function TCustomDBGridView.AcquireLockLayout: Boolean;
begin
  Result := (UpdateLock = 0) and (FLayoutLock = 0);
  if Result then
    LockLayout;
end;

procedure TCustomDBGridView.ChangeIndicator;
begin
  UpdateHeader;
  UpdateScrollBars;
  UpdateVisOriginSize;
  UpdateCursorPos;
  UpdateEdit(Editing);
  Invalidate;
end;

procedure TCustomDBGridView.ChangeScale(M, D: Integer);
begin
  inherited ChangeScale(M, D);
  if M <> D then
    FIndicatorWidth := MulDiv(FIndicatorWidth, M, D);
end;

procedure TCustomDBGridView.ClearSelection;
begin
  if MultiSelect and (SelectedRows.Count > 0) then
  begin
    FBookmarks.Clear;
    if Assigned(FOnClearMultiSelect) then
      FOnClearMultiSelect(Self);
  end;
end;

function TCustomDBGridView.CreateColumns: TGridColumns;
begin
  Result := TDBGridColumns.Create(Self);
end;

function TCustomDBGridView.CreateFixed: TCustomGridFixed;
begin
  Result := TDBGridFixed.Create(Self);
end;

function TCustomDBGridView.CreateHeader: TCustomGridHeader;
begin
  Result := TDBGridHeader.Create(Self);
end;

function TCustomDBGridView.CreateRows: TCustomGridRows;
begin
  Result := TDBGridRows.Create(Self);
end;

function TCustomDBGridView.CreateScrollBar(Kind: TScrollBarKind)
  : TGridScrollBar;
begin
  if Kind = sbVertical then
    Result := TDBGridScrollBar.Create(Self, Kind)
  else
    Result := inherited CreateScrollBar(Kind);
end;

procedure TCustomDBGridView.DataEditError(E: Exception;
  var Action: TDBGridDataAction);
begin
  if Assigned(FOnDataEditError) then
    FOnDataEditError(Self, E, Action);
end;

procedure TCustomDBGridView.DataFieldUpdated(Field: TField);
begin
  if Assigned(FOnDataUpdateField) then
    FOnDataUpdateField(Self, EditField);
end;

procedure TCustomDBGridView.DataLayoutChanged;
begin
  if AcquireLockLayout then
  begin
    UnLockLayout(False);
    if Assigned(FOnDataLayoutChanged) then
      FOnDataLayoutChanged(Self);
  end;
end;

procedure TCustomDBGridView.DataLinkActivate(Active: Boolean);
begin
  ResetClickPos;
  DataLayoutChanged;
  ClearSelection;
  UpdateScrollBars;
  UpdateScrollPos;
  UpdateCursorPos;
  UpdateEdit(Editing);
  if Assigned(FOnDataActiveChanged) then
    FOnDataActiveChanged(Self);
end;

procedure TCustomDBGridView.DataRecordChanged(Field: TField);
var
  i     : Integer;
  CField: TField;
begin
  if Field <> nil then
  begin
    for i := 0 to Columns.Count - 1 do
      if Columns[i].Field = Field then
        InvalidateColumn(i);
    InvalidateRow(CellFocused.Row);
    CField := EditField;
    if (CField = Field) and (CField.Text <> FFieldText) then
    begin
      UpdateEditContents(False);
      if Edit <> nil then
        Edit.Deselect;
    end;
  end
  else
    Invalidate;
end;

procedure TCustomDBGridView.DataSetChanged;
begin
  ResetClickPos;
  UpdateRowCount;
  UpdateScrollBars;
  UpdateCursorPos;
  UpdateEditContents(False);
  ValidateRect(Handle, nil);
  Invalidate;
  if Assigned(FOnDataChanged) then
    FOnDataChanged(Self);
end;

procedure TCustomDBGridView.DataSetDeleteRecord;
const
  Flags = MB_ICONQUESTION or MB_YESNO;
var
  AllowDelete: Boolean;
  Msg        : string;
begin
  if (not FDataLink.Active) or (FDataLink.DataSet = nil) then
    Exit;
  with FDataLink.DataSet do
    if (State <> dsInsert) and (not IsEmpty) and
      CanModify and (not ReadOnly) then
    begin
      AllowDelete := FAllowDeleteRecord;
      if not Assigned(FOnDataDeleteRecord) then
      begin
        Msg := SDeleteRecordQuestion;
        with Application do
          AllowDelete := AllowDelete and
            (MessageBox(PChar(Msg), PChar(Title), Flags) = ID_YES);
      end
      else
        FOnDataDeleteRecord(Self, AllowDelete);
      if AllowDelete then
        Delete;
    end;
end;

procedure TCustomDBGridView.DataSetInsertRecord(AppendMode: Boolean);
var
  AllowInsert: Boolean;
begin
  if (not FDataLink.Active) or (FDataLink.DataSet = nil) then
    Exit;
  with FDataLink.DataSet do
    if (State <> dsInsert) and CanModify and (not ReadOnly) and AllowEdit then
    begin
      AllowInsert := FAllowInsertRecord;
      if Assigned(FOnDataInsertRecord) then
        FOnDataInsertRecord(Self, AllowInsert);
      if AllowInsert then
      begin
        if AppendMode then
          Append
        else
          Insert;
        Editing := True;
      end;
    end;
end;

procedure TCustomDBGridView.DataSetScrolled(Distance: Integer);
var
  R: TRect;
begin
  HideCursor;
  try
    if FDataLink.ActiveRecord >= Rows.Count then
      UpdateRowCount;
    UpdateScrollBars;
    UpdateCursorPos;
    if Distance <> 0 then
    begin
      if Abs(Distance) <= VisSize.Row then
      begin
        R := GetRowsRect(0, VisSize.Row - 1);
        ScrollWindowEx(Handle, 0, -Distance * Rows.Height, @R, @R, 0, nil,
          SW_INVALIDATE);
      end
      else
        Invalidate;
    end;
  finally
    ShowCursor;
    // added by TS (is this the best place to update the indicator area?)
    if MultiSelect then
      ChangeIndicator;
  end;
end;

procedure TCustomDBGridView.DataUpdateError(E: Exception;
  var Action: TDBGridDataAction);
begin
  if Assigned(FOnDataUpdateError) then
    FOnDataUpdateError(Self, E, Action);
end;

procedure TCustomDBGridView.DefaultDrawCell(Cell: TGridCell; Rect: TRect);
var
  b : Boolean;
begin
  inherited;
  b := True;
  UpdateSelection(Cell, b);
end;

procedure TCustomDBGridView.DefineProperties(Filer: TFiler);
var
  HasColumns: Boolean;
  AGrid: TCustomDBGridView;
begin
  HasColumns := not DefaultLayout;
  if HasColumns and (Filer.Ancestor <> nil) then
  begin
    AGrid := TCustomDBGridView(Filer.Ancestor);
    if not AGrid.DefaultLayout then
      HasColumns := not CollectionsEqual(Columns, AGrid.Columns, nil, nil);
  end;
  Filer.DefineProperty('Columns', ReadColumns, WriteColumns, HasColumns);
end;

function TCustomDBGridView.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(OnMouseWheelDown) then
    OnMouseWheelDown(Self, Shift, MousePos, Result);
   if (not Result) and (gkMouseWheel in CursorKeys) then
  begin
    SetCursor(GetCursorCell(CellFocused, goMouseWheelDown), True, True);
    MoveRow(1, MultiSelect and (ssShift in Shift));
    Result := True;
  end;
end;

function TCustomDBGridView.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(OnMouseWheelUp) then
    OnMouseWheelUp(Self, Shift, MousePos, Result);
  if (not Result) and (gkMouseWheel in CursorKeys) then
  begin
    SetCursor(GetCursorCell(CellFocused, goMouseWheelUp), True, True);
    MoveRow(-1, MultiSelect and (ssShift in Shift));
    Result := True;
  end;
end;

function TCustomDBGridView.EditCanAcceptKey(Cell: TGridCell; Key: Char)
  : Boolean;
begin
  Result := False;
  if IsCellValid(Cell) and (Cell.Col >= Fixed.Count) and DataLink.Active then
  begin
    with Columns[Cell.Col] do
      Result := (Field <> nil) and Field.IsValidChar(Key);
    if Assigned(OnEditAcceptKey) then
      OnEditAcceptKey(Self, Cell, Key, Result);
  end;
end;

function TCustomDBGridView.EditCanModify(Cell: TGridCell): Boolean;
var
  Action : TDBGridDataAction;
  B      : Boolean;
begin
// updated TS : eventhandler should be able to overrule the other restrictions
  B := DataLink.Active and
    (not DataLink.ReadOnly) and (not IsReadOnlyField(EditField)) and
    (EditField <> nil) and (EditField.CanModify);
  Result := inherited EditCanModify(Cell) or b;
  if Result then
    try
      if not DataLink.Editing then
        Result := DataLink.Edit;
      if Result then
        DataLink.Modified;
    except
      on E: Exception do
      begin
        if not(E is EAbort) then
        begin
          Action := gdaFail;
        DataEditError(E, Action);
      end
      else
        Action := gdaAbort;
      if Action = gdaFail then
        raise;
      if Action = gdaAbort then
        System.SysUtils.Abort;
    end;
  end;
end;

function TCustomDBGridView.EditCanShow(Cell: TGridCell): Boolean;
begin
  Result := DataLink.Active and inherited EditCanShow(Cell);
end;

procedure TCustomDBGridView.GetCellColors(Cell: TGridCell; Canvas: TCanvas);
var
  OldActive: Integer;
begin
  if FDataLink.Active and IsCellValidEx(Cell, True, False) then
  begin
    OldActive := FDataLink.ActiveRecord;
    try
      FDataLink.ActiveRecord := Cell.Row;
      inherited;
    finally
      FDataLink.ActiveRecord := OldActive;
    end;
  end
  else
    inherited;

  if Cell.Col >= Fixed.Count then
  begin
    if Enabled and not IsCellHighlighted(Cell) and
      IsRowMultiSelected(Cell.Row) then
    begin
      Canvas.Brush.Color := clGray;
      Canvas.Font.Color := clHighlightText;
    end
  end;
end;

function TCustomDBGridView.GetCellText(Cell: TGridCell): string;
var
  OldActive: Integer;
  Field: TField;
begin
  Result := inherited GetCellText(Cell);
  if FDataLink.Active and IsCellValidEx(Cell, True, False) then
  begin
    OldActive := FDataLink.ActiveRecord;
    try
      FDataLink.ActiveRecord := Cell.Row;
      Field := Columns[Cell.Col].Field;
      if (Field <> nil) and (Field.DataSet <> nil) then
        Result := Field.DisplayText;
      if Assigned(OnGetCellText) then
        OnGetCellText(Self, Cell, Result);
    finally
      FDataLink.ActiveRecord := OldActive;
    end;
  end;
end;

function TCustomDBGridView.GetColumnClass: TGridColumnClass;
begin
  Result := TDBGridColumn;
end;

function TCustomDBGridView.GetEditClass(Cell: TGridCell): TGridEditClass;
begin
  Result := TDBGridEdit;
end;

function TCustomDBGridView.GetEditText(Cell: TGridCell): string;
begin
  Result := inherited GetEditText(Cell);
  if EditField <> nil then
    FFieldText := EditField.Text
  else
    FFieldText := '';
end;

procedure TCustomDBGridView.HideCursor;
begin
  inherited;
  InvalidateIndicatorImage(Row);
end;

procedure TCustomDBGridView.InvalidateIndicator;
begin
  InvalidateRect(GetIndicatorRect);
end;

procedure TCustomDBGridView.InvalidateIndicatorImage;
begin
  InvalidateRect(GetIndicatorImageRect(Row));
end;

procedure TCustomDBGridView.KeyDown(var Key: Word; Shift: TShiftState);
var
  KeyScroll: Boolean;
begin
  KeyScroll := Key in [VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT, VK_HOME, VK_END,
    VK_TAB, VK_RETURN, VK_ESCAPE, VK_INSERT, VK_DELETE];
  if KeyScroll then
    LockScroll;
  try
    inherited;
    if (not FDataLink.Active) or (FDataLink.DataSet = nil) then
      Exit;
    try
      if gkArrows in CursorKeys then
        case Key of
          VK_UP:
            begin
              MoveRow(-1, ssShift in Shift);
              SetCursor(CellFocused, True, True);
            end;
          VK_DOWN:
            begin
              MoveRow(1, ssShift in Shift);
              SetCursor(CellFocused, True, True);
            end;
          VK_PRIOR:
            begin
              MoveRow(-VisSize.Row, ssShift in Shift);
              SetCursor(CellFocused, True, True);
            end;
          VK_NEXT:
            begin
              MoveRow(VisSize.Row, ssShift in Shift);
              SetCursor(CellFocused, True, True);
            end;
          VK_HOME:
            if ssCtrl in Shift then
            begin
              FDataLink.DataSet.First;
              SetCursor(CellFocused, True, True);
            end;
          VK_END:
            if ssCtrl in Shift then
            begin
              FDataLink.DataSet.Last;
              SetCursor(CellFocused, True, True);
            end;
        end;
      if (gkTabs in CursorKeys) and (Key = VK_TAB) and (not RowSelect) then
      begin
        if (CellFocused.Col = Columns.Count - 1) and
          (not(ssShift in Shift)) then
        begin
          MoveRow(1);
          SetCursor(GetCursorCell(CellFocused, goHome), True, True);
        end;
        if (CellFocused.Col = Fixed.Count) and (ssShift in Shift) then
        begin
          MoveRow(-1);
          SetCursor(GetCursorCell(CellFocused, goEnd), True, True);
        end;
      end;
      case Key of
        VK_ESCAPE:
          begin
            CancelEdit;
            SetCursor(CellFocused, True, True);
          end;
        VK_INSERT:
          if (Shift = []) and (not Editing)
          then
            DataSetInsertRecord(False);
        VK_DELETE:
          if (Shift = []) and (not Editing) then
          begin
            DataSetDeleteRecord;
            SetCursor(CellFocused, True, True);
          end;
      end;
    except
      SetCursor(CellFocused, True, True);
      raise;
    end;
  finally
    if KeyScroll then
      UnLockScroll(False);
  end;
end;

procedure TCustomDBGridView.KeyPress(var Key: Char);
var
  ReturnScroll: Boolean;
begin
  { go to next line if we use return to go to the next cell when we are in the
    last column }

  { changed by TS: and Editing was outcommented }
  ReturnScroll := (Key = #13) and (gkReturn in CursorKeys);
  if ReturnScroll then
    LockScroll;
  try
    inherited;
    // SPACE can be used to toggle selection of the current row when RowSelect
    // and MultiSelect is True.
    if (Key = #32) and RowSelect and MultiSelect then
    begin
      RowSelected[Row] := not RowSelected[Row];
      InvalidateIndicator;
    end;
    if ReturnScroll and (Key = #13) and
       (CellFocused.Col >= LastSelectableCol(CellFocused.Row)) then
    begin
      MoveRow(1);
      SetCursor(GetCursorCell(CellFocused, goHome), True, True);
    end;
    { Clear multiselection when ESC is pressed }
    if (Key = #27) and MultiSelect and (SelectedRows.Count > 0) then
      DeselectAll;
  finally
    if ReturnScroll then
      UnLockScroll(False);
  end;
end;

procedure TCustomDBGridView.Loaded;
begin
  inherited;
  DataLayoutChanged;
end;

procedure TCustomDBGridView.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Select      : Boolean;
  SelectRange : Boolean;

begin
  if not AcquireFocus then
  begin
    MouseCapture := False;
    Exit;
  end;
  Select := (Button = mbLeft) and MultiSelect and FDatalink.Active and
    ((ssCtrl in Shift) or (ssShift in Shift));
  SelectRange := Select and (ssShift in Shift);
  MoveRowByMouse(Shift, X, Y, Select, SelectRange);
  inherited;
end;

procedure TCustomDBGridView.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if (gkMouseMove in CursorKeys) and (not ColResizing) then
  begin
    MoveRowByMouse(Shift, X, Y, True, True);
  end;
  inherited;
end;

{ TS: added code to handle multiselection }

procedure TCustomDBGridView.MoveRow(Distance: Integer; Select : Boolean);
begin
  if (not FDatalink.Active) or (FDataLink.DataSet = nil) then
    Exit;
  with FDatalink.DataSet do
    if Distance < 0 then
    begin
      if (State = dsInsert) and (not Modified) and FDataLink.EOF and
        (not FDatalink.FModified) then
      begin
        Editing := False;
        Cancel;
      end
      else
        DoSelection(Select, Distance);
    end
    else if Distance > 0 then
    begin
      if (State = dsInsert) and (not Modified) and
        (not FDataLink.FModified) then
        if not FDataLink.EOF then
        begin
          Cancel;
          Editing := False;
        end
        else
          Exit
      else
        DoSelection(Select, Distance);
      if FDataLink.EOF then
        DataSetInsertRecord(True);
    end
    else // distance = 0
    begin
      DoSelection(Select, 0);
    end;
end;

{ Moves the active row to the new position after selection with the mouse. }

procedure TCustomDBGridView.MoveRowByMouse(Shift: TShiftState; X, Y: Integer;
  Select, SelectRange: Boolean);
var
  C: TGridCell;

  function IsLeftButtonPressed: Boolean;
  begin
    Result := (ssLeft in Shift) or ((ssRight in Shift) and RightClickSelect);
  end;

begin
  if (gkMouse in CursorKeys) and IsLeftButtonPressed then
  begin
    if PtInRect(GetIndicatorFixedRect, Point(X, Y)) then
    begin
      C.Col := CellFocused.Col;
      C.Row := GetRowAt(X, Y);
    end
    else if PtInRect(GetGridRect, Point(X, Y)) then
      C := GetCellAt(X, Y)
    else
      Exit;
    if C.Row <> -1 then
    begin
      LockScroll;
      try
        if Select then
        begin
          if SelectRange then
          begin
            MoveRow(C.Row - CellFocused.Row, True);
            SetCursor(C, True, True);
          end
          else
          begin
            MoveRow(C.Row - CellFocused.Row);
            SetCursor(C, True, True);
            MoveRow(0, True);
          end;
          InvalidateIndicator;
        end
        else
        begin
          ClearSelection;
          SetCursor(C, True, True);
        end;
      finally
        UnLockScroll(False);
      end;
    end;
  end;
end;

procedure TCustomDBGridView.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  I: Integer;
  NeedRefresh: Boolean;
begin
  inherited;
  if Operation = opRemove then
    if AComponent is TField then
    begin
      NeedRefresh := False;
      LockUpdate;
      try
        if Columns <> nil then
          for I := Columns.Count - 1 downto 0 do
            if Columns[I].FField = AComponent then
            begin
              Columns[I].FField := nil;
              NeedRefresh := True;
            end;
      finally
        UnlockUpdate(NeedRefresh);
      end;
    end
    else if AComponent = DataSource then
      DataSource := nil
    else if AComponent = FIndicatorImages then
      IndicatorImages := nil;
end;

procedure TCustomDBGridView.Paint;
begin
  if ShowIndicator then
  begin
    PaintIndicatorHeader;
    PaintIndicatorFixed;
    if GridLines then
      PaintIndicatorGrid;
  end;
  inherited;
end;

procedure TCustomDBGridView.PaintCell(Cell: TGridCell; Rect: TRect);
var
  OldActive: Integer;
begin
  OldActive := FDataLink.ActiveRecord;
  try
    FDataLink.ActiveRecord := Cell.Row;
    inherited;
  finally
    FDataLink.ActiveRecord := OldActive;
  end;
end;

procedure TCustomDBGridView.PaintIndicatorFixed;
var
  J: Integer;
  R: TRect;
  LDetails: TThemedElementDetails;
begin
  Canvas.Brush.Color := Fixed.Color;
  R := GetIndicatorFixedRect;
  R.Bottom := GetRowRect(VisOrigin.Row).Top;
  for J := 0 to VisSize.Row - 1 do
  begin
    R.Top := R.Bottom;
    R.Bottom := R.Bottom + Rows.Height;
    if RectVisible(Canvas.Handle, R) then
    begin
      Canvas.FillRect(R);
      PaintIndicatorImage(R, J);
    end;
  end;
  R.Top := R.Bottom;
  R.Bottom := GetIndicatorFixedRect.Bottom;
  with Canvas do
  begin
{$IF CompilerVersion > 21}
    LDetails := StyleServices.GetElementDetails(tgFixedCellNormal);
{$ELSE}
    LDetails := StyleServices.GetElementDetails(thHeaderItemNormal);
{$IFEND}
    if not (IsFixedVisible or (gsListViewLike in GridStyle)) then
      Brush.Color := Color;
    StyleServices.DrawElement(Canvas.Handle, LDetails, R);
    FillRect(R);
  end;
  if Fixed.Flat then
  begin
    R := GetIndicatorFixedRect;
    if not IsFixedVisible then
    begin
      if VisSize.Row = 0 then
        Exit;
      R.Bottom := GetRowRect(VisOrigin.Row + VisSize.Row).Top;
    end;
    if Fixed.GridColor then
    begin
      if not (gsDotLines in GridStyle) then
      begin
        Canvas.Pen.Color := GetGridLineColor(Color);
        Canvas.Pen.Width := GridLineWidth;
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
        LineTo(R.Right - 2, R.Bottom - 1);
        Pen.Color := clBtnHighlight;
        MoveTo(R.Right - 1, R.Bottom - 1);
        LineTo(R.Right - 1, R.Top - 1);
      end;
  end;
end;

procedure TCustomDBGridView.PaintIndicatorGrid;
var
  Points        : PIntArray;
  PointCount    : Integer;
  StrokeList    : PIntArray;
  StrokeCount   : Integer;
  I, L, R, Y, C : Integer;
  Index         : Integer;
  Rect          : TRect;

  procedure ShiftGridPoints(DX, DY: Integer);
  var
    i: Integer;
  begin
    i := 0;
    while i < PointCount * 2 do
    begin
      if i mod 2 = 1 then
        Points^[i] := Points^[i] + DY;
      Inc(i);
    end;
  end;

  procedure Paint3DCells(Rect: TRect);
  var
    R: TRect;
  begin
    R := Rect;
    R.Bottom := R.Top;
    while R.Bottom < Rect.Bottom do
    begin
      R.Top := R.Bottom;
      R.Bottom := R.Bottom + Rows.Height;
      if RectVisible(Canvas.Handle, R) then
        Paint3DFrame(R, BF_RECT);
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
      StrokeCount := VisSize.Row;
      if gsListViewLike in GridStyle then
        StrokeCount := GetGridHeight div Rows.Height;
    end;
    if StrokeCount > 0 then
    begin
      PointCount := StrokeCount * 2;
      StrokeList := AllocMem(StrokeCount * SizeOf(Integer));
      Points := AllocMem(PointCount * SizeOf(TPoint));
      FillDWord(StrokeList^, StrokeCount, 2);
      Rect := GetIndicatorFixedRect;
      if gsHorzLine in GridStyle then
      begin
        L := Rect.Left;
        R := Rect.Right;
        Y := GetRowRect(VisOrigin.Row).Top;
        C := VisSize.Row;
        if gsListViewLike in GridStyle then
          C := GetGridHeight div Rows.Height;
        for I := 0 to C - 1 do
        begin
          Y := Y + Rows.Height;
          Index := I * 4;
          Points^[Index + 0] := L;
          Points^[Index + 1] := Y - 2;
          Points^[Index + 2] := R - 1;
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
            Pen.Width := GridLineWidth;
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
  else if gsHorzLine in GridStyle then
  begin
    Rect := GetIndicatorFixedRect;
    if not (gsListViewLike in GridStyle) then
      Rect.Bottom := GetRowRect(VisOrigin.Row + VisSize.Row).Top;
    Paint3DCells(Rect);
    if IsFixedVisible and (not (gsListViewLike in GridStyle)) then
    begin
      Rect.Top := Rect.Bottom;
      Rect.Bottom := GetFixedRect.Bottom;
      PaintBottom3DMargin(Rect);
    end;
  end
  else
  begin
    Rect := GetIndicatorFixedRect;
    if not IsFixedVisible then
    begin
      Rect.Bottom := GetRowRect(VisOrigin.Row + VisSize.Row).Top;
      Paint3DFrame(Rect, BF_RECT);
    end
    else
      PaintBottom3DMargin(Rect);
  end;
end;

procedure TCustomDBGridView.PaintIndicatorHeader;
var
  Rect: TRect;
begin
  with Canvas do
  begin
    Rect := GetIndicatorHeaderRect;
    Brush.Color := Header.Color;
    Font := Header.Font;
    FillRect(Rect);
    if Header.Flat then
    begin
      if Header.GridColor then
      begin
        Pen.Color := GetGridLineColor(Color);
        Pen.Width := GridLineWidth;
        MoveTo(Rect.Right, Rect.Top);
        LineTo(Rect.Right, Rect.Bottom);
        MoveTo(Rect.Left, Rect.Bottom - 1);
        LineTo(Rect.Right, Rect.Bottom - 1);
      end
      else
      begin
        Pen.Width := 1;
        Pen.Color := clBtnShadow;
        MoveTo(Rect.Right - 2, Rect.Top);
        LineTo(Rect.Right - 2, Rect.Bottom - 2);
        Pen.Color := clBtnHighlight;
        MoveTo(Rect.Right - 1, Rect.Top);
        LineTo(Rect.Right - 1, Rect.Bottom - 2);
        Pen.Color := clBtnShadow;
        MoveTo(Rect.Left, Rect.Bottom - 2);
        LineTo(Rect.Right, Rect.Bottom - 2);
        Pen.Color := clBtnHighlight;
        MoveTo(Rect.Left, Rect.Bottom - 1);
        LineTo(Rect.Right, Rect.Bottom - 1);
      end;
    end
    else
      Paint3DFrame(Rect, BF_RECT);
  end;
end;

procedure TCustomDBGridView.PaintIndicatorImage(Rect: TRect; DataRow: Integer);
var
  i, X, Y, W, H: Integer;
  IL           : TImageList;
  BKC, BLC     : DWORD;
begin
  i := GetIndicatorImage(DataRow);
  if i = -1 then
    Exit;
  IL := FIndicatorImages;
  if IL = nil then
    IL := FIndicatorsDef;
  W := IL.Width;
  H := IL.Height;
  X := Rect.Right - Rect.Left - W;
  X := Rect.Left + X div 2 - Ord(Fixed.Flat);
  if X + W > Rect.Right then
    W := Rect.Right - X;
  Y := Rect.Bottom - Rect.Top - H;
  Y := Rect.Top + Y div 2 - Ord(Fixed.Flat);
  if Y + H > Rect.Bottom then
    H := Rect.Bottom - Y;
  BKC := ColorToRGB(IL.BkColor);
  BLC := ColorToRGB(IL.BlendColor);
  ImageList_DrawEx(IL.Handle, i, Canvas.Handle, X, Y, W, H, BKC, BLC,
    ILD_TRANSPARENT);
end;

procedure TCustomDBGridView.ChangeEditText(const S: string);
begin
  if Editing and EditCanModify(EditCell) then
  begin
    Edit.Text := S;
    DataLink.Modified;
  end;
end;

function TCustomDBGridView.IsCellReadOnly(Cell: TGridCell): Boolean;
begin
// changed TS : the eventhandler should make the final decision in determinating
// the ReadOnly status of a cell.
  if IsCellValid(Cell) then
    Result := inherited IsCellReadOnly(Cell) and
      IsReadOnlyField(Columns[Cell.Col].Field)
  else
    Result := True;
end;

function TCustomDBGridView.IsFixedVisible: Boolean;
begin
  Result := (Columns.Count > 0) and (Fixed.Count > 0);
end;

function TCustomDBGridView.IsRowMultiSelected(ARow: Integer): Boolean;
var
  OldActive: Integer;
begin
  Result := False;
  if ARow >= 0 then
  begin
    OldActive := FDataLink.ActiveRecord;
    try
      FDatalink.ActiveRecord := ARow;
      Result := IsRowMultiSelected;
    finally
      FDatalink.ActiveRecord := OldActive;
    end;
  end;
end;

function TCustomDBGridView.IsRowMultiSelected: Boolean;
begin
  Result := MultiSelect and DataLink.Active and
    FBookmarks.GetCurrentRowSelected;
end;

procedure TCustomDBGridView.SetEditText(Cell: TGridCell; var Value: string);
begin
  if IsCellEqual(Cell, EditCell) and DataLink.Active and
    (not ReadOnly) and (not IsReadOnlyField(EditField)) then
    DataLink.UpdateData;
end;

procedure TCustomDBGridView.Resize;
begin
  UpdateRowCount;
  UpdateCursorPos;
  UpdateScrollPos;
  inherited;
end;

procedure TCustomDBGridView.UpdateData;
var
  Text  : string;
  Action: TDBGridDataAction;
begin
  if DataLink.Active and (DataLink.InUpdateData <> 0) and (not ReadOnly) and
    (EditField <> nil) and (not IsReadOnlyField(EditField)) and
    (Edit <> nil) then
  begin
    try
      Text := Edit.Text;
      inherited SetEditText(EditCell, Text);
      EditField.Text := Text;
    except
      on E: Exception do
      begin
        if not (E is EAbort) then
        begin
          Action := gdaFail;
          DataUpdateError(E, Action);
        end
        else
          Action := gdaAbort;
        if Action = gdaFail then
          raise;
        if Action = gdaAbort then
          System.SysUtils.Abort;
      end;
    end;
    DataFieldUpdated(EditField);
  end;
end;

procedure TCustomDBGridView.AutoSizeCols(AIncludeTitles,
  OnlyVisibleRows: Boolean);
begin
  LockLayout;
  inherited AutoSizeCols(AIncludeTitles, OnlyVisibleRows);
  UnlockLayout(True);
end;

procedure TCustomDBGridView.CancelEdit;
begin
  FDataLink.Reset;
  inherited;
end;

function TCustomDBGridView.GetClientRect: TRect;
begin
  Result := inherited GetClientRect;
  if ShowIndicator and (FContextPopup = 0) then
  begin
    Inc(Result.Left, IndicatorWidth);
    Inc(Result.Right, IndicatorWidth);
  end;
end;

function TCustomDBGridView.GetIndicatorHeaderRect: TRect;
begin
  Result := GetHeaderRect;
  Result.Left := GetIndicatorRect.Left;
  Result.Right := GetIndicatorRect.Right;
end;

function TCustomDBGridView.GetIndicatorFixedRect: TRect;
begin
  Result := GetFixedRect;
  Result.Left := GetIndicatorRect.Left;
  Result.Right := GetIndicatorRect.Right;
end;

function TCustomDBGridView.GetIndicatorImage(DataRow: Integer): Integer;
begin
  Result := -1;
  if DataRow = DataLink.ActiveRecord then
  begin
    Result := 0;
    if DataLink.DataSet <> nil then
      case DataLink.DataSet.State of
        dsEdit:
          Result := 1;
        dsInsert:
          Result := 2;
      end;
    if IsRowMultiSelected(DataRow) then
      Result := 4; // multiselected + current
  end
  else
  begin
    if IsRowMultiSelected(DataRow) then
      Result := 3;
  end;
  if Assigned(FOnGetIndicatorImage) then
    FOnGetIndicatorImage(Self, DataRow, Result);
end;

function TCustomDBGridView.GetIndicatorImageRect(DataRow: Integer): TRect;
begin
  Result := GetIndicatorRect;
  Result.Top := GetRowTopBottom(DataRow).Top;
  Result.Bottom := GetRowTopBottom(DataRow).Bottom;
end;

function TCustomDBGridView.GetIndicatorRect: TRect;
begin
  Result := GetClientRect;
  Result.Right := Result.Left;
  Result.Left := 0;
end;

function TCustomDBGridView.GetMultiSelect: Boolean;
begin
  Result := FMultiSelect;
end;

procedure TCustomDBGridView.LockLayout;
begin
  LockUpdate;
  Inc(FLayoutLock);
  if FLayoutLock = 1 then
    Columns.BeginUpdate;
end;

procedure TCustomDBGridView.LockScroll;
begin
  Inc(FScrollLock);
  if FScrollLock = 1 then
  begin
    FScrollCell := CellFocused;
    FScrollSelected := CellSelected;
  end;
end;

procedure TCustomDBGridView.MakeCellVisible(Cell: TGridCell;
  PartialOK: Boolean);
begin
  if Cell.Row = CellFocused.Row then
    inherited MakeCellVisible(Cell, PartialOK);
end;

procedure TCustomDBGridView.SetCursor(Cell: TGridCell; Selected,
  Visible: Boolean);
var
  IC: TGridCell;
begin
  IC := CellFocused;
  if (FScrollLock <> 0) and (FCursorFromDataSet = 0) then
  begin
    FScrollCell := Cell;
    FScrollSelected := Selected;
    Exit;
  end;
  if FCursorFromDataSet = 0 then
  begin
    Cell.Row := CellFocused.Row;
  end;
  if IC.Row <> Cell.Row then
    InvalidateIndicatorImage(Row);
  inherited SetCursor(Cell, Selected, Visible);
  if IC.Row <> Cell.Row then
    InvalidateIndicatorImage(Row);
end;

procedure TCustomDBGridView.UndoEdit;
begin
  if DataLink.FModified then
    DataLink.Reset;
end;

procedure TCustomDBGridView.UnLockLayout(CancelChanges: Boolean);
begin
  if FLayoutLock = 1 then
  begin
    if not CancelChanges then
      UpdateLayout;
    Columns.EndUpdate;
  end;
  Dec(FLayoutLock);
  UnLockUpdate(False);
end;

procedure TCustomDBGridView.UnLockScroll(CancelScroll: Boolean);
begin
  Dec(FScrollLock);
  if (FScrollLock = 0) and ((not IsCellEqual(FScrollCell, CellFocused)) or
    (FScrollSelected <> CellSelected)) then
  begin
    SetCursor(GridCell(FScrollCell.Col, CellFocused.Row),
      FScrollSelected, True);
    if (not CancelScroll) and (FScrollCell.Row <> CellFocused.Row) then
    begin
      if (not FDataLink.Active) or (FDataLink.DataSet = nil) then
        Exit;
      FDataLink.MoveBy(FScrollCell.Row - CellFocused.Row);
    end;
  end;
end;

procedure TCustomDBGridView.UpdateCursorPos;
var
  Cell: TGridCell;
begin
  Inc(FCursorFromDataSet);
  try
    if FDataLink.Active then
    begin
      Cell.Col := CellFocused.Col;
      Cell.Row := FDataLink.ActiveRecord;
    end
    else
      Cell := GridCell(0, 0);
    SetCursor(Cell, CellSelected, True);
  finally
    Dec(FCursorFromDataSet);
  end;
end;

procedure TCustomDBGridView.UpdateLayout;
var
  I      : Integer;
  List   : TList;
  Column : TDBGridColumn;

  procedure GetFields(Fields: TFields);
  var
    I: Integer;
  begin
    for I := 0 to Fields.Count - 1 do
    begin
      List.Add(Fields[I]);
      if Fields[I].DataType in [ftADT, ftArray] then
        GetFields((Fields[I] as TObjectField).Fields);
    end;
  end;

begin
  if ([csLoading, csDestroying] * ComponentState) <> [] then
    Exit;

  if FDefaultLayout then
  begin
    if FDataLink.Active and (FDataLink.DataSet <> nil) then
    begin
      List := TList.Create;
      try
        GetFields(FDataLink.DataSet.Fields);
        while (List.Count > 0) and (Columns.Count < List.Count) do
          Columns.Add;
        while (Columns.Count > 0) and (Columns.Count > List.Count) do
          Columns[0].Free;
        for I := 0 to List.Count - 1 do
        begin
          Column := Columns[I];
          Column.FieldName := TField(List[I]).FullName;
          Column.Field := nil;
          Column.RestoreDefaults;
          Column.FDefaultColumn := True;
        end;
      finally
        List.Free;
      end;
    end
    else
      Columns.Clear;
    Header.FullSynchronizing := True;
    Header.Synchronized := True;
  end
  else
    for I := 0 to Columns.Count - 1 do
    begin
      Column := Columns[I];
      Column.Field := nil;
      if Column.DefaultColumn then
      begin
        Column.RestoreDefaults;
        Column.FDefaultColumn := True;
      end;
    end;
  Fixed.SetCount(Fixed.Count);
  UpdateRowCount;
  UpdateCursorPos;
end;

procedure TCustomDBGridView.UpdateRowCount;

  procedure SetRowsCount(Value: Integer);
  begin
    with TDBGridRows(Rows) do
    begin
      Inc(FRowsFromGrid);
      try
        SetCount(Value);
      finally
        Dec(FRowsFromGrid);
      end;
    end;
  end;

begin
  if DataLink.Active then
  begin
    FDataLink.BufferCount := GetGridHeight div Rows.Height;
    SetRowsCount(FDataLink.RecordCount);
  end
  else
    SetRowsCount(0);
end;

procedure TCustomDBGridView.UpdateSelection(var Cell: TGridCell;
  var Selected: Boolean);
begin
  inherited;
  Cell.Row := FDataLink.ActiveRecord;
 end;

{ TBookmarkList }

constructor TBookmarkList.Create(AGrid: TCustomDBGridView);
begin
  inherited Create;
  FList := TList<TBookmark>.Create;
  FList.OnNotify := BookmarksChanged;
  FGrid := AGrid;
end;

destructor TBookmarkList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TBookmarkList.Clear;
begin
  if FList.Count = 0 then
    Exit;
  FList.Clear;
  FGrid.Invalidate;
end;

function TBookmarkList.Compare(const Item1, Item2: TBookmark): Integer;
begin
  with FGrid.DataLink.DataSource.DataSet do
    Result := CompareBookmarks(TBookmark(Item1), TBookmark(Item2));
end;

function TBookmarkList.CurrentRow: TBookmark;
begin
  Result := FGrid.DataLink.DataSource.DataSet.Bookmark;
end;

function TBookmarkList.GetCurrentRowSelected: Boolean;
var
  Index: Integer;
begin
  Result := Find(CurrentRow, Index);
end;

function TBookmarkList.Find(const Item: TBookmark;
  var Index: Integer): Boolean;
var
  L, H, i, C: Integer;
begin
  if (Item = FCache) and (FCacheIndex >= 0) then
  begin
    Index := FCacheIndex;
    Result := FCacheFind;
    Exit;
  end;
  Result := False;
  L := 0;
  H := FList.Count - 1;
  while L <= H do
  begin
    i := (L + H) shr 1;
    C := Compare(FList[i], Item);
    if C < 0 then
      L := i + 1
    else
    begin
      H := i - 1;
      if C = 0 then
      begin
        Result := True;
        L := i;
      end;
    end;
  end;
  Index := L;
  FCache := Item;
  FCacheIndex := Index;
  FCacheFind := Result;
end;

function TBookmarkList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TBookmarkList.GetItem(Index: Integer): TBookmark;
begin
  Result := FList[Index];
end;

function TBookmarkList.IndexOf(const Item: TBookmark): Integer;
begin
  if not Find(Item, Result) then
    Result := -1;
end;

procedure TBookmarkList.LinkActive(Value: Boolean);
begin
  Clear;
  FLinkActive := Value;
end;

function TBookmarkList.Refresh: Boolean;
var
  I: Integer;
begin
  Result := False;
  with FGrid.DataLink.Datasource.Dataset do
  try
    CheckBrowseMode;
    for I := FList.Count - 1 downto 0 do
      if not BookmarkValid(TBookmark(FList[I])) then
        begin
          Result := True;
          FList.Delete(i);
        end;
    finally
      UpdateCursorPos;
      if Result then
        FGrid.Invalidate;
    end;
end;

procedure TBookmarkList.SetCurrentRowSelected(Value: Boolean);
var
  Index   : Integer;
  Current : TBookmark;
begin
  Current := CurrentRow;
  if (Length(Current) = 0) or (Find(Current, Index) = Value) then
    Exit;
  if Value then
    FList.Insert(Index, Current)
  else
    FList.Delete(Index);
  FGrid.InvalidateRow(FGrid.Row);
end;

procedure TBookmarkList.BookmarksChanged(Sender: TObject; const Item: TBookmark;
  Action: TCollectionNotification);
begin
  if Action in [cnAdded, cnRemoved] then
  begin
    FCache := nil;
    FCacheIndex := -1;
  end;
end;

end.
