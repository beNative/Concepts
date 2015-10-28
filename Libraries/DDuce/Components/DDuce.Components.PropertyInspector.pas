{
  Copyright (C) 2013-2015 Tim Sinaeve tim.sinaeve@gmail.com

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
  The Original Code is part of 'Extension Library'. The Initial Developer of
  the Original Code (ELPropInsp.pas) is Evgeny Balabuyev (stalcer@rambler.ru).
  Portions created by the Initial Developer are Copyright (C) 2002. All Rights
  Reserved. You may obtain a copy of the original code at
  http://www.torry.net/vcl/packs/lite/extlib.zip

  Changes by Tim Sinaeve:
    - The original comments were translated from Russian to English using
      machine translation.
    - Applied Borland style source formatting
    - Stringlist editor can also be used for string properties
    - replaced Russian comments by machine translation to English
    - unicode support
    - OnDrawCell
    - removed dependencies to other units from the original library.
}

unit DDuce.Components.PropertyInspector;

{$I ..\DDuce.inc}

interface

uses
  System.Classes, System.Types, System.Variants, System.SysUtils,
  System.TypInfo,
  Vcl.Controls, Vcl.Grids, Vcl.Graphics, Vcl.Forms, Vcl.StdCtrls,
  Vcl.Menus, Vcl.Dialogs,
  Winapi.Windows, Winapi.Messages,

  DDuce.Components.PropertyInspector.StringsEditor;

{ TBaseObjectList }

type
  EBaseObjectList = class(Exception);

  TItemByProc = procedure(
        AItem   : TObject;
        AData   : Pointer;
    var AResult : Boolean
  ) of object;

  TBaseObjectList = class
  private
    FItems        : TList;
    FChangingCount: Boolean;
    function GetItems(AIndex: Integer): TObject;
    function GetCount: Integer;
    procedure SetCount(const Value: Integer);
  protected
    function CreateItem: TObject; virtual;
    procedure ValidateAddition; virtual;
    procedure ValidateDeletion; virtual;
    procedure Change; virtual;
    procedure Added; virtual;
    procedure Deleted; virtual;
    function DoItemBy(AData: Pointer; AItemByProc: TItemByProc): TObject;
    function DoFind(AData: Pointer; AItemByProc: TItemByProc): TObject;
    function DoSearch(AData: Pointer; AItemByProc: TItemByProc): TObject;
  public
    constructor Create;
    destructor Destroy; override;
    function Add: Integer;
    procedure Remove(AItem: TObject);
    procedure Delete(AIndex: Integer);
    procedure Clear;
    function IndexOf(AItem: TObject): Integer;

    property Items[AIndex: Integer]: TObject
      read GetItems; default;
    property Count: Integer
      read GetCount write SetCount;
  end;

{
  ToDo ( TCustomPropsPage ):

 - When SetDisplayValue exits exceptional situation in certain cases
 (when focus departs from the component and when you press Enter) the contents
 InplaceEditor first it changes, and then already it shows Exception, although
 it is must vice versa (as in Delphi Object inspector). Although upon transfer
 to another line everything works as must.
 - With Expand and Collapse descends the current line, if AdjustTopRow changes
   upper line.
}

  { TPropsPage }

type
  EPropsPage = class(Exception);

  TCustomPropsPage = class;
  TPropsPageItems  = class;

  TPropsPageInplaceEdit = class(TInplaceEditList)
  private
    FChangingBounds: Boolean;
    FReadOnlyStyle : Boolean;
    procedure PickListMeasureItem(Control: TWinControl; Index: Integer;
      var Height: Integer);
    procedure PickListDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure WMLButtonDblClk(var Message: TWMMouse); message WM_LBUTTONDBLCLK;

  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DropDown; override;
    procedure UpdateContents; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoEditButtonClick; override;
    procedure DoGetPickListItems; override;
    procedure CloseUp(Accept: Boolean); override;
    procedure DblClick; override;
    procedure BoundsChanged; override;

  public
    constructor Create(AOwner: TComponent); override;

    property ReadOnlyStyle: Boolean
      read FReadOnlyStyle;
  end;

  TPropsPageItemExpandable = (
    mieAuto,
    mieYes,
    mieNo
  );

  TPropsPageItem = class(TBaseObjectList)
  private
    FParent            : TPropsPageItem;
    FOwner             : TCustomPropsPage;
    FExpandable        : TPropsPageItemExpandable;
    FCaption           : string;
    FExpanded          : Boolean;
    FDisplayValue      : string;
    FEditStyle         : TEditStyle;
    FRow               : Integer;
    FReadOnly          : Boolean;
    FAutoUpdate        : Boolean;
    FOwnerDrawPickList : Boolean;

    function CanExpand: Boolean;
    function Ident: Integer;
    function IsOnExpandButton(AX: Integer): Boolean;
    function GetItems(AIndex: Integer): TPropsPageItem;
    procedure SetExpandable(const Value: TPropsPageItemExpandable);
    procedure SetCaption(const Value: string);
    function GetLevel: Integer;
    procedure SetEditStyle(const Value: TEditStyle);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetAutoUpdate(const Value: Boolean);
    procedure SetOwnerDrawPickList(const Value: Boolean);

  protected
    function CreateItem: TObject; override;
    procedure Change; override;
    procedure Deleted; override;
    function GetDisplayValue: string; virtual;
    procedure SetDisplayValue(const Value: string); virtual;
    procedure EditButtonClick; dynamic;
    procedure EditDblClick; dynamic;
    procedure GetEditPickList(APickList: TStrings); virtual;
    procedure PickListMeasureHeight(const AValue: string; ACanvas: TCanvas;
      var AHeight: Integer); virtual;
    procedure PickListMeasureWidth(const AValue: string; ACanvas: TCanvas;
      var AWidth: Integer); virtual;
    procedure PickListDrawValue(const AValue: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean); virtual;

  public
    constructor Create(AOwner: TCustomPropsPage;
      AParent: TPropsPageItem); virtual;
    destructor Destroy; override;
    procedure Expand;
    procedure Collapse;

    property Owner: TCustomPropsPage
      read FOwner;

    property Parent: TPropsPageItem
      read FParent;

    property Expandable: TPropsPageItemExpandable
      read FExpandable write SetExpandable;

    property Expanded: Boolean
      read FExpanded;

    property Level: Integer
      read GetLevel;

    property Caption: string
      read FCaption write SetCaption;

    property DisplayValue: string
      read GetDisplayValue write SetDisplayValue;

    property EditStyle: TEditStyle
      read FEditStyle write SetEditStyle;

    property ReadOnly: Boolean
      read FReadOnly write SetReadOnly;

    property AutoUpdate: Boolean
      read FAutoUpdate write SetAutoUpdate;

    property OwnerDrawPickList: Boolean
      read FOwnerDrawPickList write SetOwnerDrawPickList;

    property Items[AIndex: Integer]: TPropsPageItem
      read GetItems; default;
  end;

  TPropsPageItems = class(TBaseObjectList)
  private
    FOwner: TCustomPropsPage;
    function GetItems(AIndex: Integer): TPropsPageItem;

  protected
    function CreateItem: TObject; override;
    procedure Change; override;

  public
    constructor Create(AOwner: TCustomPropsPage);

    property Owner: TCustomPropsPage
      read FOwner;

    property Items[AIndex: Integer]: TPropsPageItem
      read GetItems; default;
  end;

  TPropsPageState = set of (
    ppsMovingSplitter,
    ppsChanged,
    ppsDestroying,
    ppsUpdatingEditorContent
  );

  TCustomPropsPage = class(TCustomGrid)
  private
    FState          : TPropsPageState;
    FOldRow         : Integer;
    FSplitterOffset : Integer;
    FEditText       : string;
    FItems          : TPropsPageItems;
    FRows           : array of TPropsPageItem;
    FUpdateCount    : Integer;
    FValuesColor    : TColor;
    FBitmap         : Vcl.Graphics.TBitmap;
    FBitmapBkColor  : TColor;
    FBrush          : HBRUSH;
    FCellBitmap     : Vcl.Graphics.TBitmap;

    procedure ItemsChange;
    function IsOnSplitter(AX: Integer): Boolean;
    procedure UpdateColWidths;
    procedure UpdateScrollBar;
    procedure AdjustTopRow;
    function ItemByRow(ARow: Integer): TPropsPageItem;
    procedure UpdateData(ARow: Integer);
    procedure UpdatePattern;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk);
      message WM_LBUTTONDBLCLK;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest);
      message CM_DESIGNHITTEST;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMExit(var Message: TMessage); message CM_EXIT;
    function GetActiveItem: TPropsPageItem;
    function GetSplitter: Integer;
    procedure SetSplitter(const Value: Integer);
    procedure SetValuesColor(const Value: TColor);

  protected
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect;
      AState: TGridDrawState); override;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    function CreateEditor: TInplaceEdit; override;
    procedure Paint; override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint)
      : Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint)
      : Boolean; override;
    function GetEditStyle(ACol, ARow: Longint): TEditStyle; override;
    function CanEditModify: Boolean; override;
    function GetEditText(ACol, ARow: Longint): string; override;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    procedure CreateHandle; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure TopLeftChanged; override;
    procedure SizeChanged(OldColCount, OldRowCount: Longint); override;
    function CreateItem(AParent: TPropsPageItem): TPropsPageItem; virtual;
    procedure ItemExpanded(AItem: TPropsPageItem); virtual;
    procedure ItemCollapsed(AItem: TPropsPageItem); virtual;
    function GetItemCaptionColor(AItem: TPropsPageItem): TColor; virtual;

    property Items: TPropsPageItems
      read FItems;

    property Splitter: Integer
      read GetSplitter write SetSplitter;

    property ValuesColor: TColor
      read FValuesColor write SetValuesColor default clNavy;

    property Color
      default clBtnFace;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;

    property ActiveItem: TPropsPageItem
      read GetActiveItem;

  end;

  TPropsPage = class(TCustomPropsPage)
  public
    property Items;
    property ActiveItem;

  published
    property Splitter;
    property ValuesColor;
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDock;
    property OnStartDrag;
  end;

{ TPropertyEditor }

type
  TPropertyEditor      = class;
  TPropertyEditorClass = class of TPropertyEditor;

  EPropertyEditor = class(Exception);

  TPropertyAttribute = (
    praValueList,
    praSubProperties,
    praDialog,
    praMultiSelect,
    praSortList,
    praReadOnly,
    praVolatileSubProperties,
    praNotNestable,
    praAutoUpdate,
    praOwnerDrawValues,
    praComponentRef
  );
  TPropertyAttributes = set of TPropertyAttribute;

  TGetEditorClassProc = function(
    AInstance : TPersistent;
    APropInfo : PPropInfo
  ) : TPropertyEditorClass of object;

  TOnGetComponent = procedure(
          Sender         : TObject;
    const AComponentName : string;
      var AComponent     : TComponent
  ) of object;

  TOnGetComponentNames = procedure(
    Sender  : TObject;
    AClass  : TComponentClass;
    AResult : TStrings
  ) of object;

  TOnGetComponentName = procedure(
        Sender     : TObject;
        AComponent : TComponent;
    var AName      : string
  ) of object;

  TPropEditorPropListItem = packed record
    Instance: TPersistent;
    PropInfo: PPropInfo;
  end;

  PPropEditorPropList = ^TPropEditorPropList;
  TPropEditorPropList = array [0 .. 1023 { Range not used }]
    of TPropEditorPropListItem;

  TPropertyEditor = class
  private
    FPropList            : PPropEditorPropList;
    FPropCount           : Integer;
    FOnModified          : TNotifyEvent;
    FOnGetComponent      : TOnGetComponent;
    FOnGetComponentNames : TOnGetComponentNames;
    FOnGetComponentName  : TOnGetComponentName;
    FDesigner            : Pointer;

    function GetPropTypeInfo: PTypeInfo;
    function DoGetValue: string;

  protected
    procedure SetPropEntry(AIndex: Integer; AInstance: TPersistent;
      APropInfo: PPropInfo);
    function GetComponent(const AComponentName: string): TComponent;
    procedure GetComponentNames(AClass: TComponentClass; AResult: TStrings);
    function GetComponentName(AComponent: TComponent): string;
    function GetValue: string; virtual;
    procedure SetValue(const Value: string); virtual;
    function GetAttrs: TPropertyAttributes; virtual;
    procedure GetValues(AValues: TStrings); virtual;
    procedure GetSubProps(AGetEditorClassProc: TGetEditorClassProc;
      AResult: TList); virtual; // Returns a list of TPropertyEditor
    function GetPropName: string; virtual;
    function AllEqual: Boolean; virtual;
    procedure Edit; virtual;
    procedure ValuesMeasureHeight(const AValue: string; ACanvas: TCanvas;
      var AHeight: Integer); virtual;
    procedure ValuesMeasureWidth(const AValue: string; ACanvas: TCanvas;
      var AWidth: Integer); virtual;
    procedure ValuesDrawValue(const AValue: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean); virtual;

  protected
    function GetPropInfo(AIndex: Integer): PPropInfo;
    function GetInstance(AIndex: Integer): TPersistent;
    function GetFloatValue(AIndex: Integer): Extended;
    function GetInt64Value(AIndex: Integer): Int64;
    function GetOrdValue(AIndex: Integer): Longint;
    function GetStrValue(AIndex: Integer): string;
    function GetVarValue(AIndex: Integer): Variant;
    procedure SetFloatValue(Value: Extended);
    procedure SetInt64Value(Value: Int64);
    procedure SetOrdValue(Value: Longint);
    procedure SetStrValue(const Value: string);
    procedure SetVarValue(const Value: Variant);

  public
    constructor Create(ADesigner: Pointer; APropCount: Integer); virtual;
    destructor Destroy; override;
    procedure Modified;

    property PropName: string
      read GetPropName;

    property PropTypeInfo: PTypeInfo
      read GetPropTypeInfo;

    property PropCount: Integer
      read FPropCount;

    property Value: string
      read DoGetValue write SetValue;

    property Designer: Pointer
      read FDesigner;

    property OnModified: TNotifyEvent
      read FOnModified write FOnModified;

    property OnGetComponent: TOnGetComponent
      read FOnGetComponent write FOnGetComponent;

    property OnGetComponentNames: TOnGetComponentNames
      read FOnGetComponentNames write FOnGetComponentNames;

    property OnGetComponentName: TOnGetComponentName
      read FOnGetComponentName write FOnGetComponentName;
  end;

  TNestedPropertyEditor = class(TPropertyEditor)
  protected
    function GetPropName: string; override;

  public
    constructor Create(AParent: TPropertyEditor); reintroduce;
    destructor Destroy; override;
  end;

  { Standard property editors }

  TOrdinalPropertyEditor = class(TPropertyEditor)
  protected
    function AllEqual: Boolean; override;
  end;

  TIntegerPropertyEditor = class(TOrdinalPropertyEditor)
  protected
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TMethodPropertyEditor = class(TOrdinalPropertyEditor)
  protected
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TCharPropertyEditor = class(TOrdinalPropertyEditor)
  protected
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TEnumPropertyEditor = class(TOrdinalPropertyEditor)
  protected
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    function GetAttrs: TPropertyAttributes; override;
    procedure GetValues(AValues: TStrings); override;
  end;

  TFloatPropertyEditor = class(TPropertyEditor)
  protected
    function AllEqual: Boolean; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TStringPropertyEditor = class(TPropertyEditor)
  protected
    function AllEqual: Boolean; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    function GetAttrs: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TFileNamePropertyEditor = class(TStringPropertyEditor)
    procedure Edit; override;
  end;

  TSetElementPropertyEditor = class(TNestedPropertyEditor)
  private
    FElement: Integer;

  protected
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    function GetAttrs: TPropertyAttributes; override;
    procedure GetValues(AValues: TStrings); override;
    function GetPropName: string; override;
    function AllEqual: Boolean; override;

  public
    constructor Create(AParent: TPropertyEditor; AElement: Integer); reintroduce;
    property Element: Integer read FElement;
  end;

  TSetPropertyEditor = class(TOrdinalPropertyEditor)
  protected
    function GetValue: string; override;
    function GetAttrs: TPropertyAttributes; override;
    procedure GetSubProps(AGetEditorClassProc: TGetEditorClassProc;
      AResult: TList); override;
  end;

  TClassPropertyEditor = class(TPropertyEditor)
  protected
    function GetValue: string; override;
    function GetAttrs: TPropertyAttributes; override;
    procedure GetSubProps(AGetEditorClassProc: TGetEditorClassProc;
      AResult: TList); override;
  end;

  TCollectionPropertyEditor = class(TClassPropertyEditor)
  public
    function GetAttrs: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TComponentPropertyEditor = class(TPropertyEditor)
  protected
    function AllEqual: Boolean; override;
    function GetAttrs: TPropertyAttributes; override;
    procedure GetSubProps(AGetEditorClassProc: TGetEditorClassProc;
      AResult: TList); override;
    function GetValue: string; override;
    procedure GetValues(AValues: TStrings); override;
    procedure SetValue(const Value: string); override;
  end;

  TVariantTypePropertyEditor = class(TNestedPropertyEditor)
  private
    FVarTypeNames: TStringList;

  protected
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    procedure GetValues(AValues: TStrings); override;
    function GetPropName: string; override;
    function GetAttrs: TPropertyAttributes; override;
    function AllEqual: Boolean; override;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

  TVariantPropertyEditor = class(TPropertyEditor)
  protected
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    function GetAttrs: TPropertyAttributes; override;
    procedure GetSubProps(AGetEditorClassProc: TGetEditorClassProc;
      AResult: TList); override;
  end;

  TInt64PropertyEditor = class(TPropertyEditor)
  protected
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    function AllEqual: Boolean; override;
  end;

  TComponentNamePropertyEditor = class(TStringPropertyEditor)
  protected
    function GetAttrs: TPropertyAttributes; override;
  end;

  TDatePropertyEditor = class(TPropertyEditor)
  protected
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    function GetAttrs: TPropertyAttributes; override;
  end;

  TTimePropertyEditor = class(TPropertyEditor)
  protected
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    function GetAttrs: TPropertyAttributes; override;
  end;

  TDateTimePropertyEditor = class(TPropertyEditor)
  protected
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    function GetAttrs: TPropertyAttributes; override;
  end;

  { VCL property editors }

  TCaptionPropertyEditor = class(TStringPropertyEditor)
  protected
    function GetAttrs: TPropertyAttributes; override;
  end;

  TColorPropertyEditor = class(TIntegerPropertyEditor)
  private
    FValues: TStrings;
    procedure AddValue(const LS: string);

  protected
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    procedure GetValues(AValues: TStrings); override;
    function GetAttrs: TPropertyAttributes; override;
    procedure Edit; override;
    procedure ValuesMeasureHeight(const AValue: string; ACanvas: TCanvas;
      var AHeight: Integer); override;
    procedure ValuesMeasureWidth(const AValue: string; ACanvas: TCanvas;
      var AWidth: Integer); override;
    procedure ValuesDrawValue(const AValue: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean); override;
  end;

  TCursorPropertyEditor = class(TIntegerPropertyEditor)
  private
    FValues: TStrings;
    procedure AddValue(const LS: string);

  protected
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    procedure GetValues(AValues: TStrings); override;
    function GetAttrs: TPropertyAttributes; override;
    procedure ValuesMeasureHeight(const AValue: string; ACanvas: TCanvas;
      var AHeight: Integer); override;
    procedure ValuesMeasureWidth(const AValue: string; ACanvas: TCanvas;
      var AWidth: Integer); override;
    procedure ValuesDrawValue(const AValue: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean); override;
  end;

  TFontCharsetPropertyEditor = class(TIntegerPropertyEditor)
  private
    FValues: TStrings;
    procedure AddValue(const LS: string);

  protected
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    procedure GetValues(AValues: TStrings); override;
    function GetAttrs: TPropertyAttributes; override;
  end;

  TFontNamePropertyEditor = class(TStringPropertyEditor)
  protected
    function GetAttrs: TPropertyAttributes; override;
    procedure GetValues(AValues: TStrings); override;
  end;

  TImeNamePropertyEditor = class(TStringPropertyEditor)
  protected
    function GetAttrs: TPropertyAttributes; override;
    procedure GetValues(AValues: TStrings); override;
  end;

  TFontPropertyEditor = class(TClassPropertyEditor)
  protected
    function GetAttrs: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TModalResultPropertyEditor = class(TIntegerPropertyEditor)
  protected
    function GetAttrs: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(AValues: TStrings); override;
    procedure SetValue(const Value: string); override;
  end;

  TPenStylePropertyEditor = class(TEnumPropertyEditor)
  protected
    function GetAttrs: TPropertyAttributes; override;
    procedure ValuesMeasureHeight(const AValue: string; ACanvas: TCanvas;
      var AHeight: Integer); override;
    procedure ValuesMeasureWidth(const AValue: string; ACanvas: TCanvas;
      var AWidth: Integer); override;
    procedure ValuesDrawValue(const AValue: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean); override;
  end;

  TBrushStylePropertyEditor = class(TEnumPropertyEditor)
  protected
    function GetAttrs: TPropertyAttributes; override;
    procedure ValuesMeasureHeight(const AValue: string; ACanvas: TCanvas;
      var AHeight: Integer); override;
    procedure ValuesMeasureWidth(const AValue: string; ACanvas: TCanvas;
      var AWidth: Integer); override;
    procedure ValuesDrawValue(const AValue: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean); override;
  end;

  TTabOrderPropertyEditor = class(TIntegerPropertyEditor)
  protected
    function GetAttrs: TPropertyAttributes; override;
  end;

  TShortCutPropertyEditor = class(TOrdinalPropertyEditor)
  protected
    function GetAttrs: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(AValues: TStrings); override;
    procedure SetValue(const Value: string); override;
  end;

  TStringsPropertyEditor = class(TClassPropertyEditor)
  protected
    function GetAttrs: TPropertyAttributes; override;
    procedure Edit; override;
  end;

procedure ELGetObjectsProps(ADesigner: Pointer; AObjList: TList;
  AKinds: TTypeKinds; AOnlyNestable: Boolean;
  AGetEditorClassProc: TGetEditorClassProc; AResult: TList);

{ TPropertyInspector }

{
 TODO ( TPropertyInspector ):

  - It is necessary to make so that correctly they would be reflected ReadOnly
    property, and so that they would not edit. Problem in the fact that in
    massif TCustomPropertyInspector.Objects simultaneously can it is located
    the objects with the property, which in some objects appears ReadOnly, but
    do not have any others.
}

type
  TCustomPropertyInspector = class;

  TPropertyInspectorItem = class(TPropsPageItem)
  private
    FEditor      : TPropertyEditor;
    FDisplayValue: string;
    procedure EditorModified(Sender: TObject);
    procedure EditorGetComponent(Sender: TObject; const AComponentName: string;
      var AComponent: TComponent);
    procedure EditorGetComponentNames(Sender: TObject; AClass: TComponentClass;
      AResult: TStrings);
    procedure EditorGetComponentName(Sender: TObject; AComponent: TComponent;
      var AName: string);
    procedure DoGetPickList(AResult: TStrings);
    procedure SetEditor(const Value: TPropertyEditor);

  protected
    function GetDisplayValue: string; override;
    procedure SetDisplayValue(const Value: string); override;
    procedure GetEditPickList(APickList: TStrings); override;
    procedure EditButtonClick; override;
    procedure EditDblClick; override;
    procedure PickListMeasureHeight(const AValue: string; ACanvas: TCanvas;
      var AHeight: Integer); override;
    procedure PickListMeasureWidth(const AValue: string; ACanvas: TCanvas;
      var AWidth: Integer); override;
    procedure PickListDrawValue(const AValue: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean); override;

  public
    destructor Destroy; override;
    procedure UpdateParams;
    property Editor: TPropertyEditor read FEditor write SetEditor;
  end;

  TPropertyInspectorEditorDescr = record
    TypeInfo: PTypeInfo;
    ObjectClass: TClass;
    PropName: string;
    EditorClass: TPropertyEditorClass;
  end;

  TPropertyInspectorOnFilterProp = procedure(Sender: TObject;
    AInstance: TPersistent; APropInfo: PPropInfo; var AIncludeProp: Boolean)
    of object;
  TPropertyInspectorOnGetCaptionColor = procedure(Sender: TObject;
    APropTypeInfo: PTypeInfo;
    const APropName: string; var AColor: TColor) of object;
  TPropertyInspectorOnGetEditorClass = procedure(Sender: TObject;
    AInstance: TPersistent; APropInfo: PPropInfo;
    var AEditorClass: TPropertyEditorClass) of object;

  TPropertyInspectorPropKind  = (
    pkProperties,
    pkEvents,
    pkReadOnly
  );
  TPropertyInspectorPropKinds = set of TPropertyInspectorPropKind;

  TCustomPropertyInspector = class(TCustomPropsPage)
  private
    FObjects               : TList;
    FEditors               : array of TPropertyInspectorEditorDescr;
    FOnGetComponent        : TOnGetComponent;
    FOnGetComponentNames   : TOnGetComponentNames;
    FOnModified            : TNotifyEvent;
    FOnGetComponentName    : TOnGetComponentName;
    FPropKinds             : TPropertyInspectorPropKinds;
    FOnFilterProp          : TPropertyInspectorOnFilterProp;
    FComponentRefColor     : TColor;
    FComponentRefChildColor: TColor;
    FExpandComponentRefs   : Boolean;
    FReadOnly              : Boolean;
    FDesigner              : Pointer;
    FOnGetCaptionColor     : TPropertyInspectorOnGetCaptionColor;
    FObjectsLocked         : Boolean;
    FOnGetEditorClass      : TPropertyInspectorOnGetEditorClass;

    procedure Change;
    procedure InternalModified;
    function IndexOfEditor(ATypeInfo: PTypeInfo; AObjectClass: TClass;
      const APropName: string; AEditorClass: TPropertyEditorClass): Integer;
    procedure CheckObjectsLock;
    function GetObjects(AIndex: Integer): TPersistent;
    procedure SetObjects(AIndex: Integer; const Value: TPersistent);
    function GetObjectCount: Integer;
    procedure SetPropKinds(const Value: TPropertyInspectorPropKinds);
    procedure SetComponentRefColor(const Value: TColor);
    procedure SetComponentRefChildColor(const Value: TColor);
    procedure SetExpandComponentRefs(const Value: Boolean);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetDesigner(const Value: Pointer);

  protected
    function CreateItem(AParent: TPropsPageItem): TPropsPageItem; override;
    procedure ItemExpanded(AItem: TPropsPageItem); override;
    procedure ItemCollapsed(AItem: TPropsPageItem); override;
    function GetItemCaptionColor(AItem: TPropsPageItem): TColor; override;
    function GetEditorClass(AInstance: TPersistent; APropInfo: PPropInfo)
      : TPropertyEditorClass; virtual;
    procedure GetComponent(const AComponentName: string;
      var AComponent: TComponent); virtual;
    procedure GetComponentNames(AClass: TComponentClass;
      AResult: TStrings); virtual;
    procedure GetComponentName(AComponent: TComponent;
      var AName: string); virtual;
    procedure FilterProp(AInstance: TPersistent; APropInfo: PPropInfo;
      var AIncludeProp: Boolean); virtual;
    procedure GetCaptionColor(APropTypeInfo: PTypeInfo; const APropName: string;
      var AColor: TColor); virtual;

    property Designer: Pointer
      read FDesigner write SetDesigner;

    property PropKinds: TPropertyInspectorPropKinds read FPropKinds
      write SetPropKinds default [pkProperties];

    property ComponentRefColor: TColor read FComponentRefColor
      write SetComponentRefColor default clMaroon;

    property ComponentRefChildColor: TColor
      read FComponentRefChildColor write SetComponentRefChildColor
      default clGreen;

    property ExpandComponentRefs: Boolean
      read FExpandComponentRefs write SetExpandComponentRefs default True;

    property ReadOnly: Boolean
      read FReadOnly write SetReadOnly default False;

    property OnGetComponent: TOnGetComponent
      read FOnGetComponent write FOnGetComponent;

    property OnGetComponentNames: TOnGetComponentNames
      read FOnGetComponentNames write FOnGetComponentNames;

    property OnGetComponentName: TOnGetComponentName
      read FOnGetComponentName write FOnGetComponentName;

    property OnFilterProp: TPropertyInspectorOnFilterProp
      read FOnFilterProp write FOnFilterProp;

    property OnModified: TNotifyEvent
      read FOnModified write FOnModified;

    property OnGetCaptionColor: TPropertyInspectorOnGetCaptionColor
      read FOnGetCaptionColor write FOnGetCaptionColor;

    property OnGetEditorClass: TPropertyInspectorOnGetEditorClass
      read FOnGetEditorClass write FOnGetEditorClass;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add(AObject: TPersistent);
    procedure Delete(AIndex: Integer);
    procedure Remove(AObject: TPersistent);
    procedure Clear;
    procedure UpdateItems;
    procedure AssignObjects(AObjects: TList);
    function IndexOf(AObject: TPersistent): Integer;
    procedure Modified;
    procedure RegisterPropEditor(ATypeInfo: PTypeInfo; AObjectClass: TClass;
      const APropName: string; AEditorClass: TPropertyEditorClass);
    procedure UnregisterPropEditor(ATypeInfo: PTypeInfo; AObjectClass: TClass;
      const APropName: string; AEditorClass: TPropertyEditorClass);

    property Items;

    property Objects[AIndex: Integer]: TPersistent
      read GetObjects write SetObjects;

    property ObjectCount: Integer
      read GetObjectCount;
  end;

  TPropertyInspector = class(TCustomPropertyInspector)
  public
    property Designer;

  published
    property PropKinds;
    property ComponentRefColor;
    property ComponentRefChildColor;
    property ExpandComponentRefs;
    property ReadOnly;
    property Splitter;
    property ValuesColor;
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property Visible;
    property OnGetComponent;
    property OnGetComponentNames;
    property OnGetComponentName;
    property OnFilterProp;
    property OnModified;
    property OnGetCaptionColor;
    property OnGetEditorClass;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  System.UITypes,

  DDuce.Components.PropertyInspector.CollectionEditor;

const
  SNull        = '(Null)';
  SString      = 'String';
  SUnknown     = '(Unknown)';
  SUnknownType = 'Unknown Type';
  SNone        = '(None)';

  ModalResults: array [mrNone .. mrYesToAll] of string = (
    'mrNone',
    'mrOk',
    'mrCancel',
    'mrAbort',
    'mrRetry',
    'mrIgnore',
    'mrYes',
    'mrNo',
    'mrClose',
    'mrHelp',
    'mrTryAgain',
    'mrContinue',
    'mrAll',
    'mrNoToAll',
    'mrYesToAll'
  );

  ShortCuts: array[0..108] of TShortCut = (
    scNone,
    Byte('A') or scCtrl,
    Byte('B') or scCtrl,
    Byte('C') or scCtrl,
    Byte('D') or scCtrl,
    Byte('E') or scCtrl,
    Byte('F') or scCtrl,
    Byte('G') or scCtrl,
    Byte('H') or scCtrl,
    Byte('I') or scCtrl,
    Byte('J') or scCtrl,
    Byte('K') or scCtrl,
    Byte('L') or scCtrl,
    Byte('M') or scCtrl,
    Byte('N') or scCtrl,
    Byte('O') or scCtrl,
    Byte('P') or scCtrl,
    Byte('Q') or scCtrl,
    Byte('R') or scCtrl,
    Byte('S') or scCtrl,
    Byte('T') or scCtrl,
    Byte('U') or scCtrl,
    Byte('V') or scCtrl,
    Byte('W') or scCtrl,
    Byte('X') or scCtrl,
    Byte('Y') or scCtrl,
    Byte('Z') or scCtrl,
    Byte('A') or scCtrl or scAlt,
    Byte('B') or scCtrl or scAlt,
    Byte('C') or scCtrl or scAlt,
    Byte('D') or scCtrl or scAlt,
    Byte('E') or scCtrl or scAlt,
    Byte('F') or scCtrl or scAlt,
    Byte('G') or scCtrl or scAlt,
    Byte('H') or scCtrl or scAlt,
    Byte('I') or scCtrl or scAlt,
    Byte('J') or scCtrl or scAlt,
    Byte('K') or scCtrl or scAlt,
    Byte('L') or scCtrl or scAlt,
    Byte('M') or scCtrl or scAlt,
    Byte('N') or scCtrl or scAlt,
    Byte('O') or scCtrl or scAlt,
    Byte('P') or scCtrl or scAlt,
    Byte('Q') or scCtrl or scAlt,
    Byte('R') or scCtrl or scAlt,
    Byte('S') or scCtrl or scAlt,
    Byte('T') or scCtrl or scAlt,
    Byte('U') or scCtrl or scAlt,
    Byte('V') or scCtrl or scAlt,
    Byte('W') or scCtrl or scAlt,
    Byte('X') or scCtrl or scAlt,
    Byte('Y') or scCtrl or scAlt,
    Byte('Z') or scCtrl or scAlt,
    VK_F1,
    VK_F2,
    VK_F3,
    VK_F4,
    VK_F5,
    VK_F6,
    VK_F7,
    VK_F8,
    VK_F9,
    VK_F10,
    VK_F11,
    VK_F12,
    VK_F1 or scCtrl,
    VK_F2 or scCtrl,
    VK_F3 or scCtrl,
    VK_F4 or scCtrl,
    VK_F5 or scCtrl,
    VK_F6 or scCtrl,
    VK_F7 or scCtrl,
    VK_F8 or scCtrl,
    VK_F9 or scCtrl,
    VK_F10 or scCtrl,
    VK_F11 or scCtrl,
    VK_F12 or scCtrl,
    VK_F1 or scShift,
    VK_F2 or scShift,
    VK_F3 or scShift,
    VK_F4 or scShift,
    VK_F5 or scShift,
    VK_F6 or scShift,
    VK_F7 or scShift,
    VK_F8 or scShift,
    VK_F9 or scShift,
    VK_F10 or scShift,
    VK_F11 or scShift,
    VK_F12 or scShift,
    VK_F1 or scShift or scCtrl,
    VK_F2 or scShift or scCtrl,
    VK_F3 or scShift or scCtrl,
    VK_F4 or scShift or scCtrl,
    VK_F5 or scShift or scCtrl,
    VK_F6 or scShift or scCtrl,
    VK_F7 or scShift or scCtrl,
    VK_F8 or scShift or scCtrl,
    VK_F9 or scShift or scCtrl,
    VK_F10 or scShift or scCtrl,
    VK_F11 or scShift or scCtrl,
    VK_F12 or scShift or scCtrl,
    VK_INSERT,
    VK_INSERT or scShift,
    VK_INSERT or scCtrl,
    VK_DELETE,
    VK_DELETE or scShift,
    VK_DELETE or scCtrl,
    VK_BACK or scAlt,
    VK_BACK or scShift or scAlt
  );

type
  TCustomListBoxAccess = class(TCustomListBox);

//function GetPropList(TypeInfo: PTypeInfo; TypeKinds: TTypeKinds;
//  PropList: PPropList; SortList: Boolean = False): Integer;
//var
//  I, Count: Integer;
//  //PropInfo: PPropInfo;
//  PropInfo:    PPropInfoEx;
//  TempList: PPropList;
//  Context : TRttiContext;
//  T       : TRttiType;
//  P       : TRttiProperty;
//begin
//  T := Context.GetType(TypeInfo);
//  ShowMessage(T.ToString);
//  Result := 0;
//  for P in T.GetProperties do
//  begin
//    if P.PropertyType.TypeKind in TypeKinds then
//    begin
//      if PropList <> nil then
//        PropList^[Result] := GetPropInfo(P.Handle, P.Name);
//        //PropInfo;
//      Inc(Result);
procedure ELGetObjectsProps(ADesigner: Pointer; AObjList: TList;
  AKinds: TTypeKinds; AOnlyNestable: Boolean;
  AGetEditorClassProc: TGetEditorClassProc; AResult: TList);

type
  TObjProps = record
    Props: PPropList;
    Count: Integer;
  end;

var
  PropLists                : array of TObjProps;
  Intersection             : array of array of PPropInfo;
  I, J, K, Index, ObjCount : Integer;
  EditorClass              : TPropertyEditorClass;
  Editor                   : TPropertyEditor;
  Attrs                    : TPropertyAttributes;
  Obj                      : TPersistent;

begin
  ObjCount := AObjList.Count;
    { Create prop lists }
  SetLength(PropLists, ObjCount);
  for I := 0 to ObjCount - 1 do
  begin
    Obj := AObjList[I];
    PropLists[I].Count := GetPropList(Obj.ClassInfo, AKinds, nil);
    GetMem(PropLists[I].Props, PropLists[I].Count * SizeOf(Pointer));
    try
      GetPropList(Obj.ClassInfo, AKinds, PropLists[I].Props);
    except
      FreeMem(PropLists[I].Props);
      raise;
    end;
  end;
  try
        { Initialize intersection }
    SetLength(Intersection, PropLists[0].Count);
    for I := 0 to PropLists[0].Count - 1 do
    begin
      SetLength(Intersection[I], ObjCount);
      Intersection[I][0] := PropLists[0].Props[I];
    end;
        { Intersect }
    for I := 1 to ObjCount - 1 do
      for J := High(Intersection) downto 0 do
      begin
        Index := -1;
        for K := 0 to PropLists[I].Count - 1 do
          if (PropLists[I].Props[K].PropType^ = Intersection[J][0]
            .PropType^) and
            SameText(string(PropLists[I].Props[K].Name),
            string(Intersection[J][0].Name)) then
          begin
            Index := K;
            Break;
          end;
        if Index <> -1 then
          Intersection[J][I] := PropLists[I].Props[K]
        else
        begin
          for K := J + 1 to High(Intersection) do
            Intersection[K - 1] := Intersection[K];
          SetLength(Intersection, Length(Intersection) - 1);
        end;
      end;
        { Create property editors }
    for I := 0 to High(Intersection) do
    begin
                { Determine editor class }
      EditorClass := AGetEditorClassProc(TPersistent(AObjList[0]),
        Intersection[I][0]);
      for J := 0 to ObjCount - 1 do
        if AGetEditorClassProc(TPersistent(AObjList[J]), Intersection[I][J])
          <> EditorClass then
        begin
          EditorClass := nil;
          Break;
        end;
                { Create editor }
      if EditorClass <> nil then
      begin
        Editor := EditorClass.Create(ADesigner, AObjList.Count);
        try
          for J := 0 to AObjList.Count - 1 do
            Editor.SetPropEntry(J, TPersistent(AObjList[J]),
              Intersection[I][J]);
          Attrs := Editor.GetAttrs;
          if ((ObjCount = 1) or (praMultiSelect in Attrs)) and
            (not AOnlyNestable or not(praNotNestable in Attrs)) then
            AResult.Add(Editor)
          else
          begin
            Editor.Free;
            Editor := nil;
          end;
        except
          Editor.Free;
          raise;
        end;
      end;
    end;
  finally
        { Free prop lists }
    for I := 0 to ObjCount - 1 do
      FreeMem(PropLists[I].Props);
  end;
end;

{ TBaseObjectList }

function TBaseObjectList.Add: Integer;
begin
  ValidateAddition;
  Result := FItems.Add(CreateItem);
  Added;
  if not FChangingCount then
    Change;
end;

constructor TBaseObjectList.Create;
begin
  FItems := TList.Create;
end;

function TBaseObjectList.CreateItem: TObject;
begin
  Result := nil;
end;

procedure TBaseObjectList.Delete(AIndex: Integer);
begin
  ValidateDeletion;
  TObject(FItems[AIndex]).Free;
  FItems.Delete(AIndex);
  Deleted;
  if not FChangingCount then
    Change;
end;

destructor TBaseObjectList.Destroy;
begin
  Clear;
  FItems.Free;
  inherited;
end;

function TBaseObjectList.DoFind(AData: Pointer;
  AItemByProc: TItemByProc): TObject;
var
  LI     : Integer;
  LResult: Boolean;
begin
  Result := nil;
  for LI := 0 to Count - 1 do
  begin
    LResult := False;
    AItemByProc(Items[LI], AData, LResult);
    if LResult then
    begin
      Result := Items[LI];
      Break;
    end;
  end;
end;

function TBaseObjectList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TBaseObjectList.GetItems(AIndex: Integer): TObject;
begin
  Result := FItems[AIndex];
end;

function TBaseObjectList.IndexOf(AItem: TObject): Integer;
begin
  Result := FItems.IndexOf(AItem);
end;

function TBaseObjectList.DoItemBy(AData: Pointer;
  AItemByProc: TItemByProc): TObject;
begin
  Result := DoFind(AData, AItemByProc);
  if Result = nil then
    raise EBaseObjectList.Create('Item not found');
end;

procedure TBaseObjectList.Remove(AItem: TObject);
var
  LI: Integer;
begin
  LI := FItems.IndexOf(AItem);
  if LI <> -1 then
    Delete(LI);
end;

procedure TBaseObjectList.SetCount(const Value: Integer);
begin
  FChangingCount := True;
  try
    if Value > Count then
      while Count < Value do
        Add
    else if Value < Count then
      while Count > Value do
        Delete(Count - 1);
  finally
    FChangingCount := False;
  end;
  Change;
end;

procedure TBaseObjectList.Clear;
begin
  Count := 0;
end;

function TBaseObjectList.DoSearch(AData: Pointer;
  AItemByProc: TItemByProc): TObject;
var
  LI: Integer;
begin
  Result := DoFind(AData, AItemByProc);
  if Result = nil then
    for LI := 0 to Count - 1 do
      if (Items[LI] <> nil) and (Items[LI] is TBaseObjectList) then
      begin
        Result := TBaseObjectList(Items[LI]).DoSearch(AData, AItemByProc);
        if Result <> nil then
          Break;
      end;
end;

procedure TBaseObjectList.ValidateAddition;
begin
  // Do nothing
end;

procedure TBaseObjectList.ValidateDeletion;
begin
  // Do nothing
end;

procedure TBaseObjectList.Change;
begin
  // Do nothing
end;

procedure TBaseObjectList.Added;
begin
  // Do nothing
end;

procedure TBaseObjectList.Deleted;
begin
  // Do nothing
end;

{ TMethodPropertyEditor }

function TMethodPropertyEditor.GetValue: string;
var
  M : TMethod;
begin
  with FPropList^[0] do
    M := GetMethodProp(Instance, PropInfo);
  Result := IntToStr(Integer(M.Code));
end;

procedure TMethodPropertyEditor.SetValue(const Value: string);
var
  M : TMethod;
begin
  inherited;
  with FPropList^[0] do
  begin
    M := GetMethodProp(Instance, PropInfo);
    M.Code := Pointer(StrToInt(Value));
    SetMethodProp(Instance, PropInfo, M);
  end;
end;

{ TCollectionPropertyEditor }

procedure TCollectionPropertyEditor.Edit;
var
  Collection : TCollection;
begin
  Collection := TCollection(GetOrdValue(0));
  ExecuteCollectionEditor(Collection);
  Modified;
end;

function TCollectionPropertyEditor.GetAttrs: TPropertyAttributes;
begin
  Result := [praDialog, praMultiSelect, praSubProperties];
end;

{ TCustomPropsPage }

constructor TCustomPropsPage.Create(AOwner: TComponent);
begin
  inherited;
  Width := 188;
  Height := 193;
  DefaultColWidth := 84;
  DefaultRowHeight := 16;
  ColCount := 2;
  RowCount := 0;
  FixedRows := 0;
  FixedCols := 1;
  Color := clBtnFace;
  Options := [goEditing, goAlwaysShowEditor, goThumbTracking];
  DesignOptionsBoost := [];
  FSaveCellExtents := False;
  ScrollBars := ssNone;
  DefaultDrawing := False;
  FItems := TPropsPageItems.Create(Self);
  FValuesColor := clNavy;
  FBitmap := Vcl.Graphics.TBitmap.Create;
  UpdatePattern;
  FCellBitmap := Vcl.Graphics.TBitmap.Create;
end;

function TCustomPropsPage.CreateEditor: TInplaceEdit;
begin
  Result := TPropsPageInplaceEdit.Create(Self);
end;

procedure TCustomPropsPage.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);

  procedure _DrawExpandButton(AX, AY: Integer; AExpanded: Boolean);
  begin
    with FCellBitmap.Canvas do
    begin
      Pen.Color := clBlack;
      Brush.Color := clWhite;
      Rectangle(AX, AY, AX + 9, AY + 9);
      Polyline([Point(AX + 2, AY + 4), Point(AX + 7, AY + 4)]);
      if not AExpanded then
        Polyline([Point(AX + 4, AY + 2), Point(AX + 4, AY + 7)]);
    end;
  end;

var
  S                      : string;
  ExpandButton, Expanded : Boolean;
  Ident                  : Integer;
  PI                     : TPropsPageItem;
  CaptionColor           : TColor;

begin
  if (ACol <> Col) or (ARow <> Row) or (InplaceEditor = nil) then
  begin
    FCellBitmap.Width := ARect.Right - ARect.Left;
    FCellBitmap.Height := ARect.Bottom - ARect.Top;

    with FCellBitmap.Canvas do
    begin
      PI := ItemByRow(ARow);
      if PI <> nil then
      begin
        if ACol = 0 then
          S := PI.Caption
        else
          S := PI.DisplayValue;
        ExpandButton := PI.CanExpand;
        Expanded := PI.Expanded;
        Ident := PI.Ident;
        CaptionColor := GetItemCaptionColor(PI);
      end
      else
      begin
        S := '';
        ExpandButton := False;
        Expanded := False;
        Ident := 0;
        CaptionColor := Font.Color;
      end;
      Brush.Color := Color;
      FCellBitmap.Canvas.Font := Self.Font;
      if ACol = 0 then
        Font.Color := CaptionColor
      else
        Font.Color := ValuesColor;
      TextRect(
        Rect(0, 0, FCellBitmap.Width, FCellBitmap.Height),
        1 + (12 + Ident) * Ord(ACol = 0),
        1,
        S
        );
      if ExpandButton and (ACol = 0) then
        _DrawExpandButton(2 + Ident, 3, Expanded);

      if ACol = 0 then
      begin
        { Splitter }
        Pen.Color := clBtnShadow;
        Polyline([Point(FCellBitmap.Width - 2, 0), Point(FCellBitmap.Width - 2,
          FCellBitmap.Height)]);
        Pen.Color := clBtnHighlight;
        Polyline([Point(FCellBitmap.Width - 1, 0), Point(FCellBitmap.Width - 1,
          FCellBitmap.Height)]);
      end;
      if ARow = Row - 1 then
      begin
        Pen.Color := cl3DDkShadow;
        Polyline([Point(0, FCellBitmap.Height - 2), Point(FCellBitmap.Width,
          FCellBitmap.Height - 2)]);
        Pen.Color := clBtnShadow;
        Polyline([Point(0, FCellBitmap.Height - 1), Point(FCellBitmap.Width,
          FCellBitmap.Height - 1)]);
      end
      else
        if ARow = Row then
      begin
        if ACol = 0 then
        begin
          Pen.Color := cl3DDkShadow;
          Polyline([Point(0, 0), Point(0, FCellBitmap.Height)]);
          Pen.Color := clBtnShadow;
          Polyline([Point(1, 0), Point(1, FCellBitmap.Height)]);
        end;
        Pen.Color := clBtnHighlight;
        Polyline([Point(0, FCellBitmap.Height - 2), Point(FCellBitmap.Width,
          FCellBitmap.Height - 2)]);
        Pen.Color := cl3DLight;
        Polyline([Point(0, FCellBitmap.Height - 1), Point(FCellBitmap.Width,
          FCellBitmap.Height - 1)]);
      end
      else
      begin
        { Row line }
        if FBitmapBkColor <> Color then
          UpdatePattern;
        Winapi.Windows.FillRect(Handle, Rect(0, FCellBitmap.Height - 1,
          FCellBitmap.Width, FCellBitmap.Height), FBrush);
      end;
    end;
    Canvas.Draw(ARect.Left, ARect.Top, FCellBitmap);
  end
  else
    with Canvas do
    begin
      Pen.Color := clBtnHighlight;
      Polyline([Point(ARect.Left, ARect.Bottom - 2), Point(ARect.Right,
        ARect.Bottom - 2)]);
      Pen.Color := cl3DLight;
      Polyline([Point(ARect.Left, ARect.Bottom - 1), Point(ARect.Right,
        ARect.Bottom - 1)]);
    end;
end;

function TCustomPropsPage.SelectCell(ACol, ARow: Integer): Boolean;
begin
  UpdateData(FOldRow);
  Result := inherited SelectCell(ACol, ARow);
  InvalidateRow(FOldRow - 1);
  InvalidateRow(FOldRow);
  InvalidateRow(FOldRow + 1);
  InvalidateRow(ARow - 1);
  InvalidateRow(ARow);
  InvalidateRow(ARow + 1);
  FOldRow := ARow;
end;

procedure TCustomPropsPage.Paint;
begin
  inherited;
  DrawCell(Col, Row, CellRect(Col, Row), []);
end;

function TCustomPropsPage.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := True;
  if TopRow < RowCount - VisibleRowCount then
    TopRow := TopRow + 1;
end;

function TCustomPropsPage.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := True;
  if TopRow > FixedRows then
    TopRow := TopRow - 1;
end;

procedure TCustomPropsPage.CreateHandle;
begin
  inherited;
  UpdateScrollBar;
  ShowEditor;
  UpdateColWidths;
end;

function TCustomPropsPage.GetEditStyle(ACol, ARow: Integer): TEditStyle;
var
  LItem: TPropsPageItem;
begin
  LItem := ItemByRow(ARow);
  if LItem <> nil then
    Result := LItem.EditStyle
  else
    Result := esSimple;
end;

procedure TCustomPropsPage.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  LGridCoord: TGridCoord;
begin
  if not (ppsMovingSplitter in FState) then
  begin
    inherited;
    if MouseCapture then
    begin
      LGridCoord := MouseCoord(X, Y);
      if (LGridCoord.Y <> -1) then
        Row := LGridCoord.Y
      else
        if Y < 0 then
      begin
        if Row > 0 then
          Row := Row - 1;
      end
      else
      begin
        if Row < RowCount - 1 then
          Row := Row + 1;
      end
    end;
  end;

  if ppsMovingSplitter in FState then
    Splitter := X - FSplitterOffset;
end;

function TCustomPropsPage.IsOnSplitter(AX: Integer): Boolean;
begin
  Result := (AX >= ColWidths[0] - 4) and (AX <= ColWidths[0]);
end;

procedure TCustomPropsPage.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  LGridCoord : TGridCoord;
  LCellRect  : TRect;
  LItem      : TPropsPageItem;
begin
  if ssLeft in Shift then
  begin
    if IsOnSplitter(X) then
    begin
      Include(FState, ppsMovingSplitter);
      FSplitterOffset := X - ColWidths[0];
    end
    else
    begin
      inherited;
      InvalidateEditor;
      LGridCoord := MouseCoord(X, Y);
      if (LGridCoord.X = 0) and (LGridCoord.Y <> -1) then
      begin
        LCellRect := CellRect(LGridCoord.X, LGridCoord.Y);
        LItem := ItemByRow(LGridCoord.Y);
        if (LItem <> nil) and LItem.CanExpand and LItem.IsOnExpandButton(X) then
        begin
          Row := LGridCoord.Y;
          if LItem.Expanded then
            LItem.Collapse
          else
            LItem.Expand;
        end
        else
          if (LGridCoord.Y < TopRow + VisibleRowCount) then
          Row := LGridCoord.Y;
      end;
    end;
  end;
end;

procedure TCustomPropsPage.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  LForm: TCustomForm;
begin
  if ppsMovingSplitter in FState then
    if csDesigning in ComponentState then
    begin
      LForm := GetParentForm(Self);
      if (LForm <> nil) and (LForm.Designer <> nil) then
        LForm.Designer.Modified;
    end
    else
      inherited;
  Exclude(FState, ppsMovingSplitter);
end;

procedure TCustomPropsPage.UpdateColWidths;
begin
  ColWidths[1] := Width - ColWidths[0];
end;

procedure TCustomPropsPage.UpdateScrollBar;
var
  LSI: TScrollInfo;
begin
  if HandleAllocated then
  begin
    LSI.cbSize := SizeOf(LSI);
    LSI.fMask := SIF_ALL;
    GetScrollInfo(Self.Handle, SB_VERT, LSI);
    LSI.nPage := VisibleRowCount;
    LSI.nMin := 0;
    LSI.nMax := RowCount - 1;
    LSI.nPos := TopRow;
    SetScrollInfo(Self.Handle, SB_VERT, LSI, True);
  end;
end;

procedure TCustomPropsPage.WMVScroll(var Message: TWMVScroll);
var
  LTopRow: Integer;
  LSI    : TScrollInfo;
begin
  LTopRow := TopRow;
  with Message do
    case ScrollCode of
      SB_LINEUP:
        LTopRow := LTopRow - 1;
      SB_LINEDOWN:
        LTopRow := LTopRow + 1;
      SB_PAGEUP:
        LTopRow := LTopRow - VisibleRowCount;
      SB_PAGEDOWN:
        LTopRow := LTopRow + VisibleRowCount;
      SB_THUMBPOSITION, SB_THUMBTRACK:
        begin
          LSI.cbSize := SizeOf(LSI);
          LSI.fMask := SIF_ALL;
          GetScrollInfo(Self.Handle, SB_VERT, LSI);
          LTopRow := LSI.nTrackPos;
        end;
      SB_BOTTOM:
        LTopRow := RowCount - 1;
      SB_TOP:
        LTopRow := 0;
    end;
  if LTopRow < 0 then
    LTopRow := 0;
  if LTopRow > RowCount - VisibleRowCount then
    LTopRow := RowCount - VisibleRowCount;
  TopRow := LTopRow;
  UpdateScrollBar;
  Message.Result := 0;
end;

procedure TCustomPropsPage.TopLeftChanged;
begin
  inherited;
  UpdateScrollBar;
end;

procedure TCustomPropsPage.SizeChanged(OldColCount, OldRowCount: Integer);
begin
  inherited;
  AdjustTopRow;
  UpdateScrollBar;
  ShowEditor;
end;

procedure TCustomPropsPage.WMSize(var Message: TWMSize);
begin
  inherited;
  AdjustTopRow;
  UpdateScrollBar;
  Splitter := Splitter;
  ShowEditor;
end;

procedure TCustomPropsPage.AdjustTopRow;
var
  I: Integer;
begin
  if HandleAllocated then
  begin
    I := ClientHeight div DefaultRowHeight;
    if RowCount - TopRow < I then
    begin
      I := RowCount - I;
      if I < 0 then
        I := 0;
      TopRow := I;
    end;
  end;
end;

destructor TCustomPropsPage.Destroy;
begin
  Include(FState, ppsDestroying);
  FItems.Free;
  FBitmap.Free;
  FCellBitmap.Free;
  if FBrush <> 0 then
    DeleteObject(FBrush);
  inherited;
end;

procedure TCustomPropsPage.ItemsChange;

  procedure _FillRows(AList: TBaseObjectList);
  var
    I: Integer;
  begin
    for I := 0 to AList.Count - 1 do
    begin
      SetLength(FRows, Length(FRows) + 1);
      FRows[ High(FRows)] := TPropsPageItem(AList[I]);
      TPropsPageItem(AList[I]).FRow := High(FRows);
      if TPropsPageItem(AList[I]).Expanded then
        _FillRows(TBaseObjectList(AList[I]));
    end;
  end;

var
  I           : Integer;
  LActiveItem : TPropsPageItem;

begin
  if (FUpdateCount <= 0) and not(ppsDestroying in FState) then
  begin
    LActiveItem := ActiveItem;
    for I := 0 to High(FRows) do
      if FRows[I] <> nil then
        FRows[I].FRow := -1;
    SetLength(FRows, 0);
    _FillRows(Items);
    RowCount := Length(FRows);
    while LActiveItem <> nil do
    begin
      if LActiveItem.FRow <> -1 then
      begin
        if Row <> LActiveItem.FRow then
          Row := LActiveItem.FRow;
        Break;
      end;
      LActiveItem := LActiveItem.Parent;
    end;
    Invalidate;
    LActiveItem := ActiveItem;
    if InplaceEditor <> nil then
    begin
      if LActiveItem <> nil then
      begin
        if (TPropsPageInplaceEdit(InplaceEditor).ReadOnlyStyle <>
          LActiveItem.ReadOnly) or
          (TPropsPageInplaceEdit(InplaceEditor).EditStyle <>
          LActiveItem.EditStyle) then
          InvalidateEditor;
        InplaceEditor.Text := LActiveItem.DisplayValue;
      end
      else
      begin
        if not TPropsPageInplaceEdit(InplaceEditor).ReadOnlyStyle or
          (TPropsPageInplaceEdit(InplaceEditor).EditStyle <> esSimple) then
          InvalidateEditor;
        InplaceEditor.Text := '';
      end;
      FEditText := InplaceEditor.Text;
    end;
    Update;
  end
  else
    Include(FState, ppsChanged);
end;

function TCustomPropsPage.GetActiveItem: TPropsPageItem;
var
  LItem: TPropsPageItem;
begin
  LItem := ItemByRow(Row);
  if LItem <> nil then
    Result := LItem
  else
    Result := nil;
end;

procedure TCustomPropsPage.WMLButtonDblClk(var Message: TWMLButtonDblClk);
var
  LGridCoord: TGridCoord;
  LCellRect : TRect;
  LItem     : TPropsPageItem;
begin
  inherited;
  LGridCoord := MouseCoord(Message.XPos, Message.YPos);
  if (LGridCoord.X = 0) and (LGridCoord.Y <> -1) then
  begin
    LCellRect := CellRect(LGridCoord.X, LGridCoord.Y);
    LItem := ItemByRow(LGridCoord.Y);
    if (LItem <> nil) and not LItem.IsOnExpandButton(Message.XPos) then
      if LItem.Expanded then
        LItem.Collapse
      else
        LItem.Expand;
  end;
end;

function TCustomPropsPage.CreateItem(AParent: TPropsPageItem)
  : TPropsPageItem;
begin
  Result := TPropsPageItem.Create(Self, AParent);
end;

function TCustomPropsPage.CanEditModify: Boolean;
begin
  Result := (ActiveItem <> nil) and not ActiveItem.ReadOnly;
end;

function TCustomPropsPage.GetEditText(ACol, ARow: Integer): string;
var
  LItem: TPropsPageItem;
begin
  LItem := ItemByRow(ARow);
  if (ACol = 1) and (LItem <> nil) then
    Result := LItem.DisplayValue;
  FEditText := Result;
end;

procedure TCustomPropsPage.SetEditText(ACol, ARow: Integer;
  const Value: string);
var
  LItem: TPropsPageItem;
begin
  if not(ppsUpdatingEditorContent in FState) then
  begin
    LItem := ItemByRow(ARow);
    if (ACol = 1) and (LItem <> nil) and LItem.AutoUpdate then
      LItem.DisplayValue := Value;
    FEditText := Value;
  end;
end;

function TCustomPropsPage.ItemByRow(ARow: Integer): TPropsPageItem;
begin
  if (ARow >= 0) and (ARow <= High(FRows)) then
    Result := FRows[ARow]
  else
    Result := nil;
end;

procedure TCustomPropsPage.UpdateData(ARow: Integer);
var
  LItem: TPropsPageItem;
begin
  LItem := ItemByRow(ARow);
  if LItem <> nil then
    try
      LItem.DisplayValue := FEditText;
      FEditText := LItem.DisplayValue;
    finally
      if (InplaceEditor <> nil) then
        TPropsPageInplaceEdit(InplaceEditor).UpdateContents;
    end;
end;

procedure TCustomPropsPage.CMExit(var Message: TMessage);
begin
  UpdateData(Row);
  inherited;
end;

procedure TCustomPropsPage.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TCustomPropsPage.EndUpdate;
begin
  Dec(FUpdateCount);
  if (FUpdateCount <= 0) and (ppsChanged in FState) then
  begin
    ItemsChange;
    Exclude(FState, ppsChanged);
  end;
end;

procedure TCustomPropsPage.ItemCollapsed(AItem: TPropsPageItem);
begin
    // Do nothing
end;

procedure TCustomPropsPage.ItemExpanded(AItem: TPropsPageItem);
begin
    // Do nothing
end;

procedure TCustomPropsPage.WMSetCursor(var Message: TWMSetCursor);
var
  LP: TPoint;
begin
  GetCursorPos(LP);
  LP := ScreenToClient(LP);
  if IsOnSplitter(LP.X) then
    Winapi.Windows.SetCursor(Screen.Cursors[crHSplit])
  else
    inherited;
end;

procedure TCustomPropsPage.CMDesignHitTest(var Msg: TCMDesignHitTest);
begin
  Msg.Result := Ord(IsOnSplitter(Msg.XPos) or (ppsMovingSplitter in FState));
end;

procedure TCustomPropsPage.SetSplitter(const Value: Integer);
var
  LNewVal: Integer;
begin
  LNewVal := Value;
  if LNewVal > Width - 40 then
    LNewVal := Width - 40;
  if LNewVal < 40 then
    LNewVal := 40;
  ColWidths[0] := LNewVal;
  UpdateColWidths;
end;

function TCustomPropsPage.GetSplitter: Integer;
begin
  Result := ColWidths[0];
end;

procedure TCustomPropsPage.SetValuesColor(const Value: TColor);
begin
  if FValuesColor <> Value then
  begin
    FValuesColor := Value;
    Invalidate;
  end;
end;

function TCustomPropsPage.GetItemCaptionColor(
  AItem: TPropsPageItem): TColor;
begin
  Result := Font.Color;
end;

procedure TCustomPropsPage.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Canvas.Font := Font;
  DefaultRowHeight := Canvas.TextHeight('Wg') + 3;
end;

procedure TCustomPropsPage.UpdatePattern;
var
  I: Integer;
begin
  FBitmapBkColor := Color;
  with FBitmap do
  begin
    Width := 8;
    Height := 1;
    Canvas.Brush.Color := FBitmapBkColor;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(Rect(0, 0, FBitmap.Width, FBitmap.Height));
    I := 0;
    while I < Width do
    begin
      Canvas.Pixels[I, 0] := clBtnShadow;
      Inc(I, 2);
    end;
  end;
  if FBrush <> 0 then
    DeleteObject(FBrush);
  FBrush := CreatePatternBrush(FBitmap.Handle);
end;

{ TPropsPageInplaceEdit }

procedure TPropsPageInplaceEdit.BoundsChanged;
begin
  inherited;
  if not FChangingBounds then
  begin
    FChangingBounds := True;
    try
      UpdateLoc(Rect(Left, Top, Left + Width, Top + Height - 2));
      SendMessage(Handle, EM_SETMARGINS, EC_RIGHTMARGIN,
        MakeLong(0, ButtonWidth * Ord(EditStyle <> esSimple) + 2));
    finally
      FChangingBounds := False;
    end;
  end;
end;

procedure TPropsPageInplaceEdit.CloseUp(Accept: Boolean);
begin
  inherited;
  if Accept then
    with TCustomPropsPage(Owner) do
    begin
      UpdateData(Row);
      SelectAll;
    end;
end;

constructor TPropsPageInplaceEdit.Create(AOwner: TComponent);
begin
  inherited;
  DropDownRows := 30;
  ButtonWidth  := 16;
end;

procedure TPropsPageInplaceEdit.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style and not ES_MULTILINE;
end;

procedure TPropsPageInplaceEdit.DblClick;
begin
  if TCustomPropsPage(Grid).ActiveItem <> nil then
    TCustomPropsPage(Grid).ActiveItem.EditDblClick;
end;

procedure TPropsPageInplaceEdit.DoEditButtonClick;
begin
  if TCustomPropsPage(Grid).ActiveItem <> nil then
    TCustomPropsPage(Grid).ActiveItem.EditButtonClick;
end;

procedure TPropsPageInplaceEdit.DoGetPickListItems;
begin
  if TCustomPropsPage(Grid).ActiveItem <> nil then
  begin
    PickList.Items.Clear;
    TCustomPropsPage(Grid).ActiveItem.GetEditPickList(PickList.Items);
    PickList.ItemIndex := PickList.Items.IndexOf(Text);
  end;
end;

procedure TPropsPageInplaceEdit.DropDown;
var
  LP                                 : TPoint;
  I, LY, LVisItemCount, LItemHW, LHW: Integer;
  LItem                              : TPropsPageItem;
begin
  LItem := TCustomPropsPage(Grid).ActiveItem;
  if not ListVisible and (LItem <> nil) then
  begin
    if LItem.OwnerDrawPickList then
    begin
      TCustomListBoxAccess(PickList).Style := lbOwnerDrawVariable;
      TCustomListBoxAccess(PickList).OnMeasureItem := PickListMeasureItem;
      TCustomListBoxAccess(PickList).OnDrawItem := PickListDrawItem;
    end
    else
    begin
      TCustomListBoxAccess(PickList).Style := lbStandard;
      TCustomListBoxAccess(PickList).OnMeasureItem := nil;
      TCustomListBoxAccess(PickList).OnDrawItem := nil;
    end;

    ActiveList.Width := Width;
    if ActiveList = PickList then
    begin
      { Get values }
      DoGetPickListItems;
      TCustomListBoxAccess(PickList).Color := Color;
      TCustomListBoxAccess(PickList).Font := Font;
                    { Calc initial visible item count }
      if (DropDownRows > 0) and (PickList.Items.Count >= DropDownRows) then
        LVisItemCount := DropDownRows
      else
        LVisItemCount := PickList.Items.Count;
      { Calc PickList height }
      if LItem.OwnerDrawPickList then
      begin
        LHW := 4;
        for I := 0 to LVisItemCount - 1 do
        begin
          LItemHW := TCustomListBoxAccess(PickList).ItemHeight;
          LItem.PickListMeasureHeight(PickList.Items[I],
            PickList.Canvas, LItemHW);
          Inc(LHW, LItemHW);
        end;
      end
      else
        LHW := LVisItemCount * TCustomListBoxAccess(PickList).ItemHeight + 4;
      if PickList.Items.Count > 0 then
        PickList.Height := LHW
      else
        PickList.Height := 20;
      { Set PickList selected item }
      if Text = '' then
        PickList.ItemIndex := -1
      else
        PickList.ItemIndex := PickList.Items.IndexOf(Text);
      { Calc PickList width }
      LHW := PickList.ClientWidth;
      for I := 0 to PickList.Items.Count - 1 do
      begin
        LItemHW := PickList.Canvas.TextWidth(PickList.Items[I]);
        if LItem.OwnerDrawPickList then
          LItem.PickListMeasureWidth(PickList.Items[I],
            PickList.Canvas, LItemHW);
        if LItemHW > LHW then
          LHW := LItemHW;
      end;
      PickList.ClientWidth := LHW;
    end;
    LP := Parent.ClientToScreen(Point(Left, Top));
    LY := LP.Y + Height;
    if LY + ActiveList.Height > Screen.Height then
      LY := LP.Y - ActiveList.Height;
    SetWindowPos(ActiveList.Handle, HWND_TOP, LP.X, LY, 0, 0,
      SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
    ListVisible := True;
    Invalidate;
    Winapi.Windows.SetFocus(Handle);
  end;
end;

procedure _KillMessage(AWnd: HWnd; AMsg: Integer);
var
  M: TMsg;
begin
  M.Message := 0;
  if PeekMessage(M, AWnd, AMsg, AMsg, pm_Remove) and
    (M.Message = WM_QUIT) then
    PostQuitMessage(M.wparam);
end;

procedure TPropsPageInplaceEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
      begin
        TCustomPropsPage(Grid).UpdateData(TCustomPropsPage(Grid).Row);
        if Shift = [ssCtrl] then
        begin
          _KillMessage(Handle, WM_CHAR);
          DblClick;
          SelectAll;
        end;
        Key := 0;
      end;
    VK_ESCAPE:
      if TCustomPropsPage(Grid).ActiveItem <> nil then
      begin
        Text := TCustomPropsPage(Grid).ActiveItem.DisplayValue;
        SelectAll;
        Key := 0;
      end;
    VK_HOME:
      if (SelStart = 0) and (SelLength > 0) then
        SelLength := 0;
    VK_END:
      if (SelStart + SelLength = Length(Text)) and (SelLength > 0) then
        SelLength := 0;
  end;
  if Key <> 0 then
    inherited;
end;

procedure TPropsPageInplaceEdit.PickListDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  PI: TPropsPageItem;
begin
  PI := TCustomPropsPage(Grid).ActiveItem;
  if (PI <> nil) and PI.OwnerDrawPickList then
    PI.PickListDrawValue(PickList.Items[Index], PickList.Canvas, Rect,
      odSelected in State);
end;

procedure TPropsPageInplaceEdit.PickListMeasureItem(Control: TWinControl;
  Index: Integer; var Height: Integer);
var
  PI: TPropsPageItem;
begin
  PI := TCustomPropsPage(Grid).ActiveItem;
  if (PI <> nil) and PI.OwnerDrawPickList then
    PI.PickListMeasureHeight(PickList.Items[Index], PickList.Canvas, Height);
end;

procedure TPropsPageInplaceEdit.UpdateContents;
begin
  Include(TCustomPropsPage(Grid).FState, ppsUpdatingEditorContent);
  try
    inherited;
    if not EditCanModify then
    begin
      Color := TCustomPropsPage(Owner).Color;
      Font.Color := TCustomPropsPage(Owner).ValuesColor;
    end
    else
    begin
      Color := clWindow;
      Font.Color := clWindowText;
    end;
    FReadOnlyStyle := not EditCanModify;
  finally
    Exclude(TCustomPropsPage(Grid).FState, ppsUpdatingEditorContent);
  end;
end;

procedure TPropsPageInplaceEdit.WMLButtonDblClk(var Message: TWMMouse);
begin
  if OverButton(Point(Message.XPos, Message.YPos)) then
    PostMessage(Handle, WM_LBUTTONDOWN, TMessage(Message).wparam,
      TMessage(Message).LParam)
  else
    inherited;
  SelectAll;
end;

{ TPropsPageItems }

procedure TPropsPageItems.Change;
begin
  Owner.ItemsChange;
end;

constructor TPropsPageItems.Create(AOwner: TCustomPropsPage);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TPropsPageItems.CreateItem: TObject;
begin
  Result := FOwner.CreateItem(nil);
end;

function TPropsPageItems.GetItems(AIndex: Integer): TPropsPageItem;
begin
  Result := TPropsPageItem(inherited Items[AIndex]);
end;

{ TPropsPageItem }

function TPropsPageItem.CanExpand: Boolean;
begin
  Result := (Expandable = mieYes) or ((Expandable = mieAuto) and (Count > 0));
end;

procedure TPropsPageItem.Change;
begin
  Owner.ItemsChange;
end;

procedure TPropsPageItem.Collapse;
begin
  if FExpanded then
  begin
    Owner.BeginUpdate;
    try
      FExpanded := False;
      Owner.ItemCollapsed(Self);
      Change;
    finally
      Owner.EndUpdate;
    end;
  end;
end;

constructor TPropsPageItem.Create(AOwner: TCustomPropsPage;
  AParent: TPropsPageItem);
begin
  inherited Create;
  FOwner := AOwner;
  FParent := AParent;
  FRow := -1;
end;

function TPropsPageItem.CreateItem: TObject;
begin
  Result := FOwner.CreateItem(Self);
end;

procedure TPropsPageItem.Deleted;
begin
  FExpanded := FExpanded and CanExpand;
end;

procedure TPropsPageItem.Expand;
begin
  if not FExpanded and CanExpand then
  begin
    Owner.BeginUpdate;
    try
      FExpanded := True;
      Owner.ItemExpanded(Self);
      Change;
    finally
      Owner.EndUpdate;
    end;
  end;
end;

function TPropsPageItem.GetItems(AIndex: Integer): TPropsPageItem;
begin
  Result := TPropsPageItem(inherited Items[AIndex]);
end;

function TPropsPageItem.GetLevel: Integer;
var
  LParent: TPropsPageItem;
begin
  Result := 0;
  LParent := Parent;
  while LParent <> nil do
  begin
    Inc(Result);
    LParent := LParent.Parent;
  end;
end;

procedure TPropsPageItem.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Change;
  end;
end;

procedure TPropsPageItem.SetExpandable(const Value
  : TPropsPageItemExpandable);
begin
  if FExpandable <> Value then
  begin
    FExpandable := Value;
    FExpanded := FExpanded and CanExpand;
    Change;
  end;
end;

procedure TPropsPageItem.SetDisplayValue(const Value: string);
begin
  if FDisplayValue <> Value then
  begin
    FDisplayValue := Value;
    Change;
  end;
end;

function TPropsPageItem.Ident: Integer;
begin
  Result := Level * 10;
end;

function TPropsPageItem.IsOnExpandButton(AX: Integer): Boolean;
begin
  Result := (AX >= Ident) and (AX <= Ident + 13);
end;

procedure TPropsPageItem.SetEditStyle(const Value: TEditStyle);
begin
  if FEditStyle <> Value then
  begin
    FEditStyle := Value;
    Change;
  end;
end;

destructor TPropsPageItem.Destroy;
begin
  if FRow <> -1 then
  begin
    FOwner.FRows[FRow] := nil;
    FRow := -1;
  end;
  inherited;
end;

procedure TPropsPageItem.SetReadOnly(const Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    Change;
  end;
end;

function TPropsPageItem.GetDisplayValue: string;
begin
  Result := FDisplayValue;
end;

procedure TPropsPageItem.EditButtonClick;
begin
    // Do nothing
end;

procedure TPropsPageItem.GetEditPickList(APickList: TStrings);
begin
    // Do nothing
end;

procedure TPropsPageItem.EditDblClick;
begin
    // Do nothing
end;

procedure TPropsPageItem.SetAutoUpdate(const Value: Boolean);
begin
  if FAutoUpdate <> Value then
  begin
    FAutoUpdate := Value;
    Change;
  end;
end;

procedure TPropsPageItem.SetOwnerDrawPickList(const Value: Boolean);
begin
  if FOwnerDrawPickList <> Value then
  begin
    FOwnerDrawPickList := Value;
    Change;
  end;
end;

procedure TPropsPageItem.PickListDrawValue(const AValue: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
begin
    // Do nothing
end;

procedure TPropsPageItem.PickListMeasureHeight(const AValue: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin
    // Do nothing
end;

procedure TPropsPageItem.PickListMeasureWidth(const AValue: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
    // Do nothing
end;

{ TPropertyInspectorItem }

procedure TPropertyInspectorItem.DoGetPickList(AResult: TStrings);
var
  SL: TStringList;
begin
  if FEditor <> nil then
  begin
    SL := TStringList.Create;
    try
      FEditor.GetValues(SL);
      if praSortList in FEditor.GetAttrs then
        SL.Sort;
      AResult.Assign(SL);
    finally
      SL.Free;
    end;
  end;
end;

destructor TPropertyInspectorItem.Destroy;
begin
  if Assigned(FEditor) then
    FEditor.Free;
  inherited;
end;

procedure TPropertyInspectorItem.EditButtonClick;
begin
  if Assigned(FEditor) and not TCustomPropertyInspector(Owner).ReadOnly then
    FEditor.Edit;
end;

procedure TPropertyInspectorItem.EditDblClick;
var
  LAttrs : TPropertyAttributes;
  LValues: TStringList;
  LIndex : Integer;
begin
  if (FEditor <> nil) and not TCustomPropertyInspector(Owner).ReadOnly then
  begin
    LAttrs := FEditor.GetAttrs;
    if (praValueList in LAttrs) and not(praDialog in LAttrs) then
    begin
      LValues := TStringList.Create;
      try
        DoGetPickList(LValues);
        if LValues.Count > 0 then
        begin
          LIndex := LValues.IndexOf(DisplayValue) + 1;
          if LIndex > LValues.Count - 1 then
            LIndex := 0;
          DisplayValue := LValues[LIndex];
        end;
      finally
        LValues.Free;
      end;
    end
    else
      if praDialog in LAttrs then
        EditButtonClick;
  end;
end;

procedure TPropertyInspectorItem.EditorGetComponent(Sender: TObject;
  const AComponentName: string; var AComponent: TComponent);
begin
  TCustomPropertyInspector(Owner).GetComponent(AComponentName, AComponent);
end;

procedure TPropertyInspectorItem.EditorGetComponentName(Sender: TObject;
  AComponent: TComponent; var AName: string);
begin
  TCustomPropertyInspector(Owner).GetComponentName(AComponent, AName);
end;

procedure TPropertyInspectorItem.EditorGetComponentNames(Sender: TObject;
  AClass: TComponentClass; AResult: TStrings);
begin
  TCustomPropertyInspector(Owner).GetComponentNames(AClass, AResult);
end;

procedure TPropertyInspectorItem.EditorModified(Sender: TObject);
begin
  TCustomPropertyInspector(Owner).InternalModified;
end;

function TPropertyInspectorItem.GetDisplayValue: string;
begin
  Result := FDisplayValue;
end;

procedure TPropertyInspectorItem.GetEditPickList(APickList: TStrings);
begin
  DoGetPickList(APickList);
end;

procedure TPropertyInspectorItem.PickListDrawValue(const AValue: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
begin
  if FEditor <> nil then
    FEditor.ValuesDrawValue(AValue, ACanvas, ARect, ASelected);
end;

procedure TPropertyInspectorItem.PickListMeasureHeight(
  const AValue: string; ACanvas: TCanvas; var AHeight: Integer);
begin
  if FEditor <> nil then
    FEditor.ValuesMeasureHeight(AValue, ACanvas, AHeight);
end;

procedure TPropertyInspectorItem.PickListMeasureWidth(const AValue: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
  if FEditor <> nil then
    FEditor.ValuesMeasureWidth(AValue, ACanvas, AWidth);
end;

procedure TPropertyInspectorItem.SetDisplayValue(const Value: string);
var
  OldValue: string;
begin
  if Value <> FDisplayValue then
  begin
    OldValue := FDisplayValue;
    TPropertyInspector(Owner).FObjectsLocked := True;
    try
      FEditor.Value := Value; // May raise an exception
    finally
      TPropertyInspector(Owner).FObjectsLocked := False;
    end;
    FDisplayValue := FEditor.Value; // FEditor.Value may be not equal with Value
    if OldValue <> FDisplayValue then
    begin
      Owner.BeginUpdate;
      try
        Change;
        if Expanded and (praVolatileSubProperties in FEditor.GetAttrs) then
        begin
          Collapse;
          Expand;
        end;
      finally
        Owner.EndUpdate;
      end;
    end;
  end;
end;

procedure TPropertyInspectorItem.SetEditor(const Value: TPropertyEditor);
begin
  FEditor := Value;
  FEditor.OnModified          := EditorModified;
  FEditor.OnGetComponent      := EditorGetComponent;
  FEditor.OnGetComponentNames := EditorGetComponentNames;
  FEditor.OnGetComponentName  := EditorGetComponentName;
  UpdateParams;
end;

procedure TPropertyInspectorItem.UpdateParams;

const
  LExpandables: array[Boolean] of TPropsPageItemExpandable = (mieNo, mieYes);

var
  LPropAttrs: TPropertyAttributes;
  LStr      : string;

begin
  if FEditor <> nil then
  begin
    Owner.BeginUpdate;
    try
      Caption := FEditor.PropName;
      LPropAttrs := FEditor.GetAttrs;
      if (praValueList in LPropAttrs)
        and not TCustomPropertyInspector(Owner).ReadOnly then
        EditStyle := esPickList
      else
        if (praDialog in LPropAttrs)
          and not TCustomPropertyInspector(Owner).ReadOnly then
        EditStyle := esEllipsis
      else
        EditStyle := esSimple;
      Expandable := LExpandables[
        (praSubProperties in LPropAttrs)
        and not((praComponentRef in LPropAttrs)
        and not TCustomPropertyInspector(Owner).ExpandComponentRefs)];
      ReadOnly := (praReadOnly in LPropAttrs) or
        TCustomPropertyInspector(Owner).ReadOnly;
      AutoUpdate := praAutoUpdate in LPropAttrs;
      OwnerDrawPickList := praOwnerDrawValues in LPropAttrs;
      LStr := FEditor.Value;
      if FDisplayValue <> LStr then
      begin
        FDisplayValue := LStr;
        Change;
      end;
    finally
      Owner.EndUpdate;
    end;
  end;
end;

{ TCustomPropertyInspector }

procedure TCustomPropertyInspector.Add(AObject: TPersistent);
begin
  CheckObjectsLock;
  FObjects.Add(AObject);
  Change;
end;

constructor TCustomPropertyInspector.Create(AOwner: TComponent);
begin
  inherited;
  FObjects := TList.Create;
  FPropKinds := [pkProperties];
  FComponentRefColor := clMaroon;
  FComponentRefChildColor := clGreen;
  FExpandComponentRefs := True;
end;

function TCustomPropertyInspector.CreateItem(
  AParent: TPropsPageItem): TPropsPageItem;
begin
  Result := TPropertyInspectorItem.Create(Self, AParent);
end;

procedure TCustomPropertyInspector.Delete(AIndex: Integer);
begin
  CheckObjectsLock;
  FObjects.Delete(AIndex);
  Change;
end;

destructor TCustomPropertyInspector.Destroy;
begin
  FObjects.Free;
  inherited;
end;

function TCustomPropertyInspector.GetObjectCount: Integer;
begin
  Result := FObjects.Count;
end;

function TCustomPropertyInspector.GetObjects(AIndex: Integer): TPersistent;
begin
  Result := FObjects[AIndex];
end;

function TCustomPropertyInspector.IndexOf(AObject: TPersistent): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FObjects.Count - 1 do
    if FObjects[I] = AObject then
    begin
      Result := I;
      Break;
    end;
end;

procedure TCustomPropertyInspector.Change;
var
  Editors: TList;
  I      : Integer;
begin
  BeginUpdate;
  try
    Items.Clear;
    if FObjects.Count > 0 then
    begin
      Editors := TList.Create;
      try
        ELGetObjectsProps(FDesigner, FObjects, tkAny, False, GetEditorClass,
          Editors);
        Items.Count := Editors.Count;
        for I := 0 to Editors.Count - 1 do
          TPropertyInspectorItem(Items[I]).Editor := Editors[I];
      finally
        Editors.Free;
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TCustomPropertyInspector.Remove(AObject: TPersistent);
var
  I: Integer;
begin
  I := IndexOf(AObject);
  if I <> -1 then
    Delete(I);
end;

procedure TCustomPropertyInspector.SetObjects(AIndex: Integer;
  const Value: TPersistent);
begin
  FObjects[AIndex] := Value;
  Change;
end;

procedure TCustomPropertyInspector.Modified;
begin
  UpdateItems;
end;

function TCustomPropertyInspector.GetEditorClass(AInstance: TPersistent;
  APropInfo: PPropInfo): TPropertyEditorClass;
var
  I         : Integer;
  Best      : Integer;
  TI        : PTypeInfo;
  C         : TClass;
  FilterRes : Boolean;
begin
  if Assigned(OnGetEditorClass) then
  begin
    Result := nil;
    OnGetEditorClass(Self, AInstance, APropInfo, Result);
    if Result <> nil then
      Exit;
  end;

  TI := APropInfo.PropType^;
  C := AInstance.ClassType;

  if not(((pkProperties in PropKinds) and (TI.Kind in tkProperties)) or
    ((pkEvents in PropKinds) and (TI.Kind in tkMethods))) or
    not Assigned(APropInfo.GetProc) or
    (not(pkReadOnly in PropKinds) and not Assigned(APropInfo.SetProc) and
    (APropInfo.PropType^.Kind <> tkClass)) then
  begin
    Result := nil;
    Exit;
  end;

  FilterRes := True;
  FilterProp(AInstance, APropInfo, FilterRes);
  if not FilterRes then
  begin
    Result := nil;
    Exit;
  end;

  Best := -1;
  for I := High(FEditors) downto 0 do
    if (TI = FEditors[I].TypeInfo) or
      ((TI.Kind = tkClass) and (FEditors[I].TypeInfo.Kind = tkClass) and
      GetTypeData(TI).ClassType.InheritsFrom
      (GetTypeData(FEditors[I].TypeInfo).ClassType)) then
      if ((FEditors[I].ObjectClass = nil) or
        (C.InheritsFrom(FEditors[I].ObjectClass))) and
        ((FEditors[I].PropName = '') or SameText(FEditors[I].PropName,
        string(APropInfo.Name))) then
        if (Best = -1) or ((FEditors[Best].ObjectClass = nil) and
          (FEditors[I].ObjectClass <> nil)) or
          ((FEditors[Best].PropName = '') and (FEditors[I].PropName <> '')) or
          ((FEditors[Best].TypeInfo <> TI) and
          (FEditors[I].TypeInfo = TI)) or
          ((FEditors[Best].TypeInfo <> FEditors[I].TypeInfo) and
          (FEditors[Best].TypeInfo.Kind = tkClass) and
          (FEditors[I].TypeInfo.Kind = tkClass) and
          (GetTypeData(FEditors[I].TypeInfo).ClassType.InheritsFrom
          (GetTypeData(FEditors[Best].TypeInfo).ClassType))) then
        begin
          Best := I;
          Break;
        end;
  if Best <> -1 then
    Result := FEditors[Best].EditorClass
  else
    Result := nil;

  if not Assigned(Result) then
  begin
    if TI = TypeInfo(TComponentName) then
      Result := TComponentNamePropertyEditor
    else if TI = TypeInfo(TDate) then
      Result := TDatePropertyEditor
    else if TI = TypeInfo(TTime) then
      Result := TTimePropertyEditor
    else if TI = TypeInfo(TDateTime) then
      Result := TDateTimePropertyEditor
    else if TI = TypeInfo(TCaption) then
      Result := TCaptionPropertyEditor
    else if TI = TypeInfo(TColor) then
      Result := TColorPropertyEditor
    else if TI = TypeInfo(TCursor) then
      Result := TCursorPropertyEditor
    else if TI = TypeInfo(TFontCharset) then
      Result := TFontCharsetPropertyEditor
    else if TI = TypeInfo(TFontName) then
      Result := TFontNamePropertyEditor
    else if TI = TypeInfo(TImeName) then
      Result := TImeNamePropertyEditor
    else if (TI = TypeInfo(TFont)) or
      ((TI.Kind = tkClass) and GetTypeData(TI).ClassType.InheritsFrom(TFont)) then
      Result := TFontPropertyEditor
    else if TI = TypeInfo(TModalResult) then
      Result := TModalResultPropertyEditor
    else if TI = TypeInfo(TPenStyle) then
      Result := TPenStylePropertyEditor
    else if TI = TypeInfo(TBrushStyle) then
      Result := TBrushStylePropertyEditor
    else if TI = TypeInfo(TTabOrder) then
      Result := TTabOrderPropertyEditor
    else if TI = TypeInfo(TShortCut) then
      Result := TShortCutPropertyEditor
    else if (TI = TypeInfo(TStrings)) or ((TI.Kind = tkClass) and
      GetTypeData(TI).ClassType.InheritsFrom(TStrings)) then
      Result := TStringsPropertyEditor
    else if (TI = TypeInfo(TCollection)) or ((TI.Kind = tkClass) and
      GetTypeData(TI).ClassType.InheritsFrom(TCollection)) then
      Result := TCollectionPropertyEditor;
  end;

  if not Assigned(Result) then
  begin
    case TI.Kind of
      tkInteger:
        Result := TIntegerPropertyEditor;
      tkChar:
        Result := TCharPropertyEditor;
      tkEnumeration:
        Result := TEnumPropertyEditor;
      tkFloat:
        Result := TFloatPropertyEditor;
      tkString,
        tkUString,
        tkLString,
        tkWString:
        Result := TStringPropertyEditor;
      tkSet:
        Result := TSetPropertyEditor;
      tkClass:
        if (TI = TypeInfo(TComponent)) or
          GetTypeData(TI).ClassType.InheritsFrom(TComponent) then
          Result := TComponentPropertyEditor
        else
          Result := TClassPropertyEditor;
      tkVariant:
        Result := TVariantPropertyEditor;
      tkInt64:
        Result := TInt64PropertyEditor;
      tkMethod:
        Result := TMethodPropertyEditor;
    else
      Result := TPropertyEditor;
    end;
  end;
end;

procedure TCustomPropertyInspector.ItemCollapsed(AItem: TPropsPageItem);
begin
  BeginUpdate;
  try
    AItem.Clear;
  finally
    EndUpdate;
  end;
end;

procedure TCustomPropertyInspector.ItemExpanded(AItem: TPropsPageItem);
var
  LEditor  : TPropertyEditor;
  LSubProps: TList;
  I       : Integer;
begin
  LEditor := TPropertyInspectorItem(AItem).Editor;
  if LEditor <> nil then
  begin
    LSubProps := TList.Create;
    try
      if not((praComponentRef in LEditor.GetAttrs) and
        not ExpandComponentRefs) then
      begin
        LEditor.GetSubProps(GetEditorClass, LSubProps);
        BeginUpdate;
        try
          for I := 0 to LSubProps.Count - 1 do
            TPropertyInspectorItem(AItem[AItem.Add]).Editor := LSubProps[I];
        finally
          EndUpdate;
        end;
      end;
    finally
      LSubProps.Free;
    end;
  end;
end;

procedure TCustomPropertyInspector.Clear;
begin
  CheckObjectsLock;
  FObjects.Clear;
  Change;
end;

procedure TCustomPropertyInspector.RegisterPropEditor(ATypeInfo: PTypeInfo;
  AObjectClass: TClass; const APropName: string;
  AEditorClass: TPropertyEditorClass);
var
  I: Integer;
begin
  I := IndexOfEditor(ATypeInfo, AObjectClass, APropName, AEditorClass);
  if I = -1 then
  begin
    SetLength(FEditors, Length(FEditors) + 1);
    I := High(FEditors);
  end;
  with FEditors[I] do
  begin
    TypeInfo := ATypeInfo;
    ObjectClass := AObjectClass;
    PropName := APropName;
    EditorClass := AEditorClass;
  end;
  Change;
end;

procedure TCustomPropertyInspector.UnregisterPropEditor(ATypeInfo: PTypeInfo;
  AObjectClass: TClass; const APropName: string;
  AEditorClass: TPropertyEditorClass);
var
  I, J: Integer;
begin
  I := IndexOfEditor(ATypeInfo, AObjectClass, APropName, AEditorClass);
  if I <> -1 then
  begin
    for J := I + 1 to High(FEditors) do
      FEditors[J - 1] := FEditors[J];
    SetLength(FEditors, Length(FEditors) - 1);
  end;
  Change;
end;

function TCustomPropertyInspector.IndexOfEditor(ATypeInfo: PTypeInfo;
  AObjectClass: TClass; const APropName: string;
  AEditorClass: TPropertyEditorClass): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(FEditors) do
    if (FEditors[I].TypeInfo = ATypeInfo) and
      (FEditors[I].ObjectClass = AObjectClass) and
      SameText(FEditors[I].PropName, APropName) and
      (FEditors[I].EditorClass = AEditorClass) then
    begin
      Result := I;
      Break;
    end;
end;

procedure TCustomPropertyInspector.GetComponent(const AComponentName: string;
  var AComponent: TComponent);
begin
  if Assigned(OnGetComponent) then
    OnGetComponent(Self, AComponentName, AComponent);
end;

procedure TCustomPropertyInspector.GetComponentNames(AClass: TComponentClass;
  AResult: TStrings);
begin
  if Assigned(OnGetComponentNames) then
    OnGetComponentNames(Self, AClass, AResult);
end;

procedure TCustomPropertyInspector.GetComponentName(AComponent: TComponent;
  var AName: string);
begin
  if Assigned(OnGetComponentName) then
    OnGetComponentName(Self, AComponent, AName);
end;

procedure TCustomPropertyInspector.SetPropKinds
  (const Value: TPropertyInspectorPropKinds);
begin
  if FPropKinds <> Value then
  begin
    FPropKinds := Value;
    Change;
  end;
end;

procedure TCustomPropertyInspector.FilterProp(AInstance: TPersistent;
  APropInfo: PPropInfo; var AIncludeProp: Boolean);
begin
  if Assigned(OnFilterProp) then
    OnFilterProp(Self, AInstance, APropInfo, AIncludeProp);
end;

function TCustomPropertyInspector.GetItemCaptionColor(
  AItem: TPropsPageItem): TColor;
begin
  if (TPropertyInspectorItem(AItem).FEditor <> nil) and
    (praComponentRef in TPropertyInspectorItem(AItem).FEditor.GetAttrs) then
    Result := ComponentRefColor
  else
    if (AItem.Parent <> nil) and (TPropertyInspectorItem(AItem.Parent).FEditor
    <> nil) and
    (praComponentRef in TPropertyInspectorItem(AItem.Parent)
    .FEditor.GetAttrs) then
    Result := ComponentRefChildColor
  else
    Result := inherited GetItemCaptionColor(AItem);

  if (TPropertyInspectorItem(AItem).FEditor <> nil) then
    GetCaptionColor(TPropertyInspectorItem(AItem).FEditor.PropTypeInfo,
      TPropertyInspectorItem(AItem).FEditor.PropName, Result);
end;

procedure TCustomPropertyInspector.SetComponentRefColor(
  const Value: TColor);
begin
  if FComponentRefColor <> Value then
  begin
    FComponentRefColor := Value;
    Invalidate;
  end;
end;

procedure TCustomPropertyInspector.SetComponentRefChildColor(
  const Value: TColor);
begin
  if FComponentRefChildColor <> Value then
  begin
    FComponentRefChildColor := Value;
    Invalidate;
  end;
end;

procedure TCustomPropertyInspector.SetExpandComponentRefs(
  const Value: Boolean);
begin
  if FExpandComponentRefs <> Value then
  begin
    FExpandComponentRefs := Value;
    Change;
  end;
end;

procedure TCustomPropertyInspector.SetReadOnly(const Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    Change;
  end;
end;

procedure TCustomPropertyInspector.AssignObjects(AObjects: TList);
var
  I: Integer;
begin
  CheckObjectsLock;
  FObjects.Clear;
  for I := 0 to AObjects.Count - 1 do
    FObjects.Add(AObjects[I]);
  Change;
end;

procedure TCustomPropertyInspector.SetDesigner(const Value: Pointer);
begin
  if FDesigner <> Value then
  begin
    FDesigner := Value;
    Change;
  end;
end;

procedure TCustomPropertyInspector.GetCaptionColor(
  APropTypeInfo: PTypeInfo; const APropName: string; var AColor: TColor);
begin
  if Assigned(OnGetCaptionColor) then
    OnGetCaptionColor(Self, APropTypeInfo, APropName, AColor);
end;

procedure TCustomPropertyInspector.InternalModified;
begin
  UpdateItems;
  if Assigned(OnModified) then
    OnModified(Self);
end;

procedure TCustomPropertyInspector.UpdateItems;

  procedure _UpdateItems(AList: TBaseObjectList);
  var
    I: Integer;
  begin
    for I := 0 to AList.Count - 1 do
    begin
      TPropertyInspectorItem(AList[I]).UpdateParams;
      _UpdateItems(TPropertyInspectorItem(AList[I]));
    end;
  end;

begin
  BeginUpdate;
  try
    _UpdateItems(Items);
  finally
    EndUpdate;
  end;
end;

procedure TCustomPropertyInspector.CheckObjectsLock;
begin
  if FObjectsLocked then
    raise EPropsPage.Create('Property inspector is changing property value. ' +
      'Can not change objects');
end;

{ TPropertyEditor }

function TPropertyEditor.AllEqual: Boolean;
begin
  Result := FPropCount = 1;
end;

constructor TPropertyEditor.Create(ADesigner: Pointer; APropCount: Integer);
begin
  GetMem(FPropList, APropCount * SizeOf(TPropEditorPropListItem));
  FDesigner := ADesigner;
  FPropCount := APropCount;
end;

destructor TPropertyEditor.Destroy;
begin
  if FPropList <> nil then
    FreeMem(FPropList, FPropCount * SizeOf(TPropEditorPropListItem));
end;

function TPropertyEditor.DoGetValue: string;
begin
  if AllEqual then
    Result := GetValue
  else
    Result := '';
end;

procedure TPropertyEditor.Edit;
begin
    // Do nothing
end;

function TPropertyEditor.GetAttrs: TPropertyAttributes;
begin
  Result := [praMultiSelect];
end;

function TPropertyEditor.GetComponent(
  const AComponentName: string): TComponent;
begin
  Result := nil;
  if Assigned(OnGetComponent) then
    OnGetComponent(Self, AComponentName, Result);
end;

function TPropertyEditor.GetComponentName(AComponent: TComponent): string;
begin
  Result := AComponent.Name;
  if Assigned(OnGetComponentName) then
    OnGetComponentName(Self, AComponent, Result);
end;

procedure TPropertyEditor.GetComponentNames(AClass: TComponentClass;
  AResult: TStrings);
begin
  if Assigned(OnGetComponentNames) then
    OnGetComponentNames(Self, AClass, AResult);
end;

function TPropertyEditor.GetFloatValue(AIndex: Integer): Extended;
begin
  with FPropList^[AIndex] do
    Result := GetFloatProp(Instance, PropInfo);
end;

function TPropertyEditor.GetInstance(AIndex: Integer): TPersistent;
begin
  Result := FPropList[AIndex].Instance;
end;

function TPropertyEditor.GetInt64Value(AIndex: Integer): Int64;
begin
  with FPropList^[AIndex] do
    Result := GetInt64Prop(Instance, PropInfo);
end;

function TPropertyEditor.GetOrdValue(AIndex: Integer): Longint;
begin
  with FPropList^[AIndex] do
    Result := GetOrdProp(Instance, PropInfo);
end;

function TPropertyEditor.GetPropInfo(AIndex: Integer): PPropInfo;
begin
  Result := FPropList[AIndex].PropInfo;
end;

function TPropertyEditor.GetPropName: string;
begin
  Result := string(FPropList[0].PropInfo^.Name);
end;

function TPropertyEditor.GetPropTypeInfo: PTypeInfo;
begin
  Result := FPropList[0].PropInfo^.PropType^;
end;

function TPropertyEditor.GetStrValue(AIndex: Integer): string;
begin
  with FPropList^[AIndex] do
    Result := GetStrProp(Instance, PropInfo);
end;

procedure TPropertyEditor.GetSubProps(AGetEditorClassProc: TGetEditorClassProc;
  AResult: TList);
begin
    // Do nothing
end;

function TPropertyEditor.GetValue: string;
begin
  Result := SUnknown;
end;

procedure TPropertyEditor.GetValues(AValues: TStrings);
begin
    // Do nothing
end;

function TPropertyEditor.GetVarValue(AIndex: Integer): Variant;
begin
  with FPropList^[AIndex] do
    Result := GetVariantProp(Instance, PropInfo);
end;

procedure TPropertyEditor.Modified;
begin
  if Assigned(FOnModified) then
    FOnModified(Self);
end;

procedure TPropertyEditor.SetFloatValue(Value: Extended);
var
  I: Integer;
begin
  for I := 0 to FPropCount - 1 do
    with FPropList^[I] do
      SetFloatProp(Instance, PropInfo, Value);
  Modified;
end;

procedure TPropertyEditor.SetInt64Value(Value: Int64);
var
  I: Integer;
begin
  for I := 0 to FPropCount - 1 do
    with FPropList^[I] do
      SetInt64Prop(Instance, PropInfo, Value);
  Modified;
end;

procedure TPropertyEditor.SetOrdValue(Value: Integer);
var
  I: Integer;
begin
  for I := 0 to FPropCount - 1 do
    with FPropList^[I] do
      SetOrdProp(Instance, PropInfo, Value);
  Modified;
end;

procedure TPropertyEditor.SetPropEntry(AIndex: Integer; AInstance: TPersistent;
  APropInfo: PPropInfo);
begin
  with FPropList[AIndex] do
  begin
    Instance := AInstance;
    PropInfo := APropInfo;
  end;
end;

procedure TPropertyEditor.SetStrValue(const Value: string);
var
  I: Integer;
begin
  for I := 0 to FPropCount - 1 do
    with FPropList^[I] do
    begin
      SetStrProp(Instance, PropInfo, Value);
    end;
  Modified;
end;

procedure TPropertyEditor.SetValue(const Value: string);
begin
    // Do nothing
end;

procedure TPropertyEditor.SetVarValue(const Value: Variant);
var
  I: Integer;
begin
  for I := 0 to FPropCount - 1 do
    with FPropList^[I] do
      SetVariantProp(Instance, PropInfo, Value);
  Modified;
end;

procedure TPropertyEditor.ValuesDrawValue(const AValue: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
begin
    // Do nothing
end;

procedure TPropertyEditor.ValuesMeasureHeight(const AValue: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin
    // Do nothing
end;

procedure TPropertyEditor.ValuesMeasureWidth(const AValue: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
    // Do nothing
end;

{ TNestedPropertyEditor }

constructor TNestedPropertyEditor.Create(AParent: TPropertyEditor);
begin
  FPropList := AParent.FPropList;
  FPropCount := AParent.FPropCount;
end;

destructor TNestedPropertyEditor.Destroy;
begin
    // Do not execute inherited
end;

function TNestedPropertyEditor.GetPropName: string;
begin
  Result := 'SubProp';
end;

{ TOrdinalPropertyEditor }

function TOrdinalPropertyEditor.AllEqual: Boolean;
var
  I: Integer;
  V: Integer;
begin
  Result := True;
  if PropCount > 1 then
  begin
    V := GetOrdValue(0);
    for I := 1 to PropCount - 1 do
      if GetOrdValue(I) <> V then
      begin
        Result := False;
        Break;
      end;
  end;
end;

{ TIntegerPropertyEditor }

function TIntegerPropertyEditor.GetValue: string;
begin
  with GetTypeData(PropTypeInfo)^ do
    if OrdType = otULong then // Unsigned
      Result := IntToStr(Cardinal(GetOrdValue(0)))
    else
      Result := IntToStr(GetOrdValue(0));
end;

procedure TIntegerPropertyEditor.SetValue(const Value: string);
var
  I: Int64;
begin
  I := StrToInt64(Value);
  with GetTypeData(PropTypeInfo)^ do
    if OrdType = otULong then
    begin // unsigned compare and reporting needed
      if (I < Cardinal(MinValue)) or (I > Cardinal(MaxValue)) then
                    // bump up to Int64 to get past the %d in the format string
        raise EPropertyEditor.CreateFmt('Value must be between %d and %d',
          [Int64(Cardinal(MinValue)), Int64(Cardinal(MaxValue))]);
    end
    else
      if (I < MinValue) or (I > MaxValue) then
      raise EPropertyEditor.CreateFmt('Value must be between %d and %d',
        [MinValue, MaxValue]);
  SetOrdValue(I);
end;

{ TCharPropertyEditor }

function TCharPropertyEditor.GetValue: string;
var
  LCh: Char;
begin
  LCh := Chr(GetOrdValue(0));
  if CharInSet(LCh, [#33 .. #127]) then
    Result := LCh
  else
    FmtStr(Result, '#%d', [Ord(LCh)]);
end;

procedure TCharPropertyEditor.SetValue(const Value: string);
var
  I: Integer;
begin
  if Length(Value) = 0 then
    I := 0
  else
    if Length(Value) = 1 then
    I := Ord(Value[1])
  else
    if Value[1] = '#' then
    I := StrToInt(Copy(Value, 2, Maxint))
  else
    raise EPropertyEditor.Create('Invalid property value');
  with GetTypeData(PropTypeInfo)^ do
    if (I < MinValue) or (I > MaxValue) then
      raise EPropertyEditor.CreateFmt('Value must be between %d and %d',
        [MinValue, MaxValue]);
  SetOrdValue(I);
end;

{ TEnumPropertyEditor }

function TEnumPropertyEditor.GetAttrs: TPropertyAttributes;
begin
  Result := [praMultiSelect, praValueList, praSortList];
end;

function TEnumPropertyEditor.GetValue: string;
var
  I: Integer;
begin
  I := GetOrdValue(0);
  with GetTypeData(PropTypeInfo)^ do
    if (I < MinValue) or (I > MaxValue) then
      I := MaxValue;
  Result := GetEnumName(PropTypeInfo, I);
end;

procedure TEnumPropertyEditor.GetValues(AValues: TStrings);
var
  I       : Integer;
  EnumType: PTypeInfo;
begin
  EnumType := PropTypeInfo;
  with GetTypeData(EnumType)^ do
  begin
    if MinValue < 0 then // Longbool/Wordbool/Bytebool
    begin
      AValues.Add(GetEnumName(EnumType, 0));
      AValues.Add(GetEnumName(EnumType, 1));
    end
    else
      for I := MinValue to MaxValue do
        AValues.Add(GetEnumName(EnumType, I));
  end;
end;

procedure TEnumPropertyEditor.SetValue(const Value: string);
var
  I: Integer;
begin
  I := GetEnumValue(PropTypeInfo, Value);
  with GetTypeData(PropTypeInfo)^ do
    if (I < MinValue) or (I > MaxValue) then
      raise EPropertyEditor.Create('Invalid property value');
  SetOrdValue(I);
end;

{ TFloatPropertyEditor }

function TFloatPropertyEditor.AllEqual: Boolean;
var
  I: Integer;
  V: Extended;
begin
  Result := True;
  if PropCount > 1 then
  begin
    V := GetFloatValue(0);
    for I := 1 to PropCount - 1 do
      if GetFloatValue(I) <> V then
      begin
        Result := False;
        Break;
      end;
  end;
end;

function TFloatPropertyEditor.GetValue: string;
const
  Precisions: array [TFloatType] of Integer = (7, 15, 18, 18, 18);
begin
  Result := FloatToStrF(GetFloatValue(0), ffGeneral,
    Precisions[GetTypeData(PropTypeInfo)^.FloatType], 0);
end;

procedure TFloatPropertyEditor.SetValue(const Value: string);
begin
  SetFloatValue(StrToFloat(Value));
end;

{ TStringPropertyEditor }

function TStringPropertyEditor.AllEqual: Boolean;
var
  I: Integer;
  V: string;
begin
  Result := True;
  if PropCount > 1 then
  begin
    V := GetStrValue(0);
    for I := 1 to PropCount - 1 do
      if GetStrValue(I) <> V then
      begin
        Result := False;
        Break;
      end;
  end;
end;

procedure TStringPropertyEditor.Edit;
var
  StringsEditorDlg: TStringsEditorDialog;
begin
  StringsEditorDlg := TStringsEditorDialog.Create(Application);
  try
    StringsEditorDlg.Lines.Text := GetStrValue(0);
    if StringsEditorDlg.Execute then
      SetStrValue(StringsEditorDlg.Lines.Text);
  finally
    StringsEditorDlg.Free;
  end;
end;

function TStringPropertyEditor.GetAttrs: TPropertyAttributes;
begin
  Result := [praDialog];
end;

function TStringPropertyEditor.GetValue: string;
begin
  Result := GetStrValue(0);
end;

procedure TStringPropertyEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

{ TSetPropertyEditor }

function TSetPropertyEditor.GetAttrs: TPropertyAttributes;
begin
  Result := [praMultiSelect, praSubProperties, praReadOnly];
end;

procedure TSetPropertyEditor.GetSubProps(AGetEditorClassProc
  : TGetEditorClassProc; AResult: TList);
var
  I: Integer;
begin
  with GetTypeData(GetTypeData(PropTypeInfo)^.CompType^)^ do
    for I := MinValue to MaxValue do
      AResult.Add(TSetElementPropertyEditor.Create(Self, I));
end;

function TSetPropertyEditor.GetValue: string;
var
  S  : TIntegerSet;
  TI : PTypeInfo;
  I  : Integer;
begin
  Integer(S) := GetOrdValue(0);
  TI := GetTypeData(PropTypeInfo)^.CompType^;
  Result := '[';
  for I := 0 to SizeOf(Integer) * 8 - 1 do
    if I in S then
    begin
      if Length(Result) <> 1 then
        Result := Result + ',';
      Result := Result + GetEnumName(TI, I);
    end;
  Result := Result + ']';
end;

{ TSetElementPropertyEditor }

function TSetElementPropertyEditor.AllEqual: Boolean;
var
  I: Integer;
  S: TIntegerSet;
  B: Boolean;
begin
  Result := True;
  if PropCount > 1 then
  begin
    Integer(S) := GetOrdValue(0);
    B := FElement in S;
    for I := 1 to PropCount - 1 do
    begin
      Integer(S) := GetOrdValue(I);
      if (FElement in S) <> B then
      begin
        Result := False;
        Break;
      end;
    end;
  end;
end;

constructor TSetElementPropertyEditor.Create(AParent: TPropertyEditor;
  AElement: Integer);
begin
  inherited Create(AParent);
  FElement := AElement;
end;

function TSetElementPropertyEditor.GetAttrs: TPropertyAttributes;
begin
  Result := [praMultiSelect, praValueList, praSortList];
end;

function TSetElementPropertyEditor.GetPropName: string;
begin
  Result := GetEnumName(GetTypeData(PropTypeInfo)^.CompType^, FElement);
end;

function TSetElementPropertyEditor.GetValue: string;
var
  S: TIntegerSet;
begin
  Integer(S) := GetOrdValue(0);
  Result := BooleanIdents[FElement in S];
end;

procedure TSetElementPropertyEditor.GetValues(AValues: TStrings);
begin
  AValues.Add(BooleanIdents[False]);
  AValues.Add(BooleanIdents[True]);
end;

procedure TSetElementPropertyEditor.SetValue(const Value: string);
var
  S: TIntegerSet;
begin
  Integer(S) := GetOrdValue(0);
  if CompareText(Value, BooleanIdents[True]) = 0 then
    Include(S, FElement)
  else
    Exclude(S, FElement);
  SetOrdValue(Integer(S));
end;

{ TClassPropertyEditor }

function TClassPropertyEditor.GetAttrs: TPropertyAttributes;
begin
  Result := [praMultiSelect, praSubProperties, praReadOnly];
end;

procedure TClassPropertyEditor.GetSubProps(AGetEditorClassProc
  : TGetEditorClassProc; AResult: TList);
var
  I       : Integer;
  J       : Integer;
  Objects : TList;
begin
  Objects := TList.Create;
  try
    for I := 0 to PropCount - 1 do
    begin
      J := GetOrdValue(I);
      if J <> 0 then
        Objects.Add(TObject(J));
    end;
    if Objects.Count > 0 then
      ELGetObjectsProps(Designer, Objects, tkAny, False,
        AGetEditorClassProc, AResult);
  finally
    Objects.Free;
  end;
end;

function TClassPropertyEditor.GetValue: string;
begin
  FmtStr(Result, '(%s)', [PropTypeInfo^.Name]);
end;

{ TInt64PropertyEditor }

function TInt64PropertyEditor.AllEqual: Boolean;
var
  I : Integer;
  V : Int64;
begin
  Result := True;
  if PropCount > 1 then
  begin
    V := GetInt64Value(0);
    for I := 1 to PropCount - 1 do
      if GetInt64Value(I) <> V then
      begin
        Result := False;
        Break;
      end;
  end;
end;

function TInt64PropertyEditor.GetValue: string;
begin
  Result := IntToStr(GetInt64Value(0));
end;

procedure TInt64PropertyEditor.SetValue(const Value: string);
begin
  SetInt64Value(StrToInt64(Value));
end;

{ TVariantPropertyEditor }

function TVariantPropertyEditor.GetAttrs: TPropertyAttributes;
begin
  Result := [praMultiSelect, praSubProperties];
end;

procedure TVariantPropertyEditor.GetSubProps(
  AGetEditorClassProc: TGetEditorClassProc; AResult: TList);
begin
  AResult.Add(TVariantTypePropertyEditor.Create(Self));
end;

function TVariantPropertyEditor.GetValue: string;

  function _GetVariantStr(const AValue: Variant): string;
  begin
    case VarType(AValue) of
      varBoolean:
        Result := BooleanIdents[AValue = True];
      varCurrency:
        Result := CurrToStr(AValue);
    else
      if TVarData(AValue).VType <> varNull then
        Result := AValue
      else
        Result := SNull;
    end;
  end;

var
  V: Variant;

begin
  V := GetVarValue(0);
  if VarType(V) <> varDispatch then
    Result := _GetVariantStr(V)
  else
    Result := 'ERROR';
end;

procedure TVariantPropertyEditor.SetValue(const Value: string);

  function _Cast(var AValue: Variant; ANewType: Integer): Boolean;
  var
    V2: Variant;
  begin
    Result := True;
//    if ANewType = varCurrency then
//      Result := AnsiPos(CurrencyString, AValue) > 0;
    if Result then
      try
        VarCast(V2, AValue, ANewType);
        Result := (ANewType = varDate) or (VarToStr(V2) = VarToStr(AValue));
        if Result then
          AValue := V2;
      except
        Result := False;
      end;
  end;

var
  V      : Variant;
  OldType: Integer;

begin
  OldType := VarType(GetVarValue(0));
  V := Value;
  if Value = '' then
    VarClear(V)
  else
    if (CompareText(Value, SNull) = 0) then
    V := NULL
  else
    if not _Cast(V, OldType) then
    V := Value;
  SetVarValue(V);
end;

{ TVariantTypePropertyEditor }

procedure TVariantTypePropertyEditor.AfterConstruction;
var
  I : Integer;
begin
  inherited;
  FVarTypeNames := TStringList.Create;
  for I := 0 to varByRef do
  begin
    FVarTypeNames.Add(VarTypeAsText(I));
  end;
end;

function TVariantTypePropertyEditor.AllEqual: Boolean;
var
  I      : Integer;
  V1, V2 : Variant;
begin
  Result := True;
  if PropCount > 1 then
  begin
    V1 := GetVarValue(0);
    for I := 1 to PropCount - 1 do
    begin
      V2 := GetVarValue(I);
      if VarType(V1) <> VarType(V2) then
      begin
        Result := False;
        Break;
      end;
    end;
  end;
end;

procedure TVariantTypePropertyEditor.BeforeDestruction;
begin
  inherited;
  FreeAndNil(FVarTypeNames);
end;

function TVariantTypePropertyEditor.GetAttrs: TPropertyAttributes;
begin
  Result := [praMultiSelect, praValueList, praSortList];
end;

function TVariantTypePropertyEditor.GetPropName: string;
begin
  Result := 'Type';
end;

function TVariantTypePropertyEditor.GetValue: string;
begin
  Result := VarTypeAsText(VarType(GetVarValue(0)));
end;

procedure TVariantTypePropertyEditor.GetValues(AValues: TStrings);
var
  I: Integer;
begin
  for I := 0 to FVarTypeNames.Count - 1 do
    if FVarTypeNames[I] <> '' then
      AValues.Add(FVarTypeNames[I]);
  AValues.Add(SString);
end;

procedure TVariantTypePropertyEditor.SetValue(const Value: string);

  function _GetSelectedType: Integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := 0 to FVarTypeNames.Count - 1 do
      if FVarTypeNames[I] = Value then
      begin
        Result := I;
        Break;
      end;
    if (Result = -1) and (Value = SString) then
      Result := varString;
  end;

var
  NewType: Integer;
  V      : Variant;

begin
  V := GetVarValue(0);
  NewType := _GetSelectedType;
  case NewType of
    varEmpty:
      VarClear(V);
    varNull:
      V := NULL;
    -1:
      raise Exception.Create(SUnknownType);
  else
    try
      VarCast(V, V, NewType);
    except
                { If it cannot cast, clear it and then cast again. }
      VarClear(V);
      VarCast(V, V, NewType);
    end;
  end;
  SetVarValue(V);
end;

{ TComponentNamePropertyEditor }

function TComponentNamePropertyEditor.GetAttrs: TPropertyAttributes;
begin
  Result := [praNotNestable];
end;

{ TDateTimePropertyEditor }

function TDateTimePropertyEditor.GetAttrs: TPropertyAttributes;
begin
  Result := [praMultiSelect];
end;

function TDateTimePropertyEditor.GetValue: string;
var
  DT: TDateTime;
begin
  DT := GetFloatValue(0);
  if DT = 0.0 then
    Result := ''
  else
    Result := DateTimeToStr(DT);
end;

procedure TDateTimePropertyEditor.SetValue(const Value: string);
var
  DT: TDateTime;
begin
  if Value = '' then
    DT := 0.0
  else
    DT := StrToDateTime(Value);
  SetFloatValue(DT);
end;

{ TDatePropertyEditor }

function TDatePropertyEditor.GetAttrs: TPropertyAttributes;
begin
  Result := [praMultiSelect];
end;

function TDatePropertyEditor.GetValue: string;
var
  DT: TDateTime;
begin
  DT := GetFloatValue(0);
  if DT = 0.0 then
    Result := ''
  else
    Result := DateToStr(DT);
end;

procedure TDatePropertyEditor.SetValue(const Value: string);
var
  DT: TDateTime;
begin
  if Value = '' then
    DT := 0.0
  else
    DT := StrToDate(Value);
  SetFloatValue(DT);
end;

{ TTimePropertyEditor }

function TTimePropertyEditor.GetAttrs: TPropertyAttributes;
begin
  Result := [praMultiSelect];
end;

function TTimePropertyEditor.GetValue: string;
var
  DT: TDateTime;
begin
  DT := GetFloatValue(0);
  if DT = 0.0 then
    Result := ''
  else
    Result := TimeToStr(DT);
end;

procedure TTimePropertyEditor.SetValue(const Value: string);
var
  DT: TDateTime;
begin
  if Value = '' then
    DT := 0.0
  else
    DT := StrToTime(Value);
  SetFloatValue(DT);
end;

{ TCaptionPropertyEditor }

function TCaptionPropertyEditor.GetAttrs: TPropertyAttributes;
begin
  Result := [praMultiSelect, praAutoUpdate];
end;

{ TColorPropertyEditor }

procedure TColorPropertyEditor.AddValue(const LS: string);
begin
  FValues.Add(LS);
end;

procedure TColorPropertyEditor.Edit;
var
  ColorDialog: TColorDialog;
begin
  ColorDialog := TColorDialog.Create(Application);
  try
    ColorDialog.Color := GetOrdValue(0);
    ColorDialog.Options := [];
    if ColorDialog.Execute then
      SetOrdValue(ColorDialog.Color);
  finally
    ColorDialog.Free;
  end;
end;

function TColorPropertyEditor.GetAttrs: TPropertyAttributes;
begin
  Result := [praMultiSelect, praDialog, praValueList, praOwnerDrawValues];
end;

function TColorPropertyEditor.GetValue: string;
begin
  Result := ColorToString(TColor(GetOrdValue(0)));
end;

procedure TColorPropertyEditor.GetValues(AValues: TStrings);
begin
  FValues := AValues;
  GetColorValues(AddValue);
end;

procedure TColorPropertyEditor.SetValue(const Value: string);
var
  NewValue: Longint;
begin
  if IdentToColor(Value, NewValue) then
    SetOrdValue(NewValue)
  else
    inherited SetValue(Value);
end;

procedure TColorPropertyEditor.ValuesDrawValue(const AValue: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);

  function _ColorToBorderColor(AColor: TColor): TColor;
  type
    TColorQuad = record
      Red,
        Green,
        Blue,
        Alpha: Byte;
    end;
  begin
    if (TColorQuad(AColor).Red > 192) or
      (TColorQuad(AColor).Green > 192) or
      (TColorQuad(AColor).Blue > 192) then
      Result := clBlack
    else
      if ASelected then
      Result := clWhite
    else
      Result := AColor;
  end;

var
  Right                      : Integer;
  OldPenColor, OldBrushColor : TColor;

begin
  Right := (ARect.Bottom - ARect.Top) + ARect.Left;
  with ACanvas do
  begin
    OldPenColor := Pen.Color;
    OldBrushColor := Brush.Color;
    Pen.Color := Brush.Color;
    Rectangle(ARect.Left, ARect.Top, Right, ARect.Bottom);
    Brush.Color := StringToColor(AValue);
    Pen.Color := _ColorToBorderColor(ColorToRGB(Brush.Color));
    Rectangle(ARect.Left + 1, ARect.Top + 1, Right - 1, ARect.Bottom - 1);
    Brush.Color := OldBrushColor;
    Pen.Color := OldPenColor;
    ACanvas.TextRect(
      Rect(Right, ARect.Top, ARect.Right, ARect.Bottom),
      Right + 1,
      ARect.Top + 1,
      AValue
    );
  end;
end;

procedure TColorPropertyEditor.ValuesMeasureHeight(const AValue: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin
  AHeight := ACanvas.TextHeight('Wg') + 2;
end;

procedure TColorPropertyEditor.ValuesMeasureWidth(const AValue: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
  AWidth := AWidth + ACanvas.TextHeight('Wg');
end;

{ TCursorPropertyEditor }

procedure TCursorPropertyEditor.AddValue(const LS: string);
begin
  FValues.Add(LS);
end;

function TCursorPropertyEditor.GetAttrs: TPropertyAttributes;
begin
  Result := [praMultiSelect, praValueList, praSortList, praOwnerDrawValues];
end;

function TCursorPropertyEditor.GetValue: string;
begin
  Result := CursorToString(TCursor(GetOrdValue(0)));
end;

procedure TCursorPropertyEditor.GetValues(AValues: TStrings);
begin
  FValues := AValues;
  GetCursorValues(AddValue);
end;

procedure TCursorPropertyEditor.SetValue(const Value: string);
var
  NewValue: Longint;
begin
  if IdentToCursor(Value, NewValue) then
    SetOrdValue(NewValue)
  else
    inherited SetValue(Value);
end;

procedure TCursorPropertyEditor.ValuesDrawValue(const AValue: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  Right       : Integer;
  CursorIndex : Integer;
  CursorHandle: THandle;
begin
  Right := ARect.Left + GetSystemMetrics(SM_CXCURSOR) + 4;
  with ACanvas do
  begin
    if not IdentToCursor(AValue, CursorIndex) then
      CursorIndex := StrToInt(AValue);
    ACanvas.FillRect(ARect);
    CursorHandle := Screen.Cursors[CursorIndex];
    if CursorHandle <> 0 then
      DrawIconEx(ACanvas.Handle, ARect.Left + 2, ARect.Top + 2, CursorHandle,
        0, 0, 0, 0, DI_NORMAL or DI_DEFAULTSIZE);
    ACanvas.TextRect(
      Rect(Right, ARect.Top, ARect.Right, ARect.Bottom),
      Right + 1,
      ARect.Top + 1,
      AValue
    );
  end;
end;

procedure TCursorPropertyEditor.ValuesMeasureHeight(const AValue: string;
  ACanvas: TCanvas; var AHeight: Integer);
var
  LTextHeight, LCursorHeight: Integer;
begin
  LTextHeight := ACanvas.TextHeight('Wg');
  LCursorHeight := GetSystemMetrics(SM_CYCURSOR) + 4;
  if LTextHeight >= LCursorHeight then
    AHeight := LTextHeight
  else
    AHeight := LCursorHeight;
end;

procedure TCursorPropertyEditor.ValuesMeasureWidth(const AValue: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
  AWidth := AWidth + GetSystemMetrics(SM_CXCURSOR) + 4;
end;

{ TFontCharsetPropertyEditor }

procedure TFontCharsetPropertyEditor.AddValue(const LS: string);
begin
  FValues.Add(LS);
end;

function TFontCharsetPropertyEditor.GetAttrs: TPropertyAttributes;
begin
  Result := [praMultiSelect, praSortList, praValueList];
end;

function TFontCharsetPropertyEditor.GetValue: string;
begin
  if not CharsetToIdent(TFontCharset(GetOrdValue(0)), Result) then
    FmtStr(Result, '%d', [GetOrdValue(0)]);
end;

procedure TFontCharsetPropertyEditor.GetValues(AValues: TStrings);
begin
  FValues := AValues;
  GetCharsetValues(AddValue);
end;

procedure TFontCharsetPropertyEditor.SetValue(const Value: string);
var
  LNewValue: Longint;
begin
  if IdentToCharset(Value, LNewValue) then
    SetOrdValue(LNewValue)
  else
    inherited SetValue(Value);
end;

{ TFontNamePropertyEditor }

function TFontNamePropertyEditor.GetAttrs: TPropertyAttributes;
begin
  Result := [praMultiSelect, praValueList, praSortList];
end;

procedure TFontNamePropertyEditor.GetValues(AValues: TStrings);
var
  I: Integer;
begin
  for I := 0 to Screen.Fonts.Count - 1 do
    AValues.Add(Screen.Fonts[I]);
end;

{ TImeNamePropertyEditor }

function TImeNamePropertyEditor.GetAttrs: TPropertyAttributes;
begin
  Result := [praValueList, praSortList, praMultiSelect];
end;

procedure TImeNamePropertyEditor.GetValues(AValues: TStrings);
var
  I: Integer;
begin
  for I := 0 to Screen.Imes.Count - 1 do
    AValues.Add(Screen.Imes[I]);
end;

{ TFontPropertyEditor }

procedure TFontPropertyEditor.Edit;
var
  FontDialog: TFontDialog;
begin
  FontDialog := TFontDialog.Create(Application);
  try
    FontDialog.Font := TFont(GetOrdValue(0));
    FontDialog.Options := FontDialog.Options + [fdForceFontExist];
    if FontDialog.Execute then
      SetOrdValue(Longint(FontDialog.Font));
  finally
    FontDialog.Free;
  end;
end;

function TFontPropertyEditor.GetAttrs: TPropertyAttributes;
begin
  Result := [praMultiSelect, praSubProperties, praDialog, praReadOnly];
end;

{ TModalResultPropertyEditor }

function TModalResultPropertyEditor.GetAttrs: TPropertyAttributes;
begin
  Result := [praMultiSelect, praValueList];
end;

function TModalResultPropertyEditor.GetValue: string;
var
  N: Longint;
begin
  N := GetOrdValue(0);
  case N of
    Low(ModalResults) .. High(ModalResults):
      Result := ModalResults[N];
  else
    Result := IntToStr(N);
  end;
end;

procedure TModalResultPropertyEditor.GetValues(AValues: TStrings);
var
  I: Integer;
begin
  for I := Low(ModalResults) to High(ModalResults) do
    AValues.Add(ModalResults[I]);
end;

procedure TModalResultPropertyEditor.SetValue(const Value: string);
var
  I: Integer;
begin
  if Value = '' then
  begin
    SetOrdValue(0);
    Exit;
  end;
  for I := Low(ModalResults) to High(ModalResults) do
    if CompareText(ModalResults[I], Value) = 0 then
    begin
      SetOrdValue(I);
      Exit;
    end;
  inherited SetValue(Value);
end;

{ TPenStylePropertyEditor }

function TPenStylePropertyEditor.GetAttrs: TPropertyAttributes;
begin
  Result := inherited GetAttrs + [praOwnerDrawValues];
end;

procedure TPenStylePropertyEditor.ValuesDrawValue(const AValue: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  Right, Top                 : Integer;
  OldPenColor, OldBrushColor : TColor;
  OldPenStyle                : TPenStyle;
begin
  Right := (ARect.Bottom - ARect.Top) * 2 + ARect.Left;
  Top := (ARect.Bottom - ARect.Top) div 2 + ARect.Top;
  with ACanvas do
  begin
    OldPenColor := Pen.Color;
    OldBrushColor := Brush.Color;
    OldPenStyle := Pen.Style;
    Pen.Color := Brush.Color;
    Rectangle(ARect.Left, ARect.Top, Right, ARect.Bottom);
    Pen.Color := clWindowText;
    Brush.Color := clWindow;
    Rectangle(ARect.Left + 1, ARect.Top + 1, Right - 1, ARect.Bottom - 1);
    Pen.Color := clWindowText;
    Pen.Style := TPenStyle(GetEnumValue(PropTypeInfo, AValue));
    MoveTo(ARect.Left + 1, Top);
    LineTo(Right - 1, Top);
    MoveTo(ARect.Left + 1, Top + 1);
    LineTo(Right - 1, Top + 1);
    Brush.Color := OldBrushColor;
    Pen.Style := OldPenStyle;
    Pen.Color := OldPenColor;
    ACanvas.TextRect(
      Rect(Right, ARect.Top, ARect.Right, ARect.Bottom),
      Right + 1,
      ARect.Top + 1,
      AValue
    );
  end;
end;

procedure TPenStylePropertyEditor.ValuesMeasureHeight(const AValue: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin
  AHeight := ACanvas.TextHeight('Wg') + 2;
end;

procedure TPenStylePropertyEditor.ValuesMeasureWidth(const AValue: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
  AWidth := AWidth + ACanvas.TextHeight('Wg') * 2;
end;

{ TBrushStylePropertyEditor }

function TBrushStylePropertyEditor.GetAttrs: TPropertyAttributes;
begin
  Result := inherited GetAttrs + [praOwnerDrawValues];
end;

procedure TBrushStylePropertyEditor.ValuesDrawValue(const AValue: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  Right         : Integer;
  OldPenColor   : TColor;
  OldBrushColor : TColor;
  OldBrushStyle : TBrushStyle;
begin
  Right := (ARect.Bottom - ARect.Top) + ARect.Left;
  with ACanvas do
  begin
    OldPenColor := Pen.Color;
    OldBrushColor := Brush.Color;
    OldBrushStyle := Brush.Style;
    Pen.Color := Brush.Color;
    Brush.Color := clWindow;
    Rectangle(ARect.Left, ARect.Top, Right, ARect.Bottom);
    Pen.Color := clWindowText;
    Brush.Style := TBrushStyle(GetEnumValue(PropTypeInfo, AValue));
    if Brush.Style = bsClear then
    begin
      Brush.Color := clWindow;
      Brush.Style := bsSolid;
    end
    else
      Brush.Color := clWindowText;
    Rectangle(ARect.Left + 1, ARect.Top + 1, Right - 1, ARect.Bottom - 1);
    Brush.Color := OldBrushColor;
    Brush.Style := OldBrushStyle;
    Pen.Color := OldPenColor;
    ACanvas.TextRect(
      Rect(Right, ARect.Top, ARect.Right, ARect.Bottom),
      Right + 1,
      ARect.Top + 1,
      AValue
    );
  end;
end;

procedure TBrushStylePropertyEditor.ValuesMeasureHeight(const AValue: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin
  AHeight := ACanvas.TextHeight('Wg') + 2;
end;

procedure TBrushStylePropertyEditor.ValuesMeasureWidth(const AValue: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
  AWidth := AWidth + ACanvas.TextHeight('Wg') * 2;
end;

{ TTabOrderPropertyEditor }

function TTabOrderPropertyEditor.GetAttrs: TPropertyAttributes;
begin
  Result := [];
end;

{ TShortCutPropertyEditor }

function TShortCutPropertyEditor.GetAttrs: TPropertyAttributes;
begin
  Result := [praMultiSelect, praValueList];
end;

function TShortCutPropertyEditor.GetValue: string;
var
  SC: TShortCut;
begin
  SC := GetOrdValue(0);
  if SC = scNone then
    Result := SNone
  else
    Result := ShortCutToText(SC);
end;

procedure TShortCutPropertyEditor.GetValues(AValues: TStrings);
var
  I: Integer;
begin
  AValues.Add(SNone);
  for I := 1 to High(ShortCuts) do
    AValues.Add(ShortCutToText(ShortCuts[I]));
end;

procedure TShortCutPropertyEditor.SetValue(const Value: string);
var
  SC: TShortCut;
begin
  SC := 0;
  if (Value <> '') and (AnsiCompareText(Value, SNone) <> 0) then
  begin
    SC := TextToShortCut(Value);
    if SC = 0 then
      raise EPropertyEditor.Create('Invalid property value');
  end;
  SetOrdValue(SC);
end;

{ TComponentPropertyEditor }

function TComponentPropertyEditor.AllEqual: Boolean;
var
  I : Integer;
  C : TComponent;
begin
  Result := True;
  C := TComponent(GetOrdValue(0));
  if PropCount > 1 then
    for I := 1 to PropCount - 1 do
      if TComponent(GetOrdValue(I)) <> C then
      begin
        Result := False;
        Break;
      end;
end;

function TComponentPropertyEditor.GetAttrs: TPropertyAttributes;
begin
  Result := [praMultiSelect, praComponentRef];
  if Assigned(GetPropInfo(0).SetProc) then
    Result := Result + [praValueList, praSortList]
  else
    Result := Result + [praReadOnly];
  if (TComponent(GetOrdValue(0)) <> nil) and AllEqual then
    Result := Result + [praSubProperties, praVolatileSubProperties];
end;

procedure TComponentPropertyEditor.GetSubProps(
  AGetEditorClassProc: TGetEditorClassProc; AResult: TList);
var
  I       : Integer;
  J       : Integer;
  Objects : TList;
begin
  Objects := TList.Create;
  try
    for I := 0 to PropCount - 1 do
    begin
      J := GetOrdValue(I);
      if J <> 0 then
        Objects.Add(TObject(J));
    end;
    if Objects.Count > 0 then
      ELGetObjectsProps(Designer, Objects, tkAny, True,
        AGetEditorClassProc, AResult);
  finally
    Objects.Free;
  end;
end;

function TComponentPropertyEditor.GetValue: string;
var
  LComponent: TComponent;
begin
  LComponent := TComponent(GetOrdValue(0));
  if LComponent <> nil then
    Result := GetComponentName(LComponent)
  else
    Result := '';
end;

procedure TComponentPropertyEditor.GetValues(AValues: TStrings);
begin
  GetComponentNames(TComponentClass(GetTypeData(PropTypeInfo)
    ^.ClassType), AValues);
end;

procedure TComponentPropertyEditor.SetValue(const Value: string);
var
  C: TComponent;
begin
  C := nil;
  if Value <> '' then
  begin
    C := GetComponent(Value);
    if not(C is GetTypeData(PropTypeInfo)^.ClassType) then
      raise EPropertyError.Create('Invalid property value');
  end;
  SetOrdValue(Longint(C));
end;

{ TStringsPropertyEditor }

procedure TStringsPropertyEditor.Edit;
var
  StringsEditorDlg: TStringsEditorDialog;
begin
  StringsEditorDlg := TStringsEditorDialog.Create(Application);
  try
    StringsEditorDlg.Lines := TStrings(GetOrdValue(0));
    if StringsEditorDlg.Execute then
      SetOrdValue(Longint(StringsEditorDlg.Lines));
  finally
    StringsEditorDlg.Free;
  end;
end;

function TStringsPropertyEditor.GetAttrs: TPropertyAttributes;
begin
  Result := [praMultiSelect, praDialog, praReadOnly];
end;

{ TFileNamePropertyEditor }

procedure TFileNamePropertyEditor.Edit;
var
  FD: TFileOpenDialog;
begin
  FD := TFileOpenDialog.Create(Application);
  try
    FD.Options := [fdoPickFolders, fdoShareAware, fdoForceShowHidden, fdoNoValidate];
    FD.FileName := GetStrValue(0);
    if FD.Execute then
      SetStrValue(FD.FileName);
  finally
    FD.Free;
  end;
end;

end.
