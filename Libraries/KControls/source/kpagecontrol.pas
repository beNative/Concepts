{ @abstract(This file is part of the KControls component suite for Delphi and Lazarus.)
  @author(Tomas Krysl)

  Copyright (c) 2020 Tomas Krysl<BR><BR>

  <B>License:</B><BR>
  This code is licensed under BSD 3-Clause Clear License, see file License.txt or https://spdx.org/licenses/BSD-3-Clause-Clear.html.
}

unit kpagecontrol; // lowercase name because of Lazarus/Linux

{$include kcontrols.inc}
{$WEAKPACKAGEUNIT ON}

interface

uses
{$IFDEF FPC}
  LCLType, LCLIntf, LMessages, LCLProc,
{$ELSE}
  Windows, Messages,
{$ENDIF}
  Classes, Controls, Graphics, ExtCtrls, Forms, StdCtrls, ComCtrls, Contnrs, ImgList, Buttons,
  KFunctions, KControls, KButtons;

type
  TKTabOption = (toDrag, toDragUndock, toBrightTopColors);

  TKTabOptions = set of TKTabOption;

const
  cDefaultScrollButtonSize = 20;
  cDefaultTabPadding = 2;
  cDefaultTabHeight = 24;
  cDefaultTabPosition = tpTop;
  cDefaultTabWidth = 0;
  cDefaultTabOptions = [toDrag];

  cDefaultHotTop = clGradientActiveCaption;
  cDefaultHotBottom = clGradientInactiveCaption;
  cDefaultNormalTop = clBtnFace;
  cDefaultNormalBottom = clBtnHighlight;
  cDefaultNormalText = clBtnText;
  cDefaultSelectedTop = clActiveCaption;
  cDefaultSelectedBottom = clActiveCaption;
  cDefaultSelectedText = clCaptionText;
  cDefaultTabBorder = clBtnShadow;

  ciHotTop = TKColorIndex(0);
  ciHotBottom = TKColorIndex(1);
  ciNormalTop = TKColorIndex(2);
  ciNormalBottom = TKColorIndex(3);
  ciNormalText = TKColorIndex(4);
  ciSelectedTop = TKColorIndex(5);
  ciSelectedBottom = TKColorIndex(6);
  ciSelectedText = TKColorIndex(7);
  ciTabBorder = TKColorIndex(8);

  ciMaxIndex = ciTabBorder;

type
  TKTabColors = class(TKCustomColors)
  protected
    { Returns color specification structure for given index. }
    function GetColorSpec(Index: TKColorIndex): TKColorSpec; override;
    { Returns maximum color index. }
    function GetMaxIndex: Integer; override;
  published
    property HotTop: TColor index ciHotTop read GetColor write SetColor default cDefaultHotTop;
    property HotBottom: TColor index ciHotBottom read GetColor write SetColor default cDefaultHotBottom;
    property NormalTop: TColor index ciNormalTop read GetColor write SetColor default cDefaultNormalTop;
    property NormalBottom: TColor index ciNormalBottom read GetColor write SetColor default cDefaultNormalBottom;
    property NormalText: TColor index ciNormalText read GetColor write SetColor default cDefaultNormalText;
    property SelectedTop: TColor index ciSelectedTop read GetColor write SetColor default cDefaultSelectedTop;
    property SelectedBottom: TColor index ciSelectedBottom read GetColor write SetColor default cDefaultSelectedBottom;
    property SelectedText: TColor index ciSelectedText read GetColor write SetColor default cDefaultSelectedText;
    property TabBorder: TColor index ciTabBorder read GetColor write SetColor default cDefaultTabBorder;
  end;

  TKTabState = (tsNormal, tsHot, tsSelected);

  TKTabInfo = record
    CloseHeight,
    CloseWidth,
    ImageHeight,
    ImageWidth,
    ImagePadding,
    LeftPadding,
    RightPadding,
    TabExtent,
    TextPadding,
    TextWidth: Integer;
  end;

  TKTabPaintInfo = record
    TabRect: TRect;
    ImageRect: TRect;
    TextRect: TRect;
    CloseRect: TRect;
  end;

  TKTabPanel = class;

  TKTabPanelOptions = class(TPersistent)
  private
    FCloseButtonIndex: TImageIndex;
    FColors: TKTabColors;
    FLeftButtonIndex: TImageIndex;
    FOptions: TKTabOptions;
    FPadding: Integer;
    FRightButtonIndex: TImageIndex;
    FScrollButtonSize: Integer;
    procedure SetCloseButtonIndex(const Value: TImageIndex);
    procedure SetColors(const Value: TKTabColors);
    procedure SetOptions(const Value: TKTabOptions);
    procedure SetLeftButtonIndex(const Value: TImageIndex);
    procedure SetPadding(const Value: Integer);
    procedure SetRightButtonIndex(const Value: TImageIndex);
    procedure SetScrollButtonSize(const Value: Integer);
  public
    FPanel: TKTabPanel;
    constructor Create(APanel: TKTabPanel);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure UpdateTabPanel;
  published
    property CloseButtonIndex: TImageIndex read FCloseButtonIndex write SetCloseButtonIndex default -1;
    property Colors: TKTabColors read FColors write SetColors;
    property Options: TKTabOptions read FOptions write SetOptions default cDefaultTabOptions;
    property LeftButtonIndex: TImageIndex read FLeftButtonIndex write SetLeftButtonIndex default -1;
    property Padding: Integer read FPadding write SetPadding default cDefaultTabPadding;
    property RightButtonIndex: TImageIndex read FRightButtonIndex write SetRightButtonIndex default -1;
    property ScrollButtonSize: Integer read FScrollButtonSize write SetScrollButtonSize default cDefaultScrollButtonSize;
  end;

  TKCustomPageControl = class;

  { TKTabPanel }

  TKTabPanel = class(TKCustomControl)
  private
    FFirstVisibleTab: Integer;
    FOptions: TKTabPanelOptions;
    FPageControl: TKCustomPageControl;
    procedure CMCursorChanged(var Message: TLMessage); message CM_CURSORCHANGED;
    function GetTabs(Index: Integer): TKString;
    procedure SetFirstVisibleTab(Value: Integer);
    procedure SetOptions(const Value: TKTabPanelOptions);
    procedure SetPageControl(const Value: TKCustomPageControl);
  protected
    FAllTabsExtent: Integer;
    FDraggedTab: Integer;
    FDraggedPos: TPoint;
    FFullyVisibleTabsExtent: Integer;
    FInvisibleTabsExtent: Integer;
    FLastDraggedTab: Integer;
    FLastFullyVisibleTab: Integer;
    FLeftScrollButton: TKSpeedButton;
    FMaxFirstVisibleTab: Integer;
    FMouseIndex: Integer;
    FMouseInCloseButton: Boolean;
    FPageToClose: Integer;
    FRightScrollButton: TKSpeedButton;
    FScrolling: Boolean;
    FTabAreaExtent: Integer;
    FVisibleTabsExtent: Integer;
    function GetTabInfo(ACanvas: TCanvas; ATabIndex: Integer; out Info: TKTabInfo): Boolean; virtual;
    function GetTabPaintInfo(ACanvas: TCanvas; ATabIndex: Integer; out Info: TKTabPaintInfo): Boolean; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure LeftScrollButtonClick(Sender: TObject);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseFormLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMoveCaptured(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseOver(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function PaintTab(ACanvas: TCanvas; ATabIndex: Integer): Boolean; virtual;
    procedure PaintTabBackground(ACanvas: TCanvas; const ARect: TRect; AState: TKTabState); virtual;
    procedure PaintTabCloseButton(ACanvas: TCanvas; const ARect: TRect); virtual;
    procedure PaintTabImage(ACanvas: TCanvas; const ARect: TRect; ATabIndex: Integer); virtual;
    procedure PaintTabText(ACanvas: TCanvas; const ARect: TRect; ATabIndex: Integer; AState: TKTabState); virtual;
    procedure PaintAfterTabs(ACanvas: TCanvas; ALastTabIndex: Integer); virtual;
    procedure PaintToCanvas(ACanvas: TCanvas); override;
    procedure RightScrollButtonClick(Sender: TObject);
    procedure UpdateTabPanelLayout(ACanvas: TCanvas); virtual;
    procedure UpdateSize; override;
    procedure UpdateScrollRange; virtual;
    procedure UpdateTabPanel; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IndexOfTabAt(X, Y: Integer): Integer; virtual;
    function TabRect(AIndex: Integer): TRect; virtual;
    property FirstVisibleTab: Integer read FFirstVisibleTab write SetFirstVisibleTab;
    property Options: TKTabPanelOptions read FOptions write SetOptions;
    property PageControl: TKCustomPageControl read FPageControl write SetPageControl;
    property Tabs[Index: Integer]: TKString read GetTabs;
  end;

  { TKTabSheet }

  TKTabSheet = class(TWinControl)
  private
    FImageIndex: TImageIndex;
    FPageControl: TKCustomPageControl;
    FHighlighted: Boolean;
    FOnHide: TNotifyEvent;
    FOnShow: TNotifyEvent;
    function GetPageIndex: Integer;
    procedure SetHighlighted(Value: Boolean);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SetPageControl(APageControl: TKCustomPageControl);
    procedure SetPageIndex(Value: Integer);
    procedure CMShowingChanged(var Message: TLMessage); message CM_SHOWINGCHANGED;
    procedure CMTextChanged(var Message: TLMessage); message CM_TEXTCHANGED;
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure WMPaint(var Msg: TLMPaint); message LM_PAINT;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoHide; dynamic;
    procedure DoShow; dynamic;
    procedure ReadState(Reader: TReader); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property PageControl: TKCustomPageControl read FPageControl write SetPageControl;
  published
    property BorderWidth;
    property Caption;
    property DragMode;
    property Enabled;
    property Font;
    property Height stored False;
    property Highlighted: Boolean read FHighlighted write SetHighlighted default False;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default 0;
    property Left stored False;
    property Constraints;
    property PageIndex: Integer read GetPageIndex write SetPageIndex stored False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Top stored False;
    property Visible stored False;
    property Width stored False;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
    property OnMouseDown;
  {$IFDEF COMPILER9_UP}
    property OnMouseEnter;
    property OnMouseLeave;
  {$ENDIF}
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnStartDrag;
  end;

  TKTabSheets = class(TObjectList)
  private
    function GetItem(Index: Integer): TKTabSheet;
    procedure SetItem(Index: Integer; const Value: TKTabSheet);
  public
    function Add(AItem: TKTabSheet): Integer;
    function IndexOf(AItem: TKTabSheet): Integer;
    property Items[Index: Integer]: TKTabSheet read GetItem write SetItem; default;
  end;

  TKTabClickEvent = procedure(Sender: TObject; TabIndex: Integer) of object;
  TKTabCloseQueryEvent = procedure(Sender: TObject; TabIndex: Integer;
    var CanClose: Boolean) of object;

  { TKCustomPageControl }

  TKCustomPageControl = class(TKCustomControl)
  private
    FActivateNewDocked: Boolean;
    FActivePageIndex: Integer;
    FDisabledImages: TImageList;
    FHotTrack: Boolean;
    FImages: TImageList;
    FPages: TKTabSheets;
    FTabHeight: Integer;
    FTabPanel: TKTabPanel;
    FTabPosition: TTabPosition;
    FTabWidth: Integer;
    FOnChange: TNotifyEvent;
    FOnChanging: TTabChangingEvent;
    FOnGetImageIndex: TTabGetImageEvent;
    FOnTabClick: TKTabClickEvent;
    FOnTabCloseQuery: TKTabCloseQueryEvent;
    function GetActivePage: TKTabSheet;
    function GetPage(Index: Integer): TKTabSheet;
    function GetPageCount: Integer;
    procedure SetActivePageIndex(const Value: Integer);
    procedure SetDisabledImages(const Value: TImageList);
    procedure SetHotTrack(const Value: Boolean);
    procedure SetImages(const Value: TImageList);
    procedure SetTabHeight(const Value: Integer);
    procedure SetTabPanel(const Value: TKTabPanel);
    procedure SetTabPanelOptions(const Value: TKTabPanelOptions);
    procedure SetTabPosition(Value: TTabPosition);
    procedure SetTabWidth(const Value: Integer);
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
{$IFDEF FPC}
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
{$ELSE}
    procedure CMDockNotification(var Message: TCMDockNotification); message CM_DOCKNOTIFICATION;
    procedure CMDockClient(var Message: TCMDockClient); message CM_DOCKCLIENT;
    procedure CMUnDockClient(var Message: TCMUnDockClient); message CM_UNDOCKCLIENT;
{$ENDIF}
    procedure WMEraseBkgnd(var Msg: TLMessage); message LM_ERASEBKGND;
    function GetTabPanelOptions: TKTabPanelOptions;
  protected
    FDeletingPage: Boolean;
    FLoadedActivePageIndex: Integer;
    FNewDockSheet: TKTabSheet;
    FUndockingPage: TKTabSheet;
    function CanChange: Boolean; virtual;
    procedure Change; virtual;
    procedure ChangeActivePage(Page: TKTabSheet); virtual;
    procedure DoAddDockClient(Client: TControl; const ARect: TRect); override;
    procedure DockOver(Source: TDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure DoRemoveDockClient(Client: TControl); override;
    procedure DoTabClick(AIndex: Integer); virtual;
    procedure DoTabClose(AIndex: Integer); virtual;
    procedure FreePage(Page: TKTabSheet); virtual;
    function GetImageIndex(TabIndex: Integer): Integer; virtual;
    function GetPageFromDockClient(Client: TControl): TKTabSheet;
    procedure GetSiteInfo(Client: TControl; var InfluenceRect: TRect;
      MousePos: TPoint; var CanDock: Boolean); override;
    procedure InsertPage(Page: TKTabSheet); virtual;
    procedure Loaded; override;
{$IFDEF FPC}
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
{$ENDIF}
    procedure MovePage(CurIndex, NewIndex: Integer);
    procedure PaintToCanvas(ACanvas: TCanvas); override;
    function ReleasePage(Index: Integer): TKTabSheet; virtual;
    procedure RemovePage(Page: TKTabSheet); virtual;
    procedure SetActivePage(Page: TKTabSheet);
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    procedure ShowControl(AControl: TControl); override;
    procedure UpdateAllDesignerFlags;
    procedure UpdateDesignerFlags(APageIndex: integer);
    procedure UpdateTabPanel; virtual;
    procedure UpdateTabPanelPosition; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddPage(AOwner: TComponent): TKTabSheet; virtual;
    procedure DeletePage(AIndex: Integer); overload; virtual;
    procedure DeletePage(APage: TKTabSheet); overload; virtual;
    function FindNextPage(CurPage: TKTabSheet; GoForward: Boolean): TKTabSheet;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function IndexOfTabAt(X, Y: Integer): Integer;
    procedure SelectNextPage(GoForward: Boolean);
    function TabRect(Index: Integer): TRect;
    property ActivateNewDocked: Boolean read FActivateNewDocked write FActivateNewDocked default True;
    property ActivePage: TKTabSheet read GetActivePage write SetActivePage;
    property ActivePageIndex: Integer read FActivePageIndex write SetActivePageIndex default -1;
    property DisabledImages: TImageList read FDisabledImages write SetDisabledImages;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default True;
    property Images: TImageList read FImages write SetImages;
    property ImageIndex[Index: Integer]: Integer read GetImageIndex;
    property PageCount: Integer read GetPageCount;
    property Pages[Index: Integer]: TKTabSheet read GetPage;
    property TabPosition: TTabPosition read FTabPosition write SetTabPosition default cDefaultTabPosition;
    property TabHeight: Integer read FTabHeight write SetTabHeight default cDefaultTabHeight;
    property TabPanel: TKTabPanel read FTabPanel write SetTabPanel;
    property TabPanelOptions: TKTabPanelOptions read GetTabPanelOptions write SetTabPanelOptions;
    property TabWidth: Integer read FTabWidth write SetTabWidth default cDefaultTabWidth;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TTabChangingEvent read FOnChanging write FOnChanging;
    property OnGetImageIndex: TTabGetImageEvent read FOnGetImageIndex write FOnGetImageIndex;
    property OnTabClick: TKTabClickEvent read FOnTabClick write FOnTabClick;
    property OnTabCloseQuery: TKTabCloseQueryEvent read FOnTabCloseQuery write FOnTabCloseQuery;
  end;

  TKPageControl = class(TKCustomPageControl)
  published
    property ActivateNewDocked;
    property ActivePageIndex;
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderStyle default bsNone;
    property Constraints;
    property DisabledImages;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HotTrack;
    property Images;
//    property MultiLine;
    property ParentBackground;
    property ParentBiDiMode;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
//    property RaggedRight;
//    property ScrollOpposite;
    property ShowHint;
//    property Style;
    property TabHeight;
    property TabOrder;
    property TabPanelOptions;
    property TabPosition;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnChange;
    property OnChanging;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetImageIndex;
    property OnGetSiteInfo;
    property OnMouseDown;
  {$IFDEF COMPILER9_UP}
    property OnMouseEnter;
    property OnMouseLeave;
  {$ENDIF}
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnTabClick;
    property OnUnDock;
  end;

implementation

uses
  SysUtils
{$IFDEF USE_THEMES}
  , Themes
 {$IFNDEF FPC}
  , UxTheme
 {$ENDIF}
{$ENDIF}
  , KGraphics;

{ TKTabColors }

function TKTabColors.GetColorSpec(Index: TKColorIndex): TKColorSpec;
begin
  case Index of
    ciNormalTop: begin Result.Def := cDefaultNormalTop; Result.Name := ''; end;
    ciNormalBottom: begin Result.Def := cDefaultNormalBottom; Result.Name := ''; end;
    ciNormalText: begin Result.Def := cDefaultNormalText; Result.Name := ''; end;
    ciSelectedTop: begin Result.Def := cDefaultSelectedTop; Result.Name := ''; end;
    ciSelectedBottom: begin Result.Def := cDefaultSelectedBottom; Result.Name := ''; end;
    ciSelectedText: begin Result.Def := cDefaultSelectedText; Result.Name := ''; end;
    ciHotTop: begin Result.Def := cDefaultHotTop; Result.Name := ''; end;
    ciHotBottom: begin Result.Def := cDefaultHotBottom; Result.Name := ''; end;
    ciTabBorder: begin Result.Def := cDefaultTabBorder; Result.Name := ''; end;
  else
    Result := inherited GetColorSpec(Index);
  end;
end;

function TKTabColors.GetMaxIndex: Integer;
begin
  Result := ciMaxIndex;
end;


{ TKTabPanelOptions }

constructor TKTabPanelOptions.Create(APanel: TKTabPanel);
begin
  FPanel := APanel;
  FColors := TKTabColors.Create(FPanel);
  FCloseButtonIndex := -1;
  FLeftButtonIndex := -1;
  FOptions := cDefaultTabOptions;
  FPadding := cDefaultTabPadding;
  FRightButtonIndex := -1;
  FScrollButtonSize := cDefaultScrollButtonSize;
end;

destructor TKTabPanelOptions.Destroy;
begin
  FColors.Free;
  inherited;
end;

procedure TKTabPanelOptions.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TKTabPanelOptions then
  begin
    CloseButtonIndex := TKTabPanelOptions(Source).CloseButtonIndex;
    Colors := TKTabPanelOptions(Source).Colors;
    Options := TKTabPanelOptions(Source).Options;
    LeftButtonIndex := TKTabPanelOptions(Source).LeftButtonIndex;
    Padding := TKTabPanelOptions(Source).Padding;
    RightButtonIndex := TKTabPanelOptions(Source).RightButtonIndex;
    ScrollButtonSize := TKTabPanelOptions(Source).ScrollButtonSize;
  end;
end;

procedure TKTabPanelOptions.SetCloseButtonIndex(const Value: TImageIndex);
begin
  if Value <> FCloseButtonIndex then
  begin
    FCloseButtonIndex := Value;
    UpdateTabPanel;
  end;
end;

procedure TKTabPanelOptions.SetColors(const Value: TKTabColors);
begin
  FColors.Assign(Value);
end;

procedure TKTabPanelOptions.SetLeftButtonIndex(const Value: TImageIndex);
begin
  if Value <> FLeftButtonIndex then
  begin
    FLeftButtonIndex := Value;
    UpdateTabPanel;
  end;
end;

procedure TKTabPanelOptions.SetOptions(const Value: TKTabOptions);
begin
  if Value <> FOptions then
  begin
    FOptions := Value;
    UpdateTabPanel;
  end;
end;

procedure TKTabPanelOptions.SetPadding(const Value: Integer);
begin
  if Value <> Fpadding then
  begin
    FPadding := Value;
    UpdateTabPanel;
  end;
end;

procedure TKTabPanelOptions.SetRightButtonIndex(const Value: TImageIndex);
begin
  if Value <> FRightButtonIndex then
  begin
    FRightButtonIndex := Value;
    UpdateTabPanel;
  end;
end;

procedure TKTabPanelOptions.SetScrollButtonSize(const Value: Integer);
begin
  if Value <> FScrollButtonSize then
  begin
    FScrollButtonSize := Value;
    UpdateTabPanel;
  end;
end;

procedure TKTabPanelOptions.UpdateTabPanel;
begin
  if FPanel <> nil then
    FPanel.UpdateTabPanel;
end;

{ TKTabPanel }

constructor TKTabPanel.Create(AOwner: TComponent);
begin
  inherited;
  BorderStyle := bsNone;
  ControlStyle := ControlStyle - [csCaptureMouse];
{$IFDEF FPC}
  ControlStyle := ControlStyle + [csDesignInteractive];
{$ENDIF}
  DoubleBuffered := True;
  //SetSubComponent(True);
  FAllTabsExtent := 0;
  FFirstVisibleTab := 0;
  FFullyVisibleTabsExtent := 0;
  FInvisibleTabsExtent := 0;
  FLeftScrollButton := TKSpeedButton.Create(Self);
  FLeftScrollButton.FocusRect := False;
  FLeftScrollButton.Visible := False;
  FLeftScrollButton.OnClick := LeftScrollButtonClick;
  FLeftScrollButton.Parent := Self;
  FMaxFirstVisibleTab := 0;
  FMouseIndex := -1;
  FMouseInCloseButton := False;
  FPageControl := nil;
  FOptions := TKTabPanelOptions.Create(Self);
  FRightScrollButton := TKSpeedButton.Create(Self);
  FRightScrollButton.FocusRect := False;
  FRightScrollButton.Visible := False;
  FRightScrollButton.OnClick := RightScrollButtonClick;
  FRightScrollButton.Parent := Self;
  FScrolling := False;
  FVisibleTabsExtent := 0;
  UpdateTabPanel;
end;

destructor TKTabPanel.Destroy;
begin
  FOptions.Free;
  inherited;
end;

procedure TKTabPanel.CMCursorChanged(var Message: TLMessage);
begin
{$IFDEF FPC}
  SetTempCursor(Screen.Cursors[Cursor]);
{$ELSE}
  Windows.SetCursor(Screen.Cursors[Cursor]);
{$ENDIF}
end;

function TKTabPanel.GetTabInfo(ACanvas: TCanvas; ATabIndex: Integer;
  out Info: TKTabInfo): Boolean;
var
  Page: TKTabSheet;
  ImageIndex: Integer;
begin
  if FPageControl <> nil then
  begin
    Page := FPageControl.Pages[ATabIndex];
    ImageIndex := FPageControl.GetImageIndex(ATabIndex);
    if (FPageControl.Images <> nil) and (ImageIndex >= 0) and (ImageIndex < FPageControl.Images.Count) then
    begin
      Info.ImageWidth := FPageControl.Images.Width;
      Info.ImageHeight := FPageControl.Images.Height;
    end else
    begin
      Info.ImageWidth := 0;
      Info.ImageHeight := 0;
    end;
    if Info.ImageWidth <> 0 then
      Info.ImagePadding := FOptions.Padding
    else
      Info.ImagePadding := 0;
    if FPageControl.TabWidth <> 0 then
      Info.TextWidth := FPageControl.TabWidth
    else
      Info.TextWidth := ACanvas.TextWidth(Page.Caption);
    if Info.TextWidth <> 0 then
      Info.TextPadding := FOptions.Padding
    else
      Info.TextPadding := 0;
    if (FPageControl.Images <> nil) and (FOptions.CloseButtonIndex >= 0) and (FOptions.CloseButtonIndex < FPageControl.Images.Count) then
    begin
      Info.CloseWidth := FPageControl.Images.Width;
      Info.CloseHeight := FPageControl.Images.Height;
    end else
    begin
      Info.CloseHeight := 0;
      Info.CloseWidth := 0;
    end;
    Info.LeftPadding := 2 * FOptions.Padding;
    Info.RightPadding := 2 * FOptions.Padding;
    Info.TabExtent := Info.LeftPadding + Info.ImageWidth + Info.ImagePadding +
      Info.TextWidth + Info.TextPadding + Info.CloseWidth + Info.RightPadding;
    Result := True;
  end else
    Result := False;
end;

function TKTabPanel.GetTabPaintInfo(ACanvas: TCanvas; ATabIndex: Integer; out Info: TKTabPaintInfo): Boolean;
var
  I, Extent, Limit, ScrollButtonExtent: Integer;
  TI: TKTabInfo;
begin
  Result := False;
  if (FPageControl <> nil) and (ATabIndex >= 0) and (ATabIndex < FPageControl.PageCount) then
  begin
    if FAllTabsExtent > Width then
    begin
      ScrollButtonExtent := 2 * FOptions.Padding + FOptions.ScrollButtonSize;
      Extent := ScrollButtonExtent - FInvisibleTabsExtent;
      Limit := Width - ScrollButtonExtent;
    end else
    begin
      Extent := -FInvisibleTabsExtent;
      Limit := Width;
    end;
    I := 0;
    while I <= ATabIndex do
    begin
      GetTabInfo(ACanvas, I, TI);
      if (I = ATabIndex) and (Extent < Limit) then
      begin
        Info.TabRect := Rect(Extent, 0, Extent + TI.TabExtent, Height);
        Info.ImageRect.TopLeft := Point(Extent + TI.LeftPadding, (Height - TI.ImageHeight) div 2);
        Info.ImageRect.BottomRight := Point(Info.ImageRect.Left + TI.ImageWidth, Info.ImageRect.Top + TI.ImageHeight);
        Info.TextRect.TopLeft := Point(Extent + TI.LeftPadding + TI.ImageWidth + TI.ImagePadding, 0);
        Info.TextRect.BottomRight := Point(Info.TextRect.Left + TI.TextWidth, Info.TextRect.Top + Height);
        Info.CloseRect.TopLeft := Point(Extent + TI.LeftPadding + TI.ImageWidth + TI.ImagePadding + TI.TextWidth + TI.TextPadding, (Height - TI.CloseHeight) div 2);
        Info.CloseRect.BottomRight := Point(Info.CloseRect.Left + TI.CloseWidth, Info.CloseRect.Top + TI.CloseHeight);
        Result := True;
      end;
      Inc(Extent, TI.TabExtent);
      Inc(I);
    end;
  end;
end;

function TKTabPanel.GetTabs(Index: Integer): TKString;
begin
  if (FPageControl <> nil) and (Index >= 0) and (Index < FPageControl.PageCount) then
    Result := FPageControl.Pages[Index].Caption
  else
    Result := '';
end;

function TKTabPanel.IndexOfTabAt(X, Y: Integer): Integer;
var
  I: Integer;
  Info: TKTabPaintInfo;
  Pt: TPoint;
begin
  Result := -1;
  if FPageControl <> nil then
  begin
    Pt := Point(X, Y);
    for I := 0 to FPageControl.PageCount - 1 do
      if getTabPaintInfo(Canvas, I, Info) then
        if PtInRect(Info.TabRect, Pt) then
        begin
          Result := I;
          Exit;
        end;
  end;
end;

procedure TKTabPanel.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = VK_ESCAPE) and MouseCapture then
  begin
    Cursor := crDefault;
    MouseCapture := False;
  end;
end;

procedure TKTabPanel.LeftScrollButtonClick(Sender: TObject);
begin
  FirstVisibleTab := FirstVisibleTab - 1;
end;

procedure TKTabPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
  Info: TKTabPaintInfo;
  MousePt: TPoint;
begin
  inherited;
  FPageToClose := -1;
  FDraggedTab := -1;
  FLastDraggedTab := -1;
  if (FPageControl <> nil) and (Button = mbLeft) then
  begin
    MouseCapture := True;
    MousePt := Point(X, Y);
    for I := 0 to FPageControl.PageCount - 1 do
      if getTabPaintInfo(Canvas, I, Info) then
      begin
        {if I > FLastFullyVisibleTab then
          FirstVisibleTab := FFirstVisibleTab + I - FLastFullyVisibleTab;}
        if PtInRect(Info.CloseRect, MousePt) then
        begin
          FPageToClose := I;
          Break;
        end
        else if PtInRect(Info.TabRect, MousePt) then
        begin
          if toDrag in FOptions.Options then
          begin
            FDraggedTab := I;
            FDraggedPos := MousePt;
            if CanFocus then
              SetFocus;
          end;
          FPageControl.DoTabClick(I);
          FPageControl.ActivePageIndex := I;
          Break;
        end;
      end;
  end;
end;

procedure TKTabPanel.MouseFormLeave;
var
  MousePt: TPoint;
begin
  inherited;
  MousePt := ScreenToClient(Mouse.CursorPos);
  MouseOver([], MousePt.X, MousePt.Y);
end;

procedure TKTabPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if MouseCapture then
    MouseMoveCaptured(Shift, X, Y)
  else
    MouseOver(Shift, X, Y);
end;

procedure TKTabPanel.MouseMoveCaptured(Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
  MousePt: TPoint;
  R: TRect;
  Info: TKTabPaintInfo;
begin
  if FPageControl <> nil then
  begin
    if FDraggedTab >= 0 then
    begin
      MousePt := Point(X, Y);
      R := Rect(FDraggedPos.X - 5, FDraggedPos.Y - 5, FDraggedPos.X + 5, FDraggedPos.Y + 5);
      if not PtInRect(R, MousePt) then
      begin
        Cursor := crDrag;
        // move the tab while dragging
        for I := 0 to FPageControl.PageCount - 1 do
          if getTabPaintInfo(Canvas, I, Info) then
            if PtInRect(Info.TabRect, MousePt) then
              if I = FDraggedTab then
                FLastDraggedTab := -1
              else if FLastDraggedTab < 0 then
              begin
                FPageControl.MovePage(FDraggedTab, I);
                FLastDraggedTab := FDraggedTab;
                FDraggedTab := I;
                Exit;
              end;
        end;
        // allow undock outside of tab area + some margin
        if toDragUndock in FOptions.Options then
        begin
          R := ClientRect;
          InflateRect(R, 50, 50);
          if not PtInRect(R, MousePt) then
          begin
            Cursor := crDefault;
            MouseCapture := False;
            if FPageControl.Pages[FDraggedTab].ControlCount > 0 then
              FPageControl.Pages[FDraggedTab].Controls[0].BeginDrag(True, 0);
          end;
        end;
      end;
  end;
end;

procedure TKTabPanel.MouseOver(Shift: TShiftState; X, Y: Integer);
var
  I, NewMouseIndex: Integer;
  NewMouseInCloseButton: Boolean;
  MousePt: TPoint;
  Info, OldInfo: TKTabPaintInfo;
begin
  if FPageControl <> nil then
  begin
    NewMouseIndex := -1;
    MousePt := Point(X, Y);
    for I := 0 to FPageControl.PageCount - 1 do
      if getTabPaintInfo(Canvas, I, Info) then
      begin
        if PtInRect(Info.TabRect, MousePt) then
        begin
          NewMouseIndex := I;
          Break;
        end
      end;
    if NewMouseIndex <> FMouseIndex then
    begin
      if FMouseIndex >= 0 then
        if getTabPaintInfo(Canvas, FMouseIndex, OldInfo) then
          InvalidateRectArea(OldInfo.TabRect);
      FMouseIndex := NewMouseIndex;
      if FMouseIndex >= 0 then
        InvalidateRectArea(Info.TabRect);
    end
    else if NewMouseIndex >= 0 then
    begin
      NewMouseInCloseButton := PtInRect(Info.CloseRect, MousePt);
      if NewMouseInCloseButton <> FMouseInCloseButton then
      begin
        FMouseInCloseButton := NewMouseInCloseButton;
        InvalidateRectArea(Info.CloseRect);
      end;
    end;
  end;
end;

procedure TKTabPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
  Info: TKTabPaintInfo;

begin
  inherited;
  if (FPageControl <> nil) and (Button = mbLeft) and MouseCapture then
  begin
    MouseCapture := False;
    Cursor := crDefault;
    for I := 0 to FPageControl.PageCount - 1 do
      if getTabPaintInfo(Canvas, I, Info) then
      begin
        if (FPageToClose = I) and PtInRect(Info.CloseRect, Point(X, Y)) then
        begin
          FPageControl.DoTabClose(I);
          Break;
        end
      end;
  end;
end;

procedure TKTabPanel.PaintAfterTabs(ACanvas: TCanvas; ALastTabIndex: Integer);
var
  ScrollButtonExtent: Integer;
  R: TRect;
  T, B, L: Integer;
begin
  if FPageControl <> nil then
  begin
    ACanvas.Pen.Color := FOptions.Colors.TabBorder;
    T := 0;
    B := 0;
    L := 0;
    case FPageControl.TabPosition of
      tpTop:
      begin
        T := 0;
        B := Height - 1;
        L := B;
      end;
      tpBottom:
      begin
        T := 1;
        B := Height;
        L := 0;
      end;
      tpLeft:
      begin
        { TODO }
        T := 0;
        B := Height - 1;
        L := B;
      end;
      tpRight:
      begin
        { TODO }
        T := 1;
        B := Height;
        L := 0;
      end;
    end;
    with ACanvas do
    begin
      if FAllTabsExtent <= Width then
      begin
        MoveTo(FAllTabsExtent, L);
        LineTo(Width, L);
      end else
      begin
        // paint background for scroll buttons
        // this will erase part of the last visible tab
        Brush.Color := Color;
        ScrollButtonExtent := 2 * FOptions.Padding + FOptions.ScrollButtonSize;
        R := Rect(0, T, ScrollButtonExtent, B);
        FillRect(R);
        MoveTo(R.Left, L);
        LineTo(R.Right, L);
        R := Rect(Width - ScrollButtonExtent, T, Width, B);
        FillRect(R);
        if FFullyVisibleTabsExtent = 0 then
          MoveTo(R.Left, L)
        else
          MoveTo(FFullyVisibleTabsExtent + ScrollButtonExtent, L);
        LineTo(R.Right, L);
      end;
    end;
  end;
end;

function TKTabPanel.PaintTab(ACanvas: TCanvas; ATabIndex: Integer): Boolean;
var
  Info: TKTabPaintInfo;
  State: TKTabState;
  MousePt: TPoint;
begin
  Result := getTabPaintInfo(ACanvas, ATabIndex, Info);
  if Result then
  begin
    MousePt := ScreenToClient(Mouse.CursorPos);
    if FPageControl.ActivePageIndex = ATabIndex then
      State := tsSelected
    else if not MouseCapture and FPageControl.HotTrack and PtInRect(Info.TabRect, MousePt) then
      State := tsHot
    else
      State := tsNormal;
    if not IsRectEmpty(Info.TabRect) then with ACanvas do
    begin
      PaintTabBackground(ACanvas, Info.TabRect, State);
      if not IsRectEmpty(Info.ImageRect) then
        PaintTabImage(ACanvas, Info.ImageRect, ATabIndex);
      if not IsRectEmpty(Info.TextRect) then
        PaintTabText(ACanvas, Info.TextRect, ATabIndex, State);
      if not IsRectEmpty(Info.CloseRect) then
        PaintTabCloseButton(ACanvas, Info.CloseRect);
    end;
  end;
end;

procedure TKTabPanel.PaintTabBackground(ACanvas: TCanvas; const ARect: TRect; AState: TKTabState);
var
  R: TRect;
  Descent: Integer;
  StartColor, EndColor: TColor;
begin
  if (FPageControl <> nil) and not IsRectEmpty(ARect) then with ACanvas do
  begin
    Pen.Color := FOptions.Colors.TabBorder;
    R := ARect;
    if AState = tsSelected then
      Descent := 1
    else
      Descent := 2;
    case FPageControl.TabPosition of
      tpTop:
      begin
        MoveTo(R.Left, R.Bottom - 1);
        LineTo(R.Left, R.Top + 2 + Descent);
        LineTo(R.Left + 2, R.Top + Descent);
        LineTo(R.Right - 2, R.Top + Descent);
        LineTo(R.Right, R.Top + 2 + Descent);
        LineTo(R.Right, R.Bottom - 1);
        if AState <> tsSelected then
          LineTo(R.Left, R.Bottom - 1);
        R := Rect(R.Left + 2, R.Top + 2 + Descent, R.Right - 1, R.Bottom);
      end;
      tpBottom:
      begin
        MoveTo(R.Left, R.Top);
        LineTo(R.Left, R.Bottom - 2 - Descent);
        LineTo(R.Left + 2, R.Bottom - Descent);
        LineTo(R.Right - 2, R.Bottom - Descent);
        LineTo(R.Right, R.Bottom - 2 - Descent);
        LineTo(R.Right, R.Top);
        if AState <> tsSelected then
          LineTo(R.Left, R.Top);
        R := Rect(R.Left + 2, R.Top + 1, R.Right - 1, R.Bottom - 1 - Descent);
      end;
      tpLeft: {TODO};
      tpRight: {TODO};
    end;
    case AState of
      tsHot:
      begin
        StartColor := FOptions.Colors.HotTop;
        EndColor := FOptions.Colors.HotBottom;
      end;
      tsSelected:
      begin
        StartColor := FOptions.Colors.SelectedTop;
        EndColor := FOptions.Colors.SelectedBottom;
      end
    else
      StartColor := FOptions.Colors.NormalTop;
      EndColor := FOptions.Colors.NormalBottom;
    end;
    if toBrightTopColors in FOptions.Options then
      EndColor := BrightColor(ColorToRGB(StartColor), 0.75, bsOfTop);
    DrawGradientRect(ACanvas, R, ColorToRGB(StartColor), ColorToRGB(EndColor), 10, False);
    // simulate rounded corners
    Canvas.Pixels[R.Left, R.Top] := clBtnFace;
    Canvas.Pixels[R.Right - 1, R.Top] := clBtnFace;
  end;
end;

procedure TKTabPanel.PaintTabCloseButton(ACanvas: TCanvas; const ARect: TRect);
var
  Images: TImageList;
  MousePt: TPoint;
begin
  if (FPageControl <> nil) and not IsRectEmpty(ARect) then with ACanvas do
  begin
    MousePt := ScreenToClient(Mouse.CursorPos);
    if (not MouseCapture and PtInRect(ARect, MousePt)) or (FPageControl.DisabledImages = nil) then
      Images := FPageControl.Images
    else
      Images := FPageControl.DisabledImages;
    Images.Draw(ACanvas, ARect.Left, ARect.Top, FOptions.CloseButtonIndex);
  end;
end;

procedure TKTabPanel.PaintTabText(ACanvas: TCanvas; const ARect: TRect; ATabIndex: Integer; AState: TKTabState);
var
  TextBox: TKTextBox;
begin
  if (FPageControl <> nil) and (ATabIndex >= 0) and (ATabIndex < FPageControl.PageCount) and not IsRectEmpty(ARect) then with ACanvas do
  begin
    TextBox := TKTextBox.Create;
    try
      if AState = tsSelected then
        Font.Color := FOptions.Colors.SelectedText
      else
        Font.Color := FOptions.Colors.NormalText;
      TextBox.HAlign := halLeft;
      TextBox.VAlign := valCenter;
      TextBox.Text := FPageControl.Pages[ATabIndex].Caption;
      TextBox.Draw(ACanvas, ARect);
    finally
      TextBox.Free;
    end;
  end
end;

procedure TKTabPanel.PaintTabImage(ACanvas: TCanvas; const ARect: TRect; ATabIndex: Integer);
begin
  if (FPageControl <> nil) and (ATabIndex >= 0) and (ATabIndex < FPageControl.PageCount) and not IsRectEmpty(ARect) then
  begin
    FPageControl.Images.Draw(ACanvas, ARect.Left, ARect.Top, FPageControl.GetImageIndex(ATabIndex));
  end
end;

procedure TKTabPanel.PaintToCanvas(ACanvas: TCanvas);
var
  I: Integer;
begin
  inherited;
  if FPageControl <> nil then with ACanvas do
  begin
    I := 0;
    while (I < FPageControl.PageCount) and PaintTab(ACanvas, I) do
      Inc(I);
    PaintAfterTabs(ACanvas, I - 1);
  end;
end;

procedure TKTabPanel.RightScrollButtonClick(Sender: TObject);
begin
  FirstVisibleTab := FirstVisibleTab + 1;
end;

procedure TKTabPanel.SetFirstVisibleTab(Value: Integer);
begin
  Value := MinMax(Value, 0, FMaxFirstVisibleTab);
  if Value <> FFirstVisibleTab then
  begin
    FScrolling := True;
    try
      FFirstVisibleTab := Value;
      UpdateTabPanel;
    finally
      FScrolling := False;
    end;
  end;
end;

procedure TKTabPanel.SetPageControl(const Value: TKCustomPageControl);
begin
  if Value <> FPageControl then
  begin
    FPageControl := Value;
    UpdateTabPanel;
  end;
end;

procedure TKTabPanel.SetOptions(const Value: TKTabPanelOptions);
begin
  FOptions.Assign(Value);
end;

function TKTabPanel.TabRect(AIndex: Integer): TRect;
var
  Info: TKTabPaintInfo;
begin
  Result := CreateEmptyRect;
  if FPageControl <> nil then
    if getTabPaintInfo(Canvas, AIndex, Info) then
      Result := Info.TabRect;
end;

procedure TKTabPanel.UpdateScrollRange;
var
  R: TRect;
begin
  UpdateTabPanelLayout(Canvas);
  if FAllTabsExtent > Width then
  begin
    R.TopLeft := Point(FOptions.Padding, (Height - FOptions.ScrollButtonSize) div 2);
    R.BottomRight := Point(R.Left + FOptions.ScrollButtonSize, R.Top + FOptions.ScrollButtonSize);
    FLeftScrollButton.BoundsRect := R;
    FLeftScrollButton.Visible := True;
    R.TopLeft := Point(Width - FOptions.Padding - FOptions.ScrollButtonSize, (Height - FOptions.ScrollButtonSize) div 2);
    R.BottomRight := Point(R.Left + FOptions.ScrollButtonSize, R.Top + FOptions.ScrollButtonSize);
    FRightScrollButton.BoundsRect := R;
    FRightScrollButton.Visible := True;
  end else
  begin
    FLeftScrollButton.Visible := False;
    FRightScrollButton.Visible := False;
  end;
  Invalidate;
end;

procedure TKTabPanel.UpdateSize;
begin
  inherited;
  UpdateScrollRange;
end;

procedure TKTabPanel.UpdateTabPanel;
begin
  if FPageControl <> nil then
  begin
    FLeftScrollButton.Images := FPageControl.Images;
    FRightScrollButton.Images := FPageControl.Images;
  end;
  FLeftScrollButton.ImageIndex := FOptions.LeftButtonIndex;
  FRightScrollButton.ImageIndex := FOptions.RightButtonIndex;
  UpdateScrollRange;
end;

procedure TKTabPanel.UpdateTabPanelLayout(ACanvas: TCanvas);
var
  I, ScrollButtonExtent, ActivePageExtent, ActivePagePos: Integer;
  MaxFirstVisibleTabSet: Boolean;
  TI: TKTabInfo;
begin
  FMouseIndex := -1;
  FMouseInCloseButton := False;
  FAllTabsExtent := 0;
  FInvisibleTabsExtent := 0;
  FMaxFirstVisibleTab := 0;
  FLastFullyVisibleTab := 0;
  FVisibleTabsExtent := 0;
  FFullyVisibleTabsExtent := 0;
  if FPageControl <> nil then
  begin
    MaxFirstVisibleTabSet := False;
    ScrollButtonExtent := 2 * FOptions.Padding + FOptions.ScrollButtonSize;
    ActivePageExtent := 0;
    ActivePagePos := 0;
    // calculate extent of all tabs and initialize some helper variables
    for I := FPageControl.PageCount - 1 downto 0 do
      if GetTabInfo(ACanvas, I, TI) then
      begin
        Inc(FAllTabsExtent, TI.TabExtent);
        if not MaxFirstVisibleTabSet and (FAllTabsExtent > Width - 2 * ScrollButtonExtent) then
        begin
          FMaxFirstVisibleTab := I + 1;
          MaxFirstVisibleTabSet := True;
        end;
        if I = FPageControl.ActivePageIndex then
          ActivePageExtent := TI.TabExtent
        else if I < FPageControl.ActivePageIndex then
          Inc(ActivePagePos, TI.TabExtent);
      end;
    if FAllTabsExtent <= Width then
    begin
      FTabAreaExtent := Width;
      FMaxFirstVisibleTab := 0;
    end else
      FTabAreaExtent := Width - 2 * ScrollButtonExtent;
    // limit important tab indexes
    FMaxFirstVisibleTab := MinMax(FMaxFirstVisibleTab, 0, FPageControl.PageCount - 1);
    FFirstVisibleTab := MinMax(FFirstVisibleTab, 0, FMaxFirstVisibleTab);
    // ensure active page is visible if hidden to the left, do only when FirstVisibleTab is not being set explictly
    if not FScrolling then
    begin
      if (FPageControl.ActivePageIndex >= 0) and (FPageControl.ActivePageIndex < FFirstVisibleTab) then
        FFirstVisibleTab := FPageControl.ActivePageIndex;
    end;
    // calculate invisible tabs to the left
    for I := 0 to FFirstVisibleTab - 1 do
      if GetTabInfo(ACanvas, I, TI) then
        Inc(FInvisibleTabsExtent, TI.TabExtent);
    // ensure active page is visible if hidden to the right, do only when FirstVisibleTab is not being set explictly
    if not FScrolling then
    begin
      while (FFirstVisibleTab < FMaxFirstVisibleTab) and (FTabAreaExtent > ActivePageExtent) and (ActivePagePos + ActivePageExtent - FInvisibleTabsExtent > FTabAreaExtent) do
      begin
        if GetTabInfo(ACanvas, FFirstVisibleTab, TI) then
          Inc(FinvisibleTabsExtent, TI.TabExtent);
        Inc(FFirstVisibleTab);
      end;
    end;
    // calculate remaining tabs, including invisible ones to the right
    for I := FFirstVisibleTab to FPageControl.PageCount - 1 do
      if GetTabInfo(ACanvas, I, TI) then
      begin
        Inc(FVisibleTabsExtent, TI.TabExtent);
        if FVisibleTabsExtent < FTabAreaExtent then
        begin
          FFullyVisibleTabsExtent := FVisibleTabsExtent;
          FLastFullyVisibleTab := I;
        end;
      end;
  end;
end;

{ TKTabSheet }

constructor TKTabSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Align := alClient;
{$IFDEF FPC}
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csDesignFixedBounds, csDoubleClicks, csDesignInteractive];
{$ELSE}
  ControlStyle := [csAcceptsControls, csOpaque];
{$ENDIF}
  Visible := False;
  FHighlighted := False;
end;

procedure TKTabSheet.CMTextChanged(var Message: TLMessage);
begin
  if FPageControl <> nil then
    FPageControl.UpdateTabPanel;
end;

procedure TKTabSheet.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
{$IFDEF FPC}
  if Message.DC <> 0 then
    EraseBackground(Message.DC);
{$ELSE}
  inherited;
{$ENDIF}
end;

procedure TKTabSheet.WMPaint(var Msg: TLMPaint);
{$IFDEF FPC}
var
  Notebook: TKCustomPageControl;
{$ENDIF}
begin
{$IFDEF FPC}
  if Parent is TKCustomPageControl then
  begin
    NoteBook := TKCustomPageControl(Parent);
    if (NoteBook.ActivePageIndex >= 0) and (NoteBook.Pages[NoteBook.ActivePageIndex] = Self) then
      inherited;
  end else
{$ENDIF}
    inherited;
end;

procedure TKTabSheet.CMShowingChanged(var Message: TLMessage);
begin
  inherited;
  if Showing then
  begin
    try
      DoShow
    except
      Application.HandleException(Self);
    end;
  end
  else
  if not Showing then
  begin
    try
      DoHide;
    except
      Application.HandleException(Self);
    end;
  end;
end;

destructor TKTabSheet.Destroy;
begin
  if FPageControl <> nil then
  begin
    if FPageControl.FUndockingPage = Self then
      FPageControl.FUndockingPage := nil;
    FPageControl.RemovePage(Self);
  end;
  inherited Destroy;
end;

procedure TKTabSheet.DoHide;
begin
  if Assigned(FOnHide) then
    FOnHide(Self);
end;

procedure TKTabSheet.DoShow;
begin
  if Assigned(FOnShow) then
    FOnShow(Self);
end;

function TKTabSheet.GetPageIndex: Integer;
begin
  if FPageControl <> nil then
    Result := FPageControl.FPages.IndexOf(Self)
  else
    Result := -1;
end;

procedure TKTabSheet.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do
    style := style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TKTabSheet.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  if Reader.Parent is TKCustomPageControl then
    PageControl := TKCustomPageControl(Reader.Parent);
end;

procedure TKTabSheet.SetHighlighted(Value: Boolean);
begin
  if Value <> FHighLighted then
  begin
    FHighlighted := Value;
    if FPageControl <> nil then
      FPageControl.UpdateTabPanel;
  end;
end;

procedure TKTabSheet.SetImageIndex(Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    if FPageControl <> nil then
      FPageControl.UpdateTabPanel;
  end;
end;

procedure TKTabSheet.SetPageControl(APageControl: TKCustomPageControl);
begin
  if FPageControl <> APageControl then
  begin
    if FPageControl <> nil then
      FPageControl.RemovePage(Self);
    FPageControl := APageControl;
    if FPageControl <> nil then
      FPageControl.InsertPage(Self);
  end;
end;

procedure TKTabSheet.SetPageIndex(Value: Integer);
begin
  if FPageControl <> nil then
  begin
    Value := MinMax(Value, 0, FPageControl.PageCount - 1);
    FPageControl.MovePage(PageIndex, Value);
  end;
end;

{ TKTabSheets }

function TKTabSheets.Add(AItem: TKTabSheet): Integer;
begin
  Result := inherited Add(AItem);
end;

function TKTabSheets.GetItem(Index: Integer): TKTabSheet;
begin
  Result := TKTabSheet(inherited GetItem(Index));
end;

function TKTabSheets.IndexOf(AItem: TKTabSheet): Integer;
begin
  Result := inherited IndexOf(AItem);
end;

procedure TKTabSheets.SetItem(Index: Integer; const Value: TKTabSheet);
begin
  inherited SetItem(Index, Value);
end;

{ TKCustomPageControl }

constructor TKCustomPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle := bsNone;
  ControlStyle := ControlStyle + [csOpaque, csDoubleClicks];
{$IFDEF FPC}
  ControlStyle := ControlStyle + [csDesignInteractive];
{$ENDIF}
  Width := 400;
  Height := 300;
  FActivateNewDocked := True;
  FActivePageIndex := -1;
  FDeletingPage := False;
  FDisabledImages := nil;
  FHotTrack := True;
  FImages := nil;
  FLoadedActivePageIndex := -1;
  FPages := TKTabSheets.Create;
  FPages.OwnsObjects := False;
  FTabHeight := cDefaultTabHeight;
  FTabPosition := cDefaultTabPosition;
  FTabWidth := cDefaultTabWidth;
  FOnChange := nil;
  FOnChanging := nil;
  FOnGetImageIndex := nil;
  FOnTabClick := nil;
  FTabPanel := TKTabPanel.Create(nil);
  UpdateTabPanelPosition;
end;

destructor TKCustomPageControl.Destroy;
begin
  FreeAndNil(FTabPanel);
  while FPages.Count > 0 do
    FPages[0].PageControl := nil;
  FPages.Free;
  inherited Destroy;
end;

function TKCustomPageControl.AddPage(AOwner: TComponent): TKTabSheet;
begin
  Result := TKTabSheet.Create(AOwner);
  Result.PageControl := Self;
end;

procedure TKCustomPageControl.DeletePage(AIndex: Integer);
begin
  FreePage(ReleasePage(AIndex));
end;

procedure TKCustomPageControl.DeletePage(APage: TKTabSheet);
begin
  FreePage(ReleasePage(FPages.IndexOf(APage)));
end;

function TKCustomPageControl.CanChange: Boolean;
begin
  Result := True;
  if Assigned(FOnChanging) and not (csDestroying in ComponentState) then
    FOnChanging(Self, Result);
end;

procedure TKCustomPageControl.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TKCustomPageControl.ChangeActivePage(Page: TKTabSheet);
var
  ParentForm: TCustomForm;
  CurPage: TKTabSheet;
  Index: Integer;
begin
  Index := FPages.IndexOf(Page);
  if (Index >= 0) and (Index <> FActivePageIndex) and CanChange then
  begin
    CurPage := ActivePage;
    if CurPage <> nil then
      CurPage.Visible := False;
    FActivePageIndex := Index;
    CurPage := ActivePage;
    if CurPage <> nil then
    begin
      CurPage.BringToFront;
      CurPage.Visible := True;
      ParentForm := GetParentForm(Self);
      if (ParentForm <> nil) and (CurPage <> nil) and (ParentForm.ActiveControl = CurPage) then
      begin
        CurPage.SelectFirst;
        if Page.CanFocus then
          ParentForm.ActiveControl := Page
        else
          ParentForm.ActiveControl := Self;
      end;
    end;
    Change;
    UpdateAllDesignerFlags;
    UpdateTabPanel;
  end;
end;

procedure TKCustomPageControl.CMDialogKey(var Message: TCMDialogKey);
begin
  if (Focused {$IFnDEF FPC} or IsChild(Handle, GetFocus) {$ENDIF}) and
    (Message.CharCode = VK_TAB) and (GetKeyState(VK_CONTROL) < 0) then
  begin
    SelectNextPage(GetKeyState(VK_SHIFT) >= 0);
    Message.Result := 1;
  end else
    inherited;
end;

{$IFDEF FPC}

procedure TKCustomPageControl.CMDesignHitTest(var Message: TCMDesignHitTest);
begin
  Message.Result := 1;
end;

{$ELSE}
procedure TKCustomPageControl.CMDockClient(var Message: TCMDockClient);
var
  DockCtl: TControl;
  I: Integer;
begin
  with Message do
  begin
    Result := 0;
    DockCtl := DockSource.Control;
    { First, look and see if the page is already docked. If it is,
      then simply move the page index to the end }
    for I := 0 to PageCount - 1 do
    begin
      if DockCtl.Parent = Pages[I] then
      begin
        { We did find it; just move the page to the end }
        Pages[I].PageIndex := PageCount - 1;
        Exit;
      end;
    end;

    FNewDockSheet := TKTabSheet.Create(Self);
    try
      try
        if DockCtl is TCustomForm then
          FNewDockSheet.Caption := TCustomForm(DockCtl).Caption;
        FNewDockSheet.PageControl := Self;
        DockCtl.Dock(Self, DockSource.DockRect);
      except
        FNewDockSheet.Free;
        raise;
      end;
      if DockCtl.Visible then
        if FActivateNewDocked then
          ActivePage := FNewDockSheet
        else
          UpdateTabPanel;
      DockCtl.Align := alClient;
    finally
      FNewDockSheet := nil;
    end;
  end;
end;

procedure TKCustomPageControl.CMDockNotification(var Message: TCMDockNotification);
var
  I: Integer;
  S: TKString;
  Page: TKTabSheet;
begin
  Page := GetPageFromDockClient(Message.Client);
  if Page <> nil then
    case Message.NotifyRec.ClientMsg of
      WM_SETTEXT:
        begin
          S := PChar(Message.NotifyRec.MsgLParam);
          // Search for first CR/LF and end string there
          for I := 1 to Length(S) do
            if CharInSetEx(S[I], [#13, #10]) then
            begin
              SetLength(S, I - 1);
              Break;
            end;
          Page.Caption := S;
        end;
    end;
  inherited;
end;

procedure TKCustomPageControl.CMUnDockClient(var Message: TCMUnDockClient);
var
  Page: TKTabSheet;
begin
  with Message do
  begin
    Result := 0;
    Page := GetPageFromDockClient(Client);
    if Page <> nil then
    begin
      FUndockingPage := Page;
      Client.Align := alNone;
    end;
  end;
end;
{$ENDIF}

function TKCustomPageControl.ReleasePage(Index: Integer): TKTabSheet;
var
  NextPage: TKTabSheet;
begin
  Result := nil;
  if not FDeletingPage and (Index >= 0) and (Index < FPages.Count) then
  begin
    FDeletingPage := True;
    try
      Result := FPages[Index];
      NextPage := FindNextPage(Result, True);
      if NextPage = Result then
        NextPage := nil;
      Result.Parent := nil;
      Result.PageControl := nil;
      FPages.Delete(Index);
      if (Index = FActivePageIndex) or (FActivePageIndex >= FPages.Count) then
        FActivePageIndex := -1;
      ActivePage := NextPage;
      UpdateTabPanel;
    finally
      FDeletingPage := False;
    end;
  end;
end;

procedure TKCustomPageControl.FreePage(Page: TKTabSheet);
begin
{$IFDEF FPC}
  Application.ReleaseComponent(Page);
{$ELSE}
  Page.Free;
{$ENDIF}
end;

procedure TKCustomPageControl.DoAddDockClient(Client: TControl; const ARect: TRect);
begin
  if FNewDockSheet <> nil then
    Client.Parent := FNewDockSheet;
end;

procedure TKCustomPageControl.DockOver(Source: TDragDockObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  R: TRect;
begin
  GetWindowRect(Handle, R);
  Source.DockRect := R;
  DoDockOver(Source, X, Y, State, Accept);
end;

procedure TKCustomPageControl.DoRemoveDockClient(Client: TControl);
begin
  if (FUndockingPage <> nil) and not (csDestroying in ComponentState) then
  begin
    RemovePage(FUndockingPage);
    FUndockingPage := nil;
  end;
end;

procedure TKCustomPageControl.DoTabClick(AIndex: Integer);
begin
  if Assigned(FOnTabClick) then
    FOnTabClick(Self, AIndex);
end;

procedure TKCustomPageControl.DoTabClose(AIndex: Integer);
var
  CanClose: Boolean;
begin
  CanClose := True;
  if Assigned(FOnTabCloseQuery) then
    FOnTabCloseQuery(Self, AIndex, CanClose);
  if CanClose then
    DeletePage(AIndex);
end;

function TKCustomPageControl.FindNextPage(CurPage: TKTabSheet; GoForward: Boolean): TKTabSheet;
var
  I, StartIndex: Integer;
begin
  if FPages.Count <> 0 then
  begin
    StartIndex := FPages.IndexOf(CurPage);
    if StartIndex = -1 then
      if GoForward then
        StartIndex := FPages.Count - 1
      else
        StartIndex := 0;
    I := StartIndex;
    repeat
      if GoForward then
      begin
        Inc(I);
        if I = FPages.Count then
          I := 0;
      end else
      begin
        if I = 0 then
          I := FPages.Count;
        Dec(I);
      end;
      Result := FPages[I];
      if Result <> nil then
        Exit;
    until I = StartIndex;
  end;
  Result := nil;
end;

function TKCustomPageControl.GetActivePage: TKTabSheet;
begin
  if FActivePageIndex >= 0 then
    Result := FPages[FActivePageIndex]
  else
    Result := nil;
end;

procedure TKCustomPageControl.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  for I := 0 to FPages.Count - 1 do
    Proc(TComponent(FPages[I]));
end;

function TKCustomPageControl.GetImageIndex(TabIndex: Integer): Integer;
begin
  Result := FPages[TabIndex].ImageIndex;
  if Assigned(FOnGetImageIndex) then
    FOnGetImageIndex(Self, TabIndex, Result);
end;

function TKCustomPageControl.GetPageFromDockClient(Client: TControl): TKTabSheet;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to PageCount - 1 do
  begin
    if (Client.Parent = Pages[I]) and (Client.HostDockSite = Self) then
    begin
      Result := Pages[I];
      Exit;
    end;
  end;
end;

function TKCustomPageControl.GetPage(Index: Integer): TKTabSheet;
begin
  Result := TKTabSheet(FPages[Index]);
end;

function TKCustomPageControl.GetPageCount: Integer;
begin
  Result := FPages.Count;
end;

procedure TKCustomPageControl.GetSiteInfo(Client: TControl; var InfluenceRect: TRect;
  MousePos: TPoint; var CanDock: Boolean);
begin
  CanDock := GetPageFromDockClient(Client) = nil;
  inherited GetSiteInfo(Client, InfluenceRect, MousePos, CanDock);
end;

function TKCustomPageControl.GetTabPanelOptions: TKTabPanelOptions;
begin
  if FTabPanel <> nil then
    Result := FTabPanel.Options
  else
    Result := nil;
end;

function TKCustomPageControl.IndexOfTabAt(X, Y: Integer): Integer;
begin
  if FTabPanel <> nil then
    Result := FTabPanel.IndexOfTabAt(X, Y)
  else
    Result := -1;
end;

procedure TKCustomPageControl.InsertPage(Page: TKTabSheet);
begin
  FPages.Add(Page);
  Page.PageControl := Self;
  Page.Parent := Self;
end;

procedure TKCustomPageControl.Loaded;
begin
  inherited;
  ActivePageIndex := FLoadedActivePageIndex;
end;

{$IFDEF FPC}
procedure TKCustomPageControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  MousePt: TPoint;
  Form: TCustomForm;
begin
  if (csDesigning in ComponentState) and (FTabPanel <> nil) then
  begin
    MousePt := Point(X,Y);
    if PtInRect(FTabPanel.BoundsRect, MousePt) then
    begin
      MousePt := FTabPanel.ScreenToClient(ClientToScreen(MousePt));
      FTabPanel.MouseDown(Button, Shift, MousePt.X, MousePt.Y);
      Form := GetParentForm(Self);
      if Form <> nil then
        Form.Designer.SelectOnlyThisComponent(Self);
    end else
      inherited;
  end else
    inherited;
end;
{$ENDIF}

procedure TKCustomPageControl.MovePage(CurIndex, NewIndex: Integer);
begin
  FPages.Move(CurIndex, NewIndex);
  if CurIndex = FActivePageIndex then
    FActivePageIndex := NewIndex
  else if NewIndex = FActivePageIndex then
    FActivePageIndex := CurIndex;
  UpdateTabPanel;
end;

procedure TKCustomPageControl.PaintToCanvas(ACanvas: TCanvas);
begin
  if FPages.Count = 0 then
    ACanvas.FillRect(ClientRect);
end;

procedure TKCustomPageControl.RemovePage(Page: TKTabSheet);
begin
  ReleasePage(FPages.IndexOf(Page));
end;

procedure TKCustomPageControl.SelectNextPage(GoForward: Boolean);
var
  Page: TKTabSheet;
begin
  Page := FindNextPage(ActivePage, GoForward);
  if (Page <> nil) and (Page <> ActivePage) then
    ActivePage := Page;
end;

procedure TKCustomPageControl.SetActivePage(Page: TKTabSheet);
begin
  if (Page <> nil) and (Page.PageControl <> Self) then
    Exit;
  ChangeActivePage(Page);
end;

procedure TKCustomPageControl.SetActivePageIndex(const Value: Integer);
begin
  if csLoading in ComponentState then
    FLoadedActivePageIndex := Value
  else if (Value >= 0) and (Value < PageCount) then
    ActivePage := Pages[Value]
  else
    ActivePage := nil;
end;

procedure TKCustomPageControl.SetChildOrder(Child: TComponent; Order: Integer);
begin
  TKTabSheet(Child).PageIndex := Order;
end;

procedure TKCustomPageControl.SetDisabledImages(const Value: TImageList);
begin
  if FDisabledImages <> Value then
  begin
    FDisabledImages := Value;
    UpdateTabPanel;
  end;
end;

procedure TKCustomPageControl.SetHotTrack(const Value: Boolean);
begin
  if Value <> FHotTrack then
  begin
    FHotTrack := Value;
    UpdateTabPanel;
  end;
end;

procedure TKCustomPageControl.SetImages(const Value: TImageList);
begin
  if FImages <> Value then
  begin
    FImages := Value;
    UpdateTabPanel;
  end;
end;

procedure TKCustomPageControl.SetTabHeight(const Value: Integer);
begin
  if Value <> FTabHeight then
  begin
    FTabHeight := Value;
    UpdateTabPanelPosition;
  end;
end;

procedure TKCustomPageControl.SetTabPanel(const Value: TKTabPanel);
begin
  FTabPanel.Assign(Value);
  UpdateTabPanelPosition;
end;

procedure TKCustomPageControl.SetTabPanelOptions(const Value: TKTabPanelOptions);
begin
  if FTabPanel <> nil then
    FTabPanel.Options := Value;
end;

procedure TKCustomPageControl.SetTabPosition(Value: TTabPosition);
begin
  // these not supported yet
  if Value in [tpLeft, tpRight] then
    Exit;
  if Value <> FTabPosition then
  begin
    FTabPosition := Value;
    UpdateTabPanelPosition;
  end;
end;

procedure TKCustomPageControl.SetTabWidth(const Value: Integer);
begin
  if Value <> FTabWidth then
  begin
    FTabWidth := Value;
    UpdateTabPanel;
  end;
end;

procedure TKCustomPageControl.ShowControl(AControl: TControl);
begin
  if (AControl is TKTabSheet) and (TKTabSheet(AControl).PageControl = Self) then
    ActivePage := TKTabSheet(AControl);
  inherited ShowControl(AControl);
end;

function TKCustomPageControl.TabRect(Index: Integer): TRect;
begin
  if FTabPanel <> nil then
    Result := FTabPanel.TabRect(Index)
  else
    Result := CreateEmptyRect;
end;

procedure TKCustomPageControl.UpdateAllDesignerFlags;
var
  i: integer;
begin
  for i := 0 to FPages.Count - 1 do
    UpdateDesignerFlags(i);
end;

procedure TKCustomPageControl.UpdateDesignerFlags(APageIndex: integer);
var
  CurPage: TKTabSheet;
begin
  CurPage := Pages[APageIndex];
  if APageIndex <> FActivePageIndex then
    CurPage.ControlStyle := CurPage.ControlStyle + [csNoDesignVisible]
  else
    CurPage.ControlStyle := CurPage.ControlStyle - [csNoDesignVisible];
end;

procedure TKCustomPageControl.UpdateTabPanel;
begin
  if (FTabPanel <> nil) and not (csDestroying in ComponentState) then
  begin
    FTabPanel.UpdateTabPanel;
  end;
end;

procedure TKCustomPageControl.UpdateTabPanelPosition;
begin
  if FTabPanel <> nil then
  begin
    case FTabPosition of
      tpBottom:
      begin
        FTabPanel.Align := alBottom;
        FTabPanel.Height := FTabHeight;
      end;
      tpTop:
      begin
        FTabPanel.Align := alTop;
        FTabPanel.Height := FTabHeight;
      end;
      tpLeft:
      begin
        FTabPanel.Align := alLeft;
        FTabPanel.Width := FTabHeight;
      end;
      tpRight:
      begin
        FTabPanel.Align := alRight;
        FTabPanel.Width := FTabHeight;
      end;
    end;
    FTabPanel.PageControl := Self;
    FTabPanel.Parent := Self;
  end;
end;

procedure TKCustomPageControl.WMEraseBkgnd(var Msg: TLMessage);
begin
  Msg.Result := 1;
end;

end.
