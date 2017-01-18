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
  The Original Code is VirtualLogTree.pas. The Initial Developer of the Original
  Code is Paul Thornton. Portions created by the Initial Developer are
  Copyright (C), All Rights Reserved.
}

unit DDuce.Components.LogTree;

{$I ..\DDuce.inc}

interface

uses
  WinApi.Windows,
  System.Classes, System.SysUtils, System.Types,
  Vcl.Graphics, Vcl.ImgList, Vcl.Menus,

  VirtualTrees;

const
  DEFAULT_DATETIMEFORMAT = 'dd-mm-yyyy hh:nn:ss.zzz';

type
  TLogLevel = (
    llNone,
    llError,
    llInfo,
    llWarning,
    llDebug
  );

  TLogLevels = set of TLogLevel;

  TLogNodeData = record
    LogLevel  : TLogLevel;
    Timestamp : TDateTime;
    LogText   : string;
  end;

  PLogNodeData = ^TLogNodeData;

  TOnLogEvent = procedure(
    Sender           : TObject;
    var ALogText     : string;
    var ACancelEntry : Boolean;
    ALogLevel        : TLogLevel
  ) of object;
  TOnPopupMenuItemClickEvent = procedure(
    Sender    : TObject;
    AMenuItem : TMenuItem
  ) of object;

  TLogPopupmenu = class(TPopupMenu)
  private
    FOwner                : TComponent;
    FOnPopupMenuItemClick : TOnPopupMenuItemClickEvent;

    procedure OnMenuItemClick(Sender: TObject);

  public
    constructor Create(AOwner: TComponent); override;

    property OnPopupMenuItemClick: TOnPopupMenuItemClickEvent
      read FOnPopupMenuItemClick write FOnPopupMenuItemClick;
  end;

  TLogTree = class(TVirtualStringTree)
  private
    FOnBeforeLog             : TOnLogEvent;
    FOnAfterLog              : TNotifyEvent;
    FHTMLSupport             : Boolean;
    FAutoScroll              : Boolean;
    FRemoveControlCharacters : Boolean;
    FLogLevels               : TLogLevels;
    FAutoLogLevelColors      : Boolean;
    FShowDateColumn          : Boolean;
    FShowImages              : Boolean;
    FMaximumLines            : Cardinal;
    FDateTimeFormat          : string;

    function DrawHTML(const ARect: TRect; const ACanvas: TCanvas;
      const Text: string; Selected: Boolean): Integer;
    function GetCellText(const Node: PVirtualNode; const Column:
      TColumnIndex): string;
    procedure SetLogLevels(const Value: TLogLevels);
    procedure UpdateVisibleItems;
    procedure OnPopupMenuItemClick(Sender: TObject; AMenuItem: TMenuItem);
    procedure SetShowDateColumn(const Value: Boolean);
    procedure SetShowImages(const Value: Boolean);
    procedure AddDefaultColumns(const ColumnNames: array of string;
      const ColumnWidths: array of Integer);
    function IfThen(Condition: Boolean; TrueResult,
      FalseResult: Variant): Variant;
    function StripHTMLTags(const Value: string): string;
    function RemoveCtrlChars(const Value: string): string;
    function GetDateTimeFormat: string;
    procedure SetDateTimeFormat(const Value: string);

  protected
    procedure DoOnBeforeLog(
      var ALogText     : string;
      var ACancelEntry : Boolean;
      ALogLevel        : TLogLevel
    ); virtual;
    procedure DoOnAfterLog; virtual;
    procedure DoAfterCellPaint(
      ACanvas  : TCanvas;
      ANode    : PVirtualNode;
      Column   : TColumnIndex;
      CellRect : TRect
    ); override;
    procedure DoGetText(var pEventArgs : TVSTGetCellTextEventArgs); override;
    procedure DoFreeNode(Node : PVirtualNode); override;
    function DoGetImageIndex(
      Node        : PVirtualNode;
      Kind        : TVTImageKind;
      Column      : TColumnIndex;
      var Ghosted : Boolean;
      var Index   : Integer
    ): TCustomImageList; override;
    procedure DoPaintText(
      Node         : PVirtualNode;
      const Canvas : TCanvas;
      Column       : TColumnIndex;
      TextType     : TVSTTextType
    ); override;
    procedure Loaded; override;
    procedure DoMeasureItem(
      TargetCanvas   : TCanvas;
      Node           : PVirtualNode;
      var NodeHeight : Integer
    ); override;
    procedure DoInitNode(
      Parent         : PVirtualNode;
      Node           : PVirtualNode;
      var InitStates : TVirtualNodeInitStates
    ); override;

  public
    constructor Create(AOwner: TComponent); override;

    procedure Log(
      AValue     : string;
      ALogLevel  : TLogLevel = llInfo;
      ATimestamp : TDateTime = 0
    );
    procedure LogFmt(
      AValue      : string;
      const AArgs : array of const;
      ALogLevel   : TLogLevel = llInfo;
      ATimestamp  : TDateTime = 0
    );
    procedure SaveToFileWithDialog;
    procedure SaveToFile(const AFilename: string);
    procedure SaveToStrings(const AStrings: TStrings);
    procedure CopyToClipboard; reintroduce;
    procedure Init;

  published
    property OnBeforeLog: TOnLogEvent
      read FOnBeforeLog write FOnBeforeLog;

    property OnAfterLog: TNotifyEvent
      read FOnAfterLog write FOnAfterLog;

    property HTMLSupport: Boolean
      read FHTMLSupport write FHTMLSupport;

    property AutoScroll: Boolean
      read FAutoScroll write FAutoScroll;

    property RemoveControlCharacters: Boolean
      read FRemoveControlCharacters write FRemoveControlCharacters;

    property LogLevels: TLogLevels
      read FLogLevels write SetLogLevels;

    property AutoLogLevelColors: Boolean
      read FAutoLogLevelColors write FAutoLogLevelColors;

    property ShowDateColumn: Boolean
      read FShowDateColumn write SetShowDateColumn;

    property ShowImages: Boolean
      read FShowImages write SetShowImages;

    property MaximumLines: Cardinal
      read FMaximumLines write FMaximumLines;

    property DateTimeFormat: string
      read GetDateTimeFormat write SetDateTimeFormat;
  end;

implementation

uses
  System.UITypes,
  Vcl.Dialogs, Vcl.Clipbrd;

resourcestring
  SSaveLog         = '&Save';
  SCopyToClipboard = '&Copy';
  SClear           = 'Clea&r';
  STextFilesTxt    = 'Text files (*.txt)|*.txt|All files (*.*)|*.*';
  SSave            = 'Save';
  SDate            = 'Date';
  SLog             = 'Log';

constructor TLogTree.Create(AOwner: TComponent);
begin
  inherited;
  FDateTimeFormat          := DEFAULT_DATETIMEFORMAT;
  FAutoScroll              := True;
  FHTMLSupport             := True;
  FRemoveControlCharacters := False;
  FShowDateColumn          := True;
  FShowImages              := True;
  FLogLevels               := [llError, llInfo, llWarning, llDebug];
  NodeDataSize             := SizeOf(TLogNodeData);
  Loaded;
end;

procedure TLogTree.DoAfterCellPaint(ACanvas: TCanvas; ANode: PVirtualNode;
  Column: TColumnIndex; CellRect: TRect);
var
  ColWidth: Integer;
begin
  inherited;

  if Column = 1 then
  begin
    if FHTMLSupport then
      ColWidth := DrawHTML(CellRect, ACanvas, GetCellText(ANode,
        Column), Selected[ANode])
    else
      ColWidth := ACanvas.TextWidth(GetCellText(ANode, Column));

    if not FShowDateColumn then
      ColWidth := ColWidth + 32; // Width of image

    if ColWidth > Header.Columns[1].MinWidth then
      Header.Columns[1].MinWidth := ColWidth;
  end;
end;

procedure TLogTree.DoFreeNode(Node: PVirtualNode);
var
  NodeData: PLogNodeData;
begin
  inherited;

  NodeData := GetNodeData(Node);

  if Assigned(NodeData) then
    NodeData.LogText := '';
end;

function TLogTree.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean;
  var Index: Integer): TCustomImageList;
var
  NodeData: PLogNodeData;
begin
  Images.Count;

  if ((FShowImages) and (Kind in [ikNormal, ikSelected])) and
    (((FShowDateColumn) and (Column <= 0)) or
    ((not FShowDateColumn) and (Column = 1))) then
  begin
    NodeData := GetNodeData(Node);

    if Assigned(NodeData) then
      case NodeData.LogLevel of
        llError:
          Index := 3;
        llInfo:
          Index := 2;
        llWarning:
          Index := 1;
        llDebug:
          Index := 0;
      else
        Index := 4;
      end;
  end;

  Result := inherited DoGetImageIndex(Node, Kind, Column, Ghosted, Index);
end;

procedure TLogTree.DoGetText(var pEventArgs: TVSTGetCellTextEventArgs);
begin
  if (pEventArgs.Column <= 0) or (not FHTMLSupport) then
    pEventArgs.CellText := GetCellText(pEventArgs.Node, pEventArgs.Column)
  else
    pEventArgs.CellText := '';
end;

procedure TLogTree.DoInitNode(Parent, Node: PVirtualNode;
  var InitStates: TVirtualNodeInitStates);
begin
  inherited;
  Include(InitStates, ivsMultiline);
end;

procedure TLogTree.DoMeasureItem(TargetCanvas: TCanvas;
  Node: PVirtualNode; var NodeHeight: Integer);
var
  I  : Integer;
  H  : Integer;
begin
  inherited;
  if MultiLine[Node] then
  begin
    TargetCanvas.Font := Font;
    NodeHeight := DefaultNodeHeight;
    for I := 0 to Header.Columns.Count - 1 do
    begin
      H := ComputeNodeHeight(TargetCanvas, Node, I);
      if H > NodeHeight then
        NodeHeight := H;
    end;
    if Cardinal(NodeHeight) > DefaultNodeHeight then
      NodeHeight := NodeHeight + 4; // needed to avoid multiline text drawing issues
  end;
end;

procedure TLogTree.DoOnAfterLog;
begin
  if Assigned(FOnAfterLog) then
    FOnAfterLog(Self);
end;

procedure TLogTree.DoOnBeforeLog(var ALogText: string; var
  ACancelEntry: Boolean; ALogLevel: TLogLevel);
begin
  if Assigned(FOnAfterLog) then
    FOnBeforeLog(Self, ALogText, ACancelEntry, ALogLevel);
end;

procedure TLogTree.DoPaintText(Node: PVirtualNode; const Canvas: TCanvas;
  Column: TColumnIndex; TextType: TVSTTextType);
begin
  inherited;
  Canvas.Font.Color := clBlack;
end;

function TLogTree.GetCellText(const Node: PVirtualNode; const
  Column: TColumnIndex): string;
var
  NodeData: PLogNodeData;
begin
  NodeData := GetNodeData(Node);

  if Assigned(NodeData) then
    case Column of
      - 1, 0:
        Result := FormatDateTime(DateTimeFormat, NodeData.Timestamp);
      1:
        Result := NodeData.LogText;
    end;
end;

function TLogTree.GetDateTimeFormat: string;
begin
  Result := FDateTimeFormat;
end;

procedure TLogTree.AddDefaultColumns(
  const ColumnNames: array of string; const ColumnWidths: array of Integer);
var
  I     : Integer;
  Column: TVirtualTreeColumn;
begin
  Header.Columns.Clear;

  if High(ColumnNames) <> High(ColumnWidths) then
    raise Exception.Create
      ('Number of column names must match the number of column widths.')
  else
  begin
    for I := Low(ColumnNames) to High(ColumnNames) do
    begin
      Column := Header.Columns.Add;
      Column.Text := ColumnNames[I];
      if ColumnWidths[I] > 0 then
        Column.Width := ColumnWidths[I]
      else
      begin
        Header.AutoSizeIndex := Column.Index;
        Header.Options := Header.Options + [hoAutoResize];
      end;
    end;
  end;
end;

procedure TLogTree.Loaded;
begin
  inherited Loaded;

  TreeOptions.PaintOptions := TreeOptions.PaintOptions - [toShowRoot,
    toShowTreeLines, toShowButtons] + [toUseBlendedSelection,
    toShowHorzGridLines, toHideFocusRect];
  TreeOptions.SelectionOptions := TreeOptions.SelectionOptions +
    [toFullRowSelect, toRightClickSelect];

  AddDefaultColumns([SDate, SLog], [100, 100]);
  Header.AutoSizeIndex := 1;
  Header.Columns[1].MinWidth := 80;
  Header.Options := Header.Options + [hoAutoResize];

  if not Assigned(PopupMenu) and not (csDesigning in ComponentState) then
  begin
    PopupMenu := TLogPopupmenu.Create(Self);
    TLogPopupmenu(PopupMenu).OnPopupMenuItemClick :=
      OnPopupMenuItemClick;
  end;

  SetShowDateColumn(FShowDateColumn);
end;

procedure TLogTree.OnPopupMenuItemClick(Sender: TObject;
  AMenuItem: TMenuItem);
begin
  if AMenuItem.Tag = 1 then
    SaveToFileWithDialog
  else if AMenuItem.Tag = 2 then
    CopyToClipboard
  else if AMenuItem.Tag = 3 then
    Clear;
end;

procedure TLogTree.SaveToFileWithDialog;
var
  SaveDialog: TSaveDialog;
begin
  SaveDialog := TSaveDialog.Create(Self);
  try
    SaveDialog.DefaultExt := '.txt';
    SaveDialog.Title := SSave;
    SaveDialog.Options := SaveDialog.Options + [ofOverwritePrompt];
    SaveDialog.Filter := STextFilesTxt;

    if SaveDialog.Execute then
      SaveToFile(SaveDialog.Filename);
  finally
    FreeAndNil(SaveDialog);
  end;
end;

procedure TLogTree.SaveToFile(const AFilename: string);
var
  SaveStrings: TStringList;
begin
  SaveStrings := TStringList.Create;
  try
    SaveToStrings(SaveStrings);

    SaveStrings.SaveToFile(AFilename);
  finally
    FreeAndNil(SaveStrings);
  end;
end;

procedure TLogTree.CopyToClipboard;
var
  CopyStrings: TStringList;
begin
  CopyStrings := TStringList.Create;
  try
    SaveToStrings(CopyStrings);

    Clipboard.AsText := CopyStrings.Text;
  finally
    FreeAndNil(CopyStrings);
  end;
end;

function TLogTree.IfThen(Condition: Boolean; TrueResult,
  FalseResult: Variant): Variant;
begin
  if Condition then
    Result := TrueResult
  else
    Result := FalseResult;
end;

procedure TLogTree.Init;
begin
  Loaded;
end;

function TLogTree.StripHTMLTags(const Value: string): string;
var
  TagBegin, TagEnd, TagLength: Integer;
begin
  Result := Value;

  TagBegin := Pos('<', Result); // search position of first <

  while TagBegin > 0 do
  begin
    TagEnd := Pos('>', Result);
    TagLength := TagEnd - TagBegin + 1;

    Delete(Result, TagBegin, TagLength);
    TagBegin := Pos('<', Result);
  end;
end;

procedure TLogTree.SaveToStrings(const AStrings: TStrings);
var
  Node: PVirtualNode;
begin
  Node := GetFirst;

  while Assigned(Node) do
  begin
    AStrings.Add(Concat(IfThen(FShowDateColumn,
      Concat(GetCellText(Node, 0), #09), ''), IfThen(FHTMLSupport,
      StripHTMLTags(GetCellText(Node, 1)), GetCellText(Node, 1))));
    Node := Node.NextSibling;
  end;
end;

function TLogTree.RemoveCtrlChars(const Value: string): string;
var
  I: Integer;
begin
 // Replace CTRL characters with <whitespace>
  Result := '';

  for I := 1 to Length(Value) do
    if (AnsiChar(Value[I]) in [#0 .. #31, #127]) then
      Result := Result + ' '
    else
      Result := Result + Value[I];
end;

procedure TLogTree.Log(AValue: string; ALogLevel: TLogLevel;
  ATimestamp: TDateTime);
var
  ACancelEntry: Boolean;
  Node       : PVirtualNode;
  NodeData   : PLogNodeData;
  DoScroll   : Boolean;
begin
  ACancelEntry := False;
  DoOnBeforeLog(AValue, ACancelEntry, ALogLevel);
  if not ACancelEntry then
  begin
    DoScroll := ((not Focused) or (GetLast = FocusedNode)) and FAutoScroll;
    Node := AddChild(nil);
    NodeData := GetNodeData(Node);

    if Assigned(NodeData) then
    begin
      NodeData.LogLevel := ALogLevel;

      if ATimestamp = 0 then
        NodeData.Timestamp := now
      else
        NodeData.Timestamp := ATimestamp;

      if FRemoveControlCharacters then
        AValue := RemoveCtrlChars(AValue);

      if FAutoLogLevelColors then
      begin
        case ALogLevel of
          llError:
            AValue := Concat('<font-color=clRed>', AValue, '</font-color>');
          llInfo:
            AValue := Concat('<font-color=clBlack>', AValue, '</font-color>');
          llWarning:
            AValue := Concat('<font-color=clBlue>', AValue, '</font-color>');
          llDebug:
            AValue := Concat('<font-color=clGreen>', AValue, '</font-color>')
        end;
      end;

      NodeData.LogText := AValue;
      IsVisible[Node] := NodeData.LogLevel in FLogLevels;
      DoOnAfterLog;
    end;

    if FMaximumLines <> 0 then
    begin
      while RootNodeCount > FMaximumLines do
        DeleteNode(GetFirst);
    end;

    if DoScroll then
    begin
      ScrollIntoView(GetLast, False);
    end;
  end;
end;

procedure TLogTree.LogFmt(AValue: string; const AArgs: Array of
  const; ALogLevel: TLogLevel; ATimestamp: TDateTime);
begin
  Log(Format(AValue, AArgs), ALogLevel, ATimestamp);
end;

procedure TLogTree.SetDateTimeFormat(const Value: string);
begin
  if Value <> DateTimeFormat then
  begin
    FDateTimeFormat := Value;
  end;
end;

procedure TLogTree.SetLogLevels(const Value: TLogLevels);
begin
  FLogLevels := Value;
  UpdateVisibleItems;
end;

procedure TLogTree.SetShowDateColumn(const Value: Boolean);
begin
  FShowDateColumn := Value;

  if Header.Columns.Count > 0 then
  begin
    if FShowDateColumn then
      Header.Columns[0].Options := Header.Columns[0].Options + [coVisible]
    else
      Header.Columns[0].Options := Header.Columns[0].Options - [coVisible]
  end;
end;

procedure TLogTree.SetShowImages(const Value: Boolean);
begin
  FShowImages := Value;
  Invalidate;
end;

procedure TLogTree.UpdateVisibleItems;
var
  Node    : PVirtualNode;
  NodeData: PLogNodeData;
begin
  BeginUpdate;
  try
    Node := GetFirst;
    while Assigned(Node) do
    begin
      NodeData := GetNodeData(Node);
      if Assigned(NodeData) then
        IsVisible[Node] := NodeData.LogLevel in FLogLevels;
      Node := Node.NextSibling;
    end;
    Invalidate;
  finally
    EndUpdate;
  end;
end;

function TLogTree.DrawHTML(const ARect: TRect; const ACanvas:
  TCanvas; const Text: string; Selected: Boolean): Integer;
{
 DrawHTML - Draws text on a canvas using tags based on a simple
 subset of HTML/CSS

 <b> - Bold e.g. <b>This is bold</b>
 <i> - Italic e.g. <i>This is italic</i>
 <u> - Underline e.g. <u>This is underlined</u>
 <font-color=X> - Font color
   examples:
   <font-color=clRed>Delphi red</font-color>
   <font-color=#FFFFFF>Web white</font-color>
   <font-color=$000000>Hex black</font-color>
 <font-size=X> Font size
   example: <font-size=30>This is some big text</font-size>
 <font-family> Font family
   example: <font-family=Arial>This is Arial</font-family>
}

  function CloseTag(const ATag: string): string;
  begin
    Result := Concat('/', ATag);
  end;

  function GetTagValue(const ATag: string): string;
  var
    P: Integer;
  begin
    P := Pos('=', ATag);

    if P = 0 then
      Result := ''
    else
      Result := Copy(ATag, P + 1, MaxInt);
  end;

  function ColorCodeToColor(const Value: string): TColor;
  var
    HexValue: string;
  begin
    Result := 0;

    if Value <> '' then
    begin
      if (Length(Value) >= 2) and (Copy(Uppercase(Value), 1, 2) = 'CL') then
      begin
       // Delphi color
        Result := StringToColor(Value);
      end
      else if Value[1] = '#' then
      begin
       // Web color
        HexValue := Copy(Value, 2, 6);

        Result := RGB(StrToInt('$' + Copy(HexValue, 1, 2)),
          StrToInt('$' + Copy(HexValue, 3, 2)),
          StrToInt('$' + Copy(HexValue, 5, 2)));
      end
      else
       // Hex or decimal color
        Result := StrToIntDef(Value, 0);
    end;
  end;

const
  TagBold       = 'B';
  TagItalic     = 'I';
  TagUnderline  = 'U';
  TagBreak      = 'BR';
  TagFontSize   = 'FONT-SIZE';
  TagFontFamily = 'FONT-FAMILY';
  TagFontColor  = 'FONT-COLOR';
  TagColor      = 'COLOR';

var
  X                  : Integer;
  Y                  : Integer;
  Idx                : Integer;
  CharWidth          : Integer;
  MaxCharHeight      : Integer;
  CurrChar           : Char;
  Tag                : string;
  TagValue           : string;
  PreviousFontColor  : TColor;
  PreviousFontFamily : string;
  PreviousFontSize   : Integer;
  PreviousColor      : TColor;

begin
  ACanvas.Font.Size := Canvas.Font.Size;
  ACanvas.Font.Name := Canvas.Font.Name;
  ACanvas.Font.Color := Canvas.Font.Color;
  ACanvas.Font.Style := Canvas.Font.Style;

  PreviousFontColor := ACanvas.Font.Color;
  PreviousFontFamily := ACanvas.Font.Name;
  PreviousFontSize := ACanvas.Font.Size;
  PreviousColor := ACanvas.Brush.Color;

  X := ARect.Left;
  Y := ARect.Top + 1;
  Idx := 1;

  MaxCharHeight := ACanvas.TextHeight('Ag');

  while Idx <= Length(Text) do
  begin
    CurrChar := Text[Idx];

   // Is this a tag?
    if CurrChar = '<' then
    begin
      Tag := '';
      Inc(Idx);
     // Find the end of then tag
      while (Text[Idx] <> '>') and (Idx <= Length(Text)) do
      begin
        Tag := Concat(Tag, Uppercase(Text[Idx]));
        Inc(Idx);
      end;

     // Simple tags
      if Tag = TagBold then
        ACanvas.Font.Style := ACanvas.Font.Style + [fsBold]
      else if Tag = TagItalic then
        ACanvas.Font.Style := ACanvas.Font.Style + [fsItalic]
      else if Tag = TagUnderline then
        ACanvas.Font.Style := ACanvas.Font.Style + [fsUnderline]
      else if Tag = TagBreak then
      begin
        X := ARect.Left;
        Inc(Y, MaxCharHeight);
      end
      // Closing tags
      else if Tag = CloseTag(TagBold) then
        ACanvas.Font.Style := ACanvas.Font.Style - [fsBold]
      else if Tag = CloseTag(TagItalic) then
        ACanvas.Font.Style := ACanvas.Font.Style - [fsItalic]
      else if Tag = CloseTag(TagUnderline) then
        ACanvas.Font.Style := ACanvas.Font.Style - [fsUnderline]
      else if Tag = CloseTag(TagFontSize) then
        ACanvas.Font.Size := PreviousFontSize
      else if Tag = CloseTag(TagFontFamily) then
        ACanvas.Font.Name := PreviousFontFamily
      else if Tag = CloseTag(TagFontColor) then
        ACanvas.Font.Color := PreviousFontColor
      else if Tag = CloseTag(TagColor) then
        ACanvas.Brush.Color := PreviousColor
      else
     // Tags with values
      begin
       // Get the tag value (everything after '=')
        TagValue := GetTagValue(Tag);

        if TagValue <> '' then
        begin
         // Remove the value from the tag
          Tag := Copy(Tag, 1, Pos('=', Tag) - 1);

          if Tag = TagFontSize then
          begin
            PreviousFontSize := ACanvas.Font.Size;
            ACanvas.Font.Size := StrToIntDef(TagValue, ACanvas.Font.Size);
          end
          else if Tag = TagFontFamily then
          begin
            PreviousFontFamily := ACanvas.Font.Name;
            ACanvas.Font.Name := TagValue;
          end;

          if Tag = TagFontColor then
          begin
            PreviousFontColor := ACanvas.Font.Color;
            try
              ACanvas.Font.Color := ColorCodeToColor(TagValue);
            except
             //Just in case the canvas color is invalid
            end;
          end
          else if Tag = TagColor then
          begin
            PreviousColor := ACanvas.Brush.Color;
            try
              ACanvas.Brush.Color := ColorCodeToColor(TagValue);
            except
             //Just in case the canvas color is invalid
            end;
          end;
        end;
      end;
    end
    else if CurrChar >= #32 then // Draw the character if it's not a ctrl char
    begin
      CharWidth := ACanvas.TextWidth(CurrChar);

      if Y + MaxCharHeight < ARect.Bottom then
      begin
        ACanvas.Brush.Style := bsClear;
        ACanvas.TextOut(X, Y, CurrChar);
      end;

      X := X + CharWidth;
    end;

    Inc(Idx);
  end;

  Result := X - ARect.Left;
end;

{ TLogPopupmenu }

constructor TLogPopupmenu.Create(AOwner: TComponent);

  function AddMenuItem(const ACaption: string; ATag: Integer): TMenuItem;
  begin
    Result := TMenuItem.Create(Self);

    Result.Caption := ACaption;
    Result.Tag := ATag;
    Result.OnClick := OnMenuItemClick;

    Items.Add(Result);
  end;

begin
  inherited Create(AOwner);

  FOwner := AOwner;

  AddMenuItem(SSaveLog, 1);
  AddMenuItem('-', -1);
  AddMenuItem(SCopyToClipboard, 2);
  AddMenuItem(SClear, 3);
end;

procedure TLogPopupmenu.OnMenuItemClick(Sender: TObject);
begin
  if Assigned(FOnPopupMenuItemClick) then
    FOnPopupMenuItemClick(Self, TMenuItem(Sender));
end;

end.
