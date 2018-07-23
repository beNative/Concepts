{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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
  The Initial Developer of the Original Code (Ex_Inspector.pas) is Roman M.
  Mochalov (roman@tersy.ru). Portions created by the Initial Developer are
  Copyright (C) 1997-2007. All Rights Reserved. You may obtain a copy of the
  original code at http://www.tersy.ru/~roman/download/

  Changes and improvements by Tim Sinaeve:
   - Code ported and refactored to support unicode and later versions of Delphi
   - OnGetCellColors was not triggered anymore
}

{$I DDuce.inc}

unit DDuce.Components.Inspector;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.Classes, System.Math,
  Vcl.Controls, Vcl.Graphics, Vcl.Forms, Vcl.StdCtrls,

  DDuce.Components.GridView;

type
  TInspectorEdit = class(TGridEdit)
  protected
    procedure UpdateBounds(ScrollCaret: Boolean); override;
    procedure UpdateColors; override;

  public
    procedure Invalidate; override;

  end;

  TInspectorCategoryRowEvent = procedure(
    Sender       : TObject;
    Row          : Longint;
    var Category : Boolean
  ) of object;

  TCustomInspector = class(TCustomGridView)
  private
    FNameFont         : TFont;
    FValueFont        : TFont;
    FCategoryFont     : TFont;
    FHitTest          : TPoint;
    FColUpdate        : Integer;
    FBitmap           : TBitmap;
    FBrush            : HBRUSH;
    FOnGetCategoryRow : TInspectorCategoryRowEvent;

    procedure HandleFontChange(Sender: TObject);

    procedure SetCategoryFont(Value: TFont);
    procedure SetNameFont(Value: TFont);
    procedure SetValueFont(Value: TFont);

    procedure WMNCHitTest(var AMessage: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMSetCursor(var AMessage: TWMSetCursor); message WM_SETCURSOR;

  protected
    procedure UpdatePattern; virtual;
    procedure ChangeColumns; override;
    function ColResizeAllowed(X, Y: Integer): Boolean;
    procedure ColumnResizing(Column: Integer; var Width: Integer); override;
    function EditCanShow(Cell: TGridCell): Boolean; override;
    procedure GetCellColors(Cell: TGridCell; Canvas: TCanvas); override;
    function GetCellHintRect(Cell: TGridCell): TRect; override;
    function GetCellText(Cell: TGridCell): string; override;
    function GetCellTextIndent(Cell: TGridCell): TPoint; override;
    function GetEditClass(Cell: TGridCell): TGridEditClass; override;
    procedure GetEditListBounds(Cell: TGridCell; var Rect: TRect); override;
    function GetTipsRect(Cell: TGridCell): TRect; override;
    procedure HideCursor; override;
    procedure HideFocus; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure PaintCell(ACell: TGridCell; ARect: TRect); override;
    procedure PaintFocus; override;
    procedure Resize; override;
    procedure ShowCursor; override;
    procedure ShowFocus; override;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function GetColumnAt(X, Y: Integer): Integer; override;
    function GetEditRect(Cell: TGridCell): TRect; override;
    function GetFocusRect: TRect; override;
    function GetRowAt(X, Y: Integer): Integer; override;
    function IsCategoryRow(Row: Integer): Boolean; virtual;
    procedure UpdateColumnsSize; virtual;
    procedure UpdateScrollBars; override;

    property CategoryFont: TFont
      read FCategoryFont write SetCategoryFont;

    property NameFont: TFont
      read FNameFont write SetNameFont;

    property ValueFont: TFont
      read FValueFont write SetValueFont;

    property OnGetCategoryRow: TInspectorCategoryRowEvent
      read FOnGetCategoryRow write FOnGetCategoryRow;
  end;

  TInspector = class(TCustomInspector)
  published
    property Align;
    property AllowEdit default True;
    property AlwaysEdit default True;
    property AlwaysSelected;
    property Anchors;
    property BorderStyle;
    property CursorKeys;
    property CategoryFont;
    property Color default clBtnFace;
    property Constraints;
    property ColumnsFullDrag default True;
    property DoubleBuffered;
    property Enabled;
    property EndEllipsis default False;
    property FlatBorder;
    property GridColor;
    property GridLines;
    property GridStyle;   // TODO!!!
    property Font;
    property NameFont;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Rows;
    property ShowCellTips;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property ValueFont;
    property Visible;

    property OnCellClick;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnCellAcceptCursor;
    property OnEditAcceptKey;
    property OnEditCanModify;
    property OnEditButtonPress;
    property OnEditCanceled;
    property OnEditChange;
    property OnEditCloseUp;
    property OnEditSelectNext;
    property OnEnter;
    property OnExit;
    property OnGetCheckAlignment;
    property OnGetCheckImage;
    property OnGetCheckIndent;
    property OnGetCheckKind;
    property OnGetCheckState;
    property OnGetCategoryRow;
    property OnGetCellText;
    property OnGetCellColors;
    property OnGetCellReadOnly;
    property OnGetEditList;
    property OnGetEditStyle;
    property OnGetEditText;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyDown;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnKeyPress;
    property OnKeyUp;
    property OnResize;
    property OnSetEditText;
    property OnStartDrag;
  end;

implementation

uses
  System.Types, System.UITypes;

{$REGION 'TInspectorEdit'}
procedure TInspectorEdit.UpdateBounds(ScrollCaret: Boolean);
begin
  inherited UpdateBounds(ScrollCaret);
  ButtonWidth := Height;
end;

procedure TInspectorEdit.UpdateColors;
begin
  inherited UpdateColors;
  Color      := clWindow;
  Font.Color := clBtnText;
end;

procedure TInspectorEdit.Invalidate;
begin
  inherited Invalidate;
  if Assigned(Grid) then
  begin
    Grid.InvalidateFocus;
  end;
end;
{$ENDREGION}

{$REGION 'TCustomInspector'}
{$REGION 'construction and destruction'}
procedure TCustomInspector.AfterConstruction;
begin
  inherited AfterConstruction;
  Color      := clBtnFace;
  RowSelect  := False;
  AllowEdit  := True;
  AlwaysEdit := True;
  with Columns.Add do
  begin
    Caption := 'Property';
    FixedSize := True;
    WordWrap := False;
    WantReturns := False;
    ReadOnly := True;
    TabStop := False;
  end;
  with Columns.Add do
  begin
    Caption := 'Value';
    WordWrap := False;
    WantReturns := False;
    FixedSize := True;
  end;
  FBitmap               := TBitmap.Create;
  Header.Synchronized   := True;
  ShowHeader            := False;
  Rows.AutoHeight       := False;
  Rows.Height           := 16;
  HorzScrollBar.Visible := False;
  ColumnsFullDrag       := True;
  CheckBoxes            := True;
  EndEllipsis           := False;
  GridLines             := False;
  TextTopIndent         := 1;
  TextRightIndent       := 1;
  CursorKeys            := CursorKeys + [gkMouseMove];
  FNameFont             := TFont.Create;
  FNameFont.Assign(Font);
  FNameFont.Color    := clBtnText;
  FNameFont.OnChange := HandleFontChange;
  FValueFont         := TFont.Create;
  FValueFont.Assign(Font);
  FValueFont.Color    := clNavy;
  FValueFont.OnChange := HandleFontChange;
  FCategoryFont       := TFont.Create;
  FCategoryFont.Assign(Font);
  FCategoryFont.Color    := clPurple;
  FCategoryFont.Style    := FValueFont.Style + [fsBold];
  FCategoryFont.OnChange := HandleFontChange;
end;

procedure TCustomInspector.BeforeDestruction;
begin
  FBitmap.Free;
  FCategoryFont.Free;
  FValueFont.Free;
  FNameFont.Free;
  if FBrush <> 0 then
    DeleteObject(FBrush);
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TCustomInspector.HandleFontChange(Sender: TObject);
begin
  InvalidateGrid;
end;
{$ENDREGION}

{$REGION 'message handlers'}
procedure TCustomInspector.WMNCHitTest(var AMessage: TWMNCHitTest);
begin
  inherited;
  FHitTest := ScreenToClient(SmallPointToPoint(AMessage.Pos));
end;

procedure TCustomInspector.WMSetCursor(var AMessage: TWMSetCursor);
begin
  if (AMessage.HitTest = HTCLIENT) and not (csDesigning in ComponentState) then
  begin
    if ColResizeAllowed(FHitTest.X, FHitTest.Y) then
    begin
      Winapi.Windows.SetCursor(Screen.Cursors[crHSplit]);
      Exit;
    end;
  end;
  inherited;
end;
{$ENDREGION}

{$REGION 'property access methods'}
procedure TCustomInspector.SetCategoryFont(Value: TFont);
begin
  FCategoryFont.Assign(Value);
end;

procedure TCustomInspector.SetNameFont(Value: TFont);
begin
  FNameFont.Assign(Value);
end;

procedure TCustomInspector.SetValueFont(Value: TFont);
begin
  FValueFont.Assign(Value);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TCustomInspector.ChangeColumns;
begin
  inherited ChangeColumns;
  UpdateColumnsSize;
end;

function TCustomInspector.ColResizeAllowed(X, Y: Integer): Boolean;
begin
  Result := (Columns.Count > 0) and (X >= Columns[0].Width - 4)
    and (X <= Columns[0].Width);
end;

procedure TCustomInspector.ColumnResizing(Column: Integer; var Width: Integer);
begin
  if Width > ClientWidth - 35 then
    Width := ClientWidth - 35;
  if Width < 35 then
    Width := 35;
end;

function TCustomInspector.EditCanShow(Cell: TGridCell): Boolean;
begin
  Result := (not IsCategoryRow(Cell.Row)) and inherited EditCanShow(Cell);
end;

procedure TCustomInspector.GetCellColors(Cell: TGridCell; Canvas: TCanvas);
begin
  Canvas.Brush.Color := Self.Color;
  if IsCategoryRow(Cell.Row) then
    Canvas.Font := CategoryFont
  else if Cell.Col = 1 then
    Canvas.Font := ValueFont
  else if Cell.Col = 0 then
    Canvas.Font := NameFont
  else
    Canvas.Font := Self.Font;

  if Assigned(OnGetCellColors) then
    OnGetCellColors(Self, Cell, Canvas);
end;

function TCustomInspector.GetCellHintRect(Cell: TGridCell): TRect;
begin
  Result := inherited GetCellHintRect(Cell);
  if IsCategoryRow(Cell.Row) then
  begin
    Result.Left := GetColumnRect(0).Left;
    Result.Right := GetColumnRect(1).Right;
  end;
end;

function TCustomInspector.GetCellText(Cell: TGridCell): string;
begin
  if (Cell.Col <> 0) and IsCategoryRow(Cell.Row) then
    Cell.Col := 0;
  Result := inherited GetCellText(Cell);
end;

function TCustomInspector.GetCellTextIndent(Cell: TGridCell): TPoint;
begin
  Result.X := 2 - Cell.Col;
  Result.Y := TextTopIndent;
end;

function TCustomInspector.GetEditClass(Cell: TGridCell): TGridEditClass;
begin
  Result := TInspectorEdit;
end;

procedure TCustomInspector.GetEditListBounds(Cell: TGridCell; var Rect: TRect);
begin
  Dec(Rect.Left, 2);
  inherited GetEditListBounds(Cell, Rect);
end;

function TCustomInspector.GetTipsRect(Cell: TGridCell): TRect;
var
  DX: Integer;
begin
  Result := inherited GetTipsRect(Cell);
  if (Cell.Col <> 0) and IsCategoryRow(Cell.Row) then
  begin
    DX := GetColumnRect(0).Left - Result.Left;
    OffsetRect(Result, DX, 0);
  end;
end;

procedure TCustomInspector.HideCursor;
begin
  if IsCategoryRow(CellFocused.Row) then
  begin
    InvalidateFocus;
    Exit;
  end;
  inherited HideCursor;
end;

procedure TCustomInspector.HideFocus;
begin
  { suppress inherited call }
end;

procedure TCustomInspector.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not AcquireFocus then
  begin
    MouseCapture := False;
    Exit;
  end;
  if Button = mbLeft then
  begin
    if ColResizeAllowed(X, Y) then
    begin
      StartColResize(Header.Sections[0], X, Y);
      Exit;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomInspector.PaintCell(ACell: TGridCell; ARect: TRect);
//var
//  R : TRect;
begin
  if IsCategoryRow(ACell.Row) then
  begin
    ARect.Left  := GetGridRect.Left;
    ARect.Right := GetGridRect.Right;
    if ACell.Col <> 0 then
      Exit;
  end;

  inherited PaintCell(ACell, ARect);

  if ACell.Row <> CellFocused.Row then
  begin
//    R := ARect;
//    R.Top := R.Bottom - 1;
//    with Canvas do
//    begin
//      Brush.Color := clGray xor clSilver;
//      Font.Color := clBlack;
//      UpdatePattern;
//      Winapi.Windows.FillRect(Handle, R, FBrush);
//    end;
  end;

  if (ACell.Col = 0) and (not IsCategoryRow(ACell.Row)) then
  begin
    with Canvas do
    begin
      Pen.Color := clBtnShadow;
      Pen.Width := 1;
      MoveTo(ARect.Right - 2, ARect.Top - 1);
      LineTo(ARect.Right - 2, ARect.Bottom);
      Pen.Color := clBtnHighlight;
      MoveTo(ARect.Right - 1, ARect.Bottom - 1);
      LineTo(ARect.Right - 1, ARect.Top - 1);
    end;
  end;

//  if ACell.Row = CellFocused.Row then
//  begin
//    DrawEdge(Canvas.Handle, ARect, BDR_SUNKENOUTER, BF_BOTTOM)
//  end
//  else if ACell.Row = CellFocused.Row - 1 then
//  begin
//    R := ARect;
//    R.Top := R.Bottom - 2;
//    DrawEdge(Canvas.Handle, R, BDR_SUNKENOUTER, BF_TOP);
//    InflateRect(R, 0, -1);
//    DrawEdge(Canvas.Handle, R, BDR_SUNKENINNER, BF_TOP);
//  end;
end;

procedure TCustomInspector.PaintFocus;
begin
  { suppress inherited call }
end;

procedure TCustomInspector.Resize;
begin
  UpdateColumnsSize;
  inherited Resize;
end;

procedure TCustomInspector.ShowCursor;
begin
  if IsCategoryRow(CellFocused.Row) then
  begin
    InvalidateFocus;
    Exit;
  end;
  inherited ShowCursor;
end;

procedure TCustomInspector.ShowFocus;
begin
  { suppress inherited call }
end;
{$ENDREGION}

{$REGION 'public methods'}
function TCustomInspector.GetColumnAt(X, Y: Integer): Integer;
var
  C1, C2: TGridCell;
  R: TRect;
begin
  C1 := GridCell(0, VisOrigin.Row);
  C2 := GridCell(1, VisOrigin.Row + VisSize.Row - 1);
  R := GetCellsRect(C1, C2);
  if X < R.Left then
  begin
    Result := C1.Col;
    Exit;
  end;
  if X >= R.Right then
  begin
    Result := C2.Col;
    Exit;
  end;
  Result := inherited GetColumnAt(X, Y);
end;

function TCustomInspector.GetEditRect(Cell: TGridCell): TRect;
begin
  Result := inherited GetEditRect(Cell);
  Dec(Result.Bottom, 1);
end;

function TCustomInspector.GetFocusRect: TRect;
begin
  Result := GetRowRect(CellFocused.Row);
  Dec(Result.Top, 2);
end;

function TCustomInspector.GetRowAt(X, Y: Integer): Integer;
var
  C1, C2: TGridCell;
  R: TRect;
begin
  C1 := GridCell(0, VisOrigin.Row);
  C2 := GridCell(1, VisOrigin.Row + VisSize.Row - 1);
  R := GetCellsRect(C1, C2);
  if Y < R.Top then
  begin
    Result := MaxIntValue([C1.Row - 1, 0]);
    Exit;
  end;
  if Y >= R.Bottom then
  begin
    Result := MinIntValue([C2.Row + 1, Rows.Count - 1]);
    Exit;
  end;
  Result := inherited GetRowAt(X, Y);
end;

function TCustomInspector.IsCategoryRow(Row: Integer): Boolean;
begin
  Result := False;
  if Assigned(FOnGetCategoryRow) then FOnGetCategoryRow(Self, Row, Result);
end;

procedure TCustomInspector.UpdateColumnsSize;
begin
  if (FColUpdate = 0) and (Columns.Count = 2) then
  begin
    Inc(FColUpdate);
    try
      Columns[1].Width := ClientWidth - Columns[0].Width;
      if Columns[1].Width < 35 then
      begin
        Columns[1].Width := 35;
        Columns[0].Width := ClientWidth - 35;
      end;
      if Columns[0].Width < 35 then
      begin
        Columns[0].Width := 35;
        Columns[1].Width := ClientWidth - 35;
      end;
    finally
      Dec(FColUpdate);
    end;
  end;
end;

procedure TCustomInspector.UpdatePattern;
var
  I: Integer;
begin
  with FBitmap do
  begin
    Width := 8;
    Height := 1;
    Canvas.Brush.Color := Color;
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


procedure TCustomInspector.UpdateScrollBars;
begin
  inherited UpdateScrollBars;
  UpdateColumnsSize;
end;
{$ENDREGION}
{$ENDREGION}

end.
