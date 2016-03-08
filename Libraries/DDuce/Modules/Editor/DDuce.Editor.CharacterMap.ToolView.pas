{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.Editor.CharacterMap.Toolview;

interface

uses
  System.Classes, System.SysUtils,
  Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.Grids, Vcl.ComCtrls, Vcl.ExtCtrls,

  DDuce.Editor.Interfaces, DDuce.Editor.ToolView.Base;

type
  TfrmCharacterMap = class(TCustomEditorToolView, IEditorToolView)
    cbxUnicodeRange    : TComboBox;
    imgChar            : TImage;
    lblCharInfo        : TLabel;
    pcMain             : TPageControl;
    grdANSI            : TStringGrid;
    grdUnicode         : TStringGrid;
    pnlChar            : TPanel;
    shpChar            : TShape;
    tsANSI             : TTabSheet;
    tsUnicode          : TTabSheet;
    lblUnicodeCharInfo : TLabel;

    procedure cbxUnicodeRangeSelect(Sender: TObject);
    procedure grdANSIKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure grdANSIMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure grdANSIPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure grdANSISelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure grdUnicodeKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure grdUnicodeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure grdANSIMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure grdUnicodeMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure grdUnicodeSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);

  strict private

    procedure FillCharMap;
    procedure UpdateCharacterBitmap(const ACharacter: string);
    procedure UpdateUnicodeDisplay(ACol, ARow: Integer);
    procedure UpdateANSIDisplay(ACol, ARow: Integer);

  strict protected
    procedure EditorSettingsChanged(Sender: TObject); override;

  public
    procedure AfterConstruction; override;
  end;

implementation

uses
  Winapi.Windows,
  Vcl.Graphics, Vcl.Dialogs;

{$R *.dfm}

resourcestring
  SCharacterMap = 'Character Map';

{$REGION 'non-interfaced routines'}
function RoundUp(Value, Divi: Integer): Integer;
begin
  if Value mod Divi = 0 then
    Result := Value div Divi
  else
    Result := (Value div Divi) + 1;
end;

function CreateCharacterBitmap(
  const AFontName     : string;
  const ACharacter    : string;
  const ABitmapHeight : Integer
): TBitmap;
begin
  Result := TBitmap.Create;
  Result.Height := ABitmapHeight;
  Result.Width  := ABitmapHeight;
  Result.Canvas.Brush.Color := clWhite;
  Result.Canvas.FillRect(Result.Canvas.ClipRect);
  Result.Canvas.Font.Name := AFontName;
  Result.Canvas.Font.Height := ABitmapHeight - 10;
  Result.Canvas.TextOut(
    (Result.Width - Result.Canvas.TextWidth(ACharacter)) div 2,
    0,
    ACharacter
  );
end;
{$ENDREGION}

{$REGION 'construction and destruction'}
procedure TfrmCharacterMap.AfterConstruction;
var
  I : Integer;
begin
  inherited AfterConstruction;
  Caption := SCharacterMap;
  lblCharInfo.Caption := '-';
  lblUnicodeCharInfo.Caption := '-';
  grdANSI.Font.Assign(Manager.Settings.EditorFont);
  grdANSI.Font.Size := 12;
  grdUnicode.Font.Assign(Manager.Settings.EditorFont);
  grdUnicode.Font.Size := 12;
  FillCharMap;
//  grdANSI.AutoSizeColumns;
  cbxUnicodeRange.Items.Clear;
//  for I := 0 to MaxUnicodeBlocks do
//  begin
//    cbxUnicodeRange.Items.Add(UnicodeBlocks[I].PG);
//  end;
  pcMain.ActivePageIndex := 0;
  cbxUnicodeRange.ItemIndex := 0;
  cbxUnicodeRangeSelect(nil);
  Manager.Settings.OnChanged.Add(EditorSettingsChanged);
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmCharacterMap.cbxUnicodeRangeSelect(Sender: TObject);
//var
//  N : Integer;
//  X : Integer;
//  Y : Integer;
//  S : Integer;
//  E : Integer;
begin
//  S := UnicodeBlocks[cbxUnicodeRange.ItemIndex].S;
//  E := UnicodeBlocks[cbxUnicodeRange.ItemIndex].E;
  //grdUnicode.Clear;
//  grdUnicode.ColCount := 16;
//  grdUnicode.RowCount := RoundUp(E - S, 16);
//  N                   := 0;
//  for Y := 0 to grdUnicode.RowCount - 1 do
//    for X := 0 to grdUnicode.ColCount - 1 do
//    begin
//      if S + N <= E then
//        //grdUnicode.Cells[X, Y] := UnicodeToUTF8(S + N);
//      Inc(N);
//    end;
  //grdUnicode.AutoSizeColumns;
end;

procedure TfrmCharacterMap.grdANSIKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    Manager.Commands.InsertTextAtCaret(grdANSI.Cells[grdANSI.Col, grdANSI.Row]);
end;

procedure TfrmCharacterMap.EditorSettingsChanged(Sender: TObject);
begin
  grdANSI.Font.Assign(Manager.Settings.EditorFont);
  grdUnicode.Font.Assign(Manager.Settings.EditorFont);
end;

procedure TfrmCharacterMap.grdANSIMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  R : Integer;
  C : Integer;
begin
  R := 0;
  C := 0;
  //if (Button = mbLeft) and (grdANSI.MouseToGridZone(X, Y) = gzNormal) then
  begin
    grdANSI.MouseToCell(X, Y, C, R);
    if grdANSI.Cells[C, R] <> '' then
      Manager.Commands.InsertTextAtCaret(grdANSI.Cells[C, R])
  end;
end;

procedure TfrmCharacterMap.grdANSIPrepareCanvas(sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
begin
  if gdFixed in aState then
  begin
    grdANSI.Canvas.Font.Assign(Font);
    grdANSI.Canvas.Font.Size := 8;
  end
end;

procedure TfrmCharacterMap.grdANSISelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  UpdateANSIDisplay(aCol, aRow);
end;

procedure TfrmCharacterMap.grdUnicodeKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    Manager.Commands.InsertTextAtCaret(grdUnicode.Cells[grdUnicode.Col, grdUnicode.Row]);
end;

procedure TfrmCharacterMap.grdUnicodeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  R : Integer;
  C : Integer;
begin
  R := 0;
  C := 0;
  //if (Button = mbLeft) and (grdUnicode.MouseToGridZone(X, Y) = gzNormal) then
  begin
    grdUnicode.MouseToCell(X, Y, C, R);
    Manager.Commands.InsertTextAtCaret(grdUnicode.Cells[C, R])
  end;
end;

procedure TfrmCharacterMap.grdANSIMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
//var
//  R : Integer;
//  C : Integer;
begin
//  R := 0;
//  C := 0;
  //if grdANSI.MouseToGridZone(X, Y) = gzNormal then
//  begin
//    grdANSI.MouseToCell(X, Y, C, R);
//    UpdateANSIDisplay(C, R);
//  end
//  else
//  begin
//    lblCharInfo.Caption := '-';
//  end;
end;

procedure TfrmCharacterMap.grdUnicodeMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
//var
//  Row   : Integer;
//  Col   : Integer;
begin
//  Row := 0;
//  Col := 0;
//  if grdUnicode.MouseToGridZone(X, Y) = gzNormal then
//  begin
//    grdUnicode.MouseToCell(X, Y, Col, Row);
//    UpdateUnicodeDisplay(Col, Row);
//  end
//  else
//  begin
//    lblCharInfo.Caption := '-';
//  end;
end;

procedure TfrmCharacterMap.grdUnicodeSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  UpdateUnicodeDisplay(aCol, aRow);
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TfrmCharacterMap.FillCharMap;
var
  R : Integer;
  C : Integer;
begin
  for R := 0 to Pred(grdANSI.RowCount) do
  begin
    if R <> 0 then
      grdANSI.Cells[0, R] := Format('%.3d +', [Succ(R) * 16]);
    for C := 1 to Pred(grdANSI.ColCount) do
    begin
      if R = 0 then
        grdANSI.Cells[C, R] := Format('%.2d', [Pred(C)])
      else
        grdANSI.Cells[C, R] := AnsiToUTF8(Chr(Succ(R) * 16 + Pred(C)));
    end;
  end;
end;

procedure TfrmCharacterMap.UpdateCharacterBitmap(const ACharacter: string);
var
  Bmp: TBitmap;
begin
  Bmp:= CreateCharacterBitmap(
    Manager.Settings.EditorFont.Name,
    ACharacter,
    imgChar.Height
  );
  try
    imgChar.Picture.Graphic := Bmp
  finally
    Bmp.Free;
  end;
end;

procedure TfrmCharacterMap.UpdateUnicodeDisplay(ACol, ARow: Integer);
//var
//  I     : Integer;
//  Start : Cardinal;
//  T1    : string;
//  T2    : string;
begin
//  Start  := UnicodeBlocks[cbxUnicodeRange.ItemIndex].S + ACol + (ARow * 16);
//  T1 := UnicodeToUTF8(Start);
//  T2 := '';
//  for I := 1 to Length(T1) do
//    T2 := T2 + '$' + IntToHex(Ord(T1[I]), 2);
//  lblUnicodeCharInfo.Caption := 'U+' + IntToHex(Start, 4) + ', UTF-8 = ' + T2;
//  UpdateCharacterBitmap(grdUnicode.Cells[ACol, ARow]);
end;

procedure TfrmCharacterMap.UpdateANSIDisplay(ACol, ARow: Integer);
//var
//  B: Byte;
begin
  if grdANSI.Cells[ACol, ARow] <> '' then
  begin
//    B  := Ord(grdANSI.Cells[ACol, ARow][1]);
//    lblCharInfo.Caption := 'Decimal = ' + IntToStr(B) +
//      ', Hex = $' + HexStr(B, 2);
//    UpdateCharacterBitmap(grdANSI.Cells[ACol, ARow]);
  end
  else
    lblCharInfo.Caption := '-';
end;
{$ENDREGION}

end.

