{ @abstract(This file is part of the KControls component suite for Delphi and Lazarus.)
  @author(Tomas Krysl)

  Copyright (c) 2020 Tomas Krysl<BR><BR>

  <B>License:</B><BR>
  This code is licensed under BSD 3-Clause Clear License, see file License.txt or https://spdx.org/licenses/BSD-3-Clause-Clear.html.
}

unit kmemodlgparastyle; // lowercase name because of Lazarus/Linux

interface

uses
{$IFDEF FPC}
  LCLType, LCLIntf, LMessages, LCLProc, LResources,
{$ELSE}
  Windows, Messages,
{$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, KEdits, KControls, KButtons, KMemo;

type
  TKMemoParaStyleForm = class(TForm)
    BUOk: TButton;
    BUCancel: TButton;
    GBCommon: TGroupBox;
    CoBAlign: TComboBox;
    LBalignment: TLabel;
    GBIndent: TGroupBox;
    LBFirstIndent: TLabel;
    CoBFIrstIndent: TComboBox;
    EDFirstIndent: TKNumberEdit;
    EDLeftIndent: TKNumberEdit;
    LBLeftIndent: TLabel;
    EDRightIndent: TKNumberEdit;
    LBRightIndent: TLabel;
    GBSpacing: TGroupBox;
    LBSpaceAbove: TLabel;
    LBSpaceBelow: TLabel;
    EDSpaceAbove: TKNumberEdit;
    EDSpaceBelow: TKNumberEdit;
    GBShading: TGroupBox;
    LBBorderLeft: TLabel;
    LBBorderRight: TLabel;
    EDBorderLeft: TKNumberEdit;
    EDBorderRight: TKNumberEdit;
    LBBorderTop: TLabel;
    LBBorderBottom: TLabel;
    EDBorderTop: TKNumberEdit;
    EDBorderBottom: TKNumberEdit;
    CLBBorder: TKColorButton;
    CLBShading: TKColorButton;
    LBBorderColor: TLabel;
    LBShading: TLabel;
    LBBorderRadius: TLabel;
    EDBorderRadius: TKNumberEdit;
    CBWordWrap: TCheckBox;
    LBLineSpacing: TLabel;
    CoBLineSpacing: TComboBox;
    LBLineSpacingValue: TLabel;
    EDLineSpacingValue: TKNumberEdit;
    procedure CoBLineSpacingClick(Sender: TObject);
  private
    { Private declarations }
    FMemo: TKmemo;
    FStyle: TKMemoParaStyle;
    procedure UpdateLineSpacingValue;
  public
    { Public declarations }
    procedure Load(AMemo: TKMemo; AStyle: TKMemoParaStyle);
    procedure Save(AStyle: TKMemoParaStyle);
  end;

implementation

{$IFDEF FPC}
 {$R *.lfm}
{$ELSE}
 {$R *.dfm}
{$ENDIF}

uses
  Math, KFunctions, KGraphics;

const
  cLsSingle = 0;
  cLs1HalfLines = 1;
  cLsDouble = 2;
  cLsLeast = 3;
  cLsExact = 4;
  cLsFactor = 5;

  cSpNone = 0;
  cSpFirstLine = 1;
  cSpHanging = 2;

{ TKMemoParaStyleForm }

procedure TKMemoParaStyleForm.CoBLineSpacingClick(Sender: TObject);
begin
  UpdateLineSpacingValue;
end;

procedure TKMemoParaStyleForm.Load(AMemo: TKMemo; AStyle: TKMemoParaStyle);
begin
  Assert(AMemo <> nil);
  Assert(AStyle <> nil);
  FMemo := AMemo;
  FStyle := AStyle;
  CoBAlign.ItemIndex := Integer(AStyle.HAlign);
  EDLeftIndent.Value := FMemo.Px2PtX(AStyle.LeftPadding);
  EDRightIndent.Value := FMemo.Px2PtX(AStyle.RightPadding);
  EDFirstIndent.Value := FMemo.Px2PtX(Abs(AStyle.FirstIndent));
  if AStyle.FirstIndent = 0 then
    CoBFirstIndent.ItemIndex := cSpNone
  else if AStyle.FirstIndent > 0 then
    CoBFirstIndent.ItemIndex := cSpFirstLine
  else
    CoBFirstIndent.ItemIndex := cSpHanging;
  EDSpaceAbove.Value := FMemo.Px2PtY(AStyle.TopPadding);
  EDSpaceBelow.Value := FMemo.Px2PtY(AStyle.BottomPadding);
  EDBorderBottom.Value := FMemo.Px2PtY(AStyle.BorderWidths.Bottom);
  EDBorderLeft.Value := FMemo.Px2PtX(AStyle.BorderWidths.Left);
  EDBorderRight.Value := FMemo.Px2PtX(AStyle.BorderWidths.Right);
  EDBorderTop.Value := FMemo.Px2PtY(AStyle.BorderWidths.Top);
  EDBorderRadius.Value := FMemo.Px2PtX(AStyle.BorderRadius);
  CLBBorder.DlgColor := AStyle.BorderColor;
  if AStyle.Brush.Style <> bsClear then
    CLBShading.DlgColor := AStyle.Brush.Color
  else
    CLBShading.DlgColor := clNone;
  CBWordWrap.Checked := AStyle.WordWrap;
  case AStyle.LineSpacingMode of
    lsmFactor:
    begin
      if SameValue(AStyle.LineSpacingFactor, 1) then
        CoBLineSpacing.ItemIndex := cLsSingle
      else if SameValue(AStyle.LineSpacingFactor, 1.5) then
        CoBLineSpacing.ItemIndex := cLs1HalfLines
      else if SameValue(AStyle.LineSpacingFactor, 2) then
        CoBLineSpacing.ItemIndex := cLsDouble
      else
        CoBLineSpacing.ItemIndex := cLsFactor;
    end;
    lsmValue:
    begin
      if AStyle.LineSpacingValue >= 0 then
        CoBLineSpacing.ItemIndex := cLsLeast
      else
        CoBLineSpacing.ItemIndex := cLsExact;
    end;
  end;
  UpdateLineSpacingValue;
end;

procedure TKMemoParaStyleForm.Save(AStyle: TKMemoParaStyle);
begin
  if AStyle <> nil then
  begin
    AStyle.HAlign := TKHAlign(CoBAlign.ItemIndex);
    AStyle.LeftPadding := FMemo.Pt2PxX(EDLeftIndent.Value);
    AStyle.RightPadding := FMemo.Pt2PxX(EDRightIndent.Value);
    case CoBFirstIndent.ItemIndex of
      cSpFirstLine: AStyle.FirstIndent := FMemo.Pt2PxX(EDFirstIndent.Value);
      cSpHanging: AStyle.FirstIndent := FMemo.Pt2PxX(-EDFirstIndent.Value);
    else
      AStyle.FirstIndent := 0;
    end;
    AStyle.TopPadding := FMemo.Pt2PxY(EDSpaceAbove.Value);
    AStyle.BottomPadding := FMemo.Pt2PxY(EDSpaceBelow.Value);
    AStyle.BorderWidths.Bottom := FMemo.Pt2PxY(EDBorderBottom.Value);
    AStyle.BorderWidths.Left := FMemo.Pt2PxX(EDBorderLeft.Value);
    AStyle.BorderWidths.Right := FMemo.Pt2PxX(EDBorderRight.Value);
    AStyle.BorderWidths.Top := FMemo.Pt2PxY(EDBorderTop.Value);
    AStyle.BorderRadius := FMemo.Pt2PxX(EDBorderRadius.Value);
    AStyle.BorderColor := CLBBorder.DlgColor;
    if CLBShading.DlgColor <> clNone then
      AStyle.Brush.Color := CLBShading.DlgColor;
    AStyle.WordWrap := CBWordWrap.Checked;
    case CoBLineSpacing.ItemIndex of
      cLs1HalfLines:
      begin
        AStyle.LineSpacingMode := lsmFactor;
        AStyle.LineSpacingFactor := 1.5;
      end;
      cLsDouble:
      begin
        AStyle.LineSpacingMode := lsmFactor;
        AStyle.LineSpacingFactor := 2;
      end;
      cLsLeast:
      begin
        AStyle.LineSpacingMode := lsmValue;
        AStyle.LineSpacingValue := FMemo.Pt2PxY(MinMax(EDLineSpacingValue.Value, 1, 100));;
      end;
      cLsExact:
      begin
        AStyle.LineSpacingMode := lsmValue;
        AStyle.LineSpacingValue := -FMemo.Pt2PxY(MinMax(EDLineSpacingValue.Value, 1, 100));
      end;
      cLsFactor:
      begin
        AStyle.LineSpacingMode := lsmFactor;
        AStyle.LineSpacingFactor := MinMax(EDLineSpacingValue.Value / 100, 1, 10);
      end;
    else
      AStyle.LineSpacingMode := lsmFactor;
      AStyle.LineSpacingFactor := 1;
    end;
  end;
end;

procedure TKMemoParaStyleForm.UpdateLineSpacingValue;
begin
  EDLineSpacingValue.Min := 0;
  EDLineSpacingValue.Max := 1000;
  EDLineSpacingValue.Precision := 0;
  EDLineSpacingValue.CustomSuffix := '%';
  case CoBLineSpacing.ItemIndex of
    cLs1HalfLines:
    begin
      EDLineSpacingValue.Enabled := False;
      EDLineSpacingValue.Value := 150;
    end;
    cLsDouble:
    begin
      EDLineSpacingValue.Enabled := False;
      EDLineSpacingValue.Value := 200;
    end;
    cLsLeast, cLsExact:
    begin
      EDLineSpacingValue.Min := 1;
      EDLineSpacingValue.Max := 100;
      EDLineSpacingValue.CustomSuffix := 'pt';
      EDLineSpacingValue.Precision := 2;
      EDLineSpacingValue.Enabled := True;
      EDLineSpacingValue.Value := MinMax(FMemo.Px2PtY(Abs(FStyle.LineSpacingValue)), 1, 100);
    end;
    cLsFactor:
    begin
      EDLineSpacingValue.Min := 100;
      EDLineSpacingValue.Enabled := True;
      EDLineSpacingValue.Value := FStyle.LineSpacingFactor * 100;
    end;
  else
    EDLineSpacingValue.Enabled := False;
    EDLineSpacingValue.Value := 100;
  end;
end;

end.
