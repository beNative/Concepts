{ @abstract(This file is part of the KControls component suite for Delphi and Lazarus.)
  @author(Tomas Krysl)

  Copyright (c) 2020 Tomas Krysl<BR><BR>

  <B>License:</B><BR>
  This code is licensed under BSD 3-Clause Clear License, see file License.txt or https://spdx.org/licenses/BSD-3-Clause-Clear.html.
}

unit kmemodlgnumbering; // lowercase name because of Lazarus/Linux

interface

uses
{$IFDEF FPC}
  LCLType, LCLIntf, LMessages, LCLProc, LResources,
{$ELSE}
  Windows, Messages,
{$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, KEdits, KControls, KButtons, KMemo, ExtCtrls;

type
  TKMemoNumberingForm = class(TForm)
    BUOk: TButton;
    BUCancel: TButton;
    RGNumbering: TRadioGroup;
    GBOptions: TGroupBox;
    LBFirstIndent: TLabel;
    LBLeftIndent: TLabel;
    EDFirstIndent: TKNumberEdit;
    EDLeftIndent: TKNumberEdit;
    LBListLevel: TLabel;
    CoBListLevel: TComboBox;
    GBStartAt: TGroupBox;
    RBContinuous: TRadioButton;
    RBStartFromOne: TRadioButton;
    RBStartAt: TRadioButton;
    EDStartAt: TKNumberEdit;
    procedure RGNumberingClick(Sender: TObject);
    procedure CoBListLevelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RBContinuousClick(Sender: TObject);
  private
    { Private declarations }
    FMemo: TKMemo;
    FListID: Integer;
    FListLevel: Integer;
    FListTable: TKMemoListTable;
    FListTableCopy: TKMemoListTable;
    FLoading, FUpdating: Boolean;
    FPara: TKMemoParagraph;
    procedure UpdateFields;
  public
    { Public declarations }
    procedure Load(AMemo: TKMemo; AListTable: TKMemoListTable; APara: TKMemoParagraph);
    procedure Save;
  end;

implementation

{$IFDEF FPC}
 {$R *.lfm}
{$ELSE}
 {$R *.dfm}
{$ENDIF}

uses
  Math, KGraphics;

{ TKMemoNumberingForm }

procedure TKMemoNumberingForm.CoBListLevelClick(Sender: TObject);
begin
  if not FLoading then
  begin
    FListLevel := CoBListLevel.ItemIndex;
    UpdateFields;
  end;
end;

procedure TKMemoNumberingForm.FormCreate(Sender: TObject);
begin
  FListTableCopy := TKMemoListTable.Create;
  FLoading := False;
  FUpdating := False;
end;

procedure TKMemoNumberingForm.FormDestroy(Sender: TObject);
begin
  FListTableCopy.Free;
end;

procedure TKMemoNumberingForm.Load(AMemo: TKMemo; AListTable: TKMemoListTable; APara: TKMemoParagraph);
begin
  Assert(AMemo <> nil);
  Assert(AListTable <> nil);
  Assert(APara <> nil);
  FMemo := AMemo;
  FListTable := AListTable;
  FPara := APara;
  if FPara <> nil then
  begin
    FLoading := True;
    try
      FListTableCopy.Assign(FListTable);
      FListID := FPara.ParaStyle.NumberingList;
      FListLevel := FPara.ParaStyle.NumberingListLevel;
      RGNumbering.ItemIndex := Integer(FPara.Numbering);
      UpdateFields;
    finally
      FLoading := False;
    end;
  end;
end;

procedure TKMemoNumberingForm.RBContinuousClick(Sender: TObject);
begin
  EDStartAt.Enabled := RBStartAt.Enabled and RBStartAt.Checked;
end;

procedure TKMemoNumberingForm.RGNumberingClick(Sender: TObject);
begin
  if not FLoading then
    UpdateFields;
end;

procedure TKMemoNumberingForm.Save;
var
  ListLevel: TKMemoListLevel;
begin
  FListTable.Assign(FListTableCopy);
  FPara.ParaStyle.SetNumberingListAndLevel(FListID, FListLevel);
  ListLevel := FPara.NumberingListLevel;
  if ListLevel <> nil then
  begin
    ListLevel.FirstIndent := FMemo.Pt2PxX(EDFirstIndent.Value);
    ListLevel.LeftIndent := FMemo.Pt2PxX(EDLeftIndent.Value);
    if RBContinuous.Checked then
      FPara.ParaStyle.NumberStartAt := 0
    else if RBStartFromOne.Checked then
      FPara.ParaStyle.NumberStartAt := 1
    else
      FPara.ParaStyle.NumberStartAt := EDStartAt.ValueAsInt
  end;
end;

procedure TKMemoNumberingForm.UpdateFields;
var
  List: TKMemoList;
  ListLevel: TKMemoListLevel;
begin
  if not FUpdating and (FPara <> nil) then
  begin
    FUpdating := True;
    try
      FListLevel := Max(FListLevel, 0);
      List := FListTableCopy.ListByNumbering(FListID, FListLevel, TKMemoParaNumbering(RGNumbering.ItemIndex));
      if List <> nil then
      begin
        FListID := List.ID;
        ListLevel := List.Levels[FListLevel];
        CoBListLevel.ItemIndex := FListLevel;
        EDFirstIndent.Value := FMemo.Px2PtX(ListLevel.FirstIndent);
        EDLeftIndent.Value := FMemo.Px2PtX(ListLevel.LeftIndent);
        case FPara.ParaStyle.NumberStartAt of
          0: RBContinuous.Checked := True;
          1: RBStartFromOne.Checked := True;
        else
          RBStartAt.Checked := True;
          EDStartAt.ValueAsInt := FPara.ParaStyle.NumberStartAt;
        end;
      end else
      begin
        FListID := cInvalidListID;
        FListLevel := -1;
        CoBListLevel.ItemIndex := -1;
        EDFirstIndent.Value := 0;
        EDLeftIndent.Value := 0;
        RBContinuous.Checked := True;
        EDStartAt.ValueAsInt := 1;
      end;
      EDFirstIndent.Enabled := List <> nil;
      EDLeftIndent.Enabled := List <> nil;
      CoBListLevel.Enabled := List <> nil;
      RBContinuous.Enabled := List <> nil;
      RBStartFromOne.Enabled := List <> nil;
      RBStartAt.Enabled := List <> nil;
    finally
      FUpdating := False;
    end;
  end;
end;

end.
