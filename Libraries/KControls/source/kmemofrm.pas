{ @abstract(This file is part of the KControls component suite for Delphi and Lazarus.)
  @author(Tomas Krysl)

  Copyright (c) 2020 Tomas Krysl<BR><BR>

  <B>License:</B><BR>
  This code is licensed under BSD 3-Clause Clear License, see file License.txt or https://spdx.org/licenses/BSD-3-Clause-Clear.html.
}

unit kmemofrm; // lowercase name because of Lazarus/Linux

{$include kcontrols.inc}
{$WEAKPACKAGEUNIT ON}

interface

uses
{$IFDEF FPC}
  LCLType, LCLIntf, LMessages, LCLProc, LResources,
{$ELSE}
  Windows, Messages,
{$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms, Menus,
  Dialogs, ToolWin, ComCtrls, ImgList, KFunctions, KControls, KMemo, ActnList,
  KDialogs, KMemoDlgParaStyle, KMemoDlgTextStyle, KMemoDlgHyperlink,
  KMemoDlgImage, KMemoDlgNumbering, KMemoDlgContainer, System.Actions,
  System.ImageList;

type

  { TKMemoFrame }

  TKMemoFrame = class(TFrame)
    ACParaNumbering: TAction;
    ACEditImage: TAction;
    ACInsertImage: TAction;
    Editor: TKMemo;
    ILMain: TImageList;
    PMIEditImage: TMenuItem;
    ToBFirst: TToolBar;
    ToBNew: TToolButton;
    ToBOpen: TToolButton;
    ToBSave: TToolButton;
    ToBSep1: TToolButton;
    ToBCut: TToolButton;
    ToBCopy: TToolButton;
    ToBPaste: TToolButton;
    ToBPrint: TToolButton;
    ToBPreview: TToolButton;
    ToBSep3: TToolButton;
    ALMain: TActionList;
    ACEditCopy: TKMemoEditCopyAction;
    ACEditCut: TKMemoEditCutAction;
    ACEditPaste: TKMemoEditPasteAction;
    ACFileOpen: TAction;
    ACFileNew: TAction;
    ACFilePrint: TAction;
    ACFileSave: TAction;
    ACFilePreview: TAction;
    ACFontBold: TAction;
    ACFontItalic: TAction;
    ACFontUnderline: TAction;
    ACFontStrikeout: TAction;
    ACFontStyle: TAction;
    ACParaLeft: TAction;
    ACParaCenter: TAction;
    ACParaRight: TAction;
    ACParaIncIndent: TAction;
    ACParaDecIndent: TAction;
    ACParaStyle: TAction;
    ODMain: TOpenDialog;
    SDMain: TSaveDialog;
    ACFileSaveAs: TAction;
    ToBSaveAs: TToolButton;
    PrintSetupDialog: TKPrintSetupDialog;
    PrintPreviewDialog: TKPrintPreviewDialog;
    ACFormatCopy: TAction;
    ACShowFormatting: TAction;
    ToBShowFormatting: TToolButton;
    ToBSep2: TToolButton;
    ACInsertHyperlink: TAction;
    ToBInsertHyperlink: TToolButton;
    PMMain: TPopupMenu;
    PMIEditCopy: TMenuItem;
    PMIEditCut: TMenuItem;
    PMIEditPaste: TMenuItem;
    N1: TMenuItem;
    PMIEditSelectAll: TMenuItem;
    ACEditSelectAll: TKMemoEditSelectAllAction;
    N2: TMenuItem;
    PMIFontStyle: TMenuItem;
    PMIParaStyle: TMenuItem;
    PMIEditHyperlink: TMenuItem;
    N3: TMenuItem;
    ACEditHyperlink: TAction;
    ToBSecond: TToolBar;
    ToBFormatCopy: TToolButton;
    ToBSep4: TToolButton;
    ToBFontBold: TToolButton;
    ToBFontItalic: TToolButton;
    ToBFontUnderline: TToolButton;
    ToBFont: TToolButton;
    ToBSep5: TToolButton;
    ToBParaLeft: TToolButton;
    ToBParaCenter: TToolButton;
    ToBParaRight: TToolButton;
    ToBParaIncIndent: TToolButton;
    ToBParaDecIndent: TToolButton;
    ToBParaNumbering: TToolButton;
    ToBPara: TToolButton;
    ToBFontSubscript: TToolButton;
    ACFontSuperscript: TAction;
    ACFontSubscript: TAction;
    ToBFontSuperscript: TToolButton;
    ToBSelectAll: TToolButton;
    ToBInsertImage: TToolButton;
    MainMenu: TMainMenu;
    MGFile: TMenuItem;
    MGEdit: TMenuItem;
    MGFont: TMenuItem;
    MGInsert: TMenuItem;
    MIFileNew: TMenuItem;
    MIFileOpen: TMenuItem;
    MIFileSave: TMenuItem;
    MiFileSaveAs: TMenuItem;
    MIFilePreview: TMenuItem;
    MIFilePrint: TMenuItem;
    MIFileExit: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    MIEditCopy: TMenuItem;
    MIEditCut: TMenuItem;
    MIEditPaste: TMenuItem;
    MIEditSelectAll: TMenuItem;
    MIFontBold: TMenuItem;
    MIFontItalic: TMenuItem;
    MiFontStrikeout: TMenuItem;
    MIFontUnderline: TMenuItem;
    MIFontSubscript: TMenuItem;
    MIFontSuperscript: TMenuItem;
    MIFontStyle: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    MIFormatCopy: TMenuItem;
    MGPara: TMenuItem;
    MIParaLeft: TMenuItem;
    MIParaCenter: TMenuItem;
    MIParaRight: TMenuItem;
    MIParaIncIndent: TMenuItem;
    N8: TMenuItem;
    MIParaDecIndent: TMenuItem;
    N9: TMenuItem;
    MIParaNumbering: TMenuItem;
    N10: TMenuItem;
    MIParaStyle: TMenuItem;
    N11: TMenuItem;
    N12: TMenuItem;
    MIInsertHyperlink: TMenuItem;
    MIInsertImage: TMenuItem;
    MIShowFormatting: TMenuItem;
    ACEditContainer: TAction;
    ACInsertContainer: TAction;
    MIInsertContainer: TMenuItem;
    PMIEditContainer: TMenuItem;
    ToBInsertContainer: TToolButton;
    procedure ACFileNewExecute(Sender: TObject);
    procedure ACFileNewUpdate(Sender: TObject);
    procedure ACFileOpenExecute(Sender: TObject);
    procedure ACFileSaveExecute(Sender: TObject);
    procedure ACFileSaveUpdate(Sender: TObject);
    procedure ACFileSaveAsExecute(Sender: TObject);
    procedure ACFilePrintExecute(Sender: TObject);
    procedure ACFilePreviewExecute(Sender: TObject);
    procedure ACFontBoldExecute(Sender: TObject);
    procedure ACFontBoldUpdate(Sender: TObject);
    procedure ACFontStyleUpdate(Sender: TObject);
    procedure ACFontItalicExecute(Sender: TObject);
    procedure ACFontItalicUpdate(Sender: TObject);
    procedure ACFontUnderlineExecute(Sender: TObject);
    procedure ACFontUnderlineUpdate(Sender: TObject);
    procedure ACFontStrikeoutExecute(Sender: TObject);
    procedure ACFontStrikeoutUpdate(Sender: TObject);
    procedure ACFontStyleExecute(Sender: TObject);
    procedure ACParaLeftExecute(Sender: TObject);
    procedure ACParaLeftUpdate(Sender: TObject);
    procedure ACParaCenterUpdate(Sender: TObject);
    procedure ACParaCenterExecute(Sender: TObject);
    procedure ACParaRightExecute(Sender: TObject);
    procedure ACParaRightUpdate(Sender: TObject);
    procedure ACParaIncIndentUpdate(Sender: TObject);
    procedure ACParaIncIndentExecute(Sender: TObject);
    procedure ACParaDecIndentExecute(Sender: TObject);
    procedure ACParaDecIndentUpdate(Sender: TObject);
    procedure ACParaStyleUpdate(Sender: TObject);
    procedure ACParaStyleExecute(Sender: TObject);
    procedure ACFormatCopyExecute(Sender: TObject);
    procedure ACShowFormattingExecute(Sender: TObject);
    procedure ACShowFormattingUpdate(Sender: TObject);
    procedure ACInsertHyperlinkExecute(Sender: TObject);
    procedure ACEditHyperlinkUpdate(Sender: TObject);
    procedure ACParaNumberingExecute(Sender: TObject);
    procedure ACParaNumberingUpdate(Sender: TObject);
    procedure ACFontSuperscriptExecute(Sender: TObject);
    procedure ACFontSuperscriptUpdate(Sender: TObject);
    procedure ACFontSubscriptExecute(Sender: TObject);
    procedure ACFontSubscriptUpdate(Sender: TObject);
    procedure ACInsertImageExecute(Sender: TObject);
    procedure ACEditImageUpdate(Sender: TObject);
    procedure PMMainPopup(Sender: TObject);
    procedure MIFileExitClick(Sender: TObject);
    procedure ACEditContainerUpdate(Sender: TObject);
    procedure ACInsertContainerExecute(Sender: TObject);
    procedure EditorBlockEdit(Sender: TObject; ABlock: TKMemoBlock; var Result: Boolean);
    procedure EditorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditorDropFiles(Sender: TObject; X, Y: Integer; Files: TStrings);
  private
    { Private declarations }
    FDefaultIndent: Integer;
    FDefaultTextBoxBorderWidth: Integer;
    FDefaultTextBoxMargin: Integer;
    FDefaultTextBoxPadding: Integer;
    FDefaultTextBoxSize: TPoint;
    FNewFile: Boolean;
    FLastFileName: TKString;
    procedure ParaStyleChanged(Sender: TObject; AReasons: TKMemoUpdateReasons);
    function SelectedBlock: TKMemoBlock;
    procedure TextStyleChanged(Sender: TObject);
  protected
    FActionStateChanging: Boolean;
    FFormatCopyParaStyle: TKMemoParaStyle;
    FFormatCopyTextStyle: TKMemoTextStyle;
    FContainerForm: TKMemoContainerForm;
    FHyperlinkForm: TKMemoHyperlinkForm;
    FNumberingForm: TKMemoNumberingForm;
    FImageForm: TKMemoImageForm;
    FParaStyle: TKMemoParaStyle;
    FParaStyleForm: TKMemoParaStyleForm;
    FTextStyle: TKMemoTextStyle;
    FTextStyleForm: TKMemoTextStyleForm;
    procedure AddToMRUFs(const AFileName: TKString); virtual;
    procedure DeleteFromMRUFs(const AFileName: TKString); virtual;
    procedure ChangeActionState(AAction: TObject; AState: Boolean);
    function EditContainer(AItem: TKMemoBlock): Boolean; virtual;
    function EditImage(AItem: TKMemoBlock): Boolean; virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CloseFile;
    procedure OpenNewFile;
    procedure OpenFile(FileName: TKString);
    function SaveFile(SaveAs, NeedAnotherOp: Boolean): Boolean;
    property DefaultIndent: Integer read FDefaultIndent write FDefaultIndent;
    property DefaultTextBoxSize: TPoint read FDefaultTextBoxSize write FDefaultTextBoxSize;
    property DefaultTextBoxBorderWidth: Integer read FDefaultTextBoxBorderWidth write FDefaultTextBoxBorderWidth;
    property DefaultTextBoxMargin: Integer read FDefaultTextBoxMargin write FDefaultTextBoxMargin;
    property DefaultTextBoxPadding: Integer read FDefaultTextBoxPadding write FDefaultTextBoxPadding;
    property NewFile: Boolean read FNewFile;
    property LastFileName: TKString read FLastFileName;
  end;

implementation

{$IFDEF FPC}
  {$R *.lfm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

uses
  Math,
  KEditCommon, KGraphics, KMessageBox, KRes;

constructor TKMemoFrame.Create(AOwner: TComponent);
begin
  inherited;
  FActionStateChanging := False;
  FDefaultIndent := 20;
  FDefaultTextBoxSize := Point(200, 150);
  FDefaultTextBoxBorderWidth := 2;
  FDefaultTextBoxMargin := 5;
  FDefaultTextBoxPadding := 5;
  FLastFileName := '';
  FNewFile := False;
  FContainerForm := TKMemoContainerForm.Create(Self);
  FFormatCopyParaStyle := TKMemoParaStyle.Create;
  FFormatCopyTextStyle := TKMemoTextStyle.Create;
  FHyperlinkForm := TKMemoHyperlinkForm.Create(Self);
  FImageForm := TKMemoImageForm.Create(Self);
  FNumberingForm := TKMemoNumberingForm.Create(Self);
  FParaStyle := TKMemoParaStyle.Create;
  FParaStyle.OnChanged := ParaStyleChanged;
  FParaStyleForm := TKMemoParaStyleForm.Create(Self);
  FTextStyle := TKMemoTextStyle.Create;
  FTextStyle.OnChanged := TextStyleChanged;
  FTextStyleForm := TKMemoTextStyleForm.Create(Self);
  OpenNewFile;
end;

destructor TKMemoFrame.Destroy;
begin
  FFormatCopyTextStyle.Free;
  FFormatCopyParaStyle.Free;
  FParaStyle.Free;
  FTextStyle.Free;
  inherited;
end;

procedure TKMemoFrame.ACEditContainerUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := SelectedBlock is TKMemoContainer;
end;

procedure TKMemoFrame.ACEditHyperlinkUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := SelectedBlock is TKMemoHyperlink;
end;

procedure TKMemoFrame.ACEditImageUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := SelectedBlock is TKMemoImageBlock;
end;

procedure TKMemoFrame.ACFileNewExecute(Sender: TObject);
begin
  OpenNewFile;
end;

procedure TKMemoFrame.ACFileNewUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := True;
end;

procedure TKMemoFrame.ACFileOpenExecute(Sender: TObject);
begin
  OpenFile('');
end;

procedure TKMemoFrame.ACFilePreviewExecute(Sender: TObject);
begin
  Editor.PageSetup.Title := FLastFileName;
  PrintPreviewDialog.Execute;
end;

procedure TKMemoFrame.ACFilePrintExecute(Sender: TObject);
begin
  Editor.PageSetup.Title := FLastFileName;
  PrintSetupDialog.Execute;
end;

procedure TKMemoFrame.ACFileSaveAsExecute(Sender: TObject);
begin
  SaveFile(True, False);
end;

procedure TKMemoFrame.ACFileSaveExecute(Sender: TObject);
begin
  SaveFile(False, False);
end;

procedure TKMemoFrame.ACFileSaveUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Editor.Modified;
end;

procedure TKMemoFrame.ACFontBoldExecute(Sender: TObject);
begin
  if not FActionStateChanging then
    if TAction(Sender).Checked then
      FTextStyle.Font.Style := FTextStyle.Font.Style - [fsBold]
    else
      FTextStyle.Font.Style := FTextStyle.Font.Style + [fsBold];
end;

procedure TKMemoFrame.ACFontBoldUpdate(Sender: TObject);
begin
  ChangeActionState(Sender, fsBold in FTextStyle.Font.Style);
end;

procedure TKMemoFrame.ACFontItalicExecute(Sender: TObject);
begin
  if not FActionStateChanging then
    if TAction(Sender).Checked then
      FTextStyle.Font.Style := FTextStyle.Font.Style - [fsItalic]
    else
      FTextStyle.Font.Style := FTextStyle.Font.Style + [fsItalic];
end;

procedure TKMemoFrame.ACFontItalicUpdate(Sender: TObject);
begin
  ChangeActionState(Sender, fsItalic in FTextStyle.Font.Style);
end;

procedure TKMemoFrame.ACFontStrikeoutExecute(Sender: TObject);
begin
  if not FActionStateChanging then
    if TAction(Sender).Checked then
      FTextStyle.Font.Style := FTextStyle.Font.Style - [fsStrikeout]
    else
      FTextStyle.Font.Style := FTextStyle.Font.Style + [fsStrikeout];
end;

procedure TKMemoFrame.ACFontStrikeoutUpdate(Sender: TObject);
begin
  ChangeActionState(Sender, fsStrikeout in FTextStyle.Font.Style);
end;

procedure TKMemoFrame.ACFontStyleExecute(Sender: TObject);
begin
  FTextStyleForm.Load(FTextStyle);
  if FTextStyleForm.ShowModal = mrOk then
    FTextStyleForm.Save(FTextStyle);
end;

procedure TKMemoFrame.ACFontStyleUpdate(Sender: TObject);
begin
  FTextStyle.OnChanged := nil;
  try
    if Editor.NewTextStyleValid then
      FTextStyle.Assign(Editor.NewTextStyle)
    else
      FTextStyle.Assign(Editor.SelectionTextStyle);
  finally
    FTextStyle.OnChanged := TextStyleChanged;
  end;
end;

procedure TKMemoFrame.ACFontSubscriptExecute(Sender: TObject);
begin
  if not FActionStateChanging then
    if TAction(Sender).Checked then
      FTextStyle.ScriptPosition := tpoNormal
    else
      FTextStyle.ScriptPosition := tpoSubscript;
end;

procedure TKMemoFrame.ACFontSubscriptUpdate(Sender: TObject);
begin
  ChangeActionState(Sender, FTextStyle.ScriptPosition = tpoSubScript);
end;

procedure TKMemoFrame.ACFontSuperscriptExecute(Sender: TObject);
begin
  if not FActionStateChanging then
    if TAction(Sender).Checked then
      FTextStyle.ScriptPosition := tpoNormal
    else
      FTextStyle.ScriptPosition := tpoSuperscript;
end;

procedure TKMemoFrame.ACFontSuperscriptUpdate(Sender: TObject);
begin
  ChangeActionState(Sender, FTextStyle.ScriptPosition = tpoSuperScript);
end;

procedure TKMemoFrame.ACFontUnderlineExecute(Sender: TObject);
begin
  if not FActionStateChanging then
    if TAction(Sender).Checked then
      FTextStyle.Font.Style := FTextStyle.Font.Style - [fsUnderline]
    else
      FTextStyle.Font.Style := FTextStyle.Font.Style + [fsUnderline];
end;

procedure TKMemoFrame.ACFontUnderlineUpdate(Sender: TObject);
begin
  ChangeActionState(Sender, fsUnderline in FTextStyle.Font.Style);
end;

procedure TKMemoFrame.ACFormatCopyExecute(Sender: TObject);
begin
  FFormatCopyParaStyle.Assign(FParaStyle);
  FFormatCopyTextStyle.Assign(FTextStyle);
  TAction(Sender).Checked := True;
end;

procedure TKMemoFrame.EditorBlockEdit(Sender: TObject; ABlock: TKMemoBlock;
  var Result: Boolean);
begin
  if ABlock is TKMemoImageBlock then
    Result := EditImage(ABlock)
  else if ABlock is TKMemoContainer then
    Result := EditContainer(ABlock);
end;

procedure TKMemoFrame.ACInsertContainerExecute(Sender: TObject);
begin
  EditContainer(SelectedBlock);
end;

procedure TKMemoFrame.ACInsertHyperlinkExecute(Sender: TObject);
var
  Block: TKMemoBlock;
  Hyperlink: TKMemoHyperlink;
  Created: Boolean;
begin
  Created := False;
  if Editor.SelAvail then
  begin
    Hyperlink := TKMemoHyperlink.Create;
    Hyperlink.Text := Editor.SelText;
    Block := Editor.ActiveInnerBlock;
    if Block is TKMemoHyperlink then
      Hyperlink.URL := TKMemoHyperlink(Block).URL;
    Created := True;
  end else
  begin
    Block := Editor.ActiveInnerBlock;
    if Block is TKMemoHyperlink then
      Hyperlink := TKMemoHyperlink(Block)
    else
    begin
      Hyperlink := TKMemoHyperlink.Create;
      Created := True;
    end;
  end;
  FHyperlinkForm.Load(Hyperlink);
  if FHyperlinkForm.ShowModal = mrOk then
  begin
    FHyperlinkForm.Save(Hyperlink);
    if Created then
    begin
      if Editor.SelAvail then
        Editor.ClearSelection;
      Editor.ActiveInnerBlocks.AddHyperlink(Hyperlink, Editor.SplitAt(Editor.SelEnd));
    end;
    Editor.Modified := True;
  end
  else if Created then
    Hyperlink.Free;
end;

procedure TKMemoFrame.ACInsertImageExecute(Sender: TObject);
begin
  EditImage(SelectedBlock);
end;

procedure TKMemoFrame.ACParaCenterExecute(Sender: TObject);
begin
  if not FActionStateChanging then
    FParaStyle.HAlign := halCenter;
end;

procedure TKMemoFrame.ACParaCenterUpdate(Sender: TObject);
begin
  ChangeActionState(Sender, FParaStyle.HAlign = halCenter);
end;

procedure TKMemoFrame.ACParaDecIndentExecute(Sender: TObject);
begin
  FParaStyle.LeftPadding := Max(FParaStyle.LeftPadding - Editor.Pt2PxX(FDefaultIndent), 0);
end;

procedure TKMemoFrame.ACParaDecIndentUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := FParaStyle.LeftPadding > 0;
end;

procedure TKMemoFrame.ACParaIncIndentExecute(Sender: TObject);
begin
  FParaStyle.LeftPadding := Min(FParaStyle.LeftPadding + Editor.Pt2PxX(FDefaultIndent), Editor.RequiredContentWidth - FParaStyle.RightPadding - Editor.Pt2PxX(FDefaultIndent));
end;

procedure TKMemoFrame.ACParaIncIndentUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := FParaStyle.LeftPadding < Editor.RequiredContentWidth - FParaStyle.RightPadding - Editor.Pt2PxX(FDefaultIndent);
end;

procedure TKMemoFrame.ACParaLeftExecute(Sender: TObject);
begin
  if not FActionStateChanging then
    FParaStyle.HAlign := halLeft;
end;

procedure TKMemoFrame.ACParaLeftUpdate(Sender: TObject);
begin
  ChangeActionState(Sender, FParaStyle.HAlign = halLeft);
end;

procedure TKMemoFrame.ACParaNumberingExecute(Sender: TObject);
begin
  FNumberingForm.Load(Editor, Editor.ListTable, Editor.NearestParagraph);
  if FNumberingForm.ShowModal = mrOk then
    FNumberingForm.Save;
end;

procedure TKMemoFrame.ACParaNumberingUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Editor.NearestParagraph <> nil;
end;

procedure TKMemoFrame.ACParaRightExecute(Sender: TObject);
begin
  if not FActionStateChanging then
    FParaStyle.HAlign := halRight;
end;

procedure TKMemoFrame.ACParaRightUpdate(Sender: TObject);
begin
  ChangeActionState(Sender, FParaStyle.HAlign = halRight);
end;

procedure TKMemoFrame.ACParaStyleExecute(Sender: TObject);
begin
  FParaStyleForm.Load(Editor, FParaStyle);
  if FParaStyleForm.ShowModal = mrOk then
    FParaStyleForm.Save(FParaStyle);
end;

procedure TKMemoFrame.ACParaStyleUpdate(Sender: TObject);
begin
  FParaStyle.OnChanged := nil;
  try
    FParaStyle.Assign(Editor.SelectionParaStyle);
  finally
    FParaStyle.OnChanged := ParaStyleChanged;
  end;
end;

procedure TKMemoFrame.ACShowFormattingExecute(Sender: TObject);
begin
  if not FActionStateChanging then
    if ACShowFormatting.Checked then
      Editor.Options := Editor.Options - [eoShowFormatting]
    else
      Editor.Options := Editor.Options + [eoShowFormatting];
end;

procedure TKMemoFrame.ACShowFormattingUpdate(Sender: TObject);
begin
  ChangeActionState(Sender, eoShowFormatting in Editor.Options);
end;

procedure TKMemoFrame.AddToMRUFs(const AFileName: TKString);
begin
end;

procedure TKMemoFrame.ChangeActionState(AAction: TObject; AState: Boolean);
begin
  FActionStateChanging := True;
  try
    TAction(AAction).Checked := AState;
  finally
    FActionStateChanging := False;
  end;
end;

procedure TKMemoFrame.CloseFile;
begin
  Editor.Clear;
  if FLastFileName <> '' then
    AddToMRUFs(FLastFileName);
  FLastFileName := '';
end;

procedure TKMemoFrame.DeleteFromMRUFs(const AFileName: TKString);
begin
end;

function TKMemoFrame.EditContainer(AItem: TKMemoBlock): Boolean;
var
  Cont: TKMemoContainer;
  Blocks: TKMemoBlocks;
  Created: Boolean;
begin
  Result := False;
  Created := False;
  if (AItem is TKMemoContainer) and (AItem.Position <> mbpText) then
    Cont := TKMemoContainer(AItem)
  else
  begin
    Blocks := AItem.ParentRootBlocks;
    if (Blocks.Parent is TKMemoContainer) and (Blocks.Parent.Position <> mbpText) then
      Cont := TKMemoContainer(Blocks.Parent)
    else
    begin
      Cont := TKMemoContainer.Create;
      Cont.BlockStyle.ContentPadding.All := Editor.Pt2PxX(FDefaultTextBoxPadding);
      Cont.BlockStyle.ContentMargin.All := Editor.Pt2PxX(FDefaultTextBoxMargin);
      Cont.FixedWidth := True;
      Cont.FixedHeight := True;
      Cont.RequiredWidth := Editor.Pt2PxX(FDefaultTextBoxSize.X);
      Cont.RequiredHeight := Editor.Pt2PxY(FDefaultTextBoxSize.Y);
      Cont.BlockStyle.BorderWidth := Editor.Pt2PxX(FDefaultTextBoxBorderWidth);
      Cont.InsertString(sMemoSampleTextBox + cEOL);
      Created := True;
    end;
  end;
  FContainerForm.Load(Editor, Cont);
  if FContainerForm.ShowModal = mrOk then
  begin
    FContainerForm.Save(Cont);
    if Created then
    begin
      if Editor.SelAvail then
        Editor.ClearSelection;
      Editor.ActiveBlocks.AddAt(Cont, Editor.NearestParagraphIndex + 1);
    end;
    Editor.Modified := True;
    Result := True;
  end
  else if Created then
    Cont.Free;
end;

function TKMemoFrame.EditImage(AItem: TKMemoBlock): Boolean;
var
  Image: TKMemoImageBlock;
  Created: Boolean;
begin
  Result := False;
  Created := False;
  if AItem is TKMemoImageBlock then
    Image := TKMemoImageBlock(AItem)
  else
  begin
    Image := TKMemoImageBlock.Create;
    Created := True;
  end;
  FImageForm.Load(Editor, Image);
  if FImageForm.ShowModal = mrOk then
  begin
    FImageForm.Save(Image);
    if Created then
    begin
      if Editor.SelAvail then
        Editor.ClearSelection;
      Editor.ActiveInnerBlocks.AddAt(Image, Editor.SplitAt(Editor.SelEnd));
    end;
    Editor.Modified := True;
    Result := True;
  end
  else if Created then
    Image.Free;
end;

procedure TKMemoFrame.EditorDropFiles(Sender: TObject; X, Y: Integer;
  Files: TStrings);
begin
  if Files.Count > 0 then
  begin
    Application.BringToFront;
    OpenFile(Files[0]);
  end;
end;

procedure TKMemoFrame.EditorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
    Editor.MoveCaretToMouseCursor(True);
end;

procedure TKMemoFrame.EditorMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ACFormatCopy.Checked then
  begin
    if Editor.SelAvail then
    begin
      Editor.SelectionTextStyle := FFormatCopyTextStyle;
      if Editor.SelectionHasPara then
        Editor.SelectionParaStyle := FFormatCopyParaStyle;
    end;
    ACFormatCopy.Checked := False;
  end;
end;

procedure TKMemoFrame.MIFileExitClick(Sender: TObject);
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  if Form <> nil then
    Form.Close;
end;

procedure TKMemoFrame.OpenNewFile;
begin
  if SaveFile(False, True) then
  begin
    CloseFile;
    FNewFile := True;
  end;
end;

procedure TKMemoFrame.OpenFile(FileName: TKString);
begin
  if SaveFile(False, True) then
  begin
    if FileName = '' then
      if ODMain.Execute then
        FileName := ODMain.FileName;
    if FileName <> '' then
    begin
      CloseFile;
      if ExtractFileDir(FileName) = '' then
        FileName := Format('%s\%s', [GetCurrentDir, FileName]);
      try
        Editor.LoadFromFile(FileName);
        FLastFileName := FileName;
        FNewFile := False;
      except
        KMsgBox(sAppError, Format(sErrMemoLoadFromFile, [FileName]), [mbOk], miStop);
      end;
      DeleteFromMRUFs(FileName);
    end;
  end;
end;

procedure TKMemoFrame.ParaStyleChanged(Sender: TObject; AReasons: TKMemoUpdateReasons);
begin
  Editor.SelectionParaStyle := FParaStyle;
end;

procedure TKMemoFrame.PMMainPopup(Sender: TObject);
begin
  ACEditImage.Update;
  ACEditHyperlink.Update;
  ACEditContainer.Update;
end;

function TKMemoFrame.SaveFile(SaveAs, NeedAnotherOp: Boolean): Boolean;
var
  NeedDlg: Boolean;
  FileName: string;
begin
  Result := False;
  if FNewFile then
    FileName := sMemoDefaultFileName
  else
    FileName := ExtractFileName(FLastFileName);
  if NeedAnotherOp then
  begin
    if Editor.Modified then
    begin
      case KMsgBox(sAppQuery, Format(sQueryFileSave, [FileName]), [mbYes, mbNo, mbCancel], miQuestion) of
        2: Result := True;
        3: Exit;
      end
    end else
      Result := True;
  end;
  if not Result then
  begin
    NeedDlg := FNewFile or SaveAs;
    SDMain.FileName := FileName;
    if not NeedDlg or SDMain.Execute then
    begin
      if NeedDlg then FLastFileName := SDMain.Filename;
      try
        Editor.SaveToFile(FLastFileName);
        Editor.Modified := False;
        FNewFile := False;
        Result := True;
      except
        KMsgBox(sAppError, Format(sErrMemoSaveToFile, [FileName]), [mbOk], miStop);
      end;
    end;
  end;
  if Result then
    Editor.Modified := False;
end;

function TKMemoFrame.SelectedBlock: TKMemoBlock;
begin
  Result := Editor.SelectedBlock;
  if Result = nil then
    Result := Editor.ActiveInnerBlock;
end;

procedure TKMemoFrame.TextStyleChanged(Sender: TObject);
var
  SelAvail: Boolean;
  SelEnd, StartIndex, EndIndex: TKMemoSelectionIndex;
begin
  // if there is no selection then simulate one word selection or set style for new text
  SelAvail := Editor.SelAvail;
  SelEnd := Editor.SelEnd;
  if SelAvail then
    Editor.SelectionTextStyle := FTextStyle
  else if Editor.GetNearestWordIndexes(SelEnd, False, StartIndex, EndIndex) and (StartIndex < SelEnd) and (SelEnd < EndIndex) then
    // simulate MS Word behavior here, SelEnd is caret position
    // do not select the word if we are at the beginning or end of the word
    // and allow set another text style for newly added text
    Editor.SetRangeTextStyle(StartIndex, EndIndex, FTextStyle)
  else
    Editor.NewTextStyle := FTextStyle;
end;

end.
