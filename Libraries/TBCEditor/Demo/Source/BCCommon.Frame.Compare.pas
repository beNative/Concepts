unit BCCommon.Frame.Compare;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, BCCommon.Diff,
  Vcl.Grids, Vcl.StdCtrls, Vcl.ActnList, BCControl.Panel, BCControl.ComboBox, System.Actions, sComboBox,
  BCControl.SpeedButton, sFrameAdapter, Vcl.Dialogs, sDialogs, Vcl.Buttons, sSpeedButton, Vcl.ExtCtrls, sPanel,
  BCEditor.Editor.Base, BCEditor.Editor;

type
  TSyncKind = (skBoth, skVScroll, skHScroll);

  TCompareFrame = class(TFrame)
    ActionList: TActionList;
    CopyLeftSpeedButton: TBCSpeedButton;
    CopyRightSpeedButton: TBCSpeedButton;
    ActionCopySelectionLeft: TAction;
    ActionCopySelectionRight: TAction;
    DrawBarPanel: TBCPanel;
    ActionFindNextDifference: TAction;
    FindNextDifferenceSpeedButton: TBCSpeedButton;
    LeftComboBox: TBCComboBox;
    ActionLeftComboBoxChange: TAction;
    ActionLeftDocumentButtonClick: TAction;
    LeftPanel: TBCPanel;
    LeftTopPanel: TBCPanel;
    Panel: TBCPanel;
    ActionRefresh: TAction;
    RefreshSpeedButton: TBCSpeedButton;
    RightComboBox: TBCComboBox;
    ActionRightComboBoxChange: TAction;
    ActionRightDocumentButtonClick: TAction;
    RightPanel: TBCPanel;
    RightTopPanel: TBCPanel;
    ActionSaveLeftDocument: TAction;
    ActionSaveRightDocument: TAction;
    SaveSpeedButton1: TBCSpeedButton;
    SaveSpeedButton2: TBCSpeedButton;
    TopMiddlePanel: TBCPanel;
    FrameAdapter: TsFrameAdapter;
    OpenDialog: TsOpenDialog;
    DrawGrid: TDrawGrid;
    SpeedButtonDirectory: TBCSpeedButton;
    BCSpeedButton1: TBCSpeedButton;
    EditorLeft: TBCEditor;
    EditorRight: TBCEditor;
    BCSpeedButton2: TBCSpeedButton;
    BCSpeedButton3: TBCSpeedButton;
    procedure ActionCopySelectionLeftExecute(Sender: TObject);
    procedure ActionCopySelectionRightExecute(Sender: TObject);
    procedure DrawGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure FilenameEditLeftKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FilenameEditRightKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ActionFindNextDifferenceExecute(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure ActionLeftComboBoxChangeExecute(Sender: TObject);
    procedure ActionLeftDocumentButtonClickExecute(Sender: TObject);
    procedure LeftGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure LeftGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ActionRefreshExecute(Sender: TObject);
    procedure ActionRightComboBoxChangeExecute(Sender: TObject);
    procedure ActionRightDocumentButtonClickExecute(Sender: TObject);
    procedure RightGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure UpdateLeftRowActionExecute(Sender: TObject);
    procedure UpdateRightRowActionExecute(Sender: TObject);
  private
    FDiff: TDiff;
    FHashListLeft, FHashListRight: TList;
    FSpecialChars: Boolean;
    FLineNumbers: Boolean;
    function OpenDocument(AInitialDir: string; var AFileName: string): Boolean;
    procedure ClearLeftEditor;
    procedure ClearRightEditor;
    procedure OpenFileToLeftEditor(AFilename: string);
    procedure OpenFileToRightEditor(AFilename: string);

    function CheckIfFileExists(Filename: string): Boolean;
    function GetComparedFilesSet: Boolean;
    procedure BuildHashListLeft;
    procedure BuildHashListRight;

    procedure Compare;

    procedure SetMaxCounts;
    procedure SetOpenDocumentsList(Value: TStringList);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetCompareFile(Filename: string; AFileDragDrop: Boolean = False);

    function ToggleSpecialChars: Boolean;
    function ToggleLineNumbers: Boolean;
    procedure UpdateLanguage(SelectedLanguage: string);
    property ComparedFilesSet: Boolean read GetComparedFilesSet;
    property OpenDocumentsList: TStringList write SetOpenDocumentsList;
    property SpecialChars: Boolean write FSpecialChars;
    property LineNumbers: Boolean write FLineNumbers;
  end;

implementation

{$R *.dfm}

uses
  BCCommon.Hash, System.Math, System.Types, BCCommon.Language.Strings,
  BCCommon.Options.Container, BCCommon.Messages, BCCommon.Language.Utils;

{const
  TabChar = WideChar($2192);       //'->'
  LineBreakChar = WideChar($00B6); //'¶'
  SpaceChar = WideChar($2219);     //'·'    }

constructor TCompareFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDiff := TDiff.Create;

  FHashListLeft := TList.Create;
  FHashListRight := TList.Create;
end;

function TCompareFrame.ToggleSpecialChars: Boolean;
begin
  FSpecialChars := not FSpecialChars;
  Result := FSpecialChars;
end;

function TCompareFrame.ToggleLineNumbers: Boolean;
begin
  FLineNumbers := not FLineNumbers;
  Result := FLineNumbers;
end;

destructor TCompareFrame.Destroy;
begin
  FHashListLeft.Free;
  FHashListRight.Free;

  inherited Destroy;
end;

procedure TCompareFrame.DrawGridDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
const
  PaleRed: TColor = $9999FF;
  PaleBlue: TColor = $FF9999;
  PaleGray: TColor = $D0D0D0;
{var
  LeftRowColor, RightRowColor: TColor;
  RowInsideVisibleRows: Boolean;      }
begin
  {LStyles := StyleServices;
  LeftRowColor := clNone;
  RightRowColor := clNone;
 // RowInsideVisibleRows := (ARow >= LeftGrid.TopRow) and (ARow < LeftGrid.TopRow + LeftGrid.VisibleRowCount);
  if FDiff.Count = 0 then
 // with DrawGrid.Canvas do
  begin
    if LStyles.Enabled then
      Brush.Color := LStyles.GetStyleColor(scPanel)
    else
      Brush.Color := clWhite;
    FillRect(Rect);
  end
  else
  begin
    if (ARow < FDiff.Count) then
    case FDiff.Compares[ARow].Kind of
      ckNone:
        begin
          if LStyles.Enabled then
          begin
            LeftRowColor := LStyles.GetStyleColor(scPanel);
            RightRowColor := LStyles.GetStyleColor(scPanel);
          end
          else
          begin
            LeftRowColor := clWhite;
            RightRowColor := clWhite;
          end;

          if RowInsideVisibleRows then
          begin
            if LStyles.Enabled then
            begin
              LeftRowColor := LStyles.GetStyleColor(scEdit);
              RightRowColor := LStyles.GetStyleColor(scEdit);
            end
            else
            begin
              LeftRowColor := clSilver;
              RightRowColor := clSilver;
            end;
          end;
        end;
      ckModify:
        begin
          LeftRowColor := PaleRed;
          RightRowColor := PaleRed;
          if RowInsideVisibleRows then
          begin
            LeftRowColor := clRed;
            RightRowColor := clRed;
          end;
        end;
      ckDelete:
        begin
          if LStyles.Enabled then
            RightRowColor := LStyles.GetStyleColor(scEdit)
          else
            RightRowColor := PaleGray;
          LeftRowColor := PaleBlue;
          if RowInsideVisibleRows then
          begin
            LeftRowColor := clBlue;
            if LStyles.Enabled then
              RightRowColor := LStyles.GetStyleColor(scPanel)
            else
              RightRowColor := clBtnShadow;
          end;
        end;
      ckAdd:
        begin
          if LStyles.Enabled then
            LeftRowColor := LStyles.GetStyleColor(scEdit)
          else
            LeftRowColor := PaleGray;
          RightRowColor := PaleBlue;
          if RowInsideVisibleRows then
          begin
            if LStyles.Enabled then
              LeftRowColor := LStyles.GetStyleColor(scPanel)
            else
              LeftRowColor := clBtnShadow;
            RightRowColor := clBlue;
          end;
        end;
    end;

   // with DrawGrid.Canvas do
    begin
      if RowInsideVisibleRows then
      begin
        if LStyles.Enabled then
          Brush.Color := LStyles.GetStyleColor(scBorder)
        else
          Brush.Color := clBlack;
        FillRect(System.Types.Rect(0, Rect.top, 1, Rect.bottom));
        if LStyles.Enabled then
          Brush.Color := LStyles.GetStyleColor(scBorder)
        else
          Brush.Color := clBlack;
        FillRect(System.Types.Rect(21, Rect.top, 22, Rect.bottom));
      end;

      { Draw grids }
    {  Brush.Color := LeftRowColor;
      FillRect(System.Types.Rect(1, Rect.top, 11, Rect.bottom));
      Brush.Color := RightRowColor;
      FillRect(System.Types.Rect(11, Rect.top, 21, Rect.bottom));

     // if ARow = LeftGrid.Row then
      begin
        if LStyles.Enabled then
          Brush.Color := LStyles.GetSystemColor(clHighlight)
        else
          Brush.Color := clHighlight;

        FillRect(System.Types.Rect(1, Rect.top, 21, Rect.bottom));
      end;

      { Draw a rectangle around visible area }
      //if LeftGrid.TopRow + LeftGrid.VisibleRowCount = ARow then
      {begin
        if LStyles.Enabled then
          Brush.Color := LStyles.GetStyleColor(scBorder)
        else
          Brush.Color := clBlack;
        FillRect(System.Types.Rect(0, Rect.top, 22, Rect.bottom));
      end;
      //if LeftGrid.TopRow - 1 = ARow then
      begin
        if LStyles.Enabled then
          Brush.Color := LStyles.GetStyleColor(scBorder)
        else
          Brush.Color := clBlack;
        FillRect(System.Types.Rect(0, Rect.top, 22, Rect.bottom));
      end;
    end;
  end; }
end;

procedure TCompareFrame.FilenameEditLeftKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    OpenFileToLeftEditor(LeftComboBox.Text);
end;

procedure TCompareFrame.FilenameEditRightKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    OpenFileToRightEditor(RightComboBox.Text);
end;

procedure TCompareFrame.ActionLeftComboBoxChangeExecute(Sender: TObject);
begin
  OpenFileToLeftEditor(LeftComboBox.Text);
end;

function TCompareFrame.OpenDocument(AInitialDir: string; var AFileName: string): Boolean;
begin
  Result := False;
  AFileName := '';
  OpenDialog.InitialDir := AInitialDir;
  OpenDialog.Filter := Format('%s'#0'*.*'#0#0, [LanguageDataModule.GetConstant('AllFiles')]);
  OpenDialog.Title := LanguageDataModule.GetConstant('Open');
  if OpenDialog.Execute(Handle) then
  begin
    AFileName := OpenDialog.Files[0];
    Result := True;
  end;
end;

procedure TCompareFrame.ActionLeftDocumentButtonClickExecute(Sender: TObject);
var
  LFileName: string;
begin
  if OpenDocument(LeftComboBox.Text, LFileName) then
   begin
    LeftComboBox.Text := LFileName;
    OpenFileToLeftEditor(LFileName);
  end;
end;

procedure TCompareFrame.ActionRightDocumentButtonClickExecute(Sender: TObject);
var
  LFileName: string;
begin
  if OpenDocument(RightComboBox.Text, LFileName) then
   begin
    RightComboBox.Text := LFileName;
    OpenFileToRightEditor(LFileName);
  end;
end;

procedure TCompareFrame.LeftGridDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
const
  PaleRed: TColor = $6666FF;
  PaleBlue: TColor = $FF6666;
{var
  s: string;
  clr: TColor;
  LColor: TColor; }
begin
  (*LStyles := StyleServices;

  if FDiff.Count = 0 then
  begin
    if LStyles.Enabled then
      clr := LStyles.GetStyleColor(scEdit)
    else
      clr := clWhite
  end
  else
  begin
    if LStyles.Enabled then
      clr := LStyles.GetSystemColor(clBtnFace)
    else
      clr := clBtnFace;
  end;

  if (ACol = 1) and (ARow < FDiff.Count) then
    case FDiff.Compares[ARow].Kind of
      ckNone:
        if LStyles.Enabled then
          clr := LStyles.GetStyleColor(scEdit)
        else
          clr := clWhite;
      ckModify:
        if LStyles.Enabled then
          clr := LStyles.GetSystemColor(PaleRed)
        else
          clr := PaleRed;
      ckDelete:
        if LStyles.Enabled then
          clr := LStyles.GetSystemColor(PaleBlue)
        else
          clr := PaleBlue;
      ckAdd:
        if LStyles.Enabled then
          clr := LStyles.GetSystemColor(clBtnFace)
        else
          clr := clBtnFace;
    end;

  //with LeftGrid.Canvas do
  begin
    if not LStyles.GetElementColor(LStyles.GetElementDetails(tgCellNormal), ecTextColor, LColor) or  (LColor = clNone) then
      LColor := LStyles.GetSystemColor(clWindowText);
    //get and set the background color
    Brush.Color := LStyles.GetStyleColor(scListView);
    Font.Color := LColor;
    if (gdSelected in State) then
    begin
      if LStyles.Enabled then
      begin
        Brush.Color := LStyles.GetSystemColor(clHighlight);
        Font.Color := LStyles.GetSystemColor(clHighlightText);
      end
      else
      begin
        Brush.Color := clHighlight;
        Font.Color := clHighlightText;
      end;
    end
    else
      Brush.Color := clr;
    FillRect(Rect);
    //s := LeftGrid.Cells[ACol, ARow];
    if ACol = 1 then
      s := FormatText(s);
    if (ACol = 0) and not FLineNumbers then
      s := '';
    //TextRect(Rect, Rect.Left + 3, Rect.top + 2, s);

    if FSourceLeft.Count = 0 then
      Exit;

    if ACol = 0 then
    begin
      //if LStyles.Enabled then
      //  Pen.Color := LStyles.GetStyleColor(scEdit)
      //else
      //  Pen.Color := clWhite;
      //MoveTo(Rect.Right - 1, 0);
      LineTo(Rect.Right - 1, Rect.bottom);
    end
    else
    begin
      if (ACol = 1) then
      begin
       { if LStyles.Enabled then
          Pen.Color := LStyles.GetSystemColor($333333)
        else
          Pen.Color := $333333;
        MoveTo(Rect.Right - 1, 0);  }
        LineTo(Rect.Right - 1, Rect.bottom);
      end;
    {  if LStyles.Enabled then
        Pen.Color := LStyles.GetStyleColor(scPanel)
      else
        Pen.Color := clSilver;
      MoveTo(Rect.Left, 0); }
      LineTo(Rect.Left, Rect.bottom);
    end;
  end;  *)
end;

procedure TCompareFrame.LeftGridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  {if Key = VK_PRIOR then
    LeftGrid.Row := Max(LeftGrid.Row - LeftGrid.VisibleRowCount, 0);
  if Key = VK_NEXT then
    LeftGrid.Row := Min(LeftGrid.Row + LeftGrid.VisibleRowCount, LeftGrid.RowCount - 1);
  if (Key = VK_PRIOR) or (Key = VK_NEXT) then
    Key := 0;
 }
end;

procedure TCompareFrame.RightGridDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
const
  PaleRed: TColor = $6666FF;
  PaleBlue: TColor = $FF6666;
{var
  s: string;
  clr: TColor;
  LColor: TColor; }
begin
 { LStyles := StyleServices;

  if FDiff.Count = 0 then
  begin
    if LStyles.Enabled then
      clr := LStyles.GetStyleColor(scEdit)
    else
      clr := clWhite
  end
  else
  begin
    if LStyles.Enabled then
      clr := LStyles.GetSystemColor(clBtnFace)
    else
      clr := clBtnFace;
  end;

  if (ACol in [1, 3]) and (ARow < FDiff.Count) then
    case FDiff.Compares[ARow].Kind of
      ckNone:
        if LStyles.Enabled then
          clr := LStyles.GetStyleColor(scEdit)
        else
          clr := clWhite;
      ckModify:
        if LStyles.Enabled then
          clr := LStyles.GetSystemColor(PaleRed)
        else
          clr := PaleRed;
      ckDelete:
        if LStyles.Enabled then
          clr := LStyles.GetSystemColor(clBtnFace)
        else
          clr := clBtnFace;
      ckAdd:
        if LStyles.Enabled then
          clr := LStyles.GetSystemColor(PaleBlue)
        else
          clr := PaleBlue;
    end;

  //with RightGrid.Canvas do
  {begin
    if not LStyles.GetElementColor(LStyles.GetElementDetails(tgCellNormal), ecTextColor, LColor) or  (LColor = clNone) then
      LColor := LStyles.GetSystemColor(clWindowText);
    //get and set the background color
    Brush.Color := LStyles.GetStyleColor(scListView);
    Font.Color := LColor;
    if (gdSelected in State) then
    begin
      if LStyles.Enabled then
      begin
        Brush.Color := LStyles.GetSystemColor(clHighlight);
        Font.Color := LStyles.GetSystemColor(clHighlightText);
      end
      else
      begin
        Brush.Color := clHighlight;
        Font.Color := clHighlightText;
      end;
    end
    else
      Brush.Color := clr;
    FillRect(Rect);
    s := RightGrid.Cells[ACol, ARow];
    if ACol = 1 then
      s := FormatText(s);
    if (ACol = 0) and not FLineNumbers then
      s := '';
    TextRect(Rect, Rect.Left + 3, Rect.top + 2, s);

    if FSourceRight.Count = 0 then
      Exit;

    if ACol = 0 then
    begin
      if LStyles.Enabled then
        Pen.Color := LStyles.GetStyleColor(scEdit)
      else
        Pen.Color := clWhite;
      MoveTo(Rect.Right - 1, 0);
      LineTo(Rect.Right - 1, Rect.bottom);
    end
    else
    begin
      if (ACol = 1) then
      begin
        if LStyles.Enabled then
          Pen.Color := LStyles.GetSystemColor($333333)
        else
          Pen.Color := $333333;
        MoveTo(Rect.Right - 1, 0);
        LineTo(Rect.Right - 1, Rect.bottom);
      end;
      if LStyles.Enabled then
        Pen.Color := LStyles.GetStyleColor(scPanel)
      else
        Pen.Color := clSilver;
      MoveTo(Rect.Left, 0);
      LineTo(Rect.Left, Rect.bottom);
    end;
  end; }
end;

procedure TCompareFrame.ActionRefreshExecute(Sender: TObject);
begin
  OpenFileToLeftEditor(LeftComboBox.Text);
  OpenFileToRightEditor(RightComboBox.Text);
end;

function TCompareFrame.CheckIfFileExists(Filename: string): Boolean;
begin
  Result := FileExists(Filename);
  if not Result then
    ShowErrorMessage(Format(LanguageDataModule.GetErrorMessage('FileNotFound'), [Filename]))
end;

procedure TCompareFrame.OpenFileToLeftEditor(AFilename: string);
var
  FName: string;
begin
  ClearLeftEditor;

  FName := Trim(System.SysUtils.StringReplace(AFilename, '"', '', [rfReplaceAll]));
  if FName = '' then
    Exit;

  if not CheckIfFileExists(FName) then
    Exit;


  BuildHashListLeft;
  SetMaxCounts;
  //if LeftComboBox.Items.IndexOf(FName) = -1 then
  //  LeftComboBox.Items.Add(FName);
  Compare;
 { FindNextDifferenceAction.Enabled := ((FDiff.DiffStats.adds <> 0) or
    (FDiff.DiffStats.deletes <> 0) or (FDiff.DiffStats.modifies <> 0)) and
    (FSourceRight.Count <> 0) and (FSourceLeft.Count <> 0);
  RefreshAction.Enabled := (FSourceRight.Count <> 0) and (FSourceLeft.Count <> 0);
  CopySelectionRightAction.Enabled := (FSourceRight.Count <> 0) and (FSourceLeft.Count <> 0);
  CopySelectionLeftAction.Enabled := (FSourceRight.Count <> 0) and (FSourceLeft.Count <> 0);   }
  //AutosizeCol(LeftGrid);
  //if FSourceRight.Count <> 0 then
  //  AutosizeCol(RightGrid);

 // LeftMemo.Enabled := True;
  //LeftMemo.Text := LeftGrid.Cells[1, 0];

  //DrawGrid.Enabled := True;
  //DrawGrid.RowCount := Max(LeftGrid.RowCount, RightGrid.RowCount);
  SetMaxCounts;
  //DrawGrid.Invalidate;
end;

procedure TCompareFrame.OpenFileToRightEditor(AFilename: string);
var
  FName: string;
begin
  ClearRightEditor;

  FName :=  Trim(System.SysUtils.StringReplace(AFilename, '"', '', [rfReplaceAll]));
  if FName = '' then
    Exit;

  if not CheckIfFileExists(FName) then
    Exit;

  BuildHashListRight;
  SetMaxCounts;
  //if RightComboBox.Items.IndexOf(FName) = -1 then
  //  RightComboBox.Items.Add(FName);
  Compare;
  {FindNextDifferenceAction.Enabled := ((FDiff.DiffStats.adds <> 0) or
    (FDiff.DiffStats.deletes <> 0) or (FDiff.DiffStats.modifies <> 0)) and
    (FSourceRight.Count <> 0) and (FSourceLeft.Count <> 0);
  RefreshAction.Enabled := (FSourceRight.Count <> 0) and (FSourceLeft.Count <> 0);
  CopySelectionRightAction.Enabled := (FSourceRight.Count <> 0) and (FSourceLeft.Count <> 0);
  CopySelectionLeftAction.Enabled := (FSourceRight.Count <> 0) and (FSourceLeft.Count <> 0);   }
  //AutosizeCol(RightGrid);

  //if FSourceLeft.Count <> 0 then
  //  AutosizeCol(LeftGrid);

  //RightMemo.Enabled := True;
  //RightMemo.Text := RightGrid.Cells[1, 0];

  //DrawGrid.Enabled := True;
  //DrawGrid.RowCount := Max(LeftGrid.RowCount, RightGrid.RowCount);
  //DrawGrid.Invalidate;
end;

procedure TCompareFrame.ClearLeftEditor;
begin
  FDiff.Clear;
  FHashListLeft.Clear;
  EditorLeft.Clear;
  ActionSaveLeftDocument.Enabled := False;
  ActionCopySelectionRight.Enabled := False;
  ActionFindNextDifference.Enabled := False;
  ActionRefresh.Enabled := False;
end;

procedure TCompareFrame.ClearRightEditor;
begin
  FDiff.Clear;
  FHashListRight.Clear;
  EditorRight.Clear;
  ActionSaveRightDocument.Enabled := False;
  ActionCopySelectionLeft.Enabled := False;
  ActionFindNextDifference.Enabled := False;
  ActionRefresh.Enabled := False;
end;

procedure TCompareFrame.ActionRightComboBoxChangeExecute(Sender: TObject);
begin
  OpenFileToRightEditor(RightComboBox.Text);
end;

procedure TCompareFrame.BuildHashListLeft;
var
  i: Integer;
begin
  FHashListLeft.Clear;
  for i := 0 to EditorLeft.Lines.Count - 1 do
    FHashListLeft.Add(HashLine(EditorLeft.Lines[i], OptionsContainer.CompareIgnoreCase, OptionsContainer.CompareIgnoreBlanks));
end;

procedure TCompareFrame.BuildHashListRight;
var
  i: Integer;
begin
  FHashListRight.Clear;
  for i := 0 to EditorRight.Lines.Count - 1 do
    FHashListRight.Add(HashLine(EditorRight.Lines[i], OptionsContainer.CompareIgnoreCase, OptionsContainer.CompareIgnoreBlanks));
end;

procedure TCompareFrame.SetMaxCounts;
begin
  //LeftGrid.RowCount := Max(FSourceLeft.Count, FSourceRight.Count);
  //RightGrid.RowCount := LeftGrid.RowCount;
  //DrawGrid.RowCount := LeftGrid.RowCount;
end;

procedure TCompareFrame.UpdateLeftRowActionExecute(Sender: TObject);
//var
//  i: Integer;
begin
  //SaveLeftGridAction.Enabled := True;
  {LeftGrid.Cells[1, LeftGrid.Row] := LeftMemo.Text;
  if LeftGrid.Cells[0, LeftGrid.Row] = '' then
    LeftGrid.Cells[0, LeftGrid.Row] := '+';

  FSourceLeft.Clear;
  for i := 0 to LeftGrid.RowCount - 1 do
  begin
    if LeftGrid.Cells[0, i] <> '' then
      FSourceLeft.Add(LeftGrid.Cells[1, i]);
  end;
  BuildHashListLeft;
  Compare;
  DrawGrid.Invalidate;  }

 // UpdateLeftRowAction.Enabled := False;
 // CancelLeftRowAction.Enabled := False;
end;

procedure TCompareFrame.UpdateRightRowActionExecute(Sender: TObject);
//var
//  i: Integer;
begin
  //SaveRightGridAction.Enabled := True;
 { RightGrid.Cells[1, RightGrid.Row] := RightMemo.Text;
  if RightGrid.Cells[0, RightGrid.Row] = '' then
    RightGrid.Cells[0, RightGrid.Row] := '+';

  FSourceRight.Clear;
  for i := 0 to RightGrid.RowCount - 1 do
  begin
    if RightGrid.Cells[0, i] <> '' then
      FSourceRight.Add(RightGrid.Cells[1, i]);
  end;
  BuildHashListRight;
  Compare;
  DrawGrid.Invalidate;
  }
  //UpdateRightRowAction.Enabled := False;
  //CancelRightRowAction.Enabled := False;
end;

procedure TCompareFrame.ActionFindNextDifferenceExecute(Sender: TObject);
//var
//  Row: Integer;
begin
  {if (FDiff.DiffStats.adds = 0) and
     (FDiff.DiffStats.deletes = 0) and
     (FDiff.DiffStats.modifies = 0)then
    Exit;
  Row := Min(LeftGrid.Row + 1, LeftGrid.RowCount - 1);
  while (Row < LeftGrid.RowCount - 1) and (FDiff.Compares[Row].Kind = ckNone) do
    Inc(Row);
 }
end;

procedure TCompareFrame.FrameResize(Sender: TObject);
begin
  LeftPanel.Width := ((Width - DrawBarPanel.Width) div 2) - 2;
  RightPanel.Width := LeftPanel.Width;
  if Width mod 2 = 0 then
    RightPanel.Width := RightPanel.Width - 1;
  DrawGrid.Invalidate;
end;

procedure TCompareFrame.Compare;
//var
//  i: Integer;
begin
  (*if (FHashListLeft.Count = 0) or (FHashListRight.Count = 0) then
    Exit;
  FDiff.Clear;
  Screen.Cursor := crHourGlass;
  try
    //FDiff.Execute(PInteger(FHashListLeft.List), PInteger(FHashListRight.List),
    //  FHashListLeft.Count, FHashListRight.Count);
    FDiff.Execute(FHashListLeft.List, FHashListRight.List,
      FHashListLeft.Count, FHashListRight.Count);
    if FDiff.Cancelled then
      Exit;

    { fill ResultGrid with the differences }
    for i := 0 to 1 do
    begin
      LeftGrid.Cols[i].BeginUpdate;
      LeftGrid.Cols[i].Clear;
      RightGrid.Cols[i].BeginUpdate;
      RightGrid.Cols[i].Clear;
    end;
    try
      LeftGrid.RowCount := FDiff.Count;
      RightGrid.RowCount := FDiff.Count;
      for i := 0 to FDiff.Count - 1 do
        with FDiff.Compares[i] do
        begin
          if Kind <> ckAdd then
          begin
            LeftGrid.Cells[0, i] := IntToStr(oldIndex1 + 1);
            LeftGrid.Cells[1, i] := FSourceLeft[oldIndex1];
          end;
          if Kind <> ckDelete then
          begin
            RightGrid.Cells[0, i] := IntToStr(oldIndex2 + 1);
            RightGrid.Cells[1, i] := FSourceRight[oldIndex2];
          end;
        end;
    finally
      for i := 0 to 1 do
      begin
        LeftGrid.Cols[i].EndUpdate;
        RightGrid.Cols[i].EndUpdate;
      end;
    end;
  finally
    Screen.Cursor := crDefault;
  end; *)
end;

procedure TCompareFrame.ActionCopySelectionLeftExecute(Sender: TObject);
//var
//  i: Integer;
begin
  {SaveLeftGridAction.Enabled := True;
  for i := RightGrid.Selection.bottom downto RightGrid.Selection.top do
  begin
    case FDiff.Compares[i].Kind of
      ckNone, ckModify:
        LeftGrid.Cells[1, i] := RightGrid.Cells[1, i];
      ckAdd:
      begin
        LeftGrid.Cells[0, i] := '+';
        LeftGrid.Cells[1, i] := RightGrid.Cells[1, i];
      end;
      ckDelete:
        LeftGrid.RemoveRow(i);
    end;
  end;
  FSourceLeft.Clear;
  for i := 0 to LeftGrid.RowCount - 1 do
  begin
    if LeftGrid.Cells[0, i] <> '' then
      FSourceLeft.Add(LeftGrid.Cells[1, i]);
  end;
  BuildHashListLeft;
  SetMaxCounts;
  Compare;
  AutosizeCol(LeftGrid);
  LeftMemo.Text := LeftGrid.Cells[1, LeftGrid.Row];
  DrawGrid.Invalidate;}
end;

procedure TCompareFrame.ActionCopySelectionRightExecute(Sender: TObject);
//var
//  i: Integer;
begin
  {SaveRightGridAction.Enabled := True;
  for i := LeftGrid.Selection.Bottom downto LeftGrid.Selection.Top do
  begin
    case FDiff.Compares[i].Kind of
      ckNone, ckModify:
        RightGrid.Cells[1, i] := LeftGrid.Cells[1, i];
      ckDelete:
      begin
        RightGrid.Cells[0, i] := '+';
        RightGrid.Cells[1, i] := LeftGrid.Cells[1, i];
      end;
      ckAdd:
        RightGrid.RemoveRow(i);
    end;
  end;
  FSourceRight.Clear;
  for i := 0 to RightGrid.RowCount - 1 do
  begin
    if RightGrid.Cells[0, i] <> '' then
      FSourceRight.Add(RightGrid.Cells[1, i]);
  end;
  BuildHashListRight;
  SetMaxCounts;
  Compare;
  AutosizeCol(RightGrid);
  RightMemo.Text := RightGrid.Cells[1, RightGrid.Row];
  DrawGrid.Invalidate; }
end;

procedure TCompareFrame.SetOpenDocumentsList(Value: TStringList);
begin
{  Value.Sort;
  LeftComboBox.Items := Value;
  RightComboBox.Items := Value;  }
end;

function TCompareFrame.GetComparedFilesSet: Boolean;
begin
  Result := (LeftComboBox.Text <> '') and (RightComboBox.Text <> '');
end;

procedure TCompareFrame.SetCompareFile(Filename: string; AFileDragDrop: Boolean);
begin
  if (not AFileDragDrop and (EditorLeft.Text = '')) or
    (AFileDragDrop and EditorLeft.MouseInClient) then
  begin
    LeftComboBox.Text := Filename;
    OpenFileToLeftEditor(Filename);
  end
  else
  if (not AFileDragDrop and (EditorRight.Text = '')) or
    (AFileDragDrop and EditorRight.MouseInClient) then
  begin
    RightComboBox.Text := Filename;
    OpenFileToRightEditor(Filename);;
  end;
end;

procedure TCompareFrame.UpdateLanguage(SelectedLanguage: string);
begin
  BCCommon.Language.Utils.UpdateLanguage(TForm(Self), SelectedLanguage);
end;

end.
