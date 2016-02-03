unit BCCommon.Form.SearchForFiles;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  Vcl.ActnList, Vcl.ImgList, BCControl.ImageList, VirtualTrees, BCControl.ProgressBar,
  Vcl.ComCtrls, BCControl.ButtonedEdit, System.Win.TaskbarCore, Vcl.Taskbar, BCControl.Panel,
  sSkinProvider, BCControl.Statusbar, System.Diagnostics, System.UITypes, System.Actions, sStatusBar, Vcl.StdCtrls,
  sPanel, System.ImageList;

type
  TOpenFileEvent = procedure(var AFileName: string);

  TSearchForFilesForm = class(TForm)
    ActionClear: TAction;
    ActionList: TActionList;
    ActionSearch: TAction;
    EditSearchFor: TBCButtonedEdit;
    PanelSearchingFiles: TBCPanel;
    SkinProvider: TsSkinProvider;
    StatusBar: TBCStatusBar;
    Taskbar: TTaskbar;
    VirtualDrawTreeSearch: TVirtualDrawTree;
    ImageList: TImageList;
    procedure ActionClearExecute(Sender: TObject);
    procedure ActionSearchExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TaskBarHide(Sender: TObject);
    procedure TaskBarShow(Sender: TObject);
    procedure TaskBarStepChange(Sender: TObject);
    procedure VirtualDrawTreeSearchCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VirtualDrawTreeSearchDblClick(Sender: TObject);
    procedure VirtualDrawTreeSearchDrawNode(Sender: TBaseVirtualTree; const PaintInfo: TVTPaintInfo);
    procedure VirtualDrawTreeSearchFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VirtualDrawTreeSearchGetNodeWidth(Sender: TBaseVirtualTree; HintCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; var NodeWidth: Integer);
    procedure VirtualDrawTreeSearchGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
  private
    FFormClosing: Boolean;
    FOpenFile: TOpenFileEvent;
    FProgressBar: TBCProgressBar;
    FStopWatch: TStopWatch;
    procedure CreateProgressBar;
    procedure ReadFiles(RootDirectory: string);
    procedure ReadIniFile;
    procedure ResizeProgressBar;
    procedure SetVisibleRows;
    procedure WriteIniFile;
  public
    procedure Open(RootDirectory: string);
    property OnOpenFile: TOpenFileEvent read FOpenFile write FOpenFile;
  end;

function SearchForFilesForm(AOwner: TForm): TSearchForFilesForm;

implementation

{$R *.dfm}

uses
  Winapi.CommCtrl, BCCommon.Language.Utils, System.IniFiles, BCCommon.FileUtils,
  BCCommon.Language.Strings, System.Types, BCCommon.Utils, BCControl.Utils, sGraphUtils, sVCLUtils, sDefaults;

type
  PSearchRec = ^TSearchRec;
  TSearchRec = packed record
    FileName: string;
    FilePath: string;
    ImageIndex: Integer;
  end;

var
  FSearchForFilesForm: TSearchForFilesForm;

function SearchForFilesForm(AOwner: TForm): TSearchForFilesForm;
begin
  if not Assigned(FSearchForFilesForm) then
    FSearchForFilesForm := TSearchForFilesForm.Create(AOwner);
  Result := FSearchForFilesForm;
  UpdateLanguage(FSearchForFilesForm, GetSelectedLanguage);
end;

procedure TSearchForFilesForm.ResizeProgressBar;
var
  R: TRect;
begin
  if Assigned(FProgressBar) then
  begin
    Statusbar.Perform(SB_GETRECT, 0, Integer(@R));
    FProgressBar.Top    := R.Top + 3;
    FProgressBar.Left   := R.Left;
    FProgressBar.Width  := R.Right - R.Left - 3;
    FProgressBar.Height := R.Bottom - R.Top - 3;
  end;
end;

procedure TSearchForFilesForm.TaskBarStepChange(Sender: TObject);
begin
  Taskbar.ProgressValue := FProgressBar.Progress;
end;

procedure TSearchForFilesForm.TaskBarShow(Sender: TObject);
begin
  Taskbar.ProgressState := TTaskBarProgressState.Normal;
end;

procedure TSearchForFilesForm.TaskBarHide(Sender: TObject);
begin
  if Assigned(Taskbar) then
    Taskbar.ProgressState := TTaskBarProgressState.None;
end;

procedure TSearchForFilesForm.CreateProgressBar;
begin
  FProgressBar := TBCProgressBar.Create(StatusBar);
  FProgressBar.OnStepChange := TaskBarStepChange;
  FProgressBar.OnShow := TaskBarShow;
  FProgressBar.OnHide := TaskBarHide;
  FProgressBar.Hide;
  ResizeProgressBar;
  FProgressBar.Parent := Statusbar;
end;

procedure TSearchForFilesForm.SetVisibleRows;
var
  CurNode: PVirtualNode;
  Data: PSearchRec;
begin
  with VirtualDrawTreeSearch do
  begin
    CurNode := GetFirst;
    while Assigned(CurNode) do
    begin
      Data := GetNodeData(CurNode);
      IsVisible[CurNode] := (Pos(UpperCase(EditSearchFor.Text), UpperCase(Data.FileName)) <> 0) or (EditSearchFor.Text = '');
      CurNode := CurNode.NextSibling;
    end;
  end;
end;

procedure TSearchForFilesForm.ActionClearExecute(Sender: TObject);
begin
  EditSearchFor.Text := '';
end;

procedure TSearchForFilesForm.ActionSearchExecute(Sender: TObject);
begin
  EditSearchFor.RightButton.Visible := Trim(EditSearchFor.Text) <> '';
  SetVisibleRows;
end;

procedure TSearchForFilesForm.VirtualDrawTreeSearchCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
  Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PSearchRec;
begin
  if Result = 0 then
  begin
    Data1 := VirtualDrawTreeSearch.GetNodeData(Node1);
    Data2 := VirtualDrawTreeSearch.GetNodeData(Node2);

    Result := -1;

    if not Assigned(Data1) or not Assigned(Data2) then
      Exit;

    Result := AnsiCompareText(Data1.FileName, Data2.FileName);
  end;
end;

procedure TSearchForFilesForm.VirtualDrawTreeSearchDblClick(Sender: TObject);
var
  S: string;
  Node: PVirtualNode;
  Data: PSearchRec;
begin
  Node := VirtualDrawTreeSearch.GetFirstSelected;
  Data := VirtualDrawTreeSearch.GetNodeData(Node);
  if Assigned(Data) then
    if Assigned(FOpenFile) then
    begin
      {$WARNINGS OFF} { IncludeTrailingBackslash is specific to a platform }
      S := IncludeTrailingBackslash(Data.FilePath) + Data.FileName;
      {$WARNINGS ON}
      FOpenFile(S);
    end;
end;

procedure TSearchForFilesForm.VirtualDrawTreeSearchDrawNode(Sender: TBaseVirtualTree; const PaintInfo: TVTPaintInfo);
var
  Data: PSearchRec;
  S: string;
  R: TRect;
  Format: Cardinal;
begin
  with Sender as TVirtualDrawTree, PaintInfo do
  begin
    Data := Sender.GetNodeData(Node);

    if not Assigned(Data) then
      Exit;

    Canvas.Font.Color := SkinProvider.SkinData.SkinManager.GetActiveEditFontColor;

    if vsSelected in PaintInfo.Node.States then
    begin
      Canvas.Brush.Color := SkinProvider.SkinData.SkinManager.GetHighLightColor;
      Canvas.Font.Color := SkinProvider.SkinData.SkinManager.GetHighLightFontColor
    end;

    SetBKMode(Canvas.Handle, TRANSPARENT);

    R := ContentRect;
    InflateRect(R, -TextMargin, 0);
    Dec(R.Right);
    Dec(R.Bottom);

    S := Data.Filename;

    if Length(S) > 0 then
    begin
      Format := DT_TOP or DT_LEFT or DT_VCENTER or DT_SINGLELINE;

      DrawText(Canvas.Handle, S, Length(S), R, Format);
      R.Left := R.Left + Canvas.TextWidth(S);
      S := System.SysUtils.Format(' (%s)', [Data.FilePath]);
      Canvas.Font.Color := MixColors(ColorToRGB(Font.Color), GetControlColor(Parent), DefDisabledBlend);

      DrawText(Canvas.Handle, S, Length(S), R, Format);
    end;
  end;
end;

procedure TSearchForFilesForm.VirtualDrawTreeSearchFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PSearchRec;
begin
  Data := Sender.GetNodeData(Node);
  Finalize(Data^);
  inherited;
end;

procedure TSearchForFilesForm.VirtualDrawTreeSearchGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data: PSearchRec;
begin
  Data := VirtualDrawTreeSearch.GetNodeData(Node);
  if Assigned(Data) then
    ImageIndex := Data.ImageIndex;
end;

procedure TSearchForFilesForm.VirtualDrawTreeSearchGetNodeWidth(Sender: TBaseVirtualTree; HintCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; var NodeWidth: Integer);
var
  Data: PSearchRec;
  AMargin: Integer;
begin
  with Sender as TVirtualDrawTree do
  begin
    AMargin := TextMargin;
    Data := GetNodeData(Node);
    if Assigned(Data) then
      NodeWidth := Canvas.TextWidth(Format('%s (%s)', [Data.FileName, Data.FilePath])) + 2 * AMargin;
  end;
end;

procedure TSearchForFilesForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WriteIniFile;
  FFormClosing := True;
  Application.ProcessMessages;
  TForm(Owner).SetFocus;
  Action := caFree;
end;

procedure TSearchForFilesForm.FormCreate(Sender: TObject);
var
  SysImageList: THandle;
begin
  FFormClosing := False;
  VirtualDrawTreeSearch.NodeDataSize := SizeOf(TSearchRec);
  VirtualDrawTreeSearch.Images := TImageList.Create(Self);
  SysImageList := GetSysImageList;
  if SysImageList <> 0 then
  begin
    VirtualDrawTreeSearch.Images.Handle := SysImageList;
    VirtualDrawTreeSearch.Images.BkColor := clNone;
    VirtualDrawTreeSearch.Images.ShareImages := True;
  end;
end;

procedure TSearchForFilesForm.FormDestroy(Sender: TObject);
begin
  Taskbar.ProgressState := TTaskBarProgressState.None;
  FProgressBar.Free;
  VirtualDrawTreeSearch.Images.Free;
  FSearchForFilesForm := nil;
  inherited;
end;

procedure TSearchForFilesForm.FormResize(Sender: TObject);
begin
  ResizeProgressBar;
end;

procedure TSearchForFilesForm.FormShow(Sender: TObject);
begin
  CreateProgressBar;
end;

procedure TSearchForFilesForm.Open(RootDirectory: string);
var
  LTimeDifference: string;
  LCount: Integer;
begin
  if RootDirectory = '' then
    Exit;
  Caption := Format('%s - [%s]', [Caption, RootDirectory]);
  PanelSearchingFiles.Visible := False;
  ReadIniFile;
  Show;
  Screen.Cursor := crHourGlass;
  try
    StatusBar.Panels[0].Text := LanguageDataModule.GetConstant('CountingFiles');
    Application.ProcessMessages;
    LCount := CountFilesInFolder(RootDirectory, '*.*', '*.*');
  finally
    Screen.Cursor := crDefault;
  end;
  FProgressBar.Show(LCount);
  FStopWatch.Reset;
  FStopWatch.Start;
  try
    ReadFiles(RootDirectory);
  finally
    FProgressBar.Hide;
    if not FFormClosing then
    begin
      FStopWatch.Stop;
      if StrToInt(FormatDateTime('n', FStopWatch.ElapsedMilliseconds / MSecsPerDay)) < 1 then
        LTimeDifference := FormatDateTime(Format('s.zzz "%s"', [LanguageDataModule.GetConstant('Second')]), FStopWatch.ElapsedMilliseconds / MSecsPerDay)
      else
        LTimeDifference := FormatDateTime(Format('n "%s" s.zzz "%s"', [LanguageDataModule.GetConstant('Minute'), LanguageDataModule.GetConstant('Second')]), FStopWatch.ElapsedMilliseconds / MSecsPerDay);
      StatusBar.Panels[0].Text := Format(LanguageDataModule.GetConstant('FilesFound'), [LCount, LTimeDifference]);

      PanelSearchingFiles.Visible := False;
    end;
  end;
end;

procedure TSearchForFilesForm.ReadIniFile;
begin
  with TMemIniFile.Create(GetIniFilename) do
  try
    { Size }
    Width := ReadInteger('SearchForFilesSize', 'Width', Width);
    Height := ReadInteger('SearchForFilesSize', 'Height', Height);
    { Position }
    Left := ReadInteger('SearchForFilesPosition', 'Left', (Screen.Width - Width) div 2);
    Top := ReadInteger('SearchForFilesPosition', 'Top', (Screen.Height - Height) div 2);
    { Check if the form is outside the workarea }
    Left := SetFormInsideWorkArea(Left, Width);
  finally
    Free;
  end;
end;

procedure TSearchForFilesForm.WriteIniFile;
begin
  if Windowstate = wsNormal then
  with TMemIniFile.Create(GetIniFilename) do
  try
    { Size }
    WriteInteger('SearchForFilesSize', 'Width', Width);
    WriteInteger('SearchForFilesSize', 'Height', Height);
    { Position }
    WriteInteger('SearchForFilesPosition', 'Left', Left);
    WriteInteger('SearchForFilesPosition', 'Top', Top);
  finally
    UpdateFile;
    Free;
  end;
end;

procedure TSearchForFilesForm.ReadFiles(RootDirectory: string);
var
  Node: PVirtualNode;
  NodeData: PSearchRec;
  shFindFile: THandle;
  sWin32FD: TWin32FindData;
  FName: string;

  function IsDirectory(dWin32FD: TWin32FindData): Boolean;
  var
    TmpAttr: DWORD;
  begin
    with dWin32FD do
    begin
      TmpAttr := dwFileAttributes and (FILE_ATTRIBUTE_READONLY or FILE_ATTRIBUTE_HIDDEN or FILE_ATTRIBUTE_SYSTEM or FILE_ATTRIBUTE_ARCHIVE or FILE_ATTRIBUTE_NORMAL or FILE_ATTRIBUTE_DIRECTORY);

      Result := (TmpAttr and FILE_ATTRIBUTE_DIRECTORY = FILE_ATTRIBUTE_DIRECTORY);
    end;
  end;
begin
  if FFormClosing then
    Exit;
  {$WARNINGS OFF} { IncludeTrailingBackslash is specific to a platform }
  shFindFile := FindFirstFile(PChar(IncludeTrailingBackslash(RootDirectory) + '*.*'), sWin32FD);
  {$WARNINGS ON}
  if shFindFile <> INVALID_HANDLE_VALUE then
  try
    repeat
      if FFormClosing then
        Exit;
      FName := StrPas(sWin32FD.cFileName);
      if (FName <> '.') and (FName <> '..') then
      begin
        if IsDirectory(sWin32FD) then
          {$WARNINGS OFF} { IncludeTrailingBackslash is specific to a platform }
          ReadFiles(IncludeTrailingBackslash(RootDirectory) + FName)
          {$WARNINGS ON}
        else
        begin
          FProgressBar.StepIt;
          Application.ProcessMessages;
          if not FFormClosing then
          begin
            Node := VirtualDrawTreeSearch.AddChild(nil);
            NodeData := VirtualDrawTreeSearch.GetNodeData(Node);
            NodeData.FileName := FName;
            {$WARNINGS OFF} { ExcludeTrailingBackslash is specific to a platform }
            NodeData.FilePath := ExcludeTrailingBackslash(RootDirectory);
            NodeData.ImageIndex := GetIconIndex(IncludeTrailingBackslash(RootDirectory) + FName);
            {$WARNINGS ON}
            VirtualDrawTreeSearch.IsVisible[Node] := (Pos(UpperCase(EditSearchFor.Text), UpperCase(NodeData.FileName)) <> 0) or (EditSearchFor.Text = '');
          end;
          VirtualDrawTreeSearch.Sort(nil, 0, sdAscending, False);
          VirtualDrawTreeSearch.Invalidate;
          Application.ProcessMessages;
        end;
      end;
    until not FindNextFile(shFindFile, sWin32FD) and not FFormClosing;
  finally
    Winapi.Windows.FindClose(shFindFile);
  end;
end;

end.
