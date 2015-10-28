unit MainView;

interface

{$IFDEF MSWINDOWS}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
{$ENDIF}

uses
  Windows, Messages, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, fs_synmemo, ExtCtrls, VirtualTrees, ToolWin, ComCtrls, ImgList, StdCtrls
  ,MainRunner, ActnList, Menus, uVSTSearchEdit
  ;

type
  TViewMain = class(TForm)
    pLeft: TPanel;
    pRight: TPanel;
    sbBot: TStatusBar;
    vtTables: TVirtualStringTree;
    spl1: TSplitter;
    fsSyntaxMemo1: TfsSyntaxMemo;
    pProperties: TPanel;
    edConString: TButtonedEdit;
    ilMain: TImageList;
    edDbName: TEdit;
    edSchemaName: TEdit;
    btnExec: TButton;
    btnConnect: TButton;
    edOutputDir: TButtonedEdit;
    pmTree: TPopupMenu;
    alMain: TActionList;
    aShowPreview: TAction;
    ShowPreview1: TMenuItem;
    aCheckAll: TAction;
    aUncheckAll: TAction;
    CheckAll1: TMenuItem;
    UncheckAll1: TMenuItem;
    aGenerateUnits: TAction;
    edUnitPrefix: TEdit;
    cbUseNullableTypes: TCheckBox;
    edVstSearch: TButtonedEdit;
    procedure edConStringRightButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure vtTablesInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure vtTablesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    procedure edOutputDirRightButtonClick(Sender: TObject);
    procedure aShowPreviewUpdate(Sender: TObject);
    procedure aShowPreviewExecute(Sender: TObject);
    procedure aCheckAllExecute(Sender: TObject);
    procedure aUncheckAllExecute(Sender: TObject);
    procedure aGenerateUnitsUpdate(Sender: TObject);
    procedure aGenerateUnitsExecute(Sender: TObject);
    procedure edOutputDirLeftButtonClick(Sender: TObject);
    procedure edVstSearchRightButtonClick(Sender: TObject);
    procedure vtTablesDblClick(Sender: TObject);
  private
    { Private declarations }
    FRunner: TMainRunner;
    FVSTSearch: TVSTSearchEdit;
  protected
    procedure SetupRunner();
    procedure LoadFromRunner();
    procedure SaveToFile(AIndex: Integer; const AUnitText: string);
    procedure ShowPreview(AIndex: Integer);
    procedure ShowStatus(const AMessage: string);

    function MySelectDirectory(var ADirectory: string): Boolean;
    procedure DoCheckNodes(AChecked: Boolean);
    procedure DoGenerateUnits();
  public
    { Public declarations }
  end;

var
  ViewMain: TViewMain;

implementation

uses
  ADODB
  ,FileCtrl
  ,SysUtils
  ,SvThreading
  ,ActiveX
  ,ShellAPI
  ;

{$R *.dfm}

procedure TViewMain.aCheckAllExecute(Sender: TObject);
begin
  DoCheckNodes(True);
end;

procedure TViewMain.aGenerateUnitsExecute(Sender: TObject);
begin
  ShowStatus('Generating units...');
  btnExec.Enabled := False;
  TSvParallel.Async(
    procedure
    begin
      CoInitialize(nil);
      try
        DoGenerateUnits();
      finally
        CoUninitialize;
      end;
    end,
    procedure
    begin
      btnExec.Enabled := True;
      ShowStatus('Units generated successfully.');
    end);
end;

procedure TViewMain.aGenerateUnitsUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := vtTables.CheckedCount > 0;
end;

procedure TViewMain.aShowPreviewExecute(Sender: TObject);
begin
  ShowPreview(vtTables.FocusedNode.Index);
end;

procedure TViewMain.aShowPreviewUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := vtTables.FocusedNode <> nil;
end;

procedure TViewMain.aUncheckAllExecute(Sender: TObject);
begin
  DoCheckNodes(False);
end;

procedure TViewMain.btnConnectClick(Sender: TObject);
begin
  SetupRunner();
  ShowStatus('Loading tables from database...');
  btnConnect.Enabled := False;
  TSvParallel.Async(
    procedure
    begin
      CoInitialize(nil);
      try
        FRunner.DBLoader.LoadTables;
      finally
        CoUninitialize;
      end;
    end,
    procedure
    begin
      vtTables.RootNodeCount := FRunner.DBLoader.Entities.Count;
      vtTables.EmptyListMessage := '';
      btnConnect.Enabled := True;
      ShowStatus('Tables loaded successfully');
    end);
end;

procedure TViewMain.DoCheckNodes(AChecked: Boolean);
var
  LNode: PVirtualNode;
begin
  for LNode in vtTables.Nodes do
  begin
    if AChecked then
      vtTables.CheckState[LNode] := csCheckedNormal
    else
      vtTables.CheckState[LNode] := csUncheckedNormal;
  end;
end;

procedure TViewMain.DoGenerateUnits;
var
  LNode: PVirtualNode;
  LGeneratedString: string;
begin
  SetupRunner();
  for LNode in vtTables.CheckedNodes do
  begin
    LGeneratedString := FRunner.Execute(LNode.Index);
    SaveToFile(LNode.Index, LGeneratedString);
  end;
end;

procedure TViewMain.edConStringRightButtonClick(Sender: TObject);
begin
  TButtonedEdit(Sender).Text := PromptDataSource(Self.Handle, TButtonedEdit(Sender).Text);
end;

procedure TViewMain.edOutputDirLeftButtonClick(Sender: TObject);
var
  LDir: string;
begin
  LDir := edOutputDir.Text;
  if DirectoryExists(LDir) then
  begin
    ShellExecute(Handle, nil, PChar(LDir), nil, PChar(LDir), SW_SHOWNORMAL);
  end;
end;

procedure TViewMain.edOutputDirRightButtonClick(Sender: TObject);
var
  LDirectory: string;
begin
  LDirectory := edOutputDir.Text;
  if MySelectDirectory(LDirectory) then
    edOutputDir.Text := LDirectory;
end;

procedure TViewMain.edVstSearchRightButtonClick(Sender: TObject);
begin
  edVstSearch.Clear;
end;

procedure TViewMain.FormCreate(Sender: TObject);
begin
  DesktopFont := True;

  FVSTSearch := TVSTSearchEdit.Create(Self);
  FVSTSearch.VST := vtTables;
  FVSTSearch.Edit := edVstSearch;

  FRunner := TMainRunner.Create;
  LoadFromRunner();
end;

procedure TViewMain.FormDestroy(Sender: TObject);
begin
  SetupRunner();
  FRunner.Free;
  FVSTSearch.Free;
end;

procedure TViewMain.LoadFromRunner;
begin
  edDbName.Text := FRunner.DBLoader.DatabaseName;
  edSchemaName.Text := FRunner.DBLoader.DefaultSchemaName;
  edConString.Text := FRunner.DBLoader.ConnectionString;
  edOutputDir.Text := FRunner.DBLoader.OutputDir;
  edUnitPrefix.Text := FRunner.DBLoader.UnitPrefix;
  cbUseNullableTypes.Checked := FRunner.DBLoader.UseNullableTypes;
end;

procedure TViewMain.SetupRunner;
begin
  FRunner.DBLoader.DatabaseName := edDbName.Text;
  FRunner.DBLoader.DefaultSchemaName := edSchemaName.Text;
  FRunner.DBLoader.ConnectionString := edConString.Text;
  FRunner.DBLoader.OutputDir := edOutputDir.Text;
  FRunner.DBLoader.UnitPrefix := edUnitPrefix.Text;
  FRunner.DBLoader.UseNullableTypes := cbUseNullableTypes.Checked;
end;

function TViewMain.MySelectDirectory(var ADirectory: string): Boolean;
begin
  if Win32MajorVersion >= 6 then
  begin
    with TFileOpenDialog.Create(nil) do
    try
      Title := 'Select Directory';
      Options := [fdoPickFolders, fdoPathMustExist, fdoForceFileSystem];
      OkButtonLabel := 'Select';
      DefaultFolder := ADirectory;
      FileName := ADirectory;
      if Execute then
        ADirectory := FileName;
    finally
      Free;
    end
  end
  else
    SelectDirectory('Select Directory', ExtractFileDrive(ADirectory), ADirectory, 
      [sdNewUI, sdNewFolder]);

  Result := ADirectory <> '';
end;

procedure TViewMain.SaveToFile(AIndex: Integer; const AUnitText: string);
var
  LFile: TStringStream;
  LFilename: string;
begin
  if AUnitText = '' then
    Exit;

  if not ForceDirectories(FRunner.DBLoader.OutputDir) then
    Exit;

  LFile := TStringStream.Create(AUnitText, TEncoding.UTF8);
  try
    LFilename :=  IncludeTrailingPathDelimiter(FRunner.DBLoader.OutputDir) +
      FRunner.DBLoader.GetUnitName(FRunner.DBLoader.Entities[AIndex].TableName) + '.pas';

    LFile.SaveToFile(LFilename);
  finally
    LFile.Free;
  end;
end;

procedure TViewMain.ShowPreview(AIndex: Integer);
var
  LNode: PVirtualNode;
begin
  LNode := vtTables.FocusedNode;
  if not Assigned(LNode) then
    Exit;

  SetupRunner();
  fsSyntaxMemo1.Lines.Text := FRunner.Execute(LNode.Index);
end;

procedure TViewMain.ShowStatus(const AMessage: string);
begin
  sbBot.SimpleText := AMessage;
end;

procedure TViewMain.vtTablesDblClick(Sender: TObject);
var
  LNode: PVirtualNode;
begin
  LNode := vtTables.FocusedNode;
  if Assigned(LNode) then
  begin
    ShowPreview(LNode.Index);
  end;
end;

procedure TViewMain.vtTablesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
begin
  CellText := FRunner.DBLoader.Entities[Node.Index].ToString;
end;

procedure TViewMain.vtTablesInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
begin
  Node.CheckType := ctCheckBox;
end;

end.
