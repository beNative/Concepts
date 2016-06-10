unit BCEditor.Editor.DB;

interface

uses
  Winapi.Messages, System.SysUtils, System.Classes, Vcl.Controls, Vcl.DbCtrls, Data.DB, BCEditor.Editor,
  BCEditor.Editor.KeyCommands;

type
  TBCCustomDBEditor = class(TBCCustomEditor)
  strict private
    FBeginEdit: Boolean;
    FDataLink: TFieldDataLink;
    FEditing: Boolean;
    FLoadData: TNotifyEvent;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure CMEnter(var AMessage: TCMEnter); message CM_ENTER;
    procedure CMExit(var AMessage: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var AMessage: TMessage); message CM_GETDATALINK;
    procedure DataChange(ASender: TObject);
    procedure EditingChange(ASender: TObject);
    procedure SetDataField(const AValue: string);
    procedure SetDataSource(AValue: TDataSource);
    procedure SetEditing(AValue: Boolean);
    procedure UpdateData(ASender: TObject);
  protected
    function GetReadOnly: Boolean; override;
    procedure DoChange; override;
    procedure Loaded; override;
    procedure SetReadOnly(AValue: Boolean); override;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Field: TField read GetField;
    property OnLoadData: TNotifyEvent read FLoadData write FLoadData;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DragDrop(ASource: TObject; X, Y: Integer); override;
    procedure ExecuteCommand(ACommand: TBCEditorCommand; AChar: Char; AData: pointer); override;
    procedure LoadMemo;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  end;

  TBCDBEditor = class(TBCCustomDBEditor)
  published
    property ActiveLine;
    property Align;
    property Anchors;
    property BorderStyle;
    property Caret;
    property CodeFolding;
    property CompletionProposal;
    property Constraints;
    property Ctl3D;
    property DataField;
    property DataSource;
    property Directories;
    property Enabled;
    property Field;
    property Font;
    property Height;
    property ImeMode;
    property ImeName;
    property InsertMode;
    property KeyCommands;
    property LeftMargin;
    property LineSpacing;
    property MatchingPair;
    property Minimap;
    property Name;
    property OnAfterBookmarkPanelPaint;
    property OnAfterBookmarkPlaced;
    property OnAfterClearBookmark;
    property OnAfterLinePaint;
    property OnBeforeBookmarkPanelPaint;
    property OnBeforeBookmarkPlaced;
    property OnBeforeClearBookmark;
    property OnBeforeCompletionProposalExecute;
    property OnBookmarkPanelLinePaint;
    property OnCaretChanged;
    property OnChange;
    property OnClick;
    property OnCommandProcessed;
    property OnContextHelp;
    property OnCreateFileStream;
    property OnCustomLineColors;
    property OnCustomTokenAttribute;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropFiles;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnLeftMarginClick;
    property OnLoadData;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnProcessCommand;
    property OnProcessUserCommand;
    property OnReplaceText;
    property OnRightMarginMouseUp;
    property OnScroll;
    property OnSelectionChanged;
    property OnStartDock;
    property OnStartDrag;
    property Options;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property Replace;
    property RightMargin;
    property Scroll;
    property Search;
    property Selection;
    property ShowHint;
    {$IFDEF USE_ALPHASKINS}
    property SkinData;
    {$ENDIF}
    property SpecialChars;
    property SyncEdit;
    property TabOrder;
    property Tabs;
    property TabStop;
    property Tag;
    property Undo;
    property WantReturns;
    property Width;
    property Visible;
    property WordWrap;
  end;

implementation

uses
  BCEditor.Consts;

constructor TBCCustomDBEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
end;

destructor TBCCustomDBEditor.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;

  inherited Destroy;
end;

procedure TBCCustomDBEditor.CMEnter(var AMessage: TCMEnter);
begin
  SetEditing(True);

  inherited;
end;

procedure TBCCustomDBEditor.CMExit(var AMessage: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  SetEditing(False);

  inherited;
end;

procedure TBCCustomDBEditor.CMGetDataLink(var AMessage: TMessage);
begin
  AMessage.Result := Integer(FDataLink);
end;

procedure TBCCustomDBEditor.DataChange(ASender: TObject);
begin
  if Assigned(FDataLink.Field) then
  begin
    if FBeginEdit then
    begin
      FBeginEdit := False;
      Exit;
    end;
    if FDataLink.Field.IsBlob then
      LoadMemo
    else
      Text := FDataLink.Field.Text;
    if Assigned(FLoadData) then
      FLoadData(Self);
  end
  else
  begin
    if csDesigning in ComponentState then
      Text := name
    else
      Text := '';
  end;
end;

procedure TBCCustomDBEditor.DragDrop(ASource: TObject; X, Y: Integer);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TBCCustomDBEditor.EditingChange(ASender: TObject);
begin
  if FDataLink.Editing then
    if Assigned(FDataLink.DataSource) and (FDataLink.DataSource.State <> dsInsert) then
      FBeginEdit := True;
end;

procedure TBCCustomDBEditor.ExecuteCommand(ACommand: TBCEditorCommand; AChar: Char; AData: pointer);
begin
  if (ACommand = ecChar) and (AChar = BCEDITOR_ESCAPE) then
    FDataLink.Reset
  else
  if (ACommand >= ecEditCommandFirst) and (ACommand <= ecEditCommandLast) then
    if not FDataLink.Edit then
      Exit;

  inherited;
end;

function TBCCustomDBEditor.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TBCCustomDBEditor.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TBCCustomDBEditor.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TBCCustomDBEditor.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TBCCustomDBEditor.Loaded;
begin
  inherited Loaded;

  if csDesigning in ComponentState then
    DataChange(Self);
end;

procedure TBCCustomDBEditor.LoadMemo;
var
  LBlobStream: TStream;
begin
  try
    LBlobStream := FDataLink.DataSet.CreateBlobStream(FDataLink.Field, bmRead);
    Lines.BeginUpdate;
    Lines.LoadFromStream(LBlobStream, TEncoding.Default);
    Lines.EndUpdate;
    LBlobStream.Free;
    Modified := False;
    ClearUndo;
  except
    on E: EInvalidOperation do
      Lines.Text := Format('(%s)', [E.Message]);
  end;
  EditingChange(Self);
end;

procedure TBCCustomDBEditor.DoChange;
begin
  FDataLink.Modified;

  inherited;
end;

procedure TBCCustomDBEditor.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);

  if (AOperation = opRemove) and Assigned(FDataLink) and (AComponent = DataSource) then
    DataSource := nil;
end;

procedure TBCCustomDBEditor.SetDataField(const AValue: string);
begin
  FDataLink.FieldName := AValue;
end;

procedure TBCCustomDBEditor.SetDataSource(AValue: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := AValue;
  if Assigned(AValue) then
    AValue.FreeNotification(Self);
end;

procedure TBCCustomDBEditor.SetEditing(AValue: Boolean);
begin
  if FEditing <> AValue then
  begin
    FEditing := AValue;
    if not Assigned(FDataLink.Field) or not FDataLink.Field.IsBlob then
      FDataLink.Reset;
  end;
end;

procedure TBCCustomDBEditor.SetReadOnly(AValue: Boolean);
begin
  FDataLink.ReadOnly := AValue;
end;

procedure TBCCustomDBEditor.UpdateData(ASender: TObject);
var
  LBlobStream: TStream;
begin
  if FDataLink.Field.IsBlob then
  begin
    LBlobStream := FDataLink.DataSet.CreateBlobStream(FDataLink.Field, bmWrite);
    Lines.SaveToStream(LBlobStream);
    LBlobStream.Free;
  end
  else
    FDataLink.Field.AsString := Text;
end;

end.
