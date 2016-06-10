unit BCEditor.Editor.SyncEdit;

interface

uses
  System.Classes, BCEditor.Editor.SyncEdit.Colors, BCEditor.Editor.Glyph, BCEditor.Types;

type
  TBCEditorSyncEdit = class(TPersistent)
  private
    FActive: Boolean;
    FActivator: TBCEditorGlyph;
    FBlockBeginPosition: TBCEditorTextPosition;
    FBlockEndPosition: TBCEditorTextPosition;
    FBlockSelected: Boolean;
    FColors: TBCEditorSyncEditColors;
    FEditBeginPosition: TBCEditorTextPosition;
    FEditEndPosition: TBCEditorTextPosition;
    FEditWidth: Integer;
    FEnabled: Boolean;
    FInEditor: Boolean;
    FOnChange: TNotifyEvent;
    FShortCut: TShortCut;
    FSyncItems: TList;
    FOptions: TBCEditorSyncEditOptions;
    procedure DoChange(ASender: TObject);
    procedure SetActive(AValue: Boolean);
    procedure SetActivator(const AValue: TBCEditorGlyph);
  public
    constructor Create;
    destructor Destroy; override;
    function IsTextPositionInBlock(ATextPosition: TBCEditorTextPosition): Boolean;
    function IsTextPositionInEdit(ATextPosition: TBCEditorTextPosition): Boolean;
    procedure Abort;
    procedure Assign(ASource: TPersistent); override;
    procedure ClearSyncItems;
    procedure MoveBeginPositionChar(ACount: Integer);
    procedure MoveEndPositionChar(ACount: Integer);
    property Active: Boolean read FActive write SetActive default False;
    property BlockBeginPosition: TBCEditorTextPosition read FBlockBeginPosition write FBlockBeginPosition;
    property BlockEndPosition: TBCEditorTextPosition read FBlockEndPosition write FBlockEndPosition;
    property BlockSelected: Boolean read FBlockSelected write FBlockSelected default False;
    property EditBeginPosition: TBCEditorTextPosition read FEditBeginPosition write FEditBeginPosition;
    property EditEndPosition: TBCEditorTextPosition read FEditEndPosition write FEditEndPosition;
    property EditWidth: Integer read FEditWidth write FEditWidth;
    property InEditor: Boolean read FInEditor write FInEditor default False;
    property SyncItems: TList read FSyncItems write FSyncItems;
  published
    property Colors: TBCEditorSyncEditColors read FColors write FColors;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Activator: TBCEditorGlyph read FActivator write SetActivator;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Options: TBCEditorSyncEditOptions read FOptions write FOptions default [seCaseSensitive];
    property ShortCut: TShortCut read FShortCut write FShortCut;
  end;

implementation

uses
  Vcl.Menus, Vcl.Graphics, BCEditor.Consts;

{ TBCEditorSyncEdit }

constructor TBCEditorSyncEdit.Create;
begin
  inherited Create;

  FActive := False;
  FBlockSelected := False;
  FEnabled := True;
  FInEditor := False;
  FShortCut := Vcl.Menus.ShortCut(Ord('J'), [ssCtrl, ssShift]);
  FOptions := [seCaseSensitive];
  FSyncItems := TList.Create;
  FColors := TBCEditorSyncEditColors.Create;
  FActivator := TBCEditorGlyph.Create(HInstance, BCEDITOR_SYNCEDIT, clFuchsia);
end;

destructor TBCEditorSyncEdit.Destroy;
begin
  ClearSyncItems;
  FSyncItems.Free;
  FColors.Free;
  FActivator.Free;
  inherited;
end;

procedure TBCEditorSyncEdit.ClearSyncItems;
var
  i: Integer;
begin
  for i := FSyncItems.Count - 1 downto 0 do
    Dispose(PBCEditorTextPosition(FSyncItems.Items[i]));
  FSyncItems.Clear;
end;

procedure TBCEditorSyncEdit.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorSyncEdit) then
  with ASource as TBCEditorSyncEdit do
  begin
    Self.Enabled := FEnabled;
    Self.FShortCut := FShortCut;
    Self.FActivator.Assign(FActivator);
    Self.DoChange(Self);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorSyncEdit.DoChange(ASender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(ASender);
end;

procedure TBCEditorSyncEdit.SetActive(AValue: Boolean);
begin
  FActive := AValue;
  DoChange(Self);
end;

procedure TBCEditorSyncEdit.SetActivator(const AValue: TBCEditorGlyph);
begin
  FActivator.Assign(AValue);
end;

function TBCEditorSyncEdit.IsTextPositionInEdit(ATextPosition: TBCEditorTextPosition): Boolean;
begin
  Result := ((ATextPosition.Line > FEditBeginPosition.Line) or
    (ATextPosition.Line = FEditBeginPosition.Line) and (ATextPosition.Char >= FEditBeginPosition.Char))
    and
    ((ATextPosition.Line < FEditEndPosition.Line) or
    (ATextPosition.Line = FEditEndPosition.Line) and (ATextPosition.Char < FEditEndPosition.Char));
end;

function TBCEditorSyncEdit.IsTextPositionInBlock(ATextPosition: TBCEditorTextPosition): Boolean;
begin
  Result := ((ATextPosition.Line > FBlockBeginPosition.Line) or
    (ATextPosition.Line = FBlockBeginPosition.Line) and (ATextPosition.Char >= FBlockBeginPosition.Char))
    and
    ((ATextPosition.Line < FBlockEndPosition.Line) or
    (ATextPosition.Line = FBlockEndPosition.Line) and (ATextPosition.Char < FBlockEndPosition.Char));
end;

procedure TBCEditorSyncEdit.MoveBeginPositionChar(ACount: Integer);
begin
  Inc(FEditBeginPosition.Char, ACount);
end;

procedure TBCEditorSyncEdit.MoveEndPositionChar(ACount: Integer);
begin
  Inc(FEditEndPosition.Char, ACount);
end;

procedure TBCEditorSyncEdit.Abort;
begin
  FActive := False;
end;

end.
