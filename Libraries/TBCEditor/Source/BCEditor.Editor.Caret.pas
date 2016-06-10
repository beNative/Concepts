unit BCEditor.Editor.Caret;

interface

uses
  System.Classes, BCEditor.Editor.Caret.NonBlinking, BCEditor.Editor.Caret.MultiEdit, BCEditor.Editor.Caret.Styles,
  BCEditor.Editor.Caret.Offsets, BCEditor.Types;

type
  TBCEditorCaret = class(TPersistent)
  strict private
    FMultiEdit: TBCEditorCaretMultiEdit;
    FNonBlinking: TBCEditorCaretNonBlinking;
    FOffsets: TBCEditorCaretOffsets;
    FOnChange: TNotifyEvent;
    FOptions: TBCEditorCaretOptions;
    FStyles: TBCEditorCaretStyles;
    FVisible: Boolean;
    procedure DoChange(ASender: TObject);
    procedure SetMultiEdit(AValue: TBCEditorCaretMultiEdit);
    procedure SetNonBlinking(AValue: TBCEditorCaretNonBlinking);
    procedure SetOffsets(AValue: TBCEditorCaretOffsets);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure SetOptions(const AValue: TBCEditorCaretOptions);
    procedure SetStyles(const AValue: TBCEditorCaretStyles);
    procedure SetVisible(AValue: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  published
    property MultiEdit: TBCEditorCaretMultiEdit read FMultiEdit write SetMultiEdit;
    property NonBlinking: TBCEditorCaretNonBlinking read FNonBlinking write SetNonBlinking;
    property Offsets: TBCEditorCaretOffsets read FOffsets write SetOffsets;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property Options: TBCEditorCaretOptions read FOptions write SetOptions;
    property Styles: TBCEditorCaretStyles read FStyles write SetStyles;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

implementation

{ TBCEditorCaret }

constructor TBCEditorCaret.Create;
begin
  inherited;

  FMultiEdit := TBCEditorCaretMultiEdit.Create;
  FNonBlinking := TBCEditorCaretNonBlinking.Create;
  FOffsets := TBCEditorCaretOffsets.Create;
  FStyles := TBCEditorCaretStyles.Create;
  FVisible := True;
end;

destructor TBCEditorCaret.Destroy;
begin
  FMultiEdit.Free;
  FNonBlinking.Free;
  FOffsets.Free;
  FStyles.Free;

  inherited;
end;

procedure TBCEditorCaret.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorCaret) then
  with ASource as TBCEditorCaret do
  begin
    Self.FStyles.Assign(FStyles);
    Self.FMultiEdit.Assign(FMultiEdit);
    Self.FNonBlinking.Assign(FNonBlinking);
    Self.FOffsets.Assign(FOffsets);
    Self.FOptions := FOptions;
    Self.FVisible := FVisible;
    Self.DoChange(Self);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorCaret.SetOnChange(AValue: TNotifyEvent);
begin
  FOnChange := AValue;
  FOffsets.OnChange := AValue;
  FStyles.OnChange := AValue;
  FMultiEdit.OnChange := AValue;
  FNonBlinking.OnChange := AValue;
end;

procedure TBCEditorCaret.DoChange(ASender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(ASender);
end;

procedure TBCEditorCaret.SetStyles(const AValue: TBCEditorCaretStyles);
begin
  FStyles.Assign(AValue);
end;

procedure TBCEditorCaret.SetMultiEdit(AValue: TBCEditorCaretMultiEdit);
begin
  FMultiEdit.Assign(AValue);
end;

procedure TBCEditorCaret.SetNonBlinking(AValue: TBCEditorCaretNonBlinking);
begin
  FNonBlinking.Assign(AValue);
end;

procedure TBCEditorCaret.SetVisible(AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DoChange(Self);
  end;
end;

procedure TBCEditorCaret.SetOffsets(AValue: TBCEditorCaretOffsets);
begin
  FOffsets.Assign(AValue);
end;

procedure TBCEditorCaret.SetOptions(const AValue: TBCEditorCaretOptions);
begin
  if FOptions <> AValue then
  begin
    FOptions := AValue;
    DoChange(Self);
  end;
end;

end.
