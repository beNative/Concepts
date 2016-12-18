unit BCEditor.Editor.UnknownChars;

interface

uses
  System.Classes;

type
  TBCEditorUnknownChars = class(TPersistent)
  strict private
    FEnabled: Boolean;
    FReplaceChar: AnsiChar;
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property ReplaceChar: AnsiChar read FReplaceChar write FReplaceChar default '?';
  end;

implementation

constructor TBCEditorUnknownChars.Create;
begin
  inherited;

  FEnabled := True;
  FReplaceChar := '?';
end;

procedure TBCEditorUnknownChars.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorUnknownChars) then
  with ASource as TBCEditorUnknownChars do
  begin
    Self.FEnabled := FEnabled;
    Self.FReplaceChar := FReplaceChar;
  end
  else
    inherited Assign(ASource);
end;


end.
