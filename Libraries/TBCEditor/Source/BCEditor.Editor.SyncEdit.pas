unit BCEditor.Editor.SyncEdit;

interface

uses
  System.Classes;

type
  TBCEditorSyncEdit = class(TPersistent)
  private
    FActive: Boolean;
    FShortCut: TShortCut;
  public
    property Active: Boolean read FActive write FActive default False;
    constructor Create(AOwner: TPersistent);
  published
    property ShortCut: TShortCut read FShortCut write FShortCut;
  end;

implementation

uses
  Vcl.Menus;

{ TBCEditorSyncEdit }

constructor TBCEditorSyncEdit.Create(AOwner: TPersistent);
begin
  inherited Create;

  FActive := False;
  FShortCut := Vcl.Menus.ShortCut(Ord('J'), [ssCtrl, ssShift]);
end;

end.
