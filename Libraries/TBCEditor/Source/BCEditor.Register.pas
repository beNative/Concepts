unit BCEditor.Register;

interface

uses
  System.Classes, BCEditor.Editor, BCEditor.Editor.DB, BCEditor.Print, BCEditor.Print.Preview, BCEditor.MacroRecorder;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BCEditor', [TBCEditor, TBCDBEditor, TBCEditorPrint, TBCEditorPrintPreview, TBCEditorMacroRecorder]);
end;

end.
