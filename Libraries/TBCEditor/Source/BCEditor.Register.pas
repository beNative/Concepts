unit BCEditor.Register;

interface

uses
  System.Classes, BCEditor.Editor, BCEditor.Editor.DB, BCEditor.Print, BCEditor.Print.Preview, BCEditor.MacroRecorder;

procedure Register;

implementation

uses
  DesignIntf;

procedure Register;
begin
  RegisterComponents('BCEditor', [TBCEditor, TBCDBEditor, TBCEditorPrint, TBCEditorPrintPreview, TBCEditorMacroRecorder]);
  { UnlistPublishedProperty }
  UnlistPublishedProperty(TBCEditor, 'Ctl3D');
  UnlistPublishedProperty(TBCEditor, 'CustomHint');
  UnlistPublishedProperty(TBCEditor, 'Hint');
  UnlistPublishedProperty(TBCEditor, 'HelpContext');
  UnlistPublishedProperty(TBCEditor, 'HelpKeyword');
  UnlistPublishedProperty(TBCEditor, 'HelpType');
  UnlistPublishedProperty(TBCEditor, 'ImeMode');
  UnlistPublishedProperty(TBCEditor, 'ImeName');
  UnlistPublishedProperty(TBCEditor, 'ParentColor');
  UnlistPublishedProperty(TBCEditor, 'ParentCtl3D');
  UnlistPublishedProperty(TBCEditor, 'ParentCustomHint');
  UnlistPublishedProperty(TBCEditor, 'ParentFont');
  UnlistPublishedProperty(TBCEditor, 'ParentShowHint');
  UnlistPublishedProperty(TBCEditor, 'ShowHint');
end;

end.
