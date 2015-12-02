unit BCEditor.RegisterProperty;

interface

uses
  DesignIntf, DesignEditors, VCLEditors, StrEdit, System.Classes;

procedure Register;

implementation

uses
  Vcl.Controls, BCEditor.Editor, BCEditor.MacroRecorder, System.SysUtils;

{ Register }

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(Char), nil, '', TCharProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), nil, '', TStringListProperty);
  RegisterPropertyEditor(TypeInfo(TShortCut), TBCEditorMacroRecorder, '', TShortCutProperty);
end;

end.
