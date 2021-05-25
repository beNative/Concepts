program kdbgriddemolaz;

{$include kcontrols.inc}

uses
  Forms, Interfaces,
  Main in 'Main.pas';

{$R *.res} 

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
