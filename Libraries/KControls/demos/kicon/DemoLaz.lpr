program DemoLaz;

uses
  Interfaces,
  Forms,
  Main in 'Main.pas' {MainForm}, LResources;

{$R *.res} 

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
