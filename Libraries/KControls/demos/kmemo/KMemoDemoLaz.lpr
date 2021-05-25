program KMemoDemoLaz;

uses
  Interfaces,
  Forms,
  Main;

{$R *.res} 

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
