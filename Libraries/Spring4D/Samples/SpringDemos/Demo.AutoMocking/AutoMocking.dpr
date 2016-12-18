program AutoMocking;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  BasketController in 'BasketController.pas',
  Interfaces in 'Interfaces.pas',
  CommandChannel in 'CommandChannel.pas' {$R *.res};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
