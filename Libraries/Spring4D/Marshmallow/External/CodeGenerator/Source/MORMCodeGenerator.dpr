program MORMCodeGenerator;

uses
  Forms,
  MainView in 'MainView.pas' {ViewMain},
  MainRunner in 'MainRunner.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TViewMain, ViewMain);
  Application.Run;
end.
