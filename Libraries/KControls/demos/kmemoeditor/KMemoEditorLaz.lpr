program KMemoEditorLaz;

{$MODE Delphi}

uses
  Forms, Interfaces,
  Main in 'Main.pas' {MainForm};

{$R *.res}

begin
  Application.Title:='KMemoEditor';
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.