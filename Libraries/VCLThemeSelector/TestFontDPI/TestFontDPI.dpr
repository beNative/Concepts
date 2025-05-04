program TestFontDPI;

uses
  Vcl.Forms,
  MainUnit in 'MainUnit.pas' {MainForm},
  ChildUnit in 'ChildUnit.pas' {ChildForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.MainFormOnTaskbar := True;
  Application.Run;
end.
