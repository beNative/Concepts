program TBCEditorDemo;

uses
  {$ifdef DEBUG}
  FastMM4,
  {$endif}
  Vcl.Forms,
  TBCEditorDemo.Forms.Main in 'Forms\TBCEditorDemo.Forms.Main.pas' {MainForm};

{$R *.res}

begin
  {$ifdef DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$endif}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'BCEditor - Property Demo';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
