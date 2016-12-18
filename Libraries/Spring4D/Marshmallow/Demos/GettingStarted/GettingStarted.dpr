program GettingStarted;

uses
  Forms,
  Spring.Persistence.Adapters.SQLite,
  ProductModel in 'Source\ProductModel.pas',
  ViewEditProduct in 'Views\ViewEditProduct.pas' {ProductEditForm},
  ViewMain in 'Views\ViewMain.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
{$WARNINGS OFF}
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
{$WARNINGS ON}
  Application.Run;
end.
