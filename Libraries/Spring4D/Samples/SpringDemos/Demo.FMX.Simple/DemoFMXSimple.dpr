program DemoFMXSimple;

uses
  System.StartUpCopy,
  System.Classes,
  System.Messaging,
  FMX.MobilePreview,
  FMX.Platform,
  FMX.Forms,
  Spring.Container,
  Demo.FMX.Simple.MainForm in 'Demo.FMX.Simple.MainForm.pas' {MainForm};

{$R *.res}

{$IF Defined(IOS) or Defined(ANDROID)}
  {$DEFINE MOBILE}
{$IFEND}

procedure RegisterServices(const container: TContainer);
begin
  container.RegisterType<TMainForm>
    .Implements<TMainForm>
    .AsSingleton
    .DelegateTo(
      function: TMainForm
      begin
        // This forces form creation to be synchronous.
        // May or may not be called on certain platforms already by the FMX framework.
        // Android will call this prior sending the message but iOS won't.
        // May be called multiple times with no harm done.
        Application.RealCreateForms;
        Application.CreateForm(TMainForm, Result);
        // Create instance and assign MainForm
        // (would otherwise be done by Application.CreateMainForm)
        Application.MainForm := Result;
        // And make it visible  as this may not be set in the designer and
        // no window would be displayed
        Application.MainForm.Visible := True;
      end);
  container.Build;
end;

procedure ResolveMainForm(const container: TContainer);
begin
{$IFDEF MOBILE}
  // Platform service needs to handle that
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage,
    procedure (const Sender: TObject; const M: TMessage)
    begin
      if M is TApplicationEventMessage then
        case TApplicationEventMessage(M).Value.Event of
          TApplicationEvent.FinishedLaunching: container.Resolve<TMainForm>;
        end;
    end);
{$ELSE}
  // Can be called synchronously
  container.Resolve<TMainForm>;
{$ENDIF}
end;

var
  container: TContainer;
begin
  Application.Initialize;
  container := TContainer.Create;
  try
    RegisterServices(container);
    ResolveMainForm(container);
    Application.Run;
  finally
    container.Free;
  end;
  ReportMemoryLeaksOnShutdown := True;
end.
