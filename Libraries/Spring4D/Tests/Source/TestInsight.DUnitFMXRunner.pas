unit TestInsight.DUnitFMXRunner;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls;

type
  TfrmTestInsightDUnitFMXRunner = class(TForm)
    lblText: TLabel;
    tmrStartTests: TTimer;
    procedure tmrStartTestsTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private type
    TTestingState = (Unknown, Delayed, Running, Done);
  private
    { Private declarations }
    FTestingState: TTestingState;
{$IFDEF ANDROID}
    FTimeout: Cardinal;
    procedure TimerNotify(TimerId: Integer);
{$ENDIF}
    procedure DoRunTests;
    procedure RunTests;
  public
    { Public declarations }
  end;

var
  frmTestInsightDUnitFMXRunner: TfrmTestInsightDUnitFMXRunner;

implementation

uses
  System.Generics.Collections,
{$IFDEF ANDROID}
  Androidapi.Timer,
{$ENDIF}
  Spring.TestRunner;

{$R *.fmx}

procedure TfrmTestInsightDUnitFMXRunner.DoRunTests;
begin
  FTestingState := Running;
  try
{$IFDEF ANDROID}
    // Disable timers during tests, timers make RTL debugging harder
    // Note that this will stop timers in the form (an everywhere else)
    // but we don't mind.
    AndroidTimerSetHandler(nil);
{$ENDIF}
    TThread.NameThreadForDebugging('Testing');
    RunRegisteredTests;
    lblText.Text := 'Tests are done.' + sLineBreak
      + 'See TestInsight in Delphi for results.' + sLineBreak
      + 'You may close the app.';
  finally
    FTestingState := Done;
{$IFDEF ANDROID}
    FTimeout := TThread.GetTickCount + tmrStartTests.Interval;
    AndroidTimerSetHandler(TimerNotify);
{$ENDIF}
  end;
end;

procedure TfrmTestInsightDUnitFMXRunner.FormCreate(Sender: TObject);
begin
  // FTestingState := Delayed;
  // Sometimes the app hangs in ShowForm waiting for some dispatch so run the
  // tests ASAP. FormCreate should be called after the app and its window
  // is initialized (Application.RealCreateForms) so everything shold be set up.
  if FTestingState = Unknown then
    RunTests;
  // Keep it running even without timers as the OS will otherwise relaunch the
  // app multiple times before giving up.
end;

procedure TfrmTestInsightDUnitFMXRunner.FormShow(Sender: TObject);
begin
{$IFDEF ANDROID}
  tmrStartTests.Enabled := FTestingState in [Delayed];
{$ELSE}
  tmrStartTests.Enabled := True;
{$ENDIF}
end;

procedure TfrmTestInsightDUnitFMXRunner.RunTests;
begin
  if FTestingState in [Unknown, Delayed] then
    DoRunTests;
end;

{$IFDEF ANDROID}

procedure TfrmTestInsightDUnitFMXRunner.TimerNotify(TimerId: Integer);
begin
  if TThread.GetTickCount > FTimeout then
    TThread.Queue(nil, procedure begin Close; end);
end;

{$ENDIF}

procedure TfrmTestInsightDUnitFMXRunner.tmrStartTestsTimer(Sender: TObject);
begin
  tmrStartTests.Enabled := False;
  if FTestingState = Delayed then
    RunTests;
  Close;
end;

end.
