{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}

{$I Concepts.inc}

unit Concepts.System.Threads.Form;

interface

{ Demonstration of following thread related concepts:
    - TThread.CreateAnonymousThread
    - TMonitor.Enter/Exit
    - TMonitor.Wait/Pulse/PulseAll
    - Spring.Collections (IList<TThread>)
 }

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ActnList, Vcl.ComCtrls, Vcl.CheckLst,

  Spring.Collections, Vcl.ExtCtrls;

type
  TMonitor = System.TMonitor;

type
  TfrmThreads= class(TForm)
    {$REGION 'designer controls'}
    aclMain                   : TActionList;
    actCreateAnonymousThreads : TAction;
    actMonitorEnter           : TAction;
    actMonitorExit            : TAction;
    actMonitorPulse           : TAction;
    actMonitorPulseAll        : TAction;
    actTerminateThreads       : TAction;
    btnCreateAnonymousThreads : TButton;
    btnMonitorEnter           : TButton;
    btnMonitorExit            : TButton;
    btnMonitorPulse           : TButton;
    btnMonitorPulseAll        : TButton;
    btnTerminateThreads       : TButton;
    lbxCounters               : TListBox;
    sbrMain                   : TStatusBar;
    lbxThreads                : TCheckListBox;
    pnlHeader                 : TPanel;
    lblHeader                 : TLabel;
    {$ENDREGION}

    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure actCreateAnonymousThreadsExecute(Sender: TObject);
    procedure actTerminateThreadsExecute(Sender: TObject);
    procedure actMonitorEnterExecute(Sender: TObject);
    procedure actMonitorExitExecute(Sender: TObject);
    procedure actMonitorPulseExecute(Sender: TObject);
    procedure actMonitorPulseAllExecute(Sender: TObject);

  private
    FThreads : IList<TThread>;
    FLock    : TObject; // arbitrary lock object
    FLocked  : Boolean;

    procedure CreateAnonymousThreads(
      const AThreadCount   : Integer;
      const ACountDownFrom : Integer
    );

    function ThreadsRunning: Integer;
    procedure TerminateThreads;
    procedure MonitorEnter;
    procedure MonitorExit;
    procedure UpdateStatusBar;

  protected
    procedure UpdateActions; override;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

{$R *.dfm}

uses
  System.SyncObjs;

resourcestring
  SStillThreadsRunning =
    'Cannot exit!'#13#10'There are still %d threads running!';

{$REGION 'construction and destruction'}
procedure TfrmThreads.AfterConstruction;
begin
  inherited;
  Application.HintHidePause := 10000;
  Randomize;
  FThreads := TCollections.CreateObjectList<TThread>(True);
  FLock := TObject.Create;
end;

procedure TfrmThreads.BeforeDestruction;
begin
  FLock.Free;
  inherited;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmThreads.actCreateAnonymousThreadsExecute(Sender: TObject);
begin
  CreateAnonymousThreads(20, 10);
end;

procedure TfrmThreads.actMonitorEnterExecute(Sender: TObject);
begin
  MonitorEnter;
  actMonitorExit.Enabled := True;
  actMonitorEnter.Enabled := False;
end;

procedure TfrmThreads.actMonitorExitExecute(Sender: TObject);
begin
  MonitorExit;
  actMonitorExit.Enabled := False;
  actMonitorEnter.Enabled := True;
end;

procedure TfrmThreads.actMonitorPulseAllExecute(Sender: TObject);
begin
  TMonitor.PulseAll(FLock);
end;

procedure TfrmThreads.actMonitorPulseExecute(Sender: TObject);
begin
  TMonitor.Pulse(FLock);
end;

procedure TfrmThreads.actTerminateThreadsExecute(Sender: TObject);
begin
  TerminateThreads;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmThreads.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  N : Integer;
begin
  N := ThreadsRunning;
  CanClose := N = 0;
  if not CanClose then
  begin
    ShowMessageFmt(SStillThreadsRunning, [N]);
  end;
end;
{$ENDREGION}

{$REGION 'private methods'}
{  Creates a given amount of (anonymous) threads. Each thread counts down a
   given number to zero and then terminates. The active count of each thread is
   displayed in a listbox (lbxCounters).
   Therefor it synchronizes with the main thread (using TThread.Queue) for each
   countdown. }

procedure TfrmThreads.CreateAnonymousThreads(const AThreadCount,
  ACountDownFrom: Integer);
var
  I : Integer;
  T : TThread;
begin
  FThreads.Clear;
  lbxCounters.Clear;
  lbxThreads.Clear;
  for I := 0 to AThreadCount - 1 do
  begin
    T := TThread.CreateAnonymousThread(
      procedure // code executed in the thread (= TThread.Execute method)
      var
        Count  : Integer;
        Thread : TThread;
      begin
        Count := ACountDownFrom;
        while not TThread.CheckTerminated and (Count > 0) do
        begin
          Thread := TThread.CurrentThread;
          // use thread safe Decrement because the count variable is accessed
          // from the main thread in the queued anonymous method.
          TInterlocked.Decrement(Count);
          TThread.Queue(
            TThread.CurrentThread,
            procedure // code executed in the main VCL thread to show the
            var       // current count of each thread in the listbox control.
              Index : Integer;
            begin
              Index := lbxCounters.Items.IndexOfObject(Thread);
              if Index >= 0 then
              begin
                lbxCounters.Items[Index] := Count.ToString;
              end;
            end
          );
          Sleep(Random(1000) + 100);
          if not TThread.CheckTerminated then
          begin
            // allows access of one thread only to the monitored section.
            TMonitor.Enter(FLock);
            // Wait releases the lock and waits for pulse (against our FLock
            // object, with a time-out of 10 seconds.
            // The call to Wait is only valid between an Enter/Exit code block.
            TMonitor.Wait(FLock, 10000);
            TMonitor.Exit(FLock);
          end;
        end;
      end
    ); // created suspended
    T.FreeOnTerminate := False;
    lbxCounters.AddItem('', T);
    FThreads.Add(T);
    lbxThreads.AddItem(I.ToString, T);
    T.Start;
  end; // for I := 0 to AThreadCount - 1 do
end;

procedure TfrmThreads.UpdateStatusBar;
begin
  sbrMain.Panels[0].Text := Format('%d threads running', [ThreadsRunning]);
  if FLocked then
    sbrMain.Panels[1].Text := 'Locked'
  else
    sbrMain.Panels[1].Text := '';
end;

{ Terminates all threads in the list. }

procedure TfrmThreads.TerminateThreads;
var
  T: TThread;
begin
  for T in FThreads do
    T.Terminate;
end;

{ Returns the amount of (secondary) threads that are running }

function TfrmThreads.ThreadsRunning: Integer;
var
  T: TThread;
begin
  Result := 0;
  for T in FThreads do
  begin
    if not T.Finished then
      Inc(Result);
  end;
end;

{ Blocks all threads that guard the FLock object. }

procedure TfrmThreads.MonitorEnter;
begin
  TMonitor.Enter(FLock);
  FLocked := True;
end;

{ Release lock on FLock from the main thread. }

procedure TfrmThreads.MonitorExit;
begin
  TMonitor.Exit(FLock);
  FLocked := False;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TfrmThreads.UpdateActions;
begin
  UpdateStatusBar;
  inherited;
end;
{$ENDREGION}

end.
