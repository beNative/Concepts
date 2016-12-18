{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Concepts.System.Threading.Form;

{ Demonstration of the new Delphi XE7 - System.Threading unit, which is also
  referenced to as the PPL (Parallel Programming Library). }

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  System.ImageList, System.Threading,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList, Vcl.ImgList,
  Vcl.StdCtrls, Vcl.ExtCtrls,

  Spring, Spring.Collections,

  DDuce.Components.LogTree;

type
  TfrmThreading = class(TForm)
    aclMain              : TActionList;
    actCancelTask        : TAction;
    actExecuteParallel   : TAction;
    actExecuteSequential : TAction;
    actStartTask         : TAction;
    btnExecute           : TButton;
    btnExecuteSequential : TButton;
    btnStartTask         : TButton;
    edtIterations        : TLabeledEdit;
    edtStrides           : TLabeledEdit;
    imlMain              : TImageList;
    pnlLog               : TPanel;
    btnCancelTask        : TButton;

    procedure actExecuteParallelExecute(Sender: TObject);
    procedure actExecuteSequentialExecute(Sender: TObject);
    procedure actStartTaskExecute(Sender: TObject);
    procedure actCancelTaskExecute(Sender: TObject);

  strict private
    FProc      : TProc<Integer>;
    FCount     : Integer;
    FLock      : Lock;
    FLog       : TLogTree;
    FTasks     : IList<ITask>;
    FTaskCount : Integer;

    function GetIterations: Integer;
    function GetStrides: Integer;

  protected
    procedure ExecuteParallel(
      const AProc : TProc<Integer>;
      ACount      : Integer;
      AStrides    : Integer
    );
    procedure ExecuteSequential(
      const AProc : TProc<Integer>;
      ACount      : Integer
    );

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property Iterations: Integer
      read GetIterations;

    { Amount of executions that should be executed by each thread. }
    property Strides: Integer
      read GetStrides;
  end;

implementation

{$R *.dfm}

uses
  System.Diagnostics,

  DDuce.Components.Factories, DDuce.RandomData;

resourcestring
  STaskStarted                  = 'Task %d started.';
  STaskFinished                 = 'Task %d finished.';
  SFinishedParallelExecutions   = 'Finished %d parallel executions in %dms.';
  SFinishedSequentialExecutions = 'Finished %d sequential executions in %dms.';

{$REGION 'construction and destruction'}
procedure TfrmThreading.AfterConstruction;
begin
  inherited AfterConstruction;
  FProc := procedure(AIndex: Integer)
    begin
      Sleep(1);
    end;
  FLog := TDDuceComponents.CreateLogTree(Self, pnlLog);
  FLog.DateTimeFormat := 'hh:nn:ss.zzz';
  FLog.Images := imlMain;
  FTasks := TCollections.CreateInterfaceList<ITask>;
end;

procedure TfrmThreading.BeforeDestruction;
begin
  FLog.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TfrmThreading.GetIterations: Integer;
begin
  Result := StrToIntDef(edtIterations.Text, 0);
end;

function TfrmThreading.GetStrides: Integer;
begin
  Result := StrToIntDef(edtStrides.Text, 0);
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmThreading.actCancelTaskExecute(Sender: TObject);
begin
  if FTasks.Last.Status = TTaskStatus.Running then
    FTasks.Last.Cancel;
end;

procedure TfrmThreading.actExecuteParallelExecute(Sender: TObject);
begin
  TTask.Run(procedure
    begin
      ExecuteParallel(FProc, Iterations, Strides);
    end
  );
end;

procedure TfrmThreading.actExecuteSequentialExecute(Sender: TObject);
begin
  TTask.Run(procedure
    begin
      ExecuteSequential(FProc, Iterations);
    end
  );
end;

procedure TfrmThreading.actStartTaskExecute(Sender: TObject);
var
  C : Integer;
begin
  C := FTasks.Count + 1;
  FLog.LogFmt(STaskStarted, [C]);
  FTasks.Add(
    TTask.Run(procedure
      begin
        Sleep(5000);
        TThread.Queue(TThread.Current, procedure
          begin
            FLog.LogFmt(STaskFinished, [C]);
          end
        );
      end
    )
  );
end;
{$ENDREGION}

{$REGION 'protected methods'}
{ Executes the given anonymous procedure ACount times in a parallel for loop.
  AStrides indicates how many executions of the loop are executed per thread. }

procedure TfrmThreading.ExecuteParallel(const AProc: TProc<Integer>;
  ACount: Integer; AStrides: Integer);
var
  SW : TStopwatch;
begin
  SW := TStopwatch.Create;
  SW.Start;
  FCount := 0;
  TParallel.For(AStrides, 1, ACount, AProc);
  SW.Stop;
  TThread.Queue(TThread.CurrentThread, procedure
    begin
      FLog.LogFmt(
        SFinishedParallelExecutions,
        [ACount, SW.ElapsedMilliseconds]
      );
    end
  );
end;

{ Executes the given anonymous procedure ACount times using a normal for loop. }

procedure TfrmThreading.ExecuteSequential(const AProc: TProc<Integer>;
  ACount: Integer);
var
  SW : TStopwatch;
  I  : Integer;
begin
  SW := TStopwatch.Create;
  SW.Start;
  FCount := 0;
  for I := 1 to ACount do
  begin
    FProc(I);
  end;
  SW.Stop;
  TThread.Queue(TThread.CurrentThread, procedure
    begin
      FLog.LogFmt(
        SFinishedSequentialExecutions,
        [ACount, SW.ElapsedMilliseconds]
      );
    end
  );
end;
{$ENDREGION}

end.
