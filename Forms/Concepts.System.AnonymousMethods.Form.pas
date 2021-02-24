{
  Copyright (C) 2013-2021 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Concepts.System.AnonymousMethods.Form;

{ Form demonstrating the variable (not value) capturing by anonymous methods.
  The scope is limited to only the routine where the anonymous method is
  defined, and not any other data in scope from that routine (as I thought at
  first was the case). }

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ActnList, Vcl.ExtCtrls;

{
  Alan Bauer:

  Anonymous methods (aka. Closures) capture the variables from the outer scope
  that are referenced within the body of the method itself. This is the
  intended behavior as it serves to extend the lifetime of the variable to
  match that of the anonymous method itself. This should not be confused with
  capturing the value of a variable. In my previous posts, A Sink Programming
  and More A Sink Kronos Programming, I demonstrated a technique for
  asynchronously dispatching an anonymous method from a background thread
  into the main UI thread. I also mentioned how there was a potential
  race-condition on the SR local variable. The simple, yet not very scalable,
  way of eliminating this race was to pause the loop by calling EndInvoke()
  prior to accessing SR again (in the FindNext() call). This time, I’m going to
  show a technique for capturing the value of the SR.Name field and not the
  whole SR variable. This will eliminate the race on the SR variable because
  the anonymous method body will no longer need to access it.
}

type
  TTestObject = class
  private
    FTestString  : string;
    FTestInt     : Integer;
    FTestVariant : Variant;

  public
    function Method(AParam: Integer): string;

    procedure AfterConstruction; override;

  end;

type
  TfrmAnonymousMethods = class(TForm)
    {$REGION 'designer controls'}
    aclMain               : TActionList;
    actAssignProcVariable : TAction;
    actExecuteProc        : TAction;
    btnExec               : TButton;
    btnExecuteProc        : TButton;
    pnlHeader             : TPanel;
    lblHeader             : TLabel;
    {$ENDREGION}

    procedure actExecuteProcExecute(Sender: TObject);
    procedure actAssignProcVariableExecute(Sender: TObject);

  private
    FProc : TProc;
    FCopy : string;

  end;

implementation

{$R *.dfm}

{$REGION 'TTestObject'}
{$REGION 'construction and destruction'}
procedure TTestObject.AfterConstruction;
begin
  inherited AfterConstruction;
  FTestString  := 'Original value';
  FTestInt     := 6;
  FTestVariant := 'Variant original value';
end;
{$ENDREGION}

{$REGION 'public methods'}
function TTestObject.Method(AParam: Integer): string;
var
  LInteger : Integer;
  LTestString: string;
begin
  LInteger := 8;
  //this causes a AV:
  //LTestString := 'test' + FTestString;       //  -> ONLY LOCAL Variables are captured, no references to fields of the surrounding object!!!
  LTestString := 'test';
  // this is OK:

  //FTestString := FTestString + '...OK';
  //FTestVariant := 'Variant new assigned value';


  // UNSAFE:
  //Result := Format('%s %d %d', [FTestString, AParam, LInteger]);

  // SAFE:
  Result := Format('%s %d %d', [LTestString, AParam, LInteger]);
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'TfrmAnonymousMethods'}
{$REGION 'action handlers'}
procedure TfrmAnonymousMethods.actAssignProcVariableExecute(Sender: TObject);
var
  T : TTestObject;
begin
  T := TTestObject.Create;
  try
    FProc := procedure
    begin
      FCopy := T.Method(5);
      ShowMessage(FCopy);
    end;
  finally
    FreeAndNil(T); // -> all class members are not accessable anymore and will result in AV's!
  end;
end;

procedure TfrmAnonymousMethods.actExecuteProcExecute(Sender: TObject);
begin
  if Assigned(FProc) then
    FProc()
  else
    ShowMessage('Assign proc first.');
end;
{$ENDREGION}
{$ENDREGION}

end.
