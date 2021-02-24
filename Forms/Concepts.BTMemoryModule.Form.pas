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

unit Concepts.BTMemoryModule.Form;

{ TODO: Add BTMemoryModule example. }

interface

uses
  System.Actions, System.Classes,
  Vcl.ActnList, Vcl.Controls, Vcl.StdCtrls, Vcl.Forms, Vcl.ExtCtrls;

type
  TfrmBTMemoryModule = class(TForm)
    aclMain           : TActionList;
    actExecuteFromDLL : TAction;
    btnExecuteFromDLL : TButton;
    pnlHeader         : TPanel;
    lblHeader         : TLabel;

    procedure actExecuteFromDLLExecute(Sender: TObject);

  private
    FLibHandle: Cardinal;

  end;

implementation

{$R *.dfm}

uses
  WinApi.Windows;

const
  LIB_NAME  = 'ConceptsLib.dll';
  PROC_NAME = 'ShowMessageFromDLL';

type
  TShowMessageFromDLL = procedure(AMessage: PChar); stdcall;

var
  ShowMessageFromDLL: TShowMessageFromDLL;

{$REGION 'action handlers'}
procedure TfrmBTMemoryModule.actExecuteFromDLLExecute(Sender: TObject);
begin
  FLibHandle := LoadLibrary(LIB_NAME);
  try
    if FLibHandle <> 0 then
    begin
      @ShowMessageFromDLL := GetProcAddress(FLibHandle, PROC_NAME);
    end;
    ShowMessageFromDLL('Test');
  finally
    FreeLibrary(FLibHandle);
  end;
end;
{$ENDREGION}

end.
