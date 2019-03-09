{
  Copyright (C) 2013-2019 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Concepts.System.Rtti.Form;

interface

uses
  System.Classes, System.Actions,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Forms, Vcl.ActnList;

type
  TfrmRTTI = class(TForm)
    btnExecute : TButton;
    aclMain    : TActionList;
    actExecute : TAction;

    procedure actExecuteExecute(Sender: TObject);

  end;

implementation

{$R *.dfm}

uses
  Vcl.Dialogs,

  DDuce.Utils,

  Concepts.Utils;

type
  TTest    = (ttOne, ttTwo, ttThree);
  TTestSet =  set of TTest;

procedure TfrmRTTI.actExecuteExecute(Sender: TObject);
var
  S : TTestSet;
begin
  S := [ttOne, ttThree];
  ShowMessage(SetToString(TypeInfo(TTestSet), S));
end;

end.


