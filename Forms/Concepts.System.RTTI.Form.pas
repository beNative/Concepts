{
  Copyright (C) 2013-2015 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Concepts.System.Rtti.Form;

interface

uses
  System.Classes,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Forms;

type
  TfrmRTTI = class(TForm)
    btn1: TButton;
    procedure btn1Click(Sender: TObject);

  end;

implementation

{$R *.dfm}

uses
  Vcl.Dialogs,

  Concepts.Utils;

type
  TTest    = (ttOne, ttTwo, ttThree);
  TTestSet =  set of TTest;

procedure TfrmRTTI.btn1Click(Sender: TObject);
var
  s: TTestSet;
begin
  s := [ttOne, ttThree];
  ShowMessage(SetToString(TypeInfo(TTestSet), s));
end;

end.


