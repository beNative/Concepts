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

unit Concepts.System.Threading.Form;

{ Demonstration of the new Delphi XE7 - System.Threading unit, which is also
  sometimes referenced to as the 'Parallel Programming Library'. }

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions,
  Vcl.ActnList;

type
  TfrmThreading = class(TForm)
    aclMain: TActionList;

    procedure FormCreate(Sender: TObject);
  end;

implementation

{$R *.dfm}

uses
  System.Threading;

procedure TfrmThreading.FormCreate(Sender: TObject);
var
  T : TProc<Integer>;
begin
  TParallel.For(1, 10, T);
end;

end.
