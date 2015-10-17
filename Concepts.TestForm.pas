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

unit Concepts.TestForm;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type

  IObservable<T> = interface(TFunc<T>)
    procedure Invoke(AValue: T); overload;
//    function GetValue: T;
//    procedure SetValue(const value: T);
//    property Value: T read GetValue write SetValue;
  end;

  TfrmTest = class(TForm, IInterface)
  private
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TfrmTest.AfterConstruction;
var
  f : IObservable<string>;
begin
  f('Test');
  inherited AfterConstruction;

end;

procedure TfrmTest.BeforeDestruction;
begin
  ShowMessage('Destruct');
  inherited BeforeDestruction;
end;
{$ENDREGION}

end.
