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

{$I Concepts.inc}

unit Concepts.Spring.Utils.Form;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList, Vcl.StdCtrls;

type
  TfrmSpringUtils = class(TForm)
    aclMain : TActionList;
    mmo1: TMemo;
  private

  public
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

uses
  System.Rtti,

  Spring.Utils,

  DDuce.DynamicRecord;

{ TfrmSpringUtils }

procedure TfrmSpringUtils.AfterConstruction;
//var
//  V : TValue;
//  V2 : TValue;
var
  R : TRecord;
  OS : TOperatingSystem;
  V : TFileVersionInfo;
begin
//  inherited AfterConstruction;
//  V := 3;
//  V2 := 'Test';
//  V2 := V;
  //R.AssignProperty();
  OS := TOperatingSystem.Create;
  mmo1.Lines.Text := R.ToString;
  OS.Free;



end;

end.
