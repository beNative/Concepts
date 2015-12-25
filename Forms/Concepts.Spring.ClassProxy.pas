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

unit Concepts.Spring.ClassProxy;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList,

  Spring.Interception.ClassProxy;

type
  TfrmClassProxy = class(TForm)
    aclMain             : TActionList;
    actAddFormProxy     : TAction;
    actReleaseFormProxy : TAction;
    procedure actAddFormProxyExecute(Sender: TObject);
    procedure actReleaseFormProxyExecute(Sender: TObject);
  private

  public
    procedure AfterConstruction; override;


  end;

implementation

{$R *.dfm}

procedure TfrmClassProxy.actAddFormProxyExecute(Sender: TObject);
begin
  //
end;

procedure TfrmClassProxy.actReleaseFormProxyExecute(Sender: TObject);
begin
//
end;

procedure TfrmClassProxy.AfterConstruction;
begin
  inherited AfterConstruction;
  //TClassProxy.Create(TFormClass, [], , );

end;

end.
