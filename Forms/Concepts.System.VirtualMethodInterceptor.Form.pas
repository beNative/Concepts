{
  Copyright (C) 2013-2017 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Concepts.System.VirtualMethodInterceptor.Form;

{ Form demonstrating how to use the TVirtualMethodInterceptor class. }

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfrmVirtualMethodInterceptor = class(TForm)
    btn1: TButton;
    procedure btn1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FPanel : TPanel;
    FButton: TButton;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

{$R *.dfm}

{ TfrmVirtualMethodInterceptor }

procedure TfrmVirtualMethodInterceptor.AfterConstruction;
begin
  inherited AfterConstruction;
  FPanel := TPanel.Create(nil);
  FButton := TButton.Create(nil);
  FButton.Parent := Self;
end;

procedure TfrmVirtualMethodInterceptor.BeforeDestruction;
begin
  inherited BeforeDestruction;
//  ShowMessage(FButton.Caption);
end;

procedure TfrmVirtualMethodInterceptor.btn1Click(Sender: TObject);
begin
  FreeAndNil(FPanel);
  if Assigned(FButton) then
    ShowMessage(FButton.ClassName);
end;

procedure TfrmVirtualMethodInterceptor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

end.
