{
  Copyright (C) 2013-2025 Tim Sinaeve tim.sinaeve@gmail.com

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

{ Form demonstrating how to use the TVirtualMethodInterceptor class.

  (TODO)
}

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls;

type
  TfrmVirtualMethodInterceptor = class(TForm)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

  end;

implementation

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TfrmVirtualMethodInterceptor.AfterConstruction;
begin
  inherited AfterConstruction;
end;

destructor TfrmVirtualMethodInterceptor.Destroy;
begin
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmVirtualMethodInterceptor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;
{$ENDREGION}

end.
