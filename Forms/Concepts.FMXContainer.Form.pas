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

unit Concepts.FMXContainer.Form;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  FMX.Forms,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,

  Parnassus.FMXContainer;

type
  TfrmFMXContainer = class(TForm)
    {$REGION 'designer controls'}
    pnlFMXContainer : TPanel;
    pnlHeader       : TPanel;
    {$ENDREGION}

  private
    FFMXContainer: TFireMonkeyContainer;

  public
    procedure AfterConstruction; override;

    procedure FFMXContainerCreateFMXForm(var Form: FMX.Forms.TCommonCustomForm);
    procedure FFMXContainerDestroyFMXForm(
      var Form   : FMX.Forms.TCommonCustomForm;
      var Action : TCloseHostedFMXFormAction
    );
  end;

implementation

{$R *.dfm}

uses
  Concepts.ComponentInspector,

  Concepts.FireMonkey.Form;

{$REGION 'construction and destruction'}
procedure TfrmFMXContainer.AfterConstruction;
begin
  inherited AfterConstruction;
  FFMXContainer                  := TFireMonkeyContainer.Create(Self);
  FFMXContainer.Parent           := pnlFMXContainer;
  FFMXContainer.Align            := alClient;
  FFMXContainer.OnCreateFMXForm  := FFMXContainerCreateFMXForm;
  FFMXContainer.OnDestroyFMXForm := FFMXContainerDestroyFMXForm;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmFMXContainer.FFMXContainerCreateFMXForm(
  var Form: FMX.Forms.TCommonCustomForm);
begin
  Form := TFireMonkeyForm.Create(Self);
end;

procedure TfrmFMXContainer.FFMXContainerDestroyFMXForm(
  var Form: FMX.Forms.TCommonCustomForm; var Action: TCloseHostedFMXFormAction);
begin
  Action := fcaFree;
end;
{$ENDREGION}

end.
