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

unit Concepts.Spring.MultiCastEvents.Form;

{ Form demonstrating Spring multicast events (IEvent<T/Event<T>. }

interface

uses
  System.Classes,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Forms,

  Concepts.Spring.MultiCastEvents.Data, Vcl.ComCtrls, Vcl.Buttons, Vcl.ExtCtrls,
  System.Actions, Vcl.ActnList;

type
  TfrmMulticastEvents = class(TForm)
    btnTriggerChangeEvent: TButton;
    lblImageIndex: TLabel;
    pnlImageIndex: TPanel;
    pbrPosition: TProgressBar;
    aclMain: TActionList;
    actExecute: TAction;
    btnExecute: TButton;
    procedure actExecuteExecute(Sender: TObject);

  private
    FPosition: TPosition;

    procedure FPositionOnChange(Sender: TObject);

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;



    property Position: TPosition
      read FPosition;

  end;

implementation

{$R *.dfm}

uses
  System.SysUtils,
  Vcl.Dialogs,

  Concepts.Resources, Concepts.Spring.MultiCastEvents.ChildForm;

{$REGION 'construction and destruction'}
procedure TfrmMulticastEvents.AfterConstruction;
begin
  inherited AfterConstruction;
  pbrPosition.Max := aclMain.Images.Count;
  FPosition := TPosition.Create;
  FPosition.OnChange.Add(FPositionOnChange);
end;

procedure TfrmMulticastEvents.BeforeDestruction;
begin
  FPosition.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmMulticastEvents.FPositionOnChange(Sender: TObject);
begin
  pnlImageIndex.Caption := FPosition.Position.ToString;
  pbrPosition.Position  := FPosition.Position;
  actExecute.ImageIndex := FPosition.Position;
end;
{$ENDREGION}


procedure TfrmMulticastEvents.actExecuteExecute(Sender: TObject);
var
  F : TfrmMulticastEventsChild;
begin
  F := TfrmMulticastEventsChild.Create(Self, FPosition);
  F.Show;
end;

end.

