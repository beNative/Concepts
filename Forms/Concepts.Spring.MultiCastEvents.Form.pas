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

unit Concepts.Spring.MultiCastEvents.Form;

{ Form demonstrating Spring multicast events (IEvent<T/Event<T>. }

interface

uses
  System.Classes, System.Actions,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Forms, Vcl.ComCtrls, Vcl.Buttons, Vcl.ExtCtrls,
  Vcl.ActnList,

  Concepts.Spring.MultiCastEvents.Data;

type
  TfrmMulticastEvents = class(TForm)
    lblImageIndex : TLabel;
    pnlImageIndex : TPanel;
    pbrPosition   : TProgressBar;
    aclMain       : TActionList;
    actExecute    : TAction;
    btnExecute    : TButton;
    trbImageIndex : TTrackBar;

    procedure actExecuteExecute(Sender: TObject);
    procedure trbImageIndexChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

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

  Concepts.Spring.MultiCastEvents.ChildForm;

{$REGION 'construction and destruction'}
procedure TfrmMulticastEvents.AfterConstruction;
begin
  inherited AfterConstruction;
  pbrPosition.Max   := aclMain.Images.Count;
  trbImageIndex.Max := aclMain.Images.Count;
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
  pnlImageIndex.Caption  := FPosition.Position.ToString;
  pbrPosition.Position   := FPosition.Position;
  actExecute.ImageIndex  := FPosition.Position;
  trbImageIndex.Position := FPosition.Position;
end;

procedure TfrmMulticastEvents.trbImageIndexChange(Sender: TObject);
begin
  FPosition.Position := trbImageIndex.Position;
end;

procedure TfrmMulticastEvents.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmMulticastEvents.actExecuteExecute(Sender: TObject);
var
  F : TfrmMulticastEventsChild;
begin
  F := TfrmMulticastEventsChild.Create(Self, FPosition);
  F.Show;
end;
{$ENDREGION}

end.

