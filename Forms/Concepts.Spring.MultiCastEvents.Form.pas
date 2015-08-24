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

unit Concepts.Spring.MultiCastEvents.Form;

{ Form demonstrating Spring multicast events (IEvent<T/Event<T>. }

interface

uses
  System.Classes,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Forms,

  Concepts.Spring.MultiCastEvents.Data;

type
  TfrmMulticastEvents = class(TForm)
    btnTriggerChangeEvent: TButton;

    procedure btnTriggerChangeEventClick(Sender: TObject);

  private
    FTestComponent: TTestComponent;

    procedure TestComponentChange1(Sender: TObject);
    procedure TestComponentChange2(Sender: TObject);

  public
    procedure AfterConstruction; override;

    property TestComponent: TTestComponent
      read FTestComponent;

  end;

implementation

{$R *.dfm}

uses
  Vcl.Dialogs;

{$REGION 'construction and destruction'}
procedure TfrmMulticastEvents.AfterConstruction;
begin
  inherited AfterConstruction;
  FTestComponent := TTestComponent.Create(Self);
  FTestComponent.OnChange.Add(TestComponentChange1);
  FTestComponent.OnChange.Add(TestComponentChange2);
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmMulticastEvents.btnTriggerChangeEventClick(Sender: TObject);
begin
  TestComponent.Change;
end;

procedure TfrmMulticastEvents.TestComponentChange1(Sender: TObject);
begin
  ShowMessage('TestComponent.OnChange -> TestComponentChange1')
end;

procedure TfrmMulticastEvents.TestComponentChange2(Sender: TObject);
begin
  ShowMessage('TestComponent.OnChange -> TestComponentChange2')
end;
{$ENDREGION}

end.

