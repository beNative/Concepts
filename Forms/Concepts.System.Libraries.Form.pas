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

unit Concepts.System.Libraries.Form;

interface

{
  Form demonstrating the methods of how to use external libraries (DLL's)
    - load-time dynamic linking ('static linking')
    - run-time dynamic linking ('dynamic linking')
}

uses
  Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.ActnList,
  System.Bluetooth, System.Classes, System.Bluetooth.Components, System.Actions;

type
  TfrmLibraries = class(TForm)
    aclMain              : TActionList;
    actStartDiscoverable : TAction;
    bltBluetooth         : TBluetooth;
    chkEnabled           : TCheckBox;
    actDiscoverDevices   : TAction;
    mmoDevices           : TMemo;
    btnDiscoverDevices   : TButton;
    btnStartDiscoverable : TButton;

    procedure bltBluetoothRemoteRequestPair(const ADevice: TBluetoothDevice);
    procedure bltBluetoothDiscoveryEnd(const Sender: TObject;
      const ADeviceList: TBluetoothDeviceList);
    procedure bltBluetoothDiscoverableEnd(const Sender: TObject);
    procedure chkEnabledClick(Sender: TObject);

    procedure actStartDiscoverableExecute(Sender: TObject);
    procedure actDiscoverDevicesExecute(Sender: TObject);

  public
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TfrmLibraries.AfterConstruction;
begin
  inherited AfterConstruction;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TfrmLibraries.bltBluetoothDiscoverableEnd(const Sender: TObject);
begin
//
end;

procedure TfrmLibraries.bltBluetoothDiscoveryEnd(const Sender: TObject;
  const ADeviceList: TBluetoothDeviceList);
var
  BTD: TBluetoothDevice;
begin
  mmoDevices.Clear;
  if ADeviceList.Count > 0 then
  begin
    for BTD in ADeviceList do
    begin
      mmoDevices.Lines.Add(BTD.DeviceName);
    end;
  end
  else
  begin
    mmoDevices.Text := 'No devices found';
  end;
end;

procedure TfrmLibraries.bltBluetoothRemoteRequestPair(const ADevice: TBluetoothDevice);
begin
  bltBluetooth.Pair(ADevice);
end;

procedure TfrmLibraries.chkEnabledClick(Sender: TObject);
begin
  bltBluetooth.Enabled := chkEnabled.Checked;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmLibraries.actStartDiscoverableExecute(Sender: TObject);
begin
  bltBluetooth.StartDiscoverable(5000);
end;

procedure TfrmLibraries.actDiscoverDevicesExecute(Sender: TObject);
begin
  mmoDevices.Clear;
  bltBluetooth.DiscoverDevices(5000);
end;
{$ENDREGION}

end.
