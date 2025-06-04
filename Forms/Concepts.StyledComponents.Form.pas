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

unit Concepts.StyledComponents.Form;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls,

  zObjInspector, zObjInspTypes, Vcl.ButtonStylesAttributes, Vcl.StyledButton


  ;

type
  TfrmStyledComponents = class(TForm)
    splVertical     : TSplitter;
    sbrMain         : TStatusBar;
    pnlLeft         : TPanel;
    pnlMain         : TPanel;
    pnlHeader       : TPanel;
    btnStyledButton : TStyledButton;
    procedure FormShow(Sender: TObject);

  private
    FObjectInspector : TzObjectInspector;

    function FObjectInspectorBeforeAddItem(
      Sender : TControl;
      PItem  : PPropItem
    ): Boolean;

  public
    procedure AfterConstruction; override;

  end;

implementation

uses
  System.TypInfo, System.Rtti,

  DDuce.Factories.zObjInspector;

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TfrmStyledComponents.AfterConstruction;
begin
  inherited AfterConstruction;
  FObjectInspector := TzObjectInspectorFactory.Create(Self, pnlLeft);
  FObjectInspector.OnBeforeAddItem := FObjectInspectorBeforeAddItem;
  FObjectInspector.ObjectVisibility := mvPublished;
  FObjectInspector.Font.Size := 10;
  FObjectInspector.Component := btnStyledButton;
  FObjectInspector.Font.Size := 9;
end;
{$ENDREGION}

{$REGION 'event handlers'}
function TfrmStyledComponents.FObjectInspectorBeforeAddItem(Sender: TControl;
  PItem: PPropItem): Boolean;
begin
  Result := not PItem.Name.Contains('ComObject');
  Result := Result and (not (PItem.Prop.PropertyType is TRttiMethodType));
end;

procedure TfrmStyledComponents.FormShow(Sender: TObject);
begin
end;

{$ENDREGION}

end.
