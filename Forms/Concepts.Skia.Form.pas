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

unit Concepts.Skia.Form;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Skia,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Skia,

  zObjInspector, zObjInspTypes;

type
  TfrmSkia = class(TForm)
    pnlMain      : TPanel;
    splSplitter  : TSplitter;
    pnlLeft      : TPanel;
    pnlRight     : TPanel;
    sbrStatusBar : TStatusBar;
    pnlHeader    : TPanel;
    lblHeader    : TLabel;
    lblText      : TSkLabel;
    imgLottie    : TSkAnimatedImage;
    imgLottie1   : TSkAnimatedImage;
    lblSkia      : TSkLabel;

  private
    FObjectInspector : TzObjectInspector;

    function FObjectInspectorBeforeAddItem(
      Sender : TControl;
      PItem  : PPropItem
    ): Boolean;

    procedure PopulateLabel;

  public
    procedure AfterConstruction; override;

  end;

implementation

uses
  System.TypInfo, System.Rtti, System.UITypes,

  DDuce.Factories.zObjInspector;

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TfrmSkia.AfterConstruction;
begin
  inherited AfterConstruction;
  FObjectInspector := TzObjectInspectorFactory.Create(Self, pnlLeft);
  FObjectInspector.OnBeforeAddItem := FObjectInspectorBeforeAddItem;
  FObjectInspector.ObjectVisibility := mvPublic;
  FObjectInspector.Component := lblSkia;

  PopulateLabel;
end;
{$ENDREGION}

{$REGION 'event handlers'}
function TfrmSkia.FObjectInspectorBeforeAddItem(Sender: TControl;
  PItem: PPropItem): Boolean;
var
  LName : string;
begin
  LName := PItem.QualifiedName;
  LName := LName.Split(['.'], 2)[1];
  Result := not LName.Contains('ComObject')
    and (not (PItem.Prop.PropertyType is TRttiMethodType));
end;

procedure TfrmSkia.PopulateLabel;
begin
  lblSkia.Words.Add(' Red ', TAlphaColors.Red, 14, TSkFontComponent.TSkFontWeight.Bold);
  lblSkia.Words.Add('Green ', TAlphaColors.Green, 14, TSkFontComponent.TSkFontWeight.Bold);
  lblSkia.Words.Add('Blue', TAlphaColors.Blue, 14, TSkFontComponent.TSkFontWeight.Bold);
end;

{$ENDREGION}

end.
