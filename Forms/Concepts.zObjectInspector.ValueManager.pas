{
  Copyright (C) 2013-2020 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Concepts.zObjectInspector.ValueManager;

interface

{ This class allows us to customize the property editors of the object
  inspector instance. }

uses
  System.Classes, System.Rtti, System.Types,
  Vcl.Graphics,

  zObjInspTypes, zValueManager;

type
  TValueManager = class(TzCustomValueManager)

    /// <summary> Use custom ListBox .
    /// </summary>
    //class function GetListClass(const PItem: PPropItem): TPopupListClass; override;
    procedure SetValue(const PItem: PPropItem; var Value: TValue); override;
///    class function GetValue(const PItem: PPropItem; const Value): TValue; override;
//    class function GetValueName(const PItem: PPropItem): string; override;
    /// <summary> Check if item can assign value that is not listed in ListBox .
    /// </summary>
    //class function ValueHasOpenProbabilities(const PItem: PPropItem): Boolean; override;
    /// <summary> Check if value has an ExtraRect like (Color,Boolean)type .
    /// </summary>
    /// <returns> non zero to indicate that value must use an ExtraRect .
    /// </returns>
    //class function GetExtraRectWidth(const PItem: PPropItem): Integer; override;
    //class function GetValueType(const PItem: PPropItem): Integer; override;
    /// <summary> Paint item value name .
    /// </summary>
    //class procedure PaintValue(Canvas: TCanvas; Index: Integer; const PItem: PPropItem; R: TRect); override;
    /// <summary> Check if the current item can have button .
    /// </summary>
    //class function HasButton(const PItem: PPropItem): Boolean; override;
    /// <summary> Check if the current item can drop ListBox .
    /// </summary>
    //class function HasList(const PItem: PPropItem): Boolean; override;
    /// <summary> Check if the current item have customized dialog .
    /// </summary>
    //class function HasDialog(const PItem: PPropItem): Boolean; override;
    /// <summary> Get customized dialog for current item .
    /// </summary>
    function GetDialog(const PItem: PPropItem): TComponentClass; override;
    //class procedure DialogCode(const PItem: PPropItem; Dialog: TComponent; Code: Integer); override;
    /// <summary> Get the value returned after editing from the dialog .
    /// </summary>
    //class function DialogResultValue(const PItem: PPropItem; Dialog: TComponent): TValue; override;
    /// <summary> Return ListBox items for the current item .
    /// </summary>
    //class procedure GetListItems(const PItem: PPropItem; Items: TStrings); override;
    /// <summary> Get the value when the user click the ExtraRect .
    /// </summary>
    //class function GetExtraRectResultValue(const PItem: PPropItem): TValue; override;
  end;

implementation

uses
  Concepts.zObjectInspector.StringsDialog.Form,

  DDuce.Logger;

function TValueManager.GetDialog(const PItem: PPropItem): TComponentClass;
begin
  if GetValueType(PItem) = vtString then
  begin
    Result := TfrmStringsDialog;
  end
  else
    Result := inherited GetDialog(PItem);
end;

procedure TValueManager.SetValue(const PItem: PPropItem;
  var Value: TValue);
begin
  Logger.Track('SetValue');
  Logger.Watch('Value', Value);
  inherited;
end;

end.
