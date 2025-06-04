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

{$I DDuce.inc}

unit DDuce.Factories.zObjInspector;

interface

uses
  System.Classes, System.TypInfo,
  Vcl.Controls, Vcl.Forms,

  zObjInspector, zValueManager;

type
  TzObjectInspectorFactory = class sealed
  private class var
    FDefaultBorderStyle      : TBorderStyle;
    FDefaultObjectVisibility : TMemberVisibility;

  public
    class constructor Create;

    class function Create(
      AOwner        : TComponent;
      AParent       : TWinControl;
      AObject       : TObject = nil;
      AValueManager : TzCustomValueManager = nil;
      const AName   : string = ''
    ): TzObjectInspector; static;

    class property DefaultBorderStyle: TBorderStyle
      read FDefaultBorderStyle write FDefaultBorderStyle default bsNone;

    class property DefaultObjectVisibility: TMemberVisibility
      read FDefaultObjectVisibility write FDefaultObjectVisibility
      default mvPublic;

  end;

implementation

uses
  Vcl.Graphics;

{$REGION 'construction and destruction'}
class constructor TzObjectInspectorFactory.Create;
begin
  FDefaultBorderStyle      := bsNone;
  FDefaultObjectVisibility := mvPublic;
end;

class function TzObjectInspectorFactory.Create(AOwner: TComponent;
  AParent: TWinControl; AObject: TObject; AValueManager: TzCustomValueManager;
  const AName: string): TzObjectInspector;
var
  OI : TzObjectInspector;
begin
  OI                  := TzObjectInspector.Create(AOwner, AValueManager);
  OI.Parent           := AParent;
  OI.Align            := alClient;
  OI.AlignWithMargins := True;
  OI.Name             := AName;
  OI.Component        := AObject;
  OI.ObjectVisibility := DefaultObjectVisibility;
  OI.SplitterPos      := OI.ClientWidth div 2;
  OI.BorderStyle      := DefaultBorderStyle;
  OI.GutterEdgeColor  := clSilver;
  OI.SplitterColor    := clSilver;
  Result := OI;
end;
{$ENDREGION}

end.
