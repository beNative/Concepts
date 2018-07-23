{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.Components.Factories;

interface

uses
  System.Classes,
  Vcl.Controls,
  Data.DB,

  DDuce.Components.PropertyInspector, DDuce.Components.LogTree;

type
  TDDuceComponents = class sealed
    class function CreatePropertyInspector(
      AOwner  : TComponent;
      AParent : TWinControl;
      AObject : TObject = nil
    ): TPropertyInspector; static;

    class function CreateLogTree(
      AOwner      : TComponent;
      AParent     : TWinControl;
      const AName : string = ''
    ): TLogTree; static;

  end;

implementation

uses
  Vcl.Graphics, Vcl.Forms,

  VirtualTrees;

class function TDDuceComponents.CreateLogTree(AOwner: TComponent;
  AParent: TWinControl; const AName: string): TLogTree;
var
  VLT : TLogTree;
begin
  VLT                    := TLogTree.Create(AOwner);
  VLT.AlignWithMargins   := True;
  VLT.BorderStyle        := bsNone;
  VLT.Parent             := AParent;
  VLT.Align              := alClient;
  VLT.ShowImages         := True;
  VLT.Header.Options     := VLT.Header.Options + [hoAutoSpring];
  Result := VLT;
end;

class function TDDuceComponents.CreatePropertyInspector(AOwner: TComponent;
  AParent: TWinControl; AObject: TObject): TPropertyInspector;
var
  PI : TPropertyInspector;
begin
  PI                  := TPropertyInspector.Create(AOwner);
  PI.AlignWithMargins := True;
  PI.Parent           := AParent;
  PI.Color            := clWhite;
  PI.BorderStyle      := bsSingle;
  PI.PropKinds        := PI.PropKinds + [pkReadOnly];
  PI.Align            := alClient;
  PI.Splitter         := PI.Width div 2;
  if Assigned(AObject) then
  begin
    PI.Add(AObject);
    PI.UpdateItems;
  end;
  Result := PI;
end;

end.
