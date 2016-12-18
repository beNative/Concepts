{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.Components.Factories;

{$I ..\DDuce.inc}

interface

uses
  System.Classes,
  Vcl.Controls,
  Data.DB,

  DDuce.Components.GridView, DDuce.Components.DBGridView,
  DDuce.Components.Inspector, DDuce.Components.PropertyInspector,
  DDuce.Components.LogTree;

type
  TDDuceComponents = class sealed
    class function CreateGridView(
      AOwner      : TComponent;
      AParent     : TWinControl;
      const AName : string = ''
    ): TGridView; static;

    class function CreateDBGridView(
      AOwner      : TComponent;
      AParent     : TWinControl;
      ADataSource : TDataSource = nil;
      const AName : string = ''
    ): TDBGridView; static;

    class function CreateInspector(
      AOwner      : TComponent;
      AParent     : TWinControl;
      const AName : string = ''
    ): TInspector; static;

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

class function TDDuceComponents.CreateGridView(AOwner: TComponent;
  AParent: TWinControl; const AName: string): TGridView;
var
  GV: TGridView;
begin
  GV := TGridView.Create(AOwner);
  if AName <> '' then
    GV.Name := AName;
  GV.Parent                   := AParent;
  GV.Align                    := alClient;
  GV.Header.Flat              := False;
  GV.Header.FullSynchronizing := True;
  GV.AlignWithMargins         := True;
  GV.Parent                   := AParent;
  GV.Align                    := alClient;
  GV.CursorKeys               := GV.CursorKeys + [gkReturn];
  GV.GridStyle                := GV.GridStyle + [gsDotLines];
  GV.ColumnsFullDrag          := True;
  GV.DoubleBuffered           := True;
  GV.CheckBoxes               := True;
  GV.ShowFocusRect            := False;
  GV.CheckStyle               := csFlat;
  GV.ColumnClick              := True;
  Result := GV;
end;

class function TDDuceComponents.CreateDBGridView(AOwner: TComponent;
  AParent: TWinControl; ADataSource: TDataSource;
  const AName: string): TDBGridView;
var
  GV: TDBGridView;
begin
  GV                  := TDBGridView.Create(AOwner);
  GV.Header.Flat      := False;
  GV.AlignWithMargins := True;
  GV.Parent           := AParent;
  GV.Align            := alClient;
  GV.CursorKeys       := GV.CursorKeys + [gkReturn];
  GV.GridStyle        := GV.GridStyle + [gsDotLines];
  GV.ColumnsFullDrag  := True;
  GV.DoubleBuffered   := True;
  GV.CheckBoxes       := True;
  GV.ShowFocusRect    := False;
  GV.CheckStyle       := csFlat;
  GV.ColumnClick      := True;
  GV.ShowIndicator    := True;
  GV.DataSource       := ADataSource;
  GV.AutoSizeCols;
  Result := GV;
end;

class function TDDuceComponents.CreateInspector(AOwner: TComponent;
  AParent: TWinControl; const AName: string): TInspector;
var
  I: TInspector;
begin
  I := TInspector.Create(AOwner);
  if AName <> '' then
    I.Name := AName;
  I.Color  := clWhite;
  I.Parent := AParent;
  I.Align  := alClient;
  Result := I;
end;

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
  VLT.ShowHint           := True;
  VLT.ShowImages         := True;
  VLT.AutoLogLevelColors := True;
  VLT.Header.Options     := VLT.Header.Options + [hoAutoSpring, hoAutoResize];
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
