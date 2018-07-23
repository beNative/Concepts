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

unit DDuce.Factories.GridView;

interface

uses
  System.Classes,
  Vcl.Controls,
  Data.DB,

  DDuce.Components.GridView, DDuce.Components.DBGridView,
  DDuce.Components.Inspector;

type
  TGridViewFactory = class sealed
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
  end;

implementation

uses
  System.UITypes,
  Vcl.Graphics;

class function TGridViewFactory.CreateDBGridView(AOwner: TComponent;
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

class function TGridViewFactory.CreateGridView(AOwner: TComponent;
  AParent: TWinControl; const AName: string): TGridView;
var
  GV: TGridView;
begin
  GV := TGridView.Create(AOwner);
  if AName <> '' then
    GV.Name := AName;
  GV.Parent                   := AParent;
  GV.Align                    := alClient;
  GV.Header.Flat              := True;
  GV.Header.Font.Style        := [fsBold];
  GV.Header.GridColor         := True;
  GV.Header.FullSynchronizing := True;
  GV.AlignWithMargins         := True;
  GV.Parent                   := AParent;
  GV.Align                    := alClient;
  GV.CursorKeys               := GV.CursorKeys + [gkReturn];
  GV.GridStyle                := GV.GridStyle - [gsHorzLine];
  GV.ColumnsFullDrag          := True;
  GV.DoubleBuffered           := True;
  GV.CheckBoxes               := True;
  GV.ShowFocusRect            := False;
  GV.CheckStyle               := csFlat;
  GV.ColumnClick              := True;
  Result := GV;
end;

class function TGridViewFactory.CreateInspector(AOwner: TComponent;
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

end.
