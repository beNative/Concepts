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

unit DDuce.Editor.ToolView.Manager;

interface

{ TToolViewManager allows for lazy instantiation of registered toolviews. }

uses
  System.Classes, System.SysUtils,
  Vcl.Forms,

  Spring.Collections,

  DDuce.Editor.Interfaces, DDuce.Editor.Tools.Settings,
  DDuce.Editor.ToolView.Base;

type
  TToolView = class(TInterfacedObject, IEditorToolView)
  strict private
    FName          : string;
    FFormClass     : TComponentClass;
    FForm          : TForm;
    FManager       : IEditorManager;
    FSettingsClass : TComponentClass;
    FToolView      : IEditorToolView;

  strict protected
    function GetForm: TForm;
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    procedure SetFocus;
    function Focused: Boolean;
    function GetName: string;

  public
    constructor Create(
            AManager       : IEditorManager;
            AFormClass     : TComponentClass;
            ASettingsClass : TComponentClass;
      const AName          : string
    );
    procedure BeforeDestruction; override;

    property Name: string
      read GetName;

    { Lets the view respond to changes. }
    procedure UpdateView;

    property Form: TForm
      read GetForm;

    property FormClass: TComponentClass
      read FFormClass write FFormClass;

    property SettingsClass: TComponentClass
      read FSettingsClass write FSettingsClass;

    property Visible: Boolean
      read GetVisible write SetVisible;
  end;

  { TToolViews }

  TToolViews = class(TInterfacedObject, IEditorToolViews)
  strict private
    FItems   : IList<IEditorToolView>;
    FManager : IEditorManager;

  strict protected
    function GetView(AIndex: Integer): IEditorToolView;
    function GetViewByName(AName: string): IEditorToolView;
    function GetCount: Integer;

    function GetEnumerator: TEditorToolViewListEnumerator;

    function Register(
            AFormClass     : TComponentClass;
            ASettingsClass : TComponentClass;
      const AName          : string = ''
    ): Boolean;

    procedure Hide;

    property Views[AIndex: Integer]: IEditorToolView
      read GetView;

    property ViewByName[AName: string]: IEditorToolView
      read GetViewByName; default;

    property Count: Integer
      read GetCount;

  public
    constructor Create(AEditorManager: IEditorManager);

  end;

implementation

uses
  System.StrUtils,

  DDuce.Logger;

{$REGION 'TToolView'}
{$REGION 'construction and destruction'}
constructor TToolView.Create(AManager: IEditorManager;
  AFormClass: TComponentClass; ASettingsClass: TComponentClass;
  const AName: string);
begin
  inherited Create;
  FManager       := AManager;
  FFormClass     := AFormClass;
  FSettingsClass := ASettingsClass;
  FName          := AName;

//  if Assigned(ASettingsClass) then
//    FManager.Settings.ToolSettings.RegisterSettings(
//      ASettingsClass,
//      ASettingsClass.ClassName
//    );
end;

procedure TToolView.BeforeDestruction;
begin
  FManager := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TToolView.GetForm: TForm;
begin
  if not Assigned(FForm) then
  begin
    FForm := FFormClass.Create(
      (FManager as IInterfaceComponentReference).GetComponent
    ) as TForm;
    Logger.Send('Created ' + FForm.Name);
  end;
  Result := FForm;
end;

function TToolView.GetVisible: Boolean;
begin
  Result := Assigned(FForm) and FForm.Visible;
end;

procedure TToolView.SetVisible(AValue: Boolean);
begin
  if AValue <> Visible then
  begin
    if not AValue and Assigned(FForm) then
    begin
      FForm.Visible := False
    end
    else
      Form.Visible := AValue;
  end;
end;

procedure TToolView.SetFocus;
begin
  if Assigned(FForm) and FForm.Visible and FForm.CanFocus then
    FForm.SetFocus;
end;

function TToolView.GetName: string;
begin
  Result := FName;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TToolView.UpdateView;
begin
  if Assigned(FToolView) then
    FToolView.UpdateView;
end;

function TToolView.Focused: Boolean;
begin
  Result := Assigned(FForm) and FForm.Focused;
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'TToolViews'}
{$REGION 'construction and destruction'}
constructor TToolViews.Create(AEditorManager: IEditorManager);
begin
  inherited Create;
  FManager := AEditorManager;
  FItems   := TCollections.CreateInterfaceList<IEditorToolView>;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TToolViews.GetView(AIndex: Integer): IEditorToolView;
begin
  Result := FItems[AIndex] as IEditorToolView;
end;

function TToolViews.GetViewByName(AName: string): IEditorToolView;
var
  TV : IEditorToolView;
  I  : Integer;
begin
  I := 0;
  Result := nil;
  while (I < FItems.Count) and not Assigned(Result) do
  begin
    TV := Views[I];
    if TV.Name = AName then
      Result := TV;
    Inc(I);
  end;
end;

function TToolViews.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TToolViews.GetEnumerator: TEditorToolViewListEnumerator;
begin
  Result := TEditorToolViewListEnumerator.Create(Self);
end;
{$ENDREGION}

{$REGION 'protected methods'}
function TToolViews.Register(AFormClass: TComponentClass;
  ASettingsClass: TComponentClass; const AName: string): Boolean;
var
  S  : string;
  TV : IEditorToolView;
begin
  S  := IfThen(AName = '', AFormClass.ClassName, AName);
  TV := TToolView.Create(FManager, AFormClass, ASettingsClass, S);
  FItems.Add(TV);
  Result := True;
end;

procedure TToolViews.Hide;
var
  TV: IEditorToolView;
begin
  for TV in (Self as IEditorToolViews) do
  begin
    TV.Visible := False;
    FManager.Events.DoHideToolView(TV);
  end;
end;
{$ENDREGION}
{$ENDREGION}

end.

