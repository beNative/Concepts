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

unit DDuce.Factories.ToolBar;

interface

uses
  System.Classes,
  Vcl.ComCtrls, Vcl.Controls, Vcl.Menus;

type
  TToolBarFactory = class sealed
    class procedure CleanupToolBar(AToolBar: TToolBar);

    class function CreateToolButton(
       AParent    : TToolBar;
       AAction    : TBasicAction;
       APopupMenu : TPopupMenu = nil
    ): TToolButton; overload;

    class function CreateToolBar(
      AOwner  : TComponent;
      AParent : TWinControl
    ): TToolBar;
  end;

implementation

uses
  Spring,

  DDuce.Logger;

class procedure TToolBarFactory.CleanupToolBar(AToolBar: TToolBar);
var
  I  : Integer;
  TB : TToolButton;
  D  : Boolean; // last was divider or seperator
  B  : Boolean;
  J  : Integer;
begin
  Guard.CheckNotNull(AToolBar, 'AToolBar');
  I := 0;
  J := 0;
  while I < AToolBar.ButtonCount do
  begin
    TB := AToolBar.Buttons[I];
    B  := True;
    while B and (not TB.Visible) do
    begin
      if I < AToolBar.ButtonCount then
      begin
        TB := AToolBar.Buttons[I];
      end
      else
      begin
        B := False;
      end;
      Inc(I);
    end;
    if B then // first visible
    begin
      Logger.Send('I', I);
      Logger.Send('J', J);
      TB := AToolBar.Buttons[I];
      Logger.Send(TB.Caption, TB.Visible);
      D := (TB.Visible and (TB.Style in [tbsSeparator, tbsDivider])) or (not TB.Visible);
      J := 0;
      while B and D do
      begin
        if TB.Style in [tbsSeparator, tbsDivider] then
        begin
          TB.Visible := J = 0;
        end;
        if I < AToolBar.ButtonCount then
        begin
          TB := AToolBar.Buttons[I];
          D := (TB.Visible and (TB.Style in [tbsSeparator, tbsDivider])) or (not TB.Visible);
        end
        else
        begin
          B := False;
        end;
        Inc(I);
        if TB.Style in [tbsSeparator, tbsDivider] then
          Inc(J);
      end;
      if B then
      begin
        Inc(I);
      end;
    end;
  end;
end;

class function TToolBarFactory.CreateToolBar(AOwner: TComponent;
  AParent: TWinControl): TToolBar;
var
  TB : TToolbar;
begin
  Guard.CheckNotNull(AOwner, 'AOwner');
  Guard.CheckNotNull(AParent, 'AParent');
  TB := TToolBar.Create(AOwner);
  TB.Parent := AParent;
  //TB.Images := FActions.ActionList.Images;
  Result := TB;
end;

class function TToolBarFactory.CreateToolButton(AParent: TToolBar;
  AAction: TBasicAction; APopupMenu: TPopupMenu): TToolButton;
var
  TB: TToolButton;
begin
  TB := TToolButton.Create(AParent);
  TB.Parent := AParent;
  if not Assigned(AAction) then
  begin
    TB.Style := tbsDivider;
  end
  else
  begin
    if Assigned(APopupMenu) then
    begin
      TB.Style        := tbsDropDown;
      TB.DropdownMenu := APopupMenu;
    end;
    TB.Action := AAction;
  end;
  Result := TB;
end;

end.
