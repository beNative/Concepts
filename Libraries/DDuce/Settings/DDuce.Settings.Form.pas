{
  Copyright (C) 2013-2021 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.Settings.Form;

interface

{ TFormSettings is a general purpose data storage for form settings. }

uses
  System.Classes,
  Vcl.Forms, Vcl.Controls,

  Spring;

type
  TFormSettings = class(TPersistent)
  private
    FOnChanged   : Event<TNotifyEvent>;
    FWidth       : Integer;
    FHeight      : Integer;
    FLeft        : Integer;
    FTop         : Integer;
    FFormStyle   : TFormStyle;
    FWindowState : TWindowState;

  protected
    {$REGION 'property access methods'}
    procedure SetFormStyle(AValue: TFormStyle);
    procedure SetHeight(AValue: Integer);
    procedure SetLeft(AValue: Integer);
    procedure SetTop(AValue: Integer);
    procedure SetWidth(AValue: Integer);
    procedure SetWindowState(AValue: TWindowState);
    function GetOnChanged: IEvent<TNotifyEvent>;
    {$ENDREGION}

    procedure DoChanged;

  public
    procedure AfterConstruction; override;

    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Source: TPersistent); override;

    property OnChanged: IEvent<TNotifyEvent>
      read GetOnChanged;

  published
    property Left: Integer
      read FLeft write SetLeft;

    property Top: Integer
      read FTop write SetTop;

    property Width: Integer
      read FWidth write SetWidth;

    property Height: Integer
      read FHeight write SetHeight;

    property FormStyle: TFormStyle
      read FFormStyle write SetFormStyle;

    property WindowState: TWindowState
      read FWindowState write SetWindowState;
  end;

implementation

{$REGION 'construction and destruction'}
procedure TFormSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FOnChanged.UseFreeNotification := False;
  FWidth  := 800;
  FHeight := 600;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
procedure TFormSettings.SetWindowState(AValue: TWindowState);
begin
  if (AValue <> WindowState) and (AValue <> wsMinimized) then
  begin
    FWindowState := AValue;
    DoChanged;
  end;
end;

procedure TFormSettings.SetFormStyle(AValue: TFormStyle);
begin
  if FFormStyle <> AValue then
  begin
    FFormStyle := AValue;
    DoChanged;
  end;
end;

procedure TFormSettings.SetHeight(AValue: Integer);
begin
  if Height <> AValue then
  begin
    FHeight := AValue;
    DoChanged;
  end;
end;

procedure TFormSettings.SetLeft(AValue: Integer);
begin
  if Left <> AValue then
  begin
    FLeft := AValue;
    DoChanged;
  end;
end;

procedure TFormSettings.SetTop(AValue: Integer);
begin
  if Top <> AValue then
  begin
    FTop := AValue;
    DoChanged;
  end;
end;

procedure TFormSettings.SetWidth(AValue: Integer);
begin
  if Width <> AValue then
  begin
    FWidth := AValue;
    DoChanged;
  end;
end;

function TFormSettings.GetOnChanged: IEvent<TNotifyEvent>;
begin
  Result := FOnChanged;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TFormSettings.DoChanged;
begin
  FOnChanged.Invoke(Self);
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TFormSettings.Assign(Source: TPersistent);
var
  F  : TForm;
  FS : TFormSettings;
begin
  if Source is TForm then
  begin
    F           := TForm(Source);
    Left        := F.Left;
    Top         := F.Top;
    Width       := F.Width;
    Height      := F.Height;
    FormStyle   := F.FormStyle;
    WindowState := F.WindowState;
  end
  else if Source is TFormSettings then
  begin
    FS          := TFormSettings(Source);
    Left        := FS.Left;
    Top         := FS.Top;
    Width       := FS.Width;
    Height      := FS.Height;
    FormStyle   := FS.FormStyle;
    WindowState := FS.WindowState;
  end
  else
    inherited Assign(Source);
end;

procedure TFormSettings.AssignTo(Dest: TPersistent);
var
  F  : TForm;
  FS : TFormSettings;
begin
  if Dest is TForm then
  begin
    F             := TForm(Dest);
    F.Left        := Left;
    F.Top         := Top;
    F.Width       := Width;
    F.Height      := Height;
    F.FormStyle   := FormStyle;
    F.WindowState := WindowState;
  end
  else if Dest is TFormSettings then
  begin
    FS             := TFormSettings(Dest);
    FS.Left        := Left;
    FS.Top         := Top;
    FS.Width       := Width;
    FS.Height      := Height;
    FS.FormStyle   := FormStyle;
    FS.WindowState := WindowState;
  end
  else
    inherited AssignTo(Dest);
end;
{$ENDREGION}

end.

