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

unit DDuce.FormSettings;

interface

uses
  System.Classes,
  Vcl.Forms, Vcl.Controls;

type
  TFormSettings = class(TPersistent)
  private
    FOnChanged   : TNotifyEvent;
    FWidth       : Integer;
    FHeight      : Integer;
    FLeft        : Integer;
    FTop         : Integer;
    FFormStyle   : TFormStyle;
    FWindowState : TWindowState;

    procedure SetFormStyle(AValue: TFormStyle);
    procedure SetHeight(AValue: Integer);
    procedure SetLeft(AValue: Integer);
    procedure SetTop(AValue: Integer);
    procedure SetWidth(AValue: Integer);
    procedure SetWindowState(AValue: TWindowState);

  protected
    procedure Changed;

  public
    procedure AfterConstruction; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Source: TPersistent); override;

    property OnChanged: TNotifyEvent
      read FOnChanged write FOnChanged;

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

{$region 'construction and destruction' /fold}

procedure TFormSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FWidth  := 800;
  FHeight := 600;
end;

{$endregion}

{$region 'property access mehods' /fold}

procedure TFormSettings.SetWindowState(AValue: TWindowState);
begin
  if (AValue <> WindowState) and (AValue <> wsMinimized) then
  begin
    FWindowState := AValue;
    Changed;
  end;
end;

procedure TFormSettings.SetFormStyle(AValue: TFormStyle);
begin
  if FFormStyle = AValue then Exit;
  FFormStyle := AValue;
  Changed;
end;

procedure TFormSettings.SetHeight(AValue: Integer);
begin
  if FHeight = AValue then Exit;
  FHeight := AValue;
  Changed;
end;

procedure TFormSettings.SetLeft(AValue: Integer);
begin
  if FLeft = AValue then Exit;
  FLeft := AValue;
  Changed;
end;

procedure TFormSettings.SetTop(AValue: Integer);
begin
  if FTop = AValue then Exit;
  FTop := AValue;
  Changed;
end;

procedure TFormSettings.SetWidth(AValue: Integer);
begin
  if FWidth = AValue then Exit;
  FWidth := AValue;
  Changed;
end;

{$endregion}

{$region 'protected methods' /fold}

procedure TFormSettings.Changed;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

{$endregion}

{$region 'public methods' /fold}

procedure TFormSettings.Assign(Source: TPersistent);
var
  F : TForm;
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
  else
    inherited;
end;

procedure TFormSettings.AssignTo(Dest: TPersistent);
var
  F : TForm;
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
  else
    inherited;
end;

{$endregion}

end.
