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

unit DDuce.Settings.TextFormat;

interface

{ TTextFormatSettings is a general purpose data storage for text style and
  color settings. }

uses
  System.Classes,
  Vcl.Forms, Vcl.Controls, Vcl.Graphics,

  Spring;

type
  TTextFormatSettings = class(TPersistent)
  private
    FOnChanged           : Event<TNotifyEvent>;
    FFont                : TFont;
    FBackgroundColor     : TColor;
    FWordWrap            : Boolean;
    FHorizontalAlignment : TAlignment;
    FVerticalAlignment   : TVerticalAlignment;
    FName                : string;

  protected
    {$REGION 'property access methods'}
    function GetName: string;
    procedure SetName(const Value: string);
    function GetWordWrap: Boolean;
    procedure SetWordWrap(const Value: Boolean);
    function GetHorizontalAlignment: TAlignment;
    function GetVerticalAlignment: TVerticalAlignment;
    procedure SetHorizontalAlignment(const Value: TAlignment);
    procedure SetVerticalAlignment(const Value: TVerticalAlignment);
    function GetFont: TFont;
    function GetFontName: TFontName;
    procedure SetFontName(const Value: TFontName);
    function GetFontSize: Integer;
    function GetFontStyle: TFontStyles;
    procedure SetFontSize(const Value: Integer);
    procedure SetFontStyle(const Value: TFontStyles);
    function GetFontColor: TColor;
    procedure SetFontColor(const Value: TColor);
    function GetBackgroundColor: TColor;
    procedure SetBackgroundColor(const Value: TColor);
    function GetOnChanged: IEvent<TNotifyEvent>;
    {$ENDREGION}

    procedure DoChanged;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Source: TPersistent); override;

    property OnChanged: IEvent<TNotifyEvent>
      read GetOnChanged;

    property Font: TFont
      read GetFont;

    property Name: string
      read GetName write SetName;

  published
    property BackgroundColor: TColor
      read GetBackgroundColor write SetBackgroundColor;

    property FontName: TFontName
      read GetFontName write SetFontName;

    property FontColor: TColor
      read GetFontColor write SetFontColor;

    property FontSize: Integer
      read GetFontSize write SetFontSize;

    property FontStyle: TFontStyles
      read GetFontStyle write SetFontStyle;

    property WordWrap: Boolean
      read GetWordWrap write SetWordWrap;

    property HorizontalAlignment: TAlignment
      read GetHorizontalAlignment write SetHorizontalAlignment;

    property VerticalAlignment: TVerticalAlignment
      read GetVerticalAlignment write SetVerticalAlignment;
  end;

implementation

uses
  System.UITypes;

{$REGION 'construction and destruction'}
procedure TTextFormatSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FFont := TFont.Create;
end;

procedure TTextFormatSettings.BeforeDestruction;
begin
  FFont.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TTextFormatSettings.GetBackgroundColor: TColor;
begin
  Result := FBackgroundColor;
end;

procedure TTextFormatSettings.SetBackgroundColor(const Value: TColor);
begin
  if Value <> BackgroundColor then
  begin
    FBackgroundColor := Value;
    DoChanged;
  end;
end;

function TTextFormatSettings.GetFont: TFont;
begin
  Result := FFont;
end;

function TTextFormatSettings.GetFontColor: TColor;
begin
  Result := FFont.Color;
end;

function TTextFormatSettings.GetFontName: TFontName;
begin
  Result := FFont.Name;
end;

procedure TTextFormatSettings.SetFontName(const Value: TFontName);
begin
  if Value <> FontName then
  begin
    FFont.Name := Value;
    DoChanged;
  end;
end;

procedure TTextFormatSettings.SetFontColor(const Value: TColor);
begin
  if Value <> FontColor then
  begin
    FFont.Color := Value;
    DoChanged;
  end;
end;

function TTextFormatSettings.GetFontSize: Integer;
begin
  Result := FFont.Size;
end;

procedure TTextFormatSettings.SetFontSize(const Value: Integer);
begin
  if Value <> FontSize then
  begin
    FFont.Size := Value;
    DoChanged;
  end;
end;

function TTextFormatSettings.GetFontStyle: TFontStyles;
begin
  Result := FFont.Style;
end;

function TTextFormatSettings.GetHorizontalAlignment: TAlignment;
begin
  Result := FHorizontalAlignment;
end;

function TTextFormatSettings.GetName: string;
begin
  Result := FName;
end;

procedure TTextFormatSettings.SetName(const Value: string);
begin
  if Value <> Name then
  begin
    FName := Value;
    DoChanged;
  end;
end;

procedure TTextFormatSettings.SetFontStyle(const Value: TFontStyles);
begin
  if Value <> FontStyle then
  begin
    FFont.Style := Value;
    DoChanged;
  end;
end;

procedure TTextFormatSettings.SetHorizontalAlignment(const Value: TAlignment);
begin
  if Value <> HorizontalAlignment then
  begin
    FHorizontalAlignment := Value;
    DoChanged;
  end;
end;

function TTextFormatSettings.GetOnChanged: IEvent<TNotifyEvent>;
begin
  Result := FOnChanged;
end;

function TTextFormatSettings.GetVerticalAlignment: TVerticalAlignment;
begin
  Result := FVerticalAlignment;
end;

procedure TTextFormatSettings.SetVerticalAlignment(
  const Value: TVerticalAlignment);
begin
  if Value <> VerticalAlignment then
  begin
    FVerticalAlignment := Value;
    DoChanged;
  end;
end;

function TTextFormatSettings.GetWordWrap: Boolean;
begin
  Result := FWordWrap;
end;

procedure TTextFormatSettings.SetWordWrap(const Value: Boolean);
begin
  if Value <> WordWrap then
  begin
    FWordWrap := Value;
    DoChanged;
  end;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TTextFormatSettings.DoChanged;
begin
  FOnChanged.Invoke(Self);
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TTextFormatSettings.Assign(Source: TPersistent);
var
  TFS : TTextFormatSettings;
begin
  if Source is TTextFormatSettings then
  begin
    TFS                 := TTextFormatSettings(Source);
    BackgroundColor     := TFS.BackgroundColor;
    FontColor           := TFS.FontColor;
    FontSize            := TFS.FontSize;
    FontStyle           := TFS.FontStyle;
    HorizontalAlignment := TFS.HorizontalAlignment;
    VerticalAlignment   := TFS.VerticalAlignment;
    WordWrap            := TFS.WordWrap;
  end
  else if Source is TFont then
  begin
    FFont.Assign(Source);
  end
  else
    inherited Assign(Source);
end;

procedure TTextFormatSettings.AssignTo(Dest: TPersistent);
var
  TFS : TTextFormatSettings;
begin
  if Dest is TTextFormatSettings then
  begin
    TFS                     := TTextFormatSettings(Dest);
    TFS.BackgroundColor     := BackgroundColor;
    TFS.FontColor           := FontColor;
    TFS.FontSize            := FontSize;
    TFS.FontStyle           := FontStyle;
    TFS.HorizontalAlignment := HorizontalAlignment;
    TFS.VerticalAlignment   := VerticalAlignment;
    TFS.WordWrap            := WordWrap;
  end
  else if Dest is TFont then
  begin
    Dest.Assign(FFont);
  end
  else
    inherited AssignTo(Dest);
end;
{$ENDREGION}

end.
