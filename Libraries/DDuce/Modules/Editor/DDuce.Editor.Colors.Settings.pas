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

unit DDuce.Editor.Colors.Settings;

interface

uses
  System.Classes, System.SysUtils,
  Vcl.Graphics;

const
  DEFAULT_RIGHT_EDGE_COLOR = clSilver;

type
  TSynSelectedColor = TColor;

type
  TEditorColorSettings = class(TPersistent)
  strict private
    FIncrementColor     : TSynSelectedColor;
    FHighlightAllColor  : TSynSelectedColor;
    FBracketMatchColor  : TSynSelectedColor;
    FMouseLinkColor     : TSynSelectedColor;
    FLineHighlightColor : TSynSelectedColor;
    FFoldedCodeColor    : TSynSelectedColor;
    FOnChanged          : TNotifyEvent;
    FSelectedColor      : TSynSelectedColor;
    FRightEdgeColor     : TColor;

    function GetBracketMatchColor: TSynSelectedColor;
    function GetFoldedCodeColor: TSynSelectedColor;
    function GetHighlightAllColor: TSynSelectedColor;
    function GetIncrementColor: TSynSelectedColor;
    function GetLineHighlightColor: TSynSelectedColor;
    function GetMouseLinkColor: TSynSelectedColor;
    function GetRightEdgeColor: TColor;
    function GetSelectedColor: TSynSelectedColor;
    procedure SetBracketMatchColor(AValue: TSynSelectedColor);
    procedure SetFoldedCodeColor(AValue: TSynSelectedColor);
    procedure SetHighlightAllColor(AValue: TSynSelectedColor);
    procedure SetIncrementColor(AValue: TSynSelectedColor);
    procedure SetLineHighlightColor(AValue: TSynSelectedColor);
    procedure SetMouseLinkColor(AValue: TSynSelectedColor);
    procedure SetRightEdgeColor(AValue: TColor);
    procedure SetSelectedColor(AValue: TSynSelectedColor);

  protected
    procedure AssignDefaultColors;
    procedure Changed;

  public
    procedure AfterConstruction; override;
    procedure Assign(ASource: TPersistent); override;
    procedure BeforeDestruction; override;

    property OnChanged: TNotifyEvent
      read FOnChanged write FOnChanged;

  published
    property IncrementColor: TSynSelectedColor
      read GetIncrementColor write SetIncrementColor;

    property HighlightAllColor: TSynSelectedColor
      read GetHighlightAllColor write SetHighlightAllColor;

    property BracketMatchColor: TSynSelectedColor
      read GetBracketMatchColor write SetBracketMatchColor;

    property MouseLinkColor: TSynSelectedColor
      read GetMouseLinkColor write SetMouseLinkColor;

    property LineHighlightColor: TSynSelectedColor
      read GetLineHighlightColor write SetLineHighlightColor;

    property FoldedCodeColor: TSynSelectedColor
      read GetFoldedCodeColor write SetFoldedCodeColor;

    property SelectedColor: TSynSelectedColor
      read GetSelectedColor write SetSelectedColor;

    property RightEdgeColor: TColor
      read GetRightEdgeColor write SetRightEdgeColor
      default DEFAULT_RIGHT_EDGE_COLOR;

  end;

implementation

{$REGION 'construction and destruction'}
procedure TEditorColorSettings.AfterConstruction;
begin
  inherited AfterConstruction;
//  FIncrementColor     := TSynSelectedColor.Create;
//  FHighlightAllColor  := TSynSelectedColor.Create;
//  FBracketMatchColor  := TSynSelectedColor.Create;
//  FMouseLinkColor     := TSynSelectedColor.Create;
//  FLineHighlightColor := TSynSelectedColor.Create;
//  FFoldedCodeColor    := TSynSelectedColor.Create;
//  FSelectedColor      := TSynSelectedColor.Create;
  AssignDefaultColors;
end;

procedure TEditorColorSettings.BeforeDestruction;
begin
//  FSelectedColor.Free;
//  FIncrementColor.Free;
//  FHighlightAllColor.Free;
//  FBracketMatchColor.Free;
//  FMouseLinkColor.Free;
//  FLineHighlightColor.Free;
//  FFoldedCodeColor.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TEditorColorSettings.GetBracketMatchColor: TSynSelectedColor;
begin
  Result := FBracketMatchColor;
end;

procedure TEditorColorSettings.SetBracketMatchColor(AValue: TSynSelectedColor);
begin
  //FBracketMatchColor.Assign(AValue);
  Changed;
end;

function TEditorColorSettings.GetFoldedCodeColor: TSynSelectedColor;
begin
  Result := FFoldedCodeColor;
end;

procedure TEditorColorSettings.SetFoldedCodeColor(AValue: TSynSelectedColor);
begin
  //FFoldedCodeColor.Assign(AValue);
  Changed;
end;

function TEditorColorSettings.GetHighlightAllColor: TSynSelectedColor;
begin
  Result := FHighlightAllColor;
end;

procedure TEditorColorSettings.SetHighlightAllColor(AValue: TSynSelectedColor);
begin
  //FHighlightAllColor.Assign(AValue);
  Changed;
end;

function TEditorColorSettings.GetIncrementColor: TSynSelectedColor;
begin
  Result := FIncrementColor;
end;

procedure TEditorColorSettings.SetIncrementColor(AValue: TSynSelectedColor);
begin
  //FIncrementColor.Assign(AValue);
  Changed;
end;

function TEditorColorSettings.GetLineHighlightColor: TSynSelectedColor;
begin
  Result := FLineHighlightColor;
end;

procedure TEditorColorSettings.SetLineHighlightColor(AValue: TSynSelectedColor);
begin
  //FLineHighlightColor.Assign(AValue);
  Changed;
end;

function TEditorColorSettings.GetMouseLinkColor: TSynSelectedColor;
begin
  Result := FMouseLinkColor;
end;

function TEditorColorSettings.GetRightEdgeColor: TColor;
begin
  Result := FRightEdgeColor;
end;

procedure TEditorColorSettings.SetRightEdgeColor(AValue: TColor);
begin
  if AValue <> RightEdgeColor then
  begin
    FRightEdgeColor := AValue;
    Changed;
  end;
end;

procedure TEditorColorSettings.SetMouseLinkColor(AValue: TSynSelectedColor);
begin
  //FMouseLinkColor.Assign(AValue);
  Changed;
end;

function TEditorColorSettings.GetSelectedColor: TSynSelectedColor;
begin
  Result := FSelectedColor;
end;

procedure TEditorColorSettings.SetSelectedColor(AValue: TSynSelectedColor);
begin
  //FSelectedColor.Assign(AValue);
  Changed;
end;
{$ENDREGION}

procedure TEditorColorSettings.AssignDefaultColors;
begin
//  BracketMatchColor.Background := clAqua;
//  BracketMatchColor.Foreground := clNone;
//  BracketMatchColor.FrameColor := clBlue;
//
//  SelectedColor.Background := clMedGray;
//  SelectedColor.BackAlpha  := 128;
//  SelectedColor.Foreground := clNone;
//
//  IncrementColor.Background := clMedGray;
//  IncrementColor.BackAlpha  := 128;
//  IncrementColor.Foreground := clNone;
//
//  HighlightAllColor.Background := $000080FF; // orange
//  HighlightAllColor.BackAlpha  := 128;
//  HighlightAllColor.Foreground := clNone;
//  HighlightAllColor.FrameColor := $00006BD7; // dark orange
//
//  LineHighlightColor.Background := clYellow;
//  LineHighlightColor.BackAlpha  := 128;
//  LineHighlightColor.Foreground := clNone;
//  LineHighlightColor.FrameColor := clOlive;
//  LineHighlightColor.FrameAlpha := 64;
//  LineHighlightColor.FrameStyle := slsDashed;
//
//  FoldedCodeColor.Background := clSilver;
//  FoldedCodeColor.BackAlpha  := 50;
//  FoldedCodeColor.Foreground := clMedGray;
//  FoldedCodeColor.FrameColor := clMedGray;

  RightEdgeColor := DEFAULT_RIGHT_EDGE_COLOR;
end;

procedure TEditorColorSettings.Changed;
begin
  if Assigned(OnChanged) then
    FOnChanged(Self);
end;

{$REGION 'public methods'}
procedure TEditorColorSettings.Assign(ASource: TPersistent);
var
  ECS: TEditorColorSettings;
begin
  if ASource is TEditorColorSettings then
  begin
    ECS := TEditorColorSettings(ASource);
    SelectedColor      := ECS.SelectedColor;
    MouseLinkColor     := ECS.MouseLinkColor;
    LineHighlightColor := ECS.LineHighlightColor;
    IncrementColor     := ECS.IncrementColor;
    HighlightAllColor  := ECS.HighlightAllColor;
    FoldedCodeColor    := ECS.FoldedCodeColor;
    BracketMatchColor  := ECS.BracketMatchColor;
    RightEdgeColor     := ECS.RightEdgeColor;
  end
  else
    inherited Assign(ASource);
end;
{$ENDREGION}

end.

