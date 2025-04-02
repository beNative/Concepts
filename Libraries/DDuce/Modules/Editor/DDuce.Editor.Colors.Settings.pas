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

unit DDuce.Editor.Colors.Settings;

interface

uses
  System.Classes, System.SysUtils,
  Vcl.Graphics;

const
  DEFAULT_RIGHT_EDGE_COLOR     = clSilver;
  DEFAULT_LINE_HIGHLIGHT_COLOR = clYellow;
  DEFAULT_HIGHLIGHT_ALL_COLOR  = $000080FF; // orange
  DEFAULT_BRACKET_MATCH_COLOR  = clAqua;
  DEFAULT_MOUSELINK_COLOR      = clBlue;
  DEFAULT_SELECTED_COLOR       = clMedGray;
  DEFAULT_INCREMENT_COLOR      = clMedGray;
  DEFAULT_FOLDED_CODE_COLOR    = clSilver;

type
  TEditorColorSettings = class(TPersistent)
  private
    FIncrementColor     : TColor;
    FHighlightAllColor  : TColor;
    FBracketMatchColor  : TColor;
    FMouseLinkColor     : TColor;
    FLineHighlightColor : TColor;
    FFoldedCodeColor    : TColor;
    FSelectedColor      : TColor;
    FRightEdgeColor     : TColor;
    FOnChanged          : TNotifyEvent;

    {$REGION 'property access methods'}
    function GetBracketMatchColor: TColor;
    function GetFoldedCodeColor: TColor;
    function GetHighlightAllColor: TColor;
    function GetIncrementColor: TColor;
    function GetLineHighlightColor: TColor;
    function GetMouseLinkColor: TColor;
    function GetRightEdgeColor: TColor;
    function GetSelectedColor: TColor;
    procedure SetBracketMatchColor(AValue: TColor);
    procedure SetFoldedCodeColor(AValue: TColor);
    procedure SetHighlightAllColor(AValue: TColor);
    procedure SetIncrementColor(AValue: TColor);
    procedure SetLineHighlightColor(AValue: TColor);
    procedure SetMouseLinkColor(AValue: TColor);
    procedure SetRightEdgeColor(AValue: TColor);
    procedure SetSelectedColor(AValue: TColor);
    {$ENDREGION}

  protected
    procedure AssignDefaultColors;
    procedure Changed;

  public
    procedure AfterConstruction; override;

    procedure Assign(ASource: TPersistent); override;

    property OnChanged: TNotifyEvent
      read FOnChanged write FOnChanged;

  published
    property IncrementColor: TColor
      read GetIncrementColor write SetIncrementColor
      default DEFAULT_INCREMENT_COLOR;

    property HighlightAllColor: TColor
      read GetHighlightAllColor write SetHighlightAllColor
      default DEFAULT_HIGHLIGHT_ALL_COLOR;

    property BracketMatchColor: TColor
      read GetBracketMatchColor write SetBracketMatchColor
      default DEFAULT_BRACKET_MATCH_COLOR;

    property MouseLinkColor: TColor
      read GetMouseLinkColor write SetMouseLinkColor
      default DEFAULT_MOUSELINK_COLOR;

    property LineHighlightColor: TColor
      read GetLineHighlightColor write SetLineHighlightColor
      default DEFAULT_LINE_HIGHLIGHT_COLOR;

    property FoldedCodeColor: TColor
      read GetFoldedCodeColor write SetFoldedCodeColor
      default DEFAULT_FOLDED_CODE_COLOR;

    property SelectedColor: TColor
      read GetSelectedColor write SetSelectedColor
      default DEFAULT_SELECTED_COLOR;

    property RightEdgeColor: TColor
      read GetRightEdgeColor write SetRightEdgeColor
      default DEFAULT_RIGHT_EDGE_COLOR;

  end;

implementation

{$REGION 'construction and destruction'}
procedure TEditorColorSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  AssignDefaultColors;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TEditorColorSettings.GetBracketMatchColor: TColor;
begin
  Result := FBracketMatchColor;
end;

procedure TEditorColorSettings.SetBracketMatchColor(AValue: TColor);
begin
  if AValue <> BracketMatchColor then
  begin
    FBracketMatchColor := AValue;
    Changed;
  end;
end;

function TEditorColorSettings.GetFoldedCodeColor: TColor;
begin
  Result := FFoldedCodeColor;
end;

procedure TEditorColorSettings.SetFoldedCodeColor(AValue: TColor);
begin
  if AValue <> FoldedCodeColor then
  begin
    FFoldedCodeColor := AValue;
    Changed;
  end;
end;

function TEditorColorSettings.GetHighlightAllColor: TColor;
begin
  Result := FHighlightAllColor;
end;

procedure TEditorColorSettings.SetHighlightAllColor(AValue: TColor);
begin
  if AValue <> HighlightAllColor then
  begin
    FHighlightAllColor := AValue;
    Changed;
  end;
end;

function TEditorColorSettings.GetIncrementColor: TColor;
begin
  Result := FIncrementColor;
end;

procedure TEditorColorSettings.SetIncrementColor(AValue: TColor);
begin
  if AValue <> IncrementColor then
  begin
    FIncrementColor := AValue;
    Changed;
  end;
end;

function TEditorColorSettings.GetLineHighlightColor: TColor;
begin
  Result := FLineHighlightColor;
end;

procedure TEditorColorSettings.SetLineHighlightColor(AValue: TColor);
begin
  if AValue <> LineHighlightColor then
  begin
    FLineHighlightColor := AValue;
    Changed;
  end;
end;

function TEditorColorSettings.GetMouseLinkColor: TColor;
begin
  Result := FMouseLinkColor;
end;

procedure TEditorColorSettings.SetMouseLinkColor(AValue: TColor);
begin
  if AValue <> MouseLinkColor then
  begin
    FMouseLinkColor := AValue;
    Changed;
  end;
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

function TEditorColorSettings.GetSelectedColor: TColor;
begin
  Result := FSelectedColor;
end;

procedure TEditorColorSettings.SetSelectedColor(AValue: TColor);
begin
  if AValue <> SelectedColor then
  begin
    FSelectedColor := AValue;
    Changed;
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
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
  BracketMatchColor  := DEFAULT_BRACKET_MATCH_COLOR;
  HighlightAllColor  := DEFAULT_HIGHLIGHT_ALL_COLOR;
  LineHighlightColor := DEFAULT_LINE_HIGHLIGHT_COLOR;
  RightEdgeColor     := DEFAULT_RIGHT_EDGE_COLOR;
  MouseLinkColor     := DEFAULT_MOUSELINK_COLOR;
  SelectedColor      := DEFAULT_SELECTED_COLOR;
  IncrementColor     := DEFAULT_INCREMENT_COLOR;
  FoldedCodeColor    := DEFAULT_FOLDED_CODE_COLOR;
end;

procedure TEditorColorSettings.Changed;
begin
  if Assigned(OnChanged) then
    FOnChanged(Self);
end;
{$ENDREGION}

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

