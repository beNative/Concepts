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

unit DDuce.Editor.Options.Settings;

interface

uses
  System.Classes, System.SysUtils;

const
  DEFAULT_TAB_WIDTH          = 2;
  DEFAULT_BLOCK_INDENT       = 2;
  DEFAULT_BLOCK_TAB_INDENT   = 0;
  DEFAULT_WANT_TABS          = True;
  DEFAULT_EXTRA_CHAR_SPACING = 0;
  DEFAULT_EXTRA_LINE_SPACING = 0;
  DEFAULT_RIGHT_EDGE         = 80;

type
  TEditorOptionsSettings = class(TPersistent)
  private
    FOnChanged             : TNotifyEvent;
    FTabWidth              : Integer;
    FBlockIndent           : Integer;
    FBlockTabIndent        : Integer;
    FExtraCharSpacing      : Integer;
    FExtraLineSpacing      : Integer;
    FRightEdge             : Integer;
    FBracketHighlight      : Boolean;
    FEnhanceHomeKey        : Boolean;
    FEnhanceEndKey         : Boolean;
    FWantTabs              : Boolean;
    FAutoIndent            : Boolean;
    FAutoIndentOnPaste     : Boolean;
    FDragDropEditing       : Boolean;
    FSmartTabs             : Boolean;
    FTabsToSpaces          : Boolean;
    FTrimTrailingSpaces    : Boolean;
    FTabIndent             : Boolean;
    FShowSpecialCharacters : Boolean;
    FShowRightEdge         : Boolean;
    FCaretSkipsSelection   : Boolean;
    FCaretSkipsTab         : Boolean;
    FAlwaysVisibleCaret    : Boolean;
    FFoldedCopyPaste       : Boolean;
    FPersistentBlock       : Boolean;
    FOverwriteBlock        : Boolean;
    FAutoHideCursor        : Boolean;
    FWordWrapEnabled       : Boolean;
    FShowIndentGuides      : Boolean;
    FShowMinimap           : Boolean;
    FShowSearchmap         : Boolean;

    function GetAlwaysVisibleCaret: Boolean;
    function GetAutoHideCursor: Boolean;
    function GetAutoIndent: Boolean;
    function GetAutoIndentOnPaste: Boolean;
    function GetBlockIndent: Integer;
    function GetBlockTabIndent: Integer;
    function GetBracketHighlight: Boolean;
    function GetCaretSkipsSelection: Boolean;
    function GetCaretSkipsTab: Boolean;
    function GetDragDropEditing: Boolean;
    function GetEnhanceEndKey: Boolean;
    function GetEnhanceHomeKey: Boolean;
    function GetExtraCharSpacing: Integer;
    function GetExtraLineSpacing: Integer;
    function GetFoldedCopyPaste: Boolean;
    function GetOverwriteBlock: Boolean;
    function GetPersistentBlock: Boolean;
    function GetRightEdge: Integer;
    function GetShowRightEdge: Boolean;
    function GetShowSpecialCharacters: Boolean;
    function GetSmartTabs: Boolean;
    function GetTabIndent: Boolean;
    function GetTabsToSpaces: Boolean;
    function GetTabWidth: Integer;
    function GetTrimTrailingSpaces: Boolean;
    function GetWantTabs: Boolean;
    procedure SetAlwaysVisibleCaret(AValue: Boolean);
    procedure SetAutoHideCursor(AValue: Boolean);
    procedure SetAutoIndent(AValue: Boolean);
    procedure SetAutoIndentOnPaste(AValue: Boolean);
    procedure SetBlockIndent(AValue: Integer);
    procedure SetBlockTabIndent(AValue: Integer);
    procedure SetBracketHighlight(AValue: Boolean);
    procedure SetCaretSkipsSelection(AValue: Boolean);
    procedure SetCaretSkipsTab(AValue: Boolean);
    procedure SetDragDropEditing(AValue: Boolean);
    procedure SetEnhanceEndKey(AValue: Boolean);
    procedure SetEnhanceHomeKey(AValue: Boolean);
    procedure SetExtraCharSpacing(AValue: Integer);
    procedure SetExtraLineSpacing(AValue: Integer);
    procedure SetFoldedCopyPaste(AValue: Boolean);
    procedure SetOverwriteBlock(AValue: Boolean);
    procedure SetPersistentBlock(AValue: Boolean);
    procedure SetRightEdge(AValue: Integer);
    procedure SetShowRightEdge(AValue: Boolean);
    procedure SetShowSpecialCharacters(AValue: Boolean);
    procedure SetSmartTabs(AValue: Boolean);
    procedure SetTabIndent(AValue: Boolean);
    procedure SetTabsToSpaces(AValue: Boolean);
    procedure SetTabWidth(AValue: Integer);
    procedure SetTrimTrailingSpaces(AValue: Boolean);
    procedure SetWantTabs(AValue: Boolean);
    function GetWordWrapEnabled: Boolean;
    procedure SetWordWrapEnabled(const Value: Boolean);
    function GetShowIndentGuides: Boolean;
    procedure SetShowIndentGuides(const Value: Boolean);
    function GetShowMinimap: Boolean;
    procedure SetShowMinimap(const Value: Boolean);
    function GetShowSearchmap: Boolean;
    procedure SetShowSearchmap(const Value: Boolean);

  protected
    procedure Changed;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Assign(ASource: TPersistent); override;

    property OnChanged: TNotifyEvent
      read FOnChanged write FOnChanged;

  published
    property TabWidth: Integer
      read GetTabWidth write SetTabWidth default DEFAULT_TAB_WIDTH;

    property WantTabs: Boolean
      read GetWantTabs write SetWantTabs default DEFAULT_WANT_TABS;

    property BlockIndent: Integer
      read GetBlockIndent write SetBlockIndent
      default DEFAULT_BLOCK_INDENT;

    property BlockTabIndent: Integer
      read GetBlockTabIndent write SetBlockTabIndent
      default DEFAULT_BLOCK_TAB_INDENT;

    property ExtraCharSpacing: Integer
      read GetExtraCharSpacing write SetExtraCharSpacing
      default DEFAULT_EXTRA_CHAR_SPACING;

    property ExtraLineSpacing: Integer
      read GetExtraLineSpacing write SetExtraLineSpacing
      default DEFAULT_EXTRA_LINE_SPACING;

    property AutoIndent: Boolean
      read GetAutoIndent write SetAutoIndent default True;

    property AutoIndentOnPaste: Boolean
      read GetAutoIndentOnPaste write SetAutoIndentOnPaste default True;

    property BracketHighlight: Boolean
      read GetBracketHighlight write SetBracketHighlight default True;

    property EnhanceHomeKey: Boolean
      read GetEnhanceHomeKey write SetEnhanceHomeKey default True;

    { END key jumps to visual/hard line end whichever is nearer }
    property EnhanceEndKey: Boolean
      read GetEnhanceEndKey write SetEnhanceEndKey default True;

    property SmartTabs: Boolean
      read GetSmartTabs write SetSmartTabs default True;

    property TabIndent: Boolean
      read GetTabIndent write SetTabIndent default True;

    property TabsToSpaces: Boolean
      read GetTabsToSpaces write SetTabsToSpaces default True;

    property TrimTrailingSpaces: Boolean
      read GetTrimTrailingSpaces write SetTrimTrailingSpaces default True;

    property DragDropEditing: Boolean
      read GetDragDropEditing write SetDragDropEditing default True;

    property ShowIndentGuides: Boolean
      read GetShowIndentGuides write SetShowIndentGuides;

    property ShowMinimap: Boolean
      read GetShowMinimap write SetShowMinimap;

    property ShowSearchmap: Boolean
      read GetShowSearchmap write SetShowSearchmap default True;

    property ShowSpecialCharacters: Boolean
      read GetShowSpecialCharacters write SetShowSpecialCharacters
      default False;

    property ShowRightEdge: Boolean
      read GetShowRightEdge write SetShowRightEdge default True;

    { Caret skips selection on VK_LEFT/VK_RIGHT }
    property CaretSkipsSelection: Boolean
      read GetCaretSkipsSelection write SetCaretSkipsSelection default False;

    { Caret can not enter tabs }
    property CaretSkipsTab: Boolean
      read GetCaretSkipsTab write SetCaretSkipsTab default False;

    { Move caret to be always visible when scrolling }
    property AlwaysVisibleCaret: Boolean
      read GetAlwaysVisibleCaret write SetAlwaysVisibleCaret default False;

    { Remember folds in copy/paste operations }
    property FoldedCopyPaste: Boolean
      read GetFoldedCopyPaste write SetFoldedCopyPaste default True;

    { Keep block if caret moves away or text is edited }
    property PersistentBlock: Boolean
      read GetPersistentBlock write SetPersistentBlock default False;

    { Non persitent block, gets overwritten on insert/del }
    property OverwriteBlock: Boolean
      read GetOverwriteBlock write SetOverwriteBlock default True;

    { Hide the mouse cursor, on keyboard shortcuts }
    property AutoHideCursor: Boolean
      read GetAutoHideCursor write SetAutoHideCursor default False;

    property RightEdge: Integer
      read GetRightEdge write SetRightEdge default DEFAULT_RIGHT_EDGE;

    property WordWrapEnabled: Boolean
      read GetWordWrapEnabled write SetWordWrapEnabled default False;
  end;

implementation

{$REGION 'construction and destruction'}
procedure TEditorOptionsSettings.AfterConstruction;
begin
  inherited AfterConstruction;
  FBlockIndent     := DEFAULT_BLOCK_INDENT;
  FBlockTabIndent  := DEFAULT_BLOCK_TAB_INDENT;
  FTabWidth        := DEFAULT_TAB_WIDTH;
  FWantTabs        := DEFAULT_WANT_TABS;
  FRightEdge       := DEFAULT_RIGHT_EDGE;

  FBracketHighlight   := True;
  FEnhanceHomeKey     := True;
  FAutoIndent         := True;
  FAutoIndentOnPaste  := True;
  FDragDropEditing    := True;
  FSmartTabs          := True;
  FTabsToSpaces       := True;
  FTrimTrailingSpaces := True;
  FTabIndent          := True;
  FEnhanceEndKey      := True;
  FFoldedCopyPaste    := True;
  FShowRightEdge      := True;
  FShowSearchmap      := True;
  FOverwriteBlock     := True;
end;

procedure TEditorOptionsSettings.BeforeDestruction;
begin
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TEditorOptionsSettings.GetBlockIndent: Integer;
begin
  Result := FBlockIndent;
end;

procedure TEditorOptionsSettings.SetBlockIndent(AValue: Integer);
begin
  if AValue <> BlockIndent then
  begin
    FBlockIndent := AValue;
    Changed;
  end;
end;

function TEditorOptionsSettings.GetBlockTabIndent: Integer;
begin
  Result := FBlockTabIndent;
end;

procedure TEditorOptionsSettings.SetBlockTabIndent(AValue: Integer);
begin
  if AValue <> BlockTabIndent then
  begin
    FBlockTabIndent := AValue;
    Changed;
  end;
end;

function TEditorOptionsSettings.GetBracketHighlight: Boolean;
begin
  Result := FBracketHighlight;
end;

procedure TEditorOptionsSettings.SetBracketHighlight(AValue: Boolean);
begin
  if AValue <> BracketHighlight then
  begin
    FBracketHighlight := AValue;
    Changed;
  end;
end;

//function TEditorOptionsSettings.GetBracketHighlightStyle: TSynEditBracketHighlightStyle;
//begin
//  Result := FBracketHighlightStyle;
//end;
//
//procedure TEditorOptionsSettings.SetBracketHighlightStyle(
//  AValue: TSynEditBracketHighlightStyle);
//begin
//  if AValue <> BracketHighlightStyle then
//  begin
//    FBracketHighlightStyle := AValue;
//    Changed;
//  end;
//end;

function TEditorOptionsSettings.GetCaretSkipsSelection: Boolean;
begin
  Result := FCaretSkipsSelection;
end;

procedure TEditorOptionsSettings.SetCaretSkipsSelection(AValue: Boolean);
begin
  if AValue <> CaretSkipsSelection then
  begin
    FCaretSkipsSelection := AValue;
    Changed;
  end;
end;

function TEditorOptionsSettings.GetCaretSkipsTab: Boolean;
begin
  Result := FCaretSkipsTab;
end;

procedure TEditorOptionsSettings.SetCaretSkipsTab(AValue: Boolean);
begin
  if AValue <> CaretSkipsTab then
  begin
    FCaretSkipsTab := AValue;
    Changed;
  end;
end;

function TEditorOptionsSettings.GetDragDropEditing: Boolean;
begin
  Result := FDragDropEditing;
end;

procedure TEditorOptionsSettings.SetDragDropEditing(AValue: Boolean);
begin
  if AValue <> DragDropEditing then
  begin
    FDragDropEditing := AValue;
    Changed;
  end;
end;

function TEditorOptionsSettings.GetEnhanceEndKey: Boolean;
begin
  Result := FEnhanceEndKey;
end;

procedure TEditorOptionsSettings.SetEnhanceEndKey(AValue: Boolean);
begin
  if AValue <> EnhanceEndKey then
  begin
    FEnhanceEndKey := AValue;
    Changed;
  end;
end;

function TEditorOptionsSettings.GetEnhanceHomeKey: Boolean;
begin
  Result := FEnhanceHomeKey;
end;

procedure TEditorOptionsSettings.SetEnhanceHomeKey(AValue: Boolean);
begin
  if AValue <> EnhanceHomeKey then
  begin
    FEnhanceHomeKey := AValue;
    Changed;
  end;
end;

function TEditorOptionsSettings.GetExtraCharSpacing: Integer;
begin
  Result := FExtraCharSpacing;
end;

procedure TEditorOptionsSettings.SetExtraCharSpacing(AValue: Integer);
begin
  if AValue <> ExtraCharSpacing then
  begin
    FExtraCharSpacing := AValue;
    Changed;
  end;
end;

function TEditorOptionsSettings.GetExtraLineSpacing: Integer;
begin
  Result := FExtraLineSpacing;
end;

procedure TEditorOptionsSettings.SetExtraLineSpacing(AValue: Integer);
begin
  if AValue <> ExtraLineSpacing then
  begin
    FExtraLineSpacing := AValue;
    Changed;
  end;
end;

function TEditorOptionsSettings.GetFoldedCopyPaste: Boolean;
begin
  Result := FFoldedCopyPaste;
end;

procedure TEditorOptionsSettings.SetFoldedCopyPaste(AValue: Boolean);
begin
  if AValue <> FoldedCopyPaste then
  begin
    FFoldedCopyPaste := AValue;
    Changed;
  end;
end;

function TEditorOptionsSettings.GetOverwriteBlock: Boolean;
begin
  Result := FOverwriteBlock;
end;

procedure TEditorOptionsSettings.SetOverwriteBlock(AValue: Boolean);
begin
  if AValue <> OverwriteBlock then
  begin
    FOverwriteBlock := AValue;
    Changed;
  end;
end;

function TEditorOptionsSettings.GetPersistentBlock: Boolean;
begin
  Result := FPersistentBlock;
end;

procedure TEditorOptionsSettings.SetPersistentBlock(AValue: Boolean);
begin
  if AValue <> PersistentBlock then
  begin
    FPersistentBlock := AValue;
    Changed;
  end;
end;

function TEditorOptionsSettings.GetRightEdge: Integer;
begin
  Result := FRightEdge;
end;

procedure TEditorOptionsSettings.SetRightEdge(AValue: Integer);
begin
  if AValue <> RightEdge then
  begin
    FRightEdge := AValue;
    Changed;
  end;
end;

function TEditorOptionsSettings.GetShowIndentGuides: Boolean;
begin
  Result := FShowIndentGuides;
end;

function TEditorOptionsSettings.GetShowMinimap: Boolean;
begin
  Result := FShowMinimap;
end;

procedure TEditorOptionsSettings.SetShowMinimap(const Value: Boolean);
begin
  if Value <> ShowMinimap then
  begin
    FShowMinimap := Value;
    Changed;
  end;
end;

procedure TEditorOptionsSettings.SetShowIndentGuides(const Value: Boolean);
begin
  if Value <> ShowIndentGuides then
  begin
    FShowIndentGuides := Value;
    Changed;
  end;
end;

function TEditorOptionsSettings.GetShowRightEdge: Boolean;
begin
  Result := FShowRightEdge;
end;

procedure TEditorOptionsSettings.SetShowRightEdge(AValue: Boolean);
begin
  if AValue <> ShowRightEdge then
  begin
    FShowRightEdge := AValue;
    Changed;
  end;
end;

function TEditorOptionsSettings.GetShowSearchmap: Boolean;
begin
  Result := FShowSearchmap;
end;

procedure TEditorOptionsSettings.SetShowSearchmap(const Value: Boolean);
begin
  if Value <> ShowSearchmap then
  begin
    FShowSearchmap := Value;
    Changed;
  end;
end;

function TEditorOptionsSettings.GetShowSpecialCharacters: Boolean;
begin
  Result := FShowSpecialCharacters;
end;


procedure TEditorOptionsSettings.SetShowSpecialCharacters(AValue: Boolean);
begin
  if AValue <> ShowSpecialCharacters then
  begin
    FShowSpecialCharacters := AValue;
    Changed;
  end;
end;

function TEditorOptionsSettings.GetSmartTabs: Boolean;
begin
  Result := FSmartTabs;
end;

procedure TEditorOptionsSettings.SetSmartTabs(AValue: Boolean);
begin
  if AValue <> SmartTabs then
  begin
    FSmartTabs := AValue;
    Changed;
  end;
end;

function TEditorOptionsSettings.GetTabIndent: Boolean;
begin
  Result := FTabIndent;
end;

procedure TEditorOptionsSettings.SetTabIndent(AValue: Boolean);
begin
  if AValue <> TabIndent then
  begin
    FTabIndent := AValue;
    Changed;
  end;
end;

function TEditorOptionsSettings.GetTabsToSpaces: Boolean;
begin
  Result := FTabsToSpaces;
end;

procedure TEditorOptionsSettings.SetTabsToSpaces(AValue: Boolean);
begin
  if AValue <> TabsToSpaces then
  begin
    FTabsToSpaces := AValue;
    Changed;
  end;
end;

function TEditorOptionsSettings.GetTabWidth: Integer;
begin
  Result := FTabWidth;
end;

procedure TEditorOptionsSettings.SetTabWidth(AValue: Integer);
begin
  if AValue <> TabWidth then
  begin
    FTabWidth := AValue;
    Changed;
  end;
end;

function TEditorOptionsSettings.GetTrimTrailingSpaces: Boolean;
begin
  Result := FTrimTrailingSpaces;
end;

procedure TEditorOptionsSettings.SetTrimTrailingSpaces(AValue: Boolean);
begin
  if AValue <> TrimTrailingSpaces then
  begin
    FTrimTrailingSpaces := AValue;
    Changed;
  end;
end;

function TEditorOptionsSettings.GetWantTabs: Boolean;
begin
  Result := FWantTabs;
end;

procedure TEditorOptionsSettings.SetWantTabs(AValue: Boolean);
begin
  if AValue <> WantTabs then
  begin
    FWantTabs := AValue;
    Changed;
  end;
end;

function TEditorOptionsSettings.GetWordWrapEnabled: Boolean;
begin
  Result := FWordWrapEnabled;
end;

procedure TEditorOptionsSettings.SetWordWrapEnabled(const Value: Boolean);
begin
  if Value <> WordWrapEnabled then
  begin
    FWordWrapEnabled := Value;
    Changed;
  end;
end;

function TEditorOptionsSettings.GetAlwaysVisibleCaret: Boolean;
begin
  Result := FAlwaysVisibleCaret;
end;

procedure TEditorOptionsSettings.SetAlwaysVisibleCaret(AValue: Boolean);
begin
  if AValue <> AlwaysVisibleCaret then
  begin
    FAlwaysVisibleCaret := AValue;
    Changed;
  end;
end;

function TEditorOptionsSettings.GetAutoHideCursor: Boolean;
begin
  Result := FAutoHideCursor;
end;

procedure TEditorOptionsSettings.SetAutoHideCursor(AValue: Boolean);
begin
  if AValue <> AutoHideCursor then
  begin
    FAutoHideCursor := AValue;
    Changed;
  end;
end;

function TEditorOptionsSettings.GetAutoIndent: Boolean;
begin
  Result := FAutoIndent;
end;

procedure TEditorOptionsSettings.SetAutoIndent(AValue: Boolean);
begin
  if AValue <> AutoIndent then
  begin
    FAutoIndent := AValue;
    Changed;
  end;
end;

function TEditorOptionsSettings.GetAutoIndentOnPaste: Boolean;
begin
  Result := FAutoIndentOnPaste;
end;

procedure TEditorOptionsSettings.SetAutoIndentOnPaste(AValue: Boolean);
begin
  if AValue <> AutoIndentOnPaste then
  begin
    FAutoIndentOnPaste := AValue;
    Changed;
  end;
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TEditorOptionsSettings.Changed;
begin
  if Assigned(OnChanged) then
    FOnChanged(Self);
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TEditorOptionsSettings.Assign(ASource: TPersistent);
var
  EOS: TEditorOptionsSettings;
begin
  if ASource is TEditorOptionsSettings then
  begin
    EOS := TEditorOptionsSettings(ASource);
    EnhanceHomeKey        := EOS.EnhanceHomeKey;
    EnhanceEndKey         := EOS.EnhanceEndKey;
    AutoIndent            := EOS.AutoIndent;
    AutoIndentOnPaste     := EOS.AutoIndentOnPaste;
    DragDropEditing       := EOS.DragDropEditing;
    SmartTabs             := EOS.SmartTabs;
    TabsToSpaces          := EOS.TabsToSpaces;
    TrimTrailingSpaces    := EOS.TrimTrailingSpaces;
    TabIndent             := EOS.TabIndent;
    BlockIndent           := EOS.BlockIndent;
    BlockTabIndent        := EOS.BlockTabIndent;
    BracketHighlight      := EOS.BracketHighlight;
    WantTabs              := EOS.WantTabs;
    ExtraCharSpacing      := EOS.ExtraCharSpacing;
    ExtraLineSpacing      := EOS.ExtraLineSpacing;
    TabWidth              := EOS.TabWidth;
    ShowSpecialCharacters := EOS.ShowSpecialCharacters;
    ShowRightEdge         := EOS.ShowRightEdge;
//    BracketHighlightStyle := EOS.BracketHighlightStyle;
    RightEdge             := EOS.RightEdge;
    CaretSkipsSelection   := EOS.CaretSkipsSelection;
    CaretSkipsTab         := EOS.CaretSkipsTab;
    AlwaysVisibleCaret    := EOS.AlwaysVisibleCaret;
    FoldedCopyPaste       := EOS.FoldedCopyPaste;
    PersistentBlock       := EOS.PersistentBlock;
    OverwriteBlock        := EOS.OverwriteBlock;
    AutoHideCursor        := EOS.AutoHideCursor;
  end
  else
    inherited Assign(ASource);
end;
{$ENDREGION}

end.

