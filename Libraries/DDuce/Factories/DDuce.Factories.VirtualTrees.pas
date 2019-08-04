{
  Copyright (C) 2013-2019 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.Factories.VirtualTrees;

interface

uses
  System.Classes, System.UITypes,
  Vcl.Controls,

  Spring,

  VirtualTrees;

{ TVSTOptions is a settings container which holds the most commonly adjusted
  properties of the TVirtualStringTree component.
  This is intended to create a consistent look and feel when the
  TVirtualStringTree control is used as a tree, grid, list, treegrid, or
  treelist control in your applications. }

type
  TVSTOptions = class(TPersistent)
  strict private
    FHeaderOptions                : TVTHeaderOptions;
    FPaintOptions                 : TVTPaintOptions;
    FAnimationOptions             : TVTAnimationOptions;
    FAutoOptions                  : TVTAutoOptions;
    FStringOptions                : TVTStringOptions;
    FSelectionOptions             : TVTSelectionOptions;
    FMiscOptions                  : TVTMiscOptions;
    FEditOptions                  : TVTEditOptions;
    FColumnOptions                : TVTColumnOptions;
    FLineStyle                    : TVTLineStyle;     // style of the tree lines
    FLineMode                     : TVTLineMode;      // tree lines or bands etc.
    FDrawSelectionMode            : TVTDrawSelectionMode;
    FHintMode                     : TVTHintMode;
    FSelectionTextColor           : TColor;
    FSelectionRectangleBlendColor : TColor;
    FGridLineColor                : TColor;

  public
    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Source: TPersistent); override;

    property HeaderOptions: TVTHeaderOptions
      read FHeaderOptions write FHeaderOptions;

    property PaintOptions: TVTPaintOptions
      read FPaintOptions write FPaintOptions;

    property AnimationOptions: TVTAnimationOptions
      read FAnimationOptions write FAnimationOptions;

    property AutoOptions: TVTAutoOptions
      read FAutoOptions write FAutoOptions;

    property StringOptions: TVTStringOptions
      read FStringOptions write FStringOptions;

    property SelectionOptions: TVTSelectionOptions
      read FSelectionOptions write FSelectionOptions;

    property MiscOptions: TVTMiscOptions
      read FMiscOptions write FMiscOptions;

    property EditOptions: TVTEditOptions
      read FEditOptions write FEditOptions;

    property ColumnOptions: TVTColumnOptions // TS: todo default column options
      read FColumnOptions write FColumnOptions;

    property LineStyle: TVTLineStyle
      read FLineStyle write FLineStyle;

    property LineMode: TVTLineMode
      read FLineMode write FLineMode;

    property DrawSelectionMode: TVTDrawSelectionMode
      read FDrawSelectionMode write FDrawSelectionMode;

    property HintMode: TVTHintMode
      read FHintMode write FHintMode;

    { background color for selection. }
    property SelectionRectangleBlendColor: TColor
      read FSelectionRectangleBlendColor write FSelectionRectangleBlendColor;

    { font color for text in selection. }
    property SelectionTextColor: TColor
      read FSelectionTextColor write FSelectionTextColor;

    property GridLineColor: TColor
      read FGridLineColor write FGridLineColor;
  end;

type
  TVirtualStringTreeFactory = class sealed
  private class var
    FDefaultTreeOptions     : Lazy<TVSTOptions>;
    FDefaultGridOptions     : Lazy<TVSTOptions>;
    FDefaultListOptions     : Lazy<TVSTOptions>;
    FDefaultTreeGridOptions : Lazy<TVSTOptions>;
    FDefaultTreeListOptions : Lazy<TVSTOptions>;

    class function GetDefaultGridOptions: TVSTOptions; static;
    class function GetDefaultListOptions: TVSTOptions; static;
    class function GetDefaultTreeGridOptions: TVSTOptions; static;
    class function GetDefaultTreeListOptions: TVSTOptions; static;
    class function GetDefaultTreeOptions: TVSTOptions; static;

  public
    class constructor Create;

    class function Create(
      AOwner      : TComponent;
      AParent     : TWinControl;
      const AName : string = ''
    ): TVirtualStringTree;

    class function CreateTree(
      AOwner      : TComponent;
      AParent     : TWinControl;
      const AName : string = ''
    ): TVirtualStringTree;

    class function CreateList(
      AOwner      : TComponent;
      AParent     : TWinControl;
      const AName : string = ''
    ): TVirtualStringTree;

    class function CreateGrid(
      AOwner      : TComponent;
      AParent     : TWinControl;
      const AName : string = ''
    ): TVirtualStringTree;

    class function CreateTreeGrid(
      AOwner      : TComponent;
      AParent     : TWinControl;
      const AName : string = ''
    ): TVirtualStringTree;

    class function CreateTreeList(
      AOwner      : TComponent;
      AParent     : TWinControl;
      const AName : string = ''
    ): TVirtualStringTree;

    class property DefaultTreeOptions: TVSTOptions
      read GetDefaultTreeOptions;

    class property DefaultGridOptions: TVSTOptions
      read GetDefaultGridOptions;

    class property DefaultListOptions: TVSTOptions
      read GetDefaultListOptions;

    class property DefaultTreeGridOptions: TVSTOptions
      read GetDefaultTreeGridOptions;

    class property DefaultTreeListOptions: TVSTOptions
      read GetDefaultTreeListOptions;
  end;

implementation

uses
  System.SysUtils,
  Vcl.Graphics;

type
  TCustomVirtualStringTreeAccess = class(TCustomVirtualStringTree) end;

{$REGION 'TVSTOptions'}
procedure TVSTOptions.Assign(Source: TPersistent);
var
  LOptions : TVSTOptions;
begin
  if Source is TVSTOptions then
  begin
    LOptions := TVSTOptions(Source);
    AnimationOptions             := LOptions.AnimationOptions;
    AutoOptions                  := LOptions.AutoOptions;
    MiscOptions                  := LOptions.MiscOptions;
    PaintOptions                 := LOptions.PaintOptions;
    SelectionOptions             := LOptions.SelectionOptions;
    EditOptions                  := LOptions.EditOptions;
    HeaderOptions                := LOptions.HeaderOptions;
    LineStyle                    := LOptions.LineStyle;
    LineMode                     := LOptions.LineMode;
    DrawSelectionMode            := LOptions.DrawSelectionMode;
    HintMode                     := LOptions.HintMode;
    SelectionTextColor           := LOptions.SelectionTextColor;
    GridLineColor                := LOptions.GridLineColor;
    SelectionRectangleBlendColor := LOptions.SelectionRectangleBlendColor;
    StringOptions                := LOptions.StringOptions;
  end
  else
    inherited Assign(Source);
end;

procedure TVSTOptions.AssignTo(Dest: TPersistent);
var
  LTree    : TCustomVirtualStringTreeAccess;
  LOptions : TVSTOptions;
begin
  if Dest is TCustomVirtualStringTree then
  begin
    LTree := TCustomVirtualStringTreeAccess(Dest);
    LTree.TreeOptions.AnimationOptions        := AnimationOptions;
    LTree.TreeOptions.AutoOptions             := AutoOptions;
    LTree.TreeOptions.MiscOptions             := MiscOptions;
    LTree.TreeOptions.PaintOptions            := PaintOptions;
    LTree.TreeOptions.SelectionOptions        := SelectionOptions;
    LTree.TreeOptions.EditOptions             := EditOptions;
    LTree.Header.Options                      := HeaderOptions;
    LTree.LineStyle                           := LineStyle;
    LTree.LineMode                            := LineMode;
    LTree.DrawSelectionMode                   := DrawSelectionMode;
    LTree.HintMode                            := HintMode;
    LTree.Colors.SelectionTextColor           := SelectionTextColor;
    LTree.Colors.GridLineColor                := GridLineColor;
    LTree.Colors.SelectionRectangleBlendColor := SelectionRectangleBlendColor;
    TStringTreeOptions(LTree.TreeOptions).StringOptions := StringOptions;
  end
  else if Dest is TVSTOptions then
  begin
    LOptions                              := TVSTOptions(Dest);
    LOptions.AnimationOptions             := AnimationOptions;
    LOptions.AutoOptions                  := AutoOptions;
    LOptions.MiscOptions                  := MiscOptions;
    LOptions.PaintOptions                 := PaintOptions;
    LOptions.SelectionOptions             := SelectionOptions;
    LOptions.EditOptions                  := EditOptions;
    LOptions.HeaderOptions                := HeaderOptions;
    LOptions.LineStyle                    := LineStyle;
    LOptions.LineMode                     := LineMode;
    LOptions.DrawSelectionMode            := DrawSelectionMode;
    LOptions.HintMode                     := HintMode;
    LOptions.SelectionTextColor           := SelectionTextColor;
    LOptions.GridLineColor                := GridLineColor;
    LOptions.SelectionRectangleBlendColor := SelectionRectangleBlendColor;
    LOptions.StringOptions                := StringOptions;
  end
  else
    inherited AssignTo(Dest);
end;
{$ENDREGION}

{$REGION 'construction and destruction'}
class constructor TVirtualStringTreeFactory.Create;
begin
  {$REGION 'FDefaultTreeOptions'}
  FDefaultTreeOptions.Create(
    function: TVSTOptions
    begin
      Result := TVSTOptions.Create;
      with Result do
      begin
        HeaderOptions := [
          {hoAutoResize,}
          {hoHeightResize,}
          {hoHeightDblClickResize,}
          {hoRestrictDrag,}
          {hoShowHint,}
          {hoShowImages,}
          {hoShowSortGlyphs,}
          {hoVisible}
        ];
        PaintOptions := [
          toHideFocusRect,
          {toHideSelection,}
          toPopupMode,
          toShowButtons,
          toShowDropmark,
          toShowRoot,
          toThemeAware,
          {toUseExplorerTheme,}
          toUseBlendedImages,
          toUseBlendedSelection,
          toStaticBackground
        ];
        AnimationOptions := [
          {toAnimatedToggle,}
          {toAdvancedAnimatedToggle}
        ];
        AutoOptions := [
          {toAutoDropExpand,}
          toAutoScroll,
          {toAutoScrollOnExpand,}
          toDisableAutoscrollOnEdit,
          toAutoSort,
          toAutoTristateTracking,
          toAutoDeleteMovedNodes,
          toAutoChangeScale,
          toAutoBidiColumnOrdering
        ];
        StringOptions := [
          toAutoAcceptEditChange
        ];
        SelectionOptions := [
          {toDisableDrawSelection,}
          toExtendedFocus,
          {toFullRowSelect,}
          {toLevelSelectConstraint,}
          {toMiddleClickSelect,}
          {toMultiSelect,}
          {toRightClickSelect,}
          {toSiblingSelectConstraint,}
          {toCenterScrollIntoView,}
          {toSimpleDrawSelection,}
          toAlwaysSelectNode{,}
          {toRestoreSelection,}
          {toSyncCheckboxesWithSelection}
        ];
        MiscOptions := [
          {toAcceptOLEDrop,}
          toCheckSupport,
          {toEditable,}
          {toFullRepaintOnResize,}
          {toGridExtensions,}
          toInitOnSave,
          {toReportMode,}
          toToggleOnDblClick,
          toWheelPanning,
          {toReadOnly,}
          toVariableNodeHeight{,}
          {toFullRowDrag,}
          {toNodeHeightResize,}
          {toNodeHeightDblClickResize,}
          {toEditOnClick,}
          {toEditOnDblClick,}
          {toReverseFullExpandHotKey}
        ];
        EditOptions := toDefaultEdit;
        ColumnOptions := [
          {coAllowClick,}
          {coDraggable,}
          {coEnabled,}
          {coParentBidiMode,}
          {coParentColor,}
          {coResizable,}
          {coShowDropMark,}
          {coVisible,}
          {coAutoSpring,}
          {coFixed,}
          {coSmartResize,}
          {coAllowFocus,}
          {coDisableAnimatedResize,}
          {coWrapCaption,}
          {coUseCaptionAlignment,}
          {coEditable,}
          {coStyleColor}
        ];

        LineStyle                    := lsDotted;
        LineMode                     := lmNormal;
        DrawSelectionMode            := smBlendedRectangle;
        HintMode                     := hmTooltip;
        SelectionRectangleBlendColor := clGray;
        SelectionTextColor           := clBlack;
        GridLineColor                := clSilver;
      end;
    end,
    True
  );
  {$ENDREGION}

  {$REGION 'FDefaultGridOptions'}
  FDefaultGridOptions.Create(
    function: TVSTOptions
    begin
      Result := TVSTOptions.Create;
      with Result do
      begin
        HeaderOptions := [
          hoAutoResize,
          hoColumnResize,
          hoDblClickResize,
          hoDrag,
          {hoHotTrack,}
          {hoOwnerDraw,}
          hoRestrictDrag,
          hoShowHint,
          hoShowImages,
          hoShowSortGlyphs,
          hoVisible,
          hoAutoSpring,
          {hoFullRepaintOnResize,}
          hoDisableAnimatedResize{,}
          {hoHeightResize,}
          {hoHeightDblClickResize,}
          {hoHeaderClickAutoSort,}
          {hoAutoColumnPopupMenu}
        ];
        PaintOptions := [
          toHideFocusRect,
          {toHideSelection,}
          {toHotTrack,}
          toPopupMode,
          {toShowBackground,}
          toShowButtons,
          toShowDropmark,
          toShowHorzGridLines,
          toShowRoot,
          {toShowTreeLines,}
          toShowVertGridLines,
          toThemeAware,
          toUseBlendedImages,
          {toGhostedIfUnfocused,}
          {toFullVertGridLines,}
          {toAlwaysHideSelection,}
           toUseBlendedSelection{,}
          {toStaticBackground,}
          {toChildrenAbove,}
          {toFixedIndent,}
          {toUseExplorerTheme,}
          {toHideTreeLinesIfThemed,}
          {toShowFilteredNodes}
        ];
        AnimationOptions := [
          {toAnimatedToggle,}
          {toAdvancedAnimatedToggle}
        ];
        AutoOptions := [
          toAutoDropExpand,
          {toAutoExpand,}
          toAutoScroll,
          toAutoScrollOnExpand,
          toAutoSort,
          {toAutoSpanColumns,}
          toAutoTristateTracking,
          {toAutoHideButtons,}
          toAutoDeleteMovedNodes,
          {toDisableAutoscrollOnFocus,}
          toAutoChangeScale,
          {toAutoFreeOnCollapse,}
          toDisableAutoscrollOnEdit,
          toAutoBidiColumnOrdering
        ];
        StringOptions := [
          toAutoAcceptEditChange
        ];
        SelectionOptions := [
          {toDisableDrawSelection,}
          toExtendedFocus,
          toFullRowSelect{,}
          {toLevelSelectConstraint,}
          {toMiddleClickSelect,}
          {toMultiSelect,}
          {toRightClickSelect,}
          {toSiblingSelectConstraint,}
          {toCenterScrollIntoView,}
          {toSimpleDrawSelection,}
          {toAlwaysSelectNode,}
          {toRestoreSelection,}
          {toSyncCheckboxesWithSelection}
        ];
        MiscOptions := [
          {toAcceptOLEDrop,}
          toCheckSupport,
          {toEditable,}
          {toFullRepaintOnResize,}
          {toGridExtensions,}
          toInitOnSave,
          {toReportMode,}
          toToggleOnDblClick,
          toWheelPanning,
          {toReadOnly,}
          toVariableNodeHeight{,}
          {toFullRowDrag,}
          {toNodeHeightResize,}
          {toNodeHeightDblClickResize,}
          {toEditOnClick,}
          {toEditOnDblClick,}
          {toReverseFullExpandHotKey}
        ];
        EditOptions := toDefaultEdit;
        ColumnOptions := [
          {coAllowClick,}
          {coDraggable,}
          {coEnabled,}
          {coParentBidiMode,}
          {coParentColor,}
          {coResizable,}
          {coShowDropMark,}
          {coVisible,}
          {coAutoSpring,}
          {coFixed,}
          {coSmartResize,}
          {coAllowFocus,}
          {coDisableAnimatedResize,}
          {coWrapCaption,}
          {coUseCaptionAlignment,}
          {coEditable,}
          {coStyleColor}
        ];
        LineStyle                    := lsSolid;
        LineMode                     := lmBands;
        DrawSelectionMode            := smBlendedRectangle;
        HintMode                     := hmTooltip;
        SelectionRectangleBlendColor := clGray;
        SelectionTextColor           := clBlack;
        GridLineColor                := clSilver;
      end;
    end,
    True
  );
  {$ENDREGION}

  {$REGION 'FDefaultListOptions'}
  FDefaultListOptions.Create(
    function: TVSTOptions
    begin
      Result := TVSTOptions.Create;
      with Result do
      begin
        HeaderOptions := [
          hoAutoResize, hoAutoSpring, hoColumnResize, hoDblClickResize,
          hoDrag, hoRestrictDrag, hoDisableAnimatedResize,
          hoShowHint, hoShowImages, hoShowSortGlyphs,
          hoVisible
        ];
        PaintOptions := [
          toHideFocusRect, {toHideSelection,} toHotTrack, toPopupMode,
          {toShowButtons,} toShowDropmark, {toShowRoot,}
          {toShowHorzGridLines,} {toShowVertGridLines,}
          toThemeAware, toUseExplorerTheme,
          toUseBlendedImages, toUseBlendedSelection,
          toStaticBackground
        ];
        AnimationOptions := [
          {toAnimatedToggle,}
          {toAdvancedAnimatedToggle}
        ];
        AutoOptions := [
          toAutoDropExpand,
          toAutoScroll,
          toAutoScrollOnExpand,
          toDisableAutoscrollOnEdit,
          toAutoSort,
          toAutoTristateTracking,
          toAutoDeleteMovedNodes,
          toAutoChangeScale,
          toAutoBidiColumnOrdering
        ];
        StringOptions := [toAutoAcceptEditChange];
        SelectionOptions := [
          {toDisableDrawSelection,}
          toExtendedFocus,
          toFullRowSelect,
          {toLevelSelectConstraint,}
          {toMiddleClickSelect,}
          {toMultiSelect,}
          {toRightClickSelect,}
          {toSiblingSelectConstraint,}
          {toCenterScrollIntoView,}
          {toSimpleDrawSelection,}
          toAlwaysSelectNode{,}
          {toRestoreSelection,}
          {toSyncCheckboxesWithSelection}
        ];
        MiscOptions := [
          toCheckSupport,
          toInitOnSave,
          toToggleOnDblClick,
          toWheelPanning,
          toVariableNodeHeight
        ];
        EditOptions := toDefaultEdit;
        ColumnOptions := [
          {coAllowClick,}
          {coDraggable,}
          {coEnabled,}
          {coParentBidiMode,}
          {coParentColor,}
          {coResizable,}
          {coShowDropMark,}
          {coVisible,}
          {coAutoSpring,}
          {coFixed,}
          {coSmartResize,}
          {coAllowFocus,}
          {coDisableAnimatedResize,}
          {coWrapCaption,}
          {coUseCaptionAlignment,}
          {coEditable,}
          {coStyleColor}
        ];
        LineStyle                    := lsSolid;
        LineMode                     := lmNormal;
        DrawSelectionMode            := smBlendedRectangle;
        HintMode                     := hmTooltip;
        SelectionRectangleBlendColor := clGray;
        SelectionTextColor           := clBlack;
        GridLineColor                := clSilver;
      end;
    end,
    True
  );
  {$ENDREGION}

  {$REGION 'FDefaultTreeGridOptions'}
  FDefaultTreeGridOptions.Create(
    function: TVSTOptions
    begin
      Result := TVSTOptions.Create;
      with Result do
      begin
        HeaderOptions := [
          hoAutoResize,
          hoAutoSpring,
          hoColumnResize,
          hoDblClickResize,
          hoRestrictDrag,
          hoDisableAnimatedResize,
          hoShowHint,
          hoShowImages,
          hoShowSortGlyphs,
          hoVisible
        ];
        PaintOptions := [
          toHideFocusRect,
          {toHideSelection,}
          {toHotTrack,}
          toPopupMode,
          toShowButtons,
          toShowDropmark,
          toShowRoot,
          toShowHorzGridLines,
          toShowVertGridLines,
          toThemeAware, {toUseExplorerTheme,}
          toUseBlendedImages,
          toUseBlendedSelection,
          toStaticBackground
        ];
        AnimationOptions := [
          {toAnimatedToggle,}
          {toAdvancedAnimatedToggle}
        ];
        AutoOptions := [
          toAutoDropExpand,
          toAutoScroll,
          toAutoScrollOnExpand,
          toDisableAutoscrollOnEdit,
          toAutoSort,
          toAutoTristateTracking,
          toAutoDeleteMovedNodes,
          toAutoChangeScale,
          toAutoBidiColumnOrdering
        ];
        StringOptions := [toAutoAcceptEditChange];
        SelectionOptions := [
          {toDisableDrawSelection,}
          toExtendedFocus,
          toFullRowSelect,
          {toLevelSelectConstraint,}
          {toMiddleClickSelect,}
          {toMultiSelect,}
          {toRightClickSelect,}
          {toSiblingSelectConstraint,}
          {toCenterScrollIntoView,}
          {toSimpleDrawSelection,}
          toAlwaysSelectNode{,}
          {toRestoreSelection,}
          {toSyncCheckboxesWithSelection}
        ];
        MiscOptions := [
          toCheckSupport,
          toInitOnSave,
          toToggleOnDblClick,
          toWheelPanning,
          toVariableNodeHeight
        ];
        EditOptions := toDefaultEdit;
        ColumnOptions := [
          {coAllowClick,}
          {coDraggable,}
          {coEnabled,}
          {coParentBidiMode,}
          {coParentColor,}
          {coResizable,}
          {coShowDropMark,}
          {coVisible,}
          {coAutoSpring,}
          {coFixed,}
          {coSmartResize,}
          {coAllowFocus,}
          {coDisableAnimatedResize,}
          {coWrapCaption,}
          {coUseCaptionAlignment,}
          {coEditable,}
          {coStyleColor}
        ];

        LineStyle                    := lsSolid;
        LineMode                     := lmNormal;
        DrawSelectionMode            := smBlendedRectangle;
        HintMode                     := hmTooltip;
        SelectionRectangleBlendColor := clGray;
        SelectionTextColor           := clBlack;
        GridLineColor                := clSilver;
      end;
    end,
    True
  );
  {$ENDREGION}

  {$REGION 'FDefaultTreeListOptions'}
  FDefaultTreeListOptions.Create(
    function: TVSTOptions
    begin
      Result := TVSTOptions.Create;
      with Result do
      begin
        HeaderOptions := [
          hoAutoResize,
          hoAutoSpring,
          hoColumnResize,
          hoDblClickResize,
          hoRestrictDrag,
          hoDisableAnimatedResize,
          hoShowHint,
          hoShowImages,
          hoShowSortGlyphs,
          hoVisible
        ];
        PaintOptions := [
          toHideFocusRect,
          {toHideSelection,}
          toHotTrack,
          toPopupMode,
          toShowButtons,
          toShowDropmark,
          toShowRoot,
          {toShowHorzGridLines,}
          toShowVertGridLines,
          toThemeAware,
          toUseExplorerTheme,
          toUseBlendedSelection,
          toUseBlendedImages,
          toStaticBackground
        ];
        AnimationOptions := [
          {toAnimatedToggle,}
          {toAdvancedAnimatedToggle}
        ];
        AutoOptions := [
          toAutoDropExpand,
          toAutoScroll,
          toAutoScrollOnExpand,
          toDisableAutoscrollOnEdit,
          toAutoSort,
          toAutoTristateTracking,
          toAutoDeleteMovedNodes,
          toAutoChangeScale,
          toAutoBidiColumnOrdering
        ];
        StringOptions := [toAutoAcceptEditChange];
        SelectionOptions := [
          {toDisableDrawSelection,}
          toExtendedFocus,
          toFullRowSelect,
          {toLevelSelectConstraint,}
          {toMiddleClickSelect,}
          {toMultiSelect,}
          {toRightClickSelect,}
          {toSiblingSelectConstraint,}
          {toCenterScrollIntoView,}
          {toSimpleDrawSelection,}
          toAlwaysSelectNode{,}
          {toRestoreSelection,}
          {toSyncCheckboxesWithSelection}
        ];
        MiscOptions := [
          toCheckSupport,
          toInitOnSave,
          toToggleOnDblClick,
          toWheelPanning,
          toVariableNodeHeight
        ];
        EditOptions := toDefaultEdit;
        ColumnOptions := [
          {coAllowClick,}
          {coDraggable,}
          {coEnabled,}
          {coParentBidiMode,}
          {coParentColor,}
          {coResizable,}
          {coShowDropMark,}
          {coVisible,}
          {coAutoSpring,}
          {coFixed,}
          {coSmartResize,}
          {coAllowFocus,}
          {coDisableAnimatedResize,}
          {coWrapCaption,}
          {coUseCaptionAlignment,}
          {coEditable,}
          {coStyleColor}
        ];

        LineStyle                    := lsSolid;
        LineMode                     := lmNormal;
        DrawSelectionMode            := smBlendedRectangle;
        HintMode                     := hmTooltip;
        SelectionRectangleBlendColor := clGray;
        SelectionTextColor           := clBlack;
        GridLineColor                := clSilver;
      end;
    end,
    True
  );
  {$ENDREGION}
end;
{$ENDREGION}

{$REGION 'property access methods'}
class function TVirtualStringTreeFactory.GetDefaultGridOptions: TVSTOptions;
begin
  Result := FDefaultGridOptions;
end;

class function TVirtualStringTreeFactory.GetDefaultListOptions: TVSTOptions;
begin
  Result := FDefaultListOptions;
end;

class function TVirtualStringTreeFactory.GetDefaultTreeGridOptions: TVSTOptions;
begin
  Result := FDefaultTreeGridOptions;
end;

class function TVirtualStringTreeFactory.GetDefaultTreeListOptions: TVSTOptions;
begin
  Result := FDefaultTreeListOptions;
end;

class function TVirtualStringTreeFactory.GetDefaultTreeOptions: TVSTOptions;
begin
  Result := FDefaultTreeOptions;
end;
{$ENDREGION}

{$REGION 'public methods'}
{ Creates a TVirtualStringTree instance with stock settings. }

class function TVirtualStringTreeFactory.Create(AOwner: TComponent;
  AParent: TWinControl; const AName: string): TVirtualStringTree;
var
  VST : TVirtualStringTree;
begin
  Guard.CheckNotNull(AOwner, 'AOwner');
  Guard.CheckNotNull(AParent, 'AParent');
  VST := TVirtualStringTree.Create(AOwner);
  VST.AlignWithMargins := True;
  VST.Parent           := AParent;
  VST.Align            := alClient;
  VST.Header.Height    := 18;
  VST.ShowHint         := True;
  Result := VST;
end;

{ Creates a TVirtualStringTree that is tuned to behave and look like a grid
  control. }

class function TVirtualStringTreeFactory.CreateGrid(AOwner: TComponent;
  AParent: TWinControl; const AName: string): TVirtualStringTree;
var
  VST : TVirtualStringTree;
begin
  Guard.CheckNotNull(AOwner, 'AOwner');
  Guard.CheckNotNull(AParent, 'AParent');
  VST := TVirtualStringTree.Create(AOwner);
  VST.AlignWithMargins := True;
  VST.Parent           := AParent;
  VST.Align            := alClient;
  VST.Header.Height    := 18;
  DefaultGridOptions.AssignTo(VST);
  VST.Indent := 2; // show first column as a normal grid column
  VST.ShowHint         := True;
  Result := VST;
end;

{ Creates a TVirtualStringTree that mimics a list control. }

class function TVirtualStringTreeFactory.CreateList(AOwner: TComponent;
  AParent: TWinControl; const AName: string): TVirtualStringTree;
var
  VST : TVirtualStringTree;
begin
  Guard.CheckNotNull(AOwner, 'AOwner');
  Guard.CheckNotNull(AParent, 'AParent');
  VST := TVirtualStringTree.Create(AOwner);
  VST.AlignWithMargins := True;
  VST.Parent           := AParent;
  VST.Align            := alClient;
  VST.Header.Height    := 18;
  DefaultListOptions.AssignTo(VST);
  VST.Indent := 2; // show first column as a normal grid column
  VST.ShowHint         := True;
  Result := VST;
end;

{ Creates a TVirtualStringTree that will be used as a simple tree control with
  no header. }

class function TVirtualStringTreeFactory.CreateTree(AOwner: TComponent;
  AParent: TWinControl; const AName: string): TVirtualStringTree;
var
  VST : TVirtualStringTree;
begin
  Guard.CheckNotNull(AOwner, 'AOwner');
  Guard.CheckNotNull(AParent, 'AParent');
  VST := TVirtualStringTree.Create(AOwner);
  VST.AlignWithMargins := True;
  VST.Parent           := AParent;
  VST.Align            := alClient;
  VST.ShowHint         := True;
  DefaultTreeOptions.AssignTo(VST);
  Result := VST;
end;

{ Creates a TVirtualStringTree with a header and columns, using the first column
  to display the tree structure and tuned to behave and look like a grid
  control. }

class function TVirtualStringTreeFactory.CreateTreeGrid(AOwner: TComponent;
  AParent: TWinControl; const AName: string): TVirtualStringTree;
var
  VST : TVirtualStringTree;
begin
  Guard.CheckNotNull(AOwner, 'AOwner');
  Guard.CheckNotNull(AParent, 'AParent');
  VST := TVirtualStringTree.Create(AOwner);
  VST.AlignWithMargins := True;
  VST.Parent           := AParent;
  VST.Align            := alClient;
  VST.Header.Height    := 18;
  VST.ShowHint         := True;
  DefaultTreeGridOptions.AssignTo(VST);
  Result := VST;
end;

{ Creates a TVirtualStringTree with a header and columns, using the first column
  to display the tree structure and tuned to behave and look like a list
  control. }

class function TVirtualStringTreeFactory.CreateTreeList(AOwner: TComponent;
  AParent: TWinControl; const AName: string): TVirtualStringTree;
var
  VST : TVirtualStringTree;
begin
  Guard.CheckNotNull(AOwner, 'AOwner');
  Guard.CheckNotNull(AParent, 'AParent');
  VST := TVirtualStringTree.Create(AOwner);
  VST.AlignWithMargins := True;
  VST.Parent           := AParent;
  VST.Align            := alClient;
  VST.Header.Height    := 18;
  VST.ShowHint         := True;
  DefaultTreeListOptions.AssignTo(VST);
  Result := VST;
end;
{$ENDREGION}
end.
