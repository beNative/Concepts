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

unit DDuce.Editor.View;

interface

{$REGION'documentation'}
{
  Form holding a complete customizable text editor based on the open source
  TTextEditor component.
  Features:
    - dynamic editor creation
    - highlight selected text
    - code folding
}
{$ENDREGION}

uses
  Winapi.Messages,
  System.Classes, System.SysUtils, System.Types,
  Vcl.Controls, Vcl.Forms, Vcl.Graphics, Vcl.Menus, Vcl.Dialogs, Vcl.AppEvnts,

  TextEditor, TextEditor.Types, TextEditor.KeyCommands, TextEditor.Highlighter,

  DDuce.Editor.Resources, DDuce.Editor.Highlighters, DDuce.Editor.Interfaces,
  DDuce.Logger, System.ImageList, Vcl.ImgList, Vcl.VirtualImageList;

type
  TEditorView = class(TForm, IEditorView)
    imlMain: TVirtualImageList;
  private
    FUpdate          : Boolean;
    FLineBreakStyle  : string; // not supported
    FEditor          : TTextEditor;
    FFindHistory     : TStringList;
    FReplaceHistory  : TStringList;
    FHighlighterItem : THighlighterItem;
    FFileName        : string;
    FFoldLevel       : Integer; // not supported
    FIsFile          : Boolean;
    FOnChange        : TNotifyEvent;

    {$REGION 'event handlers'}
    procedure EditorChange(Sender: TObject);
    procedure EditorCaretChanged(
      const ASender : TObject;
      const X, Y    : Integer;
      const AOffset : Integer
    );
    procedure EditorReplaceText(
      const ASender: TObject;
      const AParams: TTextEditorReplaceTextParams;
      var AAction: TTextEditorReplaceAction
    );
    procedure EditorEnter(Sender: TObject);
    procedure EditorSettingsChanged(ASender: TObject);
    {$ENDREGION}

    {$REGION'property access methods'}
    function GetActions: IEditorActions;
    function GetBlockBegin: TPoint;
    function GetBlockEnd: TPoint;
    function GetCanPaste: Boolean;
    function GetCanRedo: Boolean;
    function GetCanUndo: Boolean;
    function GetCaretX: Integer; virtual;
    function GetCaretXY: TPoint;
    function GetCaretY: Integer; virtual;
    function GetCommands: IEditorCommands;
    function GetCurrentChar: WideChar;
    function GetCurrentWord: string;
    function GetEditor: TTextEditor;
    function GetEditorFont: TFont;
    function GetHighlighterName: string;
    function GetInsertMode: Boolean;
    function GetIsFile: Boolean;
    function GetManager: IEditorManager;
    function GetEncoding: TEncoding;
    function GetEvents: IEditorEvents;
    function GetFileName: string;
    function GetFindHistory: TStrings;
    function GetFoldLevel: Integer;
    function GetForm: TCustomForm;
    function GetHighlighterItem: THighlighterItem;
    function GetHighlighter: TTextEditorHighlighter;
    function GetLineBreakStyle: string;
    function GetLines: TStrings; virtual;
    function GetLinesInWindow: Integer;
    function GetLineText: string;
    function GetLogicalCaretXY: TPoint;
    function GetModified: Boolean;
    function GetMonitorChanges: Boolean;
    function GetName: string;
    function GetOnChange: TNotifyEvent;
    function GetOnDropFiles: TTextEditorDropFilesEvent;
    function GetParent: TWinControl;
    function GetPopupMenu: TPopupMenu; reintroduce;
    function GetReplaceHistory: TStrings;
    function GetSearchText: string;
    function GetSelectionAvailable: Boolean;
    function GetSelectionMode: TTextEditorSelectionMode;
    function GetSelEnd: Integer;
    function GetSelStart: Integer;
    function GetSelectedText: string;
    function GetSelectionLength: Integer;
    function GetSettings: IEditorSettings;
    function GetShowSpecialChars: Boolean;
    function GetText: string;
    function GetTextBetween(AStartPos, AEndPos: TPoint): string;
    function GetTextSize: Integer;
    function GetTopLine: Integer;
    function GetVisible: Boolean;
    procedure SetBlockBegin(const AValue: TPoint);
    procedure SetBlockEnd(const AValue: TPoint);
    procedure SetCaretX(const Value: Integer); virtual;
    procedure SetCaretXY(const AValue: TPoint);
    procedure SetCaretY(const Value: Integer); virtual;
    procedure SetEditorFont(AValue: TFont);
    procedure SetFileName(const AValue: string);
    procedure SetFoldLevel(const AValue: Integer);
    procedure SetHighlighterItem(const AValue: THighlighterItem);
    procedure SetHighlighterName(AValue: string);
    procedure SetInsertMode(AValue: Boolean);
    procedure SetIsFile(AValue: Boolean);
    procedure SetLineBreakStyle(const AValue: string);
    procedure SetLineText(const AValue: string);
    procedure SetLogicalCaretXY(const AValue: TPoint);
    procedure SetModified(const AValue: Boolean);
    procedure SetMonitorChanges(const AValue: Boolean);
    procedure SetName(AValue: string); reintroduce;
    procedure SetOnChange(const AValue: TNotifyEvent);
    procedure SetOnDropFiles(const AValue: TTextEditorDropFilesEvent);
    procedure SetPopupMenu(AValue: TPopupMenu);
    procedure SetSearchText(const Value: string); virtual;
    procedure SetSelectionMode(AValue: TTextEditorSelectionMode);
    procedure SetSelEnd(const AValue: Integer);
    procedure SetSelStart(const AValue: Integer);
    procedure SetSelectedText(const AValue: string);
    procedure SetShowSpecialChars(const AValue: Boolean);
    procedure SetText(const AValue: string);
    procedure SetTextBetween(AStartPos, AEndPos: TPoint; const Value: string);
    procedure SetTopLine(const AValue: Integer);
    procedure SetVisible(const AVisible: Boolean);
    {$ENDREGION}

    procedure ApplySettings;
    procedure InitializeEditor(AEditor: TTextEditor);

  protected
    procedure SetParent(Value: TWinControl); reintroduce;

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure BeginUndoBlock;
    procedure EndUndoBlock;

    procedure CopyToClipboard;
    procedure Cut;
    procedure Copy;
    procedure Paste;
    procedure Undo;
    procedure Redo;

    function EditorViewFocused: Boolean;
    function IEditorView.Focused = EditorViewFocused;

    procedure AssignHighlighterForFileType(const AFileExt: string);
    function RowColumnToCharIndex(APosition: TTextEditorTextPosition): Integer;

    procedure SelectWord;
    procedure FindNextWordOccurrence(ADirectionForward: Boolean);

    procedure Clear;
    procedure SelectAll;

    {$REGION 'event dispatch methods'}
    procedure DoChange; dynamic;
    procedure DoClose(var CloseAction: TCloseAction); override;
    {$ENDREGION}

    function IsActive: Boolean;

    // TCustomForm overrides
    procedure Activate; override;
    procedure UpdateActions; override;

  public
    // constructors and destructors
    procedure AfterConstruction; override;
    destructor Destroy; override;

    function CloseQuery: Boolean; override;
    function Focused: Boolean; override;

    function GetWordAtPosition(const APosition: TPoint): string;
    function GetWordFromCaret(const ACaretPos: TPoint): string;

    procedure Load(const AStorageName: string = '');
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
    procedure Save(const AStorageName: string = '');

    { Column and line of the start of the selected block. }
    property BlockBegin: TPoint
      read GetBlockBegin write SetBlockBegin;

    { Column and line of the end of the selected block. }
    property BlockEnd: TPoint
      read GetBlockEnd write SetBlockEnd;

    { Current position of the caret on the screen. Expanded TABs make this
      position different from LogicalCaretXY. }
    property CaretXY: TPoint
      read GetCaretXY write SetCaretXY;

    { Current position of the caret in the data buffer. }
    property LogicalCaretXY: TPoint
      read GetLogicalCaretXY write SetLogicalCaretXY;

    { current X-coordinate of the caret on the screen. }
    property CaretX: Integer
      read GetCaretX write SetCaretX;

    { current Y-coordinate of the caret on the screen. }
    property CaretY: Integer
      read GetCaretY write SetCaretY;

    property CurrentChar: WideChar
      read GetCurrentChar;

    property CurrentWord: string
      read GetCurrentWord;

    property TextSize: Integer
      read GetTextSize;

    property CanPaste: Boolean
      read GetCanPaste;

    property CanRedo: Boolean
      read GetCanRedo;

    property CanUndo: Boolean
      read GetCanUndo;

    { not working/not supported by TTextEditor }
    property FoldLevel: Integer
      read GetFoldLevel write SetFoldLevel;

    property SelStart: Integer
      read GetSelStart write SetSelStart;

    property SelEnd: Integer
      read GetSelEnd write SetSelEnd;

    property SelectionLength: Integer
      read GetSelectionLength;

    property InsertMode: Boolean
      read GetInsertMode write SetInsertMode;

    property SelectionMode: TTextEditorSelectionMode
      read GetSelectionMode write SetSelectionMode;

    property ShowSpecialChars: Boolean
      read GetShowSpecialChars write SetShowSpecialChars;

    property TextBetween[AStartPos: TPoint; AEndPos: TPoint]: string
      read GetTextBetween write SetTextBetween;

    property Modified: Boolean
      read GetModified write SetModified;

    { Component name }
    property Name: string
      read GetName write SetName;

    property IsFile: Boolean
      read GetIsFile write SetIsFile;

    { Amount of visible lines (not including folds). }
    property LinesInWindow: Integer
      read GetLinesInWindow;

    property Editor: TTextEditor
      read GetEditor;

    property Form: TCustomForm
      read GetForm;

    property LineText: string
      read GetLineText write SetLineText;

    property SelectionAvailable: Boolean
      read GetSelectionAvailable;

    property Manager: IEditorManager
      read GetManager;

    { Reference to the IEditorActions singleton that manages one or more
      IEditView instances. }
    property Actions: IEditorActions
      read GetActions;

    property Commands: IEditorCommands
      read GetCommands;

    { A set of common events to dispatch to the application. }
    property Events: IEditorEvents
      read GetEvents;

    { Global settings shared by all EditorView instances }
    property Settings: IEditorSettings
      read GetSettings;

    { TODO: assign this to active search engine }
    property FindHistory: TStrings
      read GetFindHistory;

    { TODO: assign this to active search engine }
    property ReplaceHistory: TStrings
      read GetReplaceHistory;

    property Highlighter: TTextEditorHighlighter
      read GetHighlighter;

    property HighlighterName: string
      read GetHighlighterName write SetHighlighterName;

    property HighlighterItem: THighlighterItem
      read GetHighlighterItem write SetHighlighterItem;

    { Shortcut to the text contained in the editor. }
    property Lines: TStrings
      read GetLines;

    { Not yet supported. }
    property MonitorChanges: Boolean
      read GetMonitorChanges write SetMonitorChanges;

    property Text: string
      read GetText write SetText;

    property TopLine: Integer
      read GetTopLine write SetTopLine;

    property SearchText: string
      read GetSearchText write SetSearchText;

    { Currently selected text in the editor. }
    property SelectedText: string
      read GetSelectedText write SetSelectedText;

    property FileName: string
      read GetFileName write SetFileName;

    property Parent: TWinControl
      read GetParent write SetParent;

    property Encoding: TEncoding
      read GetEncoding;

    property EditorFont: TFont
      read GetEditorFont write SetEditorFont;

    property LineBreakStyle: string
      read GetLineBreakStyle write SetLineBreakStyle;

    property PopupMenu: TPopupMenu
      read GetPopupMenu write SetPopupMenu;

    property OnChange: TNotifyEvent
      read GetOnChange write SetOnChange;

  end;

implementation

{$R *.dfm}

uses
  System.UITypes, System.IOUtils, System.Math,
  Vcl.GraphUtil,

  TextEditor.Utils,

  Spring,

  DDuce.Editor.Manager;

{$REGION'construction and destruction'}
procedure TEditorView.AfterConstruction;
begin
  inherited AfterConstruction;
  FEditor                 := TTextEditor.Create(Self);
  FFindHistory            := TStringList.Create;
  FFindHistory.Sorted     := True;
  FFindHistory.Duplicates := dupIgnore;

  FReplaceHistory            := TStringList.Create;
  FReplaceHistory.Sorted     := True;
  FReplaceHistory.Duplicates := dupIgnore;

  FIsFile := True;

  InitializeEditor(FEditor);
  Settings.OnChanged.Add(EditorSettingsChanged);
  ApplySettings;
end;

destructor TEditorView.Destroy;
begin
  if Assigned(Settings) then
    Settings.OnChanged.Remove(EditorSettingsChanged);

  FreeAndNil(FReplaceHistory);
  FreeAndNil(FFindHistory);
  FreeAndNil(FEditor);
  inherited Destroy;
end;
{$ENDREGION}

{$REGION'event handlers'}
procedure TEditorView.EditorEnter(Sender: TObject);
begin
  Activate;
end;

procedure TEditorView.EditorReplaceText(const ASender: TObject;
  const AParams: TTextEditorReplaceTextParams;
  var AAction: TTextEditorReplaceAction);
begin
//
end;

procedure TEditorView.EditorCaretChanged(const ASender: TObject; const X, Y:
  Integer; const AOffset : Integer);
begin
  if Assigned(Events) then
    Events.DoCaretPositionChange;
end;

procedure TEditorView.EditorChange(Sender: TObject);
begin
  DoChange;
  if Assigned(Events) then
    Events.DoChange;
end;
{$ENDREGION}

{$REGION'event dispatch methods'}
procedure TEditorView.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TEditorView.DoClose(var CloseAction: TCloseAction);
begin
  CloseAction :=  caFree;
  inherited DoClose(CloseAction);
end;
{$ENDREGION}

{$REGION'property access methods'}
function TEditorView.GetSelectedText: string;
begin
  Result := Editor.SelectedText;
end;

procedure TEditorView.SetSelectedText(const AValue: string);
begin
  Editor.BeginUndoBlock;
  Editor.SelectedText := AValue;
  Editor.EndUndoBlock;
end;

function TEditorView.GetFoldLevel: Integer;
begin
  Result := FFoldLevel;
end;

procedure TEditorView.SetFoldLevel(const AValue: Integer);
begin
  if AValue <> FoldLevel then
  begin
    FFoldLevel := AValue;
    Events.DoModified;
  end;
end;

function TEditorView.GetTextBetween(AStartPos,
  AEndPos: TPoint): string;
begin
  Result := Editor.TextBetween[
    GetPosition(AStartPos.X, AStartPos.Y), GetPosition(AEndPos.X, AEndPos.Y)
  ];
end;

procedure TEditorView.SetTextBetween(AStartPos, AEndPos: TPoint;
  const Value: string);
begin
  Editor.TextBetween[
    GetPosition(AStartPos.X, AStartPos.Y), GetPosition(AEndPos.X, AEndPos.Y)
  ] := Value;
end;

function TEditorView.GetText: string;
begin
  Result := Lines.Text;
end;

procedure TEditorView.SetText(const AValue: string);
begin
  if Lines.Text <> AValue then
  begin
    Lines.Text := AValue;
  end;
end;

function TEditorView.GetCaretX: Integer;
begin
  Result := Editor.TextPosition.Char
end;

procedure TEditorView.SetCaretX(const Value: Integer);
var
  P : TTextEditorTextPosition;
begin
  if Value <> CaretX then
  begin
    P.Char := Value;
    P.Line := CaretY;
    Editor.SetTextPositionAndSelection(P, P, P);
  end;
end;

function TEditorView.GetCaretY: Integer;
begin
  Result := Editor.TextPosition.Line
end;

procedure TEditorView.SetCaretY(const Value: Integer);
var
  P : TTextEditorTextPosition;
begin
  if Value <> CaretY then
  begin
    P.Char := CaretX;
    P.Line := Value;
    Editor.SetTextPositionAndSelection(P, P, P);
  end;
end;

function TEditorView.GetEditorFont: TFont;
begin
  Result := Editor.Fonts.Text;
end;

procedure TEditorView.SetEditorFont(AValue: TFont);
begin
  if not Editor.Fonts.Text.Equals(AValue) then
  begin
    Editor.Fonts.Text.Assign(AValue);
    Editor.Fonts.CodeFoldingHint.Name := AValue.Name;
    Editor.Fonts.Minimap.Name         := AValue.Name;
    Editor.Fonts.Ruler.Name           := AValue.Name;
    Editor.Fonts.LineNumbers.Name     := AValue.Name;
  end;
end;

function TEditorView.GetHighlighter: TTextEditorHighlighter;
begin
  Result := Editor.Highlighter;
end;

function TEditorView.GetHighlighterName: string;
begin
  if Assigned(HighlighterItem) then
    Result := HighlighterItem.Name
  else
    Result := '';
end;

procedure TEditorView.SetHighlighterName(AValue: string);
begin
  if AValue <> HighlighterName then
  begin
    // HighlighterItem should always be assigned
    HighlighterItem := Manager.Highlighters.ItemsByName[AValue];
    Guard.CheckNotNull(HighlighterItem, AValue);
  end;
end;

function TEditorView.GetInsertMode: Boolean;
begin
  Result := Editor.OvertypeMode = omInsert;
end;

procedure TEditorView.SetInsertMode(AValue: Boolean);
begin
  Editor.OvertypeMode := omInsert;
end;

function TEditorView.GetIsFile: Boolean;
begin
  Result := FIsFile;
end;

procedure TEditorView.SetIsFile(AValue: Boolean);
begin
  FIsFile := AValue;
end;

function TEditorView.GetLines: TStrings;
begin
  Result := Editor.Lines;
end;

function TEditorView.GetSearchText: string;
begin
  Result := Editor.SearchString;
end;

procedure TEditorView.SetSearchText(const Value: string);
begin
  if Value <> SearchText then
  begin
    Editor.SearchString := Value;
  end;
end;

function TEditorView.GetTopLine: Integer;
begin
  Result := Editor.TopLine;
end;

procedure TEditorView.SetTopLine(const AValue: Integer);
begin
  Editor.TopLine := AValue;
end;

function TEditorView.GetFileName: string;
begin
  Result := FFileName;
end;

{ TODO: is a setter needed? }

procedure TEditorView.SetFileName(const AValue: string);
begin
  if AValue <> FileName then
  begin
    FFileName := AValue;
//    FEditor.FileName := AValue;
//    Caption := ExtractFileName(AValue);
  end;
end;

function TEditorView.GetEncoding: TEncoding;
begin
  Result := Editor.Lines.Encoding;
end;

function TEditorView.GetLineBreakStyle: string;
begin
  Result := FLineBreakStyle;
end;

procedure TEditorView.SetLineBreakStyle(const AValue: string);
begin
  if AValue <> LineBreakStyle then
  begin
    FLineBreakStyle := AValue;
    Modified        := True;
  end;
end;

function TEditorView.GetLineText: string;
begin
  Result := Editor.Lines[Editor.TextPosition.Line];
end;

procedure TEditorView.SetLineText(const AValue: string);
begin
  Editor.Lines[Editor.TextPosition.Line] := AValue;
end;

{ Not yet supported. }

function TEditorView.GetMonitorChanges: Boolean;
begin
  Result := False;
end;

{ Not yet supported. }

procedure TEditorView.SetMonitorChanges(const AValue: Boolean);
begin
//
end;

function TEditorView.GetName: string;
begin
  Result := inherited Name;
end;

procedure TEditorView.SetName(AValue: string);
begin
  inherited Name := AValue;
end;

function TEditorView.GetOnChange: TNotifyEvent;
begin
  Result := FOnChange;
end;

procedure TEditorView.SetOnChange(const AValue: TNotifyEvent);
begin
  FOnChange := AValue;
end;

function TEditorView.GetOnDropFiles: TTextEditorDropFilesEvent;
begin
  Result := Editor.OnDropFiles;
end;

procedure TEditorView.SetOnDropFiles(const AValue: TTextEditorDropFilesEvent);
begin
  Editor.OnDropFiles := AValue;
end;

function TEditorView.GetParent: TWinControl;
begin
  Result := inherited Parent;
end;

procedure TEditorView.SetParent(Value: TWinControl);
begin
  inherited Parent := Value;
  if Assigned(Parent) then
    Visible := True;
end;

function TEditorView.GetPopupMenu: TPopupMenu;
begin
  Result := PopupMenu;
end;

procedure TEditorView.SetPopupMenu(AValue: TPopupMenu);
begin
  inherited PopupMenu := AValue;
end;

function TEditorView.GetLogicalCaretXY: TPoint;
begin
  Result := Point(Editor.TextPosition.Char, Editor.TextPosition.Line);
end;

procedure TEditorView.SetLogicalCaretXY(const AValue: TPoint);
var
  TP : TTextEditorTextPosition;
begin
  TP.Char := AValue.X;
  TP.Line := AValue.Y;
  Editor.TextPosition := TP;
end;

function TEditorView.GetModified: Boolean;
begin
  Result := Editor.Modified;
end;

procedure TEditorView.SetModified(const AValue: Boolean);
begin
  if AValue <> Modified then
  begin
    Editor.Modified := AValue;
  end;
end;

function TEditorView.GetSelEnd: Integer;
begin
  Result := RowColumnToCharIndex(Editor.SelectionEndPosition);
end;

procedure TEditorView.SetSelEnd(const AValue: Integer);
begin
  Editor.SelectionLength := AValue - Editor.SelectionStart;
end;

function TEditorView.GetSelStart: Integer;
begin
  if SelectionAvailable then
    Result := RowColumnToCharIndex(Editor.SelectionBeginPosition)
  else
    Result := RowColumnToCharIndex(Editor.TextPosition);
end;

procedure TEditorView.SetSelStart(const AValue: Integer);
begin
  Editor.SelectionStart := AValue;
end;

function TEditorView.GetSettings: IEditorSettings;
begin
  Result := Owner as IEditorSettings;
end;

function TEditorView.GetActions: IEditorActions;
begin
  Result := Manager as IEditorActions;
end;

function TEditorView.GetCaretXY: TPoint;
begin
  Result := TPoint(Editor.TextPosition);
end;

procedure TEditorView.SetCaretXY(const AValue: TPoint);
begin
  Editor.TextPosition := GetPosition(AValue.X, AValue.Y);
end;

function TEditorView.GetFindHistory: TStrings;
begin
  Result := FFindHistory;
end;

function TEditorView.GetHighlighterItem: THighlighterItem;
begin
  Result := FHighlighterItem;
end;

procedure TEditorView.SetHighlighterItem(const AValue: THighlighterItem);
begin
  if HighlighterItem <> AValue then
  begin
    FHighlighterItem := AValue;
    if Assigned(AValue) then
    begin
      try
        Editor.Highlighter.LoadFromFile(AValue.LayoutFileName);
        Settings.HighlighterType := FHighlighterItem.Highlighter;
        Actions.UpdateHighLighterActions;
      except
        on E: Exception do
          Logger.SendException('SetHighlighterItem', E);
      end;
    end;
    Events.DoHighlighterChange;
  end;
end;

function TEditorView.GetShowSpecialChars: Boolean;
begin
  Result := Editor.SpecialChars.Visible;
end;

procedure TEditorView.SetShowSpecialChars(const AValue: Boolean);
begin
  Editor.SpecialChars.Visible := AValue;
end;

function TEditorView.GetBlockBegin: TPoint;
begin
  Result.X := Editor.SelectionBeginPosition.Char;
  Result.Y := Editor.SelectionBeginPosition.Line;
end;

procedure TEditorView.SetBlockBegin(const AValue: TPoint);
var
  P : TTextEditorTextPosition;
begin
  P.Char := AValue.X;
  P.Line := AValue.Y;
  Editor.SelectionBeginPosition := P;
end;

function TEditorView.GetBlockEnd: TPoint;
begin
  Result.X := Editor.SelectionEndPosition.Char;
  Result.Y := Editor.SelectionEndPosition.Line;
end;

procedure TEditorView.SetBlockEnd(const AValue: TPoint);
var
  P : TTextEditorTextPosition;
begin
  P.Char := AValue.X;
  P.Line := AValue.Y;
  Editor.SelectionEndPosition:= P;
end;

function TEditorView.GetTextSize: Integer;
begin
  Result := Editor.GetTextLen;
end;

function TEditorView.GetEvents: IEditorEvents;
begin
  Result := Owner as IEditorEvents;
end;

function TEditorView.GetLinesInWindow: Integer;
begin
  Result := Editor.VisibleLineCount;
end;

function TEditorView.GetReplaceHistory: TStrings;
begin
  Result := FReplaceHistory;
end;

function TEditorView.GetSelectionAvailable: Boolean;
begin
  Result := Editor.SelectionAvailable;
end;

function TEditorView.GetSelectionLength: Integer;
begin
  if SelectionAvailable then
    Result := SelEnd - SelStart
  else
    Result := 0;
end;

function TEditorView.GetSelectionMode: TTextEditorSelectionMode;
begin
  Result := Editor.Selection.Mode;
end;

procedure TEditorView.SetSelectionMode(AValue: TTextEditorSelectionMode);
begin
  Editor.Selection.Mode := AValue;
end;

function TEditorView.GetCanPaste: Boolean;
begin
  Result := Editor.CanPaste;
end;

function TEditorView.GetCanRedo: Boolean;
begin
  Result := Editor.CanRedo;
end;

function TEditorView.GetCanUndo: Boolean;
begin
  Result := Editor.CanUndo;
end;

function TEditorView.GetForm: TCustomForm;
begin
  Result := Self;
end;

function TEditorView.GetCurrentWord: string;
begin
  Result := Editor.WordAtCursor;
end;

function TEditorView.GetCommands: IEditorCommands;
begin
  Result := Owner as IEditorCommands;
end;

function TEditorView.GetCurrentChar: WideChar;
begin
  Result := Editor.CharAtCursor;
end;

function TEditorView.GetEditor: TTextEditor;
begin
  Result := FEditor;
end;

function TEditorView.GetManager: IEditorManager;
begin
  if Assigned(Owner) and Supports(Owner, IEditorManager) then
    Result := Owner as IEditorManager
  else
    Result := nil;
end;

function TEditorView.GetVisible: Boolean;
begin
  Result := inherited Visible;
end;

procedure TEditorView.SetVisible(const AVisible: Boolean);
begin
  inherited Visible := AVisible;
end;
{$ENDREGION}

{$REGION'private methods'}
procedure TEditorView.AssignHighlighterForFileType(const AFileExt: string);
begin
  HighlighterItem := Manager.Highlighters.FindHighlighterForFileType(AFileExt);
  if not Assigned(HighlighterItem) then
  begin
    if Settings.AutoGuessHighlighterType then
    begin
      Commands.GuessHighlighterType;
    end;
    if not Assigned(HighlighterItem) then
      HighlighterName := HL_TXT;
  end
end;

function TEditorView.IsActive: Boolean;
begin
  Result := Manager.ActiveView = (Self as IEditorView);
end;

procedure TEditorView.ApplySettings;
begin
  Editor.Tabs.WantTabs        := Settings.EditorOptions.WantTabs;
  Editor.Tabs.Width           := Settings.EditorOptions.TabWidth;
  Editor.WordWrap.Active      := Settings.EditorOptions.WordWrapEnabled;
  Editor.SpecialChars.Visible := Settings.EditorOptions.ShowSpecialCharacters;
  Editor.Minimap.Visible      := Settings.EditorOptions.ShowMinimap;
  Editor.Ruler.Visible        := Settings.EditorOptions.ShowRuler;

  if Settings.EditorOptions.TabsToSpaces then
    Editor.Tabs.Options := Editor.Tabs.Options + [toTabsToSpaces]
  else
    Editor.Tabs.Options := Editor.Tabs.Options - [toTabsToSpaces];

  if Settings.EditorOptions.TabIndent then
    Editor.Tabs.Options := Editor.Tabs.Options + [toSelectedBlockIndent]
  else
    Editor.Tabs.Options := Editor.Tabs.Options - [toSelectedBlockIndent];

  if Settings.EditorOptions.AutoIndent then
    Editor.Options := Editor.Options + [eoAutoIndent]
  else
    Editor.Options := Editor.Options - [eoAutoIndent];

  if Settings.EditorOptions.DragDropEditing then
    Editor.Options := Editor.Options + [eoDragDropEditing]
  else
    Editor.Options := Editor.Options - [eoDragDropEditing];

  if Settings.EditorOptions.TrimTrailingSpaces then
    Editor.Options := Editor.Options + [eoTrimTrailingSpaces]
  else
    Editor.Options := Editor.Options - [eoTrimTrailingSpaces];

  if Settings.EditorOptions.SmartTabs then
    Editor.Tabs.Options := Editor.Tabs.Options + [toPreviousLineIndent]
  else
    Editor.Tabs.Options := Editor.Tabs.Options - [toPreviousLineIndent];

//  if Settings.EditorOptions.ShowIndentGuides then
//    Editor.CodeFolding.Options := Editor.CodeFolding.Options +
//      [cfoShowIndentGuides]
//  else
//    Editor.CodeFolding.Options := Editor.CodeFolding.Options -
//      [cfoShowIndentGuides];

  Editor.RightMargin.Visible  := Settings.EditorOptions.ShowRightEdge;
  Editor.RightMargin.Position := Settings.EditorOptions.RightEdge;

  EditorFont.Assign(Settings.EditorFont);

  Editor.MatchingPairs.Active := Settings.EditorOptions.BracketHighlight;
  Editor.Search.Map.Visible   := Settings.EditorOptions.ShowSearchmap;
  Editor.LineSpacing          := Settings.EditorOptions.ExtraLineSpacing;

  // not supported
  { Settings.EditorOptions.ExtraCharSpacing
    Settings.EditorOptions.BlockTabIndent
    Settings.EditorOptions.BlockIndent
    Settings.EditorOptions.AutoIndentOnPaste
    Settings.EditorOptions.EnhanceHomeKey
    Settings.EditorOptions.EnhanceEndKey
    Settings.EditorOptions.CaretSkipsSelection
    Settings.EditorOptions.CaretSkipsTab
    Settings.EditorOptions.AlwaysVisibleCaret
    Settings.EditorOptions.FoldedCopyPaste
    Settings.EditorOptions.PersistentBlock
    Settings.EditorOptions.OverwriteBlock
    Settings.EditorOptions.AutoHideCursor
    Settings.Colors.MouseLinkColor
    Settings.Colors.IncrementColor
  }
  Editor.Colors.CodeFoldingCollapsedLine := Settings.Colors.FoldedCodeColor;
  Editor.Colors.SelectionBackground      := Settings.Colors.SelectedColor;
  Editor.Colors.ActiveLineBackground     := Settings.Colors.LineHighlightColor;
  Editor.Colors.RightMargin              := Settings.Colors.RightEdgeColor;
  Editor.Colors.MatchingPairMatched      := Settings.Colors.BracketMatchColor;
  Editor.Colors.EditorHighlightedBlockBackground :=
    Settings.Colors.HighlightAllColor;
  Editor.Refresh; // will repaint using the actual highlighter settings
end;

procedure TEditorView.InitializeEditor(AEditor: TTextEditor);
begin
  AEditor.Parent := Self;
  AEditor.Align := alClient;
  AEditor.Fonts.Text.Assign(Settings.EditorFont);
  AEditor.Fonts.CodeFoldingHint.Name := AEditor.Fonts.Text.Name;
  AEditor.Fonts.Minimap.Name         := AEditor.Fonts.Text.Name;
  AEditor.Fonts.Ruler.Name           := AEditor.Fonts.Text.Name;
  AEditor.Fonts.LineNumbers.Name     := AEditor.Fonts.Text.Name;
  AEditor.BorderStyle := bsNone;
  AEditor.DoubleBuffered := True;
  AEditor.Options := [
    eoAutoIndent,
    eoDragDropEditing,
    eoDropFiles,
    eoTrimTrailingSpaces
  ];
  AEditor.Selection.Options := AEditor.Selection.Options + [
    soALTSetsColumnMode,
    soTripleClickRowSelect
  ];
  AEditor.LeftMargin.Autosize := True;
  AEditor.Colors.LeftMarginBackground := clWhite;
  AEditor.LeftMargin.LineNumbers.AutosizeDigitCount := 3;
  AEditor.LeftMargin.Visible := False;
  AEditor.LeftMargin.Visible := True;

  AEditor.CodeFolding.Visible                     := True;
  AEditor.Colors.CodeFoldingCollapsedLine         := clSilver;
  AEditor.Colors.CodeFoldingFoldingLine           := clSilver;
  AEditor.Colors.CodeFoldingFoldingLineHighlight  := clSilver;
  AEditor.Colors.CodeFoldingIndent                := clSilver;
  AEditor.Colors.CodeFoldingIndentHighlight       := clSilver;
  AEditor.Colors.CodeFoldingHintBorder            := clSilver;
  AEditor.CodeFolding.Options := [
   cfoExpandByHintClick,
   cfoHighlightMatchingPair,
   cfoShowTreeLine
  ];

  AEditor.OnChange               := EditorChange;
  AEditor.OnReplaceText          := EditorReplaceText;
  AEditor.OnCaretChanged         := EditorCaretChanged;
  AEditor.OnEnter                := EditorEnter;

  AEditor.Caret.MultiEdit.Active := True;
  AEditor.URIOpener := True;

  ActiveControl := Editor;
end;

procedure TEditorView.EditorSettingsChanged(ASender: TObject);
begin
  FUpdate := True;
end;
{$ENDREGION}

{$REGION'protected methods'}
procedure TEditorView.BeginUndoBlock;
begin
  Editor.BeginUndoBlock;
end;

procedure TEditorView.BeginUpdate;
begin
  Editor.BeginUpdate;
end;

procedure TEditorView.EndUndoBlock;
begin
  Editor.EndUndoBlock;
end;

procedure TEditorView.EndUpdate;
begin
  Editor.EndUpdate;
end;

procedure TEditorView.CopyToClipboard;
begin
  Editor.CopyToClipboard;
end;

procedure TEditorView.Cut;
begin
  if Editor.Focused then
  begin
    if not Editor.SelectionAvailable then
      SelectWord;
    Editor.CutToClipboard;
  end
end;

procedure TEditorView.Copy;
begin
  if Editor.Focused then
  begin
    if not Editor.SelectionAvailable then
    begin
      SelectWord;
    end;
    Editor.CopyToClipboard;
  end
end;

procedure TEditorView.Paste;
begin
  if Editor.Focused and CanPaste then;
  begin
    Editor.PasteFromClipboard;
  end;
end;

procedure TEditorView.Undo;
begin
  Editor.DoUndo;
end;

procedure TEditorView.Redo;
begin
  Editor.DoRedo;
end;

function TEditorView.RowColumnToCharIndex(
  APosition: TTextEditorTextPosition): Integer;
var
  I : Integer;
begin
  Result := 0;
  APosition.Line := Min(Lines.Count, APosition.Line) - 1;
  for I := 0 to APosition.Line do
    Result := Result + Length(Lines[I]) + 2;
  Result := Result + APosition.Char - 1;
end;

{ Make current editor instance the active one in the editor manager. This does
  not automatically make it focused as the current focus can be set to eg. a
  tool window. }

procedure TEditorView.Activate;
begin
  inherited Activate;
  Logger.Track(Self, 'Activate');
  Manager.ActiveView := Self as IEditorView;
end;

function TEditorView.EditorViewFocused: Boolean;
begin
  Result := Editor.Focused;
end;

procedure TEditorView.SelectWord;
begin
  Editor.SelectionBeginPosition := Editor.WordStart;
  Editor.SelectionEndPosition   := Editor.WordEnd;
end;

procedure TEditorView.Clear;
begin
  Editor.Clear;
end;

procedure TEditorView.SelectAll;
begin
  if Editor.Focused then
    Editor.SelectAll
end;

procedure TEditorView.FindNextWordOccurrence(ADirectionForward: Boolean);
begin
  Editor.Search.Options := Editor.Search.Options - [soHighlightResults];
  if ADirectionForward then
  begin
    Editor.TextPosition      := Editor.WordStart;
    //Editor.Search.Options := Editor.Search.Options - [TTextEditorSearchOption.soBackwards];
    Editor.Search.SearchText := Editor.WordAtCursor;
    Editor.FindNext;
  end
  else
  begin
    //Editor.Search.Options := Editor.Search.Options + [TTextEditorSearchOption.soBackwards];
    Editor.Search.SearchText := Editor.WordAtCursor;
    Editor.TextPosition      := Editor.WordStart;
    Editor.SetTextPositionAndSelection(
      Editor.SelectionBeginPosition,
      Editor.SelectionBeginPosition,
      Editor.SelectionEndPosition
    );
  end;
end;

function TEditorView.Focused: Boolean;
begin
  Result := Editor.Focused;
end;

procedure TEditorView.UpdateActions;
var
  B : Boolean;
begin
  inherited UpdateActions;
  B := Focused;
  if not B and Assigned(Parent) then
  begin
    if Parent.Focused then
      B := True;
  end;

  if B then
  begin
  {TODO -oTS -cBug : This call causes a recursive call of Activate}
    //Activate;
  end;
  if Assigned(Actions) then
  begin
//    if not (csDestroying in ComponentState) then
//    begin
//      if Assigned(Actions.ActionList) then
      try
            Actions.UpdateActions;
//        Actions.UpdateActions; // TODO: Abstract error on releasing object if this is placed outside FUpdate condition.
      except
        on E: EAbstractError do
        ;
      end;
    //end;
    if FUpdate then
    begin
      ApplySettings;
      FUpdate := False;
    end;
  end;
end;
{$ENDREGION}

{$REGION'public methods'}
function TEditorView.CloseQuery: Boolean;
var
  MR : TModalResult;
  S  : string;
  V  : IEditorView;
begin
  V := nil;
  Result := inherited CloseQuery;
  if Modified then
  begin
    if Manager.ActiveView <> (Self as IEditorView) then
    begin
      V := Manager.ActiveView;
      Activate;
    end;
    S := Format(SAskSaveChanges, [FileName]);
    MR := MessageDlg(S, mtConfirmation, [mbYes, mbNo, mbCancel], 0);
    if MR = mrYes then
    begin
      Result := Manager.SaveFile;
    end
    else if MR = mrNo then
    begin
      Result := True;
    end
    else
    begin
      // TODO: Why can't we prevent closing by setting Result to False?
      Result := False;
      Abort;
    end;
  end;
  if Assigned(V) then
  begin
    V.Activate;
  end;
end;

{ When IsFile is true this loads the given filenameinto the editor view. When
  IsFile is false, the given storagename is passed to an event which can be
  handled by the owning application to load the content from another resource
  like eg. a database table. }

procedure TEditorView.Load(const AStorageName: string);
var
  S : string;
begin
  Events.DoLoad(AStorageName);
  if IsFile then
  begin
    if (AStorageName <> '') and FileExists(AStorageName) then
      FileName := AStorageName;

    Editor.LoadFromFile(FileName);
//    LineBreakStyle := ALineBreakStyles[GuessLineBreakStyle(Text)];
    S := TPath.GetExtension(FileName);
    S := System.Copy(S, 2, Length(S));
    AssignHighlighterForFileType(S);
    Modified := False;
  end;
end;

procedure TEditorView.LoadFromStream(AStream: TStream);
begin
  Editor.LoadFromStream(AStream);
end;

procedure TEditorView.SaveToStream(AStream: TStream);
begin
  Editor.SaveToStream(AStream);
end;

procedure TEditorView.Save(const AStorageName: string);
var
  FS : TFileStream;
begin
  if IsFile then
  begin
    FS := TFileStream.Create(AStorageName, fmCreate);
    try
      SaveToStream(FS);
    finally
      FreeAndNil(FS);
    end;
  end;
  Modified := False;
end;

function TEditorView.GetWordAtPosition(const APosition: TPoint): string;
begin
  Result := Editor.GetWordAtPixels(APosition.X, APosition.Y);
end;

function TEditorView.GetWordFromCaret(const ACaretPos: TPoint): string;
var
  P : TTextEditorTextPosition;
  D : TTextEditorViewPosition;
begin
  P.Line := ACaretPos.Y;
  P.Char := ACaretPos.X;
  D := Editor.TextToViewPosition(P);
  Result := Editor.GetWordAtPixels(D.Column, D.Row);
end;
{$ENDREGION}

{$REGION'Keyboard shortcuts'}
(*//F1                      Topic Search
//Ctrl+F1                Topic Search
  ecNextEditor: SetResult(VK_F6,[]);
  ecPrevEditor: SetResult(VK_F6,[ssShift]);
  ecWordLeft:   SetResult(VK_A,[ssCtrlOS],VK_LEFT,[ssCtrlOS]);
  ecPageDown:   SetResult(VK_C,[ssCtrlOS],VK_NEXT,[]);
//Ctrl+D                 Moves the cursor right one column, accounting for the
//autoindent setting
//Ctrl+E                 Moves the cursor up one line
//Ctrl+F                 Moves one word right
//Ctrl+G                 Deletes the character to the right of the cursor
//Ctrl+H                 Deletes the character to the left of the cursor
//Ctrl+I                  Inserts a tab
//Ctrl+L                 Search|Search Again
//Ctrl+N                 Inserts a new line
//Ctrl+P                 Causes next character to be interpreted as an ASCII
//sequence
//Ctrl+R                 Moves up one screen
//Ctrl+S                 Moves the cursor left one column, accounting for the
//autoindent setting
//Ctrl+T                 Deletes a word
//Ctrl+V                 Turns insert mode on/off
//Ctrl+W                Moves down one screen
//Ctrl+X                 Moves the cursor down one line
//Ctrl+Y                 Deletes a line
//Ctrl+Z                 Moves the cursor up one line
//Ctrl+Shift+S          Performs an incremental search

//Block commands:
//---------------
//Ctrl+K+B      Marks the beginning of a block
//Ctrl+K+C      Copies a selected block
//Ctrl+K+H      Hides/shows a selected block
//Ctrl+K+I       Indents a block by the amount specified in the Block Indent
//combo box on the General page of the Editor Options dialog box.
//Ctrl+K+K      Marks the end of a block
//Ctrl+K+L       Marks the current line as a block
//Ctrl+K+N      Changes a block to uppercase
//Ctrl+K+O      Changes a block to lowercase
//Ctrl+K+P      Prints selected block
//Ctrl+K+R      Reads a block from a file
//Ctrl+K+T       Marks a word as a block
//Ctrl+K+U      Outdents a block by the amount specified in the Block Indent
//combo box on the General page of the Editor Options dialog box.
//Ctrl+K+V      Moves a selected block
//Ctrl+K+W      Writes a selected block to a file
//Ctrl+K+Y      Deletes a selected block
//Ctrl+O+C      Turns on column blocking
//Ctrl+O+I       Marks an inclusive block
//Ctrl+O+K      Turns off column blocking
//Ctrl+O+L      Marks a line as a block
//Shift+Alt+arrow Selects column-oriented blocks
//Click+Alt+mousemv Selects column-oriented blocks
//Ctrl+Q+B      Moves to the beginning of a block
//Ctrl+Q+K      Moves to the end of a block

//Miscellaneous commands:
//-----------------------
//Ctrl+K+D      Accesses the menu bar
//Ctrl+K+E       Changes a word to lowercase
//Ctrl+K+F       Changes a word to uppercase
//Ctrl+K+S      File|Save (Default and IDE Classic only)
//Ctrl+Q+A      Search|Replace
//Ctrl+Q+F      Search|Find
//Ctrl+Q+Y      Deletes to the end of a line
//Ctrl+Q+[       Finds the matching delimiter (forward)
//Ctrl+Q+Ctrl+[ Finds the matching delimiter (forward)
//Ctrl+Q+]       Finds the matching delimiter (backward)
//Ctrl+Q+Ctrl+] Finds the matching delimiter (backward)
//Ctrl+O+A      Open file at cursor
//Ctrl+O+B      Browse symbol at cursor (Delphi only)
//Alt+right arrow  For code browsing
//Alt +left arrow For code browsing
//Ctrl+O+G      Search|Go to line number
//Ctrl+O+O      Inserts compiler options and directives
//Ctrl+O+U      Toggles case
//Bookmark commands:
//------------------
//Shortcut       Action
//Ctrl+K+0       Sets bookmark 0
//Ctrl+K+1       Sets bookmark 1
//Ctrl+K+2       Sets bookmark 2
//Ctrl+K+3       Sets bookmark 3
//Ctrl+K+4       Sets bookmark 4
//Ctrl+K+5       Sets bookmark 5
//Ctrl+K+6       Sets bookmark 6
//Ctrl+K+7       Sets bookmark 7
//Ctrl+K+8       Sets bookmark 8
//Ctrl+K+9       Sets bookmark 9
//Ctrl+K+Ctrl+0 Sets bookmark 0
//Ctrl+K+Ctrl+1 Sets bookmark 1
//Ctrl+K+Ctrl+2 Sets bookmark 2
//Ctrl+K+Ctrl+3 Sets bookmark 3
//Ctrl+K+Ctrl+4 Sets bookmark 4
//Ctrl+K+Ctrl+5 Sets bookmark 5
//Ctrl+K+Ctrl+6 Sets bookmark 6
//Ctrl+K+Ctrl+7 Sets bookmark 7
//Ctrl+K+Ctrl+8 Sets bookmark 8
//Ctrl+K+Ctrl+9 Sets bookmark 9
//Ctrl+Q+0       Goes to bookmark 0
//Ctrl+Q+1       Goes to bookmark 1
//Ctrl+Q+2       Goes to bookmark 2
//Ctrl+Q+3       Goes to bookmark 3
//Ctrl+Q+4       Goes to bookmark 4
//Ctrl+Q+5       Goes to bookmark 5
//Ctrl+Q+6       Goes to bookmark 6
//Ctrl+Q+7       Goes to bookmark 7
//Ctrl+Q+8       Goes to bookmark 8
//Ctrl+Q+9       Goes to bookmark 9
//Ctrl+Q+Ctrl+0 Goes to bookmark 0
//Ctrl+Q+Ctrl+1 Goes to bookmark 1
//Ctrl+Q+Ctrl+2 Goes to bookmark 2
//Ctrl+Q+Ctrl+3 Goes to bookmark 3
//Ctrl+Q+Ctrl+4 Goes to bookmark 4
//Ctrl+Q+Ctrl+5 Goes to bookmark 5
//Ctrl+Q+Ctrl+6 Goes to bookmark 6
//Ctrl+Q+Ctrl+7 Goes to bookmark 7
//Ctrl+Q+Ctrl+8 Goes to bookmark 8
//Ctrl+Q+Ctrl+9 Goes to bookmark 9
//Cursor movement:
//----------------
//Ctrl+Q+B      Moves to the beginning of a block
//Ctrl+Q+C      Moves to end of a file
//Ctrl+Q+D      Moves to the end of a line
//Ctrl+Q+E      Moves the cursor to the top of the window
//Ctrl+Q+K      Moves to the end of a block
//Ctrl+Q+P      Moves to previous position
//Ctrl+Q+R      Moves to the beginning of a file
//Ctrl+Q+S      Moves to the beginning of a line
//Ctrl+Q+T      Moves the viewing editor so that the current line is placed at
//the top of the window
//Ctrl+Q+U      Moves the viewing editor so that the current line is placed at
//the bottom of the window, if possible
//Ctrl+Q+X      Moves the cursor to the bottom of the window
//System keys:
//------------

//F1              Displays context-sensitive Help
//F2              File|Save
//F3              File|Open
//F4              Run to Cursor
//F5              Zooms window
//F6              Displays the next page
//F7              Run|Trace Into
//F8              Run|Step Over
//F9              Run|Run
//F11             View|Object Inspector
//F12             View|Toggle Form/Unit
//Alt+0           View|Window List
//Alt+F2          View|CPU
//Alt+F3          File|Close
//Alt+F7          Displays previous error in Message view
//Alt+F8          Displays next error in Message view
//Alt+F11        File|Use Unit (Delphi)
//Alt+F11        File|Include Unit Hdr (C++)
//Alt+F12        Displays the Code editor
//Alt+X           File|Exit
//Alt+right arrow  For code browsing forward
//Alt +left arrow For code browsing backward
//Alt +up arrow  For code browsing Ctrl-click on identifier
//Alt+Page Down Goes to the next tab
//Alt+Page Up   Goes to the previous tab
//Ctrl+F1        Topic Search
//Ctrl+F2        Run|Program Reset
//Ctrl+F3        View|Call Stack
//Ctrl+F6        Open Source/Header file (C++)
//Ctrl+F7        Add Watch at Cursor
//Ctrl+F8        Toggle Breakpoint
//Ctrl+F9        Project|Compile project (Delphi)
//Ctrl+F9        Project|Make project (C++)
//Ctrl+F11       File|Open Project
//Ctrl+F12       View|Units
//Shift+F7       Run|Trace To Next Source Line
//Shift+F11      Project|Add To Project
//Shift+F12      View|Forms
//Ctrl+D         Descends item (replaces Inspector window)
//Ctrl+N         Opens a new Inspector window
//Ctrl+S          Incremental search
//Ctrl+T          Displays the Type Cast dialog
  else
    GetDefaultKeyForCommand(Command,TheKeyA,TheKeyB);
  end;
*)
{$ENDREGION}

end.


