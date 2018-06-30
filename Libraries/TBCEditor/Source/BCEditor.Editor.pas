unit BCEditor.Editor;

interface

uses
  BCEditor.Editor.Base, BCEditor.MacroRecorder, BCEditor.Editor.KeyCommands;

type
  TBCCustomEditor = class(TBCBaseEditor)
  strict private
    FDocumentName: string;
    FFilePath: string;
    FFileName: string;
    FFileDateTime: TDateTime;
    FSearchString: string;
    FMacroRecorder: TBCEditorMacroRecorder;
    procedure SetDocumentName(const AName: string);
  protected
    procedure DoOnProcessCommand(var Command: TBCEditorCommand; var AChar: Char; Data: Pointer); override;
  public
    property DocumentName: string read FDocumentName write SetDocumentName;
    property FilePath: string read FFilePath;
    property FileName: string read FFileName;
    property FileDateTime: TDateTime read FFileDateTime write FFileDateTime;
    property MacroRecorder: TBCEditorMacroRecorder read FMacroRecorder write FMacroRecorder;
    property SearchString: string read FSearchString write FSearchString;
  end;

  TBCEditor = class(TBCCustomEditor)
  published
    property About;
    property ActiveLine;
    property Align;
    property Anchors;
    property BorderStyle;
    property Caret;
    property CodeFolding;
    property CompletionProposal;
    property Constraints;
    property Ctl3D;
    property Directories;
    property Enabled;
    property Font;
    property Height;
    property ImeMode;
    property ImeName;
    property KeyCommands;
    property LeftMargin;
    property Lines;
    property LineSpacing;
    property MatchingPair;
    property Minimap;
    property Name;
    property OnAfterBookmarkPlaced;
    property OnAfterDeleteBookmark;
    property OnAfterMarkPanelPaint;
    property OnAfterMarkPlaced;
    property OnAfterDeleteMark;
    property OnAfterLinePaint;
    property OnBeforeCompletionProposalExecute;
    property OnBeforeDeleteMark;
    property OnBeforeMarkPanelPaint;
    property OnBeforeMarkPlaced;
    property OnBeforeTokenInfoExecute;
    property OnMarkPanelLinePaint;
    property OnCaretChanged;
    property OnChange;
    property OnClick;
    property OnCommandProcessed;
    property OnCompletionProposalCanceled;
    property OnCompletionProposalSelected;
    property OnContextHelp;
    property OnCreateFileStream;
    property OnCustomLineColors;
    property OnCustomTokenAttribute;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropFiles;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnLeftMarginClick;
    property OnModified;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnProcessCommand;
    property OnProcessUserCommand;
    property OnReplaceText;
    property OnRightMarginMouseUp;
    property OnScroll;
    property OnSelectionChanged;
    property OnStartDock;
    property OnStartDrag;
    property Options;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property Replace;
    property RightMargin;
    property Scroll;
    property Search;
    property Selection;
    property ShowHint;
    {$if defined(USE_ALPHASKINS)}
    property SkinData;
    {$endif}
    property SpecialChars;
    property SyncEdit;
    property TabOrder;
    property Tabs;
    property TabStop;
    property Tag;
    property TextEntryMode;
    property TokenInfo;
    property Undo;
    property UnknownChars;
    property WantReturns;
    property Width;
    property Visible;
    property WordWrap;
  end;

implementation

uses
  Winapi.Windows, System.Classes, System.SysUtils;

procedure TBCCustomEditor.DoOnProcessCommand(var Command: TBCEditorCommand; var AChar: Char; Data: Pointer);
begin
  inherited;
  if Assigned(FMacroRecorder) then
    if FMacroRecorder.State = msRecording then
      FMacroRecorder.AddEvent(Command, AChar, Data);
end;

procedure TBCCustomEditor.SetDocumentName(const AName: string);
begin
  FDocumentName := AName;
  FFilePath := ExtractFilePath(FDocumentName);
  FFileName := ExtractFileName(FDocumentName);
end;

end.
