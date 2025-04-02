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

unit DDuce.Editor.Commands;

{ Implements IEditorCommands which represents a set of commands that can be
  executed on the active editor view.

  Commands are intended to be called by actions and are typically associated
  with an IEditorView instance.
}

interface

uses
  Winapi.Windows,
  System.Classes, System.SysUtils,

  DDuce.Editor.Interfaces;

type
  TEditorCommands = class(TComponent, IEditorCommands)
  private
    {$REGION 'property access methods'}
    function GetEvents: IEditorEvents;
    function GetManager: IEditorManager;
    function GetSearchEngine: IEditorSearchEngine;
    function GetSettings: IEditorSettings;
    function GetView: IEditorView;
    {$ENDREGION}

    function StripComments(
      const AString      : string;
      const AHighlighter : string
    ): string;
    function MergeBlankLines(const AString: string): string;
    function GuessHighlighterType(const AText: string): string; overload;

    function IsXML(const AString: string): Boolean;
    function IsPAS(const AString: string): Boolean;

  protected
    procedure OpenFileAtCursor;
    procedure ToggleHighlighter;
    procedure AssignHighlighter(const AName: string);
    procedure CopyToClipboard;
    procedure CreateDesktopLink;

    procedure CompressSpace;
    procedure CompressWhitespace;
    procedure UpperCaseSelection;
    procedure LowerCaseSelection;
    procedure PascalStringFromSelection;
    procedure QuoteLinesInSelection(ADelimit : Boolean = False);
    procedure DequoteLinesInSelection;
    procedure QuoteSelection;
    procedure DequoteSelection;
    procedure Base64FromSelection(ADecode: Boolean = False);
    procedure URLFromSelection(ADecode: Boolean = False);
    procedure XMLFromSelection(ADecode: Boolean = False);
    procedure ConvertTabsToSpacesInSelection;
    procedure SyncEditSelection;
    procedure Save;
    function SaveFile(
      const AFileName : string = '';
      AShowDialog     : Boolean = False
    ): Boolean;
    procedure SaveAll;
    procedure AdjustFontSize(AOffset: Integer);

    procedure AlignSelection(
      const AToken            : string;
      ACompressWS             : Boolean;
      AInsertSpaceBeforeToken : Boolean;
      AInsertSpaceAfterToken  : Boolean;
      AAlignInParagraphs      : Boolean
    );
    procedure MergeBlankLinesInSelection;
    procedure StripCommentsFromSelection;
    procedure StripMarkupFromSelection;
    procedure StripCharsFromSelection(
      AFirst : Boolean;
      ALast  : Boolean
    );
    procedure GuessHighlighterType; overload;
    procedure Indent;
    procedure UnIndent;
    procedure ToggleLineComment;
    procedure ToggleBlockComment;
    procedure InsertTextAtCaret(const AText: string);
    procedure FormatCode;
    procedure SortSelectedLines;
    procedure SmartSelect;
    function SelectBlockAroundCursor(
      const AStartTag  : string;
      const AEndTag    : string;
      AIncludeStartTag : Boolean;
      AIncludeEndTag   : Boolean
    ): Boolean;

    procedure FindNext;
    procedure FindPrevious;

    property View: IEditorView
      read GetView;

    property Events: IEditorEvents
      read GetEvents;

    property Settings: IEditorSettings
      read GetSettings;

    property SearchEngine: IEditorSearchEngine
      read GetSearchEngine;

    property Manager: IEditorManager
      read GetManager;

  end;

implementation

uses
  System.Math, System.StrUtils, System.NetEncoding,
  Vcl.Forms,

  TextEditor.KeyCommands, TextEditor.Types,

  DDuce.Editor.Highlighters, DDuce.Editor.Resources, DDuce.Editor.Utils,
  DDuce.Editor.CommentStripper,

  DDuce.Logger;

{$REGION'property access mehods'}
function TEditorCommands.GetEvents: IEditorEvents;
begin
  Result := Manager.Events;
end;

function TEditorCommands.GetManager: IEditorManager;
begin
  Result := Owner as IEditorManager;
end;

function TEditorCommands.GetSearchEngine: IEditorSearchEngine;
begin
  Result := Manager.SearchEngine;
end;

function TEditorCommands.GetSettings: IEditorSettings;
begin
  Result := Manager.Settings;
end;

function TEditorCommands.GetView: IEditorView;
begin
  Result := Manager.ActiveView;
end;
{$ENDREGION}

{$REGION 'private methods'}

{ TODO: Use interfaces like (cfr. ICodeFormatter) }

function TEditorCommands.StripComments(const AString: string;
  const AHighlighter: string): string;
var
  SSIn  : TStringStream;
  SSOut : TStringStream;
  CS    : TCustomCommentStripper;
  C     : Char;
  S     : string;
begin
  CS := nil;
  if AnsiMatchStr(AHighlighter, [HL_PAS]) then
    CS := TPasCommentStripper.Create(nil)
  else if AnsiMatchStr(AHighlighter, [HL_CPP, HL_JAVA, HL_CS]) then
    CS := TCPPCommentStripper.Create(nil);
  if Assigned(CS) then
  begin
    try
      SSIn := TStringStream.Create('');
      try
        SSIn.WriteString(AString);
        C := #0;
        SSIn.Write(C, 1);
        SSIn.Position := 0;
        SSOut := TStringStream.Create('');
        try
          CS.InStream  := SSIn;
          CS.OutStream := SSOut;
          CS.Parse;
          SSOut.Position := 0;
          S := SSOut.ReadString(SSOut.Size);
          S := MergeBlankLines(S);
          Result := S;
        finally
          SSOut.Free;
        end;
      finally
        SSIn.Free;
      end;
    finally
      CS.Free;
    end;
  end
  else
    Result := AString;
end;

function TEditorCommands.MergeBlankLines(const AString: string): string;
var
  SL : TStrings;
begin
  SL := TStringList.Create;
  try
    SL.Text := AString;
    DDuce.Editor.Utils.MergeBlankLines(SL);
    // remove first blank line
    if (SL.Count > 0) and (Trim(SL[0]) = '') then
      SL.Delete(0);
    // remove last blank line
    if (SL.Count > 0) and (Trim(SL[SL.Count - 1]) = '') then
      SL.Delete(SL.Count - 1);
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TEditorCommands.GuessHighlighterType(const AText: string): string;
var
  SL : TStringList;
  S  : string;
begin
  Result := '';
  if Length(AText) > 0 then
  begin
    SL := TStringList.Create;
    try
      SL.Text := Copy(AText, 0, 2000);
      if SL.Count > 0 then
      begin
        S := Trim(SL[0]);
        if IsXML(S) then
          Result := HL_XML
        else
        begin
          S := SL.Text;
          if IsPAS(S) then
            Result := HL_PAS
          else if IsLFM(S) then
            Result := HL_DFM
        end;
        if IsLOG(AText) then
          Result := HL_LOG
        else if IsHTML(AText) then
          Result := HL_HTML
        else if IsXML(AText) then
          Result := HL_XML
        else if IsSQL(AText) then
          Result := HL_SQL;
      end
    finally
      SL.Free;
    end;
  end;
end;

function TEditorCommands.IsXML(const AString: string): Boolean;
const
  MATCH = '^\<\?xml version\=.+\?\>$';
begin
  Result := MatchRegExpr(AString, MATCH, False);
end;

function TEditorCommands.IsPAS(const AString: string): Boolean;
const
  MATCH =  '^(unit|program|package|library) .+;$';
var
  SL : TStrings;
  S  : string;
  B  : Boolean;
begin
  S := StripComments(AString, HL_PAS);
  SL := TStringList.Create;
  try
    SL.Text := S;
    B := False;
    while not B and (SL.Count > 0) do
    begin
      if Trim(SL[0]) = '' then
      begin
        SL.Delete(0);
        B := False;
      end
      else
        B := True;
    end;
    if SL.Count > 0 then
      S := Trim(SL[0]);
    Result := MatchRegExpr(S, MATCH, False);
  finally
    SL.Free;
  end;
end;
{$ENDREGION}

{$REGION'protected methods'}
procedure TEditorCommands.OpenFileAtCursor;
var
  FN : string;
begin
  FN := ExtractFilePath(View.FileName)
    + View.CurrentWord + ExtractFileExt(View.FileName);
  if FileExists(FN) then
    Events.DoNew(FN);
end;

procedure TEditorCommands.ToggleHighlighter;
var
  I : Integer;
  N : Integer;
begin
  if Assigned(View.HighlighterItem) then
  begin
    I := View.HighlighterItem.Index;
    N := Manager.Highlighters.Count;
    View.HighlighterItem := Manager.Highlighters[(I + 1) mod N];
    Settings.HighlighterType := View.HighlighterItem.Name;
  end;
end;

procedure TEditorCommands.AssignHighlighter(const AName: string);
var
  HLI : THighlighterItem;
begin
  if Assigned(Manager.Highlighters) then
  begin
    HLI := Manager.Highlighters.ItemsByName[AName];
    if Assigned(HLI) then
      View.HighlighterItem := HLI
    else
      raise Exception.CreateFmt('Highlighter %s not found!', [AName]);
  end;
end;

procedure TEditorCommands.CopyToClipboard;
begin
  View.Editor.CopyToClipboard;
end;

procedure TEditorCommands.CreateDesktopLink;
//var
//  PIDL     : LPItemIDList;
//  InFolder : array[0..MAX_PATH] of Char;
//  SL       : TShellLink;
begin
  Logger.Warn('Not implemented yet');
//  PIDL := nil;
//  SHGetSpecialFolderLocation(0, CSIDL_DESKTOPDIRECTORY, PIDL) ;
//  SHGetPathFromIDList(PIDL, InFolder) ;
//  SL.Filename := InFolder + '\' + ExtractFileName(View.FileName) + '.lnk';
//  SL.WorkingDir := ExtractFilePath(SL.Filename);
//  SL.ShortcutTo := Application.ExeName;
//  SL.Parameters := View.FileName;
//  CreateShellLink(SL);
end;

procedure TEditorCommands.CompressSpace;
begin
  View.SelectedText := DDuce.Editor.Utils.CompressSpace(View.SelectedText);
end;

procedure TEditorCommands.CompressWhitespace;
begin
  View.SelectedText := DDuce.Editor.Utils.CompressWhitespace(View.SelectedText);
end;

procedure TEditorCommands.UpperCaseSelection;
begin
  //View.Editor.CommandProcessor(ecUpperCaseBlock, #0, nil);
end;

procedure TEditorCommands.LowerCaseSelection;
begin
  //View.Editor.CommandProcessor(ecLowerCaseBlock, #0, nil);
end;

procedure TEditorCommands.PascalStringFromSelection;
begin
  View.SelectedText := PascalStringOf(View.SelectedText);
end;

procedure TEditorCommands.QuoteLinesInSelection(ADelimit: Boolean);
begin
  if ADelimit then
    View.SelectedText := QuoteLinesAndDelimit(View.SelectedText)
  else
    View.SelectedText := QuoteLines(View.SelectedText);
end;

procedure TEditorCommands.DequoteLinesInSelection;
begin
  View.SelectedText := DequoteLines(View.SelectedText);
end;

procedure TEditorCommands.QuoteSelection;
begin

  View.SelectedText := AnsiQuotedStr(View.SelectedText, '''');
end;

procedure TEditorCommands.DequoteSelection;
begin
  View.SelectedText := AnsiDequotedStr(View.SelectedText, '''');
end;

procedure TEditorCommands.Base64FromSelection(ADecode: Boolean);
begin
  if ADecode then
    View.SelectedText := TNetEncoding.Base64.Decode(View.SelectedText)
  else
    View.SelectedText := TNetEncoding.Base64.Encode(View.SelectedText);
  View.Modified := True;
end;

procedure TEditorCommands.URLFromSelection(ADecode: Boolean);
begin
  if ADecode then
    View.SelectedText := TNetEncoding.URL.Decode(View.SelectedText)
  else
    View.SelectedText := TNetEncoding.URL.Encode(View.SelectedText);
  View.Modified := True;
end;

procedure TEditorCommands.XMLFromSelection(ADecode: Boolean);
begin
  if ADecode then
    View.SelectedText := TNetEncoding.HTML.Decode(View.SelectedText)
  else
    View.SelectedText := TNetEncoding.HTML.Encode(View.SelectedText);
  View.Modified := True;
end;

procedure TEditorCommands.ConvertTabsToSpacesInSelection;
begin
  View.SelectedText :=
    TabsToSpaces(View.SelectedText, Settings.EditorOptions.TabWidth);
end;

procedure TEditorCommands.SyncEditSelection;
begin
  //View.Editor.SyncEdit.BlockBeginPosition := View.Editor.SelectionBeginPosition;
  //View.Editor.SyncEdit.BlockEndPosition   := View.Editor.SelectionEndPosition;

//View.Editor.CommandProcessor(ecSynPSyncroEdStart, '', nil);

end;

procedure TEditorCommands.Save;
begin
  if View.IsFile then
    SaveFile(View.FileName)
  else
  begin
    View.Save;
  end;
end;

function TEditorCommands.SaveFile(const AFileName: string; AShowDialog: Boolean
  ): Boolean;
begin
  // { TODO -oTS : Migrate implementation to here. }.
  Result := Manager.SaveFile(AFileName, AShowDialog);
end;

procedure TEditorCommands.SaveAll;
var
  V : IEditorView;
begin
  for V in Manager.Views do
  begin
    if V.Modified then
    begin
      if V.IsFile then
      begin
        Manager.SaveFile(V.FileName);
      end
      else
        V.Save;
    end;
  end;
end;

procedure TEditorCommands.AdjustFontSize(AOffset: Integer);
begin
  Settings.EditorFont.Size := Settings.EditorFont.Size + AOffset;
  Settings.Apply; // needed to trigger settings changed event.
end;

{ TODO -oTS : Align in paragraphs does not work!
  TODO -oTS : Align to leftmost/rightmost token not implemented!
}

procedure TEditorCommands.AlignSelection(const AToken: string;
  ACompressWS: Boolean; AInsertSpaceBeforeToken: Boolean;
  AInsertSpaceAfterToken: Boolean; AAlignInParagraphs: Boolean);
begin
  if View.SelectionAvailable then
  begin
    //View.Editor.Selection.
  //  Selection.Store(True, True);
//    AlignLines(
//      Selection.Lines,
//      AToken,
//      ACompressWS,
//      AInsertSpaceBeforeToken,
//      AInsertSpaceAfterToken
//    );
//    Selection.Restore;
  end;
end;

procedure TEditorCommands.MergeBlankLinesInSelection;
begin
  View.SelectedText := MergeBlankLines(View.SelectedText);
end;

procedure TEditorCommands.StripCommentsFromSelection;
begin
  //View.Editor.KeyCommands
  //View.SelectedText := StripComments()
end;

{ TODO -oTS : Not working! }

procedure TEditorCommands.StripMarkupFromSelection;
begin
  View.SelectedText := StripMarkup(View.SelectedText);
end;

{ REMARK:
    Whitespace is ignored. This routine strips the first/last non-space char
    from each line in the selection.
}

procedure TEditorCommands.StripCharsFromSelection(AFirst: Boolean;
  ALast: Boolean);
begin
  View.SelectedText := StripChars(View.SelectedText, AFirst, ALast);
end;

procedure TEditorCommands.GuessHighlighterType;
var
  S : string;
begin
  S := GuessHighlighterType(View.Text);
  if S <> '' then
    AssignHighlighter(S);
end;

procedure TEditorCommands.Indent;
begin
  View.Editor.CommandProcessor(TKeyCommands.BlockIndent, #0, nil);
end;

procedure TEditorCommands.UnIndent;
begin
  View.Editor.CommandProcessor(TKeyCommands.BlockUnindent, #0, nil);
end;

{ Comments or uncomments selected code lines based on the active highlighter. }
procedure TEditorCommands.ToggleLineComment;
begin
  View.Editor.CommandProcessor(TKeyCommands.LineComment, #0, nil);
end;

{ Comments/uncomments the selected block with the block comment tags for the
  current highlighter. }

procedure TEditorCommands.ToggleBlockComment;
begin
  View.Editor.CommandProcessor(TKeyCommands.BlockComment, #0, nil);
end;

procedure TEditorCommands.InsertTextAtCaret(const AText: string);
begin
  View.Editor.InsertBlock(
    View.Editor.SelectionBeginPosition,
    View.Editor.SelectionEndPosition,
    PWideChar(AText),
    True
  );
end;

procedure TEditorCommands.FormatCode;
//var
//  HI : THighlighterItem;
begin
//  HI := View.HighlighterItem;
//  if Assigned(HI) then
//    if Assigned(HI.CodeFormatter) then
//    begin
//      if not View.SelAvail then
//      begin
//        View.SelectAll;
//      end;
//      Selection.Store;
//      if Length(Trim(Selection.Text))>0 then
//         Selection.Text := HI.CodeFormatter.Format(Selection.Text);
//      Selection.Restore;
//    end
//    else
//      raise Exception.Create('No codeformatter for current highlighter');
end;

procedure TEditorCommands.SortSelectedLines;
begin
  View.Editor.BeginUndoBlock;
  View.Editor.Sort([soRandom]);
  View.Editor.EndUndoBlock;
end;

{ Makes a smart selection of a block around the cursor. }

{ TODO -oTS : Make this configurable per highlighter (see unit
  ts.Editor.CodeTags).  }

procedure TEditorCommands.SmartSelect;
//var
//  HI : THighlighterItem;
begin
//  HI := View.HighlighterItem;
//  if Assigned(HI) then
//  begin
//    if HI.Name = 'XML' then
//      SelectBlockAroundCursor('>', '<', False, False)
//    else if HI.Name = 'PAS' then
//      SelectBlockAroundCursor('begin', 'end', True, True)
//    else if HI.Name = 'LOG' then
//      SelectBlockAroundCursor('<XMLRoot>', '</XMLRoot>', True, True);
//  end;
end;

{ Selects block of code around cursor between AStartTag and AEndTag. Used by
  the SmartSelect procedure.

  TODO:
    - support for nested AStartTag and AEndTag (ignore sublevels)
}

function TEditorCommands.SelectBlockAroundCursor(const AStartTag: string;
  const AEndTag: string; AIncludeStartTag: Boolean;
  AIncludeEndTag: Boolean): Boolean;
var
  Pos : Integer;
  S   : string;
  B   : Boolean;
  I   : Integer;
  N   : Integer;
begin
  N := 0;
  Result := False;
  if (AStartTag = '') or (AEndTag = '') then
    Exit;

  S := View.Text;
  Pos := View.SelStart;
  B := False;
  while not B and (Pos > 1) do
  begin
    N := Length(AStartTag);
    I := N;
    B := S[Pos] = AStartTag[I];
    while B and (Pos > 1) and (I > 1) do
    begin
      Dec(I);
      Dec(Pos);
      B := S[Pos] = AStartTag[I];
    end;
    if not B and (Pos > 1) then
      Dec(Pos);
  end;
  if B then
  begin
    if AIncludeStartTag then
      View.SelStart := Pos
    else
      View.SelStart := Pos + N;
  end;

  if B then
  begin
    Pos := View.SelStart;
    B := False;
    while not B and (Pos <= Length(S)) do
    begin
      N := Length(AEndTag);
      I := 1;
      B := S[Pos] = AEndTag[I];
      while B and (Pos <= Length(S)) and (I < N) do
      begin
        Inc(I);
        Inc(Pos);
        B := S[Pos] = AEndTag[I];
      end;
      if not B and (Pos <= Length(S)) then
        Inc(Pos);
    end;
    if B then
    begin
      if AIncludeEndTag then
        View.SelEnd := Pos + 1
      else
        View.SelEnd := Pos - N + 1;
    end;
  end;
  Result := View.SelectionAvailable;
end;

procedure TEditorCommands.FindNext;
begin
  View.Editor.FindNext;
end;

procedure TEditorCommands.FindPrevious;
begin
  View.Editor.FindPrevious;
end;
{$ENDREGION}

end.

