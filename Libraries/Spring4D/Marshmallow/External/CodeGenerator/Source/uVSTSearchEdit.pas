(* KlasifSelect - uVSTSearchEdit.pas
* Created: 2012-04-05 13:54:44
* Copyright (c) 2012, Linas Naginionis
* Contacts: lnaginionis@gmail.com or support@soundvibe.net or linas@vikarina.lt
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the <organization> nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uVSTSearchEdit;

interface

{$I JEDI.inc}

uses
  SysUtils, Classes, VirtualTrees, StdCtrls, ComCtrls, StrUtils, Types, Menus, ExtCtrls;

type
  TVSTSearchEdit = class;

  TOnAfterSearchEvent = procedure(ASender : TVSTSearchEdit; ASearchText : String) of object;

  TVSTSearchEdit = class(TComponent)
  private
    FTree: TVirtualStringTree;
    FProgress: TProgressBar;
    FEdit: TCustomEdit;
    FOldOnChangeProc : TNotifyEvent;
    FOnAfterSearch: TOnAfterSearchEvent;
    FMaxLevel: Integer;
    FArrColumns: array of string;
    FPopupMenu: TPopupMenu;
    FTimer: TTimer;

    procedure DoSearch(const AWords : TStrings); overload;
    procedure DoSearch(const AWords : TStringDynArray); overload;
    function NodeMatches(const ANode : PVirtualNode; const AWords : TStrings) : Boolean; overload;
    function NodeMatches(const ANode : PVirtualNode; const AWords : TStringDynArray) : Boolean; overload;

    procedure OnEditChange(Sender : TObject);
    procedure SetEdit(const Value: TCustomEdit);
    procedure DetachFromEdit(const Value: TCustomEdit);
    procedure AttachToEdit(const Value: TCustomEdit);
    procedure SetTree(const Value: TVirtualStringTree);
    function GetTimerInterval: Cardinal;
    procedure SetTimerInterval(const Value: Cardinal);
  protected
    { Protected-Deklarationen }
    function WordHasQuotes(const AWord: string; out ADequotedWord: string): Boolean;
    procedure DoInitPopupMenu(Sender: TObject);
    function MenuExists(const AInIndex: Integer): Boolean;
    function GetColumnChecked(AColumnIndex: TColumnIndex): Boolean; virtual;
    procedure DoOnTimer(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    procedure RefreshMenu();
    procedure RunSearch; overload;
    procedure RunSearch(AText : String); overload;
    function TestNode(const ANode: PVirtualNode): Boolean; overload;
    function TestNode(const ANode: PVirtualNode; AText : String): Boolean; overload;
  published
    property OnAfterSearch : TOnAfterSearchEvent read FOnAfterSearch write FOnAfterSearch;
    property VST : TVirtualStringTree read FTree write SetTree;
    property Edit : TCustomEdit read FEdit write SetEdit;
    property Progress : TProgressBar read FProgress write FProgress;
    property MaxLevel : Integer read FMaxLevel write FMaxLevel default -1;
    property PopupMenu: TPopupMenu read FPopupMenu;
    property TimerInterval: Cardinal read GetTimerInterval write SetTimerInterval default 500;
  end;


implementation

uses
  Forms, uVirtualTreeHelpers, Controls {$ifndef DELPHI12_UP}, TntStdCtrls{$endif};

{ TVSTSearchEdit }

constructor TVSTSearchEdit.Create(AOwner: TComponent);
begin
  inherited;
  FMaxLevel := -1;
  FPopupMenu := TPopupMenu.Create(Self);
  FPopupMenu.OnPopup := DoInitPopupMenu;
  VST := nil;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 500;
  FTimer.OnTimer := DoOnTimer;
end;

destructor TVSTSearchEdit.Destroy;
begin
  DetachFromEdit(FEdit);
  FPopupMenu.Free;
  FTimer.Free;
  inherited;
end;

procedure TVSTSearchEdit.DoInitPopupMenu(Sender: TObject);
var
  LPopup: TPopupMenu;
begin
  LPopup := Sender as TPopupMenu;
  if Assigned(FTree) then
  begin
    if (LPopup.Items.Count <> FTree.Header.Columns.Count)  then
    begin
      RefreshMenu();
    end;
  end;
end;

procedure TVSTSearchEdit.DoOnTimer(Sender: TObject);
begin
  FTimer.Enabled := False;
  RunSearch();
end;

procedure TVSTSearchEdit.DoSearch(const AWords: TStringDynArray);
var
  Node,
  NewNode : PVirtualNode;
  Matches : Boolean;
begin
  FTree.BeginUpdate;
  try
    if Assigned(FProgress) then
    begin
      FProgress.Position:=0;
      FProgress.Max:=FTree.ChildCount[FTree.RootNode];
    end;

    Node:=FTree.GetFirst;

    while Assigned(Node) do
    begin
      if Assigned(FProgress) then
        FProgress.Position:=FProgress.Position+1;

      if Length(AWords)>0 then
        Matches := NodeMatches(Node, AWords)
      else
        Matches := true;


      FTree.FullyVisible[Node] := Matches;

      if not Matches then
      begin
        if FMaxLevel > -1 then
        begin
          if FTree.GetNodeLevel(Node) < Cardinal(FMaxLevel) then
            NewNode := FTree.GetFirstChild(Node)
          else
            NewNode := FTree.GetNextSibling(Node)
        end
        else
          NewNode := FTree.GetFirstChild(Node)
      end
      else
      begin
        FTree.VisibleRecursive[Node] := true;
        NewNode := FTree.GetNextSibling(Node);
      end;

      if not Assigned(NewNode) then
        NewNode := FTree.GetNext(Node);

      Node := NewNode;
    end;
  finally
    FTree.EndUpdate;
    if Assigned(FProgress) then
      FProgress.Position:=0;
  end;

end;

function TVSTSearchEdit.GetColumnChecked(AColumnIndex: TColumnIndex): Boolean;
begin
  Result := (coVisible in FTree.Header.Columns[AColumnIndex].Options);
end;

function TVSTSearchEdit.GetTimerInterval: Cardinal;
begin
  Result := FTimer.Interval;
end;

function TVSTSearchEdit.MenuExists(const AInIndex: Integer): Boolean;
begin
  Result := (AInIndex > -1) and (AInIndex < FPopupMenu.Items.Count);
end;

procedure TVSTSearchEdit.DoSearch(const AWords: TStrings);
var
  Node,
  NewNode : PVirtualNode;
  Matches : Boolean;
begin
  FTree.BeginUpdate;
  try
    if Assigned(FProgress) then
    begin
      FProgress.Position:=0;
      FProgress.Max:=FTree.ChildCount[FTree.RootNode];
    end;

    Node:=FTree.GetFirst;

    while Assigned(Node) do
    begin
      if Assigned(FProgress) then
        FProgress.Position:=FProgress.Position+1;

      if AWords.Count>0 then
        Matches := NodeMatches(Node, AWords)
      else
        Matches := true;


      FTree.FullyVisible[Node] := Matches;

      if not Matches then
      begin
        if FMaxLevel > -1 then
        begin
          if FTree.GetNodeLevel(Node) < Cardinal(FMaxLevel) then
            NewNode := FTree.GetFirstChild(Node)
          else
            NewNode := FTree.GetNextSibling(Node)
        end
        else
          NewNode := FTree.GetFirstChild(Node)
      end
      else
      begin
        FTree.VisibleRecursive[Node] := true;
        NewNode := FTree.GetNextSibling(Node);
      end;

      if not Assigned(NewNode) then
        NewNode := FTree.GetNext(Node);

      Node := NewNode;
    end;
  finally
    FTree.EndUpdate;
    if Assigned(FProgress) then
      FProgress.Position:=0;
  end;
end;

function TVSTSearchEdit.NodeMatches(const ANode: PVirtualNode;
  const AWords: TStringDynArray): Boolean;
var
  CurWord : String;
  s: string;
  i: Integer;
  bFound: Boolean;
begin
  Result := True;
  bFound := False;

  if FTree.Header.Columns.Count>0 then
  begin
    for i:=0 to FTree.Header.Columns.Count-1 do
    begin
      if MenuExists(i) and (FPopupMenu.Items[i].Checked) then
     // if (coVisible in FTree.Header.Columns[i].Options) and (FTree.Header.Columns[i].CheckBox) then
        FArrColumns[i] := UpperCase(FTree.Text[ANode, i])
      else
        FArrColumns[i] := '';
    end;
  end
  else
  begin
    FArrColumns[0] := UpperCase(FTree.Text[ANode, -1]);
  end;

  for CurWord in AWords do
  begin
    S := CurWord;

    for i := Low(FArrColumns) to High(FArrColumns) do
    begin
      //check if string is quoted
      if WordHasQuotes(CurWord, s) then
      begin
        bFound := SameStr(S, FArrColumns[i]);
      end
      else
      begin
        bFound := PosEx(S, FArrColumns[i]) > 0;
      end;

      if bFound then Break;
    end;

    if not bFound then
      Exit(False);
  end;
end;

function TVSTSearchEdit.NodeMatches(const ANode: PVirtualNode;
  const AWords: TStrings): Boolean;
var
  CurWord : String;
  idxField : Integer;
  NodeStr : String;
begin
  Result:=true;
  NodeStr:='';

  if FTree.Header.Columns.Count>0 then
  begin
    for idxField:=0 to FTree.Header.Columns.Count-1 do
      NodeStr:=NodeStr+UpperCase(FTree.Text[ANode, idxField]) + '@';
  end
  else
    NodeStr:=UpperCase(FTree.Text[ANode, -1]);

  for CurWord in AWords do
  begin
    Result:=PosEx(CurWord, NodeStr) > 0;
    if not Result then exit;
  end;
end;

procedure TVSTSearchEdit.OnEditChange(Sender: TObject);
begin
  FTimer.Enabled := False;
  FTimer.Enabled := True;

 // RunSearch;

  if Assigned(FOldOnChangeProc) then
    FOldOnChangeProc(Sender);
end;

procedure TVSTSearchEdit.RunSearch;
begin
  if Assigned(FEdit) then
    RunSearch(FEdit.Text);
end;

procedure TVSTSearchEdit.RefreshMenu;
var
  i: Integer;
  mi: TMenuItem;
begin
  FPopupMenu.Items.Clear;

  if not Assigned(FTree) then
    Exit;

  for i := 0 to FTree.Header.Columns.Count - 1 do
  begin
    mi := TMenuItem.Create(FPopupMenu);
    mi.Caption := FTree.Header.Columns[i].Text;
    mi.Tag := i;
    mi.Checked := GetColumnChecked(i);
    mi.AutoCheck := True;
    //mi.OnClick := DoCheckMenu;

    FPopupMenu.Items.Add(mi);
  end;
end;

procedure TVSTSearchEdit.RunSearch(AText: String);
begin
  if not Assigned(FTree) then exit;
  FTree.BeginUpdate;
  Screen.Cursor:=crHourGlass;
 // Words:=TStringList.Create;
  try
    if FTree.Header.Columns.Count > 0 then
    begin
      SetLength(FArrColumns, FTree.Header.Columns.Count);
    end
    else
    begin
      SetLength(FArrColumns, 1);
    end;

  //  Words.Delimiter:=#32;
   // Words.DelimitedText:= UpperCase(AText);
    DoSearch(SplitString(UpperCase(AText), ' '));
  finally
  //   Words.Free;
    FTree.EndUpdate;
    Screen.Cursor:=crDefault;
  end;

  if Assigned(FOnAfterSearch) then
    FOnAfterSearch(Self, AText);
end;

procedure TVSTSearchEdit.DetachFromEdit(const Value: TCustomEdit);
begin
  if Assigned(Value) then
  begin
    if Value is TEdit then
      TEdit(Value).OnChange:=FOldOnChangeProc
    {$ifndef DELPHI12_UP}
    else
    if Value is TTntEdit then
      TTNTEdit(Value).OnChange:=FOldOnChangeProc;
    {$endif}
  end;
end;



type
  THackedEdit = class(TCustomEdit);

procedure TVSTSearchEdit.AttachToEdit(const Value: TCustomEdit);
begin
  if Assigned(Value) then
  begin
    if Value is TCustomEdit then
    begin
      FOldOnChangeProc:=THackedEdit(FEdit).OnChange;
      THackedEdit(FEdit).OnChange:=OnEditChange;
    end
    {$ifndef DELPHI12_UP}else
    if Value is TTntEdit then
    begin
      FOldOnChangeProc:=TTNTEdit(FEdit).OnChange;
      TTNTEdit(FEdit).OnChange:=OnEditChange;
    end;{$endif}
  end;
end;

procedure TVSTSearchEdit.SetEdit(const Value: TCustomEdit);
begin
  if FEdit<>Value then
  begin
    if not (csDesigning in ComponentState) then
      DetachFromEdit(FEdit);

    FEdit := Value;

    if not (csDesigning in ComponentState) then
      AttachToEdit(FEdit);
  end;
end;

procedure TVSTSearchEdit.SetTimerInterval(const Value: Cardinal);
begin
  FTimer.Interval := Value;
end;

procedure TVSTSearchEdit.SetTree(const Value: TVirtualStringTree);
begin
  FTree := Value;
  //refresh popup menu
  RefreshMenu();
end;

function TVSTSearchEdit.TestNode(const ANode: PVirtualNode): Boolean;
begin
  Result:=Assigned(FEdit) and TestNode(ANode, FEdit.Text);
end;

function TVSTSearchEdit.TestNode(const ANode: PVirtualNode;
  AText: String): Boolean;
var
  Words : TStringList;
begin
  Result:=False;
  if not Assigned(FTree) then exit;

  Words:=TStringList.Create;
  Words.Delimiter:=#32;
  Words.DelimitedText:=AText;
  Result:=NodeMatches(ANode, Words);
  Words.Free;
end;




function TVSTSearchEdit.WordHasQuotes(const AWord: string; out ADequotedWord: string): Boolean;
begin
  Result := False;
  ADequotedWord := AWord;
  if Length(AWord) > 2 then
  begin
    Result := (AWord[1] = '"') and (AWord[Length(AWord)] = '"');
    if Result then
    begin
      ADequotedWord := AnsiDequotedStr(AWord, '"');
    end;
  end;
end;

end.
