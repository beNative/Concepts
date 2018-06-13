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

unit DDuce.Editor.Search.Engine;

{ Search logic to find text in one or all managed editor views. }

{
  Refactoring notes (20/11/2013, TS)

  - no more core search functionality in the toolview. All operations should be
    handled by the engine and any seach form who handles the multicast events
    dispatched by the engine should be able to interact with the user.
  - typical MVC configuration

  Search
    invoke the search dialog or form to enter search input

  Find
    Execute the searchengine to find matches

  Replace
    Replace search matches

}

interface

uses
  System.Classes, System.SysUtils, System.Types,

  Spring, Spring.Collections,

  BCEditor.Types, BCEditor.Search,

  DDuce.Editor.Interfaces, DDuce.Editor.Search.Engine.Settings,
  DDuce.Editor.Search.Data,

  DDuce.Logger;

type
  TSearchEngine = class(TComponent, IEditorSearchEngine)
  private
    FSearchText   : string;
    FReplaceText  : string;
    FOptions      : TBCEditorSearchOptions;
    FItemGroups   : IList<TSearchResultGroup>;
    FItemList     : IList<TSearchResult>;
    FCurrentIndex : Integer;
    FOnChange     : Event<TNotifyEvent>;
    FOnExecute    : Event<TNotifyEvent>;

    {$REGION 'property access mehods'}
    function GetCurrentIndex: Integer;
    function GetItemGroups: IList<TSearchResultGroup>;
    function GetItemList: IList<TSearchResult>;
    function GetManager: IEditorManager;
    function GetOptions: TBCEditorSearchOptions;
    function GetReplaceText: string;
    function GetSearchAllViews: Boolean;
    function GetSearchText: string;
    function GetSettings: TSearchEngineSettings;
    function GetView: IEditorView;
    function GetViews: IEditorViews;
    function GetOnChange: IEvent<TNotifyEvent>;
    function GetOnExecute: IEvent<TNotifyEvent>;
    procedure SetCurrentIndex(AValue: Integer);
    procedure SetOptions(AValue: TBCEditorSearchOptions);
    procedure SetReplaceText(AValue: string);
    procedure SetSearchAllViews(AValue: Boolean);
    procedure SetSearchText(AValue: string);
    {$ENDREGION}

    procedure DoExecute;
    procedure DoChange;

  protected
    procedure AddResultsForView(AView: IEditorView);
    function PosToLineCol(
      const AString : string;
      const AOffset : TPoint;
            APos    : Integer
    ): TPoint;

    { IEditorSearchEngine }
    procedure Execute;
    procedure FindNext;
    procedure FindPrevious;
    procedure Replace;
    procedure ReplaceAll;

    property Manager: IEditorManager
      read GetManager;

    property CurrentIndex: Integer
      read GetCurrentIndex write SetCurrentIndex;

    property Views: IEditorViews
      read GetViews;

    property View: IEditorView
      read GetView;

    property Options: TBCEditorSearchOptions
      read GetOptions write SetOptions;

    property SearchText : string
      read GetSearchText write SetSearchText;

    property ReplaceText: string
      read GetReplaceText write SetReplaceText;

    property SearchAllViews: Boolean
      read GetSearchAllViews write SetSearchAllViews;

    property Settings: TSearchEngineSettings
      read GetSettings;

    property ItemGroups: IList<TSearchResultGroup>
      read GetItemGroups;

    property ItemList: IList<TSearchResult>
      read GetItemList;

    property OnChange: IEvent<TNotifyEvent>
      read GetOnChange;

    property OnExecute: IEvent<TNotifyEvent>
      read GetOnExecute;

  public
    procedure AfterConstruction; override;
  end;

implementation

uses
  DDuce.Editor.Utils;

const
  MAX_RESULTS = 10000;

{$REGION 'construction and destruction'}
procedure TSearchEngine.AfterConstruction;
begin
  inherited AfterConstruction;
  FItemGroups := TCollections.CreateObjectList<TSearchResultGroup>;
  FItemList   := TCollections.CreateList<TSearchResult>(False);
  Options     := Settings.Options;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function TSearchEngine.GetItemList: IList<TSearchResult>;
begin
  Result := FItemList;
end;

function TSearchEngine.GetManager: IEditorManager;
begin
  Result := Owner as IEditorManager;
end;

function TSearchEngine.GetCurrentIndex: Integer;
begin
  Result := FCurrentIndex;
end;

procedure TSearchEngine.SetCurrentIndex(AValue: Integer);
begin
  FCurrentIndex := AValue;
end;

function TSearchEngine.GetItemGroups: IList<TSearchResultGroup>;
begin
  Result := FItemGroups;
end;

function TSearchEngine.GetReplaceText: string;
begin
  Result := FReplaceText;
end;

procedure TSearchEngine.SetReplaceText(AValue: string);
begin
  if AValue <> ReplaceText then
  begin
    FReplaceText := AValue;
    DoChange;
  end;
end;

function TSearchEngine.GetSearchText: string;
begin
  Result := FSearchText;
end;

function TSearchEngine.GetSettings: TSearchEngineSettings;
begin
  Result := (Manager as IEditorSettings)
    .ToolSettings.ItemsByClass[TSearchEngineSettings] as TSearchEngineSettings;
end;

procedure TSearchEngine.SetSearchText(AValue: string);
begin
  if AValue <> SearchText then
  begin
    FSearchText := AValue;
    DoChange;
  end;
end;

function TSearchEngine.GetView: IEditorView;
begin
  Result := Owner as IEditorView;
end;

function TSearchEngine.GetViews: IEditorViews;
begin
  Result := Owner as IEditorViews;
end;

function TSearchEngine.GetOnChange: IEvent<TNotifyEvent>;
begin
  Result := FOnChange;
end;

function TSearchEngine.GetOnExecute: IEvent<TNotifyEvent>;
begin
  Result := FOnExecute;
end;

function TSearchEngine.GetOptions: TBCEditorSearchOptions;
begin
  Result := FOptions;
end;

procedure TSearchEngine.SetOptions(AValue: TBCEditorSearchOptions);
begin
  if AValue <> Options then
  begin
//    FOptions := AValue;
    View.Editor.Search.Options := AValue;
    Settings.Options := AValue;
//    FEditorSearch.WholeWordsOnly := soWholeWordsOnly in Options;
//    FEditorSearch.CaseSensitive  := soCaseSensitive in Options;



//    FSESearch.RegularExpressions := ssoRegExpr in Options;
//    FSESearch.RegExprMultiLine   := ssoRegExprMultiLine in Options;
//    FSESearch.Backwards          :=
//      (ssoBackwards in Options) and not FSESearch.RegExprMultiLine;
    DoChange;
  end;
end;

function TSearchEngine.GetSearchAllViews: Boolean;
begin
  Result := Settings.SearchAllViews;
end;

procedure TSearchEngine.SetSearchAllViews(AValue: Boolean);
begin
  Settings.SearchAllViews := AValue;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
procedure TSearchEngine.DoExecute;
begin
  FOnExecute.Invoke(Self);
end;

procedure TSearchEngine.DoChange;
begin
  FOnChange.Invoke(Self);
end;
{$ENDREGION}

{$REGION 'protected methods'}
procedure TSearchEngine.AddResultsForView(AView: IEditorView);
var
  SRG  : TSearchResultGroup;
  SRL  : TSearchResultLine;
  SR   : TSearchResult;
  Line : Integer;
  N    : Integer;
  B    : Boolean;
begin
  N := 0;
  AView.Editor.Search.Options := AView.Editor.Search.Options -
    [soShowStringNotFound, soShowSearchMatchNotFound];
  Options := Options - [soShowStringNotFound, soShowSearchMatchNotFound];

  AView.Editor.Search.Options := AView.Editor.Search.Options +
    [soEntireScope, soHighlightResults];

  AView.Editor.Search.SearchText := SearchText;
  B := AView.Editor.Search.Lines.Count > 0;
  if B then
  begin
    SRG := TSearchResultGroup.Create;
    while B and (N < MAX_RESULTS) do
    begin
      SRL := TSearchResultLine.Create;
      Line :=  AView.Editor.SelectionBeginPosition.Line;
      while B and (AView.Editor.SelectionBeginPosition.Line = Line) do
      begin
        Inc(N);
        SR := TSearchResult.Create;
        SR.FileName   := ExtractFileName(AView.FileName);
        SR.ViewName   := AView.Name;
        SR.BlockBegin := TPoint(AView.Editor.SelectionBeginPosition);
        SR.BlockEnd   := TPoint(AView.Editor.SelectionEndPosition);
        SR.Column     := SR.BlockBegin.X;
        SR.Line       := SR.BlockBegin.Y;
        SR.Index      := N;
//
//    soBackwards,
//    soBeepIfStringNotFound,
//    soCaseSensitive,
//    soEntireScope,
//    soHighlightResults,
//    soSearchOnTyping,
//    soSelectedOnly,
//    soShowStringNotFound,
//    soShowSearchMatchNotFound,
//    soWholeWordsOnly,
//    soWrapAround
//
//
        //SR.ShowMatch  := ssoRegExpr in Options;
//        if SR.ShowMatch then
//          SR.Match := AView.Editor.TextBetweenPoints[ptFoundStart, ptFoundEnd];
        SRL.List.Add(SR);
        FItemList.Add(SR);
        B := AView.Editor.FindNext;
      end;
      SRL.Line := Line;
      SRG.Lines.Add(SRL);
    end;
    SRG.FileName := ExtractFileName(AView.FileName);
    SRG.ViewName := AView.Name;
    ItemGroups.Add(SRG);
  end;
end;

function TSearchEngine.PosToLineCol(const AString: string;
  const AOffset: TPoint; APos: Integer): TPoint;
var
  I: Integer;
begin
  Result := AOffset;
  I := 1;
  while I < APos do
  begin
    if CharInSet(AString[I], [#10, #13]) then
    begin
      Inc(Result.Y);
      Result.X := 1;
      Inc(I);
      if (I < APos) and  CharInSet(AString[I], [#10, #13])
        and (AString[I] <> AString[I - 1]) then
        Inc(I);
      end
      else
      begin
        Inc(Result.X);
        Inc(I);
      end;
    end;
end;

{ Get matching position of FSearchText in the AText, starting from postion APos.
    When no match is found -1 is returned.

    TODO:
      ssoSelectedOnly
      FBlockSelection.ActiveSelectionMode = smLine/smColumn
}

procedure TSearchEngine.Execute;
var
  V: IEditorView;
  I: Integer;
begin
  FItemGroups.Clear;
  FItemList.Clear;
  if SearchAllViews then
  begin
    for I := 0 to Views.Count - 1 do
    begin
      V := Views[I];
      AddResultsForView(V);
    end;
  end
  else
    AddResultsForView(View);
  DoExecute;
end;

procedure TSearchEngine.FindNext;
var
  SR: TSearchResult;
begin
  Inc(FCurrentIndex);
  if CurrentIndex < ItemList.Count then
  begin
    SR := ItemList[CurrentIndex] as TSearchResult;
    Manager.ActivateView(SR.ViewName);
//    View.SelStart := SR.StartPos;
//    View.SelEnd   := SR.StartPos + Length(SearchText);
    DoChange;
  end
  else
    Dec(FCurrentIndex);
  Logger.Send('CurrentIndex', CurrentIndex);
end;

procedure TSearchEngine.FindPrevious;
var
  SR: TSearchResult;
begin
  Dec(FCurrentIndex);
  if CurrentIndex >= 0 then
  begin
    SR := ItemList[CurrentIndex] as TSearchResult;
    Manager.ActivateView(SR.ViewName);
//    Manager.ActiveView.SelStart := SR.StartPos;
//    Manager.ActiveView.SelEnd   := SR.StartPos + Length(SearchText);
    DoChange;
  end
  else
    FCurrentIndex := 0;
  Logger.Send('CurrentIndex', CurrentIndex);
end;

procedure TSearchEngine.Replace;
//var
//  SR : TSearchResult;
begin
  if CurrentIndex >= 0 then
  begin
    //SR := ItemList[CurrentIndex] as TSearchResult;
    //Options := Options + [ssoReplace];
//    Manager.ActiveView.Editor.SearchReplaceEx(
//      SearchText,
//      ReplaceText,
//      Options,
//      SR.BlockBegin
//    );
//    Options := Options - [ssoReplace];
  end;
end;

procedure TSearchEngine.ReplaceAll;
var
  SR : TSearchResult;
  V  : IEditorView;
  I  : Integer;
begin
  if CurrentIndex >= 0 then
  begin
    SR := ItemList[CurrentIndex] as TSearchResult;
    Logger.Send('SR.Index', SR.Index);
    Logger.Watch('SearchText', SearchText);
    Logger.Watch('ReplaceText', ReplaceText);
    //Options := Options + [ssoReplaceAll];
    if SearchAllViews then
    begin
      for I := 0 to Manager.Views.Count - 1 do
      begin
        V := Manager.Views[I];
        V.BeginUpdate; // handle all replacements as one operation that we can undo
        //Options := Options + [ssoEntireScope];
        //V.Editor.SearchReplace(SearchText, ReplaceText, Options);
        V.EndUpdate;
      end;
    end
    else
    begin
      V := Manager.ActiveView;
      V.BeginUpdate; // handle all replacements as one operation that we can undo
      //V.Editor.SearchReplaceEx(SearchText, ReplaceText, Options, SR.BlockBegin);
      V.EndUpdate;
    end;
    //Options := Options - [ssoReplaceAll];
  end;
end;
{$ENDREGION}

end.

