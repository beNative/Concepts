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

unit DDuce.Editor.Highlighters;

{ Holds settings that are specific to each supported highlighter. }

interface

{ REMARKS about TTextEditorHighlighter:
  - a TTextEditorHighlighter requires a TTextEditor component to be able to load
    its properties from the definition file.
  - It uses a TTextEditorHighlighterImportJSON object to load its settings. It
    is not possible to save its settings back to JSON.
  - It is not intended as a persistable settings object, but is rather a helper
    object that works closely with the associated editor instance to render its
    text.
}

uses
  System.SysUtils, System.Classes,

  DDuce.Editor.CodeFormatters, DDuce.Editor.CodeTags,
  DDuce.Logger;

type
  THighlighters = class;

  THighlighterItem = class(TComponent)
  private
    FCodeFormatter        : ICodeFormatter;
    FDescription          : string;
    FFormatterSupport     : Boolean;
    FLayoutFileName       : string;
    FDefaultFilter        : string;
    FHighlighter          : string;
    FSmartSelectionTags   : TCodeTags;
    FFileExtensions       : TStringList;
    FUseCommonAttributes  : Boolean;

    {$REGION 'property access methods'}
    function GetDefaultFilter: string;
    function GetFileExtensions: string;
    function GetIndex: Integer;
    procedure SetDefaultFilter(AValue: string);
    procedure SetFileExtensions(AValue: string);
    procedure SetFormatterSupport(const AValue: Boolean);
    procedure SetSmartSelectionTags(AValue: TCodeTags);
    {$ENDREGION}

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    property CodeFormatter: ICodeFormatter
      read FCodeFormatter write FCodeFormatter;

    property Index: Integer
      read GetIndex;

    { Set of tags that are used for the SmartSelect feature of the editor. }
    property SmartSelectionTags: TCodeTags
      read FSmartSelectionTags write SetSmartSelectionTags;

  published
    property Highlighter: string
      read FHighlighter write FHighlighter;

    property FormatterSupport: Boolean
      read FFormatterSupport write SetFormatterSupport;

    { Comma seperated list of file extensions associated with the highlighter. }
    property FileExtensions: string
      read GetFileExtensions write SetFileExtensions;

    property DefaultFilter: string
      read GetDefaultFilter write SetDefaultFilter;

    property Description: string
      read FDescription write FDescription;

    property LayoutFileName: string
      read FLayoutFileName write FLayoutFileName;

  end;

  THighlighterItemClass = class of THighlighterItem;

  THighlighters = class(TComponent)
  type
    THighlighterEnumerator = class
    private
      FHighlighters : THighlighters;
      FPosition     : Integer;

      function GetCurrent: THighlighterItem;

    public
      constructor Create(AHighlighters: THighlighters);

      function MoveNext: Boolean;

      property Current: THighlighterItem
        read GetCurrent;
    end;

    {$REGION 'property access methods'}
    function GetCount: Integer;
    function GetFileFilter: string;
    function GetItem(Index: Integer): THighlighterItem;
    function GetItemByName(const AName: string): THighlighterItem;
    procedure SetItem(Index: Integer; const Value: THighlighterItem);
    procedure SetItemByName(const AName: string; const AValue: THighlighterItem);
    {$ENDREGION}

  public
    function Add: THighlighterItem;
    function Exists(const AName: string): Boolean;

    function GetEnumerator: THighlighterEnumerator;

    function IndexOf(const AName: string): Integer; virtual;
    function Find(const AName: string): THighlighterItem;
    function FindHighlighterForFileType(const AFileExt: string): THighlighterItem;

    procedure RegisterHighlighter(
      const ALayoutFileName : string = '';
      const AName           : string = '';       // unique name
      const ADescription    : string = '';
      const AFileExtensions : string = '';  // comma separated list
      const ACodeFormatter  : ICodeFormatter = nil
    ); virtual;

    { Provides indexed access to the list of items. }
    property Items[Index: Integer]: THighlighterItem
      read GetItem write SetItem; default;

    property Count: Integer
      read GetCount;

    property ItemsByName[const AName: string]: THighlighterItem
      read GetItemByName write SetItemByName;

    property FileFilter: string
      read GetFileFilter;

  end;

implementation

uses
  System.IOUtils,
  Vcl.Forms, Vcl.Dialogs;

{$REGION 'THighlighterEnumerator'}
function THighlighters.THighlighterEnumerator.GetCurrent: THighlighterItem;
begin
  Result := FHighlighters[FPosition];
end;

constructor THighlighters.THighlighterEnumerator.Create(AHighlighters: THighlighters);
begin
  FHighlighters := AHighlighters;
  FPosition := -1;
end;

function THighlighters.THighlighterEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FHighlighters.Count;
end;
{$ENDREGION}

{$REGION 'THighlighters'}
{$REGION 'property access mehods'}
function THighlighters.GetItem(Index: Integer): THighlighterItem;
begin
  Result := Components[Index] as THighlighterItem;
end;

procedure THighlighters.SetItem(Index: Integer; const Value: THighlighterItem);
begin
  Components[Index].Assign(Value);
end;

function THighlighters.GetFileFilter: string;
var
  S  : string;
  HI : THighlighterItem;
begin
  S := '';
  for HI in Self do
  begin
    if HI.DefaultFilter <> '' then
      S := S + HI.DefaultFilter + '|';
  end;
  Result := S;
end;

function THighlighters.GetCount: Integer;
begin
  Result := ComponentCount;
end;

function THighlighters.GetItemByName(const AName: string): THighlighterItem;
begin
  Result := Find(AName);
  if not Assigned(Result) then
    Result := Find('None');
end;

procedure THighlighters.SetItemByName(const AName: string; const AValue: THighlighterItem);
var
  Item: THighlighterItem;
begin
  Item := Find(AName);
  if Assigned(Item) then
    Item.Assign(AValue);
end;
{$ENDREGION}

{$REGION 'public methods'}
{ Adds a new THighlighterItem instance to the list. }

function THighlighters.Add: THighlighterItem;
begin
  Result := THighlighterItem.Create(Self);
end;

function THighlighters.Exists(const AName: string): Boolean;
begin
  Result := Assigned(Find(AName));
end;

function THighlighters.GetEnumerator: THighlighterEnumerator;
begin
  Result := THighlighterEnumerator.Create(Self);
end;

function THighlighters.IndexOf(const AName: string): Integer;
var
  I: Integer;
  B: Boolean;
begin
  I := 0;
  B := False;
  while not B and (I < ComponentCount) do
  begin
    B := SameText(Components[I].Name, AName);
    if not B then
      Inc(I);
  end;
  if B then
    Result := I
  else
    Result := -1;
end;

procedure THighlighters.RegisterHighlighter(const ALayoutFileName, AName,
  ADescription, AFileExtensions: string; const ACodeFormatter: ICodeFormatter);
var
  HI : THighlighterItem;
begin
  HI := Find(AName);
  if not Assigned(HI) then
  begin
    HI := Add;
    HI.Name        := AName;
    HI.Highlighter := AName;
  end;
  if ADescription <> '' then
    HI.Description := ADescription
  else
  begin
    HI.Description := TPath.GetFileNameWithoutExtension(ALayoutFileName);
  end;
  HI.CodeFormatter       := ACodeFormatter;
  if HI.LayoutFileName = '' then
    HI.LayoutFileName := ALayoutFileName;
  if HI.FileExtensions = '' then
    HI.FileExtensions := AFileExtensions;
  Logger.SendObject('Highlighter', HI);
end;

function THighlighters.Find(const AName: string): THighlighterItem;
var
  I: Integer;
begin
  I := IndexOf(AName);
  if I < 0 then
    Result := nil
  else
    Result := Items[I];
end;

{ Finds the corresponding highlighteritem for a given file extension. }

function THighlighters.FindHighlighterForFileType(const AFileExt: string): THighlighterItem;
var
  I  : Integer;
  HL : THighlighterItem;
  S  : string;
begin
  Result := nil;
  S := LowerCase(AFileExt);
  for I := 0 to Count - 1 do
  begin
    HL :=  Items[I];
    if Assigned(HL) then
    begin
      if HL.FileExtensions.Contains(S) or HL.DefaultFilter.Contains(S) then
        Result := HL;
    end;
  end;
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'THighlighterItem'}
{$REGION 'construction and destruction'}
procedure THighlighterItem.AfterConstruction;
begin
  inherited AfterConstruction;
  FFileExtensions            := TStringList.Create;
  FFileExtensions.Duplicates := dupIgnore;
  FFileExtensions.Sorted     := True;
  FSmartSelectionTags        := TCodeTags.Create(nil);
  FUseCommonAttributes       := True;
end;

destructor THighlighterItem.Destroy;
begin
  FCodeFormatter := nil;
  FreeAndNil(FFileExtensions);
  FreeAndNil(FSmartSelectionTags);
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access mehods'}
function THighlighterItem.GetDefaultFilter: string;
begin
  Result := FDefaultFilter;
end;

procedure THighlighterItem.SetDefaultFilter(AValue: string);
begin
  if AValue <> DefaultFilter then
  begin
    FDefaultFilter := AValue;
  end;
end;

function THighlighterItem.GetIndex: Integer;
begin
  Result := ComponentIndex;
end;

function THighlighterItem.GetFileExtensions: string;
begin
  Result := FFileExtensions.CommaText;
end;

procedure THighlighterItem.SetFileExtensions(AValue: string);
begin
  if AValue <> FileExtensions then
  begin
    FFileExtensions.CommaText := AValue;
  end;
end;

procedure THighlighterItem.SetFormatterSupport(const AValue: Boolean);
begin
  if AValue <> FormatterSupport then
  begin
    FFormatterSupport := AValue;
  end;
end;

procedure THighlighterItem.SetSmartSelectionTags(AValue: TCodeTags);
begin
  FSmartSelectionTags.Assign(AValue);
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure THighlighterItem.Assign(Source: TPersistent);
var
  HLI: THighlighterItem;
begin
  if (Source <> Self) and (Source is THighlighterItem) then
  begin
    HLI := THighlighterItem(Source);
    SmartSelectionTags.Assign(HLI.SmartSelectionTags);
    Highlighter          := HLI.Highlighter;
    Description          := HLI.Description;
    LayoutFileName       := HLI.LayoutFileName;
    FileExtensions       := HLI.FileExtensions;
    SmartSelectionTags   := HLI.SmartSelectionTags;
    DefaultFilter        := HLI.DefaultFilter;
    FormatterSupport     := HLI.FormatterSupport;
  end
  else
    inherited Assign(Source);
end;
{$ENDREGION}
{$ENDREGION}

initialization
  RegisterClass(THighlighters);
  RegisterClass(THighlighterItem);

end.

