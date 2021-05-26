(*
  Copyright (c) 2011-2012, Stefan Glienke
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  - Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.
  - Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.
  - Neither the name of this library nor the names of its contributors may be
    used to endorse or promote products derived from this software without
    specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
*)

unit DSharp.Windows.ColumnDefinitions;

interface

uses
  Classes,
  DSharp.Core.Collections,
  DSharp.Core.Expressions,
  DSharp.Windows.ControlTemplates,
  Graphics,
  ImgList,
  Spring,

  SysUtils,
  Types;

const
  CDefaultMinWidth = 30;
  CDefaultWidth = 100;

type
  TCanvas = Graphics.TCanvas;
  TCustomImageList = ImgList.TCustomImageList;
  TRect = Types.TRect;

  TDrawMode = DSharp.Windows.ControlTemplates.TDrawMode;

  TColumnDefinition = class;
  TColumnDefinitions = class;

  TCustomDrawEvent = function(Sender: TObject; ColumnDefinition: TColumnDefinition;
    Item: TObject; TargetCanvas: TCanvas; CellRect: TRect;
    ImageList: TCustomImageList; DrawMode: TDrawMode; Selected: Boolean): Boolean of object;
  TGetHintEvent = function(Sender: TObject; ColumnDefinition: TColumnDefinition;
    Item: TObject): string of object;
  TGetImageIndexEvent = function(Sender: TObject; ColumnDefinition: TColumnDefinition;
    Item: TObject): Integer of object;
  TGetTextEvent = function(Sender: TObject; ColumnDefinition: TColumnDefinition;
    Item: TObject): string of object;
  TSetTextEvent = procedure(Sender: TObject; ColumnDefinition: TColumnDefinition;
    Item: TObject; const Value: string) of object;

  TColumnOption = (coResizable, coSortable, coDraggable);
  TColumnOptions = set of TColumnOption;
  TColumnType = (ctText, ctCheckBox, ctProgressBar, ctImage);
  TSortingDirection = (sdNone, sdAscending, sdDescending);
  TToggleMode = (tmNone, tmClick, tmDoubleClick);

  TColumnDefinition = class(TCollectionItem)
  private
    FAllowEdit: Boolean;
    FAlignment: TAlignment;
    FAutoSize: Boolean;
    FCaption: string;
    FColumnOptions: TColumnOptions;
    FColumnType: TColumnType;
    FCustomFilter: string;
    FFilter: TPredicate<TObject>;
    FHintPropertyName: string;
    FHintPropertyExpression: IMemberExpression;
    FImageIndexOffset: Integer;
    FImageIndexPropertyExpression: IMemberExpression;
    FImageIndexPropertyName: string;
    FMinWidth: Integer;
    FOnCustomDraw: TCustomDrawEvent;
    FOnGetHint: TGetHintEvent;
    FOnGetImageIndex: TGetImageIndexEvent;
    FOnGetText: TGetTextEvent;
    FOnSetText: TSetTextEvent;
    FSortingDirection: TSortingDirection;
    FToggleMode: TToggleMode;
    FValuePropertyExpression: IMemberExpression;
    FValuePropertyName: string;
    FVisible: Boolean;
    FWidth: Integer;
    function GetCollection: TColumnDefinitions;
    procedure ReadTextPropertyName(Reader: TReader);
    procedure SetAutoSize(const Value: Boolean);
    procedure SetCollection(const Value: TColumnDefinitions); reintroduce;
    procedure SetCustomFilter(const Value: string);
    procedure SetHintPropertyName(const Value: string);
    procedure SetImageIndexPropertyName(const Value: string);
    procedure SetSortingDirection(const Value: TSortingDirection);
    procedure SetValuePropertyName(const Value: string);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function GetDisplayName: string; override;
    property Collection: TColumnDefinitions read GetCollection write SetCollection;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    property HintPropertyExpression: IMemberExpression read FHintPropertyExpression;
    property ImageIndexPropertyExpression: IMemberExpression read FImageIndexPropertyExpression;
    property ValuePropertyExpression: IMemberExpression read FValuePropertyExpression;
  published
    property AllowEdit: Boolean read FAllowEdit write FAllowEdit default False;
    property Alignment: TAlignment read FAlignment write FAlignment default taLeftJustify;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    property Caption: string read FCaption write FCaption;
    property ColumnOptions: TColumnOptions read FColumnOptions write FColumnOptions default [coResizable, coSortable, coDraggable];
    property ColumnType: TColumnType read FColumnType write FColumnType default ctText;
    property CustomFilter: string read FCustomFilter write SetCustomFilter;
    property Filter: TPredicate<TObject> read FFilter;
    property HintPropertyName: string read FHintPropertyName write SetHintPropertyName;
    property ImageIndexOffset: Integer read FImageIndexOffset write FImageIndexOffset default 0;
    property ImageIndexPropertyName: string read FImageIndexPropertyName write SetImageIndexPropertyName;
    property MinWidth: Integer read FMinWidth write FMinWidth default CDefaultMinWidth;
    property OnCustomDraw: TCustomDrawEvent read FOnCustomDraw write FOnCustomDraw;
    property OnGetHint: TGetHintEvent read FOnGetHint write FOnGetHint;
    property OnGetImageIndex: TGetImageIndexEvent read FOnGetImageIndex write FOnGetImageIndex;
    property OnGetText: TGetTextEvent read FOnGetText write FOnGetText;
    property OnSetText: TSetTextEvent read FOnSetText write FOnSetText;
    property SortingDirection: TSortingDirection read FSortingDirection write SetSortingDirection default sdNone;
    property ToggleMode: TToggleMode read FToggleMode write FToggleMode default tmNone;
    property ValuePropertyName: string read FValuePropertyName write SetValuePropertyName;
    property Visible: Boolean read FVisible write FVisible default True;
    property Width: Integer read FWidth write FWidth default CDefaultWidth;
  end;

  IColumnDefinitions = interface
    ['{FBD9C07D-E910-4291-AF4C-E7786E83372A}']
    function Add(const Caption: string; Width: Integer = CDefaultWidth;
      Alignment: TAlignment = taLeftJustify;
      MinWidth: Integer = CDefaultMinWidth): TColumnDefinition;
    function GetAutoSizeIndex: Integer;
    function GetCount: Integer;
    function GetItem(Index: Integer): TColumnDefinition;
    procedure Clear;                       // TS
    procedure Delete(Index: Integer); //TS
    function GetMainColumnIndex: Integer;
    function GetOnNotify: IEvent<TCollectionNotifyEvent<TColumnDefinition>>;
    function GetOwner: TPersistent;
    function GetSortColumnIndex: Integer;
    procedure SetItem(Index: Integer; Value: TColumnDefinition);
    procedure SetMainColumnIndex(const Value: Integer);
    property AutoSizeIndex: Integer read GetAutoSizeIndex;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TColumnDefinition read GetItem write SetItem; default;
    property MainColumnIndex: Integer read GetMainColumnIndex write SetMainColumnIndex;
    property OnNotify: IEvent<TCollectionNotifyEvent<TColumnDefinition>> read GetOnNotify;
    property Owner: TPersistent read GetOwner;
    property SortColumnIndex: Integer read GetSortColumnIndex;
  end;

  TColumnDefinitions = class(TOwnedCollection<TColumnDefinition>, IColumnDefinitions)
  private
    FRefCount: Integer;

    function GetAutoSizeIndex: Integer;
    function GetMainColumnIndex: Integer;
    function GetSortColumnIndex: Integer;
    procedure SetMainColumnIndex(const Value: Integer);
  protected
    FMainColumnIndex: Integer;
    procedure Initialize; virtual;

    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create(AOwner: TPersistent = nil); override; final;
    function Add(const Caption: string; Width: Integer = CDefaultWidth;
      Alignment: TAlignment = taLeftJustify;
      MinWidth: Integer = CDefaultMinWidth): TColumnDefinition; overload;
  end;

implementation

uses
  DSharp.Bindings.Collections,

  StrUtils;

{ TColumnDefinition }

constructor TColumnDefinition.Create(Collection: TCollection);
begin
  inherited;
  FColumnOptions := [coResizable, coSortable, coDraggable];
  FMinWidth := CDefaultMinWidth;
  FVisible := True;
  FWidth := CDefaultWidth;
end;

procedure TColumnDefinition.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TextPropertyName', ReadTextPropertyName, nil, False);
end;

function TColumnDefinition.GetCollection: TColumnDefinitions;
begin
  Result := TColumnDefinitions(inherited Collection);
end;

function TColumnDefinition.GetDisplayName: string;
begin
  Result := FCaption;
end;

procedure TColumnDefinition.ReadTextPropertyName(Reader: TReader);
begin
  ValuePropertyName := Reader.ReadString;
end;

procedure TColumnDefinition.Assign(Source: TPersistent);
var
  LSource: TColumnDefinition;
begin
  if Source is TColumnDefinition then
  begin
    LSource := TColumnDefinition(Source);
    AutoSize := LSource.AutoSize;
    Alignment := LSource.Alignment;
    Caption := LSource.Caption;
    ColumnOptions := LSource.ColumnOptions;
    ColumnType := LSource.ColumnType;
    CustomFilter := LSource.CustomFilter;
    HintPropertyName := LSource.HintPropertyName;
    ImageIndexOffset := LSource.ImageIndexOffset;
    ImageIndexPropertyName := LSource.ImageIndexPropertyName;
    MinWidth := LSource.MinWidth;
    OnCustomDraw := LSource.OnCustomDraw;
    OnGetText := LSource.OnGetText;
    SortingDirection := LSource.SortingDirection;
    ToggleMode := LSource.ToggleMode;
    ValuePropertyName := LSource.ValuePropertyName;
    Visible := LSource.Visible;
    Width := LSource.Width;
  end
  else
  begin
    inherited;
  end;
end;

procedure TColumnDefinition.SetAutoSize(const Value: Boolean);
var
  i: Integer;
begin
  FAutoSize := Value;

  if FAutoSize then
  begin
    for i := 0 to Collection.Count - 1 do
    begin
      if Collection[i].AutoSize and (Collection[i] <> Self) then
      begin
        Collection[i].AutoSize := False;
      end;
    end;
  end;
end;

procedure TColumnDefinition.SetCollection(const Value: TColumnDefinitions);
begin
  inherited Collection := Value;
end;

procedure TColumnDefinition.SetCustomFilter(const Value: string);
var
  LCollectionView: ICollectionView;
begin
  if FCustomFilter <> Value then
  begin
    FCustomFilter := Value;

    if FCustomFilter <> '' then
    begin
      FFilter :=
        function(Item: TObject): Boolean
        begin
          FValuePropertyExpression.Instance := Item;
          Result := ContainsText(FValuePropertyExpression.Value.ToString, FCustomFilter);
        end;
    end
    else
    begin
      FFilter := nil;
    end;

    if Supports(Collection.Owner, ICollectionView, LCollectionView) then
    begin
      // trigger filtering
       //LCollectionView.Filter.OnChanged(Self);
    end;
  end;
end;

procedure TColumnDefinition.SetHintPropertyName(const Value: string);
begin
  FHintPropertyName := Value;
  FHintPropertyExpression := TPropertyExpression.Create(
    TParameterExpression.Create('Instance') as IExpression, FHintPropertyName);
end;

procedure TColumnDefinition.SetImageIndexPropertyName(const Value: string);
begin
  FImageIndexPropertyName := Value;
  FImageIndexPropertyExpression := TPropertyExpression.Create(
    TParameterExpression.Create('Instance') as IExpression, FImageIndexPropertyName);
end;

procedure TColumnDefinition.SetSortingDirection(const Value: TSortingDirection);
var
  i: Integer;
begin
  FSortingDirection := Value;

  if FSortingDirection <> sdNone then
  begin
    for i := 0 to Collection.Count - 1 do
    begin
      if (Collection[i].SortingDirection <> sdNone) and (Collection[i] <> Self) then
      begin
        Collection[i].SortingDirection := sdNone;
      end;
    end;
  end;
end;

procedure TColumnDefinition.SetValuePropertyName(const Value: string);
begin
  FValuePropertyName := Value;
  FValuePropertyExpression := TPropertyExpression.Create(
    TParameterExpression.Create('Instance') as IExpression, FValuePropertyName);
end;

{ TColumnDefinitions }

constructor TColumnDefinitions.Create(AOwner: TPersistent);
begin
  inherited;
  PropName := 'ColumnDefinitions';
  Initialize();
end;

function TColumnDefinitions.GetAutoSizeIndex: Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].AutoSize then
      Exit;

  Result := -1;
end;

function TColumnDefinitions.GetMainColumnIndex: Integer;
begin
  Result := FMainColumnIndex;
end;

function TColumnDefinitions.GetSortColumnIndex: Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].SortingDirection <> sdNone then
      Exit;

  Result := -1;
end;

procedure TColumnDefinitions.Initialize;
begin
  // implemented by descendants
end;

procedure TColumnDefinitions.SetMainColumnIndex(const Value: Integer);
begin
  FMainColumnIndex := Value;
end;

function TColumnDefinitions.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TColumnDefinitions._AddRef: Integer;
begin
  Inc(FRefCount);
  Result := FRefCount;
end;

function TColumnDefinitions._Release: Integer;
begin
  Dec(FRefCount);
  Result := FRefCount;
  if Result = 0 then
    Destroy;
end;

function TColumnDefinitions.Add(const Caption: string;
  Width: Integer; Alignment: TAlignment; MinWidth: Integer): TColumnDefinition;
begin
  Result := TColumnDefinition.Create(nil);
  Result.Alignment := Alignment;
  Result.Caption := Caption;
  Result.MinWidth := MinWidth;
  Result.Width := Width;
  Result.Collection := Self;
end;

end.
