(*
  Copyright (c) 2011-2013, Stefan Glienke
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

unit DSharp.Core.DataTemplates;

interface

uses
  Classes,
  Spring.Collections,
  Rtti;

type
  IDataTemplate = interface
    ['{C03657F3-54EF-4105-9943-B07733770700}']
    // methods to display items
    function GetHint(const Item: TObject; const ColumnIndex: Integer): string;
    function GetImageIndex(const Item: TObject; const ColumnIndex: Integer): Integer;
    function GetText(const Item: TObject; const ColumnIndex: Integer): string;
    function GetValue(const Item: TObject; const ColumnIndex: Integer): TValue;
    function IsCheckBoxVisible(const Item: TObject): Boolean;

    // methods to edit items
    procedure SetText(const Item: TObject; const ColumnIndex: Integer;
      const Value: string);
    procedure SetValue(const Item: TObject; const ColumnIndex: Integer;
      const Value: TValue);

    // methods to build the tree structure
    function GetItem(const Item: TObject; const Index: Integer): TObject;
    function GetItemCount(const Item: TObject): Integer;
    function GetItems(const Item: TObject): IObjectList;
    function GetItemTemplate(const Item: TObject): IDataTemplate;

    function CompareItems(const Item1, Item2: TObject; const ColumnIndex: Integer): Integer;

    // methods to manage the template "binding"
    function GetTemplateDataClass: TClass;
    procedure RegisterDataTemplate(const DataTemplate: IDataTemplate);

    // methods to manage the default action
    function GetAction: TBasicAction;
    procedure SetAction(const Value: TBasicAction);
    property Action: TBasicAction read GetAction write SetAction;
  end;

  TDataTemplate = class(TInterfacedObject, IDataTemplate)
  private
    FAction: TBasicAction;
    FTemplates: IList<IDataTemplate>;
  protected
    function GetAction: TBasicAction;
    procedure SetAction(const Value: TBasicAction);
    property Templates: IList<IDataTemplate> read FTemplates;
  public
    constructor Create; overload;
    constructor Create(const Action: TBasicAction); overload;
    destructor Destroy; override;

    function GetHint(const Item: TObject;
      const ColumnIndex: Integer): string; virtual;
    function GetImageIndex(const Item: TObject;
      const ColumnIndex: Integer): Integer; virtual;
    function GetText(const Item: TObject;
      const ColumnIndex: Integer): string; virtual;
    function GetValue(const Item: TObject;
      const ColumnIndex: Integer): TValue; virtual;

    function IsCheckBoxVisible(const Item: TObject): Boolean; virtual;

    procedure SetText(const Item: TObject; const ColumnIndex: Integer;
      const Value: string); virtual;
    procedure SetValue(const Item: TObject; const ColumnIndex: Integer;
      const Value: TValue); virtual;

    function GetItem(const Item: TObject;
      const Index: Integer): TObject; virtual;
    function GetItemCount(const Item: TObject): Integer; virtual;
    function GetItems(const Item: TObject): IObjectList; virtual;
    function GetItemTemplate(const Item: TObject): IDataTemplate; virtual;

    function CompareItems(const Item1, Item2: TObject;
      const ColumnIndex: Integer): Integer; virtual;

    function GetTemplateDataClass: TClass; virtual;
    procedure RegisterDataTemplate(const DataTemplate: IDataTemplate);

    property Action: TBasicAction read GetAction write SetAction;
  end;

  TDataTemplate<T: class> = class(TDataTemplate)
  public
    function GetHint(const Item: TObject;
      const ColumnIndex: Integer): string; overload; override; final;
    function GetHint(const Item: T;
      const ColumnIndex: Integer): string; reintroduce; overload; virtual;

    function GetImageIndex(const Item: TObject;
      const ColumnIndex: Integer): Integer; overload; override; final;
    function GetImageIndex(const Item: T;
      const ColumnIndex: Integer): Integer; reintroduce; overload; virtual;

    procedure SetText(const Item: TObject; const ColumnIndex: Integer;
      const Value: string); overload; override; final;
    procedure SetText(const Item: T; const ColumnIndex: Integer;
      const Value: string); reintroduce; overload; virtual;
    procedure SetValue(const Item: TObject; const ColumnIndex: Integer;
      const Value: TValue); overload; override; final;
    procedure SetValue(const Item: T; const ColumnIndex: Integer;
      const Value: TValue); reintroduce; overload; virtual;

    function GetText(const Item: TObject;
      const ColumnIndex: Integer): string; overload; override; final;
    function GetText(const Item: T;
      const ColumnIndex: Integer): string; reintroduce; overload; virtual;
    function GetValue(const Item: TObject;
      const ColumnIndex: Integer): TValue; overload; override; final;
    function GetValue(const Item: T;
      const ColumnIndex: Integer): TValue; reintroduce; overload; virtual;
    function GetItem(const Item: TObject;
      const Index: Integer): TObject; overload; override; final;
    function GetItem(const Item: T;
      const Index: Integer): TObject; reintroduce; overload; virtual;
    function GetItemCount(const Item: TObject): Integer; overload; override; final;
    function GetItemCount(const Item: T): Integer; reintroduce; overload; virtual;
    function GetItems(const Item: TObject): IObjectList; overload; override; final;
    function GetItems(const Item: T): IObjectList; reintroduce; overload; virtual;

    function IsCheckBoxVisible(const Item: TObject): Boolean; overload; override; final;
    function IsCheckBoxVisible(const Item: T): Boolean; reintroduce; overload; virtual;

    function GetTemplateDataClass: TClass; override; final;
  end;

implementation

uses
  DSharp.Core.Reflection,
  Spring.Collections.Lists,
  SysUtils,
  TypInfo;

{ TDataTemplate }

function TDataTemplate.CompareItems(const Item1, Item2: TObject;
  const ColumnIndex: Integer): Integer;
var
  LItemTemplate1, LItemTemplate2: IDataTemplate;
begin
  Result := 0;

  if Assigned(Item1) and Assigned(Item2) then
  begin
    if Item1.InheritsFrom(GetTemplateDataClass) then
    begin
      if Item2.InheritsFrom(GetTemplateDataClass) then
      begin
        Result := CompareValue(GetValue(Item1, ColumnIndex), GetValue(Item2, ColumnIndex));
      end else
      begin
        Result := -1;
      end;
    end else
    begin
      if Item2.InheritsFrom(GetTemplateDataClass) then
      begin
        Result := 1;
      end else
      begin
        LItemTemplate1 := GetItemTemplate(Item1);
        LItemTemplate2 := GetItemTemplate(Item2);

        if Assigned(LItemTemplate1) and Assigned(LItemTemplate2) then
        begin
          Result := CompareValue(LItemTemplate1.GetValue(Item1, ColumnIndex),
            LItemTemplate2.GetValue(Item2, ColumnIndex));
        end;
      end;
    end;
  end;
end;

constructor TDataTemplate.Create;
begin
  FTemplates := TCollections.CreateInterfaceList<IDataTemplate>;
end;

constructor TDataTemplate.Create(const Action: TBasicAction);
begin
  Create;
  FAction := Action;
end;

destructor TDataTemplate.Destroy;
begin
  inherited;
end;

function TDataTemplate.GetAction: TBasicAction;
begin
  Result := FAction;
end;

function TDataTemplate.GetHint(const Item: TObject;
  const ColumnIndex: Integer): string;
begin
  Result := '';
end;

function TDataTemplate.GetImageIndex(const Item: TObject;
  const ColumnIndex: Integer): Integer;
begin
  Result := -1;
end;

function TDataTemplate.GetItem(const Item: TObject;
  const Index: Integer): TObject;
var
  list: IObjectList;
begin
  Result := nil;

  if Supports(Item, IObjectList, list) and (list.Count > 0) then
  begin
    Result := list[Index];
  end;
end;

function TDataTemplate.GetItemCount(const Item: TObject): Integer;
var
  list: IObjectList;
begin
  Result := 0;

  if Supports(Item, IObjectList, list) then
  begin
    Result := list.Count;
  end;
end;

function TDataTemplate.GetItems(const Item: TObject): IObjectList;
begin
  Result := nil;
end;

function TDataTemplate.GetItemTemplate(const Item: TObject): IDataTemplate;
var
  LTemplate: IDataTemplate;
begin
  Result := nil;

  if Assigned(FTemplates) then
  begin
    for LTemplate in FTemplates do
    begin
      Result := LTemplate.GetItemTemplate(Item);
      if Assigned(Result) then
      begin
        Break;
      end;
    end;
  end;

  if not Assigned(Result) and Assigned(Item) and (Item.InheritsFrom(GetTemplateDataClass)
    or IsClassCovariantTo(Item.ClassType, GetTemplateDataClass)) then
  begin
    Result := Self;
  end;
end;

function TDataTemplate.GetTemplateDataClass: TClass;
begin
  Result := TList<TObject>;
end;

function TDataTemplate.GetText(const Item: TObject;
  const ColumnIndex: Integer): string;
var
  LValue: TValue;
begin
  LValue := GetValue(Item, ColumnIndex);
  if LValue.IsEmpty then
  begin
    Result := '';
  end
  else
  begin
    Result := TValue.ToString(LValue);
  end;
end;

function TDataTemplate.GetValue(const Item: TObject;
  const ColumnIndex: Integer): TValue;
begin
  Result := TValue.Empty;
end;

function TDataTemplate.IsCheckBoxVisible(const Item: TObject): Boolean;
begin
  Result := True;
end;

procedure TDataTemplate.RegisterDataTemplate(const DataTemplate: IDataTemplate);
begin
  FTemplates.Add(DataTemplate);
end;

procedure TDataTemplate.SetAction(const Value: TBasicAction);
begin
  if FAction <> Value then
    FAction := Value;
end;

procedure TDataTemplate.SetText(const Item: TObject; const ColumnIndex: Integer;
  const Value: string);
begin
  SetValue(Item, ColumnIndex, Value);
end;

procedure TDataTemplate.SetValue(const Item: TObject;
  const ColumnIndex: Integer; const Value: TValue);
begin
  // implemented in descendants
end;

{ TDataTemplate<T> }

function TDataTemplate<T>.GetImageIndex(const Item: TObject;
  const ColumnIndex: Integer): Integer;
begin
  Result := GetImageIndex(T(Item), ColumnIndex);
end;

function TDataTemplate<T>.GetHint(const Item: T;
  const ColumnIndex: Integer): string;
begin
  Result := inherited GetHint(Item, ColumnIndex);
end;

function TDataTemplate<T>.GetHint(const Item: TObject;
  const ColumnIndex: Integer): string;
begin
  Result := GetHint(T(Item), ColumnIndex);
end;

function TDataTemplate<T>.GetImageIndex(const Item: T;
  const ColumnIndex: Integer): Integer;
begin
  Result := inherited GetImageIndex(Item, ColumnIndex);
end;

function TDataTemplate<T>.GetItem(const Item: TObject;
  const Index: Integer): TObject;
begin
  Result := GetItem(T(Item), Index);
end;

function TDataTemplate<T>.GetItem(const Item: T; const Index: Integer): TObject;
begin
  Result := inherited GetItem(Item, Index);
end;

function TDataTemplate<T>.GetItemCount(const Item: TObject): Integer;
begin
  Result := GetItemCount(T(Item));
end;

function TDataTemplate<T>.GetItemCount(const Item: T): Integer;
begin
  Result := inherited GetItemCount(Item);
end;

function TDataTemplate<T>.GetItems(const Item: TObject): IObjectList;
begin
  Result := GetItems(T(Item));
end;

function TDataTemplate<T>.GetItems(const Item: T): IObjectList;
begin
  Result := inherited GetItems(Item);
end;

function TDataTemplate<T>.GetTemplateDataClass: TClass;
begin
  Result := T;
end;

function TDataTemplate<T>.GetText(const Item: TObject;
  const ColumnIndex: Integer): string;
begin
  Result := GetText(T(Item), ColumnIndex);
end;

function TDataTemplate<T>.GetText(const Item: T;
  const ColumnIndex: Integer): string;
begin
  Result := inherited GetText(Item, ColumnIndex);
end;

function TDataTemplate<T>.GetValue(const Item: TObject;
  const ColumnIndex: Integer): TValue;
begin
  Result := GetValue(T(Item), ColumnIndex);
end;

function TDataTemplate<T>.GetValue(const Item: T;
  const ColumnIndex: Integer): TValue;
begin
  Result := inherited GetValue(Item, ColumnIndex);
end;

function TDataTemplate<T>.IsCheckBoxVisible(const Item: TObject): Boolean;
begin
  Result := IsCheckBoxVisible(T(Item));
end;

function TDataTemplate<T>.IsCheckBoxVisible(const Item: T): Boolean;
begin
  Result := inherited IsCheckBoxVisible(Item);
end;

procedure TDataTemplate<T>.SetText(const Item: TObject;
  const ColumnIndex: Integer; const Value: string);
begin
  SetText(T(Item), ColumnIndex, Value);
end;

procedure TDataTemplate<T>.SetText(const Item: T; const ColumnIndex: Integer;
  const Value: string);
begin
  inherited SetText(Item, ColumnIndex, Value);
end;

procedure TDataTemplate<T>.SetValue(const Item: TObject;
  const ColumnIndex: Integer; const Value: TValue);
begin
  SetValue(T(Item), ColumnIndex, Value);
end;

procedure TDataTemplate<T>.SetValue(const Item: T; const ColumnIndex: Integer;
  const Value: TValue);
begin
  inherited SetValue(Item, ColumnIndex, Value);
end;

end.

