{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2018 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

{$I Spring.inc}

unit Spring.Data.ObjectDataSet;

interface

uses
  Classes,
  DB,
  Rtti,
  Spring,
  Spring.Collections,
  Spring.Data.ExpressionParser,
  Spring.Data.VirtualDataSet;

type
  TObjectDataSet = class(TCustomVirtualDataSet)
  private type
    TIndexFieldInfo = record
      Field: TField;
      Prop: TRttiProperty;
      Descending: Boolean;
      CaseInsensitive: Boolean;
    end;
  private
    fDataList: IObjectList;
    fDefaultStringFieldLength: Integer;
    fFilterIndex: Integer;
    fFilterParser: TExprParser;
    fItemTypeInfo: PTypeInfo;
    fIndexFields: TArray<TIndexFieldInfo>;
    fProperties: IList<TRttiProperty>;
    fDisabledFields: ISet<TField>;
    fSort: string;
    fSorted: Boolean;
    fColumnAttributeClass: TAttributeClass;
    fTrackChanges: Boolean;

    fBeforeFilter: TDataSetNotifyEvent;
    fAfterFilter: TDataSetNotifyEvent;
    fBeforeSort: TDataSetNotifyEvent;
    fAfterSort: TDataSetNotifyEvent;

    function GetSort: string;
    procedure SetSort(const value: string);
    function GetFilterCount: Integer;
    procedure RegisterChangeHandler;
    procedure UnregisterChangeHandler;
    procedure SetDataList(const value: IObjectList);
    procedure SetTrackChanges(const value: Boolean);
  protected
    procedure DoAfterOpen; override;
    procedure DoDeleteRecord(Index: Integer); override;
    procedure DoFilterRecord(var Accept: Boolean); override;
    procedure DoGetFieldValue(Field: TField; Index: Integer; var Value: Variant); override;
    procedure DoPostRecord(Index: Integer; Append: Boolean); override;
    procedure RebuildPropertiesCache; override;

    function DataListCount: Integer;
    function GetRecordCount: Integer; override;
    procedure InitFilterParser;
    procedure UpdateFilter; override;

    procedure DoAfterFilter; virtual;
    procedure DoAfterSort; virtual;
    procedure DoBeforeFilter; virtual;
    procedure DoBeforeSort; virtual;

    procedure DoOnDataListChange(Sender: TObject; const Item: TObject; Action: TCollectionChangedAction);

    function CompareRecords(const left, right: TObject): Integer; virtual;
    function InternalGetFieldValue(field: TField; const obj: TObject): Variant; virtual;
    function ParserGetVariableValue(Sender: TObject; const VarName: string; var Value: Variant): Boolean; virtual;
    function ParserGetFunctionValue(Sender: TObject; const FuncName: string;
      const Args: Variant; var ResVal: Variant): Boolean; virtual;
    procedure InitRttiPropertiesFromItemType(AItemTypeInfo: PTypeInfo); virtual;
    procedure InternalSetSort(const value: string; index: Integer = 0); virtual;
    procedure LoadFieldDefsFromFields(Fields: TFields; FieldDefs: TFieldDefs); virtual;
    procedure LoadFieldDefsFromItemType; virtual;

    function IsCursorOpen: Boolean; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalClose; override;
    procedure InternalCreateFields; override;
    procedure InternalOpen; override;
    procedure InternalRefresh; override;
    procedure SetFilterText(const Value: string); override;

    function GetChangedSortText(const sortText: string): string;
    function CreateIndexList(const sortText: string): TArray<TIndexFieldInfo>;
    function FieldInSortIndex(AField: TField): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>
    ///   Makes the current dataset clone of <c>ASource</c>.
    /// </summary>
    procedure Clone(const ASource: TObjectDataSet);

    /// <summary>
    ///   Returns underlying model object from the current row.
    /// </summary>
    function GetCurrentModel<T: class>: T;

    /// <summary>
    ///   Returns newly created list of data containing only filtered items.
    /// </summary>
    function GetFilteredDataList<T: class>: IList<T>;

    /// <summary>
    ///   Returns the total count of filtered records.
    /// </summary>
    property FilterCount: Integer read GetFilterCount;

    /// <summary>
    ///   Checks if dataset is sorted.
    /// </summary>
    property Sorted: Boolean read fSorted;

    /// <summary>
    ///   Sorting conditions separated by commas. Can set different sort order
    ///   for multiple fields - <c>Asc</c> stands for ascending, <c>Desc</c> -
    ///   descending.
    /// </summary>
    /// <example>
    ///   <code lang="">MyDataset.Sort := 'Name, Id Desc, Description Asc';</code>
    /// </example>
    property Sort: string read GetSort write SetSort;

    /// <summary>
    ///   Class of the column attribute. If properties of the entity class are
    ///   annotated with this attribute, then the dataset will use these
    ///   properties as fields. If ColumnAttributeClass is null, all the
    ///   published properties of the entity class will be used as dataset
    ///   fields.
    /// </summary>
    /// <example>
    ///   <code lang="">MyDataset.ColumnAttributeClass := ColumnAttribute</code>
    /// </example>
    property ColumnAttributeClass: TAttributeClass
      read fColumnAttributeClass write fColumnAttributeClass;

    /// <summary>
    ///   The list of objects to display in the dataset.
    /// </summary>
    property DataList: IObjectList read fDataList write SetDataList;
  published
    /// <summary>
    ///   Default length for the string type field in the dataset.
    /// </summary>
    /// <remarks>
    ///   Defaults to <c>250</c> if not set.
    /// </remarks>
    property DefaultStringFieldLength: Integer read fDefaultStringFieldLength write fDefaultStringFieldLength default 250;
    property TrackChanges: Boolean read fTrackChanges write SetTrackChanges default False;

    property Filter;
    property Filtered;
    property FilterOptions;

    property AfterCancel;
    property AfterClose;
    property AfterDelete;
    property AfterEdit;
    property AfterInsert;
    property AfterOpen;
    property AfterPost;
    property AfterRefresh;
    property AfterScroll;
    property BeforeCancel;
    property BeforeClose;
    property BeforeDelete;
    property BeforeEdit;
    property BeforeInsert;
    property BeforeOpen;
    property BeforePost;
    property BeforeRefresh;
    property BeforeScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;

    property BeforeFilter: TDataSetNotifyEvent read fBeforeFilter write fBeforeFilter;
    property AfterFilter: TDataSetNotifyEvent read fAfterFilter write fAfterFilter;
    property BeforeSort: TDataSetNotifyEvent read fBeforeSort write fBeforeSort;
    property AfterSort: TDataSetNotifyEvent read fAfterSort write fAfterSort;
  end;

implementation

uses
  FmtBcd,
  StrUtils,
  SysUtils,
  TypInfo,
  Variants,
  Spring.Data.ExpressionParser.Functions,
  Spring.Data.ValueConverters,
  Spring.Reflection;

resourcestring
  SPropertyNotFound = 'Property %s not found';
  SColumnPropertiesNotSpecified = 'Type does not have column properties';

type
  EObjectDataSetException = class(Exception);
  
  {$WARNINGS OFF}
  TWideCharSet = set of Char;
  {$WARNINGS ON}


{$REGION 'TObjectDataSet'}

constructor TObjectDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fProperties := TCollections.CreateList<TRttiProperty>;
  fDisabledFields := TCollections.CreateSet<TField>;
  fFilterParser := TExprParser.Create;
  fFilterParser.OnGetVariable := ParserGetVariableValue;
  fFilterParser.OnExecuteFunction := ParserGetFunctionValue;
  fDefaultStringFieldLength := 250;
end;

destructor TObjectDataSet.Destroy;
begin
  fFilterParser.Free;
  inherited Destroy;
end;

procedure TObjectDataSet.Clone(const ASource: TObjectDataSet);
begin
  if Active then
    Close;

  fColumnAttributeClass := ASource.fColumnAttributeClass;
  fItemTypeInfo := ASource.fItemTypeInfo;
  fDataList := ASource.DataList;
  IndexList.DataList := ASource.IndexList.DataList;

  FilterOptions := ASource.FilterOptions;
  Filter := ASource.Filter;
  Filtered := ASource.Filtered;
  Open;
  if ASource.Sorted then
    Sort := ASource.Sort;
end;

function TObjectDataSet.CompareRecords(const left, right: TObject): Integer;
var
  i: Integer;
  fieldInfo: TIndexFieldInfo;
  leftValue, rightValue: TValue;
begin
  Result := 0;

  for i := 0 to High(fIndexFields) do
  begin
    fieldInfo := fIndexFields[i];
    leftValue := fieldInfo.Prop.GetValue(left);
    rightValue := fieldInfo.Prop.GetValue(right);

    Result := leftValue.CompareTo(rightValue);
    if fieldInfo.Descending then
      Result := -Result;

    if Result <> 0 then
      Exit;
  end;
end;

function TObjectDataSet.CreateIndexList(const sortText: string): TArray<TIndexFieldInfo>;
var
  splittedText: TStringDynArray;
  i, n: Integer;
  fieldName: string;
  info: TIndexFieldInfo;
begin
  Result := nil;
  splittedText := SplitString(sortText, ',');
  for i := 0 to High(splittedText) do
  begin
    fieldName := Trim(splittedText[i]);
    info.Descending := EndsStr(' DESC', AnsiUpperCase(fieldName));
    if info.Descending then
      fieldName := Trim(Copy(fieldName, 1, Length(fieldName) - 5))
    else if EndsStr(' ASC', AnsiUpperCase(fieldName)) then
      fieldName := Trim(Copy(fieldName, 1, Length(fieldName) - 4));

    info.Field := FindField(fieldName);
    if Assigned(info.Field) and fProperties.TryGetFirst(info.Prop,
      TPropertyFilters.IsNamed(fieldName)) then
    begin
      info.CaseInsensitive := True;
      n := Length(Result);
      SetLength(Result, n + 1);
      Result[n] := info;
    end;
  end;
end;

function TObjectDataSet.DataListCount: Integer;
begin
  Result := 0;
  if Assigned(fDataList) then
    Result := fDataList.Count;
end;

procedure TObjectDataSet.DoAfterOpen;
begin
  if Filtered then
  begin
    UpdateFilter;
    First;
  end;
  inherited DoAfterOpen;
end;

procedure TObjectDataSet.DoDeleteRecord(Index: Integer);
begin
  IndexList.DeleteItem(Index);
end;

procedure TObjectDataSet.DoFilterRecord(var Accept: Boolean);
begin
  if (fFilterIndex >= 0) and (fFilterIndex < DataListCount) then
  begin
    if Assigned(OnFilterRecord) then
      OnFilterRecord(Self, Accept)
    else
    begin
      FilterCache.Clear;
      if fFilterParser.Eval then
        Accept := fFilterParser.Value;
    end;
  end
  else
    Accept := False;
end;

procedure TObjectDataSet.DoGetFieldValue(Field: TField; Index: Integer; var Value: Variant);
var
  obj: TObject;
begin
  obj := IndexList.Items[Index];
  Value := InternalGetFieldValue(Field, obj);
end;

procedure TObjectDataSet.DoAfterFilter;
begin
  if Assigned(fAfterFilter) then
    fAfterFilter(Self);
end;

procedure TObjectDataSet.DoAfterSort;
begin
  if Assigned(fAfterSort) then
    fAfterSort(Self);
end;

procedure TObjectDataSet.DoBeforeFilter;
begin
  if Assigned(fBeforeFilter) then
    fBeforeFilter(Self);
end;

procedure TObjectDataSet.DoBeforeSort;
begin
  if Assigned(fBeforeSort) then
    fBeforeSort(Self);
end;

procedure TObjectDataSet.DoOnDataListChange(Sender: TObject;
  const Item: TObject; Action: TCollectionChangedAction);
begin
  if not Active then
    Exit;

  if IndexList.IsChanging then
    Exit;

  IndexList.Rebuild;

  DisableControls;
  try
    Refresh;
  finally
    EnableControls;
  end;
end;

procedure TObjectDataSet.DoPostRecord(Index: Integer; Append: Boolean);
var
  newItem: TObject;
  sortNeeded: Boolean;
  i: Integer;
  field: TField;
  fieldValue: Variant;
  value: TValue;
  prop: TRttiProperty;
begin
  if State = dsInsert then
    newItem := TActivator.CreateInstance(fItemTypeInfo)
  else
    newItem := IndexList.Items[Index];

  sortNeeded := False;

  for i := 0 to ModifiedFields.Count - 1 do
  begin
    field := ModifiedFields[i];

    if not sortNeeded and Sorted then
      sortNeeded := FieldInSortIndex(field);

    // Fields not found in dictionary are calculated or lookup fields, do not post them
    if fProperties.TryGetFirst(prop, TPropertyFilters.IsNamed(field.FieldName)) then
    begin
      fieldValue := field.Value;
      if VarIsNull(fieldValue) then
        prop.SetValue(newItem, TValue.Empty)
      else
        if TValue.From<Variant>(fieldValue).TryConvert(prop.PropertyType.Handle, value) then
          prop.SetValue(newItem, value);
    end;
  end;

  if State = dsInsert then
    if Append then
      Index := IndexList.AddItem(newItem)
    else
      IndexList.InsertItem(newItem, Index);

  if Sorted and sortNeeded then
    InternalSetSort(Sort, Index);

  SetCurrent(Index);
end;

function TObjectDataSet.FieldInSortIndex(AField: TField): Boolean;
var
  i: Integer;
begin
  if Sorted and Assigned(fIndexFields) then
    for i := 0 to High(fIndexFields) do
      if AField = fIndexFields[i].Field then
        Exit(True);
  Result := False;
end;

function TObjectDataSet.GetChangedSortText(const sortText: string): string;
begin
  Result := sortText;

  if EndsStr(' ', Result) then
    Result := Copy(Result, 1, Length(Result) - 1)
  else
    Result := Result + ' ';
end;

function TObjectDataSet.GetCurrentModel<T>: T;
begin
  Result := System.Default(T);
  if Active and (Index > -1) and (Index < RecordCount) then
    Result := T(IndexList.Items[Index]);
end;

function TObjectDataSet.GetFilterCount: Integer;
begin
  Result := 0;
  if Filtered then
    Result := IndexList.Count;
end;

function TObjectDataSet.GetFilteredDataList<T>: IList<T>;
var
  i: Integer;
begin
  Result := TCollections.CreateList<T>;
  for i := 0 to IndexList.Count - 1 do
    Result.Add(IndexList.Items[i]);
end;

function TObjectDataSet.GetRecordCount: Integer;
begin
  Result := IndexList.Count;
end;

function TObjectDataSet.GetSort: string;
begin
  Result := fSort;
end;

procedure TObjectDataSet.InitFilterParser;
begin
  fFilterParser.EnableWildcardMatching := not (foNoPartialCompare in FilterOptions);
  fFilterParser.CaseInsensitive := foCaseInsensitive in FilterOptions;

  if foCaseInsensitive in FilterOptions then
    fFilterParser.Expression := AnsiUpperCase(Filter)
  else
    fFilterParser.Expression := Filter;
end;

procedure TObjectDataSet.InitRttiPropertiesFromItemType(AItemTypeInfo: PTypeInfo);
var
  itemType: TRttiType;
  prop: TRttiProperty;
  field: TField;
begin
  if AItemTypeInfo = nil then
    Exit;

  fProperties.Clear;

  itemType := TType.GetType(AItemTypeInfo);
  for prop in itemType.GetProperties do
  begin
    if not (prop.Visibility in [mvPublic, mvPublished]) then
      Continue;

    if Fields.Count > 0 then
    begin
      field := Fields.FindField(prop.Name);
      if Assigned(field) and (field.FieldKind = fkData) then
      begin
        fProperties.Add(prop);
        if not prop.IsWritable then
          field.ReadOnly := True;
      end;
      Continue;
    end;

    if Assigned(fColumnAttributeClass) then
    begin
      if prop.HasCustomAttribute(fColumnAttributeClass) then
        fProperties.Add(prop);
    end
    else
      if prop.Visibility = mvPublished then
        fProperties.Add(prop);
  end;
end;

procedure TObjectDataSet.InternalClose;
begin
  inherited InternalClose;
  UnregisterChangeHandler;
end;

procedure TObjectDataSet.InternalCreateFields;
begin
  IndexList.Rebuild;

  if {$IF CompilerVersion >= 27}(FieldOptions.AutoCreateMode <> acExclusive)
    or not (lcPersistent in Fields.LifeCycles){$ELSE}DefaultFields{$IFEND} then
    CreateFields;
end;

function TObjectDataSet.InternalGetFieldValue(field: TField; const obj: TObject): Variant;
var
  prop: TRttiProperty;
begin
  if not fProperties.Any then
    InitRttiPropertiesFromItemType(obj.ClassInfo);

  if fProperties.TryGetFirst(prop, TPropertyFilters.IsNamed(field.FieldName)) then
    Result := prop.GetValue(obj).ToVariant
  else
    if field.FieldKind = fkData then
      if fDisabledFields.Add(field) then
      begin
        field.ReadOnly := True;
        field.Visible := False;
        raise EObjectDataSetException.CreateFmt(SPropertyNotFound, [field.FieldName]);
      end;
end;

procedure TObjectDataSet.InternalInitFieldDefs;
begin
  FieldDefs.Clear;
  if Fields.Count > 0 then
    LoadFieldDefsFromFields(Fields, FieldDefs)
  else
    LoadFieldDefsFromItemType;
end;

procedure TObjectDataSet.InternalOpen;
begin
  if IsFiltered then
    InitFilterParser;
  inherited InternalOpen;
end;

procedure TObjectDataSet.InternalRefresh;
begin
  inherited InternalRefresh;
  if Sorted then
    InternalSetSort(Sort);
end;

procedure TObjectDataSet.InternalSetSort(const value: string; index: Integer);
var
  pos: Integer;
  ownership: ICollectionOwnership;
  ownsObjects: Boolean;
  changed: Boolean;
begin
  if IsEmpty then
    Exit;

  DoBeforeSort;

  changed := value <> fSort;
  fIndexFields := CreateIndexList(value);
  fSorted := Length(fIndexFields) > 0;
  fSort := value;

  pos := Current;

  if fSorted then
  begin
    ownsObjects := Supports(fDataList, ICollectionOwnership, ownership)
      and ownership.OwnsObjects;
    try
      if ownsObjects then
        ownership.OwnsObjects := False;
      if changed then
        IndexList.MergeSort(CompareRecords)
      else
        IndexList.InsertionSort(index, CompareRecords);
    finally
      if ownsObjects then
        ownership.OwnsObjects := True;

      SetCurrent(pos);
    end;
  end;
  DoAfterSort;
end;

function TObjectDataSet.IsCursorOpen: Boolean;
begin
  Result := Assigned(fDataList) and inherited IsCursorOpen;
end;

procedure TObjectDataSet.LoadFieldDefsFromFields(Fields: TFields; FieldDefs: TFieldDefs);
var
  i: integer;
  field: TField;
  fieldDef: TFieldDef;
begin
  for i := 0 to Fields.Count - 1 do
  begin
    field := Fields[i];
    if FieldDefs.IndexOf(field.FieldName) = -1 then
    begin
      fieldDef := FieldDefs.AddFieldDef;
      fieldDef.Name := field.FieldName;
      fieldDef.DataType := field.DataType;
      fieldDef.Size := field.Size;
      if field.Required then
        fieldDef.Attributes := [DB.faRequired];
      if field.ReadOnly then
        fieldDef.Attributes := fieldDef.Attributes + [DB.faReadonly];
      if (field.DataType = ftBCD) and (field is TBCDField) then
        fieldDef.Precision := TBCDField(field).Precision;
      if field is TObjectField then
        LoadFieldDefsFromFields(TObjectField(field).Fields, fieldDef.ChildDefs);
    end;
  end;
end;

procedure TObjectDataSet.LoadFieldDefsFromItemType;
var
  prop: TRttiProperty;
  fieldType: TFieldType;
  fieldDef: TFieldDef;
  len, precision, scale: Integer;
  required, readOnly: Boolean;

  procedure DoGetFieldType(typeInfo: PTypeInfo);
  begin
    case typeInfo.Kind of
      tkInteger, tkEnumeration:
      begin
        len := -2;
        if typeInfo = System.TypeInfo(Boolean) then
          fieldType := ftBoolean
        else
          case typeInfo.TypeData.OrdType of
            otSByte: fieldType := ftShortint;
            otUByte: fieldType := ftByte;
            otSWord: fieldType := ftSmallint;
            otUWord: fieldType := ftWord;
            otSLong: fieldType := ftInteger;
            otULong: fieldType := ftLongWord;
        end;
      end;
      tkFloat:
      begin
        if typeInfo = System.TypeInfo(TDate) then
        begin
          fieldType := ftDate;
          len := -2;
        end
        else if typeInfo = System.TypeInfo(TDateTime) then
        begin
          fieldType := ftDateTime;
          len := -2;
        end
        else if typeInfo = System.TypeInfo(Currency) then
        begin
          fieldType := ftCurrency;
          len := -2;
        end
        else if typeInfo = System.TypeInfo(TTime) then
        begin
          fieldType := ftTime;
          len := -2;
        end
        else if (precision <> -2) or (scale <> -2) then
        begin
          fieldType := ftBCD;
          len := -2;
        end
        else
        begin
          fieldType := ftFloat;
          len := -2;
        end;
      end;
      tkString, tkLString, tkChar:
      begin
        fieldType := ftString;
        if len = -2 then
          len := fDefaultStringFieldLength;
      end;
      tkVariant, tkArray, tkDynArray:
      begin
        fieldType := ftVariant;
        len := -2;
      end;
      tkClass:
      begin
        if typeInfo = System.TypeInfo(TStringStream) then
          fieldType := ftMemo
        else
          fieldType := ftBlob;
        len := -2;
        readOnly := True;
      end;
      tkRecord:
        if typeInfo = System.TypeInfo(TGUID) then
        begin
          fieldType := ftGuid;
          len := 38;
        end
        else if typeInfo = System.TypeInfo(TBcd) then
          fieldType := ftFMTBcd
        else if IsNullable(typeInfo) then
          DoGetFieldType(GetUnderlyingType(typeInfo));
      tkInt64:
      begin
        fieldType := ftLargeint;
        len := -2;
      end;
      tkUString, tkWString, tkWChar, tkSet:
      begin
        fieldType := ftWideString;
        if len = -2 then
          len := fDefaultStringFieldLength;
      end;
    end;
  end;

begin
  InitRttiPropertiesFromItemType(fItemTypeInfo);

  if not fProperties.Any and Assigned(fColumnAttributeClass) then
    raise EObjectDataSetException.Create(SColumnPropertiesNotSpecified);
  for prop in fProperties do
  begin
    len := -2;
    precision := -2;
    scale := -2;
    required := False;
    readOnly := False;
    fieldType := ftWideString;

    DoGetFieldType(prop.PropertyType.Handle);

    fieldDef := FieldDefs.AddFieldDef;
    fieldDef.Name := prop.Name;

    fieldDef.DataType := fieldType;
    if len <> -2 then
      fieldDef.Size := len;

    fieldDef.Required := required;

    if fieldType in [ftFMTBcd, ftBCD] then
    begin
      if precision <> -2 then
        fieldDef.Precision := precision;

      if scale <> -2 then
        fieldDef.Size := scale;
    end;

    if readOnly then
      fieldDef.Attributes := fieldDef.Attributes + [DB.faReadOnly];

    if not prop.IsWritable then
      fieldDef.Attributes := fieldDef.Attributes + [DB.faReadOnly];
  end;
end;

function TObjectDataSet.ParserGetFunctionValue(Sender: TObject; const FuncName: string;
  const Args: Variant; var ResVal: Variant): Boolean;
var
  getValue: TGetValueFunc;
begin
  Result := TFilterFunctions.TryGetFunction(FuncName, getValue);
  if Result then
    ResVal := getValue(Args);
end;

function TObjectDataSet.ParserGetVariableValue(Sender: TObject;
  const VarName: string; var Value: Variant): Boolean;
var
  field: TField;
begin
  Result := FilterCache.TryGetValue(VarName, Value);
  if not Result then
  begin
    field := FindField(Varname);
    if Assigned(field) then
    begin
      Value := InternalGetFieldValue(field, IndexList.Items[Index]);
      FilterCache.Add(VarName, Value);
      Result := True;
    end;
  end;
end;

procedure TObjectDataSet.RebuildPropertiesCache;
var
  itemType: TRttiType;
  i: Integer;
begin
  fProperties.Clear;
  itemType := TType.GetType(fItemTypeInfo);
  for i := 0 to Fields.Count - 1 do
    fProperties.Add(itemType.GetProperty(Fields[i].FieldName));
end;

procedure TObjectDataSet.RegisterChangeHandler;
begin
  UnregisterChangeHandler;
  if Assigned(fDataList) and fTrackChanges then
    fDataList.OnChanged.Add(DoOnDataListChange);
end;

procedure TObjectDataSet.SetDataList(const value: IObjectList);
begin
  fDataList := value;
  fItemTypeInfo := fDataList.ElementType;
  IndexList.DataList := fDataList;
  RegisterChangeHandler;
  if Active then
    Refresh;
end;

procedure TObjectDataSet.SetFilterText(const Value: string);
begin
  if Value = Filter then
    Exit;

  if Active then
  begin
    CheckBrowseMode;
    inherited SetFilterText(Value);

    if Filtered then
    begin
      UpdateFilter;
      First;
    end
    else
    begin
      UpdateFilter;
      Resync([]);
      First;
    end;
  end
  else
    inherited SetFilterText(Value);
end;

procedure TObjectDataSet.SetSort(const value: string);
begin
  CheckActive;
  if State in dsEditModes then
    Post;

  UpdateCursorPos;
  InternalSetSort(value);
  Resync([]);
end;

procedure TObjectDataSet.SetTrackChanges(const value: Boolean);
begin
  if fTrackChanges <> value then
  begin
    fTrackChanges := value;
    RegisterChangeHandler;
  end;
end;

procedure TObjectDataSet.UnregisterChangeHandler;
begin
  if Assigned(fDataList) then
    fDataList.OnChanged.Remove(DoOnDataListChange);
end;

procedure TObjectDataSet.UpdateFilter;
begin
  if not Active then
    Exit;

  DoBeforeFilter;

  if IsFiltered then
    InitFilterParser;

  DisableControls;
  try
    First;
    if Sorted then
      InternalSetSort(GetChangedSortText(Sort));
  finally
    EnableControls;
  end;
  UpdateCursorPos;
  Resync([]);
  First;

  DoAfterFilter;
end;

{$ENDREGION}


end.
