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

unit Spring.Persistence.Mapping.CodeGenerator.DB;

interface

uses
  ADODB,
  Classes,
  DB,
  Spring.Collections,
  Spring.Persistence.Mapping.CodeGenerator.Abstract;

type
  TEntityModelDataLoader = class
  private
    fEntities: IList<TEntityModelData>;
    fConnected: Boolean;
    fDefaultSchemaName: string;
    fDBConnection: TADOConnection;
    fConnectionString: string;
    fPrimaryKeys: IDictionary<string, Boolean>;
    fDatabaseName: string;
    fOutputDir: string;
    fUnitPrefix: string;
    fUseNullableTypes: Boolean;
  protected
    function CreateEntityDataFromFields(const fields: TFields;
      const tableName: string): TEntityModelData;
    function GetFieldTypeName(const field: TField): string;
    procedure GetTables(var tables: TStrings; systemTables: Boolean = False);
    procedure ParseTableName(const ATableName: string; out ATable, ASchema: string);

    function GetIsAutoIncField(const dataset: TADODataSet; const columnName: string): Boolean;
    function GetIsPrimaryKeyField(ADataset: TADODataSet; const AColumnName: string): Boolean;
    function GetPrimKeyFindKey(const ATable, ASchema, AColumn: string): string;
    procedure LoadPrimaryKeys;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function Connect: Boolean;
    function GetUnitName(const tableName: string): string;

    function Execute: Boolean;

    procedure LoadTables;

    property Connected: Boolean read fConnected;
    property Entities: IList<TEntityModelData> read fEntities;

    //serializable properties
    property DatabaseName: string read fDatabaseName write fDatabaseName;
    property DefaultSchemaName: string read fDefaultSchemaName write fDefaultSchemaName;
    property ConnectionString: string read fConnectionString write fConnectionString;
    property OutputDir: string read fOutputDir write fOutputDir;
    property UnitPrefix: string read fUnitPrefix write fUnitPrefix;
    property UseNullableTypes: Boolean read fUseNullableTypes write fUseNullableTypes;
  end;

implementation

uses
  StrUtils,
  SysUtils,
  Variants;


{$REGION 'TEntityModelDataLoader'}

constructor TEntityModelDataLoader.Create;
begin
  inherited Create;
  fEntities := TCollections.CreateObjectList<TEntityModelData>;

  fUnitPrefix := 'ORM.Model.';

  fDBConnection := TADOConnection.Create(nil);
  fDBConnection.LoginPrompt := False;
  fPrimaryKeys := TCollections.CreateDictionary<string, Boolean>;
end;

destructor TEntityModelDataLoader.Destroy;
begin
  fDBConnection.Free;
  inherited Destroy;
end;

function TEntityModelDataLoader.Connect: Boolean;
begin
  fConnected := False;

  if fDBConnection.Connected then
    fDBConnection.Close;

  fDBConnection.ConnectionString := fConnectionString;
  fDBConnection.Open;

  fConnected := fDBConnection.Connected;
  Result := fConnected;
end;

function TEntityModelDataLoader.CreateEntityDataFromFields(
  const fields: TFields; const tableName: string): TEntityModelData;
var
  field, nameField, isNullableField, precisionField, scaleField, charLength, descriptionField: TField;
  columnData: TColumnData;
  table, schema: string;
  dataset: TADODataSet;
  schemaName: Variant;
begin
  Result := TEntityModelData.Create;
  ParseTableName(tableName, table, schema);
  Result.TableName := table;
  Result.SchemaName := schema;

  dataset := TADODataSet.Create(nil);
  try
    if schema <> '' then
      schemaName := schema;

    fDBConnection.OpenSchema(siColumns, VarArrayOf([fDatabaseName, schemaName, table]), EmptyParam, dataset);

    nameField := dataset.FieldByName('COLUMN_NAME');
    isNullableField := dataset.FieldByName('IS_NULLABLE');
    precisionField := dataset.FieldByName('NUMERIC_PRECISION');
    scaleField := dataset.FieldByName('NUMERIC_SCALE');
    charLength := dataset.FieldByName('CHARACTER_MAXIMUM_LENGTH');
    descriptionField := dataset.FieldByName('DESCRIPTION');

    while not dataset.Eof do
    begin
      columnData := TColumnData.Create;
      columnData.ColumnName := nameField.AsString;
      columnData.IsRequired := not isNullableField.AsBoolean;
      columnData.NotNull := not isNullableField.AsBoolean;

      if not precisionField.IsNull then
        columnData.ColumnPrecision := precisionField.AsInteger;

      if not scaleField.IsNull then
        columnData.ColumnScale := scaleField.AsInteger;

      if not charLength.IsNull then
        columnData.ColumnLength := charLength.AsInteger;

      if not descriptionField.IsNull then
        columnData.ColumnDescription := descriptionField.AsString;

      field := fields.FindField(columnData.ColumnName);
      if Assigned(field) then
      begin
        columnData.IsPrimaryKey :=
          GetIsPrimaryKeyField(field.DataSet as TADODataSet, columnData.ColumnName);
        field.Required := columnData.IsRequired;
        columnData.ColumnTypeName := GetFieldTypeName(field);
        columnData.DontUpdate := field.ReadOnly;
        columnData.IsAutogenerated :=
          GetIsAutoIncField(field.DataSet as TADODataSet, columnData.ColumnName);
      end;

      Result.Columns.Add(columnData);

      dataset.Next;
    end;
  finally
    dataset.Free;
  end;
end;

function TEntityModelDataLoader.Execute: Boolean;
begin
  Result := False;
end;

function TEntityModelDataLoader.GetFieldTypeName(const field: TField): string;
begin
  Result := FieldTypeNames[field.DataType];

  case field.DataType of
    ftBytes, ftVarBytes, ftBlob, ftMemo, ftFmtMemo, ftOraBlob, ftOraClob, ftStream, ftObject:
      Result := 'TMemoryStream';
    ftGraphic: Result := 'TPicture';
    ftFMTBcd:
      if TFMTBCDField(field).Size = 0 then
        Result := 'Int64'
      else
        Result := 'Double';
    ftBCD:
      if TBCDField(field).Size = 0 then
        Result := 'Int64'
      else
        Result := 'Double';
    ftBoolean: Result := 'Boolean';
    ftDate: Result := 'TDate';
    ftDateTime: Result := 'TDateTime';
    ftTime: Result := 'TTime';
    ftSmallint: Result := 'SmallInt';
    ftInteger: Result := 'Integer';
    ftWord: Result := 'Word';
    ftFloat: Result := 'Double';
    ftCurrency: Result := 'Currency';
    ftAutoInc: Result := 'Int64';
    ftLongWord: Result := 'Cardinal';
    ftGuid: Result := 'TGuid';
    ftShortint: Result := 'ShortInt';
    ftByte: Result := 'Byte';
    ftExtended: Result := 'Extended';
    ftLargeint: Result := 'Int64';
    ftWideMemo: Result := 'string';
    ftString, ftWideString, ftFixedWideChar, ftFixedChar: Result := 'string';
  end;
end;

function TEntityModelDataLoader.GetIsAutoIncField(const dataset: TADODataSet;
  const columnName: string): Boolean;
var
  props: Properties;
  i: Integer;
  propName: string;
begin
  props := dataset.Recordset.Fields.Item[columnName].Properties;

  for i := 0 to props.Count - 1 do
  begin
    propName := LowerCase( string(props.Item[i].Name));
    if Pos('autoincrement', propName) > 0 then
      Exit(props.Item[i].Value);
  end;
  Result := False;
end;

function TEntityModelDataLoader.GetIsPrimaryKeyField(ADataset: TADODataSet;
  const AColumnName: string): Boolean;
var
  i: Integer;
  LProps: Properties;
  LPropName: string;
begin
  LProps := ADataset.Recordset.Fields.Item[AColumnName].Properties;

  for i := 0 to LProps.Count - 1 do
  begin
    LPropName := LowerCase( string(LProps.Item[i].Name));
    if PosEx('keycolumn', LPropName) > 0 then
    begin
      Exit(Boolean(LProps.Item[i].Value));
    end;
  end;
  Result := False;
end;

function TEntityModelDataLoader.GetPrimKeyFindKey(const ATable, ASchema, AColumn: string): string;
begin
  Result := UpperCase(ASchema + '.' + ATable + '_' + AColumn);
end;

procedure TEntityModelDataLoader.GetTables(var tables: TStrings; systemTables: Boolean);
var
  typeField, nameField, schemaField: TField;
  tableType, tableName: string;
  dataset: TADODataSet;
begin
  if not fConnected then
    Exit;

  dataset := TADODataSet.Create(nil);
  try
    fDBConnection.OpenSchema(siTables, EmptyParam, EmptyParam, dataset);
    typeField := dataset.FieldByName('TABLE_TYPE'); { do not localize }
    nameField := dataset.FieldByName('TABLE_NAME'); { do not localize }
    schemaField := dataset.FieldByName('TABLE_SCHEMA'); { do not localize }
    tables.BeginUpdate;
    try
      tables.Clear;
      while not dataset.EOF do
      begin
        tableType := typeField.AsWideString;
        if (tableType = 'TABLE')
          or (systemTables and (tableType = 'SYSTEM TABLE')) then
        begin
          tableName := nameField.AsString;
          if schemaField.AsString <> '' then
            tableName := schemaField.AsString + '.' + tableName;
          tables.Add(tableName);
        end;
        dataset.Next;
      end;
    finally
      tables.EndUpdate;
    end;
  finally
    dataset.Free;
  end;
end;

function TEntityModelDataLoader.GetUnitName(const tableName: string): string;
begin
  Result := fUnitPrefix + tableName;
end;

procedure TEntityModelDataLoader.LoadPrimaryKeys;
var
  dataset: TADODataSet;
  tableNameField, columnNameField, schemaField: TField;
  VDBName, VSchemaName, VTableName: Variant;
  LTables: TStrings;
  LFullTable, LTable, LSchema: string;
begin
  fPrimaryKeys.Clear;

  dataset := TADODataSet.Create(nil);
  LTables := TStringList.Create;
  try
    GetTables(LTables);

    VDBName := fDatabaseName;

    for LFullTable in LTables do
    begin
      ParseTableName(LFullTable, LTable, LSchema);

      if (LSchema <> '') then
        VSchemaName := LSchema;

      VTableName := LTable;

      fDBConnection.OpenSchema(siPrimaryKeys, VarArrayOf([ VDBName , VSchemaName , VTableName ])
        , EmptyParam, dataset);

      tableNameField := dataset.FieldByName('TABLE_NAME');
      columnNameField := dataset.FieldByName('COLUMN_NAME');
      schemaField := dataset.FieldByName('TABLE_SCHEMA');

      while not dataset.Eof do
      begin
        fPrimaryKeys.Add(
          GetPrimKeyFindKey(tableNameField.AsString, schemaField.AsString,
            columnNameField.AsString), True);
        dataset.Next;
      end;
    end;
  finally
    dataset.Free;
    LTables.Free;
  end;
end;

procedure TEntityModelDataLoader.LoadTables;
var
  LTables: TStrings;
  LTableName: string;
  LDataset: TADODataSet;
  LEntityModel: TEntityModelData;
begin
  fEntities.Clear;

  if not fConnected then
    if not Connect then
      Exit;

  LTables := TStringList.Create;
  LDataset := TADODataSet.Create(nil);
  try
    LDataset.Connection := fDBConnection;
    LDataset.DisableControls;
    GetTables(LTables);

    for LTableName in LTables do
    begin
      if LDataset.Active then
        LDataset.Close;

      LDataset.CommandText := Format('SELECT * FROM %0:s WHERE 1=2', [LTableName]);
      try
        LDataset.Open;

        if LDataset.Fields.Count > 0 then
        begin
          LEntityModel := CreateEntityDataFromFields(LDataset.Fields, LTableName);
          fEntities.Add(LEntityModel);
        end;
      except
        on E:Exception do
        begin
          //spawn
        end;
      end;
    end;
  finally
    LTables.Free;
    LDataset.Free;
  end;
end;

procedure TEntityModelDataLoader.ParseTableName(const ATableName: string; out ATable,
  ASchema: string);
var
  LIndex: Integer;
begin
  ATable := ATableName;
  ASchema := '';
  LIndex := PosEx('.', ATableName);
  if (LIndex > 1) then
  begin
    ATable := Copy(ATableName, LIndex + 1, Length(ATableName)-1);
    ASchema := Copy(ATableName, 1, LIndex - 1);
  end;
end;

{$ENDREGION}


end.
