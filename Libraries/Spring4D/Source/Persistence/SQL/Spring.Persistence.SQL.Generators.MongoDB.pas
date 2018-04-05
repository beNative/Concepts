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

unit Spring.Persistence.SQL.Generators.MongoDB;

interface

uses
  Rtti,
  SysUtils,
  Spring.Persistence.Mapping.Attributes,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Generators.Abstract,
  Spring.Persistence.SQL.Generators.NoSQL,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Types;

type
  /// <summary>
  ///   Represents <b>MongoDB</b> query generator.
  /// </summary>
  TMongoDBGenerator = class(TNoSQLGenerator)
  private
    class var fsJson: TFormatSettings;
    class constructor Create;
  protected
    function ResolveFieldAndExpression(const fieldName: string; out field: string;
      out expression: string; const delta: Integer = 1): Boolean;
    function GetPrefix(const table: TSQLTable): string; virtual;
    function WrapResult(const AResult: string): string; virtual;

    function DoGetInsertJson(const command: TInsertCommand): string;
    function DoGetUpdateJson(const command: TUpdateCommand): string;
    function DoGetFindUpdateJson(const command: TUpdateCommand): string;
    function CreateClassInsertCommandAndTable(const fromValue: TValue): TInsertCommand;
    function CreateClassUpdateCommandAndTable(const fromValue: TValue): TUpdateCommand;
    function ToJsonValue(const value: TValue): string;
  public
    function GetQueryLanguage: TQueryLanguage; override;
    function GenerateUniqueId: Variant; override;
    function GetUpdateVersionFieldQuery(const command: TUpdateCommand;
      const versionColumn: VersionAttribute; const version, primaryKey: Variant): Variant; override;

    function GenerateWhere(const field: TSQLWhereField): string; override;
    function GenerateSelect(const command: TSelectCommand): string; override;
    function GenerateInsert(const command: TInsertCommand): string; override;
    function GenerateUpdate(const command: TUpdateCommand): string; override;
    function GenerateDelete(const command: TDeleteCommand): string; override;

    function GeneratePagedQuery(const sql: string; limit, offset: Integer): string; override;
    function GenerateGetQueryCount(const sql: string): string; override;
    function GetSQLTableCount(const tableName: string): string; override;
  end;

implementation

uses
  Math,
  StrUtils,
  TypInfo,
  Variants,
  mongoID,
  MongoBson,
  Spring,
  Spring.Collections,
  Spring.Persistence.Core.EntityCache,
  Spring.Persistence.SQL.Register,
  Spring.Reflection;


{$REGION 'TMongoDBGenerator'}

class constructor TMongoDBGenerator.Create;
begin
  fsJson := TFormatSettings.Create;
  fsJson.DecimalSeparator := '.';
  fsJson.ShortDateFormat := 'yyyy-mm-dd';
  fsJson.DateSeparator := '-';
  fsJson.TimeSeparator := ':';
  fsJson.LongDateFormat := 'yyyy-mm-dd hh:mm:ss';
end;

function TMongoDBGenerator.CreateClassInsertCommandAndTable(
  const fromValue: TValue): TInsertCommand;
var
  entity: TObject;
  table: TSQLTable;
begin
  entity := fromValue.AsObject;

  table := TSQLTable.Create;
  table.Name := entity.ClassName;

  Result := TInsertCommand.Create(table);
  Result.Entity := entity;
  Result.SetCommandFieldsFromColumns(TEntityCache.Get(entity.ClassType).Columns);
end;

function TMongoDBGenerator.CreateClassUpdateCommandAndTable(
  const fromValue: TValue): TUpdateCommand;
var
  entity: TObject;
  table: TSQLTable;
begin
  entity := fromValue.AsObject;

  table := TSQLTable.Create;
  table.Name := entity.ClassName;

  Result := TUpdateCommand.Create(table);
  Result.Entity := entity;
  Result.SetCommandFieldsFromColumns(TEntityCache.Get(entity.ClassType).Columns);
end;

function TMongoDBGenerator.DoGetFindUpdateJson(
  const command: TUpdateCommand): string;
var
  i: Integer;
  field: TSQLWhereField;
  value: TValue;
begin
  Result := '{';
  for i := 0 to command.WhereFields.Count - 1 do
  begin
    field := command.WhereFields[i];
    if i <> 0 then
      Result := Result + ',';

    value := field.Column.GetValue(command.Entity);
    Result := Result + QuotedStr(field.Name) + ': ' + ToJsonValue(value);
  end;  
  Result := Result + '}';
end;

function TMongoDBGenerator.DoGetInsertJson(
  const command: TInsertCommand): string;
var
  i, j: Integer;
  insertField: TSQLInsertField;
  classCommand: TInsertCommand;
  list: IList;
  current: TValue;

  function GetJsonValueFromClass(const value: TValue): string;
  begin
    Result := 'null';
    if value.AsObject = nil then
      Exit;  

    classCommand := CreateClassInsertCommandAndTable(value);
    try
      Result := DoGetInsertJson(classCommand);
    finally
      classCommand.Table.Free;
      classCommand.Free;
    end;  
  end;

begin
  Result := '{';
  for i := 0 to command.InsertFields.Count - 1 do
  begin
    if i <> 0 then
      Result := Result + ',';

    insertField := command.InsertFields[i];
    case insertField.Column.Member.MemberType.TypeKind of
      tkClass:
      begin
        Result := Result + QuotedStr(insertField.Name) + ': ';
        current := insertField.Column.GetValue(command.Entity);
        Result := Result + GetJsonValueFromClass(current);         
      end;
      tkInterface:
      begin
        Result := Result + QuotedStr(insertField.Name) + ': [';
        list := insertField.Column.GetValue(command.Entity).AsInterface as IList;
        for j := 0 to list.Count - 1 do
        begin
          if j <> 0 then
            Result := Result + ',';

          current := list[j];  
          if list.ElementType.Kind = tkClass then
            Result := Result + GetJsonValueFromClass(current)                              
          else
            Result := Result + ToJsonValue(current);
        end;
        Result := Result + ']';
      end
      else
      begin
        current := TValue.Empty;
        if command.Entity <> nil then
          current := insertField.Column.GetValue(command.Entity);
          
        Result := Result + QuotedStr(insertField.Name) + ': '
          + ToJsonValue(current);
      end;          
    end;
  end;
  Result := Result + '}';
end;

function TMongoDBGenerator.DoGetUpdateJson(
  const command: TUpdateCommand): string;
var
  i, j: Integer;
  updateField: TSQLUpdateField;
  classCommand: TUpdateCommand;
  list: IList;
  current: TValue;

  function GetJsonValueFromClass(const value: TValue): string;
  begin
    Result := 'null';
    if value.AsObject = nil then
      Exit;  

    classCommand := CreateClassUpdateCommandAndTable(value);
    try
      Result := DoGetUpdateJson(classCommand);
    finally
      classCommand.Table.Free;
      classCommand.Free;
    end;  
  end;

begin
  Result := '{ $set: {';
  for i := 0 to command.UpdateFields.Count - 1 do
  begin
    if i <> 0 then
      Result := Result + ',';
    {TODO -oLinas -cGeneral : use dot notation in future for sub properties}
    updateField := command.UpdateFields[i];
    case updateField.Column.Member.MemberType.TypeKind of
      tkClass:
      begin
        Result := Result + QuotedStr(updateField.Name) + ': ';
        current := updateField.Column.GetValue(command.Entity);
        Result := Result + GetJsonValueFromClass(current);         
      end;
      tkInterface:
      begin
        Result := Result + QuotedStr(updateField.Name) + ': [';
        list := updateField.Column.GetValue(command.Entity).AsInterface as IList;
        for j := 0 to list.Count - 1 do
        begin
          if j <> 0 then
            Result := Result + ',';

          current := list[j];  
          if list.ElementType.Kind = tkClass then
            Result := Result + GetJsonValueFromClass(current)                              
          else
            Result := Result + ToJsonValue(current);
        end;
        Result := Result + ']';
      end
      else
      begin
        current := TValue.Empty;
        if command.Entity <> nil then
          current := updateField.Column.GetValue(command.Entity);
          
        Result := Result + QuotedStr(updateField.Name) + ': '
          + ToJsonValue(current);
      end;          
    end;
  end;     
  Result := Result + '}}';  
end;

function TMongoDBGenerator.GenerateDelete(
  const command: TDeleteCommand): string;
begin
  Result := 'D' + GetPrefix(command.Table) +'{"_id": '+ command.WhereFields.First.ParamName + '}';
end;

function TMongoDBGenerator.GenerateGetQueryCount(const sql: string): string;
begin
  Result := 'count' + Copy(sql, 2, Length(sql));
end;

function TMongoDBGenerator.GenerateInsert(
  const command: TInsertCommand): string;
begin
  if command.Entity = nil then
    Exit('');
  Result := DoGetInsertJson(command);
  Result := 'I' + GetPrefix(command.Table) + Result;
end;

function TMongoDBGenerator.GeneratePagedQuery(const sql: string;
  limit, offset: Integer): string;
begin
  Result := Format('page%d_%d_%s', [limit, offset, Copy(sql, 2, Length(sql))]);
end;

function TMongoDBGenerator.GenerateSelect(const command: TSelectCommand): string;
const
  SortingDirectionNames: array[TSortingDirection] of string = ('1', '-1');
var
  field, previousField: TSQLWhereField;
  i: Integer;
  statementType: string;
begin
  Result := '';
  statementType := 'S';
  for i := 0 to command.WhereFields.Count - 1 do
  begin
    field := command.WhereFields[i];
    previousField := command.WhereFields[Max(0, i - 1)];

    if not (previousField.WhereOperator in StartOperators)
      and not (field.WhereOperator in EndOperators) and (i <> 0) then
      Result := Result + ',';

    Result := Result + GenerateWhere(field);
  end;

  for i := 0 to command.OrderByFields.Count - 1 do
  begin
    if i <> 0 then
      statementType := statementType + ','
    else
      statementType := 'SO';

    statementType := statementType + '{' + AnsiQuotedStr(command.OrderByFields[i].Name, '"') + ': ' +
      SortingDirectionNames[command.OrderByFields[i].SortingDirection] + '}';
  end;
  if Length(statementType) > 1 then
    Insert(IntToStr(Length(statementType)-2) + '_', statementType, 3); //insert length   SO100_{}

  Result := WrapResult(Result);
  Result := statementType + GetPrefix(command.Table) + Result;
end;

function TMongoDBGenerator.GenerateUniqueId: Variant;
begin
  Result := mongoObjectId;
end;

function TMongoDBGenerator.GenerateUpdate(
  const command: TUpdateCommand): string;
var
  findUpdateJson: string;
begin
  if command.Entity = nil then
    Exit('');

  findUpdateJson := DoGetFindUpdateJson(command);
  Result := Format('U%d_%s%s%s', 
    [Length(findUpdateJson), 
    findUpdateJson, 
    GetPrefix(command.Table),
    DoGetUpdateJson(command)]);
end;

function TMongoDBGenerator.GenerateWhere(const field: TSQLWhereField): string;
const
  WhereOpNames: array[TWhereOperator] of string = (
    {woEqual} '=', {woNotEqual} '$ne', {woMore} '$gt', {woLess} '$lt',
    {woLike} '$regex', {woNotLike} '', {woMoreOrEqual} '$gte',
    {woLessOrEqual} '$lte', {woIn} '$in', {woNotIn} '$nin', {woIsNull} '',
    {woIsNotNull} '', {woOr} '$or', {woOrEnd} '', {woAnd} '$and', {woAndEnd}'',
    {woNot} '$not', {woNotEnd} '',{woBetween} '', {woJunction} '');
var
  LField, LExpression: string;
begin
  case field.WhereOperator of
    woEqual: Result := '{' + AnsiQuotedStr(field.Name, '"') + ' : ' + field.ParamName + '}';
    woNotEqual, woMoreOrEqual, woMore, woLess, woLessOrEqual :
      Result := Format('{%s: { %s: %s}}', [AnsiQuotedStr(field.Name, '"'), WhereOpNames[field.WhereOperator], field.ParamName]);
    woIsNotNull: Result := Format('{%s: { $ne: null }}', [AnsiQuotedStr(field.Name, '"')]);
    woIsNull: Result := Format('{%s: null}', [AnsiQuotedStr(field.Name, '"')]);
    woBetween: Result := Format('{$and: [ { %0:s: { $gte: %1:s} }, { %0:s: { $lte: %2:s} } ] }'
      , [AnsiQuotedStr(field.Name, '"'), field.ParamName, field.ParamName2]);
    woOr, woAnd:
      Result := Format('{%s: [', [WhereOpNames[field.WhereOperator]]);
    woNot: Result := Format('%s: ', [WhereOpNames[field.WhereOperator]]);
    woNotEnd: Result := '';
    woOrEnd, woAndEnd: Result := ']}';
    woLike:
    begin
      Result := field.Name;
      if ResolveFieldAndExpression(field.Name, LField, LExpression) then
        Result := Format('{ %s: { $regex: ''.*%s.*'', $options: ''i''}}', [AnsiQuotedStr(LField, '"'), LExpression]);
    end;
    woNotLike:
    begin
      Result := field.Name;
      if ResolveFieldAndExpression(field.Name, LField, LExpression) then
        Result := Format('{ %s: { $not: "/.*%s.*/i"}}', [AnsiQuotedStr(LField, '"'), LExpression]);
    end;
    woIn, woNotIn:
    begin
      Result := field.Name;
      if ResolveFieldAndExpression(field.Name, LField, LExpression) then
        Result := Format('{%s: { %s: [%s] } }', [AnsiQuotedStr(LField, '"'), WhereOpNames[field.WhereOperator], LExpression]);
    end;
  end;
end;

function TMongoDBGenerator.GetPrefix(const table: TSQLTable): string;
begin
  Result := '[' + table.Name + ']';
end;

function TMongoDBGenerator.GetQueryLanguage: TQueryLanguage;
begin
  Result := qlMongoDB;
end;

function TMongoDBGenerator.GetSQLTableCount(const tableName: string): string;
begin
  Result := 'count' + '[' + tableName + ']';
end;

function TMongoDBGenerator.GetUpdateVersionFieldQuery(
  const command: TUpdateCommand; const versionColumn: VersionAttribute;
  const version, primaryKey: Variant): Variant;
begin
  Result := BSON([command.PrimaryKeyColumn.ColumnName, primaryKey, versionColumn.ColumnName, version]);
end;

function TMongoDBGenerator.ResolveFieldAndExpression(const fieldName: string;
  out field, expression: string; const delta: Integer): Boolean;
var
  LPos, LPos2: Integer;
begin
  //Field NOT IN (1,2,3)
  LPos := PosEx(' ', fieldName);
  field := Copy(fieldName, 1, LPos - 1);
  LPos := PosEx(' ', fieldName, LPos + 1);
  LPos2 := PosEx(' ', fieldName, LPos + 1);
  if LPos2 > 0 then
    LPos := LPos2;

  expression := Copy(fieldName, LPos + 1 + delta, Length(fieldName) - LPos - 1 - delta);
  Result := True;
end;

function IsObjectId(const value: string): Boolean;
begin
  Result := StartsText('ObjectID("', value);
end;

function TMongoDBGenerator.ToJsonValue(const value: TValue): string;
var
  variantValue: Variant;
begin
  Result := 'null';
  if value.IsEmpty then
    Exit;

  variantValue := value.ToVariant;
  case VarType(variantValue) of
    varString, varUString, varStrArg, varOleStr:
    begin
      Result := VarToStrDef(variantValue, 'null');
      if IsObjectId(Result) then   //ObjectID("sdsd457845")
        Result := '"' + ReplaceStr(Result, '"', '\"') + '"'
      else
        Result := AnsiQuotedStr(Result, '"');
    end;
    varBoolean:
    begin
      if Boolean(variantValue) then
        Result := 'true'
      else
        Result := 'false';
    end;
    varDouble:
      Result := FloatToStr(variantValue, fsJson);
    varDate:
      Result := DateTimeToStr(variantValue, fsJson);
    else
      Result := VarToStrDef(variantValue, 'null');
  end;    
end;

function TMongoDBGenerator.WrapResult(const AResult: string): string;
begin
  Result := AResult;
  if Length(Result) = 0 then
    Result := '{}'
  else
  begin
    if not StartsStr('{', Result) then
    begin
      Result := '{' + Result + '}';
    end;
  end;
end;

{$ENDREGION}


initialization
  TSQLGeneratorRegister.RegisterGenerator(TMongoDBGenerator.Create);

end.
