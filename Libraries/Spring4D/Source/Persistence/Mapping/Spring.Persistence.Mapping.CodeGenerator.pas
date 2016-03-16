{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2016 Spring4D Team                           }
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

unit Spring.Persistence.Mapping.CodeGenerator;

interface

uses
  SysUtils,
  Spring.Persistence.Mapping.CodeGenerator.Abstract;


type
  TDelphiUnitCodeGenerator = class(TAbstractCodeGenerator)
  private
    fIntfBuilder: TStringBuilder;
    fImplBuilder: TStringBuilder;
    fUnitPrefix: string;
    fUseNullableTypes: Boolean;
  protected
    function AddTypeAttribute(const attributeText: string): TStringBuilder; virtual;
    function AddClassAttribute(const attributeText: string): TStringBuilder; virtual;

    function AddUnit(const unitName: string): TStringBuilder; virtual;
    function AddPrivateField(const columnData: TColumnData): TStringBuilder; virtual;
    function AddPublicProperty(const columnData: TColumnData): TStringBuilder; virtual;
    function AddEntityDeclaration(const entityData: TEntityModelData): TStringBuilder; virtual;

    function GetPrivateFieldName(const columnData: TColumnData): string; virtual;
    function GetColumnAttributeText(const columnData: TColumnData): string; virtual;
    function GetColumnProperties(const columnData: TColumnData): string; virtual;
    function GetColumnTypeName(const columnData: TColumnData): string; virtual;

    function DoGenerate(const entityData: TEntityModelData): string; override;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function GetUnitName(const entityData: TEntityModelData): string;

    function Generate(const entityData: TEntityModelData): string;

    property UnitPrefix: string read fUnitPrefix write fUnitPrefix;
    property UseNullableTypes: Boolean read fUseNullableTypes write fUseNullableTypes;
  end;

implementation

const
  UNIT_ATTRIBUTES = 'Spring.Persistence.Mapping.Attributes';
  UNIT_GRAPHICS = 'Spring.Persistence.Core.Graphics';


{$REGION 'TDelphiUnitCodeGenerator'}

constructor TDelphiUnitCodeGenerator.Create;
begin
  inherited Create;
  fIntfBuilder := TStringBuilder.Create;
  fImplBuilder := TStringBuilder.Create;
  fUnitPrefix := 'ORM.Model.';
end;

destructor TDelphiUnitCodeGenerator.Destroy;
begin
  fIntfBuilder.Free;
  fImplBuilder.Free;
  inherited Destroy;
end;

function TDelphiUnitCodeGenerator.AddTypeAttribute(const attributeText: string): TStringBuilder;
begin
  Result := fIntfBuilder.AppendLine.Append(Indent).Append(attributeText);
end;

function TDelphiUnitCodeGenerator.AddClassAttribute(const attributeText: string): TStringBuilder;
begin
  Result := fIntfBuilder.AppendLine.Append(Indent).Append(Indent).Append(attributeText);
end;

function TDelphiUnitCodeGenerator.AddEntityDeclaration(
  const entityData: TEntityModelData): TStringBuilder;
begin
  //add attribute
  AddTypeAttribute('[Entity]');
  AddTypeAttribute(Format('[Table(%0:s, %1:s)]',
    [QuotedStr(entityData.TableName), QuotedStr(entityData.SchemaName)]));

  fIntfBuilder.AppendLine.Append(Indent);

  Result := fIntfBuilder.Append(EntityTypePrefix).Append(entityData.TableName).Append(' = class');
end;

function TDelphiUnitCodeGenerator.AddPrivateField(
  const columnData: TColumnData): TStringBuilder;
begin
  Result := fIntfBuilder.AppendLine
    .Append(Indent).Append(Indent).Append(GetPrivateFieldName(columnData))
    .Append(': ').Append(GetColumnTypeName(columnData)).Append(';');
end;

function TDelphiUnitCodeGenerator.AddPublicProperty(
  const columnData: TColumnData): TStringBuilder;
begin
  if columnData.IsAutogenerated then
    AddClassAttribute('[AutoGenerated]');

  AddClassAttribute(GetColumnAttributeText(columnData));

  Result := fIntfBuilder.AppendLine.Append(Indent).Append(Indent).Append('property ')
    .Append(columnData.ColumnName).Append(': ').Append(GetColumnTypeName(columnData))
    .Append(' read ').Append(GetPrivateFieldName(columnData)).Append(' write ')
    .Append(GetPrivateFieldName(columnData)).Append(';');
  {TODO -oLinas -cGeneral : add implementation section if needed, e.g. for lazy types}
end;

function TDelphiUnitCodeGenerator.AddUnit(const unitName: string): TStringBuilder;
begin
  Result := fIntfBuilder.Append(',').AppendLine.Append(Indent).Append(unitName);
end;

function TDelphiUnitCodeGenerator.DoGenerate(
  const entityData: TEntityModelData): string;
var
  columnData: TColumnData;
begin
  fIntfBuilder.Clear;
  fImplBuilder.Clear;

  fIntfBuilder.AppendFormat('unit %s;', [GetUnitName(entityData)]).AppendLine.AppendLine;

  fIntfBuilder.Append('interface').AppendLine.AppendLine;
  fImplBuilder.AppendLine.AppendLine.Append('implementation').AppendLine;

  fIntfBuilder.Append('uses').AppendLine.Append(Indent).Append(UNIT_ATTRIBUTES);
  AddUnit(UNIT_GRAPHICS);
  fIntfBuilder.Append(';');

  fIntfBuilder.AppendLine.AppendLine;

  fIntfBuilder.Append('type');

  AddEntityDeclaration(entityData);

  fIntfBuilder.AppendLine.Append(Indent).Append('private');
  for columnData in entityData.Columns do
    AddPrivateField(columnData);

  fIntfBuilder.AppendLine.Append(Indent).Append('public');
  for columnData in entityData.Columns do
    AddPublicProperty(columnData);

  fIntfBuilder.AppendLine.Append(Indent).Append('end').Append(';');

  fImplBuilder.AppendLine.AppendLine.Append('end').Append('.');

  Result := fIntfBuilder.ToString + fImplBuilder.ToString;
end;

function TDelphiUnitCodeGenerator.Generate(
  const entityData: TEntityModelData): string;
begin
  Result := DoGenerate(entityData);
end;

function TDelphiUnitCodeGenerator.GetColumnAttributeText(
  const columnData: TColumnData): string;
var
  builder: TStringBuilder;
begin
  builder := TStringBuilder.Create;
  try
    builder.Append('[Column');
    builder.Append('(');

    builder.Append(QuotedStr(columnData.ColumnName));
    builder.Append(',');
    builder.Append(GetColumnProperties(columnData));

    if columnData.ColumnLength.HasValue then
      builder.Append(',').Append(columnData.ColumnLength.Value);

    if columnData.ColumnPrecision.HasValue then
    begin
      builder.Append(',').Append(columnData.ColumnPrecision.Value);
      builder.Append(',').Append(columnData.ColumnScale.GetValueOrDefault(0));
    end;

    if columnData.ColumnDescription.HasValue then
      builder.Append(',').Append(columnData.ColumnDescription);

    builder.Append(')');
    builder.Append(']');

    Result := builder.ToString;
  finally
    builder.Free;
  end;
end;

function TDelphiUnitCodeGenerator.GetColumnProperties(
  const columnData: TColumnData): string;
begin
  Result := '';
  if columnData.IsRequired then
    Result := Result + 'cpRequired,';

  if columnData.IsUnique then
    Result := Result + 'cpUnique,';

  if columnData.IsPrimaryKey then
    Result := Result + 'cpPrimaryKey,';

  if columnData.DontInsert then
    Result := Result + 'cpDontInsert,';

  if columnData.DontUpdate then
    Result := Result + 'cpDontUpdate,';

  if columnData.NotNull then
    Result := Result + 'cpNotNull,';

  if columnData.IsHidden then
    Result := Result + 'cpHidden,';

  if Result <> '' then
    SetLength(Result, Length(Result) - 1);

  Result := '[' + Result + ']';
end;

function TDelphiUnitCodeGenerator.GetColumnTypeName(
  const columnData: TColumnData): string;
begin
  Result := columnData.ColumnTypeName;
  if fUseNullableTypes and not columnData.IsRequired then
    Result := 'Nullable<' + Result + '>';
end;

function TDelphiUnitCodeGenerator.GetPrivateFieldName(
  const columnData: TColumnData): string;
begin
  Result := FieldNamePrefix + columnData.ColumnName;
end;

function TDelphiUnitCodeGenerator.GetUnitName(
  const entityData: TEntityModelData): string;
begin
  Result := fUnitPrefix + entityData.TableName;
end;

{$ENDREGION}


end.
