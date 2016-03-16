unit TestEntityWrapper;

interface

uses
  TestFramework,
  TestEntities,
  Spring.Collections,
  Spring.Persistence.Core.EntityWrapper,
  Spring.Persistence.Core.Interfaces,
  Spring.TestUtils;

type
  TPair = class
  strict private
    fName: string;
    fValue: Variant;
  public
    constructor Create(const name: string; const value: Variant);

    property Name: string read fName;
    property Value: Variant read fValue;
  end;

  TResultSetListAdapter = class(TInterfacedObject, IDBResultset)
  private
    fValues: IList<TPair>;
    fCurrent: Integer;
  protected
    function IsEmpty: Boolean;
    function Next: Boolean;
    function FieldExists(const fieldName: string): Boolean;
    function GetFieldValue(index: Integer): Variant; overload;
    function GetFieldValue(const fieldName: string): Variant; overload;
    function GetFieldCount: Integer;
    function GetFieldName(index: Integer): string;
  public
    constructor Create(const values: IList<TPair>);
  end;

  TEntityWrapperTest = class(TTestCase)
  private
    fCustomer: TCustomer;
    fSut: IEntityWrapper;
  protected
    function GetCustomerResultSet(id: Variant; const name: string; age: Integer; middleName: string = ''): IDBResultSet;
    function GetEmptyResultSet: IDBResultSet;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure IsAssigned;
    procedure When_ResultSet_HasNoPrimaryKey_GetPrimaryKey_ThrowsException;
    procedure When_ResultSet_Has_PrimaryKey_GetPrimaryKey;
    procedure When_ResultSet_HasNoColumns_GetColumnValue_ThrowsException;
    procedure When_ResultSet_HasColumns_GetColumnValue_Returns_CorrectValue;
    procedure SetPrimaryKeyToEntity_Successfully;
    procedure SetPrimaryKeyToEntity_Exception;
  end;

implementation

uses
  SysUtils,
  Variants,
  TestConsts,
  Spring.Persistence.Core.Exceptions;


{$REGION 'TEntityWrapperTest'}

function TEntityWrapperTest.GetCustomerResultSet(id: Variant;
  const name: string; age: Integer; middleName: string): IDBResultSet;
var
  values: IList<TPair>;
begin
  values := TCollections.CreateObjectList<TPair>;
  if not VarIsNull(id) then
    values.Add(TPair.Create(CUSTID, id));
  values.Add(TPair.Create(CUSTNAME, name));
  values.Add(TPair.Create(CUSTAGE, age));
  values.Add(TPair.Create(CUST_MIDDLENAME, middleName));
  Result := TResultSetListAdapter.Create(values);
end;

function TEntityWrapperTest.GetEmptyResultSet: IDBResultSet;
begin
  Result := TResultSetListAdapter.Create(TCollections.CreateObjectList<TPair>);
end;

procedure TEntityWrapperTest.IsAssigned;
begin
  CheckTrue(Assigned(fSut));
end;

procedure TEntityWrapperTest.SetPrimaryKeyToEntity_Exception;
begin
  ExpectedException := EORMInvalidConversion;
  fSut.SetPrimaryKeyValue(1.01);
end;

procedure TEntityWrapperTest.SetPrimaryKeyToEntity_Successfully;
begin
  fSut.SetPrimaryKeyValue(1);
  CheckEquals(1, fCustomer.ID);
end;

procedure TEntityWrapperTest.SetUp;
begin
  inherited;
  fCustomer := TCustomer.Create;
  fSut := TEntityWrapper.Create(fCustomer);
end;

procedure TEntityWrapperTest.TearDown;
begin
  inherited;
  fCustomer.Free;
  fSut := nil;
end;

procedure TEntityWrapperTest.When_ResultSet_HasColumns_GetColumnValue_Returns_CorrectValue;
begin
  CheckEquals('Foo', GetCustomerResultSet(1, 'Foo', 10).GetFieldValue(CUSTNAME), 'Name should be Foo');
  CheckEquals(10, GetCustomerResultSet(1, 'Foo', 10).GetFieldValue(CUSTAGE), 'Age should be 10');
end;

procedure TEntityWrapperTest.When_ResultSet_HasNoColumns_GetColumnValue_ThrowsException;
begin
  ExpectedException := EORMColumnNotFound;
  GetEmptyResultSet.GetFieldValue(CUSTNAME);
end;

procedure TEntityWrapperTest.When_ResultSet_HasNoPrimaryKey_GetPrimaryKey_ThrowsException;
begin
  ExpectedException := EORMPrimaryKeyColumnNotFound;
  fSut.GetPrimaryKeyValue(GetCustomerResultSet(Null, 'Foo', 10));
end;

procedure TEntityWrapperTest.When_ResultSet_Has_PrimaryKey_GetPrimaryKey;
begin
  CheckEquals(1, fSut.GetPrimaryKeyValue(GetCustomerResultSet(1, 'Foo', 10)).AsInteger);
end;

{ TResultSetListAdapter }

constructor TResultSetListAdapter.Create(const values: IList<TPair>);
begin
  inherited Create;
  fValues := values;
  fCurrent := 0;
end;

function TResultSetListAdapter.FieldExists(const fieldName: string): Boolean;
var
  pair: TPair;
begin
  for pair in fValues do
    if SameText(fieldName, pair.Name) then
      Exit(True);
  Result := False;
end;

function TResultSetListAdapter.GetFieldCount: Integer;
begin
  Result := fValues.Count;
end;

function TResultSetListAdapter.GetFieldName(index: Integer): string;
begin
  Result := fValues[index].Name;
end;

function TResultSetListAdapter.GetFieldValue(index: Integer): Variant;
begin
  Result := fValues[index].Value;
end;

function TResultSetListAdapter.GetFieldValue(const fieldName: string): Variant;
var
  pair: TPair;
begin
  for pair in fValues do
    if SameText(fieldName, pair.Name) then
      Exit(pair.Value);
  raise EORMColumnNotFound.CreateFmt('Column %s not found in the resultSet', [fieldName]);
end;

function TResultSetListAdapter.IsEmpty: Boolean;
begin
  Result := fCurrent > 0;
  Inc(fCurrent);
end;

function TResultSetListAdapter.Next: Boolean;
begin
  Result := IsEmpty;
end;

{ TPair }

constructor TPair.Create(const name: string; const value: Variant);
begin
  fName := name;
  fValue := value;
end;

initialization
  RegisterTest('Spring.Persistence.Core', TEntityWrapperTest.Suite);

end.
