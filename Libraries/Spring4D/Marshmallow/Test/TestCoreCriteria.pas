unit TestCoreCriteria;

interface

{$I Spring.inc}

uses
  TestFramework,
  TestEntities,
  Spring.TestUtils,
  Spring.Collections,
  Spring.Persistence.Core.Session,
  Spring.Persistence.Criteria,
  Spring.Persistence.Criteria.Criterion.Abstract,
  Spring.Persistence.Criteria.Interfaces,
  Spring.Persistence.Criteria.Restrictions;

type
  TCriteriaTest = class(TTestCase)
  private
    FCriteria: ICriteria<TCustomer>;
    FSession: TSession;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Add_Eq;
    procedure AddOrder;
    procedure List_Eq_IsNull;
    procedure List_Like;
    procedure List_Ge_Gt;
    procedure List_LEq_Lt;
    procedure List_In_NotIn;
    procedure List_Property_Eq;
    procedure Page_GEq_OrderDesc;
    procedure Page_SubEntities;
    procedure List_Or_And;
    procedure List_Or_Or;
    procedure List_And_And;
    procedure List_Not_Eq;
    procedure List_NeProperty;
    procedure List_Between;
    procedure Add_SubEntity_Criterion;
    procedure Disjunction;
    procedure Conjunction;
    procedure TestJunctions;
    procedure TestSelfReferencedAssociate;
{$IFNDEF DELPHIXE}
    // TODO: split into several tests - not everything here is compatible across all Delphi versions
    procedure OperatorOverloading_Eq;
{$ENDIF}
  end;

implementation

uses
  SysUtils,
  Variants,
  Spring.Persistence.Core.ConnectionFactory,
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Criteria.OrderBy,
  Spring.Persistence.Criteria.Properties,
  Spring.Persistence.SQL.Commands,
  Spring.Persistence.SQL.Interfaces,
  Spring.Persistence.SQL.Params,
  Spring.Persistence.SQL.Register,
  Spring.Persistence.SQL.Types,
  TestConsts,
  TestSession;


procedure TCriteriaTest.SetUp;
begin
  FSession := TSession.Create(TConnectionFactory.GetInstance(dtSQLite, TestDB));
  FCriteria := FSession.CreateCriteria<TCustomer>;

  FSession.Connection.AddExecutionListener(
    procedure(const command: string; const params: IEnumerable<TDBParam>)
    var
      i: Integer;
      param: TDBParam;
    begin
      Status(command);
      i := 0;
      for param in params do
      begin
        Status(Format('%2:d Param %0:s = %1:s', [param.Name, VarToStrDef(param.ToVariant, 'NULL'), i]));
        Inc(i);
      end;
      Status('-----');
    end);
end;

procedure TCriteriaTest.TearDown;
begin
  ClearTable(TBL_PEOPLE);
  ClearTable(TBL_ORDERS);
  FCriteria := nil;
  FSession.Free;
end;

procedure TCriteriaTest.TestJunctions;
var
  orJunction1: IJunction;
  orJunction2: IJunction;
  andJunction: IJunction;
  params: IList<TDBParam>;
  sqlWhere: string;
  param: TDBParam;
  command: TSelectCommand;
  generator: ISQLGenerator;
const
  expected =
    '(t0."FIRST_NAME" = :p0 OR t0."FIRST_NAME" = :p1) AND ' +
    '(t0."LAST_NAME" = :p2 OR t0."LAST_NAME" = :p3)';
begin
  command := TSelectCommand.Create(TPerson);
  try
    generator := TSQLGeneratorRegister.GetGenerator(qlOracle);

    orJunction1 := Restrictions.Disjunction;
    orJunction1.EntityClass := TCustomer;
    orJunction1.Add(Restrictions.Eq('FIRST_NAME', ''));
    orJunction1.Add(Restrictions.Eq('FIRST_NAME', ''));
    orJunction2 := Restrictions.Disjunction;
    orJunction2.EntityClass := TCustomer;
    orJunction2.Add(Restrictions.Eq('LAST_NAME', ''));
    orJunction2.Add(Restrictions.Eq('LAST_NAME', ''));

    andJunction := Restrictions.Conjunction;
    andJunction.EntityClass := TCustomer;
    andJunction.Add(orJunction1);
    andJunction.Add(orJunction2);

    params := TCollections.CreateObjectList<TDBParam>(True);
    param := TDBParam.Create('FIRST_NAME', '');
    params.Add(Param);
    param := TDBParam.Create('FIRST_NAME', '');
    params.Add(Param);
    param := TDBParam.Create('LAST_NAME', '');
    params.Add(Param);
    param := TDBParam.Create('LAST_NAME', '');
    params.Add(Param);

    sqlWhere := andJunction.ToSqlString(params, command, generator, False);
    CheckEqualsString(expected, sqlWhere);
  finally
    command.Free;
  end;
end;

procedure TCriteriaTest.TestSelfReferencedAssociate; // see issue #219
var
  resourceName: IProperty;
  criteria: ICriteria<TResource>;
  params: IList<TDBParam>;
  sqlWhere: string;
  command: TSelectCommand;
  generator: ISQLGenerator;
const
  expected = 't0."RESOURCE_NAME" = :p0';
begin
  criteria := FSession.CreateCriteria<TResource>;

  {No entity provided so this should generate where clause against base table}
  resourceName := TProperty.Create('RESOURCE_NAME');
  criteria.Add(resourceName.Eq('Test'));

  command := TSelectCommand.Create(TResource);
  try
    generator := TSQLGeneratorRegister.GetGenerator(qlOracle);
    params := TCollections.CreateObjectList<TDBParam>(True);

    sqlWhere := (criteria as TCriteria<TResource>).Criterions[0].ToSqlString(params, command, generator, False);

    CheckEqualsString(expected, sqlWhere);
  finally
    command.Free;
  end;
end;

procedure TCriteriaTest.Add_Eq;
begin
  FCriteria.Add(Restrictions.Eq('Name', 'Foo'))
    .Add(Restrictions.Eq('Age', 42));
  CheckEquals(2, (FCriteria as TCriteria<TCustomer>).Criterions.Count);
end;

procedure TCriteriaTest.Add_SubEntity_Criterion;
var
  LOrders: IList<TCustomer_Orders>;
  Age: IProperty;
  i, LCustId: Integer;
  LCriteria: ICriteria<TCustomer_Orders>;
begin
  LCriteria := FSession.CreateCriteria<TCustomer_Orders>;

  Age := TProperty<TCustomer>.Create(CUSTAGE);

  for i := 1 to 10 do
  begin
    LCustId := InsertCustomer(i, 'Foo', Abs(i/2));
    InsertCustomerOrder(LCustId, i + 10, -1, i + 100);
  end;

  LOrders := LCriteria.Add(Age.Eq(1)).ToList;
  CheckEquals(1, LOrders.Count);
  CheckEquals(1, LOrders.First.Customer.Age);

  LCriteria.Clear;

  LOrders := LCriteria.Add(Age.GEq(1)).ToList;
  CheckEquals(10, LOrders.Count);
  CheckEquals(1, LOrders.First.Customer.Age);

  LCriteria.Clear;
  LOrders := LCriteria.Add(Age.&In(TArray<Integer>.Create(1,2,3)))
    .OrderBy(Age.Desc)
    .ToList;
  CheckEquals(3, LOrders.Count);
  CheckEquals(3, LOrders.First.Customer.Age);
end;

procedure TCriteriaTest.Conjunction;
var
  LCustomers: IList<TCustomer>;
  Age, Name: IProperty;
begin
  Age := TProperty.Create(CUSTAGE);
  Name := TProperty.Create(CUSTNAME);
  InsertCustomer(42, 'Foo');
  InsertCustomer(50, 'Bar');

  LCustomers := FCriteria.Add(
    Restrictions
      .Conjunction
        .Add(Age.Eq(42))
        .Add(Name.Eq('Foo')))
    .OrderBy(Age.Desc)
    .ToList;
  CheckEquals(1, LCustomers.Count);
  CheckEquals(42, LCustomers[0].Age);
  CheckEquals('Foo', LCustomers[0].Name);
end;

procedure TCriteriaTest.Disjunction;
var
  LCustomers: IList<TCustomer>;
  Age, Name: IProperty;
begin
  Age := TProperty.Create(CUSTAGE);
  Name := TProperty.Create(CUSTNAME);
  InsertCustomer(42, 'Foo');
  InsertCustomer(50, 'Bar');

  LCustomers := FCriteria.Add(
    Restrictions
      .Disjunction
        .Add(Age.Eq(42))
        .Add(Name.Eq('Foo'))
        .Add(Age.Eq(50)))
    .OrderBy(Age.Desc)
    .ToList;
  CheckEquals(2, LCustomers.Count);
  CheckEquals(50, LCustomers[0].Age);
  CheckEquals(42, LCustomers[1].Age);
end;

procedure TCriteriaTest.AddOrder;
var
  LCustomers: IList<TCustomer>;
begin
  InsertCustomer(42, 'foo');
  InsertCustomer(110, 'foo');
  FCriteria.Add(Restrictions.Eq(CUSTNAME, 'foo'))
    .OrderBy(TOrderBy.Desc(CUSTAGE));
  LCustomers := FCriteria.ToList;
  CheckEquals(110, LCustomers[0].Age);
  CheckEquals(42, LCustomers[1].Age);
end;

procedure TCriteriaTest.List_And_And;
var
  LCustomers: IList<TCustomer>;
  Age, Name: IProperty;
begin
  Age := TProperty.Create(CUSTAGE);
  Name := TProperty.Create(CUSTNAME);
  InsertCustomer(42, 'Foo');
  InsertCustomer(50, 'Bar');

  LCustomers := FCriteria.Add(Restrictions.And(Age.Eq(42), Name.Eq('Foo')))
    .Add(Age.GEq(10))
    .OrderBy(Age.Desc)
    .ToList;
  CheckEquals(1, LCustomers.Count);
  CheckEquals(42, LCustomers[0].Age);
end;

procedure TCriteriaTest.List_Between;
var
  LCustomers: IList<TCustomer>;
  Age: IProperty;
begin
  Age := TProperty.Create(CUSTAGE);
  InsertCustomer(42, 'Foo');
  InsertCustomer(50, 'Bar');

  LCustomers := FCriteria.Add(Restrictions.Between(CUSTAGE, 42, 50)).ToList;

  CheckEquals(2, LCustomers.Count);
  CheckEquals(42, LCustomers[0].Age);
  CheckEquals(50, LCustomers[1].Age);

  FCriteria.Clear;
  LCustomers := FCriteria.Add(Age.Between(43, 50))
    .ToList;
  CheckEquals(1, LCustomers.Count);
  CheckEquals(50, LCustomers[0].Age);
end;

procedure TCriteriaTest.List_NeProperty;
var
  LCustomers: IList<TCustomer>;
  Age, ID: IProperty;
begin
  InsertCustomer(42, 'Foo');
  InsertCustomer(50, 'Bar');

  Age := TProperty<TCustomer>.Create(CUSTAGE);
  ID := TProperty<TCustomer>.Create(CUSTID);

  LCustomers := FCriteria.Add(Restrictions.NeProperty(CUSTAGE, CUSTID))
    .ToList;
  CheckEquals(2, LCustomers.Count);
  CheckEquals(42, LCustomers[0].Age);
  CheckEquals(50, LCustomers[1].Age);

  FCriteria.Clear;
  LCustomers := FCriteria.Add(Age.NeProperty(ID))
    .ToList;
  CheckEquals(2, LCustomers.Count);
  CheckEquals(42, LCustomers[0].Age);
  CheckEquals(50, LCustomers[1].Age);

  FCriteria.Clear;
  Age := TProperty.Create(CUSTAGE);
  ID := TProperty.Create(CUSTID);
  LCustomers := FCriteria.Add(Age.NeProperty(ID)).Add(Age.NeProperty(CUSTNAME))
    .ToList;
  CheckEquals(2, LCustomers.Count);
  CheckEquals(42, LCustomers[0].Age);
  CheckEquals(50, LCustomers[1].Age);
end;

procedure TCriteriaTest.List_Eq_IsNull;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := FCriteria.Add(Restrictions.Eq(CUSTNAME, 'Foo'))
    .Add(Restrictions.Eq(CUSTAGE, 42)).Add(Restrictions.IsNull(CUSTAVATAR)).ToList;
  CheckTrue(Assigned(LCustomers));
  CheckEquals(0, LCustomers.Count);
  InsertCustomer(42, 'Foo');
  LCustomers := FCriteria.ToList;
  CheckEquals(1, LCustomers.Count);
  CheckEquals(42, LCustomers[0].Age);
  CheckEquals('Foo', LCustomers[0].Name);
  CheckEquals(0, LCustomers[0].Orders.Count);
  InsertCustomerOrder(LCustomers[0].ID, 1, 100, 100.59);
  LCustomers := FCriteria.ToList;
  CheckEquals(1, LCustomers.Count);
  CheckEquals(1, LCustomers[0].Orders.Count);
  CheckEquals(100, LCustomers[0].Orders[0].Order_Status_Code);
end;

procedure TCriteriaTest.List_Ge_Gt;
var
  LCustomers: IList<TCustomer>;
begin
  InsertCustomer(42, 'Foo');
  InsertCustomer(50, 'Bar');

  LCustomers := FCriteria.Add(Restrictions.GEq(CUSTAGE, 42))
    .ToList;
  CheckEquals(2, LCustomers.Count);
  CheckEquals(42, LCustomers[0].Age);
  CheckEquals(50, LCustomers[1].Age);

  FCriteria.Clear;
  LCustomers := FCriteria.Add(Restrictions.Gt(CUSTAGE, 42))
    .ToList;
  CheckEquals(1, LCustomers.Count);
  CheckEquals(50, LCustomers[0].Age);
end;

procedure TCriteriaTest.List_In_NotIn;
var
  LCustomers: IList<TCustomer>;
  LAges: TArray<Integer>;
begin
  InsertCustomer(42, 'Foo');
  InsertCustomer(50, 'Bar');
  InsertCustomer(68, 'FooBar');
  InsertCustomer(10, 'Fbar');

  LAges := TArray<Integer>.Create(10, 50);
  LCustomers := FCriteria.Add(Restrictions.In<Integer>(CUSTAGE, LAges))
    .ToList;
  CheckEquals(2, LCustomers.Count);
  CheckEquals(50, LCustomers[0].Age);
  CheckEquals(10, LCustomers[1].Age);

  FCriteria.Clear;
  LCustomers := FCriteria.Add(Restrictions.NotIn<Integer>(CUSTAGE, LAges))
    .ToList;
  CheckEquals(2, LCustomers.Count);
  CheckEquals(42, LCustomers[0].Age);
  CheckEquals(68, LCustomers[1].Age);

  FCriteria.Clear;
  LCustomers := FCriteria.Add(Restrictions.In<string>(CUSTNAME, TArray<string>.Create('Bar', 'Fbar')))
    .ToList;
  CheckEquals(2, LCustomers.Count);
  CheckEquals('Bar', LCustomers[0].Name);
  CheckEquals('Fbar', LCustomers[1].Name);

  FCriteria.Clear;
  LCustomers := FCriteria.Add(Restrictions.NotIn<string>(CUSTNAME, TArray<string>.Create('Bar', 'Fbar')))
    .ToList;
  CheckEquals(2, LCustomers.Count);
  CheckEquals('Foo', LCustomers[0].Name);
  CheckEquals('FooBar', LCustomers[1].Name);
end;

procedure TCriteriaTest.List_LEq_Lt;
var
  LCustomers: IList<TCustomer>;
begin
  InsertCustomer(42, 'Foo');
  InsertCustomer(50, 'Bar');

  LCustomers := FCriteria.Add(Restrictions.LEq(CUSTAGE, 50))
    .ToList;
  CheckEquals(2, LCustomers.Count);
  CheckEquals(42, LCustomers[0].Age);
  CheckEquals(50, LCustomers[1].Age);

  FCriteria.Clear;
  LCustomers := FCriteria.Add(Restrictions.Lt(CUSTAGE, 50))
    .ToList;
  CheckEquals(1, LCustomers.Count);
  CheckEquals(42, LCustomers[0].Age);
end;

procedure TCriteriaTest.List_Like;
var
  LCustomers: IList<TCustomer>;
begin
  InsertCustomer(42, 'FooBar');
  LCustomers := FCriteria.Add(Restrictions.Like(CUSTNAME, 'Foo')).ToList;
  CheckEquals(0, LCustomers.Count);
  FCriteria.Clear;
  FCriteria.Add(Restrictions.Like(CUSTNAME, 'Foo', mmAnywhere));
  LCustomers := FCriteria.ToList;
  CheckEquals(1, LCustomers.Count);

  FCriteria.Clear;
  FCriteria.Add(Restrictions.Like(CUSTNAME, 'Foo', mmStart));
  LCustomers := FCriteria.ToList;
  CheckEquals(1, LCustomers.Count);

  FCriteria.Clear;
  FCriteria.Add(Restrictions.Like(CUSTNAME, 'Bar', mmEnd));
  LCustomers := FCriteria.ToList;
  CheckEquals(1, LCustomers.Count);
end;

procedure TCriteriaTest.List_Not_Eq;
var
  LCustomers: IList<TCustomer>;
  Age: IProperty;
begin
  Age := TProperty.Create(CUSTAGE);
  InsertCustomer(42, 'Foo');
  InsertCustomer(50, 'Bar');

  LCustomers := FCriteria.Add(Restrictions.Not(Age.Eq(42)))
    .Add(Age.GEq(10))
    .OrderBy(Age.Desc)
    .ToList;
  CheckEquals(1, LCustomers.Count);
  CheckEquals(50, LCustomers[0].Age);
end;

procedure TCriteriaTest.List_Or_And;
var
  LCustomers: IList<TCustomer>;
  Age: IProperty;
begin
  Age := TProperty.Create(CUSTAGE);
  InsertCustomer(42, 'Foo');
  InsertCustomer(50, 'Bar');

  LCustomers := FCriteria.Add(Restrictions.Or(Age.Eq(42), age.Eq(50)))
    .Add(Age.GEq(10))
    .OrderBy(Age.Desc)
    .ToList;
  CheckEquals(2, LCustomers.Count);
  CheckEquals(50, LCustomers[0].Age);
  CheckEquals(42, LCustomers[1].Age);
end;

procedure TCriteriaTest.List_Or_Or;
var
  LCustomers: IList<TCustomer>;
  Age: IProperty;
begin
  Age := TProperty.Create(CUSTAGE);
  InsertCustomer(42, 'Foo');
  InsertCustomer(50, 'Bar');
  //WHERE ((A.CUSTAGE =:CUSTAGE1 OR A.CUSTAGE = :CUSTAGE2) OR A.CUSTAGE >=:CUSTAGE3)
  //ORDER BY A.CUSTAGE DESC
  LCustomers := FCriteria.Add(Restrictions.Or(Restrictions.Or(Age.Eq(42), Age.Eq(50)), Age.GEq(10)))
    .OrderBy(Age.Desc)
    .ToList;
  CheckEquals(2, LCustomers.Count);
  CheckEquals(50, LCustomers[0].Age);
  CheckEquals(42, LCustomers[1].Age);
end;

procedure TCriteriaTest.List_Property_Eq;
var
  LCustomers: IList<TCustomer>;
  Age: IProperty;
begin
  Age := TProperty.Create(CUSTAGE);
  InsertCustomer(42, 'Foo');
  InsertCustomer(50, 'Bar');

  LCustomers := FCriteria.Add(Age.Eq(42))
    .OrderBy(Age.Desc)
    .ToList;
  CheckEquals(1, LCustomers.Count);
  CheckEquals(42, LCustomers[0].Age);
end;

{$IFNDEF DELPHIXE}
procedure TCriteriaTest.OperatorOverloading_Eq;
var
  LCustomers: IList<TCustomer>;
  Age, Name: Prop;
begin
  Age := Prop.Create(CUSTAGE);
  Name := Prop.Create(CUSTNAME);
  InsertCustomer(42, 'Foo');
  InsertCustomer(50, 'Bar');
  LCustomers := FCriteria.Where((Age = 42) and (Age <> 50) and (Age = Age){$IFDEF DELPHIXE2_UP} and (Age in [42]){$ENDIF})
    .OrderBy(Age.Desc)
    .ToList;
  CheckEquals(1, LCustomers.Count);
  CheckEquals(42, LCustomers[0].Age);

{$IFDEF DELPHIXE2_UP}
  FCriteria.Clear;
  LCustomers := FCriteria.Where((Age in [42..50]) or (Name in ['Foo', 'Bar']) and (Age.IsNotNull))
    .OrderBy(Age.Desc)
    .ToList;
  CheckEquals(2, LCustomers.Count);
{$ENDIF}
end;
{$ENDIF}

procedure TCriteriaTest.Page_GEq_OrderDesc;
var
  LPage: IDBPage<TCustomer>;
  Age: IProperty;
  i: Integer;
begin
  Age := TProperty.Create(CUSTAGE);
  //add 10 customers
  for i := 1 to 10 do
  begin
    InsertCustomer(i, 'Foo', Abs(i/2));
  end;

  LPage := FCriteria.Add(Age.GEq(5))
    .OrderBy(Age.Desc).Page(0, 3);

  CheckEquals(6, LPage.ItemCount);
  CheckEquals(2, LPage.PageCount);
  CheckEquals(10, LPage.Items[0].Age);
  CheckEquals(8, LPage.Items[2].Age);
end;

procedure TCriteriaTest.Page_SubEntities;
var
  LPage: IDBPage<TCustomer>;
  Age: IProperty;
  i: Integer;
  LCustId: Integer;
begin
  Age := TProperty.Create(CUSTAGE);
  //add 10 customers
  for i := 1 to 10 do
  begin
    LCustId := InsertCustomer(i, 'Foo', Abs(i/2));
    InsertCustomerOrder(LCustId, i + 10, -1, i + 100);
  end;

  CheckEquals(10, FSession.FindAll<TCustomer_Orders>.Count);

  LPage := FCriteria.Add(Age.GEq(5))
    .OrderBy(Age.Desc).Page(0, 3);

  CheckEquals(6, LPage.ItemCount);
  CheckEquals(2, LPage.PageCount);
  CheckEquals(10, LPage.Items[0].Age);
  CheckEquals(8, LPage.Items[2].Age);

  CheckEquals(10, LPage.Items[0].Age);
  CheckEquals(20, LPage.Items[0].OrdersIntf.First.Customer_Payment_Method_Id);
end;

initialization
  RegisterTest('Spring.Persistence.Criteria', TCriteriaTest.Suite);

end.
