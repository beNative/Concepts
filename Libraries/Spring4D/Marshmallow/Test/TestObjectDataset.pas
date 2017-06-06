unit TestObjectDataSet;

interface

uses
  Classes,
  TestFramework,
  TestEntities,
  Spring.Collections,
  Spring.Data.ObjectDataSet,
  Spring.TestUtils;

type
  TObjectDataSetTest = class(TTestCase)
  private
    FDataset: TObjectDataSet;
    FRemovedItemAge: Integer;
  protected
    function CreateCustomersList(ASize: Integer = 10; ACreateMock: Boolean = False): IList<TCustomer>; virtual;
    function CreateCustomersOrdersList(ASize: Integer = 10): IList<TCustomer_Orders>; virtual;
    function CreateCustomersStreamList(const stream: TMemoryStream): IObjectList;
    procedure DoListChanged(Sender: TObject; const Item: TCustomer; Action: TCollectionChangedAction); virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AddRecord;
    procedure Append;
    procedure Append_Filtered;
    procedure Bookmark_Filtered;
    procedure Bookmark_Filtered_2;
    procedure Bookmark_Filtered_Fail;
    procedure Bookmark_Simple;
    procedure Bookmark_Sorted;
    procedure ClearField_Nullable;
    procedure ClearField_SimpleType;
    procedure Delete;
    procedure Delete_Filtered;
    procedure Delete_Sorted;
    procedure Delete_Last;
    procedure Delete_Notification;
    procedure Edit;
    procedure Edit_Nullable;
    procedure Edit_SpringNullable;
    procedure Eof_AfterLast;
    procedure Eof_AfterNext;
    procedure Filter;
    procedure Filter_Custom_Functions;
    procedure Filter_DateTime;
    procedure Filter_Null;
    {$IFDEF PERFORMANCE_TESTS}
    procedure Filter_Performance_Test;
    procedure InsertionSort_Speed;
    {$ENDIF}
    procedure Filter_Without_Brackets;
    procedure GetCurrentModel_Filtered;
    procedure GetCurrentModel_Simple;
    procedure GetCurrentModel_Sorted;
    procedure GetCurrentModel_RandomAccess;
    procedure Insert_Simple;
    procedure Iterating;
    procedure Iterating_Empty;
    procedure Locate;
    procedure MergeSort_Try;
    procedure Open;
    procedure Open_Orders;
    procedure QuickSortTest;
    procedure SimpleSort;
    procedure Sort;
    procedure Sort_Regression;
    procedure StreamRead;

    procedure SimpleDefinedFields;
    procedure LookUpField;
    procedure WhenIsTrackingChanges_DeletedItemFromList_IsSynced;
    procedure WhenIsNotTrackingChanges_DeletedItemFromList_IsNotSynced;
    {$IFDEF GUI_TESTS}
    procedure TestGUI;
    {$ENDIF}
  end;

implementation

uses
  DateUtils,
  DB,
  Diagnostics,
  Generics.Defaults,
  SysUtils,
{$IFDEF GUI_TESTS}
  ViewTestObjectDataSet,
{$ENDIF}
  Spring,
  Spring.Persistence.Mapping.Attributes;

type
  [Entity]
  TMockCustomer = class(TCustomer)
  private
    FMockId: Integer;
  public
    constructor Create(mockId: Integer);
    [Column]
    property MockID: Integer read FMockId write FMockId;
  end;

  TSpringNullableTest = class
  private
    FName: Spring.Nullable<string>;
    FAge: Spring.Nullable<Integer>;
  public
    [Column]
    property Name: Spring.Nullable<string> read FName write FName;
    [Column]
    property Age: Spring.Nullable<Integer> read FAge write FAge;
  end;

{ TObjectDataSetTest }

procedure TObjectDataSetTest.AddRecord;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;

  FDataset.AppendRecord(['Insert', 59]);
  CheckEquals(11, FDataset.RecordCount);
  FDataset.Last;
  CheckEquals('Insert', FDataset.Fields[0].AsString);
  CheckEquals(59, FDataset.Fields[1].AsInteger);
end;

procedure TObjectDataSetTest.Append_Filtered;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;
  FDataset.Filter := 'Age = 1';
  FDataset.Filtered := True;

  CheckEquals(1, FDataset.RecordCount, 'RecordCount');
  FDataset.Append;
  FDataset.FieldByName('AGE').AsInteger := 1;
  FDataset.FieldByName('Name').AsString := 'Foo';
  FDataset.Post;
  CheckEquals(2, FDataset.RecordCount, 'RecordCount');

  FDataset.Append;
  FDataset.FieldByName('AGE').AsInteger := 2;
  FDataset.FieldByName('Name').AsString := 'Bar';
  FDataset.Post;
  CheckEquals(2, FDataset.RecordCount, 'RecordCount');
end;

procedure TObjectDataSetTest.Bookmark_Filtered;
var
  LCustomers: IList<TCustomer>;
  LBookmark: TBookmark;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;
  FDataset.Filter := '(AGE <= 3)';
  FDataset.Filtered := True;
  CheckEquals(3, FDataset.RecordCount, 'RecordCount');
  FDataset.Last;
  CheckEquals(3, FDataset.FieldByName('AGE').AsInteger);
  LBookmark := FDataset.Bookmark;

  FDataset.Filter := '(AGE >= 3)';
  FDataset.Last;
  CheckEquals(10, FDataset.FieldByName('AGE').AsInteger);
  CheckTrue(FDataset.BookmarkValid(LBookmark));
  FDataset.Bookmark := LBookmark;
  CheckEquals(3, FDataset.FieldByName('AGE').AsInteger);
end;

procedure TObjectDataSetTest.Bookmark_Filtered_2;
var
  LCustomers: IList<TCustomer>;
  LBookmark: TBookmark;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;
  FDataset.Filter := '(AGE <= 3)';
  FDataset.Filtered := True;
  CheckEquals(3, FDataset.RecordCount, 'RecordCount');
  FDataset.Last;
  CheckEquals(3, FDataset.FieldByName('AGE').AsInteger);
  LBookmark := FDataset.Bookmark;

  FDataset.Filter := '';
  CheckEquals(10, FDataset.RecordCount, 'RecordCount');
  FDataset.Bookmark := LBookmark;
  CheckEquals(3, FDataset.FieldByName('AGE').AsInteger);

  FDataset.Filter := '(AGE > 3)';
  CheckEquals(7, FDataset.RecordCount, 'RecordCount');
  FDataset.Filter := '';
  CheckEquals(10, FDataset.RecordCount, 'RecordCount');
  FDataset.First;
  FDataset.Bookmark := LBookmark;
  CheckEquals(3, FDataset.FieldByName('AGE').AsInteger);
end;

procedure TObjectDataSetTest.Bookmark_Filtered_Fail;
var
  LCustomers: IList<TCustomer>;
  LBookmark: TBookmark;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;
  LBookmark := FDataset.Bookmark;

  FDataset.Filter := '(AGE = 3)';
  FDataset.Filtered := True;
  CheckEquals(1, FDataset.RecordCount, 'RecordCount');
  CheckEquals(3, FDataset.FieldByName('AGE').AsInteger);
  CheckFalse(FDataset.BookmarkValid(LBookmark));
end;

procedure TObjectDataSetTest.Bookmark_Simple;
var
  LCustomers: IList<TCustomer>;
  LBookmark: TBookmark;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;

  FDataset.Last;
  FDataset.Prior;
  CheckEquals(9, FDataset.RecNo, 'RecNo');
  CheckEquals(9, FDataset.FieldByName('AGE').AsInteger);
  LBookmark := FDataset.Bookmark;
  FDataset.First;

  CheckTrue(FDataset.BookmarkValid(LBookmark));
  FDataset.Bookmark := LBookmark;
  CheckEquals(9, FDataset.RecNo, 'RecNo');
  CheckEquals(9, FDataset.FieldByName('AGE').AsInteger);
end;

procedure TObjectDataSetTest.Bookmark_Sorted;
var
  LCustomers: IList<TCustomer>;
  LBookmark: TBookmark;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;
  FDataset.Last;
  FDataset.Prior;
  CheckEquals(9, FDataset.RecNo, 'RecNo');
  CheckEquals(9, FDataset.FieldByName('AGE').AsInteger);
  LBookmark := FDataset.Bookmark;

  FDataset.Sort := 'Age Desc';
  FDataset.First;
  CheckEquals(10, FDataset.FieldByName('Age').AsInteger);

  CheckTrue(FDataset.BookmarkValid(LBookmark));
  FDataset.Bookmark := LBookmark;
  CheckEquals(9, FDataset.FieldByName('AGE').AsInteger);
end;

procedure TObjectDataSetTest.ClearField_Nullable;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;
  LCustomers.First.MiddleName := 'Foo';
  CheckTrue(LCustomers.First.MiddleName.HasValue);

  FDataset.Edit;
  FDataset.FieldByName('MiddleName').Clear;
  FDataset.Post;
  CheckFalse(LCustomers.First.MiddleName.HasValue);
end;

procedure TObjectDataSetTest.ClearField_SimpleType;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;
  LCustomers.First.Age := 1;
  //clear simple type
  CheckEquals(1, LCustomers.First.Age);
  FDataset.Edit;
  FDataset.FieldByName('Age').Clear;
  FDataset.Post;
  CheckEquals(0, LCustomers.First.Age);
  //clear nullable type
end;

function TObjectDataSetTest.CreateCustomersList(ASize: Integer; ACreateMock: Boolean): IList<TCustomer>;
var
  LCustomer: TCustomer;
  i: Integer;
begin
  Result := TCollections.CreateObjectList<TCustomer>(True);
  for i := 1 to ASize do
  begin
    // Mock of the customer class is needed to write down ID fields. Valid values
    // are needed for some tests (e.g. Lookup).
    if ACreateMock then
      LCustomer := TMockCustomer.Create(i)
    else
      LCustomer := TCustomer.Create;
    LCustomer.Name := 'FirstName';
    LCustomer.Age := i;
    LCustomer.EMail := 'aaa@aaa.com';
    LCustomer.Height := 100.5;
    Result.Add(LCustomer);
  end;
end;

function TObjectDataSetTest.CreateCustomersOrdersList(ASize: Integer): IList<TCustomer_Orders>;
var
  LOrder: TCustomer_Orders;
  i: Integer;
begin
  Result := TCollections.CreateObjectList<TCustomer_Orders>(True);
  for i := 1 to ASize do
  begin
    LOrder := TCustomer_Orders.Create;
    LOrder.Order_Status_Code := 150;
    LOrder.Date_Order_Placed := Today;
    LOrder.Customer_ID := i;

    Result.Add(LOrder);
  end;
end;

function TObjectDataSetTest.CreateCustomersStreamList(
  const stream: TMemoryStream): IObjectList;
var
  customer: TCustomerWithStream;
  customers: IList<TCustomerWithStream>;
begin
  customers := TCollections.CreateObjectList<TCustomerWithStream>(True);
  customer := TCustomerWithStream.Create;
  customers.Add(customer);
  customer.Name := 'FirstName';
  customer.Age := 42;
  customer.EMail := 'aaa@aaa.com';
  customer.Height := 100.5;
  customer.StreamLazy := TMemoryStream(stream);
  Result := customers as IObjectList;
end;

procedure TObjectDataSetTest.Delete;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;
  CheckEquals(10, FDataset.RecordCount);
  LCustomers.Last.Name := 'Foo';
  FDataset.Last;
  FDataset.Prior;
  FDataset.Delete;
  CheckEquals(9, LCustomers.Count);
  CheckEquals(9, FDataset.RecordCount);
  FDataset.Last;
 // CheckEquals('Foo', LCustomers.Last.Name);
  CheckEquals('Foo', FDataset.FieldByName('Name').AsString);

  FDataset.Delete;
  CheckEquals(8, LCustomers.Count);
  CheckEquals(8, FDataset.RecordCount);
end;


procedure TObjectDataSetTest.Delete_Filtered;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Filtered := True;
  FDataset.Filter := 'Age < 3';
  FDataset.Open;

  CheckEquals(2, FDataset.RecordCount, 'RecordCount');
  FDataset.Delete;
  CheckEquals(1, FDataset.RecordCount, 'RecordCount');
  FDataset.Delete;
  CheckEquals(0, FDataset.RecordCount, 'RecordCount');
  CheckEquals(8, LCustomers.Count, 'Count');
end;

procedure TObjectDataSetTest.Delete_Last;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;
  FDataset.Last;

  FDataset.Delete;
  CheckEquals(9, FDataset.RecordCount, 'RecordCount');
  CheckEquals(9, FDataset.RecNo, 'RecNo');
  CheckEquals(9, FDataset.FieldByName('Age').AsInteger);
  CheckEquals('FirstName', FDataset.FieldByName('Name').AsString);

  FDataset.Delete;
  CheckEquals(8, FDataset.RecordCount, 'RecordCount');
  CheckEquals(8, FDataset.RecNo, 'RecNo');
  CheckEquals(8, FDataset.FieldByName('Age').AsInteger);
end;

procedure TObjectDataSetTest.Delete_Notification;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(10);
  LCustomers.OnChanged.Add(DoListChanged);
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;
  FRemovedItemAge := -1;
  FDataset.Last;
  FDataset.Delete;
  LCustomers.OnChanged.Clear;
  CheckEquals(10, FRemovedItemAge);
end;

procedure TObjectDataSetTest.Delete_Sorted;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;
  FDataset.Sort := 'AGE DESC';
  FDataset.RecNo := 5;
  FDataset.Insert;
  FDataset.FieldByName('age').AsInteger := 0;
  FDataset.Post;
  FDataset.Last;
  CheckEquals(0, FDataset.FieldByName('age').AsInteger);
  FDataset.Delete;
  CheckEquals(10, FDataset.RecordCount, 'RecordCount');
end;

procedure TObjectDataSetTest.DoListChanged(Sender: TObject; const Item: TCustomer;
  Action: TCollectionChangedAction);
begin
  case Action of
    caAdded: ;
    caRemoved: FRemovedItemAge := item.Age;
    caReplaced: ;
    caMoved: ;
    caReseted: ;
  end;
end;

procedure TObjectDataSetTest.Edit;
var
  LCustomers: IList<TCustomer>;
  LDate: TDateTime;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;
  CheckEquals(10, FDataset.RecordCount, 'RecordCount');
  FDataset.First;
  FDataset.Edit;
  FDataset.FieldByName('Age').AsInteger := 999;
  FDataset.FieldByName('MiddleName').AsString := 'Middle';
  LDate := Today;
  FDataset.FieldByName('LastEdited').AsDateTime := LDate;
  FDataset.Post;
  CheckEquals(1, FDataset.RecNo, 'RecNo');
  CheckEquals(999, FDataset.FieldByName('Age').AsInteger);
  CheckEquals(999, LCustomers[0].Age);
  CheckEquals('Middle', LCustomers[0].MiddleName);
  CheckEquals(LDate, LCustomers[0].LastEdited);
end;

procedure TObjectDataSetTest.Edit_Nullable;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;

  FDataset.RecNo := 4;
  FDataset.Edit;
  FDataset.FieldByName('MiddleName').AsString := 'Foo';
  FDataset.Post;
  CheckEquals('Foo', FDataset.FieldByName('MiddleName').AsString);
end;

procedure TObjectDataSetTest.Edit_SpringNullable;
var
  LCustomers: IList<TSpringNullableTest>;
begin
  LCustomers := TCollections.CreateObjectList<TSpringNullableTest>(True);
  LCustomers.Add(TSpringNullableTest.Create);
  LCustomers.Add(TSpringNullableTest.Create);
  LCustomers.Add(TSpringNullableTest.Create);

  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;

  FDataset.RecNo := 2;
  FDataset.Edit;
  FDataset.FieldByName('Name').AsString := 'Foo';
  FDataset.FieldByName('Age').AsInteger := 10;
  FDataset.Post;
  CheckEquals('Foo', FDataset.FieldByName('Name').AsString);
  CheckEquals(10, FDataset.FieldByName('Age').AsInteger);
end;

procedure TObjectDataSetTest.Eof_AfterLast;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;
  CheckFalse(FDataset.Eof);
  CheckTrue(FDataset.Bof);
  FDataset.Last;
  CheckTrue(FDataset.Eof);
  CheckFalse(FDataset.Bof);

end;

procedure TObjectDataSetTest.Eof_AfterNext;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;
  FDataset.Last;
  FDataset.Prior;
  CheckFalse(FDataset.Eof);
  FDataset.Next;
  CheckFalse(FDataset.Eof);
  FDataset.Next;
  CheckTrue(FDataset.Eof);
end;

procedure TObjectDataSetTest.Append;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;
  CheckEquals(10, FDataset.RecordCount);

  FDataset.Append;
  FDataset.FieldByName('Name').AsString := 'Foo';
  FDataset.FieldByName('Age').AsInteger := 999;
  FDataset.FieldByName('MiddleName').AsString := 'Middle';
  FDataset.Post;

  CheckEquals(11, LCustomers.Count);
  CheckEquals(11, FDataset.RecordCount);
  CheckEquals(11, FDataset.RecNo);
  CheckEquals(999, FDataset.FieldByName('Age').AsInteger);
  CheckEquals('Middle', FDataset.FieldByName('MiddleName').AsString);
  CheckEquals('Foo', FDataset.FieldByName('Name').AsString);

  CheckEquals(999, LCustomers.Last.Age);
  CheckEquals('Middle', LCustomers.Last.MiddleName);
  CheckEquals('Foo', LCustomers.Last.Name);

  FDataset.Append;
  FDataset.FieldByName('Name').AsString := 'Bar';
  FDataset.FieldByName('Age').AsInteger := 111;
  FDataset.FieldByName('MiddleName').AsString := 'Marley';
  FDataset.Post;
  CheckEquals(12, LCustomers.Count);
  CheckEquals(12, FDataset.RecordCount);
  CheckEquals(12, FDataset.RecNo);
  CheckEquals(111, FDataset.FieldByName('Age').AsInteger);
  CheckEquals('Marley', FDataset.FieldByName('MiddleName').AsString);
  CheckEquals('Bar', FDataset.FieldByName('Name').AsString);
end;

{$IFDEF PERFORMANCE_TESTS}
procedure TObjectDataSetTest.InsertionSort_Speed;
var
  LCustomers: IList<TCustomer>;
  swMerge, swInsertion: TStopwatch;
begin
  LCustomers := CreateCustomersList(10000);
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;
  //now merge sort will be used
  swMerge := TStopwatch.StartNew;
  FDataset.Sort := 'Age Desc';
  swMerge.Stop;
  FDataset.RecNo := 99995;
  swInsertion := TStopwatch.StartNew;
  FDataset.Edit;
  FDataset.FieldByName('Age').AsInteger := 99996;
  swInsertion := TStopwatch.StartNew;
  FDataset.Post;
  swInsertion.Stop;
  Status(Format('Merge Sort in %d ms. Insertion sort in %d ms.',
    [swMerge.ElapsedMilliseconds, swInsertion.ElapsedMilliseconds]));
  CheckTrue(swMerge.ElapsedMilliseconds > swInsertion.ElapsedMilliseconds);
end;
{$ENDIF}

procedure TObjectDataSetTest.Insert_Simple;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(5);
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;
  FDataset.Insert;
  FDataset.FieldByName('Age').AsInteger := 6;
  FDataset.FieldByName('Name').AsString := 'Foo';
  FDataset.Post;
  CheckEquals(6, FDataset.RecordCount);
  CheckEquals(1, FDataset.RecNo);
  CheckEquals(6, FDataset.FieldByName('Age').AsInteger);
  CheckEquals('Foo', FDataset.FieldByName('Name').AsString);
end;

procedure TObjectDataSetTest.Iterating;
var
  LCustomers: IList<TCustomer>;
  i: Integer;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;

  i := 0;

  while not FDataset.Eof do
  begin
    Inc(i);
    CheckEquals(i, FDataset.FieldByName('Age').AsInteger);
    FDataset.Next;
  end;

  CheckEquals(10, i);
end;

procedure TObjectDataSetTest.Iterating_Empty;
var
  LCustomers: IList<TCustomer>;
  i: Integer;
begin
  LCustomers := CreateCustomersList(0);
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;
  i := 0;
  while not FDataset.Eof do
  begin
    FDataset.Next;
    Inc(i);
  end;
  CheckEquals(0, i);
end;

procedure TObjectDataSetTest.Locate;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;

  CheckTrue( FDataset.Locate('Age', 5, []) );
  CheckEquals(5, FDataset.RecNo);
  CheckEquals(5, FDataset.FieldByName('Age').AsInteger);
  CheckEquals(5, LCustomers[FDataset.Index].Age);

  CheckFalse( FDataset.Locate('Age', 50, []) );
end;

procedure TObjectDataSetTest.LookUpField;
var
  LCustomers: IList<TCustomer>;
  LIntField: TIntegerField;
  LStrField: TStringField;
  LOrders: IList<TCustomer_Orders>;
  FOrdersDataSet: TObjectDataSet;
begin
  LCustomers := CreateCustomersList(10, True);
  LOrders := CreateCustomersOrdersList(10);

  FOrdersDataSet := TObjectDataSet.Create(nil);
  try
    FOrdersDataSet.ColumnAttributeClass := ColumnAttribute;
    FOrdersDataSet.DataList := LOrders as IObjectList;

    LIntField := TIntegerField.Create(FDataSet);
    LIntField.FieldName := 'MockID';
    LIntField.DataSet := FDataset;

    LStrField := TWideStringField.Create(FDataSet);
    LStrField.FieldName := 'Name';
    LStrField.FieldKind := fkLookup;
    LStrField.LookupDataSet := FOrdersDataSet;
    LStrField.LookupKeyFields := 'Customer_ID';
    LStrField.LookupResultField := 'Date_Order_Placed';
    LStrField.KeyFields := 'MockID';
    LStrField.Lookup := True;
    LStrField.DataSet := FDataset;

    FDataset.DataList := LCustomers as IObjectList;
    FDataset.Open;
    CheckTrue(True);
  finally
    FOrdersDataSet.Free;
  end;
end;

procedure TObjectDataSetTest.SimpleDefinedFields;
var
  LCustomers: IList<TCustomer>;
  LStrField: TStringField;
begin
  LCustomers := CreateCustomersList(10);
  LStrField := TWideStringField.Create(FDataSet);
  LStrField.FieldName := 'Name';
  LStrField.DataSet := FDataset;

  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;
  CheckEquals(LCustomers[0].Name, LStrField.AsString);
end;

procedure TObjectDataSetTest.MergeSort_Try;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;

 // FDataset.MergeSort(0, LCustomers.Count - 1, FDataset.CompareRecords, );

  Pass;
end;

procedure TObjectDataSetTest.Open;
var
  LCustomers: IList<TCustomer>;
  LNewCustomer: TCustomer;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.TrackChanges := True;
  FDataset.Open;
  CheckEquals(10, FDataset.RecordCount, 'RecordCount');
  CheckFalse(FDataset.IsEmpty);
  CheckTrue(FDataset.Active);
  CheckEquals(1, FDataset.FieldByName('Age').AsInteger);
  CheckEquals('FirstName', FDataset.FieldByName('Name').AsString);
  CheckEquals(100.5, FDataset.FieldByName('Height').AsFloat, 0.001);
  CheckEquals('aaa@aaa.com', FDataset.FieldByName('EMail').AsString);
  CheckTrue(FDataset.FieldByName('MiddleName').IsNull);

  FDataset.Next;
  CheckEquals(2, FDataset.FieldByName('Age').AsInteger);

  LNewCustomer := TCustomer.Create;
  LNewCustomer.Name := 'New';
  LNewCustomer.MiddleName := 'Customer';
  LNewCustomer.Age := 58;
//  FDataset.IndexList.AddModel(LNewCustomer);
  LCustomers.Add(LNewCustomer);

  CheckEquals(11, FDataset.RecordCount, 'RecordCount');
  FDataset.Last;
  CheckEquals(58, FDataset.FieldByName('Age').AsInteger);
  CheckEquals('New', FDataset.FieldByName('Name').AsString);
  CheckEquals('Customer', FDataset.FieldByName('MiddleName').AsString);
end;

procedure TObjectDataSetTest.Open_Orders;
var
  LOrders: IList<TCustomer_Orders>;
begin
  LOrders := CreateCustomersOrdersList(10);
  FDataset.DataList := LOrders as IObjectList;
  FDataset.Open;
  CheckEquals(10, FDataset.RecordCount, 'RecordCount');
  CheckFalse(FDataset.IsEmpty);
  CheckTrue(FDataset.Active);

  CheckEquals(1, LOrders.First.Customer_ID);
  CheckEquals(Today, LOrders.First.Date_Order_Placed);
  CheckEquals(150, LOrders.First.Order_Status_Code);
  CheckFalse(LOrders.First.Total_Order_Price.HasValue);
end;

procedure TObjectDataSetTest.QuickSortTest;
var
  LArray: TArray<Integer>;
begin
  LArray := TArray<Integer>.Create(2, 2, 10, 9, 8, 7, 6, 5, 4, 3);
  TArray.Sort<Integer>(LArray, TComparer<Integer>.Construct(
    function(const Left, Right: Integer): Integer
    begin
      Result := Right - Left;
    end));
  Pass;
end;

procedure TObjectDataSetTest.SetUp;
begin
  inherited;
  FDataset := TObjectDataSet.Create(nil);
  FDataset.ColumnAttributeClass := ColumnAttribute;
end;

procedure TObjectDataSetTest.SimpleSort;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(3);
  LCustomers.First.Name := 'Bob';
  LCustomers.First.MiddleName := 'Middle';

  LCustomers.Last.Name := 'Michael';
  LCustomers.Last.MiddleName := 'Jordan';

  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;

  FDataset.Filtered := False;     //2,0,1

  FDataset.Sort := 'Age Desc, MIDDLENAME, Name';
  CheckEquals(3, FDataset.RecordCount, 'RecordCount');
  CheckEquals(1, FDataset.RecNo, 'RecNo');
  CheckEquals('Michael', FDataset.FieldByName('Name').AsString);
  CheckEquals('Jordan', FDataset.FieldByName('MiddleName').AsString);

  FDataset.Next;
  CheckEquals(2, FDataset.RecNo, 'RecNo');
  CheckEquals('FirstName', FDataset.FieldByName('Name').AsString);
  CheckTrue(FDataset.FieldByName('MiddleName').IsNull);

  FDataset.Sort := 'Age Asc, MIDDLENAME, Name';
  CheckEquals(2, FDataset.RecNo, 'RecNo');
  CheckEquals('FirstName', FDataset.FieldByName('Name').AsString);
  CheckTrue(FDataset.FieldByName('MiddleName').IsNull);
  FDataset.First;
  CheckEquals('Bob', FDataset.FieldByName('Name').AsString);
  CheckEquals('Middle', FDataset.FieldByName('MiddleName').AsString);
end;

procedure TObjectDataSetTest.Sort;
var
  LCustomers: IList<TCustomer>;
  i: Integer;
  LMsg: string;
begin
  LCustomers := CreateCustomersList(10);

  LCustomers.First.Age := 2;
  LCustomers.First.Name := 'Bob';
  LCustomers.First.MiddleName := 'Middle';

  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;

  FDataset.Filtered := True;
  FDataset.Filter := 'Age > 2';

  CheckEquals(1, FDataset.RecNo, 'RecNo');
  FDataset.Last;
  CheckEquals(8, FDataset.RecNo, 'RecNo');

  FDataset.Sort := 'Age Desc, Name, MIDDLENAME';
  FDataset.First;
  CheckEquals(1, FDataset.RecNo, 'RecNo');
  CheckEquals(10, FDataset.FieldByName('Age').AsInteger);
  CheckEquals('FirstName', LCustomers.Last.Name);
  CheckEquals(8, FDataset.RecordCount, 'RecordCount');


  LMsg := '';
  Status(Format('Filter: %s. Sort: %s', [FDataset.Filter, FDataset.Sort]));
  for i := 0 to LCustomers.Count - 1 do
    Status(LMsg + Format('%d %s %d', [i, LCustomers[i].Name, LCustomers[i].Age]));

  FDataset.Filtered := False;

  LMsg := '';
  Status(Format('Filtered false. Sort: %s', [FDataset.Sort]));
  for i := 0 to LCustomers.Count - 1 do
    Status(LMsg + Format('%d %s %d', [i, LCustomers[i].Name, LCustomers[i].Age]));

  CheckEquals(10, FDataset.RecordCount, 'RecordCount');
  FDataset.Sort := 'Age Desc, MiddleName, Name';

  LMsg := '';
  Status(Format('Filter: %s. Sort: %s', [FDataset.Filter, FDataset.Sort]));
  for i := 0 to LCustomers.Count - 1 do
    Status(LMsg + Format('%d %s %d %s', [i, LCustomers[i].Name, LCustomers[i].Age, LCustomers[i].MiddleName.GetValueOrDefault]));

  FDataset.Last;
  CheckEquals(10, FDataset.RecNo, 'RecNo');
  FDataset.Prior;
  CheckEquals(9, FDataset.RecNo, 'RecNo');
  CheckEquals('Bob', FDataset.FieldByName('Name').AsString);
  CheckEquals('Middle', FDataset.FieldByName('MiddleName').AsString);

  //CheckEquals('Bob', LCustomers[8].Name);
  //CheckEquals('Middle', LCustomers[8].MiddleName);

  FDataset.Append;
  FDataset.FieldByName('name').AsString := 'aaa';
  FDataset.FieldByName('Age').AsInteger := 15;
  FDataset.Post;

  FDataset.Sort := 'name asc';

  LMsg := '';
  Status(Format('Filter: %s. Sort: %s', [FDataset.Filter, FDataset.Sort]));
  for i := 0 to LCustomers.Count - 1 do
    Status(LMsg + Format('%d %s %d %s', [i, LCustomers[i].Name, LCustomers[i].Age, LCustomers[i].MiddleName.GetValueOrDefault]));

  CheckEquals(11, FDataset.RecNo, 'RecNo');
  FDataset.First;
  CheckEquals('aaa', FDataset.FieldByName('Name').AsString);
  FDataset.Next;
  CheckEquals('Bob', FDataset.FieldByName('Name').AsString);
end;

procedure TObjectDataSetTest.Sort_Regression;
var
  LCustomers: IList<TCustomer>;
  LCust: TCustomer;
begin
  LCustomers := TCollections.CreateObjectList<TCustomer>(True);
  {
    0 Bob 2
    1 FirstName 2
    2 FirstName 10
    3 FirstName 9
    4 FirstName 8
    5 FirstName 7
    6 FirstName 6
    7 FirstName 5
    8 FirstName 4
    9 FirstName 3
  }
  //0
  LCust := TCustomer.Create;
  LCust.Name := 'Bob';
  LCust.Age := 2;
  LCustomers.Add(LCust);
  //1
  LCust := TCustomer.Create;
  LCust.Name := 'FirstName';
  LCust.Age := 2;
  LCustomers.Add(LCust);
  //2
  LCust := TCustomer.Create;
  LCust.Name := 'FirstName';
  LCust.Age := 10;
  LCustomers.Add(LCust);
  //3
  LCust := TCustomer.Create;
  LCust.Name := 'FirstName';
  LCust.Age := 9;
  LCustomers.Add(LCust);
  //4
  LCust := TCustomer.Create;
  LCust.Name := 'FirstName';
  LCust.Age := 8;
  LCustomers.Add(LCust);
  //5
  LCust := TCustomer.Create;
  LCust.Name := 'FirstName';
  LCust.Age := 7;
  LCustomers.Add(LCust);
  //6
  LCust := TCustomer.Create;
  LCust.Name := 'FirstName';
  LCust.Age := 6;
  LCustomers.Add(LCust);
  //7
  LCust := TCustomer.Create;
  LCust.Name := 'FirstName';
  LCust.Age := 5;
  LCustomers.Add(LCust);
  //8
  LCust := TCustomer.Create;
  LCust.Name := 'FirstName';
  LCust.Age := 4;
  LCustomers.Add(LCust);
  //9
  LCust := TCustomer.Create;
  LCust.Name := 'FirstName';
  LCust.Age := 3;
  LCustomers.Add(LCust);

  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;

  FDataset.Sort := 'Age Desc';

  CheckEquals(3, LCustomers.Last.Age);
  CheckEquals(4, LCustomers[8].Age);

  FDataset.Last;
  CheckEquals(10, FDataset.RecNo, 'RecNo');
  CheckEquals(2, FDataset.FieldByName('Age').AsInteger);
  FDataset.Prior;
  CheckEquals(9, FDataset.RecNo, 'RecNo');
  CheckEquals(2, FDataset.FieldByName('Age').AsInteger);

 // CheckEquals('Bob', FDataset.FieldByName('Name').AsString);
end;

procedure TObjectDataSetTest.StreamRead;
var
  actual, bytes: TBytes;
  field: TField;
  stream: Managed<TBytesStream>;
begin
  bytes := TBytes.Create(1, 2, 3);
  stream := TBytesStream.Create(bytes);
  FDataset.DataList := CreateCustomersStreamList(stream);
  FDataset.Open;

  field := FDataset.FieldByName('CUSTSTREAM');

  CheckIs(field, TBlobField);
  CheckEquals(Length(bytes), TBlobField(field).BlobSize);
  actual := TBlobField(field).AsBytes;
  CheckEquals(Length(bytes), Length(actual));
  CheckEquals(bytes[0], actual[0]);
  CheckEquals(bytes[1], actual[1]);
  CheckEquals(bytes[2], actual[2]);
end;

procedure TObjectDataSetTest.TearDown;
begin
  inherited;
  FDataset.Free;
end;

procedure TObjectDataSetTest.Filter;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.Filtered := True;
  FDataset.FilterOptions := [foCaseInsensitive];
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;

  FDataset.Filter := '(Age = 2)';
  CheckEquals(2, FDataset.FieldByName('Age').AsInteger);
  CheckEquals(1, FDataset.RecordCount, 'RecordCount');

  FDataset.Filter := '(Age > 2)';
  CheckEquals(8, FDataset.RecordCount, 'RecordCount');
end;

procedure TObjectDataSetTest.Filter_Custom_Functions;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(10);
  LCustomers.First.MiddleName := 'Foo';
  LCustomers.Last.MiddleName := 'Bar';
  FDataset.Filtered := True;
  FDataset.FilterOptions := [foCaseInsensitive];
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;
  FDataset.Filter := '(IsNull(MiddleName, ''1'') = ''1'')';
  CheckEquals(8, FDataset.RecordCount, 'RecordCount');
  FDataset.Filter := '(LastEdited = Today())';

  FDataset.Filter := '(Name = substr(Name,1,50))';
  CheckEquals(10, FDataset.RecordCount, 'RecordCount');
  FDataset.Filter := '(Length(Name) = 9)';
  CheckEquals(10, FDataset.RecordCount, 'RecordCount');
  FDataset.Filter := '(Length(Name) = 10)';
  CheckEquals(0, FDataset.RecordCount, 'RecordCount');
end;

procedure TObjectDataSetTest.Filter_DateTime;
var
  LCustomers: IList<TCustomer>;
  LDate: TDate;
begin
  LCustomers := CreateCustomersList(10);
  LDate := EncodeDate(2013,1,1);
  LCustomers.Last.LastEdited := LDate;

  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;
  FDataset.Filter := Format('(LastEdited = %s)', [QuotedStr(DateToStr(LDate))]);
  FDataset.Filtered := True;
  CheckEquals(1, FDataset.RecordCount, 'RecordCount');
end;

procedure TObjectDataSetTest.Filter_Null;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(5);
  LCustomers.Last.MiddleName := 'Foo';
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;
  FDataset.Filter := '(MiddleName <> Null)';
  FDataset.Filtered := True;
  CheckEquals(1, FDataset.RecordCount, 'RecordCount');
  FDataset.Filter := '(MiddleName = Null)';
  CheckEquals(4, FDataset.RecordCount, 'RecordCount');
end;

{$IFDEF PERFORMANCE_TESTS}
procedure TObjectDataSetTest.Filter_Performance_Test;
var
  LCustomers: IList<TCustomer>;
  sw: TStopwatch;
begin
  LCustomers := CreateCustomersList(50000);
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;
  FDataset.Filter := '((AGE = 2)) OR ((AGE = 3)) OR ((AGE = 4)) OR ((AGE = 1)) OR ((AGE = 100)) OR ((AGE = 1000)) OR ((AGE = 999)) OR '+
  ' ((NAME = ''Some Long Name Name sdsdsd sdsd aaaaaaaaaaaaaaaaaaaaaaaaaa     WWEEW    sdddddddddddddddddd sd sd  sds d sd sds d sdds sd wewewew vew ewe we we we we we we''))';
  sw := TStopwatch.StartNew;
  FDataset.Filtered := True;
  sw.Stop;
  CheckEquals(7, FDataset.RecordCount, 'RecordCount');
  Status(Format('%d records in %d ms', [LCustomers.Count, sw.ElapsedMilliseconds]));
end;
{$ENDIF}

procedure TObjectDataSetTest.Filter_Without_Brackets;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(10);
  LCustomers.First.Age := 11;
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;
  FDataset.Filter := '(AGE = 1) OR (AGE = 2)';
  FDataset.Filtered := True;
 // CheckEquals(1, FDataset.FieldByName('AGE').AsInteger);
  CheckEquals(1, FDataset.RecordCount, 'RecordCount');
end;

procedure TObjectDataSetTest.GetCurrentModel_Filtered;
var
  LCustomers: IList<TCustomer>;
  LCurrentCustomer: TCustomer;
  i: Integer;
begin
  LCustomers := CreateCustomersList(5);
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;
  FDataset.Filter := '(Age < 2)';
  FDataset.Filtered := True;
  i := 1;
  while not FDataset.Eof do
  begin
    LCurrentCustomer := FDataset.GetCurrentModel<TCustomer>;
    CheckEquals(i, LCurrentCustomer.Age);

    FDataset.Next;
    Inc(i);
  end;
end;

procedure TObjectDataSetTest.GetCurrentModel_RandomAccess;
var
  LCustomers: IList<TCustomer>;
  LCurrentCustomer: TCustomer;
  LBookmark: TBookmark;
begin
  LCustomers := CreateCustomersList(10);
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;
  FDataset.Last;

  LCurrentCustomer := FDataset.GetCurrentModel<TCustomer>;
  CheckEquals(10, LCurrentCustomer.Age);

  FDataset.RecNo := 5;
  CheckEquals(5, FDataset.GetCurrentModel<TCustomer>.Age);

  FDataset.Prior;
  CheckEquals(4, FDataset.GetCurrentModel<TCustomer>.Age);
  LBookmark := FDataset.Bookmark;
  FDataset.First;
  FDataset.Bookmark := LBookmark;
  CheckEquals(4, FDataset.GetCurrentModel<TCustomer>.Age);
end;

procedure TObjectDataSetTest.GetCurrentModel_Simple;
var
  LCustomers: IList<TCustomer>;
  LCurrentCustomer: TCustomer;
  i: Integer;
begin
  LCustomers := CreateCustomersList(5);
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;
  FDataset.First;
  i := 1;
  while not FDataset.Eof do
  begin
    LCurrentCustomer := FDataset.GetCurrentModel<TCustomer>;
    CheckEquals(i, LCurrentCustomer.Age);

    FDataset.Next;
    Inc(i);
  end;
end;

procedure TObjectDataSetTest.GetCurrentModel_Sorted;
var
  LCustomers: IList<TCustomer>;
  LCurrentCustomer: TCustomer;
  i: Integer;
begin
  LCustomers := CreateCustomersList(5);
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;
  FDataset.Sort := 'AGE DESC';
  i := 5;
  while not FDataset.Eof do
  begin
    LCurrentCustomer := FDataset.GetCurrentModel<TCustomer>;
    CheckEquals(i, LCurrentCustomer.Age);

    FDataset.Next;
    Dec(i);
  end;
end;

procedure TObjectDataSetTest.WhenIsNotTrackingChanges_DeletedItemFromList_IsNotSynced;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(5);
  FDataset.TrackChanges := False;
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;

  CheckEquals(5, FDataset.RecordCount, 'RecordCount');
  LCustomers.Remove(LCustomers.Last);
  CheckEquals(5, FDataset.RecordCount, 'RecordCount');
end;

procedure TObjectDataSetTest.WhenIsTrackingChanges_DeletedItemFromList_IsSynced;
var
  LCustomers: IList<TCustomer>;
begin
  LCustomers := CreateCustomersList(5);
  FDataset.TrackChanges := True;
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;

  CheckEquals(5, FDataset.RecordCount, 'RecordCount');
  LCustomers.Remove(LCustomers.Last);
  CheckEquals(4, FDataset.RecordCount, 'RecordCount');
end;

{$IFDEF GUI_TESTS}
procedure TObjectDataSetTest.TestGUI;
var
  LCustomers: IList<TCustomer>;
  LView: TfrmObjectDataSetTest;
  sw: TStopwatch;
  LClonedDataset: TObjectDataSet;
begin
  LCustomers := CreateCustomersList(1000);
  LCustomers.First.Age := 2;
  LCustomers.First.Name := 'Bob';
  FDataset.DataList := LCustomers as IObjectList;
  FDataset.Open;
  LView := TfrmObjectDataSetTest.Create(nil);
  LClonedDataset := TObjectDataSet.Create(nil);
  try
    LView.Dataset := FDataset;
    LView.dsList.DataSet := FDataset;

    sw := TStopwatch.StartNew;
  //  FDataset.Sort := 'Age Desc, NAME';
    FDataset.Filtered := True;
    sw.Stop;
    Status(Format('Elapsed time: %d ms', [sw.ElapsedMilliseconds]));

    LClonedDataset.Clone(FDataset);
    LView.dsClone.DataSet := LClonedDataset;

    LView.ShowModal;
  finally
    LView.Free;
    LClonedDataset.Free;
  end;
  FCheckCalled := True;
end;
{$ENDIF}

{ TMockCustomer }

constructor TMockCustomer.Create(mockId: Integer);
begin
  FMockId := mockId;
end;

initialization
  RegisterTest('Spring.Data.ObjectDataSet', TObjectDataSetTest.Suite);

end.
