unit frmEnumerableDemo;

interface

uses
  Windows, Messages, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Generics.Collections, Spring.Collections;

type
   TIntegerStringPair = TPair<integer, string>;

type
  TEnumerationDemoForm = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
  private
    { Private declarations }
    List: IList<TIntegerStringPair>;
    function CreateAnotherList: IList<TIntegerStringPair>;
    procedure Clear;
    procedure AddToMemo(const aString: string);
  public
    { Public declarations }
  end;

var
  EnumerationDemoForm: TEnumerationDemoForm;

implementation

{$R *.dfm}

uses
  SysUtils,
  Spring;

procedure TEnumerationDemoForm.Button1Click(Sender: TObject);
var
  pair: TIntegerStringPair;
begin
  Clear;
  for pair in List do
    AddToMemo(pair.Value);
end;

procedure TEnumerationDemoForm.Button2Click(Sender: TObject);
var
  predicate: Predicate<TIntegerStringPair>;
  pair: TIntegerStringPair;
  enumerable: IEnumerable<TIntegerStringPair>;
begin
  Clear;
  predicate :=
    function(const Pair: TIntegerStringPair): Boolean
    begin
      Result :=  Pair.Key mod 2 = 0;
    end;
  enumerable := List.Where(predicate);

  for pair in enumerable do
    AddToMemo(pair.Value);
end;

procedure TEnumerationDemoForm.Button3Click(Sender: TObject);
var
  pair: TIntegerStringPair;
  enumerable: IEnumerable<TIntegerStringPair>;
begin
  Clear;
  // Skip the first seven
  enumerable := List.Skip(7);

  for pair in enumerable do
    AddToMemo(pair.Value);
end;

procedure TEnumerationDemoForm.Button4Click(Sender: TObject);
var
  pair: TIntegerStringPair;
  predicate: Predicate<TIntegerStringPair>;
  enumerable: IEnumerable<TIntegerStringPair>;
begin
  Clear;
  predicate :=
    function(const Pair: TIntegerStringPair): Boolean
    begin
      Result :=  Pair.Key < 5;
    end;

  enumerable := List.SkipWhile(predicate);
  for pair in enumerable do
    AddToMemo(pair.Value);
end;

procedure TEnumerationDemoForm.Button5Click(Sender: TObject);
var
  pair: TIntegerStringPair;
  enumerable: IEnumerable<TIntegerStringPair>;
begin
  Clear;
  // Only "take" the first seven
  enumerable := List.Take(7);

  for pair in enumerable do
    AddToMemo(pair.Value);
end;

procedure TEnumerationDemoForm.Button6Click(Sender: TObject);
var
  pair: TIntegerStringPair;
  predicate: Predicate<TIntegerStringPair>;
  enumerable: IEnumerable<TIntegerStringPair>;
begin
  Clear;
  predicate :=
    function(const Pair: TIntegerStringPair): Boolean
    begin
      Result := Pair.Key < 5;
    end;

  // "Takes" the items from the enumeration as long as the Pair.Key is less than
  // five.  Once it isn't less than five, it stops and nothing else is returned.
  enumerable := List.TakeWhile(predicate);
  for pair in enumerable do
    AddToMemo(pair.Value);
end;

procedure TEnumerationDemoForm.Button7Click(Sender: TObject);
var
  pair: TIntegerStringPair;
  enumerable: IEnumerable<TIntegerStringPair>;
  tempList: IList<TIntegerStringPair>;
begin
  Clear;
  tempList := CreateAnotherList;

  enumerable := List.Concat(tempList);

  for pair in enumerable do
    AddToMemo(pair.Value);
end;

procedure TEnumerationDemoForm.Button8Click(Sender: TObject);
var
  pair: TIntegerStringPair;
begin
  Clear;
  pair := List.First;
  AddToMemo(pair.Value);
end;

procedure TEnumerationDemoForm.Button9Click(Sender: TObject);
var
  pair: TIntegerStringPair;
begin
  Clear;
  pair := List.Last;
  AddToMemo(pair.Value);
end;

procedure TEnumerationDemoForm.AddToMemo(const aString: string);
begin
  Memo1.Lines.Add(aString);
end;

procedure TEnumerationDemoForm.Button10Click(Sender: TObject);
var
  pair: TIntegerStringPair;
begin
  Clear;
  pair := List.ElementAt(4); // zero-based
  AddToMemo(pair.Value);
end;

procedure TEnumerationDemoForm.Button11Click(Sender: TObject);
var
  pair: TIntegerStringPair;
begin
  Clear;
  pair := List.Min;
  AddToMemo(pair.Value);
end;

procedure TEnumerationDemoForm.Button12Click(Sender: TObject);
var
  pair: TIntegerStringPair;
begin
  Clear;
  pair := List.Max;
  AddToMemo(pair.Value);
end;

procedure TEnumerationDemoForm.Button13Click(Sender: TObject);
var
  pair: TIntegerStringPair;
begin
  Clear;
  for pair in List.Reversed do
    AddToMemo(pair.Value);
end;

procedure TEnumerationDemoForm.Button14Click(Sender: TObject);
var
  action: Action<TIntegerStringPair>;
begin
  Clear;
  action :=
    procedure(const Pair: TIntegerStringPair)
    begin
      AddToMemo(Format('The numeric form of %s is %d', [Pair.Value, Pair.Key]))
    end;

  List.ForEach(action);
end;


procedure TEnumerationDemoForm.Button15Click(Sender: TObject);
var
  pair: TIntegerStringPair;
begin
  Clear;
  // Note that the order is changed
  for pair in TCollections.CreateSet<TIntegerStringPair>(List) do
    AddToMemo(pair.Value);
end;

procedure TEnumerationDemoForm.Clear;
begin
  Memo1.Clear;
end;

function TEnumerationDemoForm.CreateAnotherList: IList<TPair<integer, string>>;
begin
  Result := TCollections.CreateList<TIntegerStringPair>;
  Result.Add(TIntegerStringPair.Create(11, 'eleven'));
  Result.Add(TIntegerStringPair.Create(12, 'twelve'));
  Result.Add(TIntegerStringPair.Create(13, 'thirteen'));
  Result.Add(TIntegerStringPair.Create(14, 'fourteen'));
  Result.Add(TIntegerStringPair.Create(15, 'fifteen'));
end;

procedure TEnumerationDemoForm.FormCreate(Sender: TObject);
begin
  Clear;
  List := TCollections.CreateList<TIntegerStringPair>;
  List.Add(TIntegerStringPair.Create(1, 'one'));
  List.Add(TIntegerStringPair.Create(2, 'two'));
  List.Add(TIntegerStringPair.Create(3, 'three'));
  List.Add(TIntegerStringPair.Create(4, 'four'));
  List.Add(TIntegerStringPair.Create(5, 'five'));
  List.Add(TIntegerStringPair.Create(6, 'six'));
  List.Add(TIntegerStringPair.Create(7, 'seven'));
  List.Add(TIntegerStringPair.Create(8, 'eight'));
  List.Add(TIntegerStringPair.Create(9, 'nine'));
  List.Add(TIntegerStringPair.Create(10, 'ten'));
end;

end.
