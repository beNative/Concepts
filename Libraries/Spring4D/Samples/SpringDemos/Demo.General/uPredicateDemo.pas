unit uPredicateDemo;

interface

procedure CheckIsLessThan10;
procedure FilterList;
procedure FlipCoins;

implementation

uses
  Spring,
  Spring.Collections;

procedure CheckIsLessThan10;
var
  isLessThan10: Predicate<integer>;
  i: integer;
begin
  isLessThan10 :=
    function(const aValue: integer): Boolean
    begin
      Result := aValue < 10;
    end;

  Write('Enter an integer: ');
  Readln(i);

  if isLessThan10(i) then
    Writeln(i, ' is LESS THAN 10')
  else
    Writeln(i, ' is GREATER THAN OR EQUAL to 10');
end;

procedure FilterList;
var
  temp: IEnumerable<integer>;
  i: integer;
  list: IList<integer>;
begin
  list := TCollections.CreateList<Integer>;
  list.Add(3);
  list.Add(6);
  list.Add(8);
  list.Add(34);
  list.Add(65);
  list.Add(86);

  temp := list.TakeWhile(function(const aInt: integer): Boolean begin Result := aInt < 50; end);
  for i in temp do
    WriteLn(i, ' is less than 50');
end;

procedure FlipCoins;
var
  flipResults: IList<integer>;
  i: Integer;
  testTrue: Predicate<integer>;
  theTrueOnes: IEnumerable<integer>;
const
  TotalFlips = 10000;
begin
  Randomize;

  flipResults := TCollections.CreateList<Integer>;
  for i := 1 to TotalFlips do
    flipResults.Add(Random(2));
  testTrue :=
    function(const aValue: integer): Boolean
    begin
      Result := aValue = 1;
    end;
  theTrueOnes := flipResults.Where(testTrue);
  Writeln('Total Heads: ', theTrueOnes.Count, ' out of ', TotalFlips);
end;

end.
