unit SQLite3Register;

interface

procedure Register;

implementation

uses
  Classes, SQLiteTable3, SQLite3Dataset;

procedure Register;
begin
  RegisterComponents('SQLite3',
    [TSQLiteDatabase, TSQLiteDataset, TSQLiteUpdateSQL]);
end;

end.
