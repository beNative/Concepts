unit TestCommands;

interface

uses
  TestFramework,
  Spring.Persistence.SQL.Commands.CreateTable;

type
  TTableCreatorTest = class(TTestCase)
  private
    FCommand: TCreateTableExecutor;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TableExists;
  end;

implementation

uses
  Spring.Persistence.Core.Interfaces,
  Spring.Persistence.Core.ConnectionFactory,
  TestConsts,
  TestEntities,
  TestSession;

{ TTableCreatorTest }

procedure TTableCreatorTest.SetUp;
var
  connection: IDBConnection;
begin
  inherited;
  connection := TConnectionFactory.GetInstance(dtSQLite, TestDB);
  FCommand := TCreateTableExecutor.Create(connection);
end;

procedure TTableCreatorTest.TableExists;
begin
  FCommand.Build(TCustomer);
  CheckTrue( FCommand.TableExists(TBL_PEOPLE));
  CheckFalse(FCommand.TableExists('FOOBAR'));
end;

procedure TTableCreatorTest.TearDown;
begin
  FCommand.Free;
  inherited;
end;

initialization
  RegisterTest('Spring.Persistence.Commands', TTableCreatorTest.Suite);

end.
