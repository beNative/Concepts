program ORMTests;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

{$I Spring.inc}

uses
  SysUtils,
  Spring.TestRunner in '..\..\Tests\Source\Spring.TestRunner.pas',
  {$IFDEF DELPHIXE5_UP}
  FireDAC.VCLUI.Wait,
  {$ENDIF}
  TestCodeGenerator in 'TestCodeGenerator.pas',
  Spring.Persistence.Adapters.ADO in '..\..\Source\Persistence\Adapters\Spring.Persistence.Adapters.ADO.pas',
  Spring.Persistence.Adapters.ASA in '..\..\Source\Persistence\Adapters\Spring.Persistence.Adapters.ASA.pas',
  Spring.Persistence.Adapters.DBX in '..\..\Source\Persistence\Adapters\Spring.Persistence.Adapters.DBX.pas',
  Spring.Persistence.Adapters.FieldCache in '..\..\Source\Persistence\Adapters\Spring.Persistence.Adapters.FieldCache.pas',
  {$IFDEF DELPHIXE5_UP}
  Spring.Persistence.Adapters.FireDAC in '..\..\Source\Persistence\Adapters\Spring.Persistence.Adapters.FireDAC.pas',
  {$ENDIF}
  Spring.Persistence.Adapters.MongoDB in '..\..\Source\Persistence\Adapters\Spring.Persistence.Adapters.MongoDB.pas',
  Spring.Persistence.Adapters.MSSQL in '..\..\Source\Persistence\Adapters\Spring.Persistence.Adapters.MSSQL.pas',
  Spring.Persistence.Adapters.Oracle in '..\..\Source\Persistence\Adapters\Spring.Persistence.Adapters.Oracle.pas',
  Spring.Persistence.Adapters.SQLite in '..\..\Source\Persistence\Adapters\Spring.Persistence.Adapters.SQLite.pas',
  {$IFDEF ENABLE_UIB}
  Spring.Persistence.Adapters.UIB in '..\..\Source\Persistence\Adapters\Spring.Persistence.Adapters.UIB.pas',
  {$ENDIF}
  {$IFDEF ENABLE_UNIDAC}
  Spring.Persistence.Adapters.UniDAC in '..\..\Source\Persistence\Adapters\Spring.Persistence.Adapters.UniDAC.pas',
  {$ENDIF}
  {$IFDEF ENABLE_ZEOS}
  Spring.Persistence.Adapters.Zeos in '..\..\Source\Persistence\Adapters\Spring.Persistence.Adapters.Zeos.pas',
  {$ENDIF}
  Spring.Persistence.Core.AbstractSession in '..\..\Source\Persistence\Core\Spring.Persistence.Core.AbstractSession.pas',
  Spring.Persistence.Core.Base in '..\..\Source\Persistence\Core\Spring.Persistence.Core.Base.pas',
  Spring.Persistence.Core.ConnectionFactory in '..\..\Source\Persistence\Core\Spring.Persistence.Core.ConnectionFactory.pas',
  Spring.Persistence.Core.DatabaseManager in '..\..\Source\Persistence\Core\Spring.Persistence.Core.DatabaseManager.pas',
  Spring.Persistence.Core.DetachedSession in '..\..\Source\Persistence\Core\Spring.Persistence.Core.DetachedSession.pas',
  Spring.Persistence.Core.EmbeddedEntity in '..\..\Source\Persistence\Core\Spring.Persistence.Core.EmbeddedEntity.pas',
  Spring.Persistence.Core.EntityCache in '..\..\Source\Persistence\Core\Spring.Persistence.Core.EntityCache.pas',
  Spring.Persistence.Core.EntityMap in '..\..\Source\Persistence\Core\Spring.Persistence.Core.EntityMap.pas',
  Spring.Persistence.Core.Exceptions in '..\..\Source\Persistence\Core\Spring.Persistence.Core.Exceptions.pas',
  Spring.Persistence.Core.Graphics in '..\..\Source\Persistence\Core\Spring.Persistence.Core.Graphics.pas',
  Spring.Persistence.Core.Interfaces in '..\..\Source\Persistence\Core\Spring.Persistence.Core.Interfaces.pas',
  Spring.Persistence.Core.ListSession in '..\..\Source\Persistence\Core\Spring.Persistence.Core.ListSession.pas',
  Spring.Persistence.Core.Repository.MongoDB in '..\..\Source\Persistence\Core\Spring.Persistence.Core.Repository.MongoDB.pas',
  Spring.Persistence.Core.Repository.Proxy in '..\..\Source\Persistence\Core\Spring.Persistence.Core.Repository.Proxy.pas',
  Spring.Persistence.Core.Repository.Simple in '..\..\Source\Persistence\Core\Spring.Persistence.Core.Repository.Simple.pas',
  Spring.Persistence.Core.ResourceStrings in '..\..\Source\Persistence\Core\Spring.Persistence.Core.ResourceStrings.pas',
  Spring.Persistence.Core.Session in '..\..\Source\Persistence\Core\Spring.Persistence.Core.Session.pas',
  Spring.Persistence.Core.Session.MongoDB in '..\..\Source\Persistence\Core\Spring.Persistence.Core.Session.MongoDB.pas',
  Spring.Persistence.Core.ValueConverters in '..\..\Source\Persistence\Core\Spring.Persistence.Core.ValueConverters.pas',
  Spring.Persistence.Criteria in '..\..\Source\Persistence\Criteria\Spring.Persistence.Criteria.pas',
  Spring.Persistence.Criteria.Criterion.Abstract in '..\..\Source\Persistence\Criteria\Spring.Persistence.Criteria.Criterion.Abstract.pas',
  Spring.Persistence.Criteria.Criterion.BetweenExpression in '..\..\Source\Persistence\Criteria\Spring.Persistence.Criteria.Criterion.BetweenExpression.pas',
  Spring.Persistence.Criteria.Criterion.Conjunction in '..\..\Source\Persistence\Criteria\Spring.Persistence.Criteria.Criterion.Conjunction.pas',
  Spring.Persistence.Criteria.Criterion.Disjunction in '..\..\Source\Persistence\Criteria\Spring.Persistence.Criteria.Criterion.Disjunction.pas',
  Spring.Persistence.Criteria.Criterion.InExpression in '..\..\Source\Persistence\Criteria\Spring.Persistence.Criteria.Criterion.InExpression.pas',
  Spring.Persistence.Criteria.Criterion.Junction in '..\..\Source\Persistence\Criteria\Spring.Persistence.Criteria.Criterion.Junction.pas',
  Spring.Persistence.Criteria.Criterion.LikeExpression in '..\..\Source\Persistence\Criteria\Spring.Persistence.Criteria.Criterion.LikeExpression.pas',
  Spring.Persistence.Criteria.Criterion.LogicalExpression in '..\..\Source\Persistence\Criteria\Spring.Persistence.Criteria.Criterion.LogicalExpression.pas',
  Spring.Persistence.Criteria.Criterion.NullExpression in '..\..\Source\Persistence\Criteria\Spring.Persistence.Criteria.Criterion.NullExpression.pas',
  Spring.Persistence.Criteria.Criterion.PropertyExpression in '..\..\Source\Persistence\Criteria\Spring.Persistence.Criteria.Criterion.PropertyExpression.pas',
  Spring.Persistence.Criteria.Criterion.SimpleExpression in '..\..\Source\Persistence\Criteria\Spring.Persistence.Criteria.Criterion.SimpleExpression.pas',
  Spring.Persistence.Criteria.Interfaces in '..\..\Source\Persistence\Criteria\Spring.Persistence.Criteria.Interfaces.pas',
  Spring.Persistence.Criteria.OrderBy in '..\..\Source\Persistence\Criteria\Spring.Persistence.Criteria.OrderBy.pas',
  Spring.Persistence.Criteria.Properties in '..\..\Source\Persistence\Criteria\Spring.Persistence.Criteria.Properties.pas',
  Spring.Persistence.Criteria.Restrictions in '..\..\Source\Persistence\Criteria\Spring.Persistence.Criteria.Restrictions.pas',
  Spring.Persistence.Mapping.Attributes in '..\..\Source\Persistence\Mapping\Spring.Persistence.Mapping.Attributes.pas',
  Spring.Persistence.Mapping.CodeGenerator in '..\..\Source\Persistence\Mapping\Spring.Persistence.Mapping.CodeGenerator.pas',
  Spring.Persistence.Mapping.CodeGenerator.Abstract in '..\..\Source\Persistence\Mapping\Spring.Persistence.Mapping.CodeGenerator.Abstract.pas',
  Spring.Persistence.Mapping.CodeGenerator.DB in '..\..\Source\Persistence\Mapping\Spring.Persistence.Mapping.CodeGenerator.DB.pas',
  Spring.Data.ActiveX in '..\..\Source\Data\ObjectDataSet\Spring.Data.ActiveX.pas',
  Spring.Data.ExpressionParser in '..\..\Source\Data\ObjectDataSet\Spring.Data.ExpressionParser.pas',
  Spring.Data.ExpressionParser.Functions in '..\..\Source\Data\ObjectDataSet\Spring.Data.ExpressionParser.Functions.pas',
  Spring.Data.IndexList in '..\..\Source\Data\ObjectDataSet\Spring.Data.IndexList.pas',
  Spring.Data.ObjectDataSet in '..\..\Source\Data\ObjectDataSet\Spring.Data.ObjectDataSet.pas',
  Spring.Data.ValueConverters in '..\..\Source\Data\ObjectDataSet\Spring.Data.ValueConverters.pas',
  Spring.Data.VirtualDataSet in '..\..\Source\Data\ObjectDataSet\Spring.Data.VirtualDataSet.pas',
  Spring.Persistence.SQL.Commands in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Commands.pas',
  Spring.Persistence.SQL.Commands.Abstract in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Commands.Abstract.pas',
  Spring.Persistence.SQL.Commands.BulkInsert.MongoDB in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Commands.BulkInsert.MongoDB.pas',
  Spring.Persistence.SQL.Commands.CreateForeignKey in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Commands.CreateForeignKey.pas',
  Spring.Persistence.SQL.Commands.CreateSequence in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Commands.CreateSequence.pas',
  Spring.Persistence.SQL.Commands.CreateTable in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Commands.CreateTable.pas',
  Spring.Persistence.SQL.Commands.Delete in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Commands.Delete.pas',
  Spring.Persistence.SQL.Commands.Insert in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Commands.Insert.pas',
  Spring.Persistence.SQL.Commands.Page in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Commands.Page.pas',
  Spring.Persistence.SQL.Commands.Select in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Commands.Select.pas',
  Spring.Persistence.SQL.Commands.Update in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Commands.Update.pas',
  Spring.Persistence.SQL.Generators.Abstract in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Generators.Abstract.pas',
  Spring.Persistence.SQL.Generators.Ansi in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Generators.Ansi.pas',
  Spring.Persistence.SQL.Generators.ASA in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Generators.ASA.pas',
  Spring.Persistence.SQL.Generators.Firebird in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Generators.Firebird.pas',
  Spring.Persistence.SQL.Generators.MongoDB in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Generators.MongoDB.pas',
  Spring.Persistence.SQL.Generators.MSSQL in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Generators.MSSQL.pas',
  Spring.Persistence.SQL.Generators.MySQL in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Generators.MySQL.pas',
  Spring.Persistence.SQL.Generators.NoSQL in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Generators.NoSQL.pas',
  Spring.Persistence.SQL.Generators.Oracle in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Generators.Oracle.pas',
  Spring.Persistence.SQL.Generators.PostgreSQL in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Generators.PostgreSQL.pas',
  Spring.Persistence.SQL.Generators.Register in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Generators.Register.pas',
  Spring.Persistence.SQL.Generators.SQLite3 in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Generators.SQLite3.pas',
  Spring.Persistence.SQL.Interfaces in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Interfaces.pas',
  Spring.Persistence.SQL.Params in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Params.pas',
  Spring.Persistence.SQL.Register in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Register.pas',
  Spring.Persistence.SQL.Types in '..\..\Source\Persistence\SQL\Spring.Persistence.SQL.Types.pas',
  TestExceptions in 'TestExceptions.pas',
  TestAdaptersMongoDB in 'TestAdaptersMongoDB.pas',
  TestAdaptersASA in 'TestAdaptersASA.pas',
  TestAdaptersOracle in 'TestAdaptersOracle.pas',
  TestSQLServerSQLGenerator in 'TestSQLServerSQLGenerator.pas',
  TestPostgreSQLGenerator in 'TestPostgreSQLGenerator.pas',
  TestAnsiSQLGenerator in 'TestAnsiSQLGenerator.pas',
  TestCommands in 'TestCommands.pas',
  TestConnectionFactory in 'TestConnectionFactory.pas',
  TestConsts in 'TestConsts.pas',
  TestCoreCriteria in 'TestCoreCriteria.pas',
  TestCoreEntityMap in 'TestCoreEntityMap.pas',
  TestCoreUtils in 'TestCoreUtils.pas',
  TestDatabaseManager in 'TestDatabaseManager.pas',
  {$IFDEF DELPHIXE5_UP}
  TestAdaptersFireDAC in 'TestAdaptersFireDAC.pas',
  {$ENDIF}
  TestObjectDataSet in 'TestObjectDataSet.pas',
  TestPersistence in 'TestPersistence.pas',
  TestSession in 'TestSession.pas',
  TestSimpleRepository in 'TestSimpleRepository.pas',
  TestAdaptersSQLite in 'TestAdaptersSQLite.pas',
  TestEntities in 'TestEntities.pas',
  ViewTestObjectDataSet in 'ViewTestObjectDataSet.pas' {frmObjectDataSetTest},
  Spring.Persistence.Core.EntityWrapper in '..\..\Source\Persistence\Core\Spring.Persistence.Core.EntityWrapper.pas',
  TestEntityWrapper in 'TestEntityWrapper.pas',
  TestAdaptersADO in 'TestAdaptersADO.pas',
  TestMockADOConnection in 'TestMockADOConnection.pas',
  TestAdaptersMSSQL in 'TestAdaptersMSSQL.pas',
  TestAdaptersDBX in 'TestAdaptersDBX.pas',
  TestSQLConnection in 'TestSQLConnection.pas';

begin
  OutputDir := IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0)));
  PictureFilename := IncludeTrailingPathDelimiter(ExpandFileName(OutputDir + '..\..')) + 'DelphiOOP.png';
  ScannerFilename := IncludeTrailingPathDelimiter(ExpandFileName(OutputDir + '..\..')) + 'DelphiOOP.pdf';

  RunRegisteredTests;
end.

