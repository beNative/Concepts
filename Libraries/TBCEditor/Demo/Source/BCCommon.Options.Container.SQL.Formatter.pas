unit BCCommon.Options.Container.SQL.Formatter;

interface

uses
  System.Classes, IniPersist, BCCommon.SQL.Consts;

type
  TSQLFormatterOptionsContainer = class(TPersistent)
  private
    { Select Column List }
    FSelectColumnListStyle: Integer;
    FSelectColumnListLineBreak: Integer;
    FSelectColumnListColumnInNewLine: Boolean;
    FSelectColumnListAlignAlias: Boolean;
    FSelectColumnListTreatDistinctAsVirtualColumn: Boolean;
    { Select Subquery }
    FSelectSubqueryNewLineAfterIn: Boolean;
    FSelectSubqueryNewLineAfterExists: Boolean;
    FSelectSubqueryNewLineAfterComparisonOperator: Boolean;
    FSelectSubqueryNewLineBeforeComparisonOperator: Boolean;
    { Select Into Clause }
    FSelectIntoClauseInNewLine: Boolean;
    { Select From/Join Clause }
    FSelectFromClauseStyle: Integer;
    FSelectFromClauseInNewLine: Boolean;
    FSelectJoinClauseInNewLine: Boolean;
    FSelectAlignJoinWithFromKeyword: Boolean;
    FSelectAlignAndOrWithOnInJoinClause: Boolean;
    FSelectAlignAliasInFromClause: Boolean;
    { Select And/Or Clause }
    FSelectAndOrLineBreak: Integer;
    FSelectAndOrUnderWhere: Boolean;
    FSelectWhereClauseInNewline: Boolean;
    FSelectWhereClauseAlignExpr: Boolean;
    { Select Group By Clause }
    FSelectGroupByClauseStyle: Integer;
    FSelectGroupByClauseInNewLine: Boolean;
    { Select Having Clause }
    FSelectHavingClauseInNewLine: Boolean;
    { Select Order By Clause }
    FSelectOrderByClauseStyle: Integer;
    FSelectOrderByClauseInNewLine: Boolean;
    { Insert }
    FInsertColumnlistStyle: Integer;
    FInsertValuelistStyle: Integer;
    FInsertParenthesisInSeparateLine: Boolean;
    FInsertColumnsPerLine: Integer;
    { Update }
    FUpdateColumnlistStyle: Integer;
    { Database }
    FSQLDatabase: Integer;
    { Alignments }
    FKeywordAlign: Integer;
    FKeywordAlignmentLeftJustify: Boolean;
    { Whitespace }
    FWhitespaceSpaceAroundOperator: Boolean;
    FWhitespaceSpaceInsideCreate: Boolean;
    FWhitespaceSpaceInsideExpression: Boolean;
    FWhitespaceSpaceInsideSubquery: Boolean;
    FWhitespaceSpaceInsideFunction: Boolean;
    FWhitespaceSpaceInsideTypename: Boolean;
    { Indentation }
    FIndentationIndentLength: Integer;
    FIndentationUseTab: Boolean;
    FIndentationTabSize: Integer;
    FIndentationFunctionBodyIndent: Integer;
    FIndentationBlockLeftOnNewline: Boolean;
    FIndentationBlockLeftIndentSize: Integer;
    FIndentationBlockRightIndentSize: Integer;
    FIndentationBlockIndentSize: Integer;
    FIndentationIfElseSingleStmtIndentSize: Integer;
    { Capitalization }
    FCapitalizationKeywords: Integer;
    FCapitalizationIdentifier: Integer;
    FCapitalizationQuotedIdentifier: Integer;
    FCapitalizationTableName: Integer;
    FCapitalizationColumnName: Integer;
    FCapitalizationAliasName: Integer;
    FCapitalizationVariableName: Integer;
    FCapitalizationFuncname: Integer;
    FCapitalizationDatatype: Integer;
  public
    procedure ReadIniFile;
    procedure WriteIniFile;
    { Select Column List }
    [IniValue(SQLFORMATTER, SELECTCOLUMNLISTSTYLE, '0')]
    property SelectColumnListStyle: Integer read FSelectColumnListStyle write FSelectColumnListStyle;
    [IniValue(SQLFORMATTER, SELECTCOLUMNLISTLINEBREAK, '0')]
    property SelectColumnListLineBreak: Integer read FSelectColumnListLineBreak write FSelectColumnListLineBreak;
    [IniValue(SQLFORMATTER, SELECTCOLUMNLISTCOLUMNINNEWLINE, 'False')]
    property SelectColumnListColumnInNewLine: Boolean read FSelectColumnListColumnInNewLine write FSelectColumnListColumnInNewLine;
    [IniValue(SQLFORMATTER, SELECTCOLUMNLISTALIGNALIAS, 'True')]
    property SelectColumnListAlignAlias: Boolean read FSelectColumnListAlignAlias write FSelectColumnListAlignAlias;
    [IniValue(SQLFORMATTER, SELECTCOLUMNLISTTREATDISTINCTASVIRTUALCOLUMN, 'False')]
    property SelectColumnListTreatDistinctAsVirtualColumn: Boolean read FSelectColumnListTreatDistinctAsVirtualColumn write FSelectColumnListTreatDistinctAsVirtualColumn;
    { Select Subquery }
    [IniValue(SQLFORMATTER, SELECTSUBQUERYNEWLINEAFTERIN, 'False')]
    property SelectSubqueryNewLineAfterIn: Boolean read FSelectSubqueryNewLineAfterIn write FSelectSubqueryNewLineAfterIn;
    [IniValue(SQLFORMATTER, SELECTSUBQUERYNEWLINEAFTEREXISTS, 'False')]
    property SelectSubqueryNewLineAfterExists: Boolean read FSelectSubqueryNewLineAfterExists write FSelectSubqueryNewLineAfterExists;
    [IniValue(SQLFORMATTER, SELECTSUBQUERYNEWLINEAFTERCOMPARISONOPERATOR, 'False')]
    property SelectSubqueryNewLineAfterComparisonOperator: Boolean read FSelectSubqueryNewLineAfterComparisonOperator write FSelectSubqueryNewLineAfterComparisonOperator;
    [IniValue(SQLFORMATTER, SELECTSUBQUERYNEWLINEBEFORECOMPARISONOPERATOR, 'False')]
    property SelectSubqueryNewLineBeforeComparisonOperator: Boolean read FSelectSubqueryNewLineBeforeComparisonOperator write FSelectSubqueryNewLineBeforeComparisonOperator;
    { Select Into Clause }
    [IniValue(SQLFORMATTER, SELECTINTOCLAUSEINNEWLINE, 'False')]
    property SelectIntoClauseInNewLine: Boolean read FSelectIntoClauseInNewLine write FSelectIntoClauseInNewLine;
    { Select From/Join Clause }
    [IniValue(SQLFORMATTER, SELECTFROMCLAUSESTYLE, '0')]
    property SelectFromClauseStyle: Integer read FSelectFromClauseStyle write FSelectFromClauseStyle;
    [IniValue(SQLFORMATTER, SELECTFROMCLAUSEINNEWLINE, 'False')]
    property SelectFromClauseInNewLine: Boolean read FSelectFromClauseInNewLine write FSelectFromClauseInNewLine;
    [IniValue(SQLFORMATTER, SELECTJOINCLAUSEINNEWLINE, 'True')]
    property SelectJoinClauseInNewLine: Boolean read FSelectJoinClauseInNewLine write FSelectJoinClauseInNewLine;
    [IniValue(SQLFORMATTER, SELECTALIGNJOINWITHFROMKEYWORD, 'False')]
    property SelectAlignJoinWithFromKeyword: Boolean read FSelectAlignJoinWithFromKeyword write FSelectAlignJoinWithFromKeyword;
    [IniValue(SQLFORMATTER, SELECTALIGNANDORWITHONINJOINCLAUSE, 'False')]
    property SelectAlignAndOrWithOnInJoinClause: Boolean read FSelectAlignAndOrWithOnInJoinClause write FSelectAlignAndOrWithOnInJoinClause;
    [IniValue(SQLFORMATTER, SELECTALIGNALIASINFROMCLAUSE, 'False')]
    property SelectAlignAliasInFromClause: Boolean read FSelectAlignAliasInFromClause write FSelectAlignAliasInFromClause;
    { Select And/Or Clause }
    [IniValue(SQLFORMATTER, SELECTANDORLINEBREAK, '0')]
    property SelectAndOrLineBreak: Integer read FSelectAndOrLineBreak write FSelectAndOrLineBreak;
    [IniValue(SQLFORMATTER, SELECTANDORUNDERWHERE, 'False')]
    property SelectAndOrUnderWhere: Boolean read FSelectAndOrUnderWhere write FSelectAndOrUnderWhere;
    [IniValue(SQLFORMATTER, SELECTWHERECLAUSEINNEWLINE, 'False')]
    property SelectWhereClauseInNewline: Boolean read FSelectWhereClauseInNewline write FSelectWhereClauseInNewline;
    [IniValue(SQLFORMATTER, SELECTWHERECLAUSEALIGNEXPR, 'False')]
    property SelectWhereClauseAlignExpr: Boolean read FSelectWhereClauseAlignExpr write FSelectWhereClauseAlignExpr;
    { Select Group By Clause }
    [IniValue(SQLFORMATTER, SELECTGROUPBYCLAUSESTYLE, '0')]
    property SelectGroupByClauseStyle: Integer read FSelectGroupByClauseStyle write FSelectGroupByClauseStyle;
    [IniValue(SQLFORMATTER, SELECTGROUPBYCLAUSEINNEWLINE, 'False')]
    property SelectGroupByClauseInNewLine: Boolean read FSelectGroupByClauseInNewLine write FSelectGroupByClauseInNewLine;
    { Select Having Clause }
    [IniValue(SQLFORMATTER, SELECTHAVINGCLAUSEINNEWLINE, 'False')]
    property SelectHavingClauseInNewLine: Boolean read FSelectHavingClauseInNewLine write FSelectHavingClauseInNewLine;
    { Select Order By Clause }
    [IniValue(SQLFORMATTER, SELECTORDERBYCLAUSESTYLE, '0')]
    property SelectOrderByClauseStyle: Integer read FSelectOrderByClauseStyle write FSelectOrderByClauseStyle;
    [IniValue(SQLFORMATTER, SELECTORDERBYCLAUSEINNEWLINE, 'False')]
    property SelectOrderByClauseInNewLine: Boolean read FSelectOrderByClauseInNewLine write FSelectOrderByClauseInNewLine;
    { Insert }
    [IniValue(SQLFORMATTER, INSERTCOLUMNLISTSTYLE, '0')]
    property InsertColumnListStyle: Integer read FInsertColumnListStyle write FInsertColumnListStyle;
    [IniValue(SQLFORMATTER, INSERTVALUELISTSTYLE, '0')]
    property InsertValueListStyle: Integer read FInsertValueListStyle write FInsertValueListStyle;
    [IniValue(SQLFORMATTER, INSERTPARENTHESISINSEPARATELINE, 'False')]
    property InsertParenthesisInSeparateLine: Boolean read FInsertParenthesisInSeparateLine write FInsertParenthesisInSeparateLine;
    [IniValue(SQLFORMATTER, INSERTCOLUMNSPERLINE, '0')]
    property InsertColumnsPerLine: Integer read FInsertColumnsPerLine write FInsertColumnsPerLine;
    { Update }
    [IniValue(SQLFORMATTER, UPDATECOLUMNLISTSTYLE, '0')]
    property UpdateColumnListStyle: Integer read FUpdateColumnListStyle write FUpdateColumnListStyle;
    { SQL Database }
    [IniValue(SQLFORMATTER, SQLDatabase, '4')] { 4 = Generic }
    property SQLDatabase: Integer read FSQLDatabase write FSQLDatabase;
    { Alignments }
    [IniValue(SQLFORMATTER, KEYWORDALIGN, '0')]
    property KeywordAlign: Integer read FKeywordAlign write FKeywordAlign;
    [IniValue(SQLFORMATTER, KEYWORDALIGNMENTLEFTJUSTIFY, 'False')]
    property KeywordAlignmentLeftJustify: Boolean read FKeywordAlignmentLeftJustify write FKeywordAlignmentLeftJustify;
    { Whitespace }
    [IniValue(SQLFORMATTER, WHITESPACESPACEAROUNDOPERATOR, 'True')]
    property WhitespaceSpaceAroundOperator: Boolean read FWhitespaceSpaceAroundOperator write FWhitespaceSpaceAroundOperator;
    [IniValue(SQLFORMATTER, WHITESPACESPACEINSIDECREATE, 'False')]
    property WhitespaceSpaceInsideCreate: Boolean read FWhitespaceSpaceInsideCreate write FWhitespaceSpaceInsideCreate;
    [IniValue(SQLFORMATTER, WHITESPACESPACEINSIDEEXPRESSION, 'False')]
    property WhitespaceSpaceInsideExpression: Boolean read FWhitespaceSpaceInsideExpression write FWhitespaceSpaceInsideExpression;
    [IniValue(SQLFORMATTER, WHITESPACESPACEINSIDESUBQUERY, 'False')]
    property WhitespaceSpaceInsideSubquery: Boolean read FWhitespaceSpaceInsideSubquery write FWhitespaceSpaceInsideSubquery;
    [IniValue(SQLFORMATTER, WHITESPACESPACEINSIDEFUNCTION, 'False')]
    property WhitespaceSpaceInsideFunction: Boolean read FWhitespaceSpaceInsideFunction write FWhitespaceSpaceInsideFunction;
    [IniValue(SQLFORMATTER, WHITESPACESPACEINSIDETYPENAME, 'False')]
    property WhitespaceSpaceInsideTypename: Boolean read FWhitespaceSpaceInsideTypename write FWhitespaceSpaceInsideTypename;
    { Indentation }
    [IniValue(SQLFORMATTER, INDENTATIONINDENTLENGTH, '2')]
    property IndentationIndentLength: Integer read FIndentationIndentLength write FIndentationIndentLength;
    [IniValue(SQLFORMATTER, INDENTATIONUSETAB, 'False')]
    property IndentationUseTab: Boolean read FIndentationUseTab write FIndentationUseTab;
    [IniValue(SQLFORMATTER, INDENTATIONTABSIZE, '2')]
    property IndentationTabSize: Integer read FIndentationTabSize write FIndentationTabSize;
    [IniValue(SQLFORMATTER, INDENTATIONFUNCTIONBODYINDENT, '2')]
    property IndentationFunctionBodyIndent: Integer read FIndentationFunctionBodyIndent write FIndentationFunctionBodyIndent;
    [IniValue(SQLFORMATTER, INDENTATIONBLOCKLEFTONNEWLINE, 'True')]
    property IndentationBlockLeftOnNewline: Boolean read FIndentationBlockLeftOnNewline write FIndentationBlockLeftOnNewline;
    [IniValue(SQLFORMATTER, INDENTATIONBLOCKLEFTINDENTSIZE, '2')]
    property IndentationBlockLeftIndentSize: Integer read FIndentationBlockLeftIndentSize write FIndentationBlockLeftIndentSize;
    [IniValue(SQLFORMATTER, INDENTATIONBLOCKRIGHTINDENTSIZE, '2')]
    property IndentationBlockRightIndentSize: Integer read FIndentationBlockRightIndentSize write FIndentationBlockRightIndentSize;
    [IniValue(SQLFORMATTER, INDENTATIONBLOCKINDENTSIZE, '2')]
    property IndentationBlockIndentSize: Integer read FIndentationBlockIndentSize write FIndentationBlockIndentSize;
    [IniValue(SQLFORMATTER, INDENTATIONIFELSESINGLESTMTINDENTSIZE, '2')]
    property IndentationIfElseSingleStmtIndentSize: Integer read FIndentationIfElseSingleStmtIndentSize write FIndentationIfElseSingleStmtIndentSize;
    { Capitalization }
    [IniValue(SQLFORMATTER, CAPITALIZATIONKEYWORDS, '0')]
    property CapitalizationKeywords: Integer read FCapitalizationKeywords write FCapitalizationKeywords;
    [IniValue(SQLFORMATTER, CAPITALIZATIONIDENTIFIER, '1')]
    property CapitalizationIdentifier: Integer read FCapitalizationIdentifier write FCapitalizationIdentifier;
    [IniValue(SQLFORMATTER, CAPITALIZATIONQUOTEDIDENTIFIER, '3')]
    property CapitalizationQuotedIdentifier: Integer read FCapitalizationQuotedIdentifier write FCapitalizationQuotedIdentifier;
    [IniValue(SQLFORMATTER, CAPITALIZATIONTABLENAME, '1')]
    property CapitalizationTableName: Integer read FCapitalizationTableName write FCapitalizationTableName;
    [IniValue(SQLFORMATTER, CAPITALIZATIONCOLUMNNAME, '1')]
    property CapitalizationColumnName: Integer read FCapitalizationColumnName write FCapitalizationColumnName;
    [IniValue(SQLFORMATTER, CAPITALIZATIONALIASNAME, '1')]
    property CapitalizationAliasName: Integer read FCapitalizationAliasName write FCapitalizationAliasName;
    [IniValue(SQLFORMATTER, CAPITALIZATIONVARIABLENAME, '1')]
    property CapitalizationVariableName: Integer read FCapitalizationVariableName write FCapitalizationVariableName;
    [IniValue(SQLFORMATTER, CAPITALIZATIONFUNCNAME, '1')]
    property CapitalizationFuncname: Integer read FCapitalizationFuncname write FCapitalizationFuncname;
    [IniValue(SQLFORMATTER, CAPITALIZATIONDATATYPE, '0')]
    property CapitalizationDatatype: Integer read FCapitalizationDatatype write FCapitalizationDatatype;
  end;

  TSQLDatabase = (dbMSSql, dbOracle, dbMySQL, dbAccess, dbGeneric, dbDB2, dbSybase, dbInformix, dbPostgreSQL, dbFirebird, dbMdx);

  TFormatSQL = function(SQL: PWideChar; Database: Integer): PWideChar; stdcall;
  TFreeAString = procedure(AStr: PWideChar); stdcall;

function FormatSQL(SQL: string; Database: TSQLDatabase): string;
function SQLFormatterOptionsContainer: TSQLFormatterOptionsContainer;

implementation

uses
  Winapi.Windows, BCCommon.Messages, BCCommon.FileUtils, System.SysUtils;

var
  FSQLFormatterOptions: TSQLFormatterOptionsContainer;

function SQLFormatterOptionsContainer: TSQLFormatterOptionsContainer;
begin
  if not Assigned(FSQLFormatterOptions) then
    FSQLFormatterOptions := TSQLFormatterOptionsContainer.Create;
  Result := FSQLFormatterOptions;
end;

{ TSQLFormatterOptions }

procedure TSQLFormatterOptionsContainer.ReadIniFile;
begin
  TIniPersist.Load(GetIniFilename, Self);
end;

procedure TSQLFormatterOptionsContainer.WriteIniFile;
begin
  TIniPersist.Save(GetIniFilename, Self);
end;

function FormatSQL(SQL: string; Database: TSQLDatabase): string;
var
  DLLHandle: THandle;
  s: PWideChar;
  FormatSQLFunction: TFormatSQL;
  FreeAStringProcedure: TFreeAString;
begin
  s := nil;
  FreeAStringProcedure := nil;
  DLLHandle := LoadLibrary(PWideChar(GetSQLFormatterDLLFilename));
  try
    if DLLHandle = 0 then
    begin
      ShowErrorMessage('DLL load failure');
      Result := SQL;
      Exit;
    end;
    @FormatSQLFunction := GetProcAddress(DLLHandle, 'FormatSQL');
    @FreeAStringProcedure := GetProcAddress(DLLHandle, 'FreeAString');
    if Assigned(FormatSQLFunction) then
      s := FormatSQLFunction(PWideChar(SQL), Ord(Database));
    Result := s;
  finally
    if Assigned(FreeAStringProcedure) then
      FreeAStringProcedure(s);
    FreeLibrary(DLLHandle);
  end;
end;

end.
