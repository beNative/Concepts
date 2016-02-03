unit BCCommon.SQL.Consts;

interface

const
  SQLDATABASE = 'SQLDatabase';
  SQLFORMATTER = 'SQLFormatter';
  { Select Column List }
  SELECTCOLUMNLISTSTYLE = 'SelectColumnListStyle';
  SELECTCOLUMNLISTLINEBREAK = 'SelectColumnListLineBreak';
  SELECTCOLUMNLISTCOLUMNINNEWLINE = 'SelectColumnListColumnInNewLine';
  SELECTCOLUMNLISTALIGNALIAS = 'SelectColumnListAlignAlias';
  SELECTCOLUMNLISTTREATDISTINCTASVIRTUALCOLUMN = 'SelectColumnListTreatDistinctAsVirtualColumn';
  { Select Subquery }
  SELECTSUBQUERYNEWLINEAFTERIN = 'SelectSubqueryNewLineAfterIn';
  SELECTSUBQUERYNEWLINEAFTEREXISTS = 'SelectSubqueryNewLineAfterExists';
  SELECTSUBQUERYNEWLINEAFTERCOMPARISONOPERATOR = 'SelectSubqueryNewLineAfterComparisonOperator';
  SELECTSUBQUERYNEWLINEBEFORECOMPARISONOPERATOR = 'SelectSubqueryNewLineBeforeComparisonOperator';
  { Select Into Clause }
  SELECTINTOCLAUSEINNEWLINE = 'SelectIntoClauseInNewLine';
  { Select From/Join Clause }
  SELECTFROMCLAUSESTYLE = 'SelectFromClauseStyle';
  SELECTFROMCLAUSEINNEWLINE = 'SelectFromClauseInNewLine';
  SELECTJOINCLAUSEINNEWLINE = 'SelectJoinClauseInNewLine';
  SELECTALIGNJOINWITHFROMKEYWORD = 'SelectAlignJoinWithFromKeyword';
  SELECTALIGNANDORWITHONINJOINCLAUSE = 'SelectAlignAndOrWithOnInJoinClause';
  SELECTALIGNALIASINFROMCLAUSE = 'SelectAlignAliasInFromClause';
  { Select And/Or Clause }
  SELECTANDORLINEBREAK = 'SelectAndOrLineBreak';
  SELECTANDORUNDERWHERE = 'SelectAndOrUnderWhere';
  SELECTWHERECLAUSEINNEWLINE = 'SelectWhereClauseInNewline';
  SELECTWHERECLAUSEALIGNEXPR = 'SelectWhereClauseAlignExpr';
  { Select Group By Clause }
  SELECTGROUPBYCLAUSESTYLE = 'SelectGroupByClauseStyle';
  SELECTGROUPBYCLAUSEINNEWLINE = 'SelectGroupByClauseInNewLine';
  { Select Having Clause }
  SELECTHAVINGCLAUSEINNEWLINE = 'SelectHavingClauseInNewLine';
  { Select Order By Clause }
  SELECTORDERBYCLAUSESTYLE = 'SelectOrderByClauseStyle';
  SELECTORDERBYCLAUSEINNEWLINE = 'SelectOrderByClauseInNewLine';
  { Insert }
  INSERTCOLUMNLISTSTYLE = 'InsertColumnListStyle';
  INSERTVALUELISTSTYLE = 'InsertValueListStyle';
  INSERTPARENTHESISINSEPARATELINE = 'InsertParenthesisInSeparateLine';
  INSERTCOLUMNSPERLINE = 'InsertColumnsPerLine';
  { Update }
  UPDATECOLUMNLISTSTYLE = 'UpdateColumnListStyle';
  { Alignments }
  KEYWORDALIGN = 'KeywordAlign';
  KEYWORDALIGNMENTLEFTJUSTIFY = 'KeywordAlignmentLeftJustify';
  { Whitespace }
  WHITESPACESPACEAROUNDOPERATOR = 'WhitespaceSpaceAroundOperator';
  WHITESPACESPACEINSIDECREATE = 'WhitespaceSpaceInsideCreate';
  WHITESPACESPACEINSIDEEXPRESSION = 'WhitespaceSpaceInsideExpression';
  WHITESPACESPACEINSIDESUBQUERY = 'WhitespaceSpaceInsideSubquery';
  WHITESPACESPACEINSIDEFUNCTION = 'WhitespaceSpaceInsideFunction';
  WHITESPACESPACEINSIDETYPENAME = 'WhitespaceSpaceInsideTypename';
  { Indentation }
  INDENTATIONINDENTLENGTH = 'IndentationIndentLength';
  INDENTATIONUSETAB = 'IndentationUseTab';
  INDENTATIONTABSIZE = 'IndentationTabSize';
  INDENTATIONFUNCTIONBODYINDENT = 'IndentationFunctionBodyIndent';
  INDENTATIONBLOCKLEFTONNEWLINE = 'IndentationBlockLeftOnNewline';
  INDENTATIONBLOCKLEFTINDENTSIZE = 'IndentationBlockLeftIndentSize';
  INDENTATIONBLOCKRIGHTINDENTSIZE = 'IndentationBlockRightIndentSize';
  INDENTATIONBLOCKINDENTSIZE = 'IndentationBlockIndentSize';
  INDENTATIONIFELSESINGLESTMTINDENTSIZE = 'IndentationIfElseSingleStmtIndentSize';
  { Capitalization }
  CAPITALIZATIONKEYWORDS = 'CapitalizationKeywords';
  CAPITALIZATIONIDENTIFIER = 'CapitalizationIdentifier';
  CAPITALIZATIONQUOTEDIDENTIFIER = 'CapitalizationQuotedIdentifier';
  CAPITALIZATIONTABLENAME = 'CapitalizationTableName';
  CAPITALIZATIONCOLUMNNAME = 'CapitalizationColumnName';
  CAPITALIZATIONALIASNAME = 'CapitalizationAliasName';
  CAPITALIZATIONVARIABLENAME = 'CapitalizationVariableName';
  CAPITALIZATIONFUNCNAME = 'CapitalizationFuncname';
  CAPITALIZATIONDATATYPE = 'CapitalizationDatatype';

implementation

end.
