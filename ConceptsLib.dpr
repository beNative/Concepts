library ConceptsLib;

uses
  ExceptionLog,
  SysUtils, Classes, Dialogs;

{$R *.res}

procedure ShowMessageFromDLL(AMessage: PChar); stdcall;
begin
  ShowMessage(AMessage);
end;

exports
  ShowMessageFromDLL;

begin
end.
