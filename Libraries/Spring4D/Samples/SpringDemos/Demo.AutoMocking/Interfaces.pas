unit Interfaces;

interface

uses
  Spring.Collections;

type
  ICommandChannel = interface(IInvokable)
    ['{1C78299A-9963-45B6-BAD5-D31251754625}']
    function Send(const item: TObject): Integer;
    function Send2(const item: TObject): Integer;
  end;

  INumberParser = interface(IInvokable)
    ['{5CDF38F1-B28F-4B61-BA7E-6EAC0B2BDDF9}']
    function Parse(const expression: string): IEnumerable<Integer>;
  end;

  INumberParserFactory = interface(IInvokable)
    ['{C12B2F4B-11D5-4A8F-872B-FE7437BD1C70}']
    function Create(delimiter: Char): INumberParser;
  end;

implementation

end.
