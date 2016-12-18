unit CommandChannel;

interface

uses
  Interfaces;

type
  TCommandChannel = class(TInterfacedObject, ICommandChannel)
  public
    function Send(const item: TObject): Integer; virtual;
    function Send2(const item: TObject): Integer; virtual;
  end;

implementation


{$REGION 'TCommandChannel'}

function TCommandChannel.Send(const item: TObject): Integer;
begin
  Result := -1;
end;

function TCommandChannel.Send2(const item: TObject): Integer;
begin
  Result := 0;
end;

{$ENDREGION}


end.
