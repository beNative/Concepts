unit BasketController;

interface

uses
  Interfaces;

type
  TBasketController = class
  private
    fChannel: ICommandChannel;
  public
    constructor Create(const channel: ICommandChannel);
    procedure Post(const item: TObject);
  end;

implementation

{$REGION 'TBasketController'}

constructor TBasketController.Create(const channel: ICommandChannel);
begin
  inherited Create;
  fChannel := channel;
end;

procedure TBasketController.Post(const item: TObject);
begin
  fChannel.Send(item);
end;

{$ENDREGION}


end.
