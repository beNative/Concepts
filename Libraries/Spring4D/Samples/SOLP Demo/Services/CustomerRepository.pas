unit CustomerRepository;

interface

uses
  Customer,
  Spring.Collections,
  Spring.Persistence.Core.Repository.Simple;

type
  TCustomerRepository = class(TSimpleRepository<TCustomer,string>)
  public
    constructor Create;
    function FindOne(const id: string): TCustomer; override;
    function FindAll: IList<TCustomer>; override;
  end;

implementation

{ TCustomerRepository }

constructor TCustomerRepository.Create;
begin
  inherited Create(nil);
end;

function TCustomerRepository.FindAll: IList<TCustomer>;
begin
  Result := TCollections.CreateList<TCustomer>(True);
  Result.Add(FindOne('ALFKI'));
  Result.Add(FindOne('WOZKA'));
end;

function TCustomerRepository.FindOne(const id: string): TCustomer;
begin
  Result := TCustomer.Create;
  Result.CompanyName := 'Customer ' + id;
  Result.CustomerId := id;
end;

end.
