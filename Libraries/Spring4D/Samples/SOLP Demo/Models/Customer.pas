unit Customer;

interface

type
  TCustomer = class
  private
    FCustomerId: string;
    FCompanyName: string;
    FCity: string;
  public
    property CustomerId: string read FCustomerId write FCustomerId;
    property CompanyName: string read FCompanyName write FCompanyName;
    property City: string read FCity write FCity;
  end;

implementation

end.
