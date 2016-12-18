unit ViewEditProduct;

interface

uses
  Classes,
  Controls,
  Dialogs,
  Forms,
  Graphics,
  Mask,
  Messages,
  Spin,
  StdCtrls,
  SysUtils,
  Variants,
  Windows,
  ProductModel;

type
  TProductEditForm = class(TForm)
    CancelButton: TButton;
    NameEdit: TEdit;
    NameLabel: TLabel;
    OkButton: TButton;
    PriceLabel: TLabel;
    PriceMaskEdit: TMaskEdit;
    QuantityLabel: TLabel;
    QuantitySpinEdit: TSpinEdit;
    procedure OkButtonClick(Sender: TObject);
  private
    FProduct: TProduct;
    procedure SetProduct(const Value: TProduct);
  protected
    procedure DoGetProductProperties; virtual;
    procedure DoSetProductProperties; virtual;
  public
    class function Edit(AProduct: TProduct): Boolean;
    property Product: TProduct read FProduct write SetProduct;
  end;

var
  ProductEditForm: TProductEditForm;

implementation

{$R *.dfm}

{ TfrmEditProduct }

procedure TProductEditForm.OkButtonClick(Sender: TObject);
begin
  DoSetProductProperties;
end;

procedure TProductEditForm.DoGetProductProperties;
begin
  NameEdit.Text := FProduct.Name;
  PriceMaskEdit.Text := CurrToStr(FProduct.Price);
  QuantitySpinEdit.Value := FProduct.Quantity;
end;

procedure TProductEditForm.DoSetProductProperties;
begin
  FProduct.Name := NameEdit.Text;
  FProduct.Price := StrToCurr(PriceMaskEdit.Text);
  FProduct.Quantity := QuantitySpinEdit.Value;
end;

class function TProductEditForm.Edit(AProduct: TProduct): Boolean;
var
  LForm: TProductEditForm;
begin
  LForm := TProductEditForm.Create(Application);
  try
    LForm.Product := AProduct;
    Result := (LForm.ShowModal = mrOk);
  finally
    LForm.Free;
  end;
end;

procedure TProductEditForm.SetProduct(const Value: TProduct);
begin
  FProduct := Value;
  DoGetProductProperties;
end;

end.
