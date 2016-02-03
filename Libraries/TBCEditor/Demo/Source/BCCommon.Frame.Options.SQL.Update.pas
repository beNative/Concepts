unit BCCommon.Frame.Options.SQL.Update;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, BCCommon.Options.Container.SQL.Formatter,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BCCommon.Frame.Options.Base, Vcl.StdCtrls, BCControl.ComboBox,
  sComboBox, BCControl.Panel, Vcl.ExtCtrls, sPanel, sFrameAdapter;

type
  TOptionsSQLUpdateFrame = class(TBCOptionsBaseFrame)
    ComboBoxColumnListStyle: TBCComboBox;
    Panel: TBCPanel;
  protected
    procedure GetData; override;
    procedure Init; override;
    procedure PutData; override;
  public
    destructor Destroy; override;
  end;

function OptionsSQLUpdateFrame(AOwner: TComponent): TOptionsSQLUpdateFrame;

implementation

{$R *.dfm}

uses
  BCCommon.Language.Strings;

var
  FOptionsSQLUpdateFrame: TOptionsSQLUpdateFrame;

function OptionsSQLUpdateFrame(AOwner: TComponent): TOptionsSQLUpdateFrame;
begin
  if not Assigned(FOptionsSQLUpdateFrame) then
    FOptionsSQLUpdateFrame := TOptionsSQLUpdateFrame.Create(AOwner);
  Result := FOptionsSQLUpdateFrame;
end;

destructor TOptionsSQLUpdateFrame.Destroy;
begin
  inherited;
  FOptionsSQLUpdateFrame := nil;
end;

procedure TOptionsSQLUpdateFrame.Init;
begin
  with ComboBoxColumnListStyle.Items do
  begin
    Add(LanguageDatamodule.GetSQLFormatter('Stacked'));
    Add(LanguageDatamodule.GetSQLFormatter('Wrapped'));
  end;
end;

procedure TOptionsSQLUpdateFrame.GetData;
begin
  ComboBoxColumnListStyle.ItemIndex := SQLFormatterOptionsContainer.UpdateColumnListStyle;
end;

procedure TOptionsSQLUpdateFrame.PutData;
begin
  SQLFormatterOptionsContainer.UpdateColumnListStyle := ComboBoxColumnListStyle.ItemIndex;
end;

end.
