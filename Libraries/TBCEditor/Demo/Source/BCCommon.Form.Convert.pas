unit BCCommon.Form.Convert;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  BCControl.ComboBox, BCControl.Edit, Vcl.ActnList, sComboBox, System.Actions, sEdit;

type
  TConvertForm = class(TForm)
    ActionConvert: TAction;
    ActionList: TActionList;
    ActionReset: TAction;
    ComboBoxFrom: TBCComboBox;
    ComboBoxTo: TBCComboBox;
    ComboBoxType: TBCComboBox;
    EditResult: TBCEdit;
    EditValue: TBCEdit;
    procedure ActionConvertExecute(Sender: TObject);
    procedure ComboBoxTypeChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
  private
    function BinToHex(BinStr: string): string; overload;
    function HexToBin(HexStr: string): string; overload;
    procedure AddConvertFamilies;
    procedure AddConvertTypes(ComboBox: TBCComboBox); overload;
    procedure AddConvertTypes; overload;
    procedure BinToDec;
    procedure BinToHex; overload;
    procedure DecToBin;
    procedure DecToHex;
    procedure HexToBin; overload;
    procedure HexToDec;
    procedure ReadIniFile;
    procedure SameSame;
    procedure WriteIniFile;
  public
    procedure Open;
  end;

function ConvertForm(AOwner: TForm): TConvertForm;

implementation

{$R *.dfm}

uses
  System.IniFiles, BCCommon.FileUtils, System.ConvUtils, System.StdConvs, BCCommon.Language.Strings,
  BCCommon.Messages, BCCommon.Language.Utils, BCCommon.Math, BCCommon.Utils;

const
  DistanceItemIndex = 0;
  AreaItemIndex = 1;
  VolumeItemIndex = 2;
  MassItemIndex = 3;
  NumeralSystemItemIndex = 4;
  TempereatureItemIndex = 5;
  TimeItemIndex = 6;

var
  FConvertForm: TConvertForm;

function ConvertForm(AOwner: TForm): TConvertForm;
begin
  if not Assigned(FConvertForm) then
    FConvertForm := TConvertForm.Create(AOwner);
  Result := FConvertForm;
  UpdateLanguage(FConvertForm, GetSelectedLanguage);
end;

procedure TConvertForm.ActionConvertExecute(Sender: TObject);
begin
  if (ComboBoxType.ItemIndex = -1) or
     (ComboBoxFrom.ItemIndex = -1) or
     (ComboBoxTo.ItemIndex = -1) or
     (EditValue.Text = '') then
  begin
    EditResult.Text := '';
    Exit;
  end;
  try
    if ComboBoxType.ItemIndex <> NumeralSystemItemIndex then
      EditResult.Text := FloatToStr(System.ConvUtils.Convert(StrToFloat(EditValue.Text),
        TConvType(ComboBoxFrom.Items.Objects[ComboBoxFrom.ItemIndex]),
        TConvType(ComboBoxTo.Items.Objects[ComboBoxTo.ItemIndex])))
    else
    begin
      case ComboBoxFrom.ItemIndex of
        0: case ComboBoxTo.ItemIndex of
             0: SameSame;
             1: BinToDec;
             2: BinToHex;
           end;
        1: case ComboBoxTo.ItemIndex of
             0: DecToBin;
             1: SameSame;
             2: DecToHex;
           end;
        2: case ComboBoxTo.ItemIndex of
             0: HexToBin;
             1: HexToDec;
             2: SameSame;
           end;
      end;
    end;
  except
    EditResult.Text := '###';
  end;
end;

procedure TConvertForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WriteIniFile;
  Action := caFree;
end;

procedure TConvertForm.FormDestroy(Sender: TObject);
begin
  FConvertForm := nil;
  TForm(Owner).SetFocus;
end;

procedure TConvertForm.Open;
begin
  AddConvertFamilies;
  ReadIniFile;
  Show;
end;

procedure TConvertForm.AddConvertTypes;
begin
  AddConvertTypes(ComboBoxFrom);
  AddConvertTypes(ComboBoxTo);
end;

procedure TConvertForm.ComboBoxTypeChange(Sender: TObject);
begin
  AddConvertTypes;
  EditValue.Text := '';
  EditResult.Text := '';
end;

procedure TConvertForm.ReadIniFile;
begin
  with TMemIniFile.Create(GetIniFilename) do
  try
    { Position }
    Left := ReadInteger('ConvertPosition', 'Left', (Screen.Width - Width) div 2);
    Top := ReadInteger('ConvertPosition', 'Top', (Screen.Height - Height) div 2);
    { Check if the form is outside the workarea }
    Left := SetFormInsideWorkArea(Left, Width);
    ComboBoxType.ItemIndex := ReadInteger('ConvertPosition', 'TypeItemIndex', -1);
    AddConvertTypes;
    ComboBoxFrom.ItemIndex := ReadInteger('ConvertPosition', 'FromItemIndex', -1);
    ComboBoxTo.ItemIndex := ReadInteger('ConvertPosition', 'ToItemIndex', -1);
    EditValue.Text := ReadString('ConvertPosition', 'Value', '');
    EditResult.Text := ReadString('ConvertPosition', 'Result', '');
  finally
    Free;
  end;
end;

procedure TConvertForm.WriteIniFile;
begin
  if Windowstate = wsNormal then
  with TMemIniFile.Create(GetIniFilename) do
  try
    { Position }
    WriteInteger('ConvertPosition', 'Left', Left);
    WriteInteger('ConvertPosition', 'Top', Top);
    WriteInteger('ConvertPosition', 'TypeItemIndex', ComboBoxType.ItemIndex);
    WriteInteger('ConvertPosition', 'FromItemIndex', ComboBoxFrom.ItemIndex);
    WriteInteger('ConvertPosition', 'ToItemIndex', ComboBoxTo.ItemIndex);
    WriteString('ConvertPosition', 'Value', EditValue.Text);
    WriteString('ConvertPosition', 'Result', EditResult.Text);
  finally
    UpdateFile;
    Free;
  end;
end;

procedure TConvertForm.AddConvertFamilies;
begin
  with ComboBoxType.Items do
  begin
    Clear;
    Add(LanguageDataModule.GetConvertConstant('Distance'));
    Add(LanguageDataModule.GetConvertConstant('Area'));
    Add(LanguageDataModule.GetConvertConstant('Volume'));
    Add(LanguageDataModule.GetConvertConstant('Mass'));
    Add(LanguageDataModule.GetConvertConstant('NumeralSystem'));
    Add(LanguageDataModule.GetConvertConstant('Temperature'));
    Add(LanguageDataModule.GetConvertConstant('Time'));
  end;
end;

procedure TConvertForm.AddConvertTypes(ComboBox: TBCComboBox);
var
  i: Integer;
  LTypes: TConvTypeArray;
  ConvFamily: TConvFamily;
begin
  ConvFamily := 0;
  with ComboBox.Items do
  begin
    Clear;
    case ComboBoxType.ItemIndex of
      DistanceItemIndex: ConvFamily := cbDistance;
      AreaItemIndex: ConvFamily := cbArea;
      VolumeItemIndex: ConvFamily := cbVolume;
      MassItemIndex: ConvFamily := cbMass;
      NumeralSystemItemIndex:
        begin
          Add(LanguageDataModule.GetConvertConstant('Binary'));
          Add(LanguageDataModule.GetConvertConstant('Decimal'));
          Add(LanguageDataModule.GetConvertConstant('Hexadecimal'));
        end;
      TempereatureItemIndex: ConvFamily := cbTemperature;
      TimeItemIndex: ConvFamily := cbTime;
    end;

    if ComboBoxType.ItemIndex <> NumeralSystemItemIndex then
    begin
      GetConvTypes(ConvFamily, LTypes);
      for i := 0 to Length(LTypes) - 1 do
        AddObject(LanguageDataModule.GetConvertConstant(ConvTypeToDescription(LTypes[i])), TObject(LTypes[i]));
    end;
  end;
  ComboBox.DropDownCount := ComboBox.Items.Count;
end;

procedure TConvertForm.DecToHex;
begin
  EditResult.Text := IntToHex(StrToInt(EditValue.Text), 2);
end;

procedure TConvertForm.HexToDec;
begin
  EditResult.Text := IntToStr(StrToInt('$' + EditValue.Text));
end;

procedure TConvertForm.DecToBin;
begin
  EditResult.Text := IntToBin(StrToInt(EditValue.Text), Length(EditValue.Text) * 4);
end;

procedure TConvertForm.BinToDec;
begin
  EditResult.Text := IntToStr(BinToInt(EditValue.Text));
end;

function TConvertForm.HexToBin(HexStr: string): string;
const
  BinArray: array[0..15, 0..1] of string =
    (('0000', '0'), ('0001', '1'), ('0010', '2'), ('0011', '3'),
     ('0100', '4'), ('0101', '5'), ('0110', '6'), ('0111', '7'),
     ('1000', '8'), ('1001', '9'), ('1010', 'A'), ('1011', 'B'),
     ('1100', 'C'), ('1101', 'D'), ('1110', 'E'), ('1111', 'F'));
  HexAlpha: set of AnsiChar = ['0'..'9', 'A'..'F'];
var
  i, j: Integer;
begin
  Result := '';
  HexStr := AnsiUpperCase(HexStr);
  for i:= 1 to Length(HexStr) do
    if CharInSet(HexStr[i], HexAlpha) then
    begin
      for j := 1 to 16 do
        if HexStr[i] = BinArray[j - 1, 1] then
          Result := Result + BinArray[j - 1, 0];
    end
    else
    begin
      Result := '';
      ShowErrorMessage('This is not hexadecimal number.');
      Exit;
    end;
  if Result <> '' then
    while (Result[1] = '0') and (Length(Result) > 1) do
      Delete(Result, 1, 1);
end;

procedure TConvertForm.HexToBin;
begin
  EditResult.Text := HexToBin(EditValue.Text);
end;

function TConvertForm.BinToHex(BinStr: string): string;
const
  BinArray: array[0..15, 0..1] of string =
    (('0000', '0'), ('0001', '1'), ('0010', '2'), ('0011', '3'),
     ('0100', '4'), ('0101', '5'), ('0110', '6'), ('0111', '7'),
     ('1000', '8'), ('1001', '9'), ('1010', 'A'), ('1011', 'B'),
     ('1100', 'C'), ('1101', 'D'), ('1110', 'E'), ('1111', 'F'));
var
  j: Integer;
  BinPart: string;
begin
  Result := '';

  for j := 1 to Length(BinStr) do
    if not CharInSet(BinStr[j], ['0', '1']) then
    begin
      ShowErrorMessage('This is not binary number');
      Exit;
    end;

  case Length(BinStr) mod 4 of
    1: BinStr := '000'+BinStr;
    2: BinStr := '00'+BinStr;
    3: BinStr := '0'+BinStr;
  end;

  while Length(BinStr) > 0 do
  begin
    BinPart := Copy(BinStr, Length(BinStr) - 3, 4);
    Delete(BinStr, Length(BinStr) - 3, 4);
    for j := 1 to 16 do
      if BinPart = BinArray[j - 1, 0] then
        Result := BinArray[j - 1, 1] + Result;
  end;
end;

procedure TConvertForm.BinToHex;
begin
  EditResult.Text := BinToHex(EditValue.Text);
end;

procedure TConvertForm.SameSame;
begin
  EditResult.Text := EditValue.Text;
end;

end.
