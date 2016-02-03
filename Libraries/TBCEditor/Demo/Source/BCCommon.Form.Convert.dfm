object ConvertForm: TConvertForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Convert Between Numerical Units'
  ClientHeight = 150
  ClientWidth = 400
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Icon.Data = {
    0000010001001010000001002000280400001600000028000000100000002000
    0000010020000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000B88444FF00000000000000000000
    0000000000000000000000000000579870FF579870FF00000000000000000000
    00000000000000000000C4A988FFC5975EFFDFAC65FFB78241FF000000000000
    0000000000006EA384FF579870FF579870FF579870FF579870FF84AC93FF0000
    000000000000BB8C52FFCA964FFFE4B165FFF4C177FFE5DCB0FFBB8342FF0000
    000000000000559871FF90BBA0FF000000000000000073A586FF579870FF0000
    0000C2A37CFFBF8946FFD09D55FFC89F6EFFE1AC65FFB98140FF000000000000
    000000000000000000000000000000000000000000005D9B75FF579870FF0000
    0000BB8646FFDAB37EFF0000000000000000C1843DFF00000000000000000000
    000000000000BDC6C0FF6BA180FF579870FF579870FF65A07CFF000000000000
    0000C08641FF0000000000000000000000000000000000000000000000000000
    00000000000057986FFF579870FF000000000000000000000000000000000000
    00000000000000000000429ACCFF429ACCFF419ACEFF439ACCFF8EB5CCFF0000
    00000000000057986EFF5F9C76FF000000000000000000000000579870FF0000
    000000000000439ACCFF4699C8FF4799C8FF5FA7D0FF4799C7FF4799C9FF0000
    000000000000A5C8B2FF569870FF579870FF579870FF569870FF5E9D78FF0000
    000060A3CAFF4799C8FF75ABC9FF000000000000000000000000000000000000
    0000000000000000000000000000539872FF529872FF00000000000000004799
    C7FF4799C7FF4799C7FF4799C7FF4799C7FF4799C7FF00000000000000000000
    0000000000000000000000000000000000000000000000000000C68442FF0000
    00004799C7FF4799C7FF00000000000000000000000000000000000000000000
    00000000000000000000BE8342FF0000000000000000CBA472FFBC8646FF4799
    C7FF4799C7FF4799C7FF4799C7FF4799C7FF4799C7FF4799C7FF000000000000
    000000000000B9813EFFE2AF67FFC89E6CFFCF9C57FFBD8947FFC59864FF0000
    00004799C7FF4799C7FF6CA7C8FF000000000000000000000000000000000000
    0000C58235FFF0DABBFFF7C67AFFE7B56AFFCA9750FFB98546FF000000000000
    0000000000004799C7FF4799C7FF4799C7FF4799C7FF4799C7FF4E9DCAFF0000
    000000000000B9813FFFE2B068FFC0935DFFD0AE83FF00000000000000000000
    000000000000000000004B9BC8FF4799C7FF4799C7FF4799C7FF8BBFDDFF0000
    00000000000000000000B98545FF00000000000000000000000000000000}
  OldCreateOrder = False
  OnClose = FormClose
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object EditValue: TBCEdit
    Left = 9
    Top = 114
    Width = 186
    Height = 21
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 3
    OnChange = ActionConvertExecute
    SkinData.SkinSection = 'EDIT'
    BoundLabel.Active = True
    BoundLabel.Caption = 'Value'
    BoundLabel.Indent = 4
    BoundLabel.Layout = sclTopLeft
    EnterToTab = False
    OnlyNumbers = False
    NumbersWithDots = False
    NumbersWithSpots = False
    ErrorColor = 14803198
    NumbersAllowMinus = False
    NumbersAllowPlus = False
  end
  object EditResult: TBCEdit
    Left = 203
    Top = 114
    Width = 186
    Height = 21
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabStop = False
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 4
    SkinData.SkinSection = 'EDIT'
    BoundLabel.Active = True
    BoundLabel.Caption = 'Result'
    BoundLabel.Indent = 4
    BoundLabel.Layout = sclTopLeft
    EnterToTab = False
    OnlyNumbers = False
    NumbersWithDots = False
    NumbersWithSpots = False
    ErrorColor = 14803198
    NumbersAllowMinus = False
    NumbersAllowPlus = False
  end
  object ComboBoxType: TBCComboBox
    Left = 9
    Top = 24
    Width = 186
    Height = 21
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Alignment = taLeftJustify
    BoundLabel.Active = True
    BoundLabel.Caption = 'Type'
    BoundLabel.Indent = 4
    BoundLabel.Layout = sclTopLeft
    DropDownCount = 9
    SkinData.SkinSection = 'COMBOBOX'
    VerticalAlignment = taAlignTop
    Style = csDropDownList
    ItemIndex = -1
    TabOrder = 0
    OnChange = ComboBoxTypeChange
    UseMouseWheel = False
  end
  object ComboBoxFrom: TBCComboBox
    Left = 9
    Top = 68
    Width = 186
    Height = 21
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Alignment = taLeftJustify
    BoundLabel.Active = True
    BoundLabel.Caption = 'From'
    BoundLabel.Indent = 4
    BoundLabel.Layout = sclTopLeft
    DropDownCount = 9
    SkinData.SkinSection = 'COMBOBOX'
    VerticalAlignment = taAlignTop
    Style = csDropDownList
    ItemIndex = -1
    TabOrder = 1
    OnChange = ActionConvertExecute
    UseMouseWheel = False
  end
  object ComboBoxTo: TBCComboBox
    Left = 203
    Top = 68
    Width = 186
    Height = 21
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Alignment = taLeftJustify
    BoundLabel.Active = True
    BoundLabel.Caption = 'To'
    BoundLabel.Indent = 4
    BoundLabel.Layout = sclTopLeft
    DropDownCount = 9
    SkinData.SkinSection = 'COMBOBOX'
    VerticalAlignment = taAlignTop
    Style = csDropDownList
    ItemIndex = -1
    TabOrder = 2
    OnChange = ActionConvertExecute
    UseMouseWheel = False
  end
  object ActionList: TActionList
    Left = 96
    Top = 104
    object ActionConvert: TAction
      Caption = '&Convert'
      Hint = 'Convert value to selected unit'
      OnExecute = ActionConvertExecute
    end
    object ActionReset: TAction
      Caption = '&Reset'
      Hint = 'Reset fields'
    end
  end
end
