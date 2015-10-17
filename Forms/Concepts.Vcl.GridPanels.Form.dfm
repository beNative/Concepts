object frmGridPanels: TfrmGridPanels
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Gridpanels'
  ClientHeight = 153
  ClientWidth = 536
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pnlGrid: TGridPanel
    Left = 8
    Top = 8
    Width = 273
    Height = 137
    BevelOuter = bvLowered
    ColumnCollection = <
      item
        Value = 50.000000000000000000
      end
      item
        Value = 50.000000000000000000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = pnl1
        Row = 0
      end
      item
        Column = 1
        Control = pnl2
        Row = 0
      end
      item
        Column = 0
        Control = pnl3
        Row = 1
      end
      item
        Column = 1
        Control = pnl4
        Row = 1
      end
      item
        Column = 0
        Control = pnl5
        Row = 2
      end
      item
        Column = 1
        Control = pnl6
        Row = 2
      end
      item
        Column = 0
        Control = pnl7
        Row = 3
      end
      item
        Column = 1
        Control = pnl8
        Row = 3
      end>
    RowCollection = <
      item
        Value = 25.059277018580270000
      end
      item
        Value = 25.076517979261560000
      end
      item
        Value = 24.835050526963980000
      end
      item
        Value = 25.029154475194180000
      end>
    TabOrder = 0
    object pnl1: TPanel
      Left = 1
      Top = 1
      Width = 135
      Height = 33
      Align = alClient
      Caption = 'pnl1'
      TabOrder = 0
    end
    object pnl2: TPanel
      Left = 136
      Top = 1
      Width = 136
      Height = 33
      Align = alClient
      Caption = 'pnl2'
      TabOrder = 1
    end
    object pnl3: TPanel
      Left = 1
      Top = 34
      Width = 135
      Height = 33
      Align = alClient
      Caption = 'pnl3'
      TabOrder = 2
    end
    object pnl4: TPanel
      Left = 136
      Top = 34
      Width = 136
      Height = 33
      Align = alClient
      Caption = 'pnl4'
      TabOrder = 3
    end
    object pnl5: TPanel
      Left = 1
      Top = 67
      Width = 135
      Height = 33
      Align = alClient
      Caption = 'pnl5'
      TabOrder = 4
    end
    object pnl6: TPanel
      Left = 136
      Top = 67
      Width = 136
      Height = 33
      Align = alClient
      Caption = 'pnl6'
      TabOrder = 5
    end
    object pnl7: TPanel
      Left = 1
      Top = 100
      Width = 135
      Height = 36
      Align = alClient
      Caption = 'pnl7'
      TabOrder = 6
    end
    object pnl8: TPanel
      Left = 136
      Top = 100
      Width = 136
      Height = 36
      Align = alClient
      Caption = 'pnl8'
      TabOrder = 7
    end
  end
  object chk1: TCheckBox
    Left = 304
    Top = 9
    Width = 97
    Height = 17
    Caption = 'chk1'
    Checked = True
    State = cbChecked
    TabOrder = 1
  end
  object chk2: TCheckBox
    Left = 424
    Top = 9
    Width = 97
    Height = 17
    Caption = 'chk2'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object chk3: TCheckBox
    Left = 304
    Top = 32
    Width = 97
    Height = 17
    Caption = 'chk3'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object chk4: TCheckBox
    Left = 424
    Top = 32
    Width = 97
    Height = 17
    Caption = 'chk4'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object chk5: TCheckBox
    Left = 304
    Top = 57
    Width = 97
    Height = 13
    Caption = 'chk5'
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
  object chk6: TCheckBox
    Left = 424
    Top = 55
    Width = 97
    Height = 17
    Caption = 'chk6'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object chk7: TCheckBox
    Left = 304
    Top = 78
    Width = 97
    Height = 17
    Caption = 'chk7'
    Checked = True
    State = cbChecked
    TabOrder = 7
  end
  object chk8: TCheckBox
    Left = 424
    Top = 78
    Width = 97
    Height = 17
    Caption = 'chk8'
    Checked = True
    State = cbChecked
    TabOrder = 8
  end
  object btnUpdateLayout: TButton
    Left = 304
    Top = 120
    Width = 217
    Height = 25
    Caption = 'Update layout'
    Default = True
    TabOrder = 9
    OnClick = btnUpdateLayoutClick
  end
end
