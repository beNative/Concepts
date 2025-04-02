object frmMain: TfrmMain
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Concepts'
  ClientHeight = 843
  ClientWidth = 1200
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  ShowHint = True
  PixelsPerInch = 144
  TextHeight = 23
  object pnlVST: TPanel
    Left = 0
    Top = 0
    Width = 1200
    Height = 764
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object edtFilter: TEdit
      AlignWithMargins = True
      Left = 5
      Top = 5
      Width = 1190
      Height = 26
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alTop
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -17
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnChange = edtFilterChange
      OnKeyDown = edtFilterKeyDown
      OnKeyUp = edtFilterKeyUp
    end
  end
  object sbrMain: TStatusBar
    Left = 0
    Top = 815
    Width = 1200
    Height = 28
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Panels = <>
    SimplePanel = True
  end
  object pnlButtons: TGridPanel
    Left = 0
    Top = 764
    Width = 1200
    Height = 51
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alBottom
    BevelOuter = bvNone
    ColumnCollection = <
      item
        Value = 34.297279594373700000
      end
      item
        Value = 33.023156756930660000
      end
      item
        Value = 32.679563648695640000
      end>
    ControlCollection = <
      item
        Column = 1
        Control = btnExecute
        Row = 0
      end
      item
        Column = 2
        Control = btnClose
        Row = 0
      end
      item
        Column = 0
        Control = btnExecuteModal
        Row = 0
      end>
    RowCollection = <
      item
        Value = 100.000000000000000000
      end>
    TabOrder = 2
    object btnExecute: TButton
      AlignWithMargins = True
      Left = 417
      Top = 5
      Width = 386
      Height = 41
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actExecute
      Align = alClient
      DoubleBuffered = True
      ImageMargins.Left = 6
      Images = dmResources.imlMain
      ParentDoubleBuffered = False
      TabOrder = 0
    end
    object btnClose: TButton
      AlignWithMargins = True
      Left = 813
      Top = 5
      Width = 382
      Height = 41
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actClose
      Align = alClient
      DoubleBuffered = True
      ImageMargins.Left = 6
      Images = dmResources.imlMain
      ParentDoubleBuffered = False
      TabOrder = 1
    end
    object btnExecuteModal: TButton
      AlignWithMargins = True
      Left = 5
      Top = 5
      Width = 402
      Height = 41
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = actExecuteModal
      Align = alClient
      Default = True
      DoubleBuffered = True
      ImageMargins.Left = 6
      Images = dmResources.imlMain
      ParentDoubleBuffered = False
      TabOrder = 2
    end
  end
  object aclMain: TActionList
    Images = dmResources.imlMain
    Left = 24
    Top = 40
    object actExecute: TAction
      Caption = 'Execute'
      ImageIndex = 519
      OnExecute = actExecuteExecute
    end
    object actClose: TAction
      Caption = 'Close'
      ImageIndex = 339
      OnExecute = actCloseExecute
    end
    object actExecuteModal: TAction
      Caption = 'Execute modal'
      ImageIndex = 518
      OnExecute = actExecuteModalExecute
    end
    object actCenterMainForm: TAction
      Caption = 'Center form'
      Hint = 'Center main form on the screen.'
      ImageIndex = 38
      OnExecute = actCenterMainFormExecute
    end
  end
  object tbrMain: TTaskbar
    TaskBarButtons = <
      item
        Action = actCenterMainForm
        ButtonState = [Enabled, NoBackground]
        Hint = 'Center main form on the screen.'
        Icon.Data = {
          0000010001001010200000000000680400001600000028000000100000002000
          0000010020000000000000040000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000000C290
          6A9BC28D66FFBF8A64FFBD8762FFBA845FFFB8825DFFB57E5CFFB37C5AFFB17A
          58FFB07956FFAD7755FFAC7454FFAA7352FFA87151FFA86F4FFFA16C509BC891
          6AFFCECECEFFD6D6D6FFD4D4D4FFD2D2D2FFD0D0D0FFCDCDCDFFCBCBCBFFCACA
          CAFFC8C8C8FFC5C5C5FFC4C4C4FFC2C2C2FFC1C1C1FFAEAEAEFFA8704FFFCA93
          6CFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA97151FFCC96
          6DFFFFFFFFFFF7F7F7FFF5F5F5FFF4F4F4FFF3F3F3FFF2F2F2FFF2F2F2FFF2F2
          F2FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2F2FFFFFFFFFFAB7352FFCF99
          70FFFFFFFFFFFAFAFAFFF8F8F8FFF7F7F7FFF5F5F5FFF4F4F4FFF3F3F3FFF2F2
          F2FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2F2FFFFFFFFFFAC7654FFD19B
          71FFFFFFFFFFFCFCFCFFFBFBFBFFFAFAFAFFF8F8F8FFF7F7F7FFF5F5F5FFF4F4
          F4FFF3F3F3FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2F2FFFFFFFFFFAF7856FFD49D
          73FFFFFFFFFFFDFDFDFFFDFDFDFFFCFCFCFFFBFBFBFFFAFAFAFFF8F8F8FFF7F7
          F7FFF5F5F5FFF4F4F4FFF3F3F3FFF2F2F2FFF2F2F2FFFFFFFFFFB17A58FFD59F
          74FFFFFFFFFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFCFCFCFFFBFBFBFFFAFA
          FAFFF8F8F8FFF7F7F7FFF5F5F5FFF4F4F4FFF3F3F3FFFFFFFFFFB47C5AFFD8A1
          77FFFFFFFFFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFCFC
          FCFFFBFBFBFFFAFAFAFFF8F8F8FFF7F7F7FFF5F5F5FFFFFFFFFFB6805CFFD9A2
          77FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB9845EFFDBA3
          78FFD4D4D4FFD4D4D4FFD4D4D4FFD4D4D4FFD4D4D4FFD4D4D4FFD4D4D4FFD4D4
          D4FFD4D4D4FFD4D4D4FFD4D4D4FFD3D3D3FFD3D3D3FFD3D3D3FFBC8661FFDCA6
          79FFDBA378FFDAA277FFD8A177FFD7A076FFD59E74FFD39D72FFD19B71FFCF99
          70FFCD966EFFCB946CFFC9936AFFC79069FFC38E67FFC28C65FFBF8A64FFDCAA
          84FDF1DCCEFFEAC09FFFE8B891FFE8B891FFE8B891FFE8B891FFE8B891FFE8B8
          91FFCDC8C4FFE8B891FFCDC8C4FFE8B891FF4262FFFFE8C3A6FFBF8F6CFDDCAA
          84C2DCAF8CF4DCA679FFDCA578FFDAA378FFD8A177FFD8A077FFD59F74FFD49D
          73FFD29C71FFCF9970FFCE986EFFCB956DFFC9936AFFC29878F4C2916EC20000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000000}
      end>
    TabProperties = [AppThumbAlways, AppPeekAlways]
    Left = 88
    Top = 40
  end
end
