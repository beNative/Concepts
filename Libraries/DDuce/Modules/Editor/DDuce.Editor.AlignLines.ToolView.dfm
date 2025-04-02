inherited frmAlignLines: TfrmAlignLines
  Left = 38
  Top = 38
  Margins.Left = 1
  Margins.Top = 1
  Margins.Right = 1
  Margins.Bottom = 1
  Caption = 'Align selection'
  ClientHeight = 384
  ClientWidth = 275
  Color = clBtnFace
  Constraints.MinWidth = 75
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnResize = FormResize
  OnShow = FormShow
  TextHeight = 15
  object sbrMain: TScrollBox
    Left = 0
    Top = 0
    Width = 275
    Height = 360
    Margins.Left = 1
    Margins.Top = 1
    Margins.Right = 1
    Margins.Bottom = 1
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    TabOrder = 0
    DesignSize = (
      275
      360)
    object rgpSortDirection: TRadioGroup
      Left = 2
      Top = 56
      Width = 269
      Height = 41
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Sort direction:'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'Ascending'
        'Descending')
      TabOrder = 1
      OnClick = rgpSortDirectionClick
    end
    object rgpAlignAt: TRadioGroup
      Left = 2
      Top = 99
      Width = 269
      Height = 38
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Align at:'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'Leftmost token'
        'Rightmost token')
      TabOrder = 0
      Visible = False
      OnClick = rgpAlignAtClick
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 360
    Width = 275
    Height = 24
    Margins.Left = 1
    Margins.Top = 1
    Margins.Right = 1
    Margins.Bottom = 1
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      275
      24)
    object btnOK: TButton
      Left = 207
      Top = 3
      Width = 67
      Height = 18
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Action = actExecute
      Anchors = [akRight, akBottom]
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
  object aclMain: TActionList
    Left = 152
    Top = 472
    object actExecute: TAction
      Caption = 'Execute'
      OnExecute = actExecuteExecute
    end
  end
end
