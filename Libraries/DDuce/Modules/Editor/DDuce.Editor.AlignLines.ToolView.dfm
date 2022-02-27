inherited frmAlignLines: TfrmAlignLines
  Left = -521
  Top = 253
  Caption = 'Align selection'
  ClientHeight = 729
  ClientWidth = 360
  Constraints.MinWidth = 170
  OnResize = FormResize
  OnShow = FormShow
  ExplicitWidth = 376
  ExplicitHeight = 768
  PixelsPerInch = 96
  TextHeight = 13
  object sbrMain: TScrollBox
    Left = 0
    Top = 0
    Width = 360
    Height = 704
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    TabOrder = 0
    DesignSize = (
      360
      704)
    object rgpSortDirection: TRadioGroup
      Left = 5
      Top = 164
      Width = 352
      Height = 54
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
      Left = 5
      Top = 222
      Width = 352
      Height = 54
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
    Top = 704
    Width = 360
    Height = 25
    Align = alBottom
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      360
      25)
    object btnOK: TButton
      Left = 240
      Top = 0
      Width = 120
      Height = 25
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
