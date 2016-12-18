inherited frmSortStrings: TfrmSortStrings
  Caption = 'Sort strings'
  ClientHeight = 394
  ClientWidth = 282
  OnResize = FormResize
  ExplicitWidth = 298
  ExplicitHeight = 433
  PixelsPerInch = 96
  TextHeight = 13
  object rgpSortDirection: TRadioGroup
    Left = 0
    Top = 0
    Width = 282
    Height = 54
    Align = alTop
    Caption = 'Sort direction:'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Ascending'
      'Descending')
    TabOrder = 0
    OnClick = rgpSortDirectionClick
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 369
    Width = 282
    Height = 25
    Align = alBottom
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      282
      25)
    object btnOK: TButton
      Left = 161
      Top = 0
      Width = 120
      Height = 25
      Action = actExecute
      Anchors = [akRight, akBottom]
      Caption = 'Sort selection'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
  object rgpSortScope: TRadioGroup
    Left = 0
    Top = 54
    Width = 282
    Height = 66
    Align = alTop
    Caption = 'Scope'
    Columns = 3
    ItemIndex = 0
    Items.Strings = (
      'Words'
      'Lines'
      'Paragraphs')
    TabOrder = 1
    OnClick = rgpSortScopeClick
  end
  object aclMain: TActionList
    Left = 296
    Top = 280
    object actExecute: TAction
      Caption = 'Execute'
      OnExecute = actExecuteExecute
    end
  end
end
