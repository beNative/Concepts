inherited frmSortStrings: TfrmSortStrings
  Left = 38
  Top = 38
  Margins.Left = 1
  Margins.Top = 1
  Margins.Right = 1
  Margins.Bottom = 1
  Caption = 'Sort strings'
  ClientHeight = 536
  ClientWidth = 430
  StyleElements = [seFont, seClient, seBorder]
  OnResize = FormResize
  ExplicitWidth = 446
  ExplicitHeight = 575
  TextHeight = 15
  object rgpSortDirection: TRadioGroup
    AlignWithMargins = True
    Left = 1
    Top = 1
    Width = 428
    Height = 42
    Margins.Left = 1
    Margins.Top = 1
    Margins.Right = 1
    Margins.Bottom = 1
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
    Top = 531
    Width = 430
    Height = 5
    Margins.Left = 1
    Margins.Top = 1
    Margins.Right = 1
    Margins.Bottom = 1
    Align = alBottom
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitTop = 514
    ExplicitWidth = 424
    DesignSize = (
      430
      5)
    object btnOK: TButton
      Left = 405
      Top = 0
      Width = 24
      Height = 5
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Action = actExecute
      Anchors = [akRight, akBottom]
      Caption = 'Sort selection'
      Default = True
      ModalResult = 1
      TabOrder = 0
      ExplicitLeft = 399
    end
  end
  object rgpSortScope: TRadioGroup
    AlignWithMargins = True
    Left = 1
    Top = 45
    Width = 428
    Height = 46
    Margins.Left = 1
    Margins.Top = 1
    Margins.Right = 1
    Margins.Bottom = 1
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
