inherited frmViewList: TfrmViewList
  Left = -916
  Top = 250
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Editor views'
  ClientHeight = 501
  ClientWidth = 806
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Segoe UI'
  Font.Style = []
  PopupMode = pmAuto
  Position = poOwnerFormCenter
  TextHeight = 23
  object pnlVST: TPanel
    Left = 0
    Top = 0
    Width = 806
    Height = 464
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 464
    Width = 806
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      806
      37)
    object btnClose: TButton
      Left = 676
      Top = 7
      Width = 120
      Height = 25
      Action = actClose
      Anchors = [akTop, akRight, akBottom]
      Default = True
      ModalResult = 11
      TabOrder = 0
    end
  end
  object aclMain: TActionList
    Left = 256
    Top = 129
    object actClose: TAction
      Caption = 'Close'
      OnExecute = actCloseExecute
    end
    object actCloseSelectedViews: TAction
      Caption = 'Close selected views'
      OnExecute = actCloseSelectedViewsExecute
    end
  end
  object ppmMain: TPopupMenu
    Left = 180
    Top = 129
    object mniClose: TMenuItem
      Action = actCloseSelectedViews
    end
  end
end
