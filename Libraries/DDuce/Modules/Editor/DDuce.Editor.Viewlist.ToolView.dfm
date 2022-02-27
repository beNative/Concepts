inherited frmViewList: TfrmViewList
  Left = -916
  Top = 250
  Caption = 'Editor views'
  ClientHeight = 501
  ClientWidth = 800
  PopupMode = pmAuto
  Position = poOwnerFormCenter
  ExplicitWidth = 816
  ExplicitHeight = 540
  PixelsPerInch = 96
  TextHeight = 13
  object pnlVST: TPanel
    Left = 0
    Top = 0
    Width = 800
    Height = 464
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 1
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 464
    Width = 800
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      800
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
