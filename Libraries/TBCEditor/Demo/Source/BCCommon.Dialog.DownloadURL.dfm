inherited DownloadURLDialog: TDownloadURLDialog
  BorderStyle = bsToolWindow
  Caption = 'Download'
  ClientHeight = 116
  ClientWidth = 364
  Color = clActiveBorder
  Position = poMainFormCenter
  Visible = True
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ActionList: TActionList
    Left = 300
    Top = 20
    object ActionCancel: TAction
      Caption = 'Cancel'
      OnExecute = ActionCancelExecute
    end
    object ActionOK: TAction
      Caption = '&OK'
      OnExecute = ActionOKExecute
    end
  end
end
