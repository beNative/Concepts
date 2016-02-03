inherited FindInFilesDialog: TFindInFilesDialog
  Caption = 'Find in Files'
  ClientHeight = 296
  ClientWidth = 499
  Padding.Left = 6
  Padding.Top = 6
  Padding.Right = 6
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ActionList: TActionList
    Left = 378
    Top = 6
    object ActionTextToFindItemsButtonClick: TAction
      OnExecute = ActionTextToFindItemsButtonClickExecute
    end
    object ActionFileMaskItemsButtonClick: TAction
      OnExecute = ActionFileMaskItemsButtonClickExecute
    end
    object ActionDirectoryButtonClick: TAction
      ImageIndex = 0
      OnExecute = ActionDirectoryButtonClickExecute
    end
    object ActionDirectoryItemsButtonClick: TAction
      OnExecute = ActionDirectoryItemsButtonClickExecute
    end
  end
end
