object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Getting Started'
  ClientHeight = 395
  ClientWidth = 549
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = ViewMainMenu
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ProductsListView: TListView
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 543
    Height = 370
    Align = alClient
    Columns = <
      item
        AutoSize = True
        Caption = 'Name'
      end
      item
        Caption = 'Price'
        Width = 75
      end
      item
        Caption = 'Quantity'
        Width = 80
      end>
    GridLines = True
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnData = ProductsListViewData
    OnDblClick = ProductsListViewDblClick
  end
  object ViewStatusBar: TStatusBar
    Left = 0
    Top = 376
    Width = 549
    Height = 19
    Panels = <
      item
        Width = 250
      end
      item
        Width = 50
      end>
  end
  object ViewMainMenu: TMainMenu
    Left = 480
    Top = 272
    object DatabaseMenuItem: TMenuItem
      Caption = 'Database'
      object RebuildDatabaseMenuItem: TMenuItem
        Action = RebuildDatabaseAction
      end
      object N1MenuItem: TMenuItem
        Caption = '-'
      end
      object CommitChangesMenuItem: TMenuItem
        Action = CommitAction
      end
    end
    object ProductsMenuItem: TMenuItem
      Caption = 'Products'
      object ProductAddMenuItem: TMenuItem
        Action = ProductAddAction
        ShortCut = 16462
      end
      object ProductEditMenuItem: TMenuItem
        Action = ProductEditAction
        ShortCut = 16453
      end
      object N2MenuItem: TMenuItem
        Caption = '-'
      end
      object ProductRemoveMenuItem: TMenuItem
        Action = ProductRemoveAction
        ShortCut = 16430
      end
      object N3MenuItem: TMenuItem
        Caption = '-'
      end
      object ProductsRefreshMenuItem: TMenuItem
        Action = ProductsRefreshAction
        ShortCut = 116
      end
    end
  end
  object MainActionList: TActionList
    Left = 384
    Top = 272
    object RebuildDatabaseAction: TAction
      Category = 'Database'
      Caption = 'Rebuild Database'
      OnExecute = RebuildDatabaseActionExecute
    end
    object ProductAddAction: TAction
      Category = 'Products'
      Caption = 'Add New Product'
      OnExecute = ProductAddActionExecute
    end
    object ProductRemoveAction: TAction
      Category = 'Products'
      Caption = 'Remove Product'
      OnExecute = ProductRemoveActionExecute
      OnUpdate = ProductEditActionUpdate
    end
    object ProductEditAction: TAction
      Category = 'Products'
      Caption = 'Edit Product'
      OnExecute = ProductEditActionExecute
      OnUpdate = ProductEditActionUpdate
    end
    object ProductsRefreshAction: TAction
      Category = 'Products'
      Caption = 'Load Products'
      OnExecute = ProductsRefreshActionExecute
    end
    object CommitAction: TAction
      Category = 'Database'
      Caption = 'Commit Changes'
      ShortCut = 16467
      OnExecute = CommitActionExecute
    end
  end
end
