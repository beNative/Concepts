inherited OptionsToolbarFrame: TOptionsToolbarFrame
  Width = 451
  Height = 305
  Align = alClient
  object Panel: TBCPanel [0]
    Left = 0
    Top = 0
    Width = 451
    Height = 305
    Align = alClient
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    TabOrder = 0
    SkinData.SkinSection = 'CHECKBOX'
    object VirtualDrawTree: TVirtualDrawTree
      AlignWithMargins = True
      Left = 4
      Top = 62
      Width = 443
      Height = 217
      Hint = 
        'Use drag and drop to move menu items. Right click popup menu to ' +
        'insert and delete items.'
      Margins.Left = 4
      Margins.Top = 0
      Margins.Right = 4
      Margins.Bottom = 0
      Align = alClient
      Ctl3D = True
      DragOperations = []
      EditDelay = 0
      Header.AutoSizeIndex = 0
      Header.DefaultHeight = 20
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'Tahoma'
      Header.Font.Style = []
      Header.Height = 20
      Header.Options = [hoAutoResize, hoShowSortGlyphs, hoVisible, hoAutoSpring]
      Images = ImagesDataModule.ImageListSmall
      Indent = 0
      ParentCtl3D = False
      PopupMenu = PopupActionBar
      SelectionBlendFactor = 255
      TabOrder = 0
      TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toDisableAutoscrollOnFocus]
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
      TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowRoot, toThemeAware]
      TreeOptions.SelectionOptions = [toFullRowSelect, toMiddleClickSelect, toAlwaysSelectNode]
      WantTabs = True
      OnDragAllowed = VirtualDrawTreeDragAllowed
      OnDragOver = VirtualDrawTreeDragOver
      OnDragDrop = VirtualDrawTreeDragDrop
      OnDrawNode = VirtualDrawTreeDrawNode
      OnFreeNode = VirtualDrawTreeFreeNode
      OnGetImageIndex = VirtualDrawTreeGetImageIndex
      OnGetNodeWidth = VirtualDrawTreeGetNodeWidth
      Columns = <
        item
          Options = [coEnabled, coParentBidiMode, coParentColor, coVisible, coAutoSpring]
          Position = 0
          Width = 439
          WideText = 'Menu Item'
        end>
    end
    object PanelButtons: TBCPanel
      Left = 0
      Top = 0
      Width = 451
      Height = 62
      Align = alTop
      BevelOuter = bvNone
      Padding.Left = 2
      Padding.Top = 2
      Padding.Right = 2
      Padding.Bottom = 2
      ParentColor = True
      TabOrder = 1
      SkinData.SkinSection = 'CHECKBOX'
      object SpeedButtonDivider1: TBCSpeedButton
        AlignWithMargins = True
        Left = 182
        Top = 6
        Width = 10
        Height = 50
        Margins.Left = 0
        Margins.Top = 4
        Margins.Right = 0
        Margins.Bottom = 4
        Align = alLeft
        Flat = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Tahoma'
        Font.Style = []
        Layout = blGlyphTop
        ParentFont = False
        ButtonStyle = tbsDivider
        SkinData.SkinSection = 'SPEEDBUTTON'
        ImageIndex = 1
      end
      object SpeedButtonDivider2: TBCSpeedButton
        AlignWithMargins = True
        Left = 252
        Top = 6
        Width = 10
        Height = 50
        Margins.Left = 0
        Margins.Top = 4
        Margins.Right = 0
        Margins.Bottom = 4
        Align = alLeft
        Flat = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Tahoma'
        Font.Style = []
        Layout = blGlyphTop
        ParentFont = False
        ButtonStyle = tbsDivider
        SkinData.SkinSection = 'SPEEDBUTTON'
        ImageIndex = 1
      end
      object SpeedButtonDelete: TBCSpeedButton
        Left = 122
        Top = 2
        Width = 60
        Height = 58
        Action = ActionDelete
        Align = alLeft
        AllowAllUp = True
        Flat = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Segoe UI'
        Font.Style = []
        Layout = blGlyphTop
        ParentFont = False
        ParentShowHint = False
        ShowHint = False
        ButtonStyle = tbsCheck
        SkinData.SkinSection = 'TOOLBUTTON'
        Images = ImagesDataModule.ImageList
        ImageIndex = 22
      end
      object SpeedButtonAddDivider: TBCSpeedButton
        Left = 62
        Top = 2
        Width = 60
        Height = 58
        Action = ActionAddDivider
        Align = alLeft
        AllowAllUp = True
        Flat = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Segoe UI'
        Font.Style = []
        Layout = blGlyphTop
        ParentFont = False
        ParentShowHint = False
        ShowHint = False
        ButtonStyle = tbsCheck
        SkinData.SkinSection = 'TOOLBUTTON'
        Images = ImagesDataModule.ImageList
        ImageIndex = 127
      end
      object SpeedButtonAddItem: TBCSpeedButton
        Left = 2
        Top = 2
        Width = 60
        Height = 58
        Action = ActionAddItem
        Align = alLeft
        AllowAllUp = True
        Flat = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Segoe UI'
        Font.Style = []
        Layout = blGlyphTop
        ParentFont = False
        ParentShowHint = False
        ShowHint = False
        ButtonStyle = tbsCheck
        SkinData.SkinSection = 'TOOLBUTTON'
        Images = ImagesDataModule.ImageList
        ImageIndex = 18
      end
      object SpeedButtonReset: TBCSpeedButton
        Left = 192
        Top = 2
        Width = 60
        Height = 58
        Action = ActionReset
        Align = alLeft
        Flat = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Segoe UI'
        Font.Style = []
        Layout = blGlyphTop
        ParentFont = False
        ParentShowHint = False
        ShowHint = False
        ButtonStyle = tbsTextButton
        SkinData.SkinSection = 'TOOLBUTTON'
        Images = ImagesDataModule.ImageList
        ImageIndex = 128
      end
      object SpeedButtonMoveDown: TBCSpeedButton
        Left = 322
        Top = 2
        Width = 60
        Height = 58
        Action = ActionMoveDown
        Align = alLeft
        Flat = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Segoe UI'
        Font.Style = []
        Layout = blGlyphTop
        ParentFont = False
        ParentShowHint = False
        ShowHint = False
        ButtonStyle = tbsTextButton
        SkinData.SkinSection = 'TOOLBUTTON'
        Images = ImagesDataModule.ImageList
        ImageIndex = 130
      end
      object SpeedButtonMoveUp: TBCSpeedButton
        Left = 262
        Top = 2
        Width = 60
        Height = 58
        Action = ActionMoveUp
        Align = alLeft
        Flat = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Segoe UI'
        Font.Style = []
        Layout = blGlyphTop
        ParentFont = False
        ParentShowHint = False
        ShowHint = False
        ButtonStyle = tbsTextButton
        SkinData.SkinSection = 'TOOLBUTTON'
        Images = ImagesDataModule.ImageList
        ImageIndex = 129
      end
    end
    object PanelBottom: TsPanel
      Left = 0
      Top = 279
      Width = 451
      Height = 26
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
      object RadioButtonLargeIcons: TsRadioButton
        AlignWithMargins = True
        Left = 6
        Top = 3
        Width = 74
        Height = 20
        Margins.Left = 6
        Action = ActionLargeIcons
        Align = alLeft
        TabOrder = 0
      end
      object RadioButtonSmallIcons: TsRadioButton
        AlignWithMargins = True
        Left = 89
        Top = 3
        Width = 71
        Height = 20
        Margins.Left = 6
        Action = ActionSmallIcons
        Align = alLeft
        TabOrder = 1
      end
    end
  end
  object MenuActionList: TActionList
    Images = ImagesDataModule.ImageList
    Left = 247
    Top = 173
    object ActionAddItem: TAction
      Caption = 'Add item'
      Hint = 'Add item'
      ImageIndex = 18
      OnExecute = ActionAddItemExecute
    end
    object ActionDelete: TAction
      Caption = 'Delete'
      Hint = 'Delete selected item'
      ImageIndex = 22
      OnExecute = ActionDeleteExecute
    end
    object ActionAddDivider: TAction
      Caption = 'Add divider'
      Hint = 'Add divider'
      ImageIndex = 127
      OnExecute = ActionAddDividerExecute
    end
    object ActionReset: TAction
      Caption = 'Reset'
      Hint = 'Reset menu items'
      ImageIndex = 128
      OnExecute = ActionResetExecute
    end
    object ActionMoveUp: TAction
      Caption = 'Move up'
      Hint = 'Move selected item up'
      ImageIndex = 129
      OnExecute = ActionMoveUpExecute
    end
    object ActionMoveDown: TAction
      Caption = 'Move down'
      Hint = 'Move selected item down'
      ImageIndex = 130
      OnExecute = ActionMoveDownExecute
    end
    object ActionLargeIcons: TAction
      Caption = 'Large icons'
      OnExecute = ActionLargeIconsExecute
    end
    object ActionSmallIcons: TAction
      Caption = 'Small icons'
      OnExecute = ActionSmallIconsExecute
    end
  end
  object PopupActionBar: TPopupMenu
    Images = ImagesDataModule.ImageListSmall
    Left = 280
    Top = 86
    object MenuItemAddItem: TMenuItem
      Action = ActionAddItem
    end
    object MenuItemAddDivider: TMenuItem
      Action = ActionAddDivider
    end
    object MenuItemDeleteItem: TMenuItem
      Action = ActionDelete
    end
    object MenuItemDivider: TMenuItem
      Caption = '-'
    end
    object MenuItemReset: TMenuItem
      Action = ActionReset
    end
  end
end
