inherited OptionsEditorFontFrame: TOptionsEditorFontFrame
  Width = 164
  Height = 162
  Visible = False
  object Panel: TBCPanel [0]
    AlignWithMargins = True
    Left = 4
    Top = 0
    Width = 160
    Height = 162
    Margins.Left = 4
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    TabOrder = 0
    SkinData.SkinSection = 'CHECKBOX'
    object ComboBoxColor: TBCComboBox
      Left = 0
      Top = 16
      Width = 160
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Alignment = taLeftJustify
      BoundLabel.Active = True
      BoundLabel.Caption = 'Color'
      BoundLabel.Indent = 4
      BoundLabel.Layout = sclTopLeft
      DropDownCount = 9
      SkinData.SkinSection = 'COMBOBOX'
      VerticalAlignment = taAlignTop
      Style = csOwnerDrawFixed
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemIndex = -1
      ParentFont = False
      TabOrder = 0
      OnChange = ComboBoxColorChange
      UseMouseWheel = False
    end
    object ComboBoxElement: TBCComboBox
      Left = 0
      Top = 58
      Width = 160
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Alignment = taLeftJustify
      BoundLabel.Active = True
      BoundLabel.Caption = 'Element'
      BoundLabel.Indent = 4
      BoundLabel.Layout = sclTopLeft
      DropDownCount = 9
      SkinData.SkinSection = 'COMBOBOX'
      VerticalAlignment = taAlignTop
      Style = csOwnerDrawFixed
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemIndex = 0
      ParentFont = False
      TabOrder = 1
      Text = 'Code folding hint'
      OnChange = ComboBoxElementChange
      Items.Strings = (
        'Code folding hint'
        'Completion proposal'
        'Line numbers'
        'Minimap'
        'Text')
      UseMouseWheel = False
    end
    object FontComboBoxFont: TBCFontComboBox
      Left = 0
      Top = 101
      Width = 160
      Height = 22
      Alignment = taLeftJustify
      BoundLabel.Active = True
      BoundLabel.Caption = 'Font'
      BoundLabel.Indent = 4
      BoundLabel.Layout = sclTopLeft
      VerticalAlignment = taAlignTop
      TabOrder = 2
      OnChange = FontComboBoxFontChange
    end
    object EditFontSize: TBCEdit
      Left = 0
      Top = 141
      Width = 64
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnChange = EditFontSizeChange
      SkinData.SkinSection = 'EDIT'
      BoundLabel.Active = True
      BoundLabel.Caption = 'Font size'
      BoundLabel.Indent = 4
      BoundLabel.Layout = sclTopLeft
      EnterToTab = False
      OnlyNumbers = True
      NumbersWithDots = False
      NumbersWithSpots = False
      ErrorColor = 14803198
      NumbersAllowMinus = False
      NumbersAllowPlus = False
    end
  end
  object BookmarkImagesList: TBCImageList
    ShareImages = True
    Items = <>
    Left = 260
    Top = 170
  end
end
