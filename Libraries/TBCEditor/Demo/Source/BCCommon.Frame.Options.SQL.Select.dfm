inherited OptionsSQLSelectFrame: TOptionsSQLSelectFrame
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
    object PageControl: TBCPageControl
      AlignWithMargins = True
      Left = 3
      Top = 0
      Width = 448
      Height = 305
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      ActivePage = TabSheetSubquery
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      MultiLine = True
      ParentFont = False
      TabHeight = 22
      TabOrder = 0
      TabMargin = 2
      SkinData.SkinSection = 'PAGECONTROL'
      HoldShiftToDragDrop = False
      TabDragDrop = False
      object TabSheetColumnList: TsTabSheet
        Caption = 'Column list'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        SkinData.CustomColor = False
        SkinData.CustomFont = False
        object StickyLabelAlignAlias: TsStickyLabel
          Left = 9
          Top = 99
          Width = 170
          Height = 13
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          AutoSize = False
          Caption = 'Align alias'
          ParentColor = False
          AttachTo = SliderAlignAlias
          Gap = 8
        end
        object StickyLabelColumnInNewLine: TsStickyLabel
          Left = 9
          Top = 122
          Width = 170
          Height = 13
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          AutoSize = False
          Caption = 'Columns in new line'
          ParentColor = False
          AttachTo = SliderColumnInNewLine
          Gap = 8
        end
        object StickyLabelTreatDistinctAsVirtualColumn: TsStickyLabel
          Left = 9
          Top = 145
          Width = 170
          Height = 13
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          AutoSize = False
          Caption = 'Treat DISTINCT as virtual column'
          ParentColor = False
          AttachTo = SliderTreatDistinctAsVirtualColumn
          Gap = 8
        end
        object ComboBoxColumnListStyle: TBCComboBox
          Left = 9
          Top = 20
          Width = 186
          Height = 22
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Alignment = taLeftJustify
          BoundLabel.Active = True
          BoundLabel.Caption = 'Style'
          BoundLabel.Indent = 4
          BoundLabel.Layout = sclTopLeft
          DropDownCount = 9
          SkinData.SkinSection = 'COMBOBOX'
          VerticalAlignment = taAlignTop
          Style = csOwnerDrawFixed
          ItemIndex = -1
          TabOrder = 0
          UseMouseWheel = False
        end
        object ComboBoxColumnListLineBreak: TBCComboBox
          Left = 9
          Top = 65
          Width = 186
          Height = 22
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Alignment = taLeftJustify
          BoundLabel.Active = True
          BoundLabel.Caption = 'Line break'
          BoundLabel.Indent = 4
          BoundLabel.Layout = sclTopLeft
          DropDownCount = 9
          SkinData.SkinSection = 'COMBOBOX'
          VerticalAlignment = taAlignTop
          Style = csOwnerDrawFixed
          ItemIndex = -1
          TabOrder = 1
          UseMouseWheel = False
        end
        object SliderAlignAlias: TsSlider
          Left = 187
          Top = 95
          Width = 50
          AutoSize = True
          TabOrder = 2
          ImageIndexOff = 0
          ImageIndexOn = 0
          FontOn.Charset = DEFAULT_CHARSET
          FontOn.Color = clWindowText
          FontOn.Height = -11
          FontOn.Name = 'Tahoma'
          FontOn.Style = []
          SliderCaptionOn = 'Yes'
          SliderCaptionOff = 'No'
        end
        object SliderColumnInNewLine: TsSlider
          Left = 187
          Top = 118
          Width = 50
          AutoSize = True
          TabOrder = 3
          ImageIndexOff = 0
          ImageIndexOn = 0
          FontOn.Charset = DEFAULT_CHARSET
          FontOn.Color = clWindowText
          FontOn.Height = -11
          FontOn.Name = 'Tahoma'
          FontOn.Style = []
          SliderCaptionOn = 'Yes'
          SliderCaptionOff = 'No'
        end
        object SliderTreatDistinctAsVirtualColumn: TsSlider
          Left = 187
          Top = 141
          Width = 50
          AutoSize = True
          TabOrder = 4
          ImageIndexOff = 0
          ImageIndexOn = 0
          FontOn.Charset = DEFAULT_CHARSET
          FontOn.Color = clWindowText
          FontOn.Height = -11
          FontOn.Name = 'Tahoma'
          FontOn.Style = []
          SliderCaptionOn = 'Yes'
          SliderCaptionOff = 'No'
        end
      end
      object TabSheetSubquery: TsTabSheet
        Caption = 'Subquery'
        ImageIndex = 1
        SkinData.CustomColor = False
        SkinData.CustomFont = False
        object StickyLabelNewLineAfterIn: TsStickyLabel
          Left = 9
          Top = 13
          Width = 190
          Height = 13
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          AutoSize = False
          Caption = 'New line after IN'
          ParentColor = False
          AttachTo = SliderNewLineAfterIn
          Gap = 8
        end
        object StickyLabelNewLineAfterExists: TsStickyLabel
          Left = 9
          Top = 36
          Width = 190
          Height = 13
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          AutoSize = False
          Caption = 'New line after EXISTS'
          ParentColor = False
          AttachTo = SliderNewLineAfterExists
          Gap = 8
        end
        object StickyLabelNewlineAfterComparisonOperator: TsStickyLabel
          Left = 9
          Top = 59
          Width = 190
          Height = 13
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          AutoSize = False
          Caption = 'New line after comparison operator'
          ParentColor = False
          AttachTo = SliderNewlineAfterComparisonOperator
          Gap = 8
        end
        object StickyLabelNewlineBeforeComparisonOperator: TsStickyLabel
          Left = 9
          Top = 82
          Width = 190
          Height = 13
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          AutoSize = False
          Caption = 'New line before comparison operator'
          ParentColor = False
          AttachTo = SliderNewlineBeforeComparisonOperator
          Gap = 8
        end
        object SliderNewLineAfterIn: TsSlider
          Left = 207
          Top = 9
          Width = 50
          AutoSize = True
          TabOrder = 0
          ImageIndexOff = 0
          ImageIndexOn = 0
          FontOn.Charset = DEFAULT_CHARSET
          FontOn.Color = clWindowText
          FontOn.Height = -11
          FontOn.Name = 'Tahoma'
          FontOn.Style = []
          SliderCaptionOn = 'Yes'
          SliderCaptionOff = 'No'
        end
        object SliderNewLineAfterExists: TsSlider
          Left = 207
          Top = 32
          Width = 50
          AutoSize = True
          TabOrder = 1
          ImageIndexOff = 0
          ImageIndexOn = 0
          FontOn.Charset = DEFAULT_CHARSET
          FontOn.Color = clWindowText
          FontOn.Height = -11
          FontOn.Name = 'Tahoma'
          FontOn.Style = []
          SliderCaptionOn = 'Yes'
          SliderCaptionOff = 'No'
        end
        object SliderNewlineAfterComparisonOperator: TsSlider
          Left = 207
          Top = 55
          Width = 50
          AutoSize = True
          TabOrder = 2
          ImageIndexOff = 0
          ImageIndexOn = 0
          FontOn.Charset = DEFAULT_CHARSET
          FontOn.Color = clWindowText
          FontOn.Height = -11
          FontOn.Name = 'Tahoma'
          FontOn.Style = []
          SliderCaptionOn = 'Yes'
          SliderCaptionOff = 'No'
        end
        object SliderNewlineBeforeComparisonOperator: TsSlider
          Left = 207
          Top = 78
          Width = 50
          AutoSize = True
          TabOrder = 3
          ImageIndexOff = 0
          ImageIndexOn = 0
          FontOn.Charset = DEFAULT_CHARSET
          FontOn.Color = clWindowText
          FontOn.Height = -11
          FontOn.Name = 'Tahoma'
          FontOn.Style = []
          SliderCaptionOn = 'Yes'
          SliderCaptionOff = 'No'
        end
      end
      object TabSheetIntoClause: TsTabSheet
        Caption = 'INTO clause'
        ImageIndex = 2
        SkinData.CustomColor = False
        SkinData.CustomFont = False
        object StickyLabelIntoClauseInNewLine: TsStickyLabel
          Left = 11
          Top = 13
          Width = 111
          Height = 13
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'INTO clause in new line'
          ParentColor = False
          AttachTo = SliderIntoClauseInNewLine
          Gap = 8
        end
        object SliderIntoClauseInNewLine: TsSlider
          Left = 130
          Top = 9
          Width = 50
          AutoSize = True
          TabOrder = 0
          ImageIndexOff = 0
          ImageIndexOn = 0
          FontOn.Charset = DEFAULT_CHARSET
          FontOn.Color = clWindowText
          FontOn.Height = -11
          FontOn.Name = 'Tahoma'
          FontOn.Style = []
          SliderCaptionOn = 'Yes'
          SliderCaptionOff = 'No'
        end
      end
      object TabSheetFromJoinClause: TsTabSheet
        Caption = 'FROM/JOIN clause'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ImageIndex = 3
        ParentFont = False
        SkinData.CustomColor = False
        SkinData.CustomFont = False
        object StickyLabelFromClauseInNewLine: TsStickyLabel
          Left = 9
          Top = 56
          Width = 190
          Height = 13
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          AutoSize = False
          Caption = 'FROM clause in new line'
          ParentColor = False
          AttachTo = SliderFromClauseInNewLine
          Gap = 8
        end
        object StickyLabelJoinClauseInNewLine: TsStickyLabel
          Left = 9
          Top = 79
          Width = 190
          Height = 13
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          AutoSize = False
          Caption = 'JOIN clause in new line'
          ParentColor = False
          AttachTo = SliderJoinClauseInNewLine
          Gap = 8
        end
        object StickyLabelAlignJoinWithFromKeyword: TsStickyLabel
          Left = 9
          Top = 102
          Width = 190
          Height = 13
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          AutoSize = False
          Caption = 'Align JOIN with FROM keyword'
          ParentColor = False
          AttachTo = SliderAlignJoinWithFromKeyword
          Gap = 8
        end
        object StickyLabelAlignAndOrWithOnInJoinClause: TsStickyLabel
          Left = 9
          Top = 125
          Width = 190
          Height = 13
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          AutoSize = False
          Caption = 'Align AND/OR with ON in JOIN clause '
          ParentColor = False
          AttachTo = SliderAlignAndOrWithOnInJoinClause
          Gap = 8
        end
        object StickyLabelAlignAliasInFromClause: TsStickyLabel
          Left = 9
          Top = 148
          Width = 190
          Height = 13
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          AutoSize = False
          Caption = 'Align alias in FROM clause'
          ParentColor = False
          AttachTo = SliderAlignAliasInFromClause
          Gap = 8
        end
        object ComboBoxFromClauseStyle: TBCComboBox
          Left = 9
          Top = 23
          Width = 186
          Height = 22
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Alignment = taLeftJustify
          BoundLabel.Active = True
          BoundLabel.Caption = 'Style'
          BoundLabel.Indent = 4
          BoundLabel.Layout = sclTopLeft
          DropDownCount = 9
          SkinData.SkinSection = 'COMBOBOX'
          VerticalAlignment = taAlignTop
          Style = csOwnerDrawFixed
          ItemIndex = -1
          TabOrder = 0
          UseMouseWheel = False
        end
        object SliderFromClauseInNewLine: TsSlider
          Left = 207
          Top = 52
          Width = 50
          AutoSize = True
          TabOrder = 1
          ImageIndexOff = 0
          ImageIndexOn = 0
          FontOn.Charset = DEFAULT_CHARSET
          FontOn.Color = clWindowText
          FontOn.Height = -11
          FontOn.Name = 'Tahoma'
          FontOn.Style = []
          SliderCaptionOn = 'Yes'
          SliderCaptionOff = 'No'
        end
        object SliderJoinClauseInNewLine: TsSlider
          Left = 207
          Top = 75
          Width = 50
          AutoSize = True
          TabOrder = 2
          ImageIndexOff = 0
          ImageIndexOn = 0
          FontOn.Charset = DEFAULT_CHARSET
          FontOn.Color = clWindowText
          FontOn.Height = -11
          FontOn.Name = 'Tahoma'
          FontOn.Style = []
          SliderCaptionOn = 'Yes'
          SliderCaptionOff = 'No'
        end
        object SliderAlignJoinWithFromKeyword: TsSlider
          Left = 207
          Top = 98
          Width = 50
          AutoSize = True
          TabOrder = 3
          ImageIndexOff = 0
          ImageIndexOn = 0
          FontOn.Charset = DEFAULT_CHARSET
          FontOn.Color = clWindowText
          FontOn.Height = -11
          FontOn.Name = 'Tahoma'
          FontOn.Style = []
          SliderCaptionOn = 'Yes'
          SliderCaptionOff = 'No'
        end
        object SliderAlignAndOrWithOnInJoinClause: TsSlider
          Left = 207
          Top = 121
          Width = 50
          AutoSize = True
          TabOrder = 4
          ImageIndexOff = 0
          ImageIndexOn = 0
          FontOn.Charset = DEFAULT_CHARSET
          FontOn.Color = clWindowText
          FontOn.Height = -11
          FontOn.Name = 'Tahoma'
          FontOn.Style = []
          SliderCaptionOn = 'Yes'
          SliderCaptionOff = 'No'
        end
        object SliderAlignAliasInFromClause: TsSlider
          Left = 207
          Top = 144
          Width = 50
          AutoSize = True
          TabOrder = 5
          ImageIndexOff = 0
          ImageIndexOn = 0
          FontOn.Charset = DEFAULT_CHARSET
          FontOn.Color = clWindowText
          FontOn.Height = -11
          FontOn.Name = 'Tahoma'
          FontOn.Style = []
          SliderCaptionOn = 'Yes'
          SliderCaptionOff = 'No'
        end
      end
      object TabSheetAndOrKeyword: TsTabSheet
        Caption = 'AND/OR keyword'
        ImageIndex = 4
        SkinData.CustomColor = False
        SkinData.CustomFont = False
        object StickyLabelAndOrUnderWhere: TsStickyLabel
          Left = 9
          Top = 56
          Width = 170
          Height = 13
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          AutoSize = False
          Caption = 'AND/OR under where'
          ParentColor = False
          AttachTo = SliderAndOrUnderWhere
          Gap = 8
        end
        object StickyLabelWhereClauseInNewLine: TsStickyLabel
          Left = 9
          Top = 79
          Width = 170
          Height = 13
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          AutoSize = False
          Caption = 'WHERE clause in new line'
          ParentColor = False
          AttachTo = SliderWhereClauseInNewLine
          Gap = 8
        end
        object StickyLabelWhereClauseAlignExpr: TsStickyLabel
          Left = 9
          Top = 102
          Width = 170
          Height = 13
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          AutoSize = False
          Caption = 'WHERE clause align expr'
          ParentColor = False
          AttachTo = SliderWhereClauseAlignExpr
          Gap = 8
        end
        object ComboBoxAndOrLineBreak: TBCComboBox
          Left = 9
          Top = 23
          Width = 186
          Height = 22
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Alignment = taLeftJustify
          BoundLabel.Active = True
          BoundLabel.Caption = 'Line break'
          BoundLabel.Indent = 4
          BoundLabel.Layout = sclTopLeft
          DropDownCount = 9
          SkinData.SkinSection = 'COMBOBOX'
          VerticalAlignment = taAlignTop
          Style = csOwnerDrawFixed
          ItemIndex = -1
          TabOrder = 0
          UseMouseWheel = False
        end
        object SliderAndOrUnderWhere: TsSlider
          Left = 187
          Top = 52
          Width = 50
          AutoSize = True
          TabOrder = 1
          ImageIndexOff = 0
          ImageIndexOn = 0
          FontOn.Charset = DEFAULT_CHARSET
          FontOn.Color = clWindowText
          FontOn.Height = -11
          FontOn.Name = 'Tahoma'
          FontOn.Style = []
          SliderCaptionOn = 'Yes'
          SliderCaptionOff = 'No'
        end
        object SliderWhereClauseInNewLine: TsSlider
          Left = 187
          Top = 75
          Width = 50
          AutoSize = True
          TabOrder = 2
          ImageIndexOff = 0
          ImageIndexOn = 0
          FontOn.Charset = DEFAULT_CHARSET
          FontOn.Color = clWindowText
          FontOn.Height = -11
          FontOn.Name = 'Tahoma'
          FontOn.Style = []
          SliderCaptionOn = 'Yes'
          SliderCaptionOff = 'No'
        end
        object SliderWhereClauseAlignExpr: TsSlider
          Left = 187
          Top = 98
          Width = 50
          AutoSize = True
          TabOrder = 3
          ImageIndexOff = 0
          ImageIndexOn = 0
          FontOn.Charset = DEFAULT_CHARSET
          FontOn.Color = clWindowText
          FontOn.Height = -11
          FontOn.Name = 'Tahoma'
          FontOn.Style = []
          SliderCaptionOn = 'Yes'
          SliderCaptionOff = 'No'
        end
      end
      object TabSheetGroupByClause: TsTabSheet
        Caption = 'GROUP BY clause'
        ImageIndex = 5
        SkinData.CustomColor = False
        SkinData.CustomFont = False
        object StickyLabelGroupByClauseInNewLine: TsStickyLabel
          Left = 9
          Top = 52
          Width = 136
          Height = 13
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'GROUP BY clause in new line'
          ParentColor = False
          AttachTo = SliderGroupByClauseInNewLine
          Gap = 8
        end
        object ComboBoxGroupByClauseStyle: TBCComboBox
          Left = 9
          Top = 22
          Width = 186
          Height = 22
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Alignment = taLeftJustify
          BoundLabel.Active = True
          BoundLabel.Caption = 'Style'
          BoundLabel.Indent = 4
          BoundLabel.Layout = sclTopLeft
          DropDownCount = 9
          SkinData.SkinSection = 'COMBOBOX'
          VerticalAlignment = taAlignTop
          Style = csOwnerDrawFixed
          ItemIndex = -1
          TabOrder = 0
          UseMouseWheel = False
        end
        object SliderGroupByClauseInNewLine: TsSlider
          Left = 153
          Top = 48
          Width = 50
          AutoSize = True
          TabOrder = 1
          ImageIndexOff = 0
          ImageIndexOn = 0
          FontOn.Charset = DEFAULT_CHARSET
          FontOn.Color = clWindowText
          FontOn.Height = -11
          FontOn.Name = 'Tahoma'
          FontOn.Style = []
          SliderCaptionOn = 'Yes'
          SliderCaptionOff = 'No'
        end
      end
      object TabSheetHavingClause: TsTabSheet
        Caption = 'HAVING clause'
        ImageIndex = 6
        SkinData.CustomColor = False
        SkinData.CustomFont = False
        object StickyLabelHavingClauseInNewLine: TsStickyLabel
          Left = 11
          Top = 15
          Width = 124
          Height = 13
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'HAVING clause in new line'
          ParentColor = False
          AttachTo = SliderHavingClauseInNewLine
          Gap = 8
        end
        object SliderHavingClauseInNewLine: TsSlider
          Left = 143
          Top = 11
          Width = 50
          AutoSize = True
          TabOrder = 0
          ImageIndexOff = 0
          ImageIndexOn = 0
          FontOn.Charset = DEFAULT_CHARSET
          FontOn.Color = clWindowText
          FontOn.Height = -11
          FontOn.Name = 'Tahoma'
          FontOn.Style = []
          SliderCaptionOn = 'Yes'
          SliderCaptionOff = 'No'
        end
      end
      object TabSheetOrderByClause: TsTabSheet
        Caption = 'ORDER BY clause'
        ImageIndex = 7
        SkinData.CustomColor = False
        SkinData.CustomFont = False
        object StickyLabelOrderByClauseInNewLine: TsStickyLabel
          Left = 11
          Top = 54
          Width = 136
          Height = 13
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'ORDER BY clause in new line'
          ParentColor = False
          AttachTo = SliderOrderByClauseInNewLine
          Gap = 8
        end
        object ComboBoxOrderByClauseStyle: TBCComboBox
          Left = 9
          Top = 23
          Width = 186
          Height = 22
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Alignment = taLeftJustify
          BoundLabel.Active = True
          BoundLabel.Caption = 'Style'
          BoundLabel.Indent = 4
          BoundLabel.Layout = sclTopLeft
          DropDownCount = 9
          SkinData.SkinSection = 'COMBOBOX'
          VerticalAlignment = taAlignTop
          Style = csOwnerDrawFixed
          ItemIndex = -1
          TabOrder = 0
          UseMouseWheel = False
        end
        object SliderOrderByClauseInNewLine: TsSlider
          Left = 155
          Top = 50
          Width = 50
          AutoSize = True
          TabOrder = 1
          ImageIndexOff = 0
          ImageIndexOn = 0
          FontOn.Charset = DEFAULT_CHARSET
          FontOn.Color = clWindowText
          FontOn.Height = -11
          FontOn.Name = 'Tahoma'
          FontOn.Style = []
          SliderCaptionOn = 'Yes'
          SliderCaptionOff = 'No'
        end
      end
    end
  end
  inherited FrameAdapter: TsFrameAdapter
    Left = 128
    Top = 144
  end
end
