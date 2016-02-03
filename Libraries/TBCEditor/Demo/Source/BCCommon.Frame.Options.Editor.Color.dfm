inherited OptionsEditorColorFrame: TOptionsEditorColorFrame
  Width = 451
  Height = 305
  Align = alClient
  object Panel: TBCPanel [0]
    AlignWithMargins = True
    Left = 4
    Top = 0
    Width = 447
    Height = 305
    Margins.Left = 4
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alClient
    BevelOuter = bvNone
    Color = clWindow
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 0
    SkinData.SkinSection = 'CHECKBOX'
    object Splitter: TsSplitter
      Left = 0
      Top = 216
      Width = 447
      Height = 6
      Cursor = crVSplit
      Align = alTop
      SkinData.SkinSection = 'SPLITTER'
    end
    object PanelTop: TBCPanel
      AlignWithMargins = True
      Left = 0
      Top = 0
      Width = 447
      Height = 41
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Align = alTop
      BevelOuter = bvNone
      Color = clWindow
      ParentBackground = False
      TabOrder = 0
      SkinData.SkinSection = 'CHECKBOX'
      object SpeedButtonColor: TBCSpeedButton
        Left = 226
        Top = 14
        Width = 21
        Height = 21
        Action = ActionAddColor
        Flat = True
        Glyph.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FFFFFF000000
          000000000000000000000000000A000000250000003300000033000000330000
          0033000000250000000A000000000000000000000000FFFFFF00FFFFFF000000
          00000000000000000022004F2B5C008347C9008C4BFF008B4AFF008B4AFF008C
          4BFF008347C9004F2B5C0000001E0000000000000000FFFFFF00FFFFFF000000
          00000000001E008046BB009050FF01A169FF00AA76FF00AB77FF00AB77FF00AA
          76FF01A169FF009050FF007C44AA0000001E00000000FFFFFF00FFFFFF000000
          000A007C43AA009152FF02AC77FF00C38CFF00D699FF18DEA8FF18DEA8FF00D6
          99FF00C38CFF01AB76FF009253FF007C44AA0000000AFFFFFF00FFFFFF000059
          3151009051FF0FB483FF02D299FF00D69BFF00D193FFFFFFFFFFFFFFFFFF00D1
          93FF00D69BFF00D198FF01AB76FF009050FF005A3151FFFFFF00FFFFFF000083
          45C916AB78FF11C997FF00D49AFF00D297FF00CD8EFFFFFFFFFFFFFFFFFF00CD
          8EFF00D297FF00D59BFF00C18CFF01A169FF008447C9FFFFFF00FFFFFF00008A
          48FF38C49CFF00D198FF00CD92FF00CB8EFF00C787FFFFFFFFFFFFFFFFFF00C7
          87FF00CB8EFF00CE93FF00D09AFF00AB76FF008C4BFFFFFFFF00FFFFFF000089
          46FF51D2AFFF12D4A3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFF00CF97FF00AD78FF008B4AFFFFFFFF00FFFFFF000088
          45FF66DDBEFF10D0A2FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFF00CD97FF00AD78FF008B4AFFFFFFFF00FFFFFF000088
          46FF76E0C5FF00CA98FF00C590FF00C48EFF00C187FFFFFFFFFFFFFFFFFF00C1
          87FF00C48EFF00C793FF00CB99FF00AB76FF008C4BFFFFFFFF00FFFFFF000088
          46BE59C9A4FF49DEBCFF00C794FF00C794FF00C38EFFFFFFFFFFFFFFFFFF00C3
          8EFF00C896FF00CB9AFF06C190FF00A168FF008B4BBFFFFFFF00FFFFFF00008C
          4B330A9458FFADF8E9FF18D0A7FF00C494FF00C290FFFFFFFFFFFFFFFFFF00C3
          91FF00C799FF05C89BFF18B787FF009050FF008E4D33FFFFFF00FFFFFF000000
          0000008A48AA199C63FFBCFFF7FF5DE4C9FF00C397FF00BF90FF00C091FF00C4
          98FF22CAA2FF31C297FF039355FF008D4C9500000000FFFFFF00FFFFFF000000
          000000000000008A48950E9659FF74D5B6FF9FF3E0FF92EFDAFF79E5CAFF5DD6
          B5FF2EB586FF039152FF008C4CAA0000000000000000FFFFFF00FFFFFF000000
          00000000000000000000008C4A33008946BB008744FF008743FF008744FF0089
          46FF008B49BB008D4C33000000000000000000000000FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00}
        SkinData.SkinSection = 'TOOLBUTTON'
      end
      object ComboBoxColor: TBCComboBox
        Left = 0
        Top = 14
        Width = 219
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
      object ComboBoxHighlighter: TBCComboBox
        Left = 256
        Top = 14
        Width = 241
        Height = 22
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Alignment = taLeftJustify
        BoundLabel.Active = True
        BoundLabel.Caption = 'Highlighter'
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
        TabOrder = 1
        OnChange = ComboBoxHighlighterChange
        UseMouseWheel = False
      end
    end
    object Editor: TBCEditor
      Left = 0
      Top = 222
      Width = 447
      Height = 83
      Cursor = crIBeam
      Margins.Left = 0
      Margins.Top = 5
      Margins.Right = 0
      Margins.Bottom = 0
      ActiveLine.Indicator.Visible = False
      Align = alClient
      Caret.NonBlinking.Enabled = False
      Caret.Options = []
      CodeFolding.Colors.Indent = clBlack
      CodeFolding.Hint.Font.Charset = DEFAULT_CHARSET
      CodeFolding.Hint.Font.Color = clWindowText
      CodeFolding.Hint.Font.Height = -11
      CodeFolding.Hint.Font.Name = 'Courier New'
      CodeFolding.Hint.Font.Style = []
      CompletionProposal.CloseChars = '()[]. '
      CompletionProposal.Columns = <
        item
        end>
      CompletionProposal.Font.Charset = DEFAULT_CHARSET
      CompletionProposal.Font.Color = clWindowText
      CompletionProposal.Font.Height = -11
      CompletionProposal.Font.Name = 'Courier New'
      CompletionProposal.Font.Style = []
      CompletionProposal.ShortCut = 16416
      CompletionProposal.Trigger.Chars = '.'
      CompletionProposal.Trigger.Enabled = False
      Directories.Colors = 'Colors'
      Directories.Highlighters = 'Highlighters'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Style = []
      LeftMargin.Font.Charset = DEFAULT_CHARSET
      LeftMargin.Font.Color = 13408665
      LeftMargin.Font.Height = -11
      LeftMargin.Font.Name = 'Courier New'
      LeftMargin.Font.Style = []
      LeftMargin.Width = 55
      Lines.Strings = (
        '')
      LineSpacing.Spacing = 0
      MatchingPair.Enabled = True
      Minimap.Font.Charset = DEFAULT_CHARSET
      Minimap.Font.Color = clWindowText
      Minimap.Font.Height = -4
      Minimap.Font.Name = 'Courier New'
      Minimap.Font.Style = []
      Minimap.Width = 140
      RightMargin.Position = 80
      RightMargin.Visible = True
      SpecialChars.Style = scsDot
      TabOrder = 1
      WordWrap.Enabled = False
      WordWrap.Indicator.Glyph.Data = {
        7E030000424D7E0300000000000036000000280000000F0000000E0000000100
        2000000000004803000000000000000000000000000000000000FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
        000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0080000000FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
        0000FFFFFF00FFFFFF00FFFFFF00FFFFFF008000000080000000FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF008000000080000000800000008000000080000000FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF00FFFF
        FF00FFFFFF00FFFFFF008000000080000000FFFFFF00FFFFFF0080000000FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF0080000000FFFFFF00FFFFFF0080000000FFFFFF00FFFF
        FF00FFFFFF000000000000000000000000000000000000000000FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0080000000FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF0080000000FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00800000008000000080000000800000008000
        00008000000080000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
        FF00}
      WordWrap.Indicator.MaskColor = clFuchsia
      WordWrap.Position = 80
      WordWrap.Style = wwsClientWidth
    end
    object PageControl: TBCPageControl
      Left = 0
      Top = 44
      Width = 447
      Height = 172
      ActivePage = TabSheetEditor
      Align = alTop
      TabHeight = 22
      TabOrder = 2
      TabMargin = 2
      SkinData.SkinSection = 'PAGECONTROL'
      HoldShiftToDragDrop = False
      TabDragDrop = False
      object TabSheetEditor: TsTabSheet
        Caption = 'Editor'
        SkinData.CustomColor = False
        SkinData.CustomFont = False
        object ComboBoxEditorElement: TBCComboBox
          Left = 6
          Top = 22
          Width = 240
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
          ItemIndex = 0
          TabOrder = 0
          Text = 'Active line background'
          OnChange = ComboBoxEditorElementChange
          Items.Strings = (
            'Active line background'
            'Background'
            'Code folding active line background'
            'Code folding background'
            'Code folding collapsed line'
            'Code folding folding line'
            'Code folding hint background'
            'Code folding hint border'
            'Code folding hint text'
            'Code folding indent highlight'
            'Completion proposal background'
            'Completion proposal border'
            'Completion proposal foreground'
            'Completion proposal selected background'
            'Completion proposal selected text'
            'Left margin active line background'
            'Left margin background'
            'Left margin bookmark panel'
            'Left margin border'
            'Left margin line number line'
            'Left margin line numbers'
            'Left margin line state modified'
            'Left margin line state normal'
            'Matching pair matched'
            'Matching pair unmatched'
            'Minimap bookmark'
            'Minimap visible lines'
            'Right margin'
            'Right moving edge'
            'Search highlighter background'
            'Search highlighter foreground'
            'Search map activeLine'
            'Search map background'
            'Search map foreground'
            'Selection background'
            'Selection foreground'
            'Word wrap indicator arrow'
            'Word wrap indicator lines')
          UseMouseWheel = False
        end
        object ColorComboBoxEditorColor: TBCColorComboBox
          Left = 6
          Top = 64
          Width = 240
          Height = 22
          BoundLabel.Active = True
          BoundLabel.Caption = 'Color'
          BoundLabel.Indent = 4
          BoundLabel.Layout = sclTopLeft
          SkinData.SkinSection = 'COMBOBOX'
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor]
          Selected = clWindow
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          OnChange = ColorComboBoxEditorColorChange
          ColorText = 'clWindow'
        end
      end
      object TabSheetElements: TsTabSheet
        Caption = 'Elements'
        SkinData.CustomColor = False
        SkinData.CustomFont = False
        object ComboBoxElementsName: TBCComboBox
          Left = 6
          Top = 19
          Width = 240
          Height = 22
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Alignment = taLeftJustify
          BoundLabel.Active = True
          BoundLabel.Caption = 'Name'
          BoundLabel.Indent = 4
          BoundLabel.Layout = sclTopLeft
          DropDownCount = 9
          SkinData.SkinSection = 'COMBOBOX'
          VerticalAlignment = taAlignTop
          Style = csOwnerDrawFixed
          ItemIndex = 0
          TabOrder = 0
          Text = 'Assembler comment'
          OnChange = ComboBoxElementsNameChange
          Items.Strings = (
            'Assembler comment'
            'Assembler reserved word'
            'Attribute'
            'Character'
            'Comment'
            'Directive'
            'Editor'
            'Hex number'
            'Highlighted block'
            'Number'
            'Reserved word'
            'String'
            'Symbol')
          UseMouseWheel = False
        end
        object ColorComboBoxElementsForeground: TBCColorComboBox
          Left = 6
          Top = 60
          Width = 240
          Height = 22
          BoundLabel.Active = True
          BoundLabel.Caption = 'Foreground'
          BoundLabel.Indent = 4
          BoundLabel.Layout = sclTopLeft
          SkinData.SkinSection = 'COMBOBOX'
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor]
          Selected = clWindow
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          OnChange = ColorComboBoxElementsForegroundChange
          ColorText = 'clWindow'
        end
        object ColorComboBoxElementsBackground: TBCColorComboBox
          Left = 6
          Top = 102
          Width = 240
          Height = 22
          BoundLabel.Active = True
          BoundLabel.Caption = 'Background'
          BoundLabel.Indent = 4
          BoundLabel.Layout = sclTopLeft
          SkinData.SkinSection = 'COMBOBOX'
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor]
          Selected = clWindow
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          OnChange = ColorComboBoxElementsBackgroundChange
          ColorText = 'clWindow'
        end
        object GroupBoxAttributes: TBCGroupBox
          Left = 253
          Top = 12
          Width = 178
          Height = 113
          Caption = ' Attributes'
          TabOrder = 3
          SkinData.SkinSection = 'GROUPBOX'
          Checked = False
          object StickyLabelElementsAttributesBold: TsStickyLabel
            Left = 12
            Top = 26
            Width = 66
            Height = 13
            AutoSize = False
            Caption = 'Bold'
            AttachTo = SliderElementsAttributesBold
            Gap = 8
          end
          object StickyLabelElementsAttributesItalic: TsStickyLabel
            Left = 12
            Top = 53
            Width = 66
            Height = 13
            AutoSize = False
            Caption = 'Italic'
            AttachTo = SliderElementsAttributesItalic
            Gap = 8
          end
          object StickyLabelElementsAttributesUnderline: TsStickyLabel
            Left = 12
            Top = 80
            Width = 66
            Height = 13
            AutoSize = False
            Caption = 'Underline'
            AttachTo = SliderElementsAttributesUnderline
            Gap = 8
          end
          object SliderElementsAttributesBold: TsSlider
            Left = 86
            Top = 22
            Width = 50
            AutoSize = True
            TabOrder = 0
            OnClick = SliderElementsAttributesClick
            ImageIndexOff = 0
            ImageIndexOn = 0
            FontOn.Charset = DEFAULT_CHARSET
            FontOn.Color = clWindowText
            FontOn.Height = -11
            FontOn.Name = 'Tahoma'
            FontOn.Style = []
            SliderCaptionOn = 'Yes'
            SliderCaptionOff = 'No'
            SliderOn = False
          end
          object SliderElementsAttributesItalic: TsSlider
            Left = 86
            Top = 49
            Width = 50
            AutoSize = True
            TabOrder = 1
            OnClick = SliderElementsAttributesClick
            ImageIndexOff = 0
            ImageIndexOn = 0
            FontOn.Charset = DEFAULT_CHARSET
            FontOn.Color = clWindowText
            FontOn.Height = -11
            FontOn.Name = 'Tahoma'
            FontOn.Style = []
            SliderCaptionOn = 'Yes'
            SliderCaptionOff = 'No'
            SliderOn = False
          end
          object SliderElementsAttributesUnderline: TsSlider
            Left = 86
            Top = 76
            Width = 50
            AutoSize = True
            TabOrder = 2
            OnClick = SliderElementsAttributesClick
            ImageIndexOff = 0
            ImageIndexOn = 0
            FontOn.Charset = DEFAULT_CHARSET
            FontOn.Color = clWindowText
            FontOn.Height = -11
            FontOn.Name = 'Tahoma'
            FontOn.Style = []
            SliderCaptionOn = 'Yes'
            SliderCaptionOff = 'No'
            SliderOn = False
          end
        end
      end
      object TabSheetGeneral: TsTabSheet
        Caption = 'General'
        SkinData.CustomColor = False
        SkinData.CustomFont = False
        object EditVersion: TBCEdit
          Left = 6
          Top = 18
          Width = 90
          Height = 21
          TabOrder = 0
          OnChange = EditChange
          SkinData.SkinSection = 'EDIT'
          BoundLabel.Active = True
          BoundLabel.Caption = 'Version'
          BoundLabel.Indent = 4
          BoundLabel.Layout = sclTopLeft
          EnterToTab = False
          OnlyNumbers = False
          NumbersWithDots = False
          NumbersWithSpots = False
          ErrorColor = 14803455
          NumbersAllowMinus = False
          NumbersAllowPlus = False
        end
        object DateEditDate: TBCEdit
          Left = 6
          Top = 60
          Width = 89
          Height = 21
          AutoSize = False
          MaxLength = 10
          TabOrder = 1
          OnChange = EditChange
          SkinData.SkinSection = 'EDIT'
          BoundLabel.Active = True
          BoundLabel.Caption = 'Date'
          BoundLabel.Indent = 4
          BoundLabel.Layout = sclTopLeft
          EnterToTab = False
          OnlyNumbers = False
          NumbersWithDots = False
          NumbersWithSpots = False
          ErrorColor = 14803455
          NumbersAllowMinus = False
          NumbersAllowPlus = False
        end
      end
      object TabSheetAuthor: TsTabSheet
        Caption = 'Author'
        SkinData.CustomColor = False
        SkinData.CustomFont = False
        object EditName: TBCEdit
          Left = 6
          Top = 20
          Width = 237
          Height = 21
          TabOrder = 0
          OnChange = EditChange
          SkinData.SkinSection = 'EDIT'
          BoundLabel.Active = True
          BoundLabel.Caption = 'Name'
          BoundLabel.Indent = 4
          BoundLabel.Layout = sclTopLeft
          EnterToTab = False
          OnlyNumbers = False
          NumbersWithDots = False
          NumbersWithSpots = False
          ErrorColor = 14803455
          NumbersAllowMinus = False
          NumbersAllowPlus = False
        end
        object EditEmail: TBCEdit
          Left = 6
          Top = 60
          Width = 237
          Height = 21
          TabOrder = 1
          OnChange = EditChange
          SkinData.SkinSection = 'EDIT'
          BoundLabel.Active = True
          BoundLabel.Caption = 'Email'
          BoundLabel.Indent = 4
          BoundLabel.Layout = sclTopLeft
          EnterToTab = False
          OnlyNumbers = False
          NumbersWithDots = False
          NumbersWithSpots = False
          ErrorColor = 14803455
          NumbersAllowMinus = False
          NumbersAllowPlus = False
        end
      end
      object TabSheetUseSkinColor: TsTabSheet
        AlignWithMargins = True
        Margins.Top = 2
        Caption = 'Use skin color'
        SkinData.CustomColor = False
        SkinData.CustomFont = False
        object PanelUseSkinColorLeft: TBCPanel
          Left = 0
          Top = 0
          Width = 218
          Height = 135
          Align = alLeft
          AutoSize = True
          BevelOuter = bvNone
          TabOrder = 0
          SkinData.SkinSection = 'CHECKBOX'
          object StickyLabelSkinActiveLineBackground: TsStickyLabel
            Left = 0
            Top = 4
            Width = 160
            Height = 13
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            AutoSize = False
            Caption = 'Active line background'
            Color = clBtnFace
            ParentColor = False
            AttachTo = SliderSkinActiveLineBackground
            Gap = 8
          end
          object StickyLabelSkinForeground: TsStickyLabel
            Left = 0
            Top = 27
            Width = 160
            Height = 13
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            AutoSize = False
            Caption = 'Foreground'
            Color = clBtnFace
            ParentColor = False
            AttachTo = SliderSkinForeground
            Gap = 8
          end
          object StickyLabelSkinSelectionForeground: TsStickyLabel
            Left = 0
            Top = 50
            Width = 160
            Height = 13
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            AutoSize = False
            Caption = 'Selection foreground'
            Color = clBtnFace
            ParentColor = False
            AttachTo = SliderSkinSelectionForeground
            Gap = 8
          end
          object StickyLabelSkinLeftMarginBackground: TsStickyLabel
            Left = 0
            Top = 73
            Width = 160
            Height = 13
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            AutoSize = False
            Caption = 'Left margin background'
            Color = clBtnFace
            ParentColor = False
            AttachTo = SliderSkinLeftMarginBackground
            Gap = 8
          end
          object StickyLabelSkinCodeFoldingBackground: TsStickyLabel
            Left = 0
            Top = 96
            Width = 160
            Height = 13
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            AutoSize = False
            Caption = 'Code folding background'
            Color = clBtnFace
            ParentColor = False
            AttachTo = SliderSkinCodeFoldingBackground
            Gap = 8
          end
          object StickyLabelSkinCompletionProposalBackground: TsStickyLabel
            Left = 0
            Top = 119
            Width = 160
            Height = 13
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            AutoSize = False
            Caption = 'Completion proposal background'
            Color = clBtnFace
            ParentColor = False
            AttachTo = SliderSkinCompletionProposalBackground
            Gap = 8
          end
          object SliderSkinActiveLineBackground: TsSlider
            Left = 168
            Top = 0
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
            OnSliderChange = SliderSkinValueClick
          end
          object SliderSkinForeground: TsSlider
            Left = 168
            Top = 23
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
            OnSliderChange = SliderSkinValueClick
          end
          object SliderSkinSelectionForeground: TsSlider
            Left = 168
            Top = 46
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
            OnSliderChange = SliderSkinValueClick
          end
          object SliderSkinLeftMarginBackground: TsSlider
            Left = 168
            Top = 69
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
            OnSliderChange = SliderSkinValueClick
          end
          object SliderSkinCodeFoldingBackground: TsSlider
            Left = 168
            Top = 92
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
            OnSliderChange = SliderSkinValueClick
          end
          object SliderSkinCompletionProposalBackground: TsSlider
            Left = 168
            Top = 115
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
            OnSliderChange = SliderSkinValueClick
          end
        end
        object PanelUseSkinColorRight: TBCPanel
          Left = 215
          Top = 0
          Width = 218
          Height = 135
          Align = alRight
          AutoSize = True
          BevelOuter = bvNone
          TabOrder = 1
          SkinData.SkinSection = 'CHECKBOX'
          object StickyLabelSkinBackground: TsStickyLabel
            Left = 0
            Top = 27
            Width = 160
            Height = 13
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            AutoSize = False
            Caption = 'Background'
            Color = clBtnFace
            ParentColor = False
            AttachTo = SliderSkinBackground
            Gap = 8
          end
          object StickyLabelSkinSelectionBackground: TsStickyLabel
            Left = 0
            Top = 50
            Width = 160
            Height = 13
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            AutoSize = False
            Caption = 'Selection background'
            Color = clBtnFace
            ParentColor = False
            AttachTo = SliderSkinSelectionBackground
            Gap = 8
          end
          object StickyLabelSkinBookmarkPanelBackground: TsStickyLabel
            Left = 0
            Top = 73
            Width = 160
            Height = 13
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            AutoSize = False
            Caption = 'Bookmark panel background'
            Color = clBtnFace
            ParentColor = False
            AttachTo = SliderSkinBookmarkPanelBackground
            Gap = 8
          end
          object StickyLabelSkinCodeFoldingHintBackground: TsStickyLabel
            Left = 0
            Top = 96
            Width = 160
            Height = 13
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            AutoSize = False
            Caption = 'Code folding hint background'
            Color = clBtnFace
            ParentColor = False
            AttachTo = SliderSkinCodeFoldingHintBackground
            Gap = 8
          end
          object StickyLabelSkinCompletionProposalSelectionBackground: TsStickyLabel
            Left = 0
            Top = 119
            Width = 160
            Height = 13
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            AutoSize = False
            Caption = 'Completion proposal selection background'
            Color = clBtnFace
            ParentColor = False
            AttachTo = SliderSkinCompletionProposalSelectionBackground
            Gap = 8
          end
          object SliderSkinBackground: TsSlider
            Left = 168
            Top = 23
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
            OnSliderChange = SliderSkinValueClick
          end
          object SliderSkinSelectionBackground: TsSlider
            Left = 168
            Top = 46
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
            OnSliderChange = SliderSkinValueClick
          end
          object SliderSkinBookmarkPanelBackground: TsSlider
            Left = 168
            Top = 69
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
            OnSliderChange = SliderSkinValueClick
          end
          object SliderSkinCodeFoldingHintBackground: TsSlider
            Left = 168
            Top = 92
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
            OnSliderChange = SliderSkinValueClick
          end
          object SliderSkinCompletionProposalSelectionBackground: TsSlider
            Left = 168
            Top = 115
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
            OnSliderChange = SliderSkinValueClick
          end
        end
      end
    end
  end
  inherited FrameAdapter: TsFrameAdapter
    Left = 368
    Top = 24
  end
  object ActionList: TActionList
    Left = 252
    Top = 226
    object ActionAddColor: TAction
      OnExecute = ActionAddColorExecute
    end
  end
  object SaveDialog: TsSaveDialog
    DefaultExt = 'json'
    Left = 438
    Top = 79
  end
  object MultiStringHolder: TBCMultiStringHolder
    MultipleStrings = <
      item
        Name = 'DefaultJSON'
        Strings.Strings = (
          '{'
          '    "Colors": {'
          '        "Info": {'
          '            "General": {'
          '                "Version": "1.0",'
          '                "Date": "1.5.2015"'
          '            },'
          '            "Author": {'
          '                "Name": "Lasse Rautiainen",'
          '                "Mail": "lasse@bonecode.com"'
          '            }'
          '        },'
          '        "Editor": {'
          '            "Colors": {'
          '                "Background": "clWindow",'
          '                "ActiveLine": "$00E6FAFF",'
          '                "CodeFoldingBackground": "clWhite",'
          '                "CodeFoldingCollapsedLine": "$00CC9999",'
          '                "CodeFoldingFoldingLine": "$00CC9999",'
          '                "CodeFoldingIndentHighlight": "$00CC9999",'
          '                "CodeFoldingHintBackground": "clWindow",'
          '                "CodeFoldingHintBorder": "$00CC9999",'
          '                "CodeFoldingHintText": "clNone",'
          '                "CompletionProposalBackground": "clWindow",'
          '                "CompletionProposalForeground": "clNone",'
          '                "CompletionProposalBorder": "$00CC9999",'
          
            '                "CompletionProposalSelectedBackground": "clHighl' +
            'ight",'
          
            '                "CompletionProposalSelectedText": "clHighlightTe' +
            'xt",'
          '                "LeftMarginBackground": "clWhite",'
          '                "LeftMarginLineNumbers": "$00CC9999",'
          '                "LeftMarginLineStateModified": "clYellow",'
          '                "LeftMarginLineStateNormal": "clLime",'
          '                "LeftMarginBookmarkPanel": "clWhite",'
          '                "MatchingPairMatched": "clAqua",'
          '                "MatchingPairUnmatched": "clYellow",'
          '                "RightMargin": "clSilver",'
          '                "RightMovingEdge": "clSilver",'
          '                "SearchHighlighterBackground": "$0078AAFF",'
          '                "SearchHighlighterForeground": "clWindowText",'
          '                "SearchMapActiveLine": "$00F0F0F0",'
          '                "SearchMapBackground": "$00F4F4F4",'
          '                "SearchMapForeground": "$0078AAFF",'
          '                "SelectionBackground": "$00A56D53",'
          '                "SelectionForeground": "clHighlightText"'
          '            },'
          '            "Fonts": {'
          '                "LineNumbers": "Courier New",'
          '                "Text": "Courier New",'
          '                "Minimap": "Courier New",'
          '                "CodeFoldingHint": "Courier New",'
          '                "CompletionProposal": "Courier New"'
          '            },'
          '            "FontSizes": {'
          '                "LineNumbers": "8",'
          '                "Text": "9",'
          '                "Minimap": "3",'
          '                "CodeFoldingHint": "8",'
          '                "CompletionProposal": "8"'
          '            }'
          '        },'
          '        "Elements": ['
          '            {'
          '                "Name": "Editor",'
          '                "Foreground": "clWindowText",'
          '                "Background": "clWindow"'
          '            },'
          '            {'
          '                "Name": "Comment",'
          '                "Foreground": "clGreen",'
          '                "Background": "clWindow",'
          '                "Style": "Italic"'
          '            },'
          '            {'
          '                "Name": "String",'
          '                "Foreground": "clBlue",'
          '                "Background": "clWindow"'
          '            },'
          '            {'
          '                "Name": "Directive",'
          '                "Foreground": "clTeal",'
          '                "Background": "clWindow"'
          '            },'
          '            {'
          '                "Name": "Character",'
          '                "Foreground": "clPurple",'
          '                "Background": "clWindow"'
          '            },'
          '            {'
          '                "Name": "ReservedWord",'
          '                "Foreground": "clNavy",'
          '                "Background": "clWindow",'
          '                "Style": "Bold"'
          '            },'
          '            {'
          '                "Name": "Symbol",'
          '                "Foreground": "clNavy",'
          '                "Background": "clWindow"'
          '            },'
          '            {'
          '                "Name": "Number",'
          '                "Foreground": "clBlue",'
          '                "Background": "clWindow"'
          '            },'
          '            {'
          '                "Name": "HexNumber",'
          '                "Foreground": "clBlue",'
          '                "Background": "clWindow"'
          '            },'
          '            {'
          '                "Name": "HighlightedBlock",'
          '                "Foreground": "clBlack",'
          '                "Background": "$00EEFFFF"'
          '            },'
          '            {'
          '                "Name": "AssemblerComment",'
          '                "Foreground": "clGreen",'
          '                "Background": "$00EEFFFF",'
          '                "Style": "Italic"'
          '            },'
          '            {'
          '                "Name": "AssemblerReservedWord",'
          '                "Foreground": "clNavy",'
          '                "Background": "$00EEFFFF",'
          '                "Style": "Bold"'
          '            },'
          '            {'
          '                "Name": "Attribute",'
          '                "Foreground": "clMaroon",'
          '                "Background": "clWindow"'
          '            }'
          '        ]'
          '    }'
          '}')
      end>
    Left = 364
    Top = 225
  end
end
