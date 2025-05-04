object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Modern App Demo'
  ClientHeight = 669
  ClientWidth = 1034
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  ScreenSnap = True
  ShowHint = True
  OnAfterMonitorDpiChanged = FormAfterMonitorDpiChanged
  OnBeforeMonitorDpiChanged = FormBeforeMonitorDpiChanged
  OnCreate = FormCreate
  OnShow = FormShow
  TextHeight = 13
  object splSettings: TSplitter
    Left = 1031
    Top = 42
    Height = 627
    Align = alRight
    AutoSnap = False
    Beveled = True
    MinSize = 45
  end
  object splSplit: TSplitter
    Left = 200
    Top = 42
    Height = 627
    AutoSnap = False
    Beveled = True
    MinSize = 45
  end
  object PageControl: TPageControl
    Left = 203
    Top = 42
    Width = 528
    Height = 627
    ActivePage = tsStandard
    Align = alClient
    TabOrder = 3
    OnChange = PageControlChange
    object tsStandard: TTabSheet
      Caption = 'Standard Controls'
      ImageIndex = 1
      DesignSize = (
        520
        599)
      object Label1: TLabel
        Left = 8
        Top = 5
        Width = 31
        Height = 13
        Caption = 'Label1'
        Transparent = True
      end
      object IconFontImageLabel: TLabel
        Left = 8
        Top = 448
        Width = 76
        Height = 13
        Caption = 'IconFont Image'
      end
      object SVGIconImageLabel: TLabel
        Left = 135
        Top = 448
        Width = 73
        Height = 13
        Caption = 'SVGIcon Image'
      end
      object SVGIconImage: TSVGIconImage
        Left = 135
        Top = 467
        Width = 120
        Height = 120
        Cursor = crHandPoint
        Hint = 'CLick Mouse-Left or Mouse-Right to change image'
        AutoSize = False
        ImageList = SVGIconImageList
        ImageIndex = 5
        OnMouseDown = IconImageMouseDown
      end
      object Edit: TEdit
        Left = 8
        Top = 24
        Width = 223
        Height = 21
        TabOrder = 0
      end
      object HomeButton: TButton
        Left = 361
        Top = 567
        Width = 75
        Height = 25
        Action = actHome
        Anchors = [akRight, akBottom]
        Images = SVGIconImageList
        TabOrder = 13
      end
      object LogButton: TButton
        Left = 442
        Top = 567
        Width = 75
        Height = 25
        Action = actLog
        Anchors = [akRight, akBottom]
        Images = SVGIconImageList
        TabOrder = 14
      end
      object CheckListBox: TCheckListBox
        Left = 8
        Top = 59
        Width = 223
        Height = 100
        ItemHeight = 17
        Items.Strings = (
          'First'
          'Second'
          'Third')
        TabOrder = 3
      end
      object RichEdit: TRichEdit
        Left = 237
        Top = 59
        Width = 186
        Height = 100
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 4
      end
      object SpinEdit: TSpinEdit
        Left = 8
        Top = 173
        Width = 121
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 5
        Value = 0
      end
      object ComboBox: TComboBox
        Left = 135
        Top = 173
        Width = 202
        Height = 21
        Style = csDropDownList
        Color = clBtnFace
        TabOrder = 6
        Items.Strings = (
          'First'
          'Second'
          'Third')
      end
      object ListBox: TListBox
        Left = 8
        Top = 216
        Width = 121
        Height = 97
        ItemHeight = 13
        Items.Strings = (
          'One'
          'Two'
          'Three')
        TabOrder = 8
      end
      object Memo: TMemo
        Left = 136
        Top = 216
        Width = 201
        Height = 97
        TabOrder = 9
      end
      object ColorBox: TColorBox
        Left = 344
        Top = 173
        Width = 185
        Height = 24
        ItemHeight = 18
        TabOrder = 7
      end
      object MaskEdit: TMaskEdit
        Left = 237
        Top = 24
        Width = 127
        Height = 21
        EditMask = '!\(999\)000-0000;1; '
        MaxLength = 13
        TabOrder = 1
        Text = '(   )   -    '
      end
      object RadioButton: TRadioButton
        Left = 384
        Top = 28
        Width = 145
        Height = 13
        Caption = 'Radio button'
        TabOrder = 2
      end
      object RadioGroup: TRadioGroup
        Left = 343
        Top = 216
        Width = 122
        Height = 97
        Caption = 'RadioGroup'
        Items.Strings = (
          'First'
          'Second'
          'Third')
        ParentBackground = False
        TabOrder = 10
      end
      object DBRichEdit: TDBRichEdit
        Left = 8
        Top = 329
        Width = 329
        Height = 113
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        TabOrder = 11
      end
      object DateTimePicker: TDateTimePicker
        Left = 343
        Top = 329
        Width = 129
        Height = 21
        Date = 43839.000000000000000000
        Time = 0.673683599539799600
        TabOrder = 12
      end
      object IconFontImage: TIconFontImage
        Left = 8
        Top = 467
        Width = 120
        Height = 120
        Cursor = crHandPoint
        Hint = 'CLick Mouse-Left or Mouse-Right to change image'
        ImageList = IconFontsImageList
        ImageIndex = 5
        OnMouseDown = IconImageMouseDown
      end
    end
    object tsWindows10: TTabSheet
      Caption = 'Windows 10 controls'
      ImageIndex = 2
      object CalendarView: TCalendarView
        Left = 3
        Top = 16
        Width = 230
        Height = 201
        Date = 43797.000000000000000000
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -20
        Font.Name = 'Segoe UI'
        Font.Style = []
        HeaderInfo.DaysOfWeekFont.Charset = DEFAULT_CHARSET
        HeaderInfo.DaysOfWeekFont.Color = clWindowText
        HeaderInfo.DaysOfWeekFont.Height = -13
        HeaderInfo.DaysOfWeekFont.Name = 'Segoe UI'
        HeaderInfo.DaysOfWeekFont.Style = []
        HeaderInfo.Font.Charset = DEFAULT_CHARSET
        HeaderInfo.Font.Color = clWindowText
        HeaderInfo.Font.Height = -20
        HeaderInfo.Font.Name = 'Segoe UI'
        HeaderInfo.Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object CalendarPicker: TCalendarPicker
        Left = 3
        Top = 236
        Width = 146
        Height = 25
        CalendarHeaderInfo.DaysOfWeekFont.Charset = DEFAULT_CHARSET
        CalendarHeaderInfo.DaysOfWeekFont.Color = clWindowText
        CalendarHeaderInfo.DaysOfWeekFont.Height = -13
        CalendarHeaderInfo.DaysOfWeekFont.Name = 'Segoe UI'
        CalendarHeaderInfo.DaysOfWeekFont.Style = []
        CalendarHeaderInfo.Font.Charset = DEFAULT_CHARSET
        CalendarHeaderInfo.Font.Color = clWindowText
        CalendarHeaderInfo.Font.Height = -20
        CalendarHeaderInfo.Font.Name = 'Segoe UI'
        CalendarHeaderInfo.Font.Style = []
        Color = clWindow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -16
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        TextHint = 'select a date'
      end
      object ButtonEdit: TSearchBox
        Left = 3
        Top = 279
        Width = 223
        Height = 21
        TabOrder = 2
        Text = 'Milano'
        SearchIndicator = sbiAudio
      end
      object ButtonEditDate: TSearchBox
        Left = 3
        Top = 323
        Width = 223
        Height = 21
        MaxLength = 10
        TabOrder = 3
      end
    end
    object tsDatabase: TTabSheet
      Caption = 'Database'
      ImageIndex = 1
      object DBNavigator: TDBNavigator
        Left = 0
        Top = 0
        Width = 520
        Height = 25
        DataSource = DataSource
        Align = alTop
        TabOrder = 0
      end
      object DbGrid: TDBGrid
        Left = 0
        Top = 25
        Width = 520
        Height = 136
        Align = alTop
        DataSource = DataSource
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
        Columns = <
          item
            Expanded = False
            FieldName = 'Species No'
            Width = 70
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Category'
            Width = 105
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Common_Name'
            Width = 210
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Species Name'
            Width = 280
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Length (cm)'
            Width = 70
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Length_In'
            Width = 70
            Visible = True
          end>
      end
      object paEdit: TPanel
        Left = 0
        Top = 161
        Width = 520
        Height = 438
        Align = alClient
        DoubleBuffered = True
        ParentColor = True
        ParentDoubleBuffered = False
        TabOrder = 2
        DesignSize = (
          520
          438)
        object Label4: TLabel
          Left = 6
          Top = 8
          Width = 52
          Height = 13
          Caption = 'Species No'
          FocusControl = DBEdit1
        end
        object Label5: TLabel
          Left = 6
          Top = 48
          Width = 45
          Height = 13
          Caption = 'Category'
          FocusControl = DBEdit2
        end
        object Label6: TLabel
          Left = 6
          Top = 88
          Width = 74
          Height = 13
          Caption = 'Common_Name'
          FocusControl = DBEdit3
        end
        object Label7: TLabel
          Left = 6
          Top = 128
          Width = 66
          Height = 13
          Caption = 'Species Name'
          FocusControl = DBEdit4
        end
        object Label8: TLabel
          Left = 6
          Top = 176
          Width = 57
          Height = 13
          Caption = 'Length (cm)'
          FocusControl = DBEdit5
        end
        object Label9: TLabel
          Left = 6
          Top = 216
          Width = 49
          Height = 13
          Caption = 'Length_In'
          FocusControl = DBEdit6
        end
        object Label10: TLabel
          Left = 6
          Top = 259
          Width = 28
          Height = 13
          Caption = 'Notes'
          FocusControl = DBMemo1
        end
        object Label11: TLabel
          Left = 213
          Top = 8
          Width = 36
          Height = 13
          Caption = 'Graphic'
          FocusControl = DBImage
        end
        object DBEdit1: TDBEdit
          Left = 6
          Top = 24
          Width = 134
          Height = 21
          DataField = 'Species No'
          DataSource = DataSource
          TabOrder = 0
        end
        object DBEdit2: TDBEdit
          Left = 6
          Top = 64
          Width = 199
          Height = 21
          DataField = 'Category'
          DataSource = DataSource
          TabOrder = 1
        end
        object DBEdit3: TDBEdit
          Left = 6
          Top = 104
          Width = 199
          Height = 21
          DataField = 'Common_Name'
          DataSource = DataSource
          TabOrder = 2
        end
        object DBEdit4: TDBEdit
          Left = 6
          Top = 144
          Width = 199
          Height = 21
          DataField = 'Species Name'
          DataSource = DataSource
          TabOrder = 3
        end
        object DBEdit5: TDBEdit
          Left = 6
          Top = 192
          Width = 134
          Height = 21
          DataField = 'Length (cm)'
          DataSource = DataSource
          TabOrder = 4
        end
        object DBEdit6: TDBEdit
          Left = 6
          Top = 232
          Width = 134
          Height = 21
          DataField = 'Length_In'
          DataSource = DataSource
          TabOrder = 5
        end
        object DBMemo1: TDBMemo
          Left = 6
          Top = 275
          Width = 199
          Height = 156
          Anchors = [akLeft, akTop, akBottom]
          DataField = 'Notes'
          DataSource = DataSource
          TabOrder = 6
        end
        object DBImage: TDBImage
          Left = 213
          Top = 24
          Width = 296
          Height = 407
          Anchors = [akLeft, akTop, akRight, akBottom]
          Color = clAqua
          DataField = 'Graphic'
          DataSource = DataSource
          Proportional = True
          Stretch = True
          TabOrder = 7
          OnDblClick = DBImageDblClick
        end
      end
    end
  end
  object panlTop: TPanel
    Left = 0
    Top = 0
    Width = 1034
    Height = 42
    Align = alTop
    BevelOuter = bvNone
    Color = clHighlight
    ParentBackground = False
    TabOrder = 0
    object lblTitle: TLabel
      AlignWithMargins = True
      Left = 47
      Top = 3
      Width = 121
      Height = 36
      Align = alLeft
      Caption = 'TSplitView Demonstration'
      Layout = tlCenter
    end
    object MenuButtonToolbar: TToolBar
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 38
      Height = 36
      Align = alLeft
      ButtonHeight = 36
      ButtonWidth = 36
      Color = clYellow
      Images = SVGIconImageList
      ParentColor = False
      TabOrder = 0
      Transparent = True
      object MenuToolButton: TToolButton
        AlignWithMargins = True
        Left = 0
        Top = 0
        Action = actMenu
        AutoSize = True
      end
    end
    object ToolBar: TToolBar
      AlignWithMargins = True
      Left = 916
      Top = 3
      Width = 115
      Height = 36
      Align = alRight
      ButtonHeight = 36
      ButtonWidth = 36
      Color = clYellow
      Images = SVGIconImageList
      ParentColor = False
      TabOrder = 1
      Transparent = True
      object HomeToolButton: TToolButton
        Left = 0
        Top = 0
        Action = actHome
      end
      object ThemeToolButton: TToolButton
        Left = 36
        Top = 0
        Action = actChangeTheme
      end
      object SettingsToolButton: TToolButton
        Left = 72
        Top = 0
        Action = actSettings
      end
    end
  end
  object SV: TSplitView
    Left = 0
    Top = 42
    Width = 200
    Height = 627
    CloseStyle = svcCompact
    CompactWidth = 44
    OpenedWidth = 200
    ParentBackground = True
    ParentColor = True
    Placement = svpLeft
    TabOrder = 1
    OnClosed = SVClosed
    OnClosing = SVClosing
    OnOpened = SVOpened
    OnOpening = SVOpening
    OnResize = SVResize
    object catMenuItems: TCategoryButtons
      Left = 0
      Top = 0
      Width = 200
      Height = 554
      Align = alClient
      BackgroundGradientDirection = gdVertical
      BorderStyle = bsNone
      ButtonFlow = cbfVertical
      ButtonHeight = 36
      ButtonWidth = 36
      ButtonOptions = [boFullSize, boShowCaptions, boCaptionOnlyBorder]
      Categories = <
        item
          Caption = 'Actions'
          Color = clNone
          Collapsed = False
          Items = <
            item
              Action = actHome
            end
            item
              Action = actChangeTheme
            end
            item
              Action = actShowChildForm
            end>
          TextColor = clHighlightText
        end
        item
          Caption = 'Messages'
          Color = clNone
          Collapsed = False
          Items = <
            item
              Action = acErrorMessage
            end
            item
              Action = acConfirmMessage
            end
            item
              Action = acWarningMessage
            end
            item
              Action = acInfoMessage
            end>
          TextColor = clHighlightText
        end
        item
          Caption = 'Info'
          Color = clNone
          Collapsed = False
          Items = <
            item
              Action = actLog
            end
            item
              Action = actSettings
            end
            item
              Action = acAbout
            end
            item
              Action = acExit
            end>
          TextColor = clHighlightText
        end>
      Color = clHighlight
      HotButtonColor = 12500670
      Images = SVGIconImageList
      RegularButtonColor = clNone
      SelectedButtonColor = clNone
      TabOrder = 0
    end
    object catSettings: TCategoryButtons
      Left = 0
      Top = 554
      Width = 200
      Height = 73
      Align = alBottom
      BorderStyle = bsNone
      ButtonFlow = cbfVertical
      ButtonHeight = 36
      ButtonWidth = 36
      ButtonOptions = [boCaptionOnlyBorder]
      Categories = <
        item
          Caption = 'Settings'
          Color = clNone
          Collapsed = False
          Items = <
            item
              Action = actHome
            end
            item
              Action = actChangeTheme
            end
            item
              Action = actSettings
            end>
          TextColor = clHighlightText
        end>
      Color = clHighlight
      HotButtonColor = 15974029
      Images = SVGIconImageList
      RegularButtonColor = clNone
      SelectedButtonColor = clNone
      TabOrder = 1
      OnCategoryCollapase = CatPreventCollapase
    end
  end
  object svSettings: TSplitView
    Left = 731
    Top = 42
    Width = 300
    Height = 627
    AnimationDelay = 10
    AnimationStep = 30
    Color = 15000804
    CompactWidth = 44
    OpenedWidth = 300
    Placement = svpRight
    TabOrder = 2
    OnClosed = svSettingsClosed
    OnClosing = svSettingsClosing
    OnOpened = svSettingsOpened
    object catMenuSettings: TCategoryButtons
      Left = 0
      Top = 0
      Width = 300
      Height = 249
      Align = alTop
      BorderStyle = bsNone
      ButtonFlow = cbfVertical
      ButtonHeight = 36
      ButtonWidth = 36
      ButtonOptions = [boFullSize, boShowCaptions, boCaptionOnlyBorder]
      Categories = <
        item
          Color = clNone
          Collapsed = False
          Items = <
            item
              Action = acFont
            end
            item
              Action = actViewOptions
            end
            item
              Action = acIconFonts
              Caption = 'Icons'
            end
            item
              Action = actAnimate
            end
            item
              Action = actLog
            end>
        end>
      Color = clBtnFace
      HotButtonColor = 12500670
      Images = SVGIconImageList
      RegularButtonColor = clNone
      SelectedButtonColor = clNone
      TabOrder = 0
      OnCategoryCollapase = CatPreventCollapase
    end
    object pnlSettings: TPanel
      Left = 0
      Top = 249
      Width = 300
      Height = 378
      Align = alClient
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 1
      Visible = False
      object pcSettings: TPageControl
        Left = 0
        Top = 67
        Width = 300
        Height = 311
        Margins.Top = 60
        ActivePage = tsFont
        Align = alClient
        TabOrder = 0
        object tsFont: TTabSheet
          Caption = 'Font'
          ImageIndex = 3
          DesignSize = (
            292
            283)
          object Label2: TLabel
            Left = 18
            Top = 16
            Width = 56
            Height = 13
            Caption = 'Font Height'
          end
          object FontSizeLabel: TLabel
            Left = 120
            Top = 16
            Width = 6
            Height = 13
            Caption = '8'
          end
          object FontTrackBar: TTrackBar
            Left = 8
            Top = 56
            Width = 282
            Height = 36
            Anchors = [akLeft, akTop, akRight]
            Max = 26
            Min = 8
            Position = 14
            TabOrder = 0
            OnChange = FontTrackBarChange
          end
          object FontComboBox: TComboBox
            Left = 18
            Top = 112
            Width = 263
            Height = 21
            Style = csDropDownList
            TabOrder = 1
            OnSelect = FontComboBoxSelect
            Items.Strings = (
              'First'
              'Second'
              'Third')
          end
          object SaveFontButton: TButton
            Left = 18
            Top = 251
            Width = 75
            Height = 25
            Action = acApplyFont
            Anchors = [akLeft, akBottom]
            Images = SVGIconImageList
            TabOrder = 2
          end
        end
        object tsStyle: TTabSheet
          Caption = 'Menu'
          DesignSize = (
            292
            283)
          object grpDisplayMode: TRadioGroup
            Left = 2
            Top = 6
            Width = 286
            Height = 42
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Display Mode'
            Columns = 2
            ItemIndex = 0
            Items.Strings = (
              'Docked'
              'Overlay')
            TabOrder = 0
            OnClick = grpDisplayModeClick
          end
          object grpCloseStyle: TRadioGroup
            Left = 3
            Top = 65
            Width = 286
            Height = 42
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Close Style'
            Columns = 2
            ItemIndex = 1
            Items.Strings = (
              'Collapse'
              'Compact')
            TabOrder = 1
            OnClick = grpCloseStyleClick
          end
          object grpPlacement: TRadioGroup
            Left = 2
            Top = 124
            Width = 287
            Height = 46
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Placement'
            Columns = 2
            ItemIndex = 0
            Items.Strings = (
              'Left'
              'Right')
            TabOrder = 2
            OnClick = grpPlacementClick
          end
        end
        object tsIconFonts: TTabSheet
          Caption = 'Icons'
          ImageIndex = 4
          DesignSize = (
            292
            283)
          object Label3: TLabel
            Left = 26
            Top = 24
            Width = 68
            Height = 13
            Caption = 'Icon Font Size'
          end
          object IconFontsSizeLabel: TLabel
            Left = 128
            Top = 24
            Width = 12
            Height = 13
            Caption = '24'
          end
          object IconFontsTrackBar: TTrackBar
            Left = 10
            Top = 64
            Width = 282
            Height = 36
            Anchors = [akLeft, akTop, akRight]
            Max = 64
            Min = 16
            Position = 24
            TabOrder = 0
            OnChange = IconFontsTrackBarChange
          end
          object IconsToggleSwitch: TToggleSwitch
            Left = 14
            Top = 106
            Width = 112
            Height = 20
            State = tssOn
            StateCaptions.CaptionOn = 'SVG Icons'
            StateCaptions.CaptionOff = 'Fonts Icons'
            TabOrder = 1
            OnClick = IconsToggleSwitchClick
          end
        end
        object tsAnimation: TTabSheet
          Caption = 'Animation'
          ImageIndex = 1
          DesignSize = (
            292
            283)
          object lblAnimationDelay: TLabel
            Left = 11
            Top = 41
            Width = 100
            Height = 13
            Caption = 'Animation Delay (15)'
          end
          object lblAnimationStep: TLabel
            Left = 11
            Top = 108
            Width = 95
            Height = 13
            Caption = 'Animation Step (20)'
          end
          object tswAnimation: TToggleSwitch
            Left = 9
            Top = 9
            Width = 72
            Height = 20
            State = tssOn
            TabOrder = 0
            OnClick = tswAnimationClick
          end
          object trkAnimationDelay: TTrackBar
            Left = 3
            Top = 62
            Width = 282
            Height = 36
            Anchors = [akLeft, akTop, akRight]
            Max = 15
            Min = 1
            Position = 3
            TabOrder = 1
            OnChange = trkAnimationDelayChange
          end
          object trkAnimationStep: TTrackBar
            Left = 3
            Top = 129
            Width = 282
            Height = 33
            Anchors = [akLeft, akTop, akRight]
            Max = 15
            Min = 1
            Position = 4
            TabOrder = 2
            OnChange = trkAnimationStepChange
          end
          object tsvDisplayMode: TToggleSwitch
            Left = 9
            Top = 177
            Width = 94
            Height = 20
            StateCaptions.CaptionOn = 'Overlay'
            StateCaptions.CaptionOff = 'Docked'
            TabOrder = 3
            OnClick = tsvDisplayModeClick
          end
          object ttsCloseStyle: TToggleSwitch
            Left = 9
            Top = 209
            Width = 98
            Height = 20
            StateCaptions.CaptionOn = 'Collapse'
            StateCaptions.CaptionOff = 'Compact'
            TabOrder = 4
            OnClick = ttsCloseStyleClick
          end
          object ttsCloseSplitView: TToggleSwitch
            Left = 9
            Top = 241
            Width = 153
            Height = 20
            State = tssOn
            StateCaptions.CaptionOn = 'Auto close menu'
            StateCaptions.CaptionOff = 'Leave menu opened'
            TabOrder = 5
            OnClick = ttsCloseStyleClick
          end
        end
        object tsLog: TTabSheet
          Caption = 'Log'
          ImageIndex = 2
          object lstLog: TListBox
            Left = 0
            Top = 0
            Width = 292
            Height = 283
            Align = alClient
            ItemHeight = 13
            ParentColor = True
            TabOrder = 0
          end
        end
      end
      object catPanelSettings: TCategoryButtons
        Left = 0
        Top = 0
        Width = 300
        Height = 67
        Align = alTop
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        ButtonFlow = cbfVertical
        ButtonHeight = 36
        ButtonWidth = 36
        ButtonOptions = [boFullSize, boShowCaptions, boCaptionOnlyBorder]
        Categories = <
          item
            Caption = 'Settings'
            Color = clNone
            Collapsed = False
            Items = <
              item
                Action = actBack
              end>
          end>
        GradientDirection = gdVertical
        HotButtonColor = 15974029
        Images = SVGIconImageList
        RegularButtonColor = clWhite
        SelectedButtonColor = 15132390
        TabOrder = 1
        OnCategoryCollapase = CatPreventCollapase
      end
    end
  end
  object ActionList: TActionList
    Images = SVGIconImageList
    OnUpdate = ActionListUpdate
    Left = 464
    Top = 460
    object actHome: TAction
      Caption = 'Home'
      Hint = 'Home action...'
      ImageIndex = 5
      OnExecute = actHomeExecute
    end
    object actChangeTheme: TAction
      Caption = 'Theme selector'
      Hint = 'Change theme using Theme Selector...'
      ImageIndex = 7
      OnExecute = actChangeThemeExecute
    end
    object actShowChildForm: TAction
      Caption = 'Child form'
      Hint = 'Show child form...'
      ImageIndex = 3
      OnExecute = actShowChildFormExecute
    end
    object actMenu: TAction
      Hint = 'Collapse'
      ImageIndex = 0
      OnExecute = actMenuExecute
    end
    object actSettings: TAction
      Caption = 'Settings'
      ImageIndex = 1
      OnExecute = actSettingsExecute
    end
    object actViewOptions: TAction
      Caption = 'Menu'
      Hint = 'Menu settings'
      ImageIndex = 12
      OnExecute = actViewOptionsExecute
    end
    object actBack: TAction
      Caption = 'Back'
      ImageIndex = 8
      OnExecute = actBackExecute
    end
    object actAnimate: TAction
      Caption = 'Animate'
      ImageIndex = 9
      OnExecute = actAnimateExecute
    end
    object actLog: TAction
      Caption = 'Log'
      ImageIndex = 10
      OnExecute = actLogExecute
    end
    object acFont: TAction
      Caption = 'Font'
      ImageIndex = 11
      OnExecute = acFontExecute
    end
    object acApplyFont: TAction
      Caption = 'Save'
      Hint = 'Save application font...'
      ImageIndex = 13
      OnExecute = acApplyFontExecute
      OnUpdate = acApplyFontUpdate
    end
    object acIconFonts: TAction
      Caption = 'Icon Fonts'
      ImageIndex = 14
      OnExecute = acIconFontsExecute
    end
    object acErrorMessage: TAction
      Caption = 'Error Message...'
      ImageIndex = 16
      OnExecute = acMessageExecute
    end
    object acWarningMessage: TAction
      Caption = 'Warning Message...'
      ImageIndex = 15
      OnExecute = acMessageExecute
    end
    object acInfoMessage: TAction
      Caption = 'Info Message...'
      ImageIndex = 17
      OnExecute = acMessageExecute
    end
    object acConfirmMessage: TAction
      Caption = 'Confirmation Message...'
      ImageIndex = 18
      OnExecute = acMessageExecute
    end
    object acAbout: TAction
      Caption = 'About...'
      ImageIndex = 19
      OnExecute = acAboutExecute
    end
    object acExit: TAction
      Caption = 'Exit...'
      Hint = 'Exit from application'
      ImageIndex = 20
      OnExecute = acExitExecute
    end
  end
  object ClientDataSet: TClientDataSet
    Aggregates = <>
    Params = <>
    AfterPost = ClientDataSetAfterPost
    Left = 600
    Top = 120
    object ClientDataSetSpeciesNo: TFloatField
      FieldName = 'Species No'
    end
    object ClientDataSetCategory: TStringField
      FieldName = 'Category'
      Size = 15
    end
    object ClientDataSetCommon_Name: TStringField
      FieldName = 'Common_Name'
      Size = 30
    end
    object ClientDataSetSpeciesName: TStringField
      FieldName = 'Species Name'
      Size = 40
    end
    object ClientDataSetLengthcm: TFloatField
      FieldName = 'Length (cm)'
    end
    object ClientDataSetLength_In: TFloatField
      FieldName = 'Length_In'
    end
    object ClientDataSetNotes: TMemoField
      FieldName = 'Notes'
      BlobType = ftMemo
      Size = 50
    end
    object ClientDataSetGraphic: TGraphicField
      FieldName = 'Graphic'
      BlobType = ftGraphic
    end
  end
  object DataSource: TDataSource
    DataSet = ClientDataSet
    Left = 928
    Top = 144
  end
  object IconFontsImageList: TIconFontsVirtualImageList
    FontName = 'Material Design Icons Desktop'
    FontColor = clBlack
    MaskColor = clSilver
    OnFontMissing = IconFontsImageListFontMissing
    Size = 24
    ImageCollection = ImageCollectionDataModule.IconFontsImageCollectionMono
    Left = 624
    Top = 480
  end
  object IconFontsImageListColored: TIconFontsVirtualImageList
    FontName = 'Material Design Icons Desktop'
    FontColor = clBlack
    MaskColor = clWhite
    OnFontMissing = IconFontsImageListFontMissing
    Size = 24
    ImageCollection = ImageCollectionDataModule.IconFontsImageCollection
    Left = 624
    Top = 528
  end
  object FileOpenDialog: TFileOpenDialog
    DefaultExtension = '*.gif'
    FavoriteLinks = <>
    FileTypes = <>
    Options = []
    Left = 632
    Top = 424
  end
  object SVGIconImageList: TSVGIconVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'menu'
        Name = 'menu'
      end
      item
        CollectionIndex = 1
        CollectionName = 'settings'
        Name = 'settings'
      end
      item
        CollectionIndex = 2
        CollectionName = 'calculator'
        Name = 'calculator'
      end
      item
        CollectionIndex = 3
        CollectionName = 'layers'
        Name = 'layers'
      end
      item
        CollectionIndex = 4
        CollectionName = 'checkbox'
        Name = 'checkbox'
      end
      item
        CollectionIndex = 5
        CollectionName = 'home'
        Name = 'home'
      end
      item
        CollectionIndex = 6
        CollectionName = 'battery-40'
        Name = 'battery-40'
      end
      item
        CollectionIndex = 7
        CollectionName = 'palette'
        Name = 'palette'
      end
      item
        CollectionIndex = 8
        CollectionName = 'arrow-left'
        Name = 'arrow-left'
      end
      item
        CollectionIndex = 9
        CollectionName = 'play'
        Name = 'play'
      end
      item
        CollectionIndex = 10
        CollectionName = 'find'
        Name = 'find'
      end
      item
        CollectionIndex = 11
        CollectionName = 'font'
        Name = 'font'
      end
      item
        CollectionIndex = 12
        CollectionName = 'dashboard'
        Name = 'dashboard'
      end
      item
        CollectionIndex = 13
        CollectionName = 'check'
        Name = 'check'
      end
      item
        CollectionIndex = 14
        CollectionName = 'picture'
        Name = 'picture'
      end
      item
        CollectionIndex = 15
        CollectionName = 'alert'
        Name = 'alert'
      end
      item
        CollectionIndex = 16
        CollectionName = 'error'
        Name = 'error'
      end
      item
        CollectionIndex = 17
        CollectionName = 'information'
        Name = 'information'
      end
      item
        CollectionIndex = 18
        CollectionName = 'warning'
        Name = 'warning'
      end
      item
        CollectionIndex = 19
        CollectionName = 'help'
        Name = 'help'
      end
      item
        CollectionIndex = 20
        CollectionName = 'exit'
        Name = 'exit'
      end>
    ImageCollection = ImageCollectionDataModule.SVGIconImageCollection
    Width = 24
    Height = 24
    Size = 24
    Left = 624
    Top = 576
  end
end
