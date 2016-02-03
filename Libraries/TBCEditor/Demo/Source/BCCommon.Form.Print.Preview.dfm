object PrintPreviewDialog: TPrintPreviewDialog
  Left = 192
  Top = 148
  Caption = 'Print Preview'
  ClientHeight = 523
  ClientWidth = 869
  Color = clWindow
  ParentFont = True
  Icon.Data = {
    0000010001001010000001002000280400001600000028000000100000002000
    0000010020000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000BABA
    B9FFAFAFADFFAEAEABFFADADABFFADADABFFADADABFFAEAEABFFAEAEACFFAFAF
    ADFFAFAFADFFB0B0AEFFB6B3AFFFC7C1BBFF31699EFF396D9EFF00000000B0B0
    ADFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFF23629EFF25B0FFFF44C8FFFF3A83CCFFAEAE
    ABFFFFFFFFFFFDFDFDFFFCFCFDFFFDFDFDFFFEFEFEFFFFFFFFFFD3D3D4FF6E6F
    71FF58585CFF5A5A5DFF636060FF7D7068FF3AC6FFFF57DBFFFF3981C9FFADAD
    ABFFFFFFFFFFFAF8F8FFF9F8F8FFFAF9F9FFFFFDFEFFD1D1D2FF676463FFE3C3
    8CFFFFEBA4FFFFF3AFFFE7D3A1FF76706BFFA09085FF3280CFFF00000000ADAD
    ABFFFFFFFFFFF6F6F6FFF6F6F6FFF8F8F8FFFEFFFFFF777879FFE3BE84FFFFE6
    A5FFFFE7A6FFFFEFB3FFFFF9BBFFE9D6A2FF807875FF0000000000000000ADAD
    ABFFFFFFFFFFF4F4F3FFF4F4F3FFF6F6F5FFFDFEFEFF696A6DFFFFE09DFFFFEF
    CAFFFFE7B3FFFFE9ABFFFFEFB2FFFFF4AFFF787677FF0000000000000000ADAD
    ABFFFFFFFFFFF2F1F0FFF2F1F0FFF4F3F2FFFBFAFBFF6F6F72FFFFDD97FFFFF7
    E4FFFFEDC8FFFFE7B2FFFFE6A5FFFFECA5FF7C7B7EFF0000000000000000ADAD
    ABFFFFFFFFFFEFEFEEFFEFEFEEFFF0F1F0FFF6F7F7FF848587FFE7BB7CFFFFF5
    DBFFFFF7E4FFFFEECAFFFFE5A4FFEBCC95FF868587FF0000000000000000ADAD
    ABFFFFFFFFFFECEBEAFFEDECEBFFEEECEBFFF1F0F0FFCFCFD0FF83807DFFE8BC
    7DFFFFDC97FFFFDF9CFFEAC58BFF8C8887FF000000000000000000000000ADAD
    ABFFFFFFFFFFE9E9E8FFEAEAE9FFEAEAE9FFEBECEBFFEFEFEFFFD8D7D8FF9494
    96FF858588FF848487FF979698FFA9AAAAFF000000000000000000000000ADAE
    ABFFFFFFFFFFE7E5E4FFE8E7E6FFE8E7E6FFE8E7E6FFE8E7E6FFFFFFFFFFD3D3
    D3FFAFB0AFFFAEAFADFFFFFFFFFFB3B3B1FF000000000000000000000000AEAE
    ABFFFFFFFFFFE3E3E2FFE4E4E3FFE4E5E4FFE4E4E3FFE3E3E2FFFFFFFFFFA8A8
    A6FFEDEDECFFFFFFFFFFEAEAEAFFCACAC9FF000000000000000000000000AEAE
    ACFFFFFFFFFFE0DFDEFFE1DFDEFFE1E0DFFFE1DFDEFFE0DFDEFFFFFFFFFFA5A5
    A3FFFFFFFFFFE8E8E8FFCBCBC9FF00000000000000000000000000000000AFAF
    ADFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFE9E9E9FFCACAC9FF0000000000000000000000000000000000000000B8B8
    B6FFB0B0ADFFAEAEACFFAEAEABFFAEAEABFFAEAEABFFADAEABFFAEAEABFFAFAF
    ADFFB7B7B5FF000000000000000000000000000000000000000000000000}
  OldCreateOrder = True
  Position = poMainFormCenter
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TBCStatusBar
    Left = 0
    Top = 498
    Width = 869
    Height = 25
    Panels = <
      item
        Width = 172
      end
      item
        Alignment = taCenter
        Width = 50
      end
      item
        Width = 120
      end
      item
        Width = 400
      end>
    ParentShowHint = False
    ShowHint = True
    SkinData.SkinSection = 'STATUSBAR'
    object TrackBarZoom: TsTrackBar
      Tag = 5
      Left = 0
      Top = 0
      Width = 170
      Height = 38
      Max = 400
      Min = 25
      Position = 100
      TabOrder = 0
      TickStyle = tsNone
      OnChange = TrackBarZoomChange
      SkinData.SkinSection = 'TRACKBAR'
      ShowProgress = True
      BarOffsetV = 0
      BarOffsetH = 0
    end
  end
  object PanelButtons: TBCPanel
    Left = 0
    Top = 0
    Width = 869
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
    object SpeedButtonLast: TBCSpeedButton
      Left = 182
      Top = 2
      Width = 60
      Height = 58
      Action = ActionLast
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
      ImageIndex = 80
    end
    object SpeedButtonNext: TBCSpeedButton
      Left = 122
      Top = 2
      Width = 60
      Height = 58
      Action = ActionNext
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
      ImageIndex = 37
    end
    object SpeedButtonPrevious: TBCSpeedButton
      Left = 62
      Top = 2
      Width = 60
      Height = 58
      Action = ActionPrevious
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
      ImageIndex = 38
    end
    object SpeedButtonFirst: TBCSpeedButton
      Left = 2
      Top = 2
      Width = 60
      Height = 58
      Action = ActionFirst
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
      ImageIndex = 79
    end
    object SpeedButtonDivider2: TBCSpeedButton
      AlignWithMargins = True
      Left = 432
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
    object SpeedButtonDivider3: TBCSpeedButton
      AlignWithMargins = True
      Left = 682
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
    object SpeedButtonDivider4: TBCSpeedButton
      AlignWithMargins = True
      Left = 752
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
    object SpeedButtonDivider1: TBCSpeedButton
      AlignWithMargins = True
      Left = 242
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
    object SpeedButtonZoom: TBCSpeedButton
      Left = 372
      Top = 2
      Width = 60
      Height = 58
      Hint = 'Zoom|Zoom In/Out'
      Align = alLeft
      Caption = 'Zoom'
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
      ButtonStyle = tbsDropDown
      SkinData.SkinSection = 'TOOLBUTTON'
      DropdownMenu = PopupMenuZoom
      Images = ImagesDataModule.ImageList
      ImageIndex = 34
    end
    object SpeedButtonZoomOut: TBCSpeedButton
      Left = 312
      Top = 2
      Width = 60
      Height = 58
      Action = ActionZoomOut
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
      ImageIndex = 82
    end
    object SpeedButtonZoomIn: TBCSpeedButton
      Left = 252
      Top = 2
      Width = 60
      Height = 58
      Action = ActionZoomIn
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
      ImageIndex = 81
    end
    object SpeedButtonHighlighter: TBCSpeedButton
      Left = 622
      Top = 2
      Width = 60
      Height = 58
      Action = ActionHighlight
      Align = alLeft
      AllowAllUp = True
      GroupIndex = 4
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
      ImageIndex = 84
    end
    object SpeedButtonColors: TBCSpeedButton
      Left = 562
      Top = 2
      Width = 60
      Height = 58
      Action = ActionColors
      Align = alLeft
      AllowAllUp = True
      GroupIndex = 3
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
      ImageIndex = 83
    end
    object SpeedButtonWordWrap: TBCSpeedButton
      Left = 502
      Top = 2
      Width = 60
      Height = 58
      Action = ActionWordWrap
      Align = alLeft
      AllowAllUp = True
      GroupIndex = 2
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
      ImageIndex = 57
    end
    object SpeedButtonLineNumbers: TBCSpeedButton
      Left = 442
      Top = 2
      Width = 60
      Height = 58
      Action = ActionLineNumbers
      Align = alLeft
      AllowAllUp = True
      GroupIndex = 1
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
      ImageIndex = 58
    end
    object SpeedButtonPrint: TBCSpeedButton
      Left = 692
      Top = 2
      Width = 60
      Height = 58
      Action = ActionPrint
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
      ImageIndex = 9
    end
    object SpeedButtonExit: TBCSpeedButton
      Left = 762
      Top = 2
      Width = 60
      Height = 58
      Action = ActionExit
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
      ImageIndex = 11
    end
  end
  object PanelPrintPreview: TBCPanel
    Left = 0
    Top = 62
    Width = 869
    Height = 436
    Align = alClient
    BevelOuter = bvLowered
    TabOrder = 2
    SkinData.SkinSection = 'CHECKBOX'
    object PrintPreview: TBCEditorPrintPreview
      AlignWithMargins = True
      Left = 3
      Top = 1
      Width = 863
      Height = 432
      Margins.Left = 2
      Margins.Top = 0
      Margins.Right = 2
      Margins.Bottom = 2
      BorderStyle = bsNone
      Color = clBtnFace
      OnMouseDown = PrintPreviewMouseDown
      OnPreviewPage = PrintPreviewPreviewPage
      OnScaleChange = PrintPreviewScaleChange
      ScaleMode = pscPageWidth
    end
  end
  object ActionList: TActionList
    Images = ImagesDataModule.ImageList
    Left = 104
    Top = 111
    object ActionColors: TAction
      AutoCheck = True
      Caption = 'Colors'
      GroupIndex = 3
      Hint = 'Toggle colors'
      ImageIndex = 83
      OnExecute = ActionColorsExecute
      OnUpdate = ActionColorsUpdate
    end
    object ActionExit: TAction
      Caption = 'Exit'
      Hint = 'Exit the print preview'
      ImageIndex = 11
      OnExecute = ActionExitExecute
    end
    object ActionFirst: TAction
      Caption = 'First'
      Hint = 'First|Go to first page'
      ImageIndex = 79
      ShortCut = 32838
      OnExecute = ActionFirstExecute
      OnUpdate = ActionFirstUpdate
    end
    object ActionHighlight: TAction
      AutoCheck = True
      Caption = 'Highlight'
      GroupIndex = 4
      Hint = 'Toggle highlight'
      ImageIndex = 84
      OnExecute = ActionHighlightExecute
    end
    object ActionLast: TAction
      Caption = 'Last'
      Hint = 'Last|Go to last page'
      ImageIndex = 80
      ShortCut = 32844
      OnExecute = ActionLastExecute
      OnUpdate = ActionLastUpdate
    end
    object ActionLineNumbers: TAction
      AutoCheck = True
      Caption = 'Line numbers'
      GroupIndex = 1
      Hint = 'Toggle line numbers'
      ImageIndex = 58
      OnExecute = ActionLineNumbersExecute
    end
    object ActionNext: TAction
      Caption = 'Next'
      Hint = 'Next|Go to next page'
      ImageIndex = 37
      ShortCut = 32846
      OnExecute = ActionNextExecute
      OnUpdate = ActionNextUpdate
    end
    object ActionPrevious: TAction
      Caption = 'Previous'
      Hint = 'Previous|Go to previous page'
      ImageIndex = 38
      ShortCut = 32848
      OnExecute = ActionPreviousExecute
      OnUpdate = ActionPreviousUpdate
    end
    object ActionPrint: TAction
      Caption = 'Print'
      Hint = 'Print|Print the document'
      ImageIndex = 9
      ShortCut = 16464
      OnExecute = ActionPrintExecute
    end
    object ActionWordWrap: TAction
      AutoCheck = True
      Caption = 'Word wrap'
      GroupIndex = 2
      Hint = 'Toggle word wrap'
      ImageIndex = 57
      OnExecute = ActionWordWrapExecute
    end
    object ActionZoomIn: TAction
      Caption = 'Zoom in'
      Hint = 'Zoom in'
      ImageIndex = 81
      OnExecute = ActionZoomInExecute
    end
    object ActionZoomOut: TAction
      Caption = 'Zoom out'
      Hint = 'Zoom out'
      ImageIndex = 82
      OnExecute = ActionZoomOutExecute
    end
    object ActionZoom: TAction
      Caption = 'Zoom'
      Hint = 'Zoom|Zoom In/Out'
      ImageIndex = 34
      ShortCut = 32858
    end
  end
  object PopupMenuZoom: TPopupMenu
    Images = ImagesDataModule.ImageListSmall
    Left = 110
    Top = 171
    object MenuItemPercent25: TMenuItem
      Tag = 25
      Caption = '25%'
      OnClick = PercentClick
    end
    object MenuItemPercent50: TMenuItem
      Tag = 50
      Caption = '50%'
      OnClick = PercentClick
    end
    object MenuItemPercent75: TMenuItem
      Tag = 75
      Caption = '75%'
      OnClick = PercentClick
    end
    object MenuItemPercent100: TMenuItem
      Tag = 100
      Caption = '100%'
      OnClick = PercentClick
    end
    object MenuItemPercent125: TMenuItem
      Tag = 125
      Caption = '125%'
      OnClick = PercentClick
    end
    object MenuItemPercent150: TMenuItem
      Tag = 150
      Caption = '150%'
      OnClick = PercentClick
    end
    object MenuItemPercent175: TMenuItem
      Tag = 175
      Caption = '175%'
      OnClick = PercentClick
    end
    object MenuItemPercent200: TMenuItem
      Tag = 200
      Caption = '200%'
      OnClick = PercentClick
    end
    object MenuItemPercent300: TMenuItem
      Tag = 300
      Caption = '300%'
      OnClick = PercentClick
    end
    object MenuItemPercent400: TMenuItem
      Tag = 400
      Caption = '400%'
      OnClick = PercentClick
    end
  end
  object ApplicationEvents: TApplicationEvents
    OnHint = ApplicationEventsHint
    Left = 345
    Top = 65
  end
  object PrintDialog: TPrintDialog
    Left = 210
    Top = 152
  end
end
