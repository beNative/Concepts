object BCBaseForm: TBCBaseForm
  Left = 0
  Top = 0
  Caption = 'BCBaseForm'
  ClientHeight = 280
  ClientWidth = 635
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TBCStatusBar
    Left = 0
    Top = 261
    Width = 635
    Height = 19
    Panels = <>
    SkinData.SkinSection = 'STATUSBAR'
  end
  object SkinManager: TBCSkinManager
    AnimEffects.DialogShow.Active = False
    AnimEffects.FormShow.Active = False
    AnimEffects.FormHide.Active = False
    AnimEffects.DialogHide.Active = False
    AnimEffects.Minimizing.Active = False
    AnimEffects.SkinChanging.Active = False
    ButtonsOptions.ShowFocusRect = False
    Active = False
    Saturation = 10
    InternalSkins = <>
    MenuSupport.IcoLineSkin = 'ICOLINE'
    MenuSupport.UseExtraLine = True
    MenuSupport.ExtraLineFont.Charset = DEFAULT_CHARSET
    MenuSupport.ExtraLineFont.Color = clWindowText
    MenuSupport.ExtraLineFont.Height = -13
    MenuSupport.ExtraLineFont.Name = 'Tahoma'
    MenuSupport.ExtraLineFont.Style = [fsBold]
    SkinDirectory = 'Skins'
    SkinName = 'Windows 10'
    SkinInfo = 'N/A'
    ThirdParty.ThirdEdits = 'TBCEditorPrintPreview'#13#10
    ThirdParty.ThirdButtons = 'TButton'#13#10
    ThirdParty.ThirdBitBtns = ' '#13#10
    ThirdParty.ThirdCheckBoxes = ' '#13#10
    ThirdParty.ThirdGroupBoxes = ' '#13#10
    ThirdParty.ThirdListViews = ' '#13#10
    ThirdParty.ThirdPanels = ' '#13#10
    ThirdParty.ThirdGrids = ' '#13#10
    ThirdParty.ThirdTreeViews = ' '#13#10
    ThirdParty.ThirdComboBoxes = ' '#13#10
    ThirdParty.ThirdWWEdits = ' '#13#10
    ThirdParty.ThirdVirtualTrees = ' '#13#10
    ThirdParty.ThirdGridEh = ' '#13#10
    ThirdParty.ThirdPageControl = ' '#13#10
    ThirdParty.ThirdTabControl = ' '#13#10
    ThirdParty.ThirdToolBar = ' '#13#10
    ThirdParty.ThirdStatusBar = ' '#13#10
    ThirdParty.ThirdSpeedButton = ' '#13#10
    ThirdParty.ThirdScrollControl = ' '#13#10
    ThirdParty.ThirdUpDown = ' '#13#10
    ThirdParty.ThirdScrollBar = ' '#13#10
    ThirdParty.ThirdStaticText = ' '#13#10
    ThirdParty.ThirdNativePaint = ' '#13#10
    Left = 38
    Top = 20
  end
  object TitleBar: TBCTitleBar
    Items = <>
    ShowCaption = False
    Left = 40
    Top = 76
  end
  object SkinProvider: TBCSkinProvider
    AddedTitle.Font.Charset = DEFAULT_CHARSET
    AddedTitle.Font.Color = clNone
    AddedTitle.Font.Height = -11
    AddedTitle.Font.Name = 'Tahoma'
    AddedTitle.Font.Style = []
    FormHeader.AdditionalHeight = 0
    SkinData.SkinSection = 'FORM'
    TitleBar = TitleBar
    TitleButtons = <>
    Left = 38
    Top = 136
  end
  object ApplicationEvents: TApplicationEvents
    Left = 128
    Top = 86
  end
  object ActionList: TActionList
    Left = 126
    Top = 142
    object ActionFileExit: TAction
      Caption = 'Exit'
      Hint = 'Quit the application'
      ImageIndex = 11
      ShortCut = 32883
      OnExecute = ActionFileExitExecute
    end
  end
  object MainMenu: TMainMenu
    Left = 39
    Top = 190
  end
end
