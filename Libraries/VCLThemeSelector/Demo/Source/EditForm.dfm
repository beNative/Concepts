object FmEdit: TFmEdit
  Left = 0
  Top = 0
  Caption = 'Edit Form'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  ParentFont = True
  Menu = MainMenu
  ShowHint = True
  OnAfterMonitorDpiChanged = FormAfterMonitorDpiChanged
  OnClose = FormClose
  OnShow = FormShow
  TextHeight = 15
  object PageControl1: TPageControl
    Left = 0
    Top = 29
    Width = 635
    Height = 270
    ActivePage = tsParentFont
    Align = alClient
    TabOrder = 0
    object tsParentFont: TTabSheet
      Caption = 'Parent Font = True'
      object Label1: TLabel
        Left = 8
        Top = 7
        Width = 32
        Height = 15
        Caption = 'Name'
      end
      object Label2: TLabel
        Left = 271
        Top = 7
        Width = 47
        Height = 15
        Caption = 'Surname'
      end
      object NameEdit: TEdit
        Left = 8
        Top = 24
        Width = 257
        Height = 33
        Hint = 'Name'
        TabOrder = 0
        Text = 'Barazzetta'
      end
      object SurNameEdit: TEdit
        Left = 271
        Top = 24
        Width = 257
        Height = 33
        Hint = 'SurName'
        TabOrder = 1
        Text = 'Carlo'
      end
      object LabeledEdit: TLabeledEdit
        Left = 8
        Top = 72
        Width = 257
        Height = 33
        EditLabel.Width = 64
        EditLabel.Height = 15
        EditLabel.Caption = 'Labeled Edit'
        TabOrder = 2
        Text = 'Text'
      end
    end
    object tsNoParent: TTabSheet
      Caption = 'Parent Font = False'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ImageIndex = 1
      ParentFont = False
      object Label3: TLabel
        Left = 12
        Top = 11
        Width = 27
        Height = 13
        Caption = 'Name'
      end
      object Label4: TLabel
        Left = 275
        Top = 11
        Width = 42
        Height = 13
        Caption = 'Surname'
      end
      object Edit1: TEdit
        Left = 12
        Top = 28
        Width = 257
        Height = 21
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        Text = 'Barazzetta'
      end
      object Edit2: TEdit
        Left = 275
        Top = 28
        Width = 257
        Height = 21
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        Text = 'Carlo'
      end
      object LabeledEdit2: TLabeledEdit
        Left = 12
        Top = 76
        Width = 257
        Height = 21
        EditLabel.Width = 58
        EditLabel.Height = 13
        EditLabel.Caption = 'Labeled Edit'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 2
        Text = 'Text'
      end
    end
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 635
    Height = 29
    ButtonHeight = 30
    ButtonWidth = 31
    Caption = 'ToolBar'
    Images = IconFontsVirtualImageList
    TabOrder = 1
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Caption = 'ToolButton1'
      ImageIndex = 5
      ImageName = 'home'
    end
    object ToolButton2: TToolButton
      Left = 31
      Top = 0
      Caption = 'ToolButton2'
      ImageIndex = 20
      ImageName = 'exit'
      OnClick = Exit1Click
    end
  end
  object MainMenu: TMainMenu
    Images = IconFontsVirtualImageList
    Left = 312
    Top = 152
    object File1: TMenuItem
      Caption = '&File'
      object SettingsMenuitem: TMenuItem
        Caption = '&New'
        ImageIndex = 13
        ImageName = 'check-bold'
      end
      object Open1: TMenuItem
        Caption = '&Open...'
        ImageIndex = 4
        ImageName = 'checkbox-marked'
      end
      object Save1: TMenuItem
        Caption = '&Save'
        ImageIndex = 2
        ImageName = 'cake-variant'
      end
      object SaveAs1: TMenuItem
        Caption = 'Save &As...'
        ImageIndex = 3
        ImageName = 'layers'
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Print1: TMenuItem
        Caption = '&Print...'
        ImageIndex = 4
        ImageName = 'checkbox-marked'
      end
      object PrintSetup1: TMenuItem
        Caption = 'P&rint Setup...'
        ImageIndex = 5
        ImageName = 'home'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
        ImageIndex = 20
        ImageName = 'exit'
        OnClick = Exit1Click
      end
    end
  end
  object IconFontsVirtualImageList: TIconFontsVirtualImageList
    FontName = 'Material Design Icons Desktop'
    Size = 24
    ImageCollection = ImageCollectionDataModule.IconFontsImageCollectionMono
    Left = 416
    Top = 152
  end
  object SVGIconVirtualImageList: TSVGIconVirtualImageList
    ImageCollection = ImageCollectionDataModule.SVGIconImageCollection
    Width = 24
    Height = 24
    Size = 24
    Left = 416
    Top = 208
  end
end
