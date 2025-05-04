object CBVCLPreviewForm: TCBVCLPreviewForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  ClientHeight = 122
  ClientWidth = 354
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  TextHeight = 13
  object TabControl: TTabControl
    Left = 0
    Top = 0
    Width = 354
    Height = 122
    Align = alClient
    TabOrder = 0
    Tabs.Strings = (
      'Page1'
      'Page2'
      'Page3')
    TabIndex = 0
    ExplicitWidth = 350
    ExplicitHeight = 121
    object FNormalTextEdit: TEdit
      Left = 8
      Top = 32
      Width = 80
      Height = 21
      TabOrder = 0
      Text = 'Normal'
    end
    object FButtonNormal: TButton
      Left = 8
      Top = 60
      Width = 75
      Height = 25
      Caption = 'Normal'
      TabOrder = 3
      OnMouseDown = FButtonNormalMouseDown
      OnMouseEnter = FButtonNormalMouseEnter
      OnMouseLeave = FButtonNormalMouseLeave
      OnMouseUp = FButtonNormalMouseUp
    end
    object FButtonDisabled: TButton
      Left = 94
      Top = 60
      Width = 75
      Height = 25
      Caption = 'Disabled'
      Enabled = False
      TabOrder = 4
    end
    object FRequiredTextEdit: TEdit
      Left = 94
      Top = 32
      Width = 80
      Height = 21
      TabOrder = 1
      Text = 'Required'
    end
    object FReadOnlyTextEdit: TEdit
      Left = 182
      Top = 32
      Width = 80
      Height = 21
      TabOrder = 2
      Text = 'ReadOnly'
    end
    object CheckBox: TCheckBox
      Left = 182
      Top = 60
      Width = 97
      Height = 17
      Caption = 'Check'
      Checked = True
      State = cbChecked
      TabOrder = 5
    end
    object ScrollBar: TScrollBar
      Left = 4
      Top = 101
      Width = 346
      Height = 17
      Align = alBottom
      PageSize = 0
      TabOrder = 6
      ExplicitTop = 100
      ExplicitWidth = 342
    end
  end
  object MainMenu: TMainMenu
    Left = 216
    Top = 32
    object FMenu1: TMenuItem
      Caption = 'File'
    end
    object FMenu2: TMenuItem
      Caption = 'Edit'
    end
    object FMenu3: TMenuItem
      Caption = 'View'
    end
    object FMenu4: TMenuItem
      Caption = 'Help'
    end
  end
end
