object frmDDetours: TfrmDDetours
  Left = 0
  Top = 0
  Caption = 'DDetours'
  ClientHeight = 78
  ClientWidth = 309
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnClose = FormClose
  TextHeight = 13
  object btnUnitName: TButton
    Left = 145
    Top = 42
    Width = 156
    Height = 25
    Action = actCallDetouredMethod
    TabOrder = 0
  end
  object chkDetour1Enabled: TCheckBox
    Left = 8
    Top = 32
    Width = 131
    Height = 17
    Caption = 'Detour1 enabled'
    TabOrder = 1
    OnClick = chkDetour1EnabledClick
  end
  object chkDetour2Enabled: TCheckBox
    Left = 8
    Top = 55
    Width = 121
    Height = 18
    Caption = 'Detour2 enabled'
    TabOrder = 2
    OnClick = chkDetour2EnabledClick
  end
  object pnlHeader: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 303
    Height = 29
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentBackground = False
    ParentFont = False
    TabOrder = 3
    ExplicitWidth = 297
    object lblHeader: TLabel
      Left = 0
      Top = 0
      Width = 303
      Height = 29
      Align = alClient
      Alignment = taCenter
      Caption = 'This form demonstrates how to use the DDetours unit.'
      Layout = tlCenter
      WordWrap = True
      ExplicitWidth = 281
      ExplicitHeight = 26
    end
  end
  object aclMain: TActionList
    Left = 304
    Top = 104
    object actCallDetouredMethod: TAction
      Caption = 'Call detoured method'
      OnExecute = actCallDetouredMethodExecute
    end
  end
end
