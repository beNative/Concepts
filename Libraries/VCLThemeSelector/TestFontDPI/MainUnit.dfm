object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 299
  ClientWidth = 588
  Color = clBtnFace
  ParentFont = True
  OnAfterMonitorDpiChanged = FormAfterMonitorDpiChanged
  OnShow = FormShow
  TextHeight = 15
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 588
    Height = 258
    Align = alClient
    AutoSize = False
    Caption = 'Label1'
    WordWrap = True
    ExplicitTop = 2
  end
  object Panel1: TPanel
    Left = 0
    Top = 258
    Width = 588
    Height = 41
    Align = alBottom
    BevelOuter = bvLowered
    TabOrder = 0
    ExplicitTop = 257
    ExplicitWidth = 584
    DesignSize = (
      588
      41)
    object Edit1: TEdit
      Left = 8
      Top = 8
      Width = 281
      Height = 23
      TabOrder = 0
      Text = 'Edit1'
    end
    object Button1: TButton
      Left = 448
      Top = 6
      Width = 131
      Height = 25
      Action = acOpenChildForm
      Anchors = [akTop, akRight]
      TabOrder = 1
      ExplicitLeft = 444
    end
  end
  object ActionList: TActionList
    OnUpdate = ActionListUpdate
    Left = 288
    Top = 128
    object acOpenChildForm: TAction
      Caption = 'Open Child Form'
      OnExecute = acOpenChildFormExecute
    end
  end
end
