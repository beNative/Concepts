object ChildForm: TChildForm
  Left = 0
  Top = 0
  Caption = 'ChildForm'
  ClientHeight = 299
  ClientWidth = 588
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnAfterMonitorDpiChanged = FormAfterMonitorDpiChanged
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 588
    Height = 258
    Align = alClient
    AutoSize = False
    Caption = 'Label1'
    ExplicitWidth = 561
  end
  object Panel1: TPanel
    Left = 0
    Top = 258
    Width = 588
    Height = 41
    Align = alBottom
    BevelOuter = bvLowered
    TabOrder = 0
    DesignSize = (
      588
      41)
    object Edit1: TEdit
      Left = 8
      Top = 8
      Width = 281
      Height = 21
      TabOrder = 0
      Text = 'Edit1'
    end
    object Button1: TButton
      Left = 448
      Top = 6
      Width = 131
      Height = 25
      Action = acCloseChildForm
      Anchors = [akTop, akRight]
      TabOrder = 1
    end
  end
  object ActionList: TActionList
    OnUpdate = ActionListUpdate
    Left = 288
    Top = 128
    object acCloseChildForm: TAction
      Caption = 'Close Child Form'
      OnExecute = acCloseChildFormExecute
    end
  end
end
