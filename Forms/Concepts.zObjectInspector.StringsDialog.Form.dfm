object frmStringsDialog: TfrmStringsDialog
  Left = 0
  Top = 0
  ActiveControl = mmoMain
  Caption = 'String editor'
  ClientHeight = 411
  ClientWidth = 570
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PopupMode = pmAuto
  Position = poMainFormCenter
  ShowHint = True
  DesignSize = (
    570
    411)
  PixelsPerInch = 96
  TextHeight = 13
  object btnOk: TButton
    Left = 318
    Top = 378
    Width = 120
    Height = 25
    Action = actOK
    Anchors = [akRight, akBottom]
    Default = True
    TabOrder = 0
    ExplicitLeft = 584
  end
  object btnCancel: TButton
    Left = 442
    Top = 378
    Width = 120
    Height = 25
    Action = actCancel
    Anchors = [akRight, akBottom]
    TabOrder = 1
    ExplicitLeft = 708
  end
  object mmoMain: TMemo
    Left = 8
    Top = 8
    Width = 554
    Height = 353
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 2
    ExplicitWidth = 820
  end
  object aclMain: TActionList
    Left = 416
    Top = 208
    object actOK: TAction
      Caption = '&OK'
      OnExecute = actOKExecute
    end
    object actCancel: TAction
      Caption = '&Cancel'
      OnExecute = actCancelExecute
    end
  end
end
