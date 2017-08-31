object frmInterfaceImplementationByAggregation: TfrmInterfaceImplementationByAggregation
  Left = 0
  Top = 0
  Caption = 'Interface implementation by aggregation'
  ClientHeight = 124
  ClientWidth = 379
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  DesignSize = (
    379
    124)
  PixelsPerInch = 96
  TextHeight = 13
  object btnInnerMethod: TButton
    Left = 8
    Top = 8
    Width = 363
    Height = 25
    Action = actInnerMethod
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object btnOuterMethod: TButton
    Left = 8
    Top = 39
    Width = 363
    Height = 25
    Action = actOuterMethod
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object aclMain: TActionList
    Left = 16
    Top = 72
    object actInnerMethod: TAction
      Caption = 'Call method from IInnerInterface'
      OnExecute = actInnerMethodExecute
    end
    object actOuterMethod: TAction
      Caption = 'Call method from IOuterInterface'
      OnExecute = actOuterMethodExecute
    end
  end
end
