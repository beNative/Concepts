object frmInterfaceImplementationByAggregation: TfrmInterfaceImplementationByAggregation
  Left = 0
  Top = 0
  Caption = 'Interface implementation by aggregation'
  ClientHeight = 104
  ClientWidth = 379
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  DesignSize = (
    379
    104)
  PixelsPerInch = 96
  TextHeight = 13
  object btnInnerMethod: TButton
    Left = 8
    Top = 40
    Width = 363
    Height = 25
    Action = actInnerMethod
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object btnOuterMethod: TButton
    Left = 8
    Top = 71
    Width = 363
    Height = 25
    Action = actOuterMethod
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object pnlHeader: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 373
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
    TabOrder = 2
    ExplicitLeft = 0
    ExplicitTop = 0
    ExplicitWidth = 379
    object lblHeader: TLabel
      Left = 0
      Top = 0
      Width = 373
      Height = 29
      Align = alClient
      Alignment = taCenter
      Caption = 
        'This form demonstrates how interface methods can be implemented ' +
        'by delegation.'
      Layout = tlCenter
      WordWrap = True
      ExplicitWidth = 312
      ExplicitHeight = 26
    end
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
