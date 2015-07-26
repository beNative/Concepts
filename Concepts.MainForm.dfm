object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Concepts'
  ClientHeight = 468
  ClientWidth = 788
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    788
    468)
  PixelsPerInch = 96
  TextHeight = 13
  object btnExecute: TBitBtn
    AlignWithMargins = True
    Left = 3
    Top = 421
    Width = 782
    Height = 25
    Action = actExecuteModal
    Align = alBottom
    Caption = 'Execute modal'
    Default = True
    TabOrder = 0
  end
  object btnExecute1: TBitBtn
    AlignWithMargins = True
    Left = 3
    Top = 391
    Width = 782
    Height = 24
    Action = actExecute
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Execute non-modal'
    Default = True
    TabOrder = 1
  end
  object pnlVST: TPanel
    Left = 0
    Top = 0
    Width = 788
    Height = 385
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 2
    object edtFilter: TEdit
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 782
      Height = 21
      Align = alTop
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnChange = edtFilterChange
      OnKeyDown = edtFilterKeyDown
      OnKeyUp = edtFilterKeyUp
    end
  end
  object sbrMain: TStatusBar
    Left = 0
    Top = 449
    Width = 788
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object aclMain: TActionList
    Left = 192
    Top = 128
    object actExecute: TAction
      Caption = 'Execute modal'
      OnExecute = actExecuteExecute
    end
    object actClose: TAction
      Caption = 'Close'
    end
    object actExecuteModal: TAction
      Caption = 'Execute modal'
      OnExecute = actExecuteModalExecute
    end
  end
end
