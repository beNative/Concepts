object frmTreeViewPresenterTree: TfrmTreeViewPresenterTree
  Left = 441
  Top = 465
  Caption = 'Virtual treeview presenter'
  ClientHeight = 694
  ClientWidth = 1111
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  PixelsPerInch = 96
  TextHeight = 13
  object splVertical: TSplitter
    Left = 361
    Top = 0
    Width = 8
    Height = 632
    ExplicitLeft = 289
    ExplicitHeight = 661
  end
  object pnlTop: TPanel
    Left = 369
    Top = 0
    Width = 742
    Height = 632
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 632
    Width = 1111
    Height = 62
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 361
    Height = 632
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 2
    object splHorizontal: TSplitter
      Left = 0
      Top = 345
      Width = 361
      Height = 8
      Cursor = crVSplit
      Align = alTop
    end
    object pnlTreeviewPresenter: TPanel
      Left = 0
      Top = 0
      Width = 361
      Height = 25
      Align = alTop
      BevelOuter = bvNone
      Caption = 'TTreeviewPresenter'
      TabOrder = 0
    end
    object pnlLeftTop: TPanel
      Left = 0
      Top = 25
      Width = 361
      Height = 320
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
    end
    object pnlLeftBottom: TPanel
      Left = 0
      Top = 353
      Width = 361
      Height = 279
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 2
    end
  end
  object aclMain: TActionList
    Left = 552
    Top = 352
  end
end
