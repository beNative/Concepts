object frmTreeViewPresenterList: TfrmTreeViewPresenterList
  Left = 441
  Top = 465
  Caption = 'Virtual treeview presenter list'
  ClientHeight = 694
  ClientWidth = 1111
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poDesigned
  TextHeight = 13
  object splVertical: TSplitter
    Left = 361
    Top = 0
    Width = 8
    Height = 688
    ExplicitLeft = 289
    ExplicitHeight = 661
  end
  object pnlTop: TPanel
    Left = 369
    Top = 0
    Width = 742
    Height = 688
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pnlVirtualStringTreeTitle: TPanel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 736
      Height = 20
      Align = alTop
      BevelOuter = bvNone
      Caption = 'TVirtualStringTree'
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentBackground = False
      ParentFont = False
      TabOrder = 0
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 688
    Width = 1111
    Height = 6
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 361
    Height = 688
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 2
    object splHorizontal: TSplitter
      Left = 0
      Top = 346
      Width = 361
      Height = 8
      Cursor = crVSplit
      Align = alTop
      ExplicitTop = 345
    end
    object pnlLeftTop: TPanel
      Left = 0
      Top = 26
      Width = 361
      Height = 320
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
    end
    object pnlLeftBottom: TPanel
      Left = 0
      Top = 354
      Width = 361
      Height = 334
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
    end
    object pnlTreeviewPresenter: TPanel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 355
      Height = 20
      Align = alTop
      BevelOuter = bvNone
      Caption = 'TTreeviewPresenter'
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentBackground = False
      ParentFont = False
      TabOrder = 2
    end
  end
  object aclMain: TActionList
    Left = 552
    Top = 352
  end
end
