object frmTreeViewPresenterTree: TfrmTreeViewPresenterTree
  Left = 441
  Top = 465
  ActiveControl = edtFilter
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
    Left = 449
    Top = 0
    Width = 6
    Height = 688
  end
  object pnlTop: TPanel
    Left = 455
    Top = 0
    Width = 656
    Height = 688
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pnlTreeView: TPanel
      Left = 0
      Top = 40
      Width = 656
      Height = 648
      Align = alBottom
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelEdges = []
      BevelOuter = bvNone
      TabOrder = 0
    end
    object edtFilter: TLabeledEdit
      Left = 120
      Top = 8
      Width = 153
      Height = 21
      EditLabel.Width = 105
      EditLabel.Height = 13
      EditLabel.Caption = 'Filter qualified names:'
      LabelPosition = lpLeft
      TabOrder = 1
      Text = 'DDuce.Logger'
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
    Width = 449
    Height = 688
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 2
    object splHorizontal: TSplitter
      Left = 0
      Top = 130
      Width = 449
      Height = 5
      Cursor = crVSplit
      Align = alTop
    end
    object pnlTreeviewPresenter: TPanel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 443
      Height = 20
      Align = alTop
      BevelKind = bkFlat
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
      TabOrder = 0
    end
    object pnlLeftTop: TPanel
      Left = 0
      Top = 26
      Width = 449
      Height = 104
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
    end
    object pnlLeftBottom: TPanel
      Left = 0
      Top = 135
      Width = 449
      Height = 553
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 2
      object mmoDetails: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 29
        Width = 443
        Height = 521
        Align = alClient
        BorderStyle = bsNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Consolas'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        WordWrap = False
      end
      object pnlType: TPanel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 443
        Height = 20
        Align = alTop
        BevelKind = bkFlat
        BevelOuter = bvNone
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentBackground = False
        ParentFont = False
        TabOrder = 1
      end
    end
  end
  object btnExecute: TButton
    Left = 736
    Top = 5
    Width = 113
    Height = 25
    Action = actExecute
    Default = True
    ImageMargins.Left = 4
    Images = dmResources.imlMain
    TabOrder = 3
  end
  object aclMain: TActionList
    Images = dmResources.imlMain
    Left = 552
    Top = 352
    object actExecute: TAction
      Caption = 'Execute'
      ImageIndex = 518
      OnExecute = actExecuteExecute
    end
  end
end
