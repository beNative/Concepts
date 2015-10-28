object frmTreeViewPresenter: TfrmTreeViewPresenter
  Left = 441
  Top = 465
  Caption = 'Virtual treeview presenter'
  ClientHeight = 737
  ClientWidth = 667
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
  object spl1: TSplitter
    Left = 0
    Top = 505
    Width = 667
    Height = 10
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 499
  end
  object pnlTop: TPanel
    Left = 289
    Top = 0
    Width = 378
    Height = 505
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 515
    Width = 667
    Height = 222
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      667
      222)
    object lblChange: TLabel
      Left = 216
      Top = 209
      Width = 225
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Change FirstName property of item with index:'
    end
    object edtFilter: TEdit
      Left = 8
      Top = 201
      Width = 121
      Height = 21
      Anchors = [akLeft, akBottom]
      TabOrder = 0
    end
    object btnFilter: TButton
      Left = 135
      Top = 197
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Filter'
      TabOrder = 1
      OnClick = btnFilterClick
    end
    object edtIndex: TSpinEdit
      Left = 446
      Top = 200
      Width = 41
      Height = 22
      Anchors = [akLeft, akBottom]
      MaxValue = 0
      MinValue = 0
      TabOrder = 2
      Value = 0
    end
    object edtName: TEdit
      Left = 490
      Top = 201
      Width = 111
      Height = 21
      Anchors = [akLeft, akBottom]
      TabOrder = 3
    end
    object btnEvent: TButton
      Left = 598
      Top = 197
      Width = 53
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Change'
      TabOrder = 4
      OnClick = btnEventClick
    end
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 289
    Height = 505
    Align = alLeft
    BevelOuter = bvNone
    Caption = 'pnlLeft'
    TabOrder = 2
  end
end
