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
    Top = 694
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
    Height = 694
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitHeight = 505
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 704
    Width = 667
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      667
      33)
    object lblChange: TLabel
      Left = 216
      Top = 20
      Width = 225
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Change FirstName property of item with index:'
      ExplicitTop = 209
    end
    object edtFilter: TEdit
      Left = 8
      Top = 12
      Width = 121
      Height = 21
      Anchors = [akLeft, akBottom]
      TabOrder = 0
      ExplicitTop = 201
    end
    object btnFilter: TButton
      Left = 135
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Filter'
      TabOrder = 1
      OnClick = btnFilterClick
      ExplicitTop = 197
    end
    object edtIndex: TSpinEdit
      Left = 446
      Top = 11
      Width = 41
      Height = 22
      Anchors = [akLeft, akBottom]
      MaxValue = 0
      MinValue = 0
      TabOrder = 2
      Value = 0
      ExplicitTop = 200
    end
    object edtName: TEdit
      Left = 490
      Top = 12
      Width = 111
      Height = 21
      Anchors = [akLeft, akBottom]
      TabOrder = 3
      ExplicitTop = 201
    end
    object btnEvent: TButton
      Left = 598
      Top = 8
      Width = 53
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Change'
      TabOrder = 4
      OnClick = btnEventClick
      ExplicitTop = 197
    end
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 289
    Height = 694
    Align = alLeft
    BevelOuter = bvNone
    Caption = 'pnlLeft'
    TabOrder = 2
    ExplicitHeight = 505
  end
end
