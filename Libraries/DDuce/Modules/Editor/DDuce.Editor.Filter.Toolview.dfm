inherited frmFilter: TfrmFilter
  Left = 579
  Top = 257
  ClientHeight = 455
  ClientWidth = 530
  DoubleBuffered = True
  Font.Name = 'Segoe UI'
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  ExplicitWidth = 546
  ExplicitHeight = 494
  PixelsPerInch = 96
  TextHeight = 13
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 530
    Height = 432
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pnlHeader: TPanel
      Left = 0
      Top = 0
      Width = 530
      Height = 21
      Align = alTop
      AutoSize = True
      BevelOuter = bvNone
      TabOrder = 0
      object edtFilter: TEdit
        Left = 0
        Top = 0
        Width = 530
        Height = 21
        Align = alTop
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        TabOrder = 0
        OnChange = edtFilterChange
        OnKeyDown = edtFilterKeyDown
        OnKeyUp = edtFilterKeyUp
      end
    end
    object pnlView: TPanel
      Left = 0
      Top = 21
      Width = 530
      Height = 411
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
    end
  end
  object sbrMain: TStatusBar
    Left = 0
    Top = 432
    Width = 530
    Height = 23
    Panels = <>
  end
  object aclMain: TActionList
    Left = 158
    Top = 124
    object actFocusFilterText: TAction
      Caption = 'Set focus to filter text'
      ShortCut = 113
      OnExecute = actFocusFilterTextExecute
    end
  end
end
