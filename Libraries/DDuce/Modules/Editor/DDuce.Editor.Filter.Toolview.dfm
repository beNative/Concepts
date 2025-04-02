inherited frmFilter: TfrmFilter
  Left = 228
  Top = 228
  Margins.Left = 1
  Margins.Top = 1
  Margins.Right = 1
  Margins.Bottom = 1
  ClientHeight = 202
  ClientWidth = 234
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -8
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  TextHeight = 11
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 234
    Height = 191
    Margins.Left = 1
    Margins.Top = 1
    Margins.Right = 1
    Margins.Bottom = 1
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pnlHeader: TPanel
      Left = 0
      Top = 0
      Width = 234
      Height = 15
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alTop
      AutoSize = True
      BevelOuter = bvNone
      TabOrder = 0
      object edtFilter: TEdit
        Left = 0
        Top = 0
        Width = 234
        Height = 15
        Margins.Left = 1
        Margins.Top = 1
        Margins.Right = 1
        Margins.Bottom = 1
        Align = alTop
        AutoSize = False
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
      Top = 15
      Width = 234
      Height = 176
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
    end
  end
  object sbrMain: TStatusBar
    Left = 0
    Top = 191
    Width = 234
    Height = 11
    Margins.Left = 1
    Margins.Top = 1
    Margins.Right = 1
    Margins.Bottom = 1
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
