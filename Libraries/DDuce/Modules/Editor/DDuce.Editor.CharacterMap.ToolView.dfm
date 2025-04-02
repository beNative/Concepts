inherited frmCharacterMap: TfrmCharacterMap
  Left = 114
  Top = 114
  ActiveControl = pcMain
  BorderStyle = bsSingle
  ClientHeight = 649
  ClientWidth = 375
  PopupMode = pmAuto
  Position = poDefault
  StyleElements = [seFont, seClient, seBorder]
  ExplicitWidth = 391
  ExplicitHeight = 688
  TextHeight = 15
  object pcMain: TPageControl
    Left = 0
    Top = 0
    Width = 375
    Height = 404
    ActivePage = tsANSI
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 369
    ExplicitHeight = 387
    object tsANSI: TTabSheet
      Caption = 'ANSI'
      DesignSize = (
        367
        374)
      object lblCharInfo: TLabel
        Left = 6
        Top = 357
        Width = 59
        Height = 15
        Margins.Bottom = 2
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'lblCharInfo'
        Color = clBtnFace
        ParentColor = False
      end
      object grdANSI: TStringGrid
        Left = 0
        Top = 0
        Width = 357
        Height = 348
        Anchors = [akLeft, akTop, akRight, akBottom]
        BorderStyle = bsNone
        ColCount = 17
        DefaultColWidth = 25
        RowCount = 15
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnKeyUp = grdANSIKeyUp
        OnMouseDown = grdANSIMouseDown
        OnMouseMove = grdANSIMouseMove
        OnSelectCell = grdANSISelectCell
        ExplicitWidth = 351
        ExplicitHeight = 331
      end
    end
    object tsUnicode: TTabSheet
      Caption = 'Unicode'
      DesignSize = (
        367
        374)
      object lblUnicodeCharInfo: TLabel
        Left = 6
        Top = 342
        Width = 103
        Height = 15
        Margins.Bottom = 2
        Caption = 'lblUnicodeCharInfo'
        Color = clBtnFace
        ParentColor = False
      end
      object grdUnicode: TStringGrid
        Left = 0
        Top = 0
        Width = 357
        Height = 340
        Anchors = [akLeft, akTop, akRight, akBottom]
        ColCount = 16
        DefaultColWidth = 25
        FixedCols = 0
        RowCount = 15
        FixedRows = 0
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnKeyUp = grdUnicodeKeyUp
        OnMouseDown = grdUnicodeMouseDown
        OnMouseMove = grdUnicodeMouseMove
        OnSelectCell = grdUnicodeSelectCell
      end
      object cbxUnicodeRange: TComboBox
        Left = 162
        Top = 346
        Width = 189
        Height = 23
        Style = csDropDownList
        Anchors = [akRight]
        DropDownCount = 30
        TabOrder = 1
        OnSelect = cbxUnicodeRangeSelect
      end
    end
  end
  object pnlChar: TPanel
    Left = 0
    Top = 404
    Width = 375
    Height = 245
    Align = alBottom
    BevelOuter = bvNone
    Color = clWhite
    TabOrder = 1
    ExplicitTop = 387
    ExplicitWidth = 369
    object shpChar: TShape
      Left = 0
      Top = 0
      Width = 375
      Height = 245
      Align = alClient
      Brush.Style = bsClear
      Pen.Color = clSilver
      Pen.Width = 2
    end
    object imgChar: TImage
      Left = 0
      Top = 0
      Width = 375
      Height = 245
      Align = alClient
      AutoSize = True
      Center = True
      Transparent = True
    end
  end
end
