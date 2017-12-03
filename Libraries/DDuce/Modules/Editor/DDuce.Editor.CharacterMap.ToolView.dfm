inherited frmCharacterMap: TfrmCharacterMap
  Left = -750
  Top = 249
  ActiveControl = pcMain
  BorderStyle = bsSingle
  ClientHeight = 649
  ClientWidth = 369
  PopupMode = pmAuto
  Position = poDefault
  ExplicitWidth = 375
  ExplicitHeight = 678
  PixelsPerInch = 96
  TextHeight = 13
  object pcMain: TPageControl
    Left = 0
    Top = 0
    Width = 369
    Height = 404
    ActivePage = tsANSI
    Align = alClient
    TabOrder = 0
    object tsANSI: TTabSheet
      Caption = 'ANSI'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        361
        376)
      object lblCharInfo: TLabel
        Left = 6
        Top = 345
        Width = 53
        Height = 13
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'lblCharInfo'
        Color = clBtnFace
        ParentColor = False
      end
      object grdANSI: TStringGrid
        Left = 0
        Top = 0
        Width = 357
        Height = 339
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
      end
    end
    object tsUnicode: TTabSheet
      Caption = 'Unicode'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        361
        376)
      object lblUnicodeCharInfo: TLabel
        Left = 6
        Top = 342
        Width = 91
        Height = 13
        Caption = 'lblUnicodeCharInfo'
        Color = clBtnFace
        ParentColor = False
      end
      object grdUnicode: TStringGrid
        Left = 0
        Top = 0
        Width = 357
        Height = 332
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
        Top = 338
        Width = 189
        Height = 21
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
    Width = 369
    Height = 245
    Align = alBottom
    BevelInner = bvSpace
    BevelOuter = bvSpace
    Color = clWhite
    TabOrder = 1
    object shpChar: TShape
      Left = 2
      Top = 2
      Width = 365
      Height = 241
      Align = alClient
      Brush.Style = bsClear
      Pen.Color = clSilver
      Pen.Width = 2
      ExplicitWidth = 357
    end
    object imgChar: TImage
      Left = 2
      Top = 2
      Width = 365
      Height = 241
      Align = alClient
      AutoSize = True
      Center = True
      Transparent = True
      ExplicitLeft = 6
      ExplicitTop = 6
      ExplicitWidth = 349
      ExplicitHeight = 233
    end
  end
end
