object frmVirtualTreeView: TfrmVirtualTreeView
  Left = 0
  Top = 0
  Caption = 'Virtual treeview'
  ClientHeight = 678
  ClientWidth = 1342
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object splVertical: TSplitter
    Left = 329
    Top = 28
    Width = 8
    Height = 631
    ExplicitTop = -19
    ExplicitHeight = 697
  end
  object sbrMain: TStatusBar
    Left = 0
    Top = 659
    Width = 1342
    Height = 19
    Panels = <>
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 28
    Width = 329
    Height = 631
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
  end
  object pnlMain: TPanel
    Left = 337
    Top = 28
    Width = 1005
    Height = 631
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object splHorizontal: TSplitter
      Left = 0
      Top = 288
      Width = 1005
      Height = 8
      Cursor = crVSplit
      Align = alTop
      ExplicitTop = 287
    end
    object pnlColumnSettings: TGridPanel
      Left = 0
      Top = 296
      Width = 1005
      Height = 335
      Align = alClient
      BevelOuter = bvNone
      ColumnCollection = <
        item
          Value = 14.285714285714290000
        end
        item
          Value = 14.285714285714290000
        end
        item
          Value = 14.285714285714290000
        end
        item
          Value = 14.285714285714290000
        end
        item
          Value = 14.285714285714290000
        end
        item
          Value = 14.285714285714290000
        end
        item
          Value = 14.285714285714290000
        end>
      ControlCollection = <
        item
          Column = 0
          Control = pnlCol0
          Row = 0
        end
        item
          Column = 1
          Control = pnlCol1
          Row = 0
        end
        item
          Column = 2
          Control = pnlCol2
          Row = 0
        end
        item
          Column = 3
          Control = pnlCol3
          Row = 0
        end
        item
          Column = 4
          Control = pnlCol4
          Row = 0
        end
        item
          Column = 5
          Control = pnlCol5
          Row = 0
        end
        item
          Column = 6
          Control = pnlCol6
          Row = 0
        end>
      RowCollection = <
        item
          SizeStyle = ssAbsolute
          Value = 20.000000000000000000
        end
        item
          Value = 100.000000000000000000
        end>
      TabOrder = 0
      object pnlCol0: TPanel
        Left = 0
        Top = 0
        Width = 143
        Height = 20
        Align = alClient
        BevelOuter = bvNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
      end
      object pnlCol1: TPanel
        Left = 143
        Top = 0
        Width = 143
        Height = 20
        Align = alClient
        BevelOuter = bvNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
      end
      object pnlCol2: TPanel
        Left = 286
        Top = 0
        Width = 143
        Height = 20
        Align = alClient
        BevelOuter = bvNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 2
      end
      object pnlCol3: TPanel
        Left = 429
        Top = 0
        Width = 143
        Height = 20
        Align = alClient
        BevelOuter = bvNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 3
      end
      object pnlCol4: TPanel
        Left = 572
        Top = 0
        Width = 143
        Height = 20
        Align = alClient
        BevelOuter = bvNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 4
      end
      object pnlCol5: TPanel
        Left = 715
        Top = 0
        Width = 143
        Height = 20
        Align = alClient
        BevelOuter = bvNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 5
      end
      object pnlCol6: TPanel
        Left = 858
        Top = 0
        Width = 147
        Height = 20
        Align = alClient
        BevelOuter = bvNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 6
      end
    end
    object pnlTreeView: TPanel
      Left = 0
      Top = 0
      Width = 1005
      Height = 288
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
    end
  end
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 1342
    Height = 28
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 3
    object lblHeader: TLabel
      Left = 0
      Top = 0
      Width = 909
      Height = 28
      Align = alLeft
      Alignment = taCenter
      AutoSize = False
      Caption = 'This form demonstrates the TVirtualStringTree control.'
      EllipsisPosition = epWordEllipsis
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
      WordWrap = True
    end
    object lblFocusedNode: TLabel
      Left = 977
      Top = 7
      Width = 75
      Height = 13
      AutoSize = False
      Caption = 'lblFocusedNode'
      Layout = tlCenter
    end
    object btnAutoSizeColumns: TButton
      Left = 712
      Top = 2
      Width = 185
      Height = 25
      Action = actAutoSizeColumns
      TabOrder = 0
    end
  end
  object aclMain: TActionList
    Left = 664
    Top = 344
    object actAutoSizeColumns: TAction
      Caption = 'AutoSizeColumns'
      OnExecute = actAutoSizeColumnsExecute
    end
  end
end
