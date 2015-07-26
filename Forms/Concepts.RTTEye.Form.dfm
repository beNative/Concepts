object frmRTTEye: TfrmRTTEye
  Left = 350
  Top = 170
  ActiveControl = btnLoad
  Caption = 'RTTEye'
  ClientHeight = 480
  ClientWidth = 695
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 695
    Height = 439
    Align = alClient
    BorderWidth = 5
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object splVertical: TSplitter
      Left = 317
      Top = 6
      Width = 9
      Height = 427
      ExplicitLeft = 0
    end
    object tvRtti: TTreeView
      Left = 326
      Top = 6
      Width = 363
      Height = 427
      Align = alClient
      DoubleBuffered = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Consolas'
      Font.Style = []
      Indent = 19
      ParentDoubleBuffered = False
      ParentFont = False
      ReadOnly = True
      TabOrder = 0
      OnChange = tvRttiChange
      OnCustomDrawItem = tvRttiCustomDrawItem
      OnDblClick = tvRttiDblClick
    end
    object lvRtti: TListView
      Left = 6
      Top = 6
      Width = 311
      Height = 427
      Align = alLeft
      Color = clBtnFace
      Columns = <
        item
          Caption = 'Name'
          Width = 150
        end
        item
          Caption = 'Value'
          Width = 150
        end>
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Segoe UI'
      Font.Style = []
      ReadOnly = True
      RowSelect = True
      ParentFont = False
      TabOrder = 1
      ViewStyle = vsReport
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 439
    Width = 695
    Height = 41
    Align = alBottom
    BorderWidth = 5
    TabOrder = 1
    DesignSize = (
      695
      41)
    object btnLoad: TButton
      Left = 10
      Top = 6
      Width = 75
      Height = 25
      Action = actLoad
      TabOrder = 0
    end
    object btnExpand1: TButton
      Left = 91
      Top = 6
      Width = 75
      Height = 25
      Action = actExpand
      TabOrder = 1
    end
    object btnCollapse1: TButton
      Left = 172
      Top = 6
      Width = 75
      Height = 25
      Action = actCollapse
      TabOrder = 2
    end
    object EditSearch: TEdit
      Left = 391
      Top = 6
      Width = 217
      Height = 21
      Anchors = [akTop, akRight]
      TabOrder = 3
    end
    object btnSearch: TButton
      Left = 614
      Top = 6
      Width = 75
      Height = 25
      Action = actSearch
      Anchors = [akTop, akRight]
      Default = True
      TabOrder = 4
    end
  end
  object aclMain: TActionList
    Left = 384
    Top = 128
    object actLoad: TAction
      Caption = 'Load'
      OnExecute = actLoadExecute
    end
    object actCollapse: TAction
      Caption = 'Collapse'
      OnExecute = actCollapseExecute
    end
    object actExpand: TAction
      Caption = 'Expand'
      OnExecute = actExpandExecute
    end
    object actSearch: TAction
      Caption = 'Search'
      OnExecute = actSearchExecute
    end
  end
end
