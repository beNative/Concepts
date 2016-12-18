object frmRTTEye: TfrmRTTEye
  Left = 350
  Top = 170
  ActiveControl = btnLoad
  Caption = 'RTTEye'
  ClientHeight = 530
  ClientWidth = 857
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
    Width = 857
    Height = 489
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
      Height = 477
      ExplicitLeft = 0
      ExplicitHeight = 427
    end
    object tvRtti: TTreeView
      Left = 326
      Top = 6
      Width = 525
      Height = 477
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
      Height = 477
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
    Top = 489
    Width = 857
    Height = 41
    Align = alBottom
    BorderWidth = 5
    TabOrder = 1
    DesignSize = (
      857
      41)
    object btnLoad: TButton
      Left = 258
      Top = 6
      Width = 120
      Height = 25
      Action = actRefresh
      ImageMargins.Left = 4
      ImageMargins.Top = 1
      ImageMargins.Right = 1
      ImageMargins.Bottom = 1
      Images = dmResources.imlMain
      TabOrder = 0
    end
    object btnExpand: TButton
      Left = 6
      Top = 6
      Width = 120
      Height = 25
      Action = actExpand
      ImageMargins.Left = 4
      ImageMargins.Top = 1
      ImageMargins.Right = 1
      ImageMargins.Bottom = 1
      Images = dmResources.imlMain
      TabOrder = 1
    end
    object btnCollapse: TButton
      Left = 132
      Top = 6
      Width = 120
      Height = 25
      Action = actCollapse
      ImageMargins.Left = 4
      ImageMargins.Top = 1
      ImageMargins.Right = 1
      ImageMargins.Bottom = 1
      Images = dmResources.imlMain
      TabOrder = 2
    end
    object EditSearch: TEdit
      Left = 510
      Top = 8
      Width = 338
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
    end
    object btnSearch: TButton
      Left = 384
      Top = 6
      Width = 120
      Height = 25
      Action = actSearch
      Default = True
      ImageMargins.Left = 4
      ImageMargins.Top = 1
      ImageMargins.Right = 1
      ImageMargins.Bottom = 1
      Images = dmResources.imlMain
      TabOrder = 4
    end
  end
  object aclMain: TActionList
    Images = dmResources.imlMain
    Left = 384
    Top = 128
    object actRefresh: TAction
      Caption = 'Refresh'
      ImageIndex = 518
      OnExecute = actRefreshExecute
    end
    object actCollapse: TAction
      Caption = 'Collapse'
      ImageIndex = 334
      OnExecute = actCollapseExecute
    end
    object actExpand: TAction
      Caption = 'Expand'
      ImageIndex = 1
      OnExecute = actExpandExecute
    end
    object actSearch: TAction
      Caption = 'Search'
      ImageIndex = 997
      OnExecute = actSearchExecute
    end
  end
end
