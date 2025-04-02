object frmRTTEye: TfrmRTTEye
  Left = 304
  Top = 304
  ActiveControl = btnLoad
  Caption = 'RTTEye'
  ClientHeight = 530
  ClientWidth = 863
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 13
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 863
    Height = 489
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 5
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object splVertical: TSplitter
      Left = 316
      Top = 5
      Width = 6
      Height = 479
      ResizeStyle = rsLine
      ExplicitLeft = 314
      ExplicitTop = 3
      ExplicitHeight = 483
    end
    object tvRtti: TTreeView
      Left = 322
      Top = 5
      Width = 536
      Height = 479
      Align = alClient
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
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
      Left = 5
      Top = 5
      Width = 311
      Height = 479
      Align = alLeft
      BevelInner = bvNone
      BorderStyle = bsNone
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
    Width = 863
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 5
    TabOrder = 1
    DesignSize = (
      863
      41)
    object btnLoad: TButton
      Left = 258
      Top = 6
      Width = 120
      Height = 26
      Action = actRefresh
      ImageMargins.Left = 4
      ImageMargins.Top = 1
      ImageMargins.Right = 1
      ImageMargins.Bottom = 1
      Images = imlMain
      TabOrder = 0
    end
    object btnExpand: TButton
      Left = 6
      Top = 6
      Width = 120
      Height = 26
      Action = actExpand
      ImageMargins.Left = 4
      ImageMargins.Top = 1
      ImageMargins.Right = 1
      ImageMargins.Bottom = 1
      Images = imlMain
      TabOrder = 1
    end
    object btnCollapse: TButton
      Left = 132
      Top = 6
      Width = 120
      Height = 26
      Action = actCollapse
      ImageMargins.Left = 4
      ImageMargins.Top = 1
      ImageMargins.Right = 1
      ImageMargins.Bottom = 1
      Images = imlMain
      TabOrder = 2
    end
    object edtSearch: TEdit
      Left = 510
      Top = 8
      Width = 342
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
    end
    object btnSearch: TButton
      Left = 384
      Top = 6
      Width = 120
      Height = 26
      Action = actSearch
      Default = True
      ImageMargins.Left = 4
      ImageMargins.Top = 1
      ImageMargins.Right = 1
      ImageMargins.Bottom = 1
      Images = imlMain
      TabOrder = 4
    end
  end
  object aclMain: TActionList
    Images = imlMain
    Left = 564
    Top = 36
    object actRefresh: TAction
      Caption = 'Refresh'
      ImageIndex = 2
      ImageName = 'refresh'
      OnExecute = actRefreshExecute
    end
    object actCollapse: TAction
      Caption = 'Collapse'
      ImageIndex = 1
      ImageName = 'diff-removed-16'
      OnExecute = actCollapseExecute
    end
    object actExpand: TAction
      Caption = 'Expand'
      ImageIndex = 0
      ImageName = 'diff-added-16'
      OnExecute = actExpandExecute
    end
    object actSearch: TAction
      Caption = 'Search'
      ImageIndex = 3
      ImageName = 'zoom'
      OnExecute = actSearchExecute
    end
  end
  object imcMain: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = 'diff-added-16'
        SVGText = 
          '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" v' +
          'iewBox="0 0 16 16"><path d="M2.75 1h10.5c.966 0 1.75.784 1.75 1.' +
          '75v10.5A1.75 1.75 0 0 1 13.25 15H2.75A1.75 1.75 0 0 1 1 13.25V2.' +
          '75C1 1.784 1.784 1 2.75 1Zm10.5 1.5H2.75a.25.25 0 0 0-.25.25v10.' +
          '5c0 .138.112.25.25.25h10.5a.25.25 0 0 0 .25-.25V2.75a.25.25 0 0 ' +
          '0-.25-.25ZM8 4a.75.75 0 0 1 .75.75v2.5h2.5a.75.75 0 0 1 0 1.5h-2' +
          '.5v2.5a.75.75 0 0 1-1.5 0v-2.5h-2.5a.75.75 0 0 1 0-1.5h2.5v-2.5A' +
          '.75.75 0 0 1 8 4Z"/></svg>'
      end
      item
        IconName = 'diff-removed-16'
        SVGText = 
          '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" v' +
          'iewBox="0 0 16 16"><path d="M13.25 1c.966 0 1.75.784 1.75 1.75v1' +
          '0.5A1.75 1.75 0 0 1 13.25 15H2.75A1.75 1.75 0 0 1 1 13.25V2.75C1' +
          ' 1.784 1.784 1 2.75 1ZM2.75 2.5a.25.25 0 0 0-.25.25v10.5c0 .138.' +
          '112.25.25.25h10.5a.25.25 0 0 0 .25-.25V2.75a.25.25 0 0 0-.25-.25' +
          'Zm8.5 6.25h-6.5a.75.75 0 0 1 0-1.5h6.5a.75.75 0 0 1 0 1.5Z"/></s' +
          'vg>'
      end
      item
        IconName = 'refresh'
        SVGText = 
          '<svg'#10'  xmlns="http://www.w3.org/2000/svg"'#10'  width="24"'#10'  height=' +
          '"24"'#10'  viewBox="0 0 24 24"'#10'  fill="none"'#10'  stroke="currentColor"' +
          #10'  stroke-width="2"'#10'  stroke-linecap="round"'#10'  stroke-linejoin="' +
          'round"'#10'  class="icon icon-tabler icons-tabler-outline icon-table' +
          'r-refresh"'#10'>'#10'  <path stroke="none" d="M0 0h24v24H0z" fill="none"' +
          '/>'#10'  <path d="M20 11a8.1 8.1 0 0 0 -15.5 -2m-.5 -4v4h4" />'#10'  <pa' +
          'th d="M4 13a8.1 8.1 0 0 0 15.5 2m.5 4v-4h-4" />'#10'</svg>'
      end
      item
        IconName = 'zoom'
        SVGText = 
          '<svg'#10'  xmlns="http://www.w3.org/2000/svg"'#10'  width="24"'#10'  height=' +
          '"24"'#10'  viewBox="0 0 24 24"'#10'  fill="none"'#10'  stroke="currentColor"' +
          #10'  stroke-width="2"'#10'  stroke-linecap="round"'#10'  stroke-linejoin="' +
          'round"'#10'  class="icon icon-tabler icons-tabler-outline icon-table' +
          'r-zoom"'#10'>'#10'  <path stroke="none" d="M0 0h24v24H0z" fill="none"/>'#10 +
          '  <path d="M10 10m-7 0a7 7 0 1 0 14 0a7 7 0 1 0 -14 0" />'#10'  <pat' +
          'h d="M21 21l-6 -6" />'#10'</svg>'
      end>
    Left = 860
    Top = 38
  end
  object imlMain: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'diff-added-16'
        Name = 'diff-added-16'
      end
      item
        CollectionIndex = 1
        CollectionName = 'diff-removed-16'
        Name = 'diff-removed-16'
      end
      item
        CollectionIndex = 2
        CollectionName = 'refresh'
        Name = 'refresh'
      end
      item
        CollectionIndex = 3
        CollectionName = 'zoom'
        Name = 'zoom'
      end>
    ImageCollection = imcMain
    Left = 708
    Top = 36
  end
end
