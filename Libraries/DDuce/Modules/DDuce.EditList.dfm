object EditList: TEditList
  Left = 38
  Top = 38
  ClientHeight = 319
  ClientWidth = 204
  Color = clWhite
  TransparentColorValue = clWhite
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  PopupMenu = ppmMain
  ShowHint = True
  TextHeight = 13
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 204
    Height = 319
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    TabOrder = 0
    object tlbMain: TToolBar
      Left = 0
      Top = 0
      Width = 204
      Height = 25
      GradientEndColor = clWhite
      GradientStartColor = clWhite
      Images = imlMain
      TabOrder = 0
      Transparent = True
      object btnAdd: TToolButton
        Left = 0
        Top = 0
        Action = actAdd
      end
      object btnDelete: TToolButton
        Left = 23
        Top = 0
        Action = actDelete
      end
      object btnSpacer1: TToolButton
        Left = 46
        Top = 0
        Width = 10
        ImageIndex = 8
        ImageName = 'edit'
        Style = tbsSeparator
      end
      object btnDuplicate: TToolButton
        Left = 56
        Top = 0
        Action = actDuplicate
      end
      object btnSpacer2: TToolButton
        Left = 79
        Top = 0
        Width = 10
        ImageIndex = 8
        ImageName = 'edit'
        Style = tbsSeparator
      end
      object btnMoveUp: TToolButton
        Left = 89
        Top = 0
        Action = actMoveUp
      end
      object btnMoveDown: TToolButton
        Left = 112
        Top = 0
        Action = actMoveDown
      end
      object btn1: TToolButton
        Left = 135
        Top = 0
        Width = 8
        Caption = 'btn1'
        ImageIndex = 6
        ImageName = 'check'
        Style = tbsSeparator
      end
      object btnRefresh: TToolButton
        Left = 143
        Top = 0
        Action = actRefresh
      end
      object btnExecute: TToolButton
        Left = 166
        Top = 0
        Action = actExecute
      end
    end
  end
  object aclMain: TActionList
    Images = imlMain
    Left = 27
    Top = 48
    object actAdd: TAction
      Caption = 'Add'
      Hint = 'Add a new item.'
      ImageIndex = 0
      ImageName = 'plus'
      ShortCut = 16429
      OnExecute = actAddExecute
    end
    object actDelete: TAction
      Caption = 'Delete'
      Hint = 'Delete'
      ImageIndex = 1
      ImageName = 'minus'
      ShortCut = 16430
      OnExecute = actDeleteExecute
    end
    object actDuplicate: TAction
      Caption = 'Duplicate'
      Hint = 'Duplicate the selected item.'
      ImageIndex = 2
      ImageName = 'copy'
      OnExecute = actDuplicateExecute
    end
    object actExecute: TAction
      Caption = 'Execute'
      ImageIndex = 7
      ImageName = 'zap-16'
      OnExecute = actExecuteExecute
    end
    object actRefresh: TAction
      Caption = 'Refresh'
      ImageIndex = 5
      ImageName = 'refresh'
      OnExecute = actRefreshExecute
    end
    object actEdit: TAction
      Caption = 'Edit'
      ImageIndex = 8
      ImageName = 'edit'
      ShortCut = 113
      OnExecute = actEditExecute
    end
    object actMoveUp: TAction
      Caption = 'Move up'
      Hint = 'Move up'
      ImageIndex = 3
      ImageName = 'arrow-big-up'
      OnExecute = actMoveUpExecute
    end
    object actMoveDown: TAction
      Caption = 'Move down'
      Hint = 'Move down.'
      ImageIndex = 4
      ImageName = 'arrow-big-down'
      OnExecute = actMoveDownExecute
    end
  end
  object ppmMain: TPopupMenu
    Images = imlMain
    Left = 219
    Top = 51
    object mniAdd: TMenuItem
      Action = actAdd
    end
    object mniDelete: TMenuItem
      Action = actDelete
    end
    object mniN1: TMenuItem
      Caption = '-'
    end
    object mniDuplicate: TMenuItem
      Action = actDuplicate
    end
    object mniN2: TMenuItem
      Caption = '-'
    end
    object mniMoveUp: TMenuItem
      Action = actMoveUp
    end
    object mniMoveDown: TMenuItem
      Action = actMoveDown
    end
    object mniN3: TMenuItem
      Caption = '-'
    end
    object mniRefresh: TMenuItem
      Action = actRefresh
    end
    object mniExecute: TMenuItem
      Action = actExecute
    end
  end
  object imcMain: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = 'plus'
        SVGText = 
          '<svg'#10'  xmlns="http://www.w3.org/2000/svg"'#10'  width="24"'#10'  height=' +
          '"24"'#10'  viewBox="0 0 24 24"'#10'  fill="none"'#10'  stroke="currentColor"' +
          #10'  stroke-width="2"'#10'  stroke-linecap="round"'#10'  stroke-linejoin="' +
          'round"'#10'  class="icon icon-tabler icons-tabler-outline icon-table' +
          'r-plus"'#10'>'#10'  <path stroke="none" d="M0 0h24v24H0z" fill="none"/>'#10 +
          '  <path d="M12 5l0 14" />'#10'  <path d="M5 12l14 0" />'#10'</svg>'
      end
      item
        IconName = 'minus'
        SVGText = 
          '<svg'#10'  xmlns="http://www.w3.org/2000/svg"'#10'  width="24"'#10'  height=' +
          '"24"'#10'  viewBox="0 0 24 24"'#10'  fill="none"'#10'  stroke="currentColor"' +
          #10'  stroke-width="2"'#10'  stroke-linecap="round"'#10'  stroke-linejoin="' +
          'round"'#10'  class="icon icon-tabler icons-tabler-outline icon-table' +
          'r-minus"'#10'>'#10'  <path stroke="none" d="M0 0h24v24H0z" fill="none"/>' +
          #10'  <path d="M5 12l14 0" />'#10'</svg>'
      end
      item
        IconName = 'copy'
        SVGText = 
          '<svg xmlns="http://www.w3.org/2000/svg" class="icon icon-tabler ' +
          'icon-tabler-copy" width="24" height="24" viewBox="0 0 24 24" str' +
          'oke-width="2" stroke="currentColor" fill="none" stroke-linecap="' +
          'round" stroke-linejoin="round">'#10'  <path stroke="none" d="M0 0h24' +
          'v24H0z" fill="none"/>'#10'  <path d="M7 7m0 2.667a2.667 2.667 0 0 1 ' +
          '2.667 -2.667h8.666a2.667 2.667 0 0 1 2.667 2.667v8.666a2.667 2.6' +
          '67 0 0 1 -2.667 2.667h-8.666a2.667 2.667 0 0 1 -2.667 -2.667z" /' +
          '>'#10'  <path d="M4.012 16.737a2.005 2.005 0 0 1 -1.012 -1.737v-10c0' +
          ' -1.1 .9 -2 2 -2h10c.75 0 1.158 .385 1.5 1" />'#10'</svg>'#10#10#10
      end
      item
        IconName = 'arrow-big-up'
        SVGText = 
          '<svg'#10'  xmlns="http://www.w3.org/2000/svg"'#10'  width="24"'#10'  height=' +
          '"24"'#10'  viewBox="0 0 24 24"'#10'  fill="none"'#10'  stroke="currentColor"' +
          #10'  stroke-width="2"'#10'  stroke-linecap="round"'#10'  stroke-linejoin="' +
          'round"'#10'  class="icon icon-tabler icons-tabler-outline icon-table' +
          'r-arrow-big-up"'#10'>'#10'  <path stroke="none" d="M0 0h24v24H0z" fill="' +
          'none"/>'#10'  <path d="M9 20v-8h-3.586a1 1 0 0 1 -.707 -1.707l6.586 ' +
          '-6.586a1 1 0 0 1 1.414 0l6.586 6.586a1 1 0 0 1 -.707 1.707h-3.58' +
          '6v8a1 1 0 0 1 -1 1h-4a1 1 0 0 1 -1 -1z" />'#10'</svg>'
      end
      item
        IconName = 'arrow-big-down'
        SVGText = 
          '<svg'#10'  xmlns="http://www.w3.org/2000/svg"'#10'  width="24"'#10'  height=' +
          '"24"'#10'  viewBox="0 0 24 24"'#10'  fill="none"'#10'  stroke="currentColor"' +
          #10'  stroke-width="2"'#10'  stroke-linecap="round"'#10'  stroke-linejoin="' +
          'round"'#10'  class="icon icon-tabler icons-tabler-outline icon-table' +
          'r-arrow-big-down"'#10'>'#10'  <path stroke="none" d="M0 0h24v24H0z" fill' +
          '="none"/>'#10'  <path d="M15 4v8h3.586a1 1 0 0 1 .707 1.707l-6.586 6' +
          '.586a1 1 0 0 1 -1.414 0l-6.586 -6.586a1 1 0 0 1 .707 -1.707h3.58' +
          '6v-8a1 1 0 0 1 1 -1h4a1 1 0 0 1 1 1z" />'#10'</svg>'
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
        IconName = 'check'
        SVGText = 
          '<svg'#10'  xmlns="http://www.w3.org/2000/svg"'#10'  width="24"'#10'  height=' +
          '"24"'#10'  viewBox="0 0 24 24"'#10'  fill="none"'#10'  stroke="currentColor"' +
          #10'  stroke-width="2"'#10'  stroke-linecap="round"'#10'  stroke-linejoin="' +
          'round"'#10'  class="icon icon-tabler icons-tabler-outline icon-table' +
          'r-check"'#10'>'#10'  <path stroke="none" d="M0 0h24v24H0z" fill="none"/>' +
          #10'  <path d="M5 12l5 5l10 -10" />'#10'</svg>'
      end
      item
        IconName = 'zap-16'
        SVGText = 
          '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" v' +
          'iewBox="0 0 16 16"><path d="M9.504.43a1.516 1.516 0 0 1 2.437 1.' +
          '713L10.415 5.5h2.123c1.57 0 2.346 1.909 1.22 3.004l-7.34 7.142a1' +
          '.249 1.249 0 0 1-.871.354h-.302a1.25 1.25 0 0 1-1.157-1.723L5.63' +
          '3 10.5H3.462c-1.57 0-2.346-1.909-1.22-3.004L9.503.429Zm1.047 1.0' +
          '74L3.286 8.571A.25.25 0 0 0 3.462 9H6.75a.75.75 0 0 1 .694 1.034' +
          'l-1.713 4.188 6.982-6.793A.25.25 0 0 0 12.538 7H9.25a.75.75 0 0 ' +
          '1-.683-1.06l2.008-4.418.003-.006a.036.036 0 0 0-.004-.009l-.006-' +
          '.006-.008-.001c-.003 0-.006.002-.009.004Z"/></svg>'
      end
      item
        IconName = 'edit'
        SVGText = 
          '<svg'#10'  xmlns="http://www.w3.org/2000/svg"'#10'  width="24"'#10'  height=' +
          '"24"'#10'  viewBox="0 0 24 24"'#10'  fill="none"'#10'  stroke="currentColor"' +
          #10'  stroke-width="2"'#10'  stroke-linecap="round"'#10'  stroke-linejoin="' +
          'round"'#10'  class="icon icon-tabler icons-tabler-outline icon-table' +
          'r-edit"'#10'>'#10'  <path stroke="none" d="M0 0h24v24H0z" fill="none"/>'#10 +
          '  <path d="M7 7h-1a2 2 0 0 0 -2 2v9a2 2 0 0 0 2 2h9a2 2 0 0 0 2 ' +
          '-2v-1" />'#10'  <path d="M20.385 6.585a2.1 2.1 0 0 0 -2.97 -2.97l-8.' +
          '415 8.385v3h3l8.385 -8.415z" />'#10'  <path d="M16 5l3 3" />'#10'</svg>'
      end>
    Left = 24
    Top = 168
  end
  object imlMain: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'plus'
        Name = 'plus'
      end
      item
        CollectionIndex = 1
        CollectionName = 'minus'
        Name = 'minus'
      end
      item
        CollectionIndex = 2
        CollectionName = 'copy'
        Name = 'copy'
      end
      item
        CollectionIndex = 3
        CollectionName = 'arrow-big-up'
        Name = 'arrow-big-up'
      end
      item
        CollectionIndex = 4
        CollectionName = 'arrow-big-down'
        Name = 'arrow-big-down'
      end
      item
        CollectionIndex = 5
        CollectionName = 'refresh'
        Name = 'refresh'
      end
      item
        CollectionIndex = 6
        CollectionName = 'check'
        Name = 'check'
      end
      item
        CollectionIndex = 7
        CollectionName = 'zap-16'
        Name = 'zap-16'
      end
      item
        CollectionIndex = 8
        CollectionName = 'edit'
        Name = 'edit'
      end>
    ImageCollection = imcMain
    PreserveItems = True
    Left = 120
    Top = 48
  end
end
