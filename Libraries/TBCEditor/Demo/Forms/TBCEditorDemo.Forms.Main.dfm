inherited MainForm: TMainForm
  Caption = ''
  ClientHeight = 644
  ClientWidth = 1100
  Color = clWhite
  Position = poScreenCenter
  ShowHint = True
  OnShow = FormShow
  ExplicitWidth = 1116
  ExplicitHeight = 683
  PixelsPerInch = 96
  TextHeight = 13
  inherited stat1: TStatusBar
    Top = 625
    Width = 1100
    ExplicitLeft = 0
    ExplicitTop = 625
    ExplicitWidth = 1100
  end
  inherited ApplicationEvents: TApplicationEvents
    OnMessage = ApplicationEventsMessage
    Left = 88
    Top = 88
  end
  inherited ActionList: TActionList
    Left = 178
    Top = 92
    object ActionSearch: TAction
      Caption = 'ActionSearch'
      ShortCut = 16454
      OnExecute = ActionSearchExecute
    end
    object ActionFileOpen: TAction
      Caption = 'Open...'
      ImageIndex = 1
      ShortCut = 16463
      OnExecute = ActionFileOpenExecute
    end
    object ActionPreview: TAction
      Caption = 'Print preview...'
      ImageIndex = 10
      OnExecute = ActionPreviewExecute
    end
    object ActionSkins: TAction
      Caption = 'Skins...'
      ImageIndex = 76
      OnExecute = ActionSkinsExecute
    end
    object ActionFindNext: TAction
      Hint = 'Find next'
      ImageIndex = 37
      ShortCut = 114
      OnExecute = ActionFindNextExecute
    end
    object ActionFindPrevious: TAction
      Hint = 'Find previous'
      ImageIndex = 38
      ShortCut = 8306
      OnExecute = ActionFindPreviousExecute
    end
    object ActionOptions: TAction
      Hint = 'Options'
      ImageIndex = 78
      OnExecute = ActionOptionsExecute
    end
    object ActionClose: TAction
      Hint = 'Close'
      OnExecute = ActionCloseExecute
    end
  end
  object PopupMenuFile: TPopupMenu
    Left = 84
    Top = 164
    object MenuItemFileOpen: TMenuItem
      Action = ActionFileOpen
      RadioItem = True
    end
    object MenuItemSeparator1: TMenuItem
      Caption = '-'
    end
    object MenuItemPrintPreview: TMenuItem
      Action = ActionPreview
      RadioItem = True
    end
    object MenuItemSkins: TMenuItem
      Action = ActionSkins
    end
    object MenuItemSeparator2: TMenuItem
      Caption = '-'
    end
    object MenuItemExit: TMenuItem
      Action = ActionFileExit
    end
  end
  object PopupMenuDummy: TPopupMenu
    Left = 84
    Top = 220
  end
  object MultiStringHolderFileTypes: TBCMultiStringHolder
    MultipleStrings = <
      item
        Name = 'Assembler (68HC11)'
        Strings.Strings = (
          '.asm')
      end
      item
        Name = 'AutoIt v3'
        Strings.Strings = (
          '.au3')
      end
      item
        Name = 'AWK'
        Strings.Strings = (
          '.awk')
      end
      item
        Name = 'C#'
        Strings.Strings = (
          '.cs')
      end
      item
        Name = 'C++'
        Strings.Strings = (
          '.c;.cpp;.h;.hpp')
      end
      item
        Name = 'CSS'
        Strings.Strings = (
          '.css')
      end
      item
        Name = 'Delphi Form Module'
        Strings.Strings = (
          '.dfm')
      end
      item
        Name = 'HTML with Scripts'
        Strings.Strings = (
          '.htm;.html')
      end
      item
        Name = 'Java'
        Strings.Strings = (
          '.java')
      end
      item
        Name = 'JavaScript'
        Strings.Strings = (
          '.js')
      end
      item
        Name = 'JSON'
        Strings.Strings = (
          '.json')
      end
      item
        Name = 'MS-DOS Batch'
        Strings.Strings = (
          '.bat')
      end
      item
        Name = 'Object Pascal'
        Strings.Strings = (
          '.pas;.dpr')
      end
      item
        Name = 'Perl'
        Strings.Strings = (
          '.pl')
      end
      item
        Name = 'PHP'
        Strings.Strings = (
          '.php')
      end
      item
        Name = 'Python'
        Strings.Strings = (
          '.py')
      end
      item
        Name = 'SQL (Standard)'
        Strings.Strings = (
          '.sql')
      end
      item
        Name = 'Visual Basic'
        Strings.Strings = (
          '.vb')
      end
      item
        Name = 'XML'
        Strings.Strings = (
          '.xml')
      end>
    Left = 340
    Top = 52
  end
end
