inherited OptionsEditorOptionsFrame: TOptionsEditorOptionsFrame
  Width = 182
  Height = 202
  object Panel: TBCPanel [0]
    AlignWithMargins = True
    Left = 4
    Top = 0
    Width = 178
    Height = 202
    Margins.Left = 4
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    BevelOuter = bvNone
    Color = clWindow
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 0
    SkinData.SkinSection = 'CHECKBOX'
    object StickyLabelAutoIndent: TsStickyLabel
      Left = 0
      Top = 4
      Width = 120
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSize = False
      Caption = 'Auto indent'
      ParentColor = False
      AttachTo = SliderAutoIndent
      Gap = 8
    end
    object StickyLabelAutoSave: TsStickyLabel
      Left = 0
      Top = 27
      Width = 120
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSize = False
      Caption = 'Auto save'
      ParentColor = False
      AttachTo = SliderAutoSave
      Gap = 8
    end
    object StickyLabelDragDropEditing: TsStickyLabel
      Left = 0
      Top = 50
      Width = 120
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSize = False
      Caption = 'Drag drop editing'
      ParentColor = False
      AttachTo = SliderDragDropEditing
      Gap = 8
    end
    object StickyLabelDropFiles: TsStickyLabel
      Left = 0
      Top = 73
      Width = 120
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSize = False
      Caption = 'Drop files'
      ParentColor = False
      AttachTo = SliderDropFiles
      Gap = 8
    end
    object StickyLabelGroupUndo: TsStickyLabel
      Left = 0
      Top = 96
      Width = 120
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSize = False
      Caption = 'Group undo'
      ParentColor = False
      AttachTo = SliderGroupUndo
      Gap = 8
    end
    object StickyLabelTrimTrailingSpaces: TsStickyLabel
      Left = 0
      Top = 119
      Width = 120
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSize = False
      Caption = 'Trim trailing spaces'
      ParentColor = False
      AttachTo = SliderTrimTrailingSpaces
      Gap = 8
    end
    object StickyLabelUndoAfterSave: TsStickyLabel
      Left = 0
      Top = 142
      Width = 120
      Height = 13
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSize = False
      Caption = 'Undo after save'
      ParentColor = False
      AttachTo = SliderUndoAfterSave
      Gap = 8
    end
    object EditLineSpacing: TBCEdit
      Left = 0
      Top = 181
      Width = 64
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSize = False
      TabOrder = 0
      Text = '0'
      SkinData.SkinSection = 'EDIT'
      BoundLabel.Active = True
      BoundLabel.Caption = 'Line spacing'
      BoundLabel.Indent = 4
      BoundLabel.Layout = sclTopLeft
      EnterToTab = False
      OnlyNumbers = True
      NumbersWithDots = False
      NumbersWithSpots = False
      ErrorColor = 14803198
      NumbersAllowMinus = False
      NumbersAllowPlus = False
    end
    object SliderAutoIndent: TsSlider
      Left = 128
      Top = 0
      Width = 50
      AutoSize = True
      TabOrder = 1
      ImageIndexOff = 0
      ImageIndexOn = 0
      FontOn.Charset = DEFAULT_CHARSET
      FontOn.Color = clWindowText
      FontOn.Height = -11
      FontOn.Name = 'Tahoma'
      FontOn.Style = []
      SliderCaptionOn = 'Yes'
      SliderCaptionOff = 'No'
    end
    object SliderAutoSave: TsSlider
      Left = 128
      Top = 23
      Width = 50
      AutoSize = True
      TabOrder = 2
      ImageIndexOff = 0
      ImageIndexOn = 0
      FontOn.Charset = DEFAULT_CHARSET
      FontOn.Color = clWindowText
      FontOn.Height = -11
      FontOn.Name = 'Tahoma'
      FontOn.Style = []
      SliderCaptionOn = 'Yes'
      SliderCaptionOff = 'No'
    end
    object SliderDragDropEditing: TsSlider
      Left = 128
      Top = 46
      Width = 50
      AutoSize = True
      TabOrder = 3
      ImageIndexOff = 0
      ImageIndexOn = 0
      FontOn.Charset = DEFAULT_CHARSET
      FontOn.Color = clWindowText
      FontOn.Height = -11
      FontOn.Name = 'Tahoma'
      FontOn.Style = []
      SliderCaptionOn = 'Yes'
      SliderCaptionOff = 'No'
    end
    object SliderDropFiles: TsSlider
      Left = 128
      Top = 69
      Width = 50
      AutoSize = True
      TabOrder = 4
      ImageIndexOff = 0
      ImageIndexOn = 0
      FontOn.Charset = DEFAULT_CHARSET
      FontOn.Color = clWindowText
      FontOn.Height = -11
      FontOn.Name = 'Tahoma'
      FontOn.Style = []
      SliderCaptionOn = 'Yes'
      SliderCaptionOff = 'No'
    end
    object SliderGroupUndo: TsSlider
      Left = 128
      Top = 92
      Width = 50
      AutoSize = True
      TabOrder = 5
      ImageIndexOff = 0
      ImageIndexOn = 0
      FontOn.Charset = DEFAULT_CHARSET
      FontOn.Color = clWindowText
      FontOn.Height = -11
      FontOn.Name = 'Tahoma'
      FontOn.Style = []
      SliderCaptionOn = 'Yes'
      SliderCaptionOff = 'No'
    end
    object SliderTrimTrailingSpaces: TsSlider
      Left = 128
      Top = 115
      Width = 50
      AutoSize = True
      TabOrder = 6
      ImageIndexOff = 0
      ImageIndexOn = 0
      FontOn.Charset = DEFAULT_CHARSET
      FontOn.Color = clWindowText
      FontOn.Height = -11
      FontOn.Name = 'Tahoma'
      FontOn.Style = []
      SliderCaptionOn = 'Yes'
      SliderCaptionOff = 'No'
    end
    object SliderUndoAfterSave: TsSlider
      Left = 128
      Top = 138
      Width = 50
      AutoSize = True
      TabOrder = 7
      ImageIndexOff = 0
      ImageIndexOn = 0
      FontOn.Charset = DEFAULT_CHARSET
      FontOn.Color = clWindowText
      FontOn.Height = -11
      FontOn.Name = 'Tahoma'
      FontOn.Style = []
      SliderCaptionOn = 'Yes'
      SliderCaptionOff = 'No'
    end
  end
  inherited FrameAdapter: TsFrameAdapter
    Left = 134
    Top = 154
  end
end
