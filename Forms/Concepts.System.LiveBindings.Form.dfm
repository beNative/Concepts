object frmLiveBindings: TfrmLiveBindings
  Left = 177
  Top = 140
  ClientHeight = 459
  ClientWidth = 614
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object splVertical: TSplitter
    Left = 257
    Top = 0
    Width = 6
    Height = 440
    ExplicitLeft = 251
    ExplicitHeight = 459
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 257
    Height = 440
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object cbxControls: TComboBox
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 251
      Height = 21
      Margins.Bottom = 0
      Align = alTop
      Style = csDropDownList
      DropDownCount = 20
      TabOrder = 0
      OnChange = cbxControlsChange
    end
  end
  object pnlRight: TPanel
    Left = 263
    Top = 0
    Width = 351
    Height = 440
    Align = alClient
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object lblLabel: TLabel
      Left = 17
      Top = 97
      Width = 104
      Height = 132
      Alignment = taCenter
      AutoSize = False
      Caption = 'S'
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -20
      Font.Name = 'Arial'
      Font.Pitch = fpFixed
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      Transparent = False
      Layout = tlCenter
    end
    object trbTrackBar: TTrackBar
      Left = 127
      Top = 81
      Width = 218
      Height = 45
      Max = 100
      Min = 8
      Frequency = 10
      Position = 8
      TabOrder = 0
      OnChange = trbTrackBarChange
    end
    object trbMulti: TTrackBar
      Left = 17
      Top = 256
      Width = 150
      Height = 45
      TabOrder = 1
    end
    object pbMulti1: TProgressBar
      Left = 17
      Top = 296
      Width = 150
      Height = 17
      TabOrder = 2
    end
    object pbMulti2: TProgressBar
      Left = 17
      Top = 319
      Width = 150
      Height = 17
      TabOrder = 3
    end
  end
  object edtButtonCaption: TEdit
    Left = 280
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 2
    Text = 'ButtonCaption'
  end
  object btnButton: TButton
    Left = 407
    Top = 6
    Width = 179
    Height = 25
    TabOrder = 3
  end
  object pbProgressBar: TProgressBar
    Left = 396
    Top = 132
    Width = 207
    Height = 17
    DoubleBuffered = False
    ParentDoubleBuffered = False
    Smooth = True
    TabOrder = 4
  end
  object sbrMain: TStatusBar
    Left = 0
    Top = 440
    Width = 614
    Height = 19
    Panels = <>
    SimplePanel = True
    SimpleText = 'TPanel, Width: 348, Height: 440'
  end
  object lstBindings: TBindingsList
    Methods = <>
    OutputConverters = <>
    Left = 463
    Top = 248
    object lnkCaption: TLinkControlToProperty
      Category = 'Quick Bindings'
      Control = edtButtonCaption
      Track = True
      Component = btnButton
      ComponentProperty = 'Caption'
    end
    object bxpLabel: TBindExpression
      Category = 'Binding Expressions'
      ControlComponent = lblLabel
      SourceComponent = trbTrackBar
      SourceExpression = 'Position'
      ControlExpression = 'Font.Size'
      NotifyOutputs = False
      Direction = dirSourceToControl
    end
    object bxpProgressBar: TBindExpression
      Category = 'Binding Expressions'
      ControlComponent = pbProgressBar
      SourceComponent = trbTrackBar
      SourceExpression = 'Position'
      ControlExpression = 'Position'
      NotifyOutputs = False
      Direction = dirSourceToControl
    end
    object bxiExpressionItems: TBindExprItems
      Category = 'Binding Expressions'
      SourceComponent = trbMulti
      FormatExpressions = <
        item
          ControlExpression = 'pbMulti1.Position'
          SourceExpression = 'Position'
          Direction = dirBidirectional
        end
        item
          ControlExpression = 'pbMulti2.Posion'
          SourceExpression = 'Position'
        end>
      ClearExpressions = <>
      NotifyOutputs = False
    end
    object bxpStatusbar: TBindExpression
      Category = 'Binding Expressions'
      ControlComponent = sbrMain
      SourceComponent = pnlRight
      SourceExpression = 
        'ClassName() + '#39', Width: '#39' + ToStr(Width) + '#39', Height: '#39' + ToStr(' +
        'Height)'
      ControlExpression = 'SimpleText'
      NotifyOutputs = False
      Direction = dirSourceToControl
    end
  end
end
