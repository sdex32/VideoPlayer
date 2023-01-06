object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 671
  ClientWidth = 691
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 616
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Edit1: TEdit
    Left = 24
    Top = 512
    Width = 515
    Height = 21
    TabOrder = 0
    Text = 'Edit1'
  end
  object Button5: TButton
    Left = 24
    Top = 539
    Width = 75
    Height = 25
    Caption = 'Play'
    TabOrder = 1
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 105
    Top = 539
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 2
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 186
    Top = 539
    Width = 75
    Height = 25
    Caption = 'Pause'
    TabOrder = 3
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 545
    Top = 510
    Width = 75
    Height = 25
    Caption = 'Browse'
    TabOrder = 4
    OnClick = Button8Click
  end
  object ScrollBar1: TScrollBar
    Left = 24
    Top = 570
    Width = 515
    Height = 17
    Max = 1000000
    PageSize = 0
    SmallChange = 10000
    TabOrder = 5
    OnChange = ScrollBar1Change
    OnScroll = ScrollBar1Scroll
  end
  object Button9: TButton
    Left = 312
    Top = 539
    Width = 75
    Height = 25
    Caption = 'Mute'
    TabOrder = 6
    OnClick = Button9Click
  end
  object Button10: TButton
    Left = 440
    Top = 634
    Width = 75
    Height = 25
    Caption = 'Button10'
    TabOrder = 7
    OnClick = Button10Click
  end
  object Button11: TButton
    Left = 359
    Top = 634
    Width = 75
    Height = 25
    Caption = 'Button11'
    TabOrder = 8
    OnClick = Button11Click
  end
  object OpenDialog1: TOpenDialog
    Left = 640
    Top = 520
  end
  object ApplicationEvents1: TApplicationEvents
    OnMessage = ApplicationEvents1Message
    Left = 640
    Top = 568
  end
end
