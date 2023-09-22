object FrmSymmetricEncryption: TFrmSymmetricEncryption
  Left = 0
  Top = 0
  Caption = 'Symmetric Encryption'
  ClientHeight = 442
  ClientWidth = 628
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  DesignSize = (
    628
    442)
  TextHeight = 15
  object ShapeResult: TShape
    Left = 24
    Top = 369
    Width = 571
    Height = 65
    Anchors = [akLeft, akTop, akRight]
    Shape = stRoundRect
    ExplicitWidth = 577
  end
  object LabelAlgo: TLabel
    Left = 24
    Top = 32
    Width = 48
    Height = 15
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Algorithm'
    ExplicitWidth = 54
  end
  object LabelKey: TLabel
    Left = 24
    Top = 115
    Width = 13
    Height = 15
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Key'
    ExplicitWidth = 19
  end
  object LabelResult: TLabel
    Left = 55
    Top = 390
    Width = 516
    Height = 20
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 20
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 522
  end
  object LabelKeySize: TLabel
    Left = 168
    Top = 76
    Width = 71
    Height = 15
    Caption = 'Key size [bits]'
  end
  object ComboBoxAlgo: TComboBox
    Left = 112
    Top = 29
    Width = 347
    Height = 23
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemIndex = 1
    TabOrder = 0
    Text = 'AesCbcPkcs7'
    OnChange = ComboBoxAlgoChange
    Items.Strings = (
      'AesCbc'
      'AesCbcPkcs7'
      'AesCcm'
      'AesEcb'
      'AesEcbPkcs7'
      'AesGcm')
    ExplicitWidth = 341
  end
  object btnCreateKey: TButton
    Left = 24
    Top = 72
    Width = 113
    Height = 25
    Caption = 'Create Key'
    TabOrder = 1
    OnClick = btnCreateKeyClick
  end
  object EditKey: TEdit
    Left = 114
    Top = 112
    Width = 475
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 2
    ExplicitWidth = 469
  end
  object EditClear: TEdit
    Left = 24
    Top = 160
    Width = 565
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    TextHint = 'Clear text'
    ExplicitWidth = 559
  end
  object btnEncrypt: TButton
    Left = 24
    Top = 208
    Width = 75
    Height = 25
    Caption = 'Encrypt'
    Enabled = False
    TabOrder = 4
    OnClick = btnEncryptClick
  end
  object EditEncrypted: TEdit
    Left = 24
    Top = 256
    Width = 565
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
    ExplicitWidth = 559
  end
  object btnDecrypt: TButton
    Left = 24
    Top = 301
    Width = 75
    Height = 25
    Caption = 'Decrypt'
    Enabled = False
    TabOrder = 6
    OnClick = btnDecryptClick
  end
  object EditResult: TEdit
    Left = 24
    Top = 340
    Width = 571
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 7
    TextHint = 'resulting clear text'
    ExplicitWidth = 565
  end
  object btnSaveEncrypt: TButton
    Left = 445
    Top = 208
    Width = 75
    Height = 25
    Caption = 'Save'
    Enabled = False
    TabOrder = 8
    OnClick = btnSaveEncryptClick
  end
  object btnLoad: TButton
    Left = 526
    Top = 208
    Width = 75
    Height = 25
    Caption = 'Load'
    Enabled = False
    TabOrder = 9
    OnClick = btnLoadClick
  end
  object btnSaveKey: TButton
    Left = 440
    Top = 72
    Width = 68
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Save Key'
    Enabled = False
    TabOrder = 10
    OnClick = btnSaveKeyClick
    ExplicitLeft = 434
  end
  object btnLoadKey: TButton
    Left = 514
    Top = 72
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Load Key'
    TabOrder = 11
    OnClick = btnLoadKeyClick
    ExplicitLeft = 508
  end
  object EditKeySize: TEdit
    Left = 256
    Top = 73
    Width = 105
    Height = 23
    ReadOnly = True
    TabOrder = 12
  end
  object cboKeySize: TComboBox
    Left = 520
    Top = 29
    Width = 69
    Height = 23
    Style = csDropDownList
    Anchors = [akRight]
    ItemIndex = 1
    TabOrder = 13
    Text = '1024'
    OnChange = ComboBoxAlgoChange
    Items.Strings = (
      '512'
      '1024'
      '2048'
      '4096')
    ExplicitLeft = 514
    ExplicitTop = 28
  end
end
