object frmAsymmetricEncryption: TfrmAsymmetricEncryption
  Left = 0
  Top = 0
  Caption = 'Asymmetric Encryption'
  ClientHeight = 505
  ClientWidth = 622
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  DesignSize = (
    622
    505)
  TextHeight = 15
  object LabelAlgo: TLabel
    Left = 24
    Top = 32
    Width = 54
    Height = 15
    Caption = 'Algorithm'
  end
  object ShapeResult: TShape
    Left = 24
    Top = 416
    Width = 571
    Height = 65
    Anchors = [akLeft, akTop, akRight]
    Shape = stRoundRect
    ExplicitWidth = 577
  end
  object LabelResult: TLabel
    Left = 55
    Top = 440
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
  object LabelPublicKey: TLabel
    Left = 24
    Top = 115
    Width = 55
    Height = 15
    Caption = 'Public Key'
  end
  object LabelKeySize: TLabel
    Left = 152
    Top = 76
    Width = 71
    Height = 15
    Caption = 'Key size [bits]'
  end
  object LabelPrivateKey: TLabel
    Left = 24
    Top = 144
    Width = 58
    Height = 15
    Caption = 'Private Key'
  end
  object EditClear: TEdit
    Left = 24
    Top = 168
    Width = 565
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    TextHint = 'Clear text'
  end
  object btnEncrypt: TButton
    Left = 24
    Top = 216
    Width = 75
    Height = 25
    Caption = 'Encrypt'
    Enabled = False
    TabOrder = 1
    OnClick = btnEncryptClick
  end
  object EditEncrypted: TEdit
    Left = 24
    Top = 264
    Width = 565
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object btnDecrypt: TButton
    Left = 24
    Top = 312
    Width = 75
    Height = 25
    Caption = 'Decrypt'
    Enabled = False
    TabOrder = 3
    OnClick = btnDecryptClick
  end
  object ComboBoxAlgo: TComboBox
    Left = 112
    Top = 29
    Width = 477
    Height = 23
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemIndex = 0
    TabOrder = 4
    Text = 'RsaPkcs1'
    OnChange = ComboBoxAlgoChange
    Items.Strings = (
      'RsaPkcs1'
      'RsaOaepSha1'
      'RsaOaepSha256'
      'RsaOaepSha384'
      'RsaOaepSha512')
  end
  object EditResult: TEdit
    Left = 24
    Top = 368
    Width = 571
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 5
    TextHint = 'resulting clear text'
  end
  object btnCreateKeys: TButton
    Left = 24
    Top = 72
    Width = 113
    Height = 25
    Caption = 'Create Key Pair'
    TabOrder = 6
    OnClick = btnCreateKeysClick
  end
  object EditPublicKey: TEdit
    Left = 120
    Top = 112
    Width = 469
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 7
  end
  object EditKeySize: TEdit
    Left = 240
    Top = 73
    Width = 73
    Height = 23
    ReadOnly = True
    TabOrder = 8
  end
  object EditPrivateKey: TEdit
    Left = 120
    Top = 139
    Width = 469
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 9
  end
  object btnSaveKeys: TButton
    Left = 359
    Top = 72
    Width = 68
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Save Keys'
    Enabled = False
    TabOrder = 10
    OnClick = btnSaveKeysClick
  end
  object btnSaveEncrypt: TButton
    Left = 439
    Top = 216
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Save'
    Enabled = False
    TabOrder = 11
    OnClick = btnSaveEncryptClick
  end
  object btnLoadEncrypt: TButton
    Left = 520
    Top = 216
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Load'
    Enabled = False
    TabOrder = 12
    OnClick = btnLoadEncryptClick
  end
  object btnLoadKeys: TButton
    Left = 433
    Top = 72
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Load Keys'
    TabOrder = 13
    OnClick = btnLoadKeysClick
  end
  object btnLoadPubKey: TButton
    Left = 520
    Top = 72
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Load Public'
    TabOrder = 14
    OnClick = btnLoadPubKeyClick
  end
end
