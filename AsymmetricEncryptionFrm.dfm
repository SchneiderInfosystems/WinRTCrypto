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
    Width = 565
    Height = 65
    Anchors = [akLeft, akTop, akRight]
    Shape = stRoundRect
    ExplicitWidth = 577
  end
  object LabelResult: TLabel
    Left = 55
    Top = 440
    Width = 510
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
  object LabelPrivateKey: TLabel
    Left = 24
    Top = 144
    Width = 58
    Height = 15
    Caption = 'Private Key'
  end
  object LabelKeySize: TLabel
    Left = 437
    Top = 32
    Width = 71
    Height = 15
    Caption = 'Key size [bits]'
  end
  object EditClear: TEdit
    Left = 24
    Top = 168
    Width = 559
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    TextHint = 'Clear text'
    ExplicitWidth = 553
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
    Width = 559
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    ExplicitWidth = 553
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
  object cboAlgo: TComboBox
    Left = 112
    Top = 29
    Width = 289
    Height = 23
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemIndex = 0
    TabOrder = 4
    Text = 'RsaPkcs1'
    OnChange = cboAlgoChange
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
    Width = 565
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 5
    TextHint = 'resulting clear text'
    ExplicitWidth = 559
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
    Width = 463
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 7
    ExplicitWidth = 457
  end
  object EditPrivateKey: TEdit
    Left = 120
    Top = 139
    Width = 463
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 8
    ExplicitWidth = 457
  end
  object btnSaveKeys: TButton
    Left = 353
    Top = 72
    Width = 68
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Save Keys'
    Enabled = False
    TabOrder = 9
    OnClick = btnSaveKeysClick
    ExplicitLeft = 347
  end
  object btnSaveEncrypt: TButton
    Left = 427
    Top = 216
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Save'
    Enabled = False
    TabOrder = 10
    OnClick = btnSaveEncryptClick
  end
  object btnLoadEncrypt: TButton
    Left = 508
    Top = 216
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Load'
    Enabled = False
    TabOrder = 11
    OnClick = btnLoadEncryptClick
  end
  object btnLoadKeys: TButton
    Left = 427
    Top = 72
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Load Keys'
    TabOrder = 12
    OnClick = btnLoadKeysClick
    ExplicitLeft = 421
  end
  object btnLoadPubKey: TButton
    Left = 508
    Top = 72
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Load Public'
    TabOrder = 13
    OnClick = btnLoadPubKeyClick
  end
  object cboKeySize: TComboBox
    Left = 514
    Top = 29
    Width = 69
    Height = 23
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 14
    Text = '2048'
    Items.Strings = (
      '2048'
      '3072'
      '4096')
  end
end
