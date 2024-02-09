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
    Width = 559
    Height = 65
    Anchors = [akLeft, akTop, akRight]
    Shape = stRoundRect
    ExplicitWidth = 577
  end
  object LabelResult: TLabel
    Left = 55
    Top = 440
    Width = 504
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
    Left = 431
    Top = 32
    Width = 71
    Height = 15
    Anchors = [akTop, akRight]
    Caption = 'Key size [bits]'
    ExplicitLeft = 437
  end
  object lblChiffreLen: TLabel
    Left = 442
    Top = 293
    Width = 129
    Height = 15
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    AutoSize = False
    ExplicitLeft = 448
  end
  object Label1: TLabel
    Left = 24
    Top = 167
    Width = 51
    Height = 15
    Caption = 'Clear Text'
  end
  object Label2: TLabel
    Left = 24
    Top = 247
    Width = 36
    Height = 15
    Caption = 'Chiffre'
  end
  object Label3: TLabel
    Left = 24
    Top = 347
    Width = 103
    Height = 15
    Caption = 'Resulting Clear Text'
  end
  object EditClear: TEdit
    Left = 24
    Top = 187
    Width = 553
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
    Top = 268
    Width = 553
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    OnChange = EditEncryptedChange
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
    Width = 283
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
    ExplicitWidth = 277
  end
  object EditResult: TEdit
    Left = 24
    Top = 368
    Width = 559
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 5
    TextHint = 'resulting clear text'
    ExplicitWidth = 553
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
    Width = 457
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 7
    OnChange = EditKeyChange
    ExplicitWidth = 451
  end
  object EditPrivateKey: TEdit
    Left = 120
    Top = 139
    Width = 457
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 8
    OnChange = EditKeyChange
    ExplicitWidth = 451
  end
  object btnSaveKeys: TButton
    Left = 347
    Top = 72
    Width = 68
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Save Keys'
    Enabled = False
    TabOrder = 9
    OnClick = btnSaveKeysClick
    ExplicitLeft = 341
  end
  object btnSaveEncrypt: TButton
    Left = 421
    Top = 216
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Save'
    Enabled = False
    TabOrder = 10
    OnClick = btnSaveEncryptClick
    ExplicitLeft = 415
  end
  object btnLoadEncrypt: TButton
    Left = 502
    Top = 216
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Load'
    Enabled = False
    TabOrder = 11
    OnClick = btnLoadEncryptClick
    ExplicitLeft = 496
  end
  object btnLoadKeys: TButton
    Left = 421
    Top = 72
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Load Keys'
    TabOrder = 12
    OnClick = btnLoadKeysClick
    ExplicitLeft = 415
  end
  object btnLoadPubKey: TButton
    Left = 502
    Top = 72
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Load Public'
    TabOrder = 13
    OnClick = btnLoadPubKeyClick
    ExplicitLeft = 496
  end
  object cboKeySize: TComboBox
    Left = 508
    Top = 29
    Width = 69
    Height = 23
    Style = csDropDownList
    Anchors = [akTop, akRight]
    ItemIndex = 0
    TabOrder = 14
    Text = '2048'
    Items.Strings = (
      '2048'
      '3072'
      '4096')
    ExplicitLeft = 502
  end
end
