object FrmHybridEncryption: TFrmHybridEncryption
  Left = 0
  Top = 0
  Caption = 'Hybrid Encryption'
  ClientHeight = 787
  ClientWidth = 1024
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  DesignSize = (
    1024
    787)
  TextHeight = 15
  object ShapeResult: TShape
    Left = 16
    Top = 728
    Width = 974
    Height = 38
    Anchors = [akLeft, akRight, akBottom]
    Shape = stRoundRect
    ExplicitWidth = 990
  end
  object LabelAlgo: TLabel
    Left = 16
    Top = 24
    Width = 123
    Height = 15
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Symmetrical Algorithm'
  end
  object LabelKey: TLabel
    Left = 18
    Top = 336
    Width = 103
    Height = 15
    Caption = 'Created session key'
  end
  object LabelResult: TLabel
    Left = 39
    Top = 739
    Width = 911
    Height = 20
    Alignment = taCenter
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 20
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 927
  end
  object LabelSymKeySize: TLabel
    Left = 877
    Top = 335
    Width = 105
    Height = 15
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    AutoSize = False
    Caption = 'Key size [bits]'
    ExplicitLeft = 893
  end
  object Label1: TLabel
    Left = 472
    Top = 24
    Width = 130
    Height = 15
    Caption = 'Asymmetrical Algorithm'
  end
  object LabelPublicKey: TLabel
    Left = 18
    Top = 123
    Width = 103
    Height = 15
    Caption = 'Personal Public Key'
  end
  object LabelPrivateKey: TLabel
    Left = 18
    Top = 152
    Width = 106
    Height = 15
    Caption = 'Personal Private Key'
  end
  object Label3: TLabel
    Left = 18
    Top = 419
    Width = 42
    Height = 15
    Caption = 'Payload'
  end
  object Label4: TLabel
    Left = 18
    Top = 364
    Width = 245
    Height = 15
    Caption = 'Encrypted Sessionkey with Personal Public Key'
  end
  object Label5: TLabel
    Left = 18
    Top = 577
    Width = 115
    Height = 15
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Encrypted session key'
  end
  object LabelDecryptedSessionKeySize: TLabel
    Left = 877
    Top = 576
    Width = 113
    Height = 15
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    AutoSize = False
    Caption = 'Key size [bits]'
    ExplicitLeft = 912
  end
  object Label2: TLabel
    Left = 18
    Top = 187
    Width = 210
    Height = 15
    Caption = 'Clear text as encryption input argument'
  end
  object Label6: TLabel
    Left = 18
    Top = 605
    Width = 195
    Height = 15
    Caption = 'Clear text as decryption output result'
  end
  object LabelIV: TLabel
    Left = 17
    Top = 473
    Width = 10
    Height = 15
    Caption = 'IV'
  end
  object EditSessionKey: TEdit
    Left = 152
    Top = 331
    Width = 719
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 0
  end
  object btnEncrypt: TButton
    Left = 18
    Top = 305
    Width = 75
    Height = 25
    Caption = 'Encrypt'
    Enabled = False
    TabOrder = 1
    OnClick = btnEncryptClick
  end
  object edtEncryptedPayload: TEdit
    Left = 18
    Top = 440
    Width = 972
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 2
    Text = 'edtEncryptedPayload'
  end
  object btnDecrypt: TButton
    Left = 18
    Top = 541
    Width = 75
    Height = 25
    Caption = 'Decrypt'
    Enabled = False
    TabOrder = 3
    OnClick = btnDecryptClick
  end
  object MemoClear: TMemo
    Left = 16
    Top = 208
    Width = 974
    Height = 72
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      'MemoClear')
    TabOrder = 4
  end
  object MemoResult: TMemo
    Left = 18
    Top = 626
    Width = 972
    Height = 72
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      'MemoResult')
    ReadOnly = True
    TabOrder = 5
  end
  object btnCreatePersonalKeys: TButton
    Left = 18
    Top = 80
    Width = 122
    Height = 25
    Caption = 'Create Personal Key'
    TabOrder = 6
    OnClick = btnCreatePersonalKeysClick
  end
  object btnSavePersonalKeys: TButton
    Left = 152
    Top = 80
    Width = 126
    Height = 25
    Caption = 'Save Personal Keys'
    Enabled = False
    TabOrder = 7
    OnClick = btnSavePersonalKeysClick
  end
  object btnLoadKeys: TButton
    Left = 292
    Top = 80
    Width = 141
    Height = 25
    Caption = 'Load Personal Keys'
    TabOrder = 8
    OnClick = btnLoadKeysClick
  end
  object btnLoadPubKey: TButton
    Left = 445
    Top = 80
    Width = 157
    Height = 25
    Caption = 'Load Personal Public Key'
    TabOrder = 9
    OnClick = btnLoadPubKeyClick
  end
  object edtPublicKey: TEdit
    Left = 152
    Top = 120
    Width = 838
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 10
  end
  object edtPrivateKey: TEdit
    Left = 152
    Top = 147
    Width = 838
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 11
  end
  object edtEncryptedSessionKey: TEdit
    Left = 18
    Top = 390
    Width = 972
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 12
    Text = 'edtEncryptedSessionKey'
  end
  object EditDecryptedSessionKey: TEdit
    Left = 152
    Top = 573
    Width = 719
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 13
  end
  object edtUsedPublicKey: TEdit
    Left = 280
    Top = 360
    Width = 710
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 14
    Text = 'edtUsedPublicKey'
    StyleElements = [seFont, seBorder]
  end
  object btnSaveEncryptedMsg: TButton
    Left = 18
    Top = 496
    Width = 191
    Height = 25
    Caption = 'Save Encrypted Message'
    TabOrder = 15
    OnClick = btnSaveEncryptedMsgClick
  end
  object btnLoadEncryptedMessage: TButton
    Left = 226
    Top = 496
    Width = 191
    Height = 25
    Caption = 'Load Encrypted Message'
    TabOrder = 16
    OnClick = btnLoadEncryptedMessageClick
  end
  object cboSymAlgo: TComboBox
    Left = 152
    Top = 21
    Width = 209
    Height = 23
    Style = csDropDownList
    ItemIndex = 1
    TabOrder = 17
    Text = 'AesCbcPkcs7'
    OnChange = cboAlgosChange
    Items.Strings = (
      'AesCbc'
      'AesCbcPkcs7'
      'AesEcb'
      'AesEcbPkcs7')
  end
  object cboSymKeySize: TComboBox
    Left = 373
    Top = 21
    Width = 69
    Height = 23
    Style = csDropDownList
    ItemIndex = 2
    TabOrder = 18
    Text = '256'
    OnChange = CheckKeySizes
    Items.Strings = (
      '128'
      '192'
      '256')
  end
  object cboAsymAlgo: TComboBox
    Left = 608
    Top = 21
    Width = 273
    Height = 23
    Style = csDropDownList
    ItemIndex = 2
    TabOrder = 19
    Text = 'RsaOaepSha256'
    OnChange = cboAlgosChange
    Items.Strings = (
      'RsaPkcs1'
      'RsaOaepSha1'
      'RsaOaepSha256'
      'RsaOaepSha384'
      'RsaOaepSha512')
  end
  object cboAsymKeySize: TComboBox
    Left = 895
    Top = 21
    Width = 69
    Height = 23
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 20
    Text = '2048'
    OnChange = CheckKeySizes
    Items.Strings = (
      '2048'
      '3072'
      '4096')
  end
  object edtIV: TEdit
    Left = 39
    Top = 469
    Width = 297
    Height = 23
    TabOrder = 21
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'crypt'
    Filter = 'Encrypted message (*.crypt)|*.crypt'
    Left = 448
    Top = 472
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'crypt'
    Filter = 'Encrypted message (*.crypt)|*.crypt'
    Left = 520
    Top = 472
  end
  object OpenDialogPrivateKey: TOpenDialog
    DefaultExt = 'PrivateKey'
    Filter = 'Private Key (*.PrivateKey)|*.PrivateKey'
    Left = 648
    Top = 64
  end
  object OpenDialogPublicKey: TOpenDialog
    DefaultExt = 'PublicKey'
    Filter = 'Public Key (*.PublicKey)|*.PublicKey'
    Left = 792
    Top = 64
  end
end
