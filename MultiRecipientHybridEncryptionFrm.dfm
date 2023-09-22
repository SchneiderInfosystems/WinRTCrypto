object FrmMultiRecipientHybridEncryption: TFrmMultiRecipientHybridEncryption
  Left = 0
  Top = 0
  Caption = 'Hybrid Encryption for Multiple Recipients'
  ClientHeight = 702
  ClientWidth = 1014
  Color = clBtnFace
  Constraints.MinHeight = 720
  Constraints.MinWidth = 1000
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  DesignSize = (
    1014
    702)
  TextHeight = 15
  object ShapeResult: TShape
    Left = 16
    Top = 643
    Width = 968
    Height = 38
    Anchors = [akLeft, akRight, akBottom]
    Shape = stRoundRect
    ExplicitTop = 728
    ExplicitWidth = 990
  end
  object LabelAlgo: TLabel
    Left = 16
    Top = 24
    Width = 113
    Height = 15
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Symmetrical Algorithm'
    ExplicitWidth = 123
  end
  object LabelResult: TLabel
    Left = 39
    Top = 652
    Width = 905
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
    ExplicitWidth = 915
  end
  object Label1: TLabel
    Left = 483
    Top = 24
    Width = 119
    Height = 15
    Caption = 'Asymetrical Algorithm'
  end
  object Label3: TLabel
    Left = 18
    Top = 542
    Width = 42
    Height = 15
    Anchors = [akLeft, akBottom]
    Caption = 'Payload'
    ExplicitTop = 627
  end
  object Label4: TLabel
    Left = 18
    Top = 447
    Width = 290
    Height = 15
    Anchors = [akLeft, akBottom]
    Caption = 'Public key with encrypted session keys for all recipients'
    ExplicitTop = 532
  end
  object lblHeaderSize: TLabel
    Left = 670
    Top = 447
    Width = 304
    Height = 15
    Alignment = taRightJustify
    Anchors = [akRight, akBottom]
    AutoSize = False
    Caption = 'Header Size [bytes]'
    ExplicitLeft = 680
  end
  object lblPayloadSize: TLabel
    Left = 670
    Top = 542
    Width = 307
    Height = 15
    Alignment = taRightJustify
    Anchors = [akRight, akBottom]
    AutoSize = False
    Caption = 'Payload size [bytes]'
    ExplicitLeft = 680
  end
  object cboSymAlgo: TComboBox
    Left = 152
    Top = 21
    Width = 209
    Height = 23
    Style = csDropDownList
    ItemIndex = 1
    TabOrder = 0
    Text = 'AesCbcPkcs7'
    OnChange = ComboBoxAlgosChange
    Items.Strings = (
      'AesCbc'
      'AesCbcPkcs7'
      'AesCcm'
      'AesEcb'
      'AesEcbPkcs7'
      'AesGcm')
  end
  object edtEncryptedPayload: TEdit
    Left = 24
    Top = 563
    Width = 960
    Height = 23
    Anchors = [akLeft, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Pitch = fpFixed
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 1
    Text = 'edtEncryptedPayload'
    ExplicitTop = 554
    ExplicitWidth = 964
  end
  object cboSymKeySize: TComboBox
    Left = 373
    Top = 21
    Width = 69
    Height = 23
    Style = csDropDownList
    ItemIndex = 2
    TabOrder = 2
    Text = '2048'
    OnChange = CheckKeySizes
    Items.Strings = (
      '512'
      '1024'
      '2048'
      '4096')
  end
  object cboAsymAlgo: TComboBox
    Left = 608
    Top = 21
    Width = 273
    Height = 23
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 3
    Text = 'RsaPkcs1'
    OnChange = ComboBoxAlgosChange
    Items.Strings = (
      'RsaPkcs1'
      'RsaOaepSha1'
      'RsaOaepSha256'
      'RsaOaepSha384'
      'RsaOaepSha512')
  end
  object btnSaveEncryptedMsg: TButton
    Left = 18
    Top = 595
    Width = 191
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save Encrypted Message'
    TabOrder = 4
    OnClick = btnSaveEncryptedMsgClick
    ExplicitTop = 586
  end
  object btnLoadEncryptedMessage: TButton
    Left = 226
    Top = 595
    Width = 191
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Load Encrypted Message'
    TabOrder = 5
    OnClick = btnLoadEncryptedMessageClick
    ExplicitTop = 586
  end
  object PageControl: TPageControl
    Left = 18
    Top = 65
    Width = 978
    Height = 360
    ActivePage = tabKeys
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 6
    ExplicitWidth = 982
    ExplicitHeight = 351
    object tabKeys: TTabSheet
      Caption = 'Key Management'
      DesignSize = (
        970
        330)
      object LabelPublicKey: TLabel
        Left = 19
        Top = 64
        Width = 55
        Height = 15
        Caption = 'Public Key'
      end
      object LabelPrivateKey: TLabel
        Left = 19
        Top = 93
        Width = 58
        Height = 15
        Caption = 'Private Key'
      end
      object Label7: TLabel
        Left = 19
        Top = 120
        Width = 52
        Height = 15
        Caption = 'Key name'
      end
      object lblKeysSize: TLabel
        Left = 648
        Top = 120
        Width = 295
        Height = 15
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        AutoSize = False
        Caption = 'Public key size / Private key size [bytes]'
        ExplicitLeft = 658
      end
      object btnCreatePersonalKeys: TButton
        Left = 19
        Top = 18
        Width = 122
        Height = 25
        Caption = 'Create Personal Key'
        TabOrder = 0
        OnClick = btnCreatePersonalKeysClick
      end
      object btnSavePersonalKeys: TButton
        Left = 19
        Top = 157
        Width = 126
        Height = 25
        Caption = 'Save Personal Keys'
        Enabled = False
        TabOrder = 1
        OnClick = btnSavePersonalKeysClick
      end
      object btnLoadKeys: TButton
        Left = 147
        Top = 18
        Width = 141
        Height = 25
        Caption = 'Load Personal Keys'
        TabOrder = 2
        OnClick = btnLoadKeysClick
      end
      object btnLoadPubKey: TButton
        Left = 294
        Top = 18
        Width = 157
        Height = 25
        Caption = 'Load Personal Public Key'
        TabOrder = 3
        OnClick = btnLoadPubKeyClick
      end
      object edtPublicKey: TEdit
        Left = 133
        Top = 61
        Width = 818
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Pitch = fpFixed
        Font.Style = []
        ParentFont = False
        TabOrder = 4
        ExplicitWidth = 822
      end
      object edtPrivateKey: TEdit
        Left = 133
        Top = 88
        Width = 818
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Pitch = fpFixed
        Font.Style = []
        ParentFont = False
        TabOrder = 5
        ExplicitWidth = 822
      end
      object edtKeyName: TEdit
        Left = 133
        Top = 117
        Width = 287
        Height = 23
        TabOrder = 6
      end
    end
    object tabEncryption: TTabSheet
      Caption = 'Message Encryption'
      ImageIndex = 1
      DesignSize = (
        970
        330)
      object Label2: TLabel
        Left = 16
        Top = 12
        Width = 210
        Height = 15
        Caption = 'Clear text as encryption input argument'
      end
      object Label8: TLabel
        Left = 16
        Top = 117
        Width = 89
        Height = 15
        Anchors = [akLeft, akBottom]
        Caption = 'List of Recipients'
      end
      object LabelKey: TLabel
        Left = 18
        Top = 288
        Width = 103
        Height = 15
        Anchors = [akLeft, akBottom]
        Caption = 'Created session key'
      end
      object LabelSymKeySize: TLabel
        Left = 853
        Top = 287
        Width = 95
        Height = 15
        Alignment = taRightJustify
        Anchors = [akLeft, akRight, akBottom]
        AutoSize = False
        Caption = 'Key size [bits]'
        ExplicitWidth = 105
      end
      object btnEncrypt: TButton
        Left = 16
        Top = 246
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Encrypt'
        Enabled = False
        TabOrder = 0
        OnClick = btnEncryptClick
      end
      object MemoClear: TMemo
        Left = 16
        Top = 33
        Width = 935
        Height = 72
        Anchors = [akLeft, akTop, akRight, akBottom]
        Lines.Strings = (
          'MemoClear')
        TabOrder = 1
        ExplicitWidth = 945
      end
      object lstRecipients: TListBox
        Left = 17
        Top = 138
        Width = 935
        Height = 55
        Anchors = [akLeft, akRight, akBottom]
        ItemHeight = 15
        TabOrder = 2
        ExplicitWidth = 945
      end
      object btnAddRecipient: TButton
        Left = 17
        Top = 199
        Width = 105
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Add Recipient'
        TabOrder = 3
        OnClick = btnAddRecipientClick
      end
      object EditSessionKey: TEdit
        Left = 152
        Top = 283
        Width = 685
        Height = 23
        Anchors = [akLeft, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Pitch = fpFixed
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        TabOrder = 4
        ExplicitWidth = 695
      end
      object btnClearRecipients: TButton
        Left = 128
        Top = 200
        Width = 98
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Clear Recipients'
        TabOrder = 5
        OnClick = btnClearRecipientsClick
      end
    end
    object TabDecryption: TTabSheet
      Caption = 'Message Decryption'
      ImageIndex = 2
      DesignSize = (
        970
        330)
      object Label5: TLabel
        Left = 17
        Top = 190
        Width = 105
        Height = 15
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Encrypted session key'
        ExplicitWidth = 115
      end
      object Label6: TLabel
        Left = 17
        Top = 221
        Width = 195
        Height = 15
        Caption = 'Clear text as decryption output result'
      end
      object LabelDecryptedSessionKeySize: TLabel
        Left = 842
        Top = 190
        Width = 113
        Height = 15
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        AutoSize = False
        Caption = 'Key size [bits]'
        ExplicitLeft = 852
      end
      object Label9: TLabel
        Left = 16
        Top = 87
        Width = 106
        Height = 15
        Caption = 'Personal Private Key'
      end
      object Label10: TLabel
        Left = 16
        Top = 114
        Width = 32
        Height = 15
        Caption = 'Name'
      end
      object Label11: TLabel
        Left = 16
        Top = 60
        Width = 103
        Height = 15
        Caption = 'Personal Public Key'
      end
      object lblHintPublicKeyMissingInHeader: TLabel
        Left = 98
        Top = 154
        Width = 390
        Height = 15
        Caption = 
          'The personal public key is missing in the header of the encrypte' +
          'd message'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        StyleElements = [seClient, seBorder]
      end
      object btnDecrypt: TButton
        Left = 17
        Top = 150
        Width = 75
        Height = 25
        Caption = 'Decrypt'
        Enabled = False
        TabOrder = 0
        OnClick = btnDecryptClick
      end
      object EditDecryptedSessionKey: TEdit
        Left = 159
        Top = 187
        Width = 677
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Pitch = fpFixed
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        TabOrder = 1
        ExplicitWidth = 687
      end
      object MemoResult: TMemo
        Left = 17
        Top = 242
        Width = 931
        Height = 72
        Anchors = [akLeft, akTop, akRight, akBottom]
        ReadOnly = True
        TabOrder = 2
        ExplicitWidth = 941
      end
      object edtPersonalPrivateKey: TEdit
        Left = 130
        Top = 82
        Width = 818
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Pitch = fpFixed
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        ExplicitWidth = 828
      end
      object edtPersonalName: TEdit
        Left = 130
        Top = 111
        Width = 287
        Height = 23
        TabOrder = 4
      end
      object btnLoadPersonalKey: TButton
        Left = 17
        Top = 18
        Width = 141
        Height = 25
        Caption = 'Load Personal Keys'
        TabOrder = 5
        OnClick = btnLoadPersonalKeyClick
      end
      object edtPersonalPublicKey: TEdit
        Left = 130
        Top = 53
        Width = 818
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Pitch = fpFixed
        Font.Style = []
        ParentFont = False
        TabOrder = 6
        ExplicitWidth = 828
      end
    end
  end
  object lstHeader: TListBox
    Left = 24
    Top = 475
    Width = 960
    Height = 55
    Anchors = [akLeft, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Pitch = fpVariable
    Font.Style = []
    ItemHeight = 15
    ParentFont = False
    TabOrder = 7
    ExplicitTop = 466
    ExplicitWidth = 964
  end
  object cboAsymKeySize: TComboBox
    Left = 895
    Top = 21
    Width = 69
    Height = 23
    Style = csDropDownList
    ItemIndex = 2
    TabOrder = 8
    Text = '4096'
    OnChange = CheckKeySizes
    Items.Strings = (
      '1024'
      '2048'
      '4096'
      '8192')
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'crypt'
    Filter = 'Encrypted message (*.crypt)|*.crypt'
    Left = 472
    Top = 592
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'crypt'
    Filter = 'Encrypted message (*.crypt)|*.crypt'
    Left = 544
    Top = 592
  end
  object OpenDialogPrivateKey: TOpenDialog
    DefaultExt = 'PrivateKey'
    Filter = 'Private Key (*.PrivateKey)|*.PrivateKey'
    Left = 760
    Top = 592
  end
  object OpenDialogPublicKey: TOpenDialog
    DefaultExt = 'PublicKey'
    Filter = 'Public Key (*.PublicKey)|*.PublicKey'
    Left = 904
    Top = 592
  end
end
