{******************************************************************************}
{                                                                              }
{  WinRT Crypto Sample Applications                                            }
{  Copyright (c) 2023 Christoph Schneider                                      }
{  Schneider Infosystems AG, Switzerland                                       }
{  https://github.com/SchneiderInfosystems/WinRTCrypto                         }
{                                                                              }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}

unit HybridEncryptionFrm;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.Types, System.SysUtils, System.StrUtils, System.Classes, System.Win.WinRT,
  System.Win.ComObj,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Winapi.CommonTypes, Winapi.WinRT,
  Winapi.Security.Cryptography;

type
  TFrmHybridEncryption = class(TForm)
    ShapeResult: TShape;
    LabelAlgo: TLabel;
    LabelKey: TLabel;
    LabelResult: TLabel;
    LabelSymKeySize: TLabel;
    EditSessionKey: TEdit;
    btnEncrypt: TButton;
    edtEncryptedPayload: TEdit;
    btnDecrypt: TButton;
    MemoClear: TMemo;
    MemoResult: TMemo;
    Label1: TLabel;
    btnCreatePersonalKeys: TButton;
    btnSavePersonalKeys: TButton;
    btnLoadKeys: TButton;
    btnLoadPubKey: TButton;
    LabelPublicKey: TLabel;
    LabelPrivateKey: TLabel;
    edtPublicKey: TEdit;
    edtPrivateKey: TEdit;
    Label3: TLabel;
    edtEncryptedSessionKey: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    EditDecryptedSessionKey: TEdit;
    LabelDecryptedSessionKeySize: TLabel;
    edtUsedPublicKey: TEdit;
    btnSaveEncryptedMsg: TButton;
    SaveDialog: TSaveDialog;
    btnLoadEncryptedMessage: TButton;
    OpenDialog: TOpenDialog;
    OpenDialogPrivateKey: TOpenDialog;
    OpenDialogPublicKey: TOpenDialog;
    Label2: TLabel;
    Label6: TLabel;
    cboSymAlgo: TComboBox;
    cboSymKeySize: TComboBox;
    cboAsymAlgo: TComboBox;
    cboAsymKeySize: TComboBox;
    procedure btnEncryptClick(Sender: TObject);
    procedure btnDecryptClick(Sender: TObject);
    procedure btnCreatePersonalKeysClick(Sender: TObject);
    procedure cboAlgosChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSavePersonalKeysClick(Sender: TObject);
    procedure btnLoadKeysClick(Sender: TObject);
    procedure btnLoadPubKeyClick(Sender: TObject);
    procedure btnSaveEncryptedMsgClick(Sender: TObject);
    procedure btnLoadEncryptedMessageClick(Sender: TObject);
    procedure CheckKeySizes(Sender: TObject);
  private
    function SelectedAsymAlgo: Core_IAsymmetricKeyAlgorithmProvider;
    function SelectAsymKeySizeInBits: integer;
    function SelectedSymAlgo: Core_ISymmetricKeyAlgorithmProvider;
    function SelectSymKeySizeInBytes: integer;
    function DataFolder: string;
    procedure CheckActions;
    procedure ShowError(const Msg: string);
    procedure ShowResult(ClearText: IBuffer);
    procedure ClearResult;
    function ImportPublicKey(const PublicKeyAsBase64: string): Core_ICryptographicKey;
    function ImportPrivateKey(const PrivateKeyAsBase64: string): Core_ICryptographicKey;
    function AsymmetricEncrypt(PublicKey: Core_ICryptographicKey; SessionKey: IBuffer): IBuffer;
    function AsymmetricDecrypt(PrivatKey: Core_ICryptographicKey; Encrypted: IBuffer): IBuffer;
    function SymmetricEncrypt(AlgoProvider: Core_ISymmetricKeyAlgorithmProvider; SessionKey, ClearText: IBuffer): IBuffer;
    function SymmetricDecrypt(AlgoProvider: Core_ISymmetricKeyAlgorithmProvider; SessionKey, Encrypted: IBuffer): IBuffer;
    function IBufferToStr(Buf: IBuffer): string;
    function StrToIBuffer(Text: string): IBuffer;
    function DecodeFromBase64(Base64: string): IBuffer;
    function EncodeAsBase64(Buf: IBuffer): string;
  end;

var
  FrmHybridEncryption: TFrmHybridEncryption;

implementation

{$R *.dfm}

resourcestring
  rsTS =
    'This is a sample text for hybrid encryption based on symmetric algo %s and asymmetric algo %s.'#13#10 +
    'The creation date and time of this test text is %s %s.'#13#10 +
    'It contains also unicode letters Ж•₱₲₳‡₾ to test UTF8 encoding.';
  rsKeySize = 'Key size %d [bits]';

{ TFrmHybridEncryption }

procedure TFrmHybridEncryption.FormCreate(Sender: TObject);
begin
  edtEncryptedSessionKey.Text := '';
  edtEncryptedPayload.Text := '';
  edtUsedPublicKey.Text := '';
  ClearResult;
  cboAlgosChange(nil);
  CheckActions;
end;

function TFrmHybridEncryption.DataFolder: string;
begin
  result := ExpandFileName(ExtractFileDir(Application.ExeName) + '\..\..\Data\');
  if not DirectoryExists(result, false) then
    ForceDirectories(result);
end;

procedure TFrmHybridEncryption.cboAlgosChange(Sender: TObject);
begin
  MemoClear.Text := Format(rsTS, [cboSymAlgo.Text, cboAsymAlgo.Text, DateToStr(now), TimeToStr(Now)]);
end;

function TFrmHybridEncryption.SelectedAsymAlgo: Core_IAsymmetricKeyAlgorithmProvider;
var
  Algo: HString;
begin
  case cboAsymAlgo.ItemIndex of
    0: Algo := TCore_AsymmetricAlgorithmNames.RsaPkcs1;
    1: Algo := TCore_AsymmetricAlgorithmNames.RsaOaepSha1;
    2: Algo := TCore_AsymmetricAlgorithmNames.RsaOaepSha256;
    3: Algo := TCore_AsymmetricAlgorithmNames.RsaOaepSha384;
    4: Algo := TCore_AsymmetricAlgorithmNames.RsaOaepSha512;
    else
      raise Exception.Create('Unknown Core_AsymmetricAlgorithmNames');
  end;
  result := TCore_AsymmetricKeyAlgorithmProvider.OpenAlgorithm(Algo);
  cboAsymAlgo.Enabled := false;
end;

function TFrmHybridEncryption.SelectAsymKeySizeInBits: integer;
begin
  result := StrToInt(cboAsymKeySize.Text);
  cboAsymKeySize.Enabled := false;
end;

function TFrmHybridEncryption.SelectedSymAlgo: Core_ISymmetricKeyAlgorithmProvider;
var
  Algo: HString;
begin
  case cboSymAlgo.ItemIndex of
    0: Algo := TCore_SymmetricAlgorithmNames.AesCbc;
    1: Algo := TCore_SymmetricAlgorithmNames.AesCbcPkcs7;
    2: Algo := TCore_SymmetricAlgorithmNames.AesCcm;
    3: Algo := TCore_SymmetricAlgorithmNames.AesEcb;
    4: Algo := TCore_SymmetricAlgorithmNames.AesEcbPkcs7;
    5: Algo := TCore_SymmetricAlgorithmNames.AesGcm;
    else
      raise Exception.Create('Unknown Core_SymmetricAlgorithmNames');
  end;
  result := TCore_SymmetricKeyAlgorithmProvider.OpenAlgorithm(Algo);
  cboSymAlgo.Enabled := false;
end;

function TFrmHybridEncryption.SelectSymKeySizeInBytes: integer;
begin
  result := StrToInt(cboSymKeySize.Text) div 8;
  cboSymKeySize.Enabled := false;
end;

procedure TFrmHybridEncryption.CheckKeySizes(Sender: TObject);
begin
  // Asymetric key size starts with 1024 while symmetric key size starts with 512
  if cboAsymKeySize.ItemIndex < cboSymKeySize.ItemIndex then
    ShowError('Asymmetrical key size must be greater than symmetrical key size!')
  else
    ClearResult;
end;

procedure TFrmHybridEncryption.ShowError(const Msg: string);
begin
  ShapeResult.Brush.Color := clRed;
  LabelResult.Caption := Msg;
end;

procedure TFrmHybridEncryption.ShowResult(ClearText: IBuffer);
begin
  MemoResult.Text := IBufferToStr(ClearText);
  if SameText(MemoClear.Text, MemoResult.Text) then
  begin
    ShapeResult.Brush.Color := clLime;
    LabelResult.Caption := 'Test passed';
  end else begin
    ShapeResult.Brush.Color := clYellow;
    LabelResult.Caption := 'Test failed: resulting clear text different than starting text';
  end;
end;

procedure TFrmHybridEncryption.ClearResult;
begin
  ShapeResult.Brush.Color := clBtnFace;
  LabelResult.Caption := '';
end;

procedure TFrmHybridEncryption.btnCreatePersonalKeysClick(Sender: TObject);
var
  PersonalKey: Core_ICryptographicKey;
begin
  Screen.Cursor := crHourGlass;
  try
    PersonalKey := SelectedAsymAlgo.CreateKeyPair(SelectAsymKeySizeInBits);
  finally
    Screen.Cursor := crDefault;
  end;
  if assigned(PersonalKey) then
  begin
    edtPublicKey.Text := EncodeAsBase64(PersonalKey.ExportPublicKey);
    edtPrivateKey.Text := EncodeAsBase64(PersonalKey.Export(Core_CryptographicPrivateKeyBlobType.Pkcs8RawPrivateKeyInfo));
  end;
  CheckActions;
end;

procedure TFrmHybridEncryption.btnSavePersonalKeysClick(Sender: TObject);
var
  sl: TStringList;
  Name: string;
begin
  Name := InputBox('Name for Keys', 'Enter your name' , 'Myself');
  sl := TStringList.Create;
  try
    sl.Text := edtPublicKey.Text;
    sl.SaveToFile(DataFolder + Name + '.PublicKey');
    sl.Text := edtPrivateKey.Text ;
    sl.SaveToFile(DataFolder + Name + '.PrivateKey');
  finally
    sl.Free;
  end;
end;

procedure TFrmHybridEncryption.btnLoadKeysClick(Sender: TObject);
var
  sl: TStringList;
  PersonalKey: Core_ICryptographicKey;
begin
  OpenDialogPrivateKey.InitialDir := DataFolder;
  if OpenDialogPrivateKey.Execute(Handle) then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(OpenDialogPrivateKey.FileName);
      edtPrivateKey.Text := trim(sl.Text);
    finally
      sl.Free;
    end;
    PersonalKey := ImportPrivateKey(edtPrivateKey.Text);
    if assigned(PersonalKey) then
      edtPublicKey.Text := EncodeAsBase64(PersonalKey.ExportPublicKey);
    CheckActions;
  end;
end;

procedure TFrmHybridEncryption.btnLoadPubKeyClick(Sender: TObject);
var
  sl: TStringList;
begin
  OpenDialogPublicKey.InitialDir := DataFolder;
  if OpenDialogPublicKey.Execute(Handle) then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(OpenDialogPublicKey.FileName);
      edtPublicKey.Text := trim(sl.Text);
      edtPrivateKey.Text := '';
    finally
      sl.Free;
    end;
    CheckActions;
  end;
end;

procedure TFrmHybridEncryption.CheckActions;
var
  IsPublicKeyExists: boolean;
  IsPrivateKeyExists: boolean;
  IsSamePublicKey: boolean;
begin
  IsPublicKeyExists := length(edtPublicKey.Text) > 0;
  IsPrivateKeyExists := length(edtPrivateKey.Text) > 0;
  IsSamePublicKey := SameText(edtUsedPublicKey.Text, edtPublicKey.Text) and
    IsPublicKeyExists;
  btnSavePersonalKeys.Enabled := IsPublicKeyExists;
  btnEncrypt.Enabled := IsPublicKeyExists and
    (length(MemoClear.Text) > 0);
  if IsSamePublicKey then
    edtUsedPublicKey.Color := clLime
  else
    edtUsedPublicKey.Color := clWindow;
  btnDecrypt.Enabled := IsPrivateKeyExists and IsSamePublicKey and
    (length(edtEncryptedSessionKey.Text) > 0) and
    (length(edtEncryptedPayload.Text) > 0);
end;

function TFrmHybridEncryption.ImportPrivateKey(const PrivateKeyAsBase64: string): Core_ICryptographicKey;
begin
  result := nil;
  try
    result := SelectedAsymAlgo.ImportKeyPair(DecodeFromBase64(PrivateKeyAsBase64),
      Core_CryptographicPrivateKeyBlobType.Pkcs8RawPrivateKeyInfo);
  except
    on e: exception do
      ShowError('Import private key failed: ' + e.Message);
  end;
end;

function TFrmHybridEncryption.ImportPublicKey(const PublicKeyAsBase64: string): Core_ICryptographicKey;
begin
  result := nil;
  try
    result := SelectedAsymAlgo.ImportPublicKey(DecodeFromBase64(PublicKeyAsBase64));
  except
    on e: exception do
      ShowError('Import public key failed: ' + e.Message);
  end;
end;

procedure TFrmHybridEncryption.btnEncryptClick(Sender: TObject);
var
  SessionKey: IBuffer;
  PublicKey: Core_ICryptographicKey;
  EncryptedSessionKey: IBuffer;
  Payload: IBuffer;
begin
  SessionKey := TCryptographicBuffer.GenerateRandom(SelectSymKeySizeInBytes);
  EditSessionKey.Text := EncodeAsBase64(SessionKey);
  LabelSymKeySize.Caption := Format(rsKeySize, [SelectedSymAlgo.CreateSymmetricKey(SessionKey).KeySize]);
  PublicKey := ImportPublicKey(edtPublicKey.Text);
  if assigned(PublicKey) then
  begin
    edtUsedPublicKey.Text := edtPublicKey.Text;
    EncryptedSessionKey := AsymmetricEncrypt(PublicKey, SessionKey);
    if assigned(EncryptedSessionKey) then
      edtEncryptedSessionKey.Text := EncodeAsBase64(EncryptedSessionKey);
    Payload := SymmetricEncrypt(SelectedSymAlgo, SessionKey, StrToIBuffer(MemoClear.Text));
    if assigned(Payload) then
    begin
      edtEncryptedPayload.Text := EncodeAsBase64(PayLoad);
      ClearResult;
    end;
  end;
  CheckActions;
end;

procedure TFrmHybridEncryption.btnDecryptClick(Sender: TObject);
var
  SessionKey: IBuffer;
  PrivateKey: Core_ICryptographicKey;
  ClearText: IBuffer;
begin
  PrivateKey := ImportPrivateKey(edtPrivateKey.Text);
  if assigned(PrivateKey) then
  begin
    SessionKey := AsymmetricDecrypt(PrivateKey, DecodeFromBase64(edtEncryptedSessionKey.Text));
    if assigned(SessionKey) then
    begin
      EditDecryptedSessionKey.Text := EncodeAsBase64(SessionKey);
      LabelDecryptedSessionKeySize.Caption := Format(rsKeySize, [SelectedSymAlgo.CreateSymmetricKey(SessionKey).KeySize]);
      ClearText := SymmetricDecrypt(SelectedSymAlgo, SessionKey, DecodeFromBase64(edtEncryptedPayload.Text));
      if assigned(ClearText) then
        ShowResult(ClearText);
    end;
  end;
end;

function TFrmHybridEncryption.EncodeAsBase64(Buf: IBuffer): string;
begin
  result := TWindowsString.HStringToString(
              TCryptographicBuffer.EncodeToBase64String(Buf));
end;

function TFrmHybridEncryption.DecodeFromBase64(Base64: string): IBuffer;
begin
  result := TCryptographicBuffer.DecodeFromBase64String(
    TWindowsString.Create(Base64));
end;

function TFrmHybridEncryption.StrToIBuffer(Text: string): IBuffer;
var
  data: TBytes;
begin
  data := TEncoding.UTF8.GetBytes(Text);
  result := TCryptographicBuffer.CreateFromByteArray(length(data), @data[0]);
end;

function TFrmHybridEncryption.IBufferToStr(Buf: IBuffer): string;
begin
  result := TCryptographicBuffer.ConvertBinaryToString(BinaryStringEncoding.Utf8, Buf).ToString;
end;

function TFrmHybridEncryption.AsymmetricEncrypt(PublicKey: Core_ICryptographicKey; SessionKey: IBuffer): IBuffer;
begin
  result := nil;
  try
    result := TCore_CryptographicEngine.Encrypt(PublicKey, SessionKey, nil);
  except
    on e: EOleException do
      if e.ErrorCode = E_INVALIDARG then
        ShowError('Asymmetric encryption failed by invalid argument: Session key size must be smaller than recipient''s public key')
      else
        ShowError('Asymmetric encryption failed: ' + e.Message);
  end;
end;

function TFrmHybridEncryption.AsymmetricDecrypt(PrivatKey: Core_ICryptographicKey; Encrypted: IBuffer): IBuffer;
begin
  result := nil;
  try
    result := TCore_CryptographicEngine.Decrypt(PrivatKey, Encrypted, nil);
  except
    on e: exception do
      ShowError('Asymmetric decryption failed: ' + e.Message);
  end;
end;

function TFrmHybridEncryption.SymmetricEncrypt(AlgoProvider: Core_ISymmetricKeyAlgorithmProvider;
  SessionKey, ClearText: IBuffer): IBuffer;
var
  key: Core_ICryptographicKey;
begin
  result := nil;
  key := AlgoProvider.CreateSymmetricKey(SessionKey);
  try
    result := TCore_CryptographicEngine.Encrypt(key, ClearText, nil);
  except
    on e: exception do
      ShowError('Symmetric encryption failed: ' + e.Message);
  end;
end;

function TFrmHybridEncryption.SymmetricDecrypt(AlgoProvider: Core_ISymmetricKeyAlgorithmProvider;
  SessionKey, Encrypted: IBuffer): IBuffer;
var
  key: Core_ICryptographicKey;
begin
  result := nil;
  key := AlgoProvider.CreateSymmetricKey(SessionKey);
  try
    result := TCore_CryptographicEngine.Decrypt(key, Encrypted, nil);
  except
    on e: exception do
      ShowError('Symmetric decryption failed: ' + e.Message);
  end;
end;

procedure TFrmHybridEncryption.btnSaveEncryptedMsgClick(Sender: TObject);
var
  sl: TStringList;
begin
  SaveDialog.InitialDir := DataFolder;
  SaveDialog.FileName := 'Message';
  if SaveDialog.Execute(Handle) then
  begin
    sl := TStringList.Create;
    try
      sl.Add(edtUsedPublicKey.Text + ':' + edtEncryptedSessionKey.Text);
      sl.Add(edtEncryptedPayload.Text);
      sl.SaveToFile(SaveDialog.FileName);
    finally
      sl.Free;
    end;
  end;
end;

procedure TFrmHybridEncryption.btnLoadEncryptedMessageClick(Sender: TObject);
var
  sl: TStringList;
  s: TStringDynArray;
begin
  OpenDialog.InitialDir := DataFolder;
  if OpenDialog.Execute(Handle) then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(OpenDialog.FileName);
      if sl.Count = 2 then
      begin
        s := SplitString(sl[0], ':');
        if length(s) = 2 then
        begin
          edtUsedPublicKey.Text := s[0];
          edtEncryptedSessionKey.Text := s[1];
          edtEncryptedPayload.Text := sl[1];
          CheckActions;
        end;
      end;
    finally
      sl.Free;
    end;
  end;
end;

end.
