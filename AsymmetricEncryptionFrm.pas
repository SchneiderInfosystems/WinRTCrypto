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

unit AsymmetricEncryptionFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Winapi.CommonTypes, Winapi.WinRT,
  Winapi.Security.Cryptography, Vcl.ExtCtrls;

type
  TfrmAsymmetricEncryption = class(TForm)
    EditClear: TEdit;
    btnEncrypt: TButton;
    EditEncrypted: TEdit;
    btnDecrypt: TButton;
    ComboBoxAlgo: TComboBox;
    LabelAlgo: TLabel;
    EditResult: TEdit;
    ShapeResult: TShape;
    btnCreateKeys: TButton;
    LabelResult: TLabel;
    EditPublicKey: TEdit;
    LabelPublicKey: TLabel;
    EditKeySize: TEdit;
    LabelKeySize: TLabel;
    EditPrivateKey: TEdit;
    LabelPrivateKey: TLabel;
    btnSaveKeys: TButton;
    btnSaveEncrypt: TButton;
    btnLoad: TButton;
    btnLoadKeys: TButton;
    btnLoadPubKey: TButton;
    procedure btnEncryptClick(Sender: TObject);
    procedure btnDecryptClick(Sender: TObject);
    procedure btnCreateKeysClick(Sender: TObject);
    procedure btnSaveKeysClick(Sender: TObject);
    procedure btnSaveEncryptClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnLoadKeysClick(Sender: TObject);
    procedure btnLoadPubKeyClick(Sender: TObject);
    procedure ComboBoxAlgoChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fDataFolder: string;
    fAlgoProvider: Core_IAsymmetricKeyAlgorithmProvider;
    fKey: Core_ICryptographicKey;
    function SelectedAlgo: HSTRING;
    function KeyLength: integer;
    procedure InitWithNewKeys;
  end;

var
  frmAsymmetricEncryption: TfrmAsymmetricEncryption;

implementation

uses
  System.Win.WinRT,
  Winapi.Storage.Streams;

{$R *.dfm}

resourcestring
  rsTS = 'Test sample for asymmetrical encryption %s at %s with Unicode Ж•₱₲₳‡₾';

procedure TfrmAsymmetricEncryption.FormCreate(Sender: TObject);
begin
  ComboBoxAlgoChange(Sender);
end;

procedure TfrmAsymmetricEncryption.ComboBoxAlgoChange(Sender: TObject);
begin
  fAlgoProvider := TCore_AsymmetricKeyAlgorithmProvider.OpenAlgorithm(SelectedAlgo);
  fDataFolder :=
    IncludeTrailingPathDelimiter(
      ExpandFileName(
        ExtractFileDir(Application.ExeName) + '\..\..\Data\' + ComboBoxAlgo.Text));
  if not DirectoryExists(fDataFolder, false) then
    ForceDirectories(fDataFolder);
  EditPublicKey.Text := '';
  EditPrivateKey.Text := '';
  btnEncrypt.Enabled := false;
  btnDecrypt.Enabled := false;
  btnSaveKeys.Enabled := false;
  EditClear.Text := Format(rsTS, [ComboBoxAlgo.Text, TimeToStr(Now)]);
  btnLoadKeys.Enabled := FileExists(fDataFolder + 'PrivateAndPublic.key');
  btnLoadPubKey.Enabled := FileExists(fDataFolder + 'Public.key');
  btnLoad.Enabled := FileExists(fDataFolder + 'Encrypted.txt');
end;

procedure TfrmAsymmetricEncryption.btnCreateKeysClick(Sender: TObject);
var
  PublicKey, PrivateKey: IBuffer;
  hs: HSTRING;
begin
  fKey := fAlgoProvider.CreateKeyPair(KeyLength);
  EditKeySize.Text := fKey.KeySize.ToString;
  PublicKey := fKey.ExportPublicKey;
  hs := TCryptographicBuffer.EncodeToBase64String(PublicKey);
  EditPublicKey.Text := TWindowsString.HStringToString(hs);
  PrivateKey := fKey.Export(Core_CryptographicPrivateKeyBlobType.Pkcs8RawPrivateKeyInfo);
  hs := TCryptographicBuffer.EncodeToBase64String(PrivateKey); // ConvertBinaryToString(BinaryStringEncoding.Utf16BE, PrivateKey);
  EditPrivateKey.Text := TWindowsString.HStringToString(hs);
  InitWithNewKeys
end;

procedure TfrmAsymmetricEncryption.InitWithNewKeys;
begin
  btnEncrypt.Enabled := true;
  btnLoad.Enabled := true;
  btnDecrypt.Enabled := false;
  btnSaveKeys.Enabled := true;
  EditEncrypted.Text := '';
  EditResult.Text := '';
  ShapeResult.Brush.Color := clBtnFace;
  LabelResult.Caption := '';
end;

procedure TfrmAsymmetricEncryption.btnEncryptClick(Sender: TObject);
var
  data: TBytes;
  cleardata, encrypted: IBuffer;
  hs: HSTRING;
begin
  data := TEncoding.UTF8.GetBytes(EditClear.Text);
  cleardata := TCryptographicBuffer.CreateFromByteArray(length(data), @data[0]);
  try
    encrypted := TCore_CryptographicEngine.Encrypt(fKey, cleardata, nil {IV});
    hs := TCryptographicBuffer.EncodeToBase64String(encrypted);
    EditEncrypted.Text := TWindowsString.HStringToString(hs);
    btnSaveEncrypt.Enabled := true;
    btnDecrypt.Enabled := length(EditPrivateKey.Text) > 0;
  except
    on e: exception do
    begin
      ShapeResult.Brush.Color := clRed;
      LabelResult.Caption := 'Test failed: ' + e.Message;
    end;
  end;
end;

procedure TfrmAsymmetricEncryption.btnDecryptClick(Sender: TObject);
var
  data: TBytes;
  cleardata, encrypted: IBuffer;
  hs: HSTRING;
begin
  hs := TWindowsString.Create(EditEncrypted.Text);
  encrypted := TCryptographicBuffer.DecodeFromBase64String(hs);
  try
    cleardata := TCore_CryptographicEngine.Decrypt(fKey, encrypted, nil);
    Setlength(data, cleardata.Length);
    hs := TCryptographicBuffer.ConvertBinaryToString(BinaryStringEncoding.Utf8, cleardata);
    EditResult.Text := hs.ToString;
    if SameText(EditClear.Text, EditResult.Text) then
    begin
      ShapeResult.Brush.Color := clLime;
      LabelResult.Caption := 'Test passed';
    end else begin
      ShapeResult.Brush.Color := clYellow;
      LabelResult.Caption := 'Test failed: resulting clear text different than starting text';
    end;
  except
    on e: exception do
    begin
      ShapeResult.Brush.Color := clRed;
      LabelResult.Caption := 'Test failed: ' + e.Message;
    end;
  end;
end;

function TfrmAsymmetricEncryption.SelectedAlgo: HSTRING;
begin
  case ComboBoxAlgo.ItemIndex of
    0: result := TCore_AsymmetricAlgorithmNames.RsaPkcs1;
    1: result := TCore_AsymmetricAlgorithmNames.RsaOaepSha1;
    2: result := TCore_AsymmetricAlgorithmNames.RsaOaepSha256;
    3: result := TCore_AsymmetricAlgorithmNames.RsaOaepSha384;
    4: result := TCore_AsymmetricAlgorithmNames.RsaOaepSha512;
    else
      raise Exception.Create('Unknown Core_AsymmetricAlgorithmNames');
  end;
end;

function TfrmAsymmetricEncryption.KeyLength: integer;
begin
  case ComboBoxAlgo.ItemIndex of
    0: result := 2048;
    1: result := 2048;
    2: result := 4096;
    3: result := 4096;
    4: result := 4096;
    else
      raise Exception.Create('Unknown Core_AsymmetricAlgorithmNames');
  end;
end;

procedure TfrmAsymmetricEncryption.btnSaveEncryptClick(Sender: TObject);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Text := EditEncrypted.Text;
    sl.SaveToFile(fDataFolder + 'Encrypted.txt');
  finally
    sl.Free;
  end;
end;

procedure TfrmAsymmetricEncryption.btnLoadClick(Sender: TObject);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(fDataFolder + 'Encrypted.txt');
    EditEncrypted.Text := sl.Text;
    btnDecrypt.Enabled := true;
  finally
    sl.Free;
  end;
end;

procedure TfrmAsymmetricEncryption.btnSaveKeysClick(Sender: TObject);
var
  PublicKey, PrivateKey: IBuffer;
  hs: HSTRING;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    PublicKey := fKey.ExportPublicKey;
    hs := TCryptographicBuffer.EncodeToBase64String(PublicKey);
    sl.Text := TWindowsString.HStringToString(hs);
    sl.SaveToFile(fDataFolder + 'Public.key');
    PrivateKey := fKey.Export(Core_CryptographicPrivateKeyBlobType.Pkcs8RawPrivateKeyInfo);
    hs := TCryptographicBuffer.EncodeToBase64String(PrivateKey);
    sl.Text := TWindowsString.HStringToString(hs);
    sl.SaveToFile(fDataFolder + 'PrivateAndPublic.key');
  finally
    sl.Free;
  end;
end;

procedure TfrmAsymmetricEncryption.btnLoadKeysClick(Sender: TObject);
var
  sl: TStringList;
  Keys: string;
  PublicKey, PrivateKey: IBuffer;
  hs: HSTRING;
  KeyBlob: IBuffer;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(fDataFolder + 'PrivateAndPublic.key');
    Keys := sl.Text;
  finally
    sl.Free;
  end;
  hs := TWindowsString.Create(Keys);
  KeyBlob := TCryptographicBuffer.DecodeFromBase64String(hs);
  fKey := fAlgoProvider.ImportKeyPair(KeyBlob, Core_CryptographicPrivateKeyBlobType.Pkcs8RawPrivateKeyInfo);
  EditKeySize.Text := fKey.KeySize.ToString;
  PublicKey := fKey.ExportPublicKey;
  hs := TCryptographicBuffer.EncodeToBase64String(PublicKey);
  EditPublicKey.Text := TWindowsString.HStringToString(hs);
  PrivateKey := fKey.Export(Core_CryptographicPrivateKeyBlobType.Pkcs8RawPrivateKeyInfo);
  hs := TCryptographicBuffer.EncodeToBase64String(PrivateKey);
  EditPrivateKey.Text := TWindowsString.HStringToString(hs);
  InitWithNewKeys;
end;

procedure TfrmAsymmetricEncryption.btnLoadPubKeyClick(Sender: TObject);
var
  sl: TStringList;
  PubKey: string;
  PublicKey: IBuffer;
  hs: HSTRING;
  KeyBlob: IBuffer;
begin
  fAlgoProvider := TCore_AsymmetricKeyAlgorithmProvider.OpenAlgorithm(SelectedAlgo);
  sl := TStringList.Create;
  try
    sl.LoadFromFile(fDataFolder + 'Public.key');
    PubKey := sl.Text;
  finally
    sl.Free;
  end;
  hs := TWindowsString.Create(PubKey);
  KeyBlob := TCryptographicBuffer.DecodeFromBase64String(hs);
  fKey := fAlgoProvider.ImportPublicKey(KeyBlob);
  EditKeySize.Text := fKey.KeySize.ToString;
  PublicKey := fKey.ExportPublicKey;
  hs := TCryptographicBuffer.EncodeToBase64String(PublicKey);
  EditPublicKey.Text := TWindowsString.HStringToString(hs);
  EditPrivateKey.Text := '';
  btnEncrypt.Enabled := true;
  btnLoad.Enabled := false;
  InitWithNewKeys;
end;

end.
