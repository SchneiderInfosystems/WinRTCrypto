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

unit SymmetricEncryptionFrm;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Win.WinRT,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Winapi.CommonTypes, Winapi.WinRT,
  Winapi.Security.Cryptography;

type
  TFrmSymmetricEncryption = class(TForm)
    LabelAlgo: TLabel;
    ComboBoxAlgo: TComboBox;
    btnCreateKey: TButton;
    LabelKey: TLabel;
    EditKey: TEdit;
    EditClear: TEdit;
    btnEncrypt: TButton;
    EditEncrypted: TEdit;
    btnDecrypt: TButton;
    EditResult: TEdit;
    LabelResult: TLabel;
    ShapeResult: TShape;
    btnSaveEncrypt: TButton;
    btnLoad: TButton;
    btnSaveKey: TButton;
    btnLoadKey: TButton;
    LabelKeySize: TLabel;
    EditKeySize: TEdit;
    cboKeySize: TComboBox;
    procedure btnCreateKeyClick(Sender: TObject);
    procedure btnEncryptClick(Sender: TObject);
    procedure btnDecryptClick(Sender: TObject);
    procedure btnSaveEncryptClick(Sender: TObject);
    procedure ComboBoxAlgoChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveKeyClick(Sender: TObject);
    procedure btnLoadKeyClick(Sender: TObject);
  private
    fDataFolder: string;
    fAlgoProvider: Core_ISymmetricKeyAlgorithmProvider;
    fKey: Core_ICryptographicKey;
    function SelectedAlgo: HSTRING;
    function SelectKeySizeInBytes: integer;
    procedure InitWithNewKeys;
  end;

var
  FrmSymmetricEncryption: TFrmSymmetricEncryption;

implementation

{$R *.dfm}

resourcestring
  rsTS = 'Test sample for symmetrical encryption %s at %s with Unicode Ж•₱₲₳‡₾';

procedure TFrmSymmetricEncryption.FormCreate(Sender: TObject);
begin
  ComboBoxAlgoChange(Sender);
end;

procedure TFrmSymmetricEncryption.btnCreateKeyClick(Sender: TObject);
var
  Key: IBuffer;
  hs: HSTRING;
begin
  fAlgoProvider := TCore_SymmetricKeyAlgorithmProvider.OpenAlgorithm(SelectedAlgo);
  Key := TCryptographicBuffer.GenerateRandom(SelectKeySizeInBytes);
  hs := TCryptographicBuffer.EncodeToBase64String(Key);
  EditKey.Text := TWindowsString.HStringToString(hs);
  fKey := fAlgoProvider.CreateSymmetricKey(Key);
  EditKeySize.Text := fKey.KeySize.ToString;
  InitWithNewKeys;
end;

procedure TFrmSymmetricEncryption.InitWithNewKeys;
begin
  btnEncrypt.Enabled := true;
  btnSaveKey.Enabled := true;
  EditEncrypted.Text := '';
  EditResult.Text := '';
  ShapeResult.Brush.Color := clBtnFace;
  LabelResult.Caption := '';
  btnEncrypt.Enabled := true;
  btnLoad.Enabled := true;
  btnDecrypt.Enabled := false;
end;

procedure TFrmSymmetricEncryption.btnSaveKeyClick(Sender: TObject);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Text := EditKey.Text;
    sl.SaveToFile(fDataFolder + 'Private.key');
  finally
    sl.Free;
  end;
end;

procedure TFrmSymmetricEncryption.btnLoadKeyClick(Sender: TObject);
var
  sl: TStringList;
  hs: HSTRING;
  Key: IBuffer;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(fDataFolder + 'Private.key');
    EditKey.Text := sl.Text;
    hs := TWindowsString.Create(sl.Text);
    Key := TCryptographicBuffer.DecodeFromBase64String(hs);
    fAlgoProvider := TCore_SymmetricKeyAlgorithmProvider.OpenAlgorithm(SelectedAlgo);
    fKey := fAlgoProvider.CreateSymmetricKey(Key);
    EditKeySize.Text := fKey.KeySize.ToString;
  finally
    sl.Free;
  end;
  InitWithNewKeys;
end;

procedure TFrmSymmetricEncryption.btnEncryptClick(Sender: TObject);
var
  data: TBytes;
  cleardata, encrypted: IBuffer;
  hs: HSTRING;
begin
  data := TEncoding.UTF8.GetBytes(EditClear.Text);
  cleardata := TCryptographicBuffer.CreateFromByteArray(length(data), @data[0]);
//  IV := TCryptographicBuffer.CreateFromByteArray(16, @data[0]);
  try
    encrypted := TCore_CryptographicEngine.Encrypt(fKey, cleardata, nil {IV});
    hs := TCryptographicBuffer.EncodeToBase64String(encrypted);
    EditEncrypted.Text := TWindowsString.HStringToString(hs);
    btnSaveEncrypt.Enabled := true;
    btnDecrypt.Enabled := true;
  except
    on e: exception do
    begin
      ShapeResult.Brush.Color := clRed;
      LabelResult.Caption := 'Test failed: ' + e.Message;
    end;
  end;
end;

procedure TFrmSymmetricEncryption.btnLoadClick(Sender: TObject);
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

procedure TFrmSymmetricEncryption.btnSaveEncryptClick(Sender: TObject);
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

procedure TFrmSymmetricEncryption.ComboBoxAlgoChange(Sender: TObject);
begin
  fDataFolder :=
    IncludeTrailingPathDelimiter(
      ExpandFileName(
        ExtractFileDir(Application.ExeName) + '\..\..\Data\' + ComboBoxAlgo.Text));
  if not DirectoryExists(fDataFolder, false) then
    ForceDirectories(fDataFolder);
  EditKey.Text := '';
  btnEncrypt.Enabled := false;
  btnDecrypt.Enabled := false;
  btnSaveKey.Enabled := false;
  EditClear.Text := Format(rsTS, [ComboBoxAlgo.Text, TimeToStr(Now)]);
  btnLoadKey.Enabled := FileExists(fDataFolder + 'Private.key');
  btnLoad.Enabled := FileExists(fDataFolder + 'Encrypted.txt');
end;

procedure TFrmSymmetricEncryption.btnDecryptClick(Sender: TObject);
var
  data: TBytes;
  cleardata, encrypted: IBuffer;
  hs: HSTRING;
begin
  hs := TWindowsString.Create(EditEncrypted.Text);
  encrypted := TCryptographicBuffer.DecodeFromBase64String(hs);
  try
    cleardata := TCore_CryptographicEngine.Decrypt(fKey, encrypted, nil {IV});
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

function TFrmSymmetricEncryption.SelectedAlgo: HSTRING;
begin
  case ComboBoxAlgo.ItemIndex of
    0: result := TCore_SymmetricAlgorithmNames.AesCbc;
    1: result := TCore_SymmetricAlgorithmNames.AesCbcPkcs7;
    2: result := TCore_SymmetricAlgorithmNames.AesCcm;
    3: result := TCore_SymmetricAlgorithmNames.AesEcb;
    4: result := TCore_SymmetricAlgorithmNames.AesEcbPkcs7;
    5: result := TCore_SymmetricAlgorithmNames.AesGcm;
    else
      raise Exception.Create('Unknown Core_SymmetricAlgorithmNames');
  end;

end;

function TFrmSymmetricEncryption.SelectKeySizeInBytes: integer;
begin
  result := StrToInt(cboKeySize.Text) div 8;
end;

end.
