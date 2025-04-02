{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2024 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

{$I Spring.inc}

unit Spring.Cryptography.DES;

{$R-}

interface

uses
  SysUtils,
  Spring,
  Spring.Cryptography,
  Spring.Cryptography.Base;

type
  /// <summary>
  ///   Data Encryption Standard (DES)
  /// </summary>
  TDES = class(TSymmetricAlgorithmBase, IDES)
  private
    const
      fCDefaultBlockSize = 8 * 8;
      fCDefaultKeySize = 8 * 8;
  protected
    procedure DoEncryptBlock(const inputBuffer: TBytes; var outputBuffer: TBytes); override;
    procedure DoDecryptBlock(const inputBuffer: TBytes; var outputBuffer: TBytes); override;
  public
    constructor Create;
  end;

  /// <summary>
  ///   Triple Data Encryption Standard Algorithm
  /// </summary>
  TTripleDES = class(TSymmetricAlgorithmBase, ITripleDES)
  private
    const
      fCDefaultBlockSize = 8 * 8;
      fCDefaultKeySize = 24 * 8;
  private
    fKey1: TBytes;
    fKey2: TBytes;
    fKey3: TBytes;
  protected
    procedure SetKey(const value: TBuffer); override;
    procedure DoEncryptBlock(const inputBuffer: TBytes; var outputBuffer: TBytes); override;
    procedure DoDecryptBlock(const inputBuffer: TBytes; var outputBuffer: TBytes); override;
  public
    constructor Create;
  end;

procedure EncryptData(const keyData, inData: array of Byte; var outData: array of Byte);
procedure DecryptData(const keyData, inData: array of Byte; var outData: array of Byte);

implementation

uses
  Spring.ResourceStrings;

type
  TKeyByte = array [0..5] of Byte;
  TDesMode = (dmEncry, dmDecry);

const
  BitIP: array [0..63] of Byte = (
    57, 49, 41, 33, 25, 17,  9,  1,
    59, 51, 43, 35, 27, 19, 11,  3,
    61, 53, 45, 37, 29, 21, 13,  5,
    63, 55, 47, 39, 31, 23, 15,  7,
    56, 48, 40, 32, 24, 16,  8,  0,
    58, 50, 42, 34, 26, 18, 10,  2,
    60, 52, 44, 36, 28, 20, 12,  4,
    62, 54, 46, 38, 30, 22, 14,  6
  );

  BitCP: array [0..63] of Byte = (
    39,  7, 47, 15, 55, 23, 63, 31,
    38,  6, 46, 14, 54, 22, 62, 30,
    37,  5, 45, 13, 53, 21, 61, 29,
    36,  4, 44, 12, 52, 20, 60, 28,
    35,  3, 43, 11, 51, 19, 59, 27,
    34,  2, 42, 10, 50, 18, 58, 26,
    33,  1, 41,  9, 49, 17, 57, 25,
    32,  0, 40,  8, 48, 16, 56, 24
  );

  BitExp: array [0..47] of Integer = (
    31,  0,  1,  2,  3,  4,  3,  4,  5,  6,  7,  8,  7,  8,  9, 10,
    11, 12, 11, 12, 13, 14, 15, 16, 15, 16, 17, 18, 19, 20, 19, 20,
    21, 22, 23, 24, 23, 24, 25, 26, 27, 28, 27, 28, 29, 30, 31, 0
  );

  BitPM: array [0..31] of Byte = (
    15, 6, 19, 20, 28, 11, 27, 16,  0, 14, 22, 25,  4, 17, 30,  9,
     1, 7, 23, 13, 31, 26,  2,  8, 18, 12, 29,  5, 21, 10,  3, 24
  );

  sBox: array [0..7] of array [0..63] of Byte = (
  ( 14,  4, 13,  1,  2, 15, 11,  8,  3, 10,  6, 12,  5,  9,  0,  7,
     0, 15,  7,  4, 14,  2, 13,  1, 10,  6, 12, 11,  9,  5,  3,  8,
     4,  1, 14,  8, 13,  6,  2, 11, 15, 12,  9,  7,  3, 10,  5,  0,
    15, 12,  8,  2,  4,  9,  1,  7,  5, 11,  3, 14, 10,  0,  6, 13  ),

  ( 15,  1,  8, 14,  6, 11,  3,  4,  9,  7,  2, 13, 12,  0,  5, 10,
     3, 13,  4,  7, 15,  2,  8, 14, 12,  0,  1, 10,  6,  9, 11,  5,
     0, 14,  7, 11, 10,  4, 13,  1,  5,  8, 12,  6,  9,  3,  2, 15,
    13,  8, 10,  1,  3, 15,  4,  2, 11,  6,  7, 12,  0,  5, 14,  9  ),

  ( 10,  0,  9, 14,  6,  3, 15,  5,  1, 13, 12,  7, 11,  4,  2,  8,
    13,  7,  0,  9,  3,  4,  6, 10,  2,  8,  5, 14, 12, 11, 15,  1,
    13,  6,  4,  9,  8, 15,  3,  0, 11,  1,  2, 12,  5, 10, 14,  7,
     1, 10, 13,  0,  6,  9,  8,  7,  4, 15, 14,  3, 11,  5,  2, 12  ),

  (  7, 13, 14,  3,  0,  6,  9, 10,  1,  2,  8,  5, 11, 12,  4, 15,
    13,  8, 11,  5,  6, 15,  0,  3,  4,  7,  2, 12,  1, 10, 14,  9,
    10,  6,  9,  0, 12, 11,  7, 13, 15,  1,  3, 14,  5,  2,  8,  4,
     3, 15,  0,  6, 10,  1, 13,  8,  9,  4,  5, 11, 12,  7,  2, 14  ),

  (  2, 12,  4,  1,  7, 10, 11,  6,  8,  5,  3, 15, 13,  0, 14,  9,
    14, 11,  2, 12,  4,  7, 13,  1,  5,  0, 15, 10,  3,  9,  8,  6,
     4,  2,  1, 11, 10, 13,  7,  8, 15,  9, 12,  5,  6,  3,  0, 14,
    11,  8, 12,  7,  1, 14,  2, 13,  6, 15,  0,  9, 10,  4,  5,  3  ),

  ( 12,  1, 10, 15,  9,  2,  6,  8,  0, 13,  3,  4, 14,  7,  5, 11,
    10, 15,  4,  2,  7, 12,  9,  5,  6,  1, 13, 14,  0, 11,  3,  8,
     9, 14, 15,  5,  2,  8, 12,  3,  7,  0,  4, 10,  1, 13, 11,  6,
     4,  3,  2, 12,  9,  5, 15, 10, 11, 14,  1,  7,  6,  0,  8, 13  ),

  (  4, 11,  2, 14, 15,  0,  8, 13,  3, 12,  9,  7,  5, 10,  6,  1,
    13,  0, 11,  7,  4,  9,  1, 10, 14,  3,  5, 12,  2, 15,  8,  6,
     1,  4, 11, 13, 12,  3,  7, 14, 10, 15,  6,  8,  0,  5,  9,  2,
     6, 11, 13,  8,  1,  4, 10,  7,  9,  5,  0, 15, 14,  2,  3, 12  ),

  ( 13,  2,  8,  4,  6, 15, 11,  1, 10,  9,  3, 14,  5,  0, 12,  7,
     1, 15, 13,  8, 10,  3,  7,  4, 12,  5,  6, 11,  0, 14,  9,  2,
     7, 11,  4,  1,  9, 12, 14,  2,  0,  6, 10, 13, 15,  3,  5,  8,
     2,  1, 14,  7,  4, 10,  8, 13, 15, 12,  9,  0,  3,  5,  6, 11  )
  );

  BitPMC1: array [0..55] of Byte = (
    56, 48, 40, 32, 24, 16,  8,
     0, 57, 49, 41, 33, 25, 17,
     9,  1, 58, 50, 42, 34, 26,
    18, 10,  2, 59, 51, 43, 35,
    62, 54, 46, 38, 30, 22, 14,
     6, 61, 53, 45, 37, 29, 21,
    13,  5, 60, 52, 44, 36, 28,
    20, 12,  4, 27, 19, 11,  3
  );

  BitPMC2: array [0..47] of Byte = (
    13, 16, 10, 23,  0,  4,
     2, 27, 14,  5, 20,  9,
    22, 18, 11,  3, 25,  7,
    15,  6, 26, 19, 12,  1,
    40, 51, 30, 36, 46, 54,
    29, 39, 50, 44, 32, 47,
    43, 48, 38, 55, 33, 52,
    45, 41, 49, 35, 28, 31
  );

procedure initPermutation(var inData: array of Byte);
var
  newData: array [0..7] of Byte;
  i: Integer;
begin
  FillChar(newData, 8, 0);

  for i := 0 to 63 do
  begin
    if (inData[BitIP[i] shr 3] and (1 shl (7- (BitIP[i] and $07)))) <> 0 then
    begin
      newData[i shr 3] := newData[i shr 3] or (1 shl (7-(i and $07)));
    end;
  end;

  for i := 0 to 7 do
  begin
    inData[i] := newData[i];
  end;
end;

procedure conversePermutation(var inData: array of Byte);
var
  newData: array [0..7] of Byte;
  i: Integer;
begin
  FillChar(newData, 8, 0);

  for i := 0 to 63 do
  begin
    if (inData[BitCP[i] shr 3] and (1 shl (7-(BitCP[i] and $07)))) <> 0 then
    begin
      newData[i shr 3] := newData[i shr 3] or (1 shl (7-(i and $07)));
    end;
  end;

  for i := 0 to 7 do
  begin
    inData[i] := newData[i];
  end;
end;

procedure expand(const inData: array of Byte; var outData: array of Byte);
var
  i: Integer;
begin
  FillChar(outData, 6, 0);

  for i := 0 to 47 do
  begin
    if (inData[BitExp[i] shr 3] and (1 shl (7-(BitExp[i] and $07)))) <> 0 then
    begin
      outData[i shr 3] := outData[i shr 3] or (1 shl (7-(i and $07)));
    end;
  end;
end;

procedure permutation(var inData: array of Byte);
var
  newData: array [0..3] of Byte;
  i: Integer;
begin
  FillChar(newData, 4, 0);

  for i := 0 to 31 do
  begin
    if (inData[BitPM[i] shr 3] and (1 shl (7-(BitPM[i] and $07)))) <> 0 then
    begin
      newData[i shr 3] := newData[i shr 3] or (1 shl (7-(i and $07)));
    end;
  end;

  for i := 0 to 3 do
  begin
    inData[i] := newData[i];
  end;
end;

function si(s,inByte: Byte): Byte;
var
  c: Byte;
begin
  c := (inByte and $20) or ((inByte and $1e) shr 1) or ((inByte and $01) shl 4);
  Result := (sBox[s][c] and $0F);
end;

procedure permutationChoose1(const inData: array of Byte; var outData: array of Byte);
var
  i: Integer;
begin
  FillChar(outData, 7, 0);

  for i := 0 to 55 do
  begin
    if (inData[BitPMC1[i] shr 3] and (1 shl (7-(BitPMC1[i] and $07)))) <> 0 then
    begin
      outData[i shr 3] := outData[i shr 3] or (1 shl (7-(i and $07)));
    end;
  end;
end;

procedure permutationChoose2(const inData: array of Byte; var outData: array of Byte);
var
  i: Integer;
begin
  FillChar(outData, 6, 0);

  for i := 0 to 47 do
  begin
    if (inData[BitPMC2[i] shr 3] and (1 shl (7-(BitPMC2[i] and $07)))) <> 0 then
    begin
      outData[i shr 3] := outData[i shr 3] or (1 shl (7-(i and $07)));
    end;
  end;
end;

procedure cycleMove(var inData: array of Byte; bitMove: Byte);
var
  i: Integer;
begin
  for i := 0 to bitMove - 1 do //FI:W528
  begin
    inData[0] := (inData[0] shl 1) or (inData[1] shr 7);
    inData[1] := (inData[1] shl 1) or (inData[2] shr 7);
    inData[2] := (inData[2] shl 1) or (inData[3] shr 7);
    inData[3] := (inData[3] shl 1) or ((inData[0] and $10) shr 4);
    inData[0] := (inData[0] and $0F);
  end;
end;

procedure makeKey(const inKey: array of Byte; var outKey: array of TKeyByte);
const
  bitDisplace: array [0..15] of Byte = ( 1,1,2,2, 2,2,2,2, 1,2,2,2, 2,2,2,1 );
var
  outData56: array [0..6] of Byte;
  key28l: array [0..3] of Byte;
  key28r: array [0..3] of Byte;
  key56o: array [0..6] of Byte;
  i: Integer;
begin
  permutationChoose1(inKey, outData56);

  key28l[0] := outData56[0] shr 4;
  key28l[1] := (outData56[0] shl 4) or (outData56[1] shr 4);
  key28l[2] := (outData56[1] shl 4) or (outData56[2] shr 4);
  key28l[3] := (outData56[2] shl 4) or (outData56[3] shr 4);
  key28r[0] := outData56[3] and $0F;
  key28r[1] := outData56[4];
  key28r[2] := outData56[5];
  key28r[3] := outData56[6];

  for i := 0 to 15 do
  begin
    cycleMove(key28l, bitDisplace[i]);
    cycleMove(key28r, bitDisplace[i]);
    key56o[0] := (key28l[0] shl 4) or (key28l[1] shr 4);
    key56o[1] := (key28l[1] shl 4) or (key28l[2] shr 4);
    key56o[2] := (key28l[2] shl 4) or (key28l[3] shr 4);
    key56o[3] := (key28l[3] shl 4) or (key28r[0]);
    key56o[4] := key28r[1];
    key56o[5] := key28r[2];
    key56o[6] := key28r[3];
    permutationChoose2(key56o, outKey[i]);
  end;
end;

procedure encry(const inData, subKey: array of Byte; var outData: array of Byte);
var
  outBuf: array [0..5] of Byte;
  buf: array [0..7] of Byte;
  i: Integer;
begin
  expand(inData, outBuf);

  for i := 0 to 5 do outBuf[i] := outBuf[i] xor subKey[i];

  buf[0] := outBuf[0] shr 2;
  buf[1] := ((outBuf[0] and $03) shl 4) or (outBuf[1] shr 4);
  buf[2] := ((outBuf[1] and $0F) shl 2) or (outBuf[2] shr 6);
  buf[3] := outBuf[2] and $3f;
  buf[4] := outBuf[3] shr 2;
  buf[5] := ((outBuf[3] and $03) shl 4) or (outBuf[4] shr 4);
  buf[6] := ((outBuf[4] and $0F) shl 2) or (outBuf[5] shr 6);
  buf[7] := outBuf[5] and $3f;

  for i := 0 to 7 do buf[i] := si(i, buf[i]);
  for i := 0 to 3 do outBuf[i] := (buf[i*2] shl 4) or buf[i*2+1];
  permutation(outBuf);
  for i := 0 to 3 do outData[i] := outBuf[i];
end;

procedure desData(desMode: TDesMode; const keyData, inData: array of Byte; var outData: array of Byte);
var
  i, j: Integer;
  temp: array [0..3] of Byte;
  buf: array [0..3] of Byte;
  subKey: array [0..15] of TKeyByte;
begin
  makeKey(keyData, subKey);

  for i := 0 to 7 do outData[i] := inData[i];
  initPermutation(outData);

  if desMode = dmEncry then
  begin
    for i := 0 to 15 do
    begin
      for j := 0 to 3 do temp[j] := outData[j];                 // temp = Ln
      for j := 0 to 3 do outData[j] := outData[j + 4];          // Ln + 1 = Rn
      encry(outData, subKey[i], buf);                           // Rn == Kn ==> buf
      for j := 0 to 3 do outData[j + 4] := temp[j] xor buf[j];  // Rn + 1 = Ln^buf
    end;

    for j := 0 to 3 do temp[j] := outData[j + 4];
    for j := 0 to 3 do outData[j + 4] := outData[j];
    for j := 0 to 3 do outData[j] := temp[j];
  end
  else if desMode = dmDecry then
  begin
    for i := 15 downto 0 do
    begin
      for j := 0 to 3 do temp[j] := outData[j];
      for j := 0 to 3 do outData[j] := outData[j + 4];
      encry(outData, subKey[i], buf);
      for j := 0 to 3 do outData[j + 4] := temp[j] xor buf[j];
    end;
    for j := 0 to 3 do temp[j] := outData[j + 4];
    for j := 0 to 3 do outData[j + 4] := outData[j];
    for j := 0 to 3 do outData[j] := temp[j];
  end;

  conversePermutation(outData);
end;

procedure EncryptData(const keyData, inData: array of Byte; var outData: array of Byte);
begin
  desData(dmEncry, keyData, inData, outData);
end;

procedure DecryptData(const keyData, inData: array of Byte; var outData: array of Byte);
begin
  desData(dmDecry, keyData, inData, outData);
end;


{$REGION 'TDES'}

constructor TDES.Create;
begin
  inherited Create([8 * 8], [8 * 8]);
  BlockSize := fCDefaultBlockSize;
  KeySize := fCDefaultKeySize;
end;

procedure TDES.DoEncryptBlock(const inputBuffer: TBytes;
  var outputBuffer: TBytes);
begin
  EncryptData(Key.AsBytes, inputBuffer, outputBuffer);
end;

procedure TDES.DoDecryptBlock(const inputBuffer: TBytes;
  var outputBuffer: TBytes);
begin
  DecryptData(Key.AsBytes, inputBuffer, outputBuffer);
end;

{$ENDREGION}


{$REGION 'TTripleDES'}

constructor TTripleDES.Create;
begin
  inherited Create([8 * 8], [16 * 8, 24 * 8]);
  BlockSize := fCDefaultBlockSize;
  KeySize := fCDefaultKeySize;
end;

procedure TTripleDES.DoDecryptBlock(const inputBuffer: TBytes;
  var outputBuffer: TBytes);
var
  temp1, temp2: TBytes;
begin
  SetLength(temp1, BlockSizeInBytes);
  SetLength(temp2, BlockSizeInBytes);
  DecryptData(fKey3, inputBuffer, temp1);
  EncryptData(fKey2, temp1, temp2);
  DecryptData(fKey1, temp2, outputBuffer);
end;

procedure TTripleDES.DoEncryptBlock(const inputBuffer: TBytes;
  var outputBuffer: TBytes);
var
  temp1, temp2: TBytes;
begin
  SetLength(temp1, BlockSizeInBytes);
  SetLength(temp2, BlockSizeInBytes);
  EncryptData(fKey1, inputBuffer, temp1);
  DecryptData(fKey2, temp1, temp2);
  EncryptData(fKey3, temp2, outputBuffer);
end;

procedure TTripleDES.SetKey(const value: TBuffer);
var
  localKey: TBuffer;
begin
  inherited SetKey(value);
  localKey := Key;  // To avoid some certain bug on thread.
  fKey1 := localKey.Left(8);
  fKey2 := localKey.Mid(8, 8);
  if localKey.Size = 16 then
    fKey3 := fKey1
  else if localKey.Size = 24 then
    fKey3 := localKey.Right(8)
  else
    raise ECryptographicException.CreateResFmt(@SIllegalKeySize, [value.Size]);
end;

{$ENDREGION}


end.
