{
  Copyright (C) 2013-2025 Tim Sinaeve tim.sinaeve@gmail.com

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}

{$I DDuce.inc}

unit DDuce.CustomImageDrawHook;

{ Including this unit makes disabled images look better in menu's. It displays
  them as grey-scaled. }

interface

// source: http://stackoverflow.com/questions/6003018/make-disabled-menu-and-toolbar-images-look-better
// Author: Rodrigo Ruz

uses
  Winapi.Windows, Winapi.CommCtrl,
  System.Math, System.SysUtils,
  Vcl.Graphics, Vcl.ImgList;

implementation

type
  TJumpOfs = Integer;
  PPointer = ^Pointer;

  PXRedirCode = ^TXRedirCode;
  TXRedirCode = packed record
    Jump: Byte;
    Offset: TJumpOfs;
  end;

  PAbsoluteIndirectJmp = ^TAbsoluteIndirectJmp;
  TAbsoluteIndirectJmp = packed record
    OpCode: Word;
    Addr: PPointer;
  end;

  TCustomImageListHack = class(TCustomImageList);

var
  DoDrawBackup   : TXRedirCode;

function GetActualAddr(Proc: Pointer): Pointer;
begin
  if Proc <> nil then
  begin
    if (Win32Platform = VER_PLATFORM_WIN32_NT) and (PAbsoluteIndirectJmp(Proc).OpCode = $25FF) then
      Result := PAbsoluteIndirectJmp(Proc).Addr^
    else
      Result := Proc;
  end
  else
    Result := nil;
end;

procedure HookProc(Proc, Dest: Pointer; var BackupCode: TXRedirCode);
var
  n:      NativeUInt;
  Code: TXRedirCode;
begin
  Proc := GetActualAddr(Proc);
  Assert(Proc <> nil);
  if ReadProcessMemory(GetCurrentProcess, Proc, @BackupCode, SizeOf(BackupCode), n) then
  begin
    Code.Jump := $E9;
    Code.Offset := PAnsiChar(Dest) - PAnsiChar(Proc) - SizeOf(Code);
    WriteProcessMemory(GetCurrentProcess, Proc, @Code, SizeOf(Code), n);
  end;
end;

procedure UnhookProc(Proc: Pointer; var BackupCode: TXRedirCode);
var
  n: NativeUInt;
begin
  if (BackupCode.Jump <> 0) and (Proc <> nil) then
  begin
    Proc := GetActualAddr(Proc);
    Assert(Proc <> nil);
    WriteProcessMemory(GetCurrentProcess, Proc, @BackupCode, SizeOf(BackupCode), n);
    BackupCode.Jump := 0;
  end;
end;

procedure Bitmap2GrayScale(const BitMap: TBitmap);
type
  TRGBArray = array[0..32767] of TRGBTriple;
  PRGBArray = ^TRGBArray;
var
  x, y, Gray: Integer;
  Row       : PRGBArray;
begin
  BitMap.PixelFormat := pf24Bit;
  for y := 0 to BitMap.Height - 1 do
  begin
    Row := BitMap.ScanLine[y];
    for x := 0 to BitMap.Width - 1 do
    begin
      Gray             := (Row[x].rgbtRed + Row[x].rgbtGreen + Row[x].rgbtBlue) div 3;
      Row[x].rgbtRed   := Gray;
      Row[x].rgbtGreen := Gray;
      Row[x].rgbtBlue  := Gray;
    end;
  end;
end;

//from ImgList.GetRGBColor
function GetRGBColor(Value: TColor): DWORD;
begin
  Result := ColorToRGB(Value);
  case Result of
    clNone:
      Result := CLR_NONE;
    clDefault:
      Result := CLR_DEFAULT;
  end;
end;

procedure New_Draw(Self: TObject; Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean);
var
  MaskBitMap : TBitmap;
  GrayBitMap : TBitmap;
begin
  with TCustomImageListHack(Self) do
  begin
    if not HandleAllocated then Exit;
    if Enabled then
      ImageList_DrawEx(Handle, Index, Canvas.Handle, X, Y, 0, 0, GetRGBColor(BkColor), GetRGBColor(BlendColor), Style)
    else
    begin
      GrayBitMap := TBitmap.Create;
      MaskBitMap := TBitmap.Create;
      try
        GrayBitMap.SetSize(Width, Height);
        MaskBitMap.SetSize(Width, Height);
        GetImages(Index, GrayBitMap, MaskBitMap);
        Bitmap2GrayScale(GrayBitMap);
        BitBlt(Canvas.Handle, X, Y, Width, Height, MaskBitMap.Canvas.Handle, 0, 0, SRCERASE);
        BitBlt(Canvas.Handle, X, Y, Width, Height, GrayBitMap.Canvas.Handle, 0, 0, SRCINVERT);
      finally
        GrayBitMap.Free;
        MaskBitMap.Free;
      end;
    end;
  end;
end;

procedure HookDraw;
begin
  HookProc(@TCustomImageListHack.DoDraw, @New_Draw, DoDrawBackup);
end;

procedure UnHookDraw;
begin
  UnhookProc(@TCustomImageListHack.DoDraw, DoDrawBackup);
end;

initialization
 HookDraw;
finalization
 UnHookDraw;

end.
