{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.Utils;

interface

uses
  Winapi.Windows,
  System.Classes, System.SysUtils,
  Vcl.Graphics, Vcl.ExtCtrls;

function ExtractText(
  const AString : string;
  const ADelim1 : string;
  const ADelim2 : string
): string;

function GetTextWidth(const AText: string; AFont: TFont): Integer;

procedure OptimizeWidth(APanel: TPanel);

procedure OutputDebugString(const AString: string); overload;

procedure OutputDebugString(
  const AString : string;
  const AValues : array of const
); overload;

{ Draws formatted text on a canvas using XML tags

    Tag      Function           Example
    ---------------------------------------------------------------
    <b>      Bold               <b>bold text</b>
    <i>      Italic             <i>italic text</i>
    <u>      underline          <u>underlined text</u>
    <f=X>    Font name          <fn=Arial>Arial font example</fn>
    <fc=X>   Font color         <fc=clBlue>blue text</fc>
    <fs=X>   Font size          <fs=30>big text</fs>
    <x=X>    Specified offset
    <c>      Background color
    <br>     Break              line 1<br>line2<br>line3
}

function DrawFormattedText(
  const ARect   : TRect;
  const ACanvas : TCanvas;
  const AText   : string
): Integer;

implementation

uses
  System.Character, System.StrUtils;

{ Returns the total width for a given text and font. }

function GetTextWidth(const AText: string; AFont: TFont): Integer;
var
  Bitmap : TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.Canvas.Font.Assign(AFont);
    Result := Bitmap.Canvas.TextExtent(AText).cx;
  finally
    Bitmap.Free;
  end;
end;

{ Optimizes witdth according to its contents. }

procedure OptimizeWidth(APanel: TPanel);
var
  S: string;
begin
  S := APanel.Caption;
  if Trim(S) <> '' then
    APanel.Width := GetTextWidth(APanel.Caption, APanel.Font) + 10
  else
    APanel.Width := 0;
end;

{ Extracts the string between ADelim1 and ADelim2 from the given AString. }

function ExtractText(const AString: string; const ADelim1, ADelim2: string)
  : string;
var
  P1, P2: Integer;
begin
  Result := '';
  P1 := Pos(ADelim1, AString);
  if P1 > 0 then begin
    P2 := PosEx(ADelim2, AString, P1+1);
    if P2 > 0 then
      Result := Copy(AString, P1 + 1, P2 - P1 - 1);
  end;
end;

procedure OutputDebugString(const AString: string);
begin
  OutputDebugStringW(PChar(AString));
end;

procedure OutputDebugString(const AString : string; const AValues: array of const
); overload;
begin
  OutputDebugString(Format(AString, AValues));
end;

function DrawFormattedText(const ARect: TRect; const ACanvas: TCanvas;
  const AText: string): Integer;

  function GetTagValue(const ATag: string): string;
  var
    P: Integer;
  begin
    P := Pos('=', ATag);

    if P = 0 then
      Result := ''
    else
      Result := Copy(ATag, P + 1, MaxInt);
  end;

  function ColorCodeToColor(const AValue: string): TColor;
  var
    LHexValue: string;
  begin
    Result := 0;
    if AValue <> '' then
    begin
      if (Length(AValue) >= 2) and SameText(Copy(AValue, 1, 2), 'CL') then
      begin
       // Delphi color
        Result := StringToColor(AValue);
      end
      else if AValue[1] = '#' then
      begin
       // Web color
        LHexValue := Copy(AValue, 2, 6);
        Result := RGB(
          StrToInt('$' + Copy(LHexValue, 1, 2)),
          StrToInt('$' + Copy(LHexValue, 3, 2)),
          StrToInt('$' + Copy(LHexValue, 5, 2))
        );
      end
      else
        Result := StrToIntDef(AValue, 0);
    end;
  end;

const
  TAG_BOLD       = 'b';
  TAG_ITALIC     = 'i';
  TAG_UNDERLINE  = 'u';
  TAG_COLOR      = 'c';
  TAG_FONT       = 'f';
  TAG_FONT_COLOR = 'fc';
  TAG_FONT_SIZE  = 'fs';
  TAG_BREAK      = 'br';
  TAG_X          = 'x';
var
  X             : Integer;
  Y             : Integer;
  I             : Integer;
  C             : Char;
  LCharWidth    : Integer;
  LLineHeight   : Integer;
  LTag          : string;
  LTagValue     : string;
  LOldFontName  : string;
  LOldFontColor : TColor;
  LOldFontSize  : Integer;
  LOldColor     : TColor;
begin
  LOldFontColor := ACanvas.Font.Color;
  LOldFontName  := ACanvas.Font.Name;
  LOldFontSize  := ACanvas.Font.Size;
  LOldColor     := ACanvas.Brush.Color;

  X := ARect.Left;
  Y := ARect.Top;
  I := 1;

  LLineHeight := ACanvas.TextHeight('Ag');

  while I <= Length(AText) do
  begin
    C := AText[I];

    // Is this a tag?
    if C = '<' then
    begin
      LTag := '';
      Inc(I);
      // Find the end of then tag
      while (AText[I] <> '>') and (I <= Length(AText)) do
      begin
        LTag := LTag + AText[I];
        Inc(I);
      end;
      // Simple tags
      if SameText(LTag, TAG_BOLD) then
        ACanvas.Font.Style := ACanvas.Font.Style + [fsBold]
      else if SameText(LTag, TAG_ITALIC) then
        ACanvas.Font.Style := ACanvas.Font.Style + [fsItalic]
      else if SameText(LTag, TAG_UNDERLINE) then
        ACanvas.Font.Style := ACanvas.Font.Style + [fsUnderline]
      else if SameText(LTag, TAG_BREAK) then
      begin
        X := ARect.Left;
        Inc(Y, LLineHeight);
      end
      // Closing tags
      else if SameText(LTag, '/' + TAG_BOLD) then
        ACanvas.Font.Style := ACanvas.Font.Style - [fsBold]
      else if  SameText(LTag, '/' + TAG_ITALIC) then
        ACanvas.Font.Style := ACanvas.Font.Style - [fsItalic]
      else if  SameText(LTag, '/' + TAG_UNDERLINE) then
        ACanvas.Font.Style := ACanvas.Font.Style - [fsUnderline]
      else if  SameText(LTag, '/' + TAG_FONT_SIZE) then
        ACanvas.Font.Size := LOldFontSize
      else if  SameText(LTag, '/' + TAG_FONT) then
        ACanvas.Font.Name := LOldFontName
      else if  SameText(LTag, '/' + TAG_FONT_COLOR) then
        ACanvas.Font.Color := LOldFontColor
      else if  SameText(LTag, '/' + TAG_COLOR) then
        ACanvas.Brush.Color := LOldColor
      // Tags with values <tag=value>...</tag>
      else
      begin
       // Get the tag value (everything after '=')
        LTagValue := GetTagValue(LTag);

        if LTagValue <> '' then
        begin
         // Remove the value from the tag name
          LTag := Copy(LTag, 1, Pos('=', LTag) - 1);

          if LTag = TAG_FONT then
          begin
            LOldFontName := ACanvas.Font.Name;
            ACanvas.Font.Name := LTagValue;
          end
          else if LTag = TAG_FONT_SIZE then
          begin
            LOldFontSize := ACanvas.Font.Size;
            StrToIntDef(LTagValue, ACanvas.Font.Size);
          end
          else if LTag = TAG_FONT_COLOR then
          begin
            LOldFontColor := ACanvas.Font.Color;
            try
              ACanvas.Font.Color := ColorCodeToColor(LTagValue);
            except
               // Just in case the canvas color is invalid
            end;
          end
          else if LTag = TAG_COLOR then
          begin
            LOldColor := ACanvas.Brush.Color;
            try
              ACanvas.Brush.Color := ColorCodeToColor(LTagValue);
            except
               // Just in case the canvas color is invalid
            end;
          end
          else if LTag = TAG_X then
          begin
            X := ARect.Left + LTagValue.ToInteger;
          end;
        end;
      end;
    end
    else if not C.IsControl then
    begin
      LCharWidth := ACanvas.TextWidth(C);
      if Y + LLineHeight < ARect.Bottom then
      begin
        ACanvas.Brush.Style := bsClear;
        ACanvas.TextOut(X, Y, C);
      end;
      X := X + LCharWidth;
    end;
    Inc(I);
  end;
  Result := X - ARect.Left;
end;


end.
