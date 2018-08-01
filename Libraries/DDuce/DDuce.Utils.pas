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
  System.Classes, System.SysUtils, System.UITypes, System.Rtti, System.Variants,
  System.TypInfo,
  Vcl.Forms, Vcl.Controls, Vcl.Graphics, Vcl.ExtCtrls,
  Data.DB;

type
  TBitmap = Vcl.Graphics.TBitmap;

procedure AppendLine(
  var AToString : string;
  const ALine   : string
); overload;

procedure AppendLine(
  var AToString : string;
  const ALine   : string;
  const AArgs   : array of const
); overload;

function AsPropString(AValue: TValue): string;

function AsFieldString(AValue: TValue): string;

procedure FixControlStylesForDrag(AParent: TControl);

procedure HourGlass(AProc: TProc);

procedure LockPaint(AControl: TWinControl);

procedure UnlockPaint(AControl: TWinControl);

procedure AutoSizeDisplayWidths(
  ADataSet : TDataSet;
  ACount   : Integer = 100;
  AOffset  : Integer = 2
); overload;

procedure AutoSizeDisplayWidths(
  ADataSet : TDataSet;
  AFont    : TFont;
  ACount   : Integer = 100;
  AOffset  : Integer = 0
); overload;

function AskConfirmation(const AMessage: string): Boolean;

{ Make the given form a child control of a given other visual control (in most
  cases this will be a TPanel). }

procedure AssignFormParent(
  AForm   : TForm;
  AParent : TWinControl
);

function ExtractText(
  const AString : string;
  const ADelim1 : string;
  const ADelim2 : string
): string;

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

function IsValidIP(const AIP: string): Boolean;

function ContainsFocus(AControl: TWinControl): Boolean;

function GetTextWidth(const AText: string): Integer; overload;

function GetMaxTextWidth(AStrings: TStrings): Integer;

function GetTextWidth(
  const AText : string;
  AFont       : TFont
): Integer; overload;

function GetTextHeight(
  const AText : string;
  AFont       : TFont
): Integer;

function SetToString(
  ATypeInfo : PTypeInfo;
  const AValue;
  AQuoteValues : Boolean = True;
  ABrackets    : Boolean = True;
  ATrimChars   : Integer = -1
): string;

implementation

uses
  Winapi.Messages,
  System.Character, System.StrUtils,
  Vcl.Dialogs;

var
  FRtti: TRttiContext;

{$REGION 'non-interfaced routines'}
function TryGetUnderlyingValue(const AValue: TValue; out AInnerValue: TValue)
  : Boolean;
var
  T : TRttiType;
  F : TRttiField;
begin
  T := FRtti.GetType(AValue.TypeInfo);
  F := T.GetField('FValue');
  Result := False;

  if Assigned(F) then
  begin
    AInnerValue := F.GetValue(AValue.GetReferenceToRawData);
    Result := True;
  end
  else
  begin
    AInnerValue := TValue.Empty;
  end;
end;
{$ENDREGION}

procedure AppendLine(var AToString : string; const ALine : string); overload;
begin
  if ALine <> '' then
  begin
    AToString := IfThen(AToString = '', ALine, AToString + #13#10 + ALine);
  end;
end;

procedure AppendLine(var AToString : string; const ALine : string;
  const AArgs : array of const ); overload;
begin
  AppendLine(AToString, Format(ALine, AArgs));
end;

{ Returns all property values of the instance in a string. The instance can
  be an object or a record. A record can be passed as a TValue using
  TValue.From<record-type>(record-instance). For an object you just pass the
  object instance as the argument.

  Example of a TValue variable holding a TRect record and an object:

    var
      V: TValue;
      R: TRect;
      S: string;
      O: TButton;
    begin
      ...
      V := V.From<TRect>(R);     // record types need to be casted to TValue
      S := AsPropString(V);
      ...
      O := TButton.Create(nil);
      try
        S := AsPropString(O);    // object types can be passed directly
      finally
        FreeAndNil(O);
      end;
    end;

    Because property values can be received by a method that can raise an exception
    we handle these exceptions
}

function AsPropString(AValue: TValue): string;
var
  P             : TRttiProperty;
  S             : string;
  V             : TValue;
  V2            : TValue;
  N             : Integer;
  ExcludedTypes : TTypeKinds;
begin
  Result := '';
  if not AValue.IsEmpty then
  begin
    ExcludedTypes := [
      tkClassRef, tkMethod, tkInterface, tkPointer, tkUnknown, tkArray,
      tkDynArray, tkClass
    ];
    N := 0;
    for P in FRtti.GetType(AValue.TypeInfo).GetProperties do
    begin
      if not (P.PropertyType.TypeKind in ExcludedTypes) and P.IsReadable then
      begin
        if Length(P.Name) > N then
          N := Length(P.Name);
      end;
    end;
    for P in FRtti.GetType(AValue.TypeInfo).GetProperties do
    begin
      if not (P.PropertyType.TypeKind in ExcludedTypes) and P.IsReadable then
      begin
        try
          if AValue.IsObject then
            V := P.GetValue(AValue.AsObject)
          else
            V := P.GetValue(AValue.GetReferenceToRawData);
        except
          S := '<error reading value>';
          AppendLine(Result, Format('%-*s = %s', [N, P.Name, S]));
          Continue;
        end;
        try
          if V.Kind = tkClass then
          begin
            S := P.Name + ': ' + V.AsObject.ClassName;
          end
          else if V.Kind = tkVariant then
            S := VarToStrDef(V.AsVariant, '')
          // don't append #0 characters to the result.
          else if (V.Kind in [tkChar, tkWChar]) and (V.ToString = #0) then
            S := ''
          else if V.Kind = tkRecord then
          begin
            if TryGetUnderlyingValue(V, V2) then
            begin
              if (V2.Kind in [tkChar, tkWChar]) and (V.ToString = #0) then
                S := ''
              else
                S := V2.ToString;
            end
            else
            begin
              S := '<error while executing TryGetUnderlyingValue>'
            end
          end
          else
            S := V.ToString;
        except
          S := '<error reading value>';
          AppendLine(Result, Format('%-*s = %s', [N, P.Name, S]));
          Continue;
        end;
        if S <> '' then
          AppendLine(Result, Format('%-*s = %s', [N, P.Name, S]));
      end;
    end;
  end;
end;

{ Returns all field values of the given instance in a string. The instance can
  be an object or a record. A record can be passed as a TValue using
  TValue.From<record-type>(record-instance). For an object you just pass the
  object instance as the argument.

  Example of a TValue variable holding a TRect record an object:

    var
      V: TValue;
      R: TRect;
      S: string;
      O: TButton;
    begin
      ...
      V := V.From<TRect>(R);      // record types need to be casted to TValue
      S := AsFieldString(V);
      ...
      O := TButton.Create(nil);
      try
        S := AsFieldString(O);    // object types can be passed directly
      finally
        FreeAndNil(O);
      end;
    end;
}

function AsFieldString(AValue : TValue): string;
var
  F             : TRttiField;
  S             : string;
  V             : TValue;
  V2            : TValue;
  N             : Integer;
  ExcludedTypes : TTypeKinds;
begin
  Result := '';
  if not AValue.IsEmpty then
  begin
    ExcludedTypes := [
      tkClassRef, tkMethod, tkInterface, tkPointer, tkUnknown, tkArray,
      tkDynArray
    ];
    N := 0;
    for F in FRtti.GetType(AValue.TypeInfo).GetFields do
    begin
      if not (F.FieldType.TypeKind in ExcludedTypes) then
      begin
        if Length(F.Name) > N then
          N := Length(F.Name);
      end;
    end;
    for F in FRtti.GetType(AValue.TypeInfo).GetFields do
    begin
      if not (F.FieldType.TypeKind in ExcludedTypes) then
      begin
        if AValue.IsObject then
          V := F.GetValue(AValue.AsObject)
        else
          V := F.GetValue(AValue.GetReferenceToRawData);
        if V.Kind = tkClass then
        begin
          if V.AsObject is TComponent then
            S := TComponent(V.AsObject).Name + ': ' + V.AsObject.ClassName;
        end
        // don't append #0 characters to the result.
        else if (V.Kind in [tkChar, tkWChar]) and (V.ToString = #0) then
          S := ''
        else if V.Kind = tkVariant then
          S := VarToStrDef(V.AsVariant, '')
        else if V.Kind = tkRecord then
        begin
          if TryGetUnderlyingValue(V, V2) then
          begin
            if (V2.Kind in [tkChar, tkWChar]) and (V2.ToString = #0) then
              S := ''
            else
               S := V2.ToString;
          end
          else
          begin
            //raise Exception.Create('no TryGetUnderlyingValue');
          end
        end
        else
          S := V.ToString;
        AppendLine(Result, Format('%-*s = %s', [N, F.Name, S]));
      end;
    end;
  end;
end;

procedure HourGlass(AProc: TProc);
begin
  Screen.Cursor := crHourGlass;
  try
    AProc();
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure LockPaint(AControl: TWinControl);
begin
  if Assigned(AControl) and (AControl.Handle <> 0) then
  begin
    SendMessage(AControl.Handle, WM_SETREDRAW, 0, 0);
  end;
end;

procedure UnlockPaint(AControl: TWinControl);
begin
  if Assigned(AControl) and (AControl.Handle <> 0) then
  begin
    SendMessage(AControl.Handle, WM_SETREDRAW, 1, 0);
    RedrawWindow(
      AControl.Handle,
      nil,
      0,
      RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN
    );
  end;
end;

{ Adjusts the control style of all child controls of a given control so that
  during a drag operation the drag image is shown when dragging over the
  control.
  If csDisplayDragImage is not set only the drag cursor will be shown. }

procedure FixControlStylesForDrag(AParent: TControl);
var
  I: Integer;
begin
  AParent.ControlStyle := AParent.ControlStyle + [csDisplayDragImage];
  if AParent is TWinControl then
    with TWinControl(AParent) do
      for I := 0 to ControlCount - 1 do
        FixControlStylesForDrag(Controls[I]);
end;

procedure AutoSizeDisplayWidths(ADataSet : TDataSet;
                                AFont    : TFont;
                                ACount   : Integer;
                                AOffset  : Integer);
var
  BM : TBookmark;
  I  : Integer;
  J  : Integer;
  L  : Integer;

  function GetTextWidth(const AText: string; AFont: TFont): Integer;
  var
    Bitmap  : TBitmap;
    SL      : TStringList;
    I, W, R : Integer;
  begin
    SL := TStringList.Create;
    try
      SL.Text := AText;
      Bitmap := TBitmap.Create;
      try
        Bitmap.Canvas.Font.Assign(AFont);
        R := 0;
        for I := 0 to SL.Count - 1 do
        begin
          W := Bitmap.Canvas.TextWidth(SL[I]);
          if W > R then
            R := W;
        end;
        Result := R div AFont.Size;
      finally
        Bitmap.Free;
      end;
    finally
      SL.Free;
    end;
  end;

begin
  if not Assigned(ADataSet) then
    raise Exception.Create('ADataSet not assigned!');
  ADataSet.DisableControls;
  try
    BM := ADataSet.Bookmark;
    try
      for J := 0 to ADataSet.Fields.Count - 1 do
        ADataSet.Fields[J].DisplayWidth := Length(ADataSet.Fields[J].DisplayLabel);

      ADataSet.First;
      I := 0;
      while (I < ACount) and not ADataSet.Eof do
      begin
        for J := 0 to ADataSet.Fields.Count - 1 do
        begin
          if ADataSet.Fields[J].DataType in
            [ftMemo, ftWideMemo, ftString, ftWideString]  then
            L := GetTextWidth(ADataSet.Fields[J].DisplayText, AFont) + AOffset
          else
            L := Length(ADataSet.Fields[J].DisplayText) + AOffset;
          if L > ADataSet.Fields[J].DisplayWidth then
            ADataSet.Fields[J].DisplayWidth := L;
        end;
        ADataSet.Next;
        Inc(I);
      end;
    finally
      ADataSet.Bookmark := BM;
    end;
  finally
    ADataSet.EnableControls;
  end;
end;

{
  REMARK : This method is not suitable for filtered datasets. For filtered
  datasets the records should be enumerated with the dataset's FindFirst and
  FindNext methods. We didn't use those methods because when used in combination
  with a ClientDataSet (with fetch on demand enabled) this causes the provider
  to fetch all the records from the server.
}

procedure AutoSizeDisplayWidths(ADataSet : TDataSet; ACount : Integer;
  AOffSet : Integer);
var
  BM : TBookmark;
  I  : Integer;
  J  : Integer;
  L  : Integer;

  function GetTextWidth(const AText: string): Integer;
  var
    SL      : TStringList;
    I, W, R : Integer;
  begin
    SL := TStringList.Create;
    try
      SL.Text := AText;
      R := 0;
      for I := 0 to SL.Count - 1 do
      begin
        W := Length(SL[I]);
        if W > R then
          R := W;
      end;
      Result := R;
    finally
      SL.Free;
    end;
  end;

begin
  if not Assigned(ADataSet) then
    raise Exception.Create('ADataSet not assigned!');
  ADataSet.DisableControls;
  try
    BM := ADataSet.Bookmark;
    try
      for J := 0 to ADataSet.Fields.Count - 1 do
        ADataSet.Fields[J].DisplayWidth := Length(ADataSet.Fields[J].DisplayLabel);
      ADataSet.First;
      I := 0;
      while (I < ACount) and not ADataSet.Eof do
      begin
        for J := 0 to ADataSet.Fields.Count - 1 do
        begin
          if ADataSet.Fields[J].DataType in
            [ftMemo, {ftWideMemo,} ftString, ftWideString]  then
            L := GetTextWidth(ADataSet.Fields[J].DisplayText) + AOffset
          else
            L := Length(ADataSet.Fields[J].DisplayText) + AOffset;
          if L > ADataSet.Fields[J].DisplayWidth then
            ADataSet.Fields[J].DisplayWidth := L;
        end;
        ADataSet.Next;
        Inc(I);
      end;
    finally
      ADataSet.Bookmark := BM;
    end;
  finally
    ADataSet.EnableControls;
  end;
end;

function AskConfirmation(const AMessage: string): Boolean;
var
  MR: TModalResult;
begin
  MR := MessageDlg(AMessage, mtConfirmation, [mbYes, mbNo], 0);
  if MR = mrYes then
  begin
    Result := True;
  end
  else
  begin
    Result := False;
  end
end;

procedure AssignFormParent(AForm: TForm; AParent: TWinControl);
begin
  if AForm.Parent <> AParent then
  begin;
    AForm.Parent      := AParent;
    AForm.BorderStyle := bsNone;
    AForm.Align       := alClient;
    AForm.Visible     := True;
  end;
end;

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
  if P1 > 0 then
  begin
    P2 := PosEx(ADelim2, AString, P1 + 1);
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

function IsValidIP(const AIP: string): Boolean;
var
  SL : TStringList;
  N  : Integer;
begin
  SL := TStringList.Create;
  try
    SL.StrictDelimiter := True;
    SL.Delimiter := '.';
    SL.DelimitedText := AIP;
    if SL.Count <> 4 then
      Exit(False);
    for N := 0 to SL.Count - 1 do
      if not StrToIntDef(SL[N], -1) in [0..255] then
        Exit(False);

    Exit(True);
  finally
    SL.Free;
  end;
end;

function ContainsFocus(AControl: TWinControl): Boolean;
var
  H : HWND;
  C : TWinControl;
begin
  H := Winapi.Windows.GetFocus;
  C := FindControl(H);
  if not Assigned(C) then
    Result := False
  else
  begin
    Result := AControl.ContainsControl(C);
  end;
end;

function GetTextWidth(const AText: string): Integer;
var
  SL      : TStringList;
  I, W, R : Integer;
begin
  SL := TStringList.Create;
  try
    SL.Text := AText;
    R := 0;
    for I := 0 to SL.Count - 1 do
    begin
      W := Length(SL[I]);
      if W > R then
        R := W;
    end;
    Result := R;
  finally
    SL.Free;
  end;
end;

function GetMaxTextWidth(AStrings: TStrings): Integer;
var
  I : Integer;
  N : Integer;
begin
  Result := 0;
  if Assigned(AStrings) then
  begin
    for I := 0 to AStrings.Count - 1 do
    begin
      N := GetTextWidth(AStrings[I]);
      if N > Result then
        Result := N;
    end;
  end;
end;

function GetTextHeight(const AText: string; AFont: TFont): Integer;
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.Canvas.Font.Assign(AFont);
    Result := Bitmap.Canvas.TextExtent(AText).cy;
  finally
    Bitmap.Free;
  end;
end;

{ Converts a given set instance to comma seperated string values, which can
  be optionally quoted. The whole string can be optionally enclosed between
  brackets.
  TrimChars defines the count of the prefix characters which should be omitted
  when contructing the value names. If TrimChars is not specified, the function
  will determine the prefix automatically (if the prefix is lowercase as
  usually is the case for enumerated type values in Delphi.)

  Example:

  var
    S: string;
  begin
    S := SetToString(TypeInfo(TAnchors), [akLeft, akTop]);
    ShowMessage(S);
  end;

  => Following string is shown: '(Left, Top)'
}

function SetToString(ATypeInfo: PTypeInfo; const AValue; AQuoteValues: Boolean;
  ABrackets: Boolean; ATrimChars: Integer): string;
var
  S    : TIntegerSet;
  I    : Integer;
  N    : Integer;
  Name : string;

  function GetOrdValue(Info: PTypeInfo; const SetParam): Integer;
  begin
    Result := 0;

    case GetTypeData(Info)^.OrdType of
      otSByte, otUByte:
        Result := Byte(SetParam);
      otSWord, otUWord:
        Result := Word(SetParam);
      otSLong, otULong:
        Result := Integer(SetParam);
    end;
  end;

  function GetPrefixLength(const AString: string): Integer;
  var
    C : Char;
    N : Integer;
  begin
    N := 0;
    if Length(AString) > 0 then
    begin
      C := AString[1];
      while (N < Length(AString)) and C.IsLower do
      begin
        Inc(N);
        C := AString[N + 1];
      end;
    end;
    Result := N;
  end;

begin
  Result := '';
  Integer(S) := GetOrdValue(ATypeInfo, AValue);
  ATypeInfo := GetTypeData(ATypeInfo)^.CompType^;
  for I := 0 to SizeOf(Integer) * 8 - 1 do
  begin
    if I in S then
    begin
      if Result <> '' then
        Result := Result + ',';
      Name := GetEnumName(ATypeInfo, I);

      if ATrimChars >= 0 then
        N := ATrimChars
      else
        N := GetPrefixLength(Name);

      if N > 0 then
        Name := Copy(Name, N + 1, Length(Name) - N + 1);

      if AQuoteValues then
        Name := QuotedStr(Name);

      Result := Result + Name;
    end;
  end;
  if ABrackets and (Result <> '') then
    Result := '(' + Result + ')';
end;

end.

