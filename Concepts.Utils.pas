{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

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

{$I Concepts.inc}

unit Concepts.Utils;

interface

uses
  System.Classes, System.Rtti, System.TypInfo, System.SysUtils,
  Vcl.Controls, Vcl.Graphics,

  Data.DB;

type
  TBitmap = Vcl.Graphics.TBitmap;

procedure AppendLine(
  var   AToString : string;
  const ALine     : string
); overload;

procedure AppendLine(
  var   AToString : string;
  const ALine     : string;
  const AArgs     : array of const
); overload;

function AsPropString(AValue: TValue): string;

function AsFieldString(AValue: TValue): string;

procedure FixControlStylesForDrag(AParent: TControl);

procedure HourGlass(AProc: TProc);

procedure LockPaint(AControl: TWinControl);

procedure UnlockPaint(AControl: TWinControl);

function SetToString(
        ATypeInfo    : PTypeInfo;
  const AValue;
        AQuoteValues : Boolean = True;
        ABrackets    : Boolean = True;
        ATrimChars   : Integer = -1
): string;

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

implementation

uses
  Winapi.Windows, Winapi.Messages,
  System.StrUtils, System.Variants, System.Character,
  Vcl.Forms, Vcl.Dialogs;

var
  FRtti: TRttiContext;

{$REGION 'interfaced routines'}
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

function SetToString(ATypeInfo: PTypeInfo; const AValue;
  AQuoteValues: Boolean = True; ABrackets: Boolean = True;
  ATrimChars: Integer = -1): string;
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
    C: Char;
    N: Integer;
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

{$ENDREGION}

end.
