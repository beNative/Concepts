{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2017 Spring4D Team                           }
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
{$R-,Q-}

unit Spring.Data.VirtualDataSet;

{$IFDEF DELPHIXE4_UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  Classes,
  DB,
  Generics.Defaults,
  SysUtils,
  Spring.Collections,
  Spring.Data.IndexList;

type
  EVirtualDataSetException = class(Exception);

  {$IFDEF NEXTGEN}
  TRecordBuffer = TRecBuf;
  PAnsiChar = MarshaledAString;
{$ENDIF !NEXTGEN}
{$IFNDEF DELPHIXE3_UP}
  TValueBuffer  = Pointer;
{$ENDIF}
  PVariantList = ^TVariantList;
  TVariantList = array[0..0] of OleVariant;
  PArrayRecInfo = ^TArrayRecInfo;

  TArrayRecInfo = record
    Index: Integer;
    BookmarkFlag: TBookmarkFlag;
  end;

  TCustomVirtualDataSet = class;

  TChangeRecordEvent = procedure(Sender: TCustomVirtualDataSet; Index: Integer) of object;
  TGetFieldValueEvent = procedure(Sender: TCustomVirtualDataSet;
    Field: TField; Index: Integer; var Value: Variant) of object;
  TGetRecordCountEvent = procedure(Sender: TCustomVirtualDataSet; var Count: Integer) of object;

  TCustomVirtualDataSet = class(TDataSet)
  private
    fRowBufSize: Integer;
    fFilterBuffer: TRecordBuffer;
    fOldValueBuffer: TRecordBuffer;
    fReadOnly: Boolean;
    fCurrent: Integer;
    fInternalOpen: Boolean;
    fModifiedFields: IList<TField>;
    fFieldsCache: IDictionary<string,TField>;
    fFilterCache: IDictionary<string, Variant>;
    fIndexList: TIndexList;

    fOnDeleteRecord: TChangeRecordEvent;
    fOnGetFieldValue: TGetFieldValueEvent;
    fOnGetRecordCount: TGetRecordCountEvent;
    fOnInsertRecord: TChangeRecordEvent;
    fOnUpdateRecord: TChangeRecordEvent;
    {$IFDEF DELPHIXE3_UP}
    fReserved: Pointer;
    {$ENDIF}
    function GetIndex: Integer;
    procedure SetIndex(const value: Integer);
  protected
    type
      TBlobStream = class(TMemoryStream)
      strict private
        fField: TBlobField;
        fDataSet: TCustomVirtualDataSet;
        fFieldNo: Integer;
        fModified: Boolean;
        fData: Variant;
        fFieldData: Variant;
        procedure ReadBlobData;
      protected
        function Realloc(var NewCapacity: LongInt): Pointer; override;
      public
        constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
        destructor Destroy; override;

        function Write(const Buffer; Count: LongInt): LongInt; override;
        procedure Truncate;
      end;

    function IsFiltered: Boolean;

    // Abstract methods
    procedure DoDeleteRecord(Index: Integer); virtual;
    procedure DoFilterRecord(var Accept: Boolean); virtual;
    procedure DoGetFieldValue(Field: TField; Index: Integer; var Value: Variant); virtual;
    procedure DoPostRecord(Index: Integer; Append: Boolean); virtual;
    procedure UpdateFilter; virtual; abstract;
    procedure RebuildPropertiesCache; virtual; abstract;

    // Basic overrides
    function GetCanModify: Boolean; override;
    function GetRecNo: {$IFDEF DELPHIX_TOKYO_UP}Integer{$ELSE}LongInt{$ENDIF}; override;
    function GetRecordCount: Integer; override;
    procedure SetFiltered(Value: Boolean); override;

    procedure DoOnNewRecord; override;
    procedure InternalEdit; override;
    procedure SetRecNo(Value: Integer); override;
    procedure SetCurrent(value: Integer); virtual;
    procedure SetRecBufSize;
    procedure RebuildFieldCache;

    // Abstract overrides
    function AllocRecordBuffer: TRecordBuffer; {$IFNDEF NEXTGEN}override;{$ENDIF}
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); {$IFNDEF NEXTGEN}override;{$ENDIF}
    {$IFDEF NEXTGEN}
    function AllocRecBuf: TRecBuf; override;
    procedure FreeRecBuf(var Buffer: TRecBuf); override;
    {$ENDIF}

    procedure DataEvent(Event: TDataEvent; Info: {$IFDEF DELPHIXE2_UP}NativeInt{$ELSE}LongInt{$ENDIF}); override;

    {$IFDEF DELPHIXE4_UP}
    procedure GetBookmarkData(Buffer: TRecBuf; Data: TBookmark); override;
    procedure SetBookmarkData(Buffer: TRecBuf; Data: TBookmark); override;
    {$ENDIF}

    {$IFNDEF NEXTGEN}
    {$IFDEF DELPHIXE3_UP}
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: TBookmark); override;
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: TBookmark); override;
    {$ENDIF}
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    {$ENDIF}

    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override;

    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
      DoCheck: Boolean): TGetResult; override;
    function GetRecordSize: Word; override;
    procedure BindFields(Binding: Boolean); override;

    procedure InternalAddRecord(Buffer: {$IFDEF DELPHIXE3_UP}TRecordBuffer{$ELSE}Pointer{$ENDIF}; Append: Boolean); override;
    procedure InternalClose; override;
    procedure InternalCreateFields; virtual;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    procedure InternalGotoBookmark(Bookmark: {$IFDEF DELPHIXE3_UP}TBookmark{$ELSE}Pointer{$ENDIF}); override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalRefresh; override;
    procedure InternalSetToRecord(Buffer: TRecordBuffer); override;
    function IsCursorOpen: Boolean; override;

    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag); override;

    function InternalGetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; virtual;
    procedure SetFieldData(const field: TField; const buffer: Variant); overload;

    property FilterCache: IDictionary<string, Variant> read fFilterCache;
    property IndexList: TIndexList read fIndexList;
    {$IFDEF DELPHIXE3_UP}
    property Reserved: Pointer read fReserved write fReserved;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function FindField(const FieldName: string): TField; reintroduce;

    function BookmarkValid(Bookmark: TBookmark): Boolean; override;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;

    function GetBlobFieldData(FieldNo: Integer; var Buffer: TBlobByteData): Integer; override;
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
    function Lookup(const KeyFields: string; const KeyValues: Variant;
      const ResultFields: string): Variant; override;
    function GetActiveRecBuf(var RecBuf: TRecordBuffer): Boolean; virtual;
    function GetFieldData(Field: TField; {$IFDEF DELPHIXE4_UP}var{$ENDIF} Buffer: TValueBuffer): Boolean; override;
    function GetFieldData(Field: TField; {$IFDEF DELPHIXE4_UP}var{$ENDIF} Buffer: TValueBuffer; NativeFormat: Boolean): Boolean; override;
    procedure SetFieldData(Field: TField; Buffer: TValueBuffer); overload; override;
    procedure SetFieldData(Field: TField; Buffer: TValueBuffer; NativeFormat: Boolean); overload; override;

    /// <summary>
    ///   Represents current cursor index in the dataset.
    /// </summary>
    property Current: Integer read fCurrent;

    /// <summary>
    ///   Represents the current index of the dataset.
    /// </summary>
    property Index: Integer read GetIndex write SetIndex;

    property ModifiedFields: IList<TField> read fModifiedFields;
    property ReadOnly: Boolean read fReadOnly write fReadOnly default False;

    property OnDeleteRecord: TChangeRecordEvent read fOnDeleteRecord write fOnDeleteRecord;
    property OnGetFieldValue: TGetFieldValueEvent read fOnGetFieldValue write fOnGetFieldValue;
    property OnGetRecordCount: TGetRecordCountEvent read fOnGetRecordCount write fOnGetRecordCount;
    property OnInsertRecord: TChangeRecordEvent read fOnInsertRecord write fOnInsertRecord;
    property OnUpdateRecord: TChangeRecordEvent read fOnUpdateRecord write fOnUpdateRecord;
  end;

  TVirtualDataSet = class(TCustomVirtualDataSet)
  published
    property Active;
    property Filtered;
    property ReadOnly;

    property AfterCancel;
    property AfterClose;
    property AfterDelete;
    property AfterEdit;
    property AfterInsert;
    property AfterOpen;
    property AfterPost;
    property AfterRefresh;
    property AfterScroll;
    property BeforeCancel;
    property BeforeClose;
    property BeforeDelete;
    property BeforeEdit;
    property BeforeInsert;
    property BeforeOpen;
    property BeforePost;
    property BeforeRefresh;
    property BeforeScroll;

    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
  end;

implementation

uses
{$IFNDEF DELPHIXE3_UP}
  Contnrs,
{$ELSE}
  Generics.Collections,
{$ENDIF}
  DBConsts,
  FmtBcd,
  Math,
  Variants,
  VarUtils,
  Spring,
  Spring.Data.ActiveX;

resourcestring
  SUnsupportedFieldType = 'Unsupported field type (%s) in field %s';
  SPersistentFieldsRequired = 'Virtual dataset can only be used with persistent fields.';
  SIndexOutOfRange = 'Index out of range';


{$IF Defined(DELPHIXE3_UP) and not Defined(DELPHIXE8_UP)}
type
  TDBBitConverter = class
  private
    class procedure InternalFromMove<T>(const Value: T; var B: TArray<Byte>; Offset: Integer = 0); static; {$IFNDEF DELPHIXE4}inline;{$ENDIF}
    class function InternalIntoMove<T>(const B: TArray<Byte>; Offset: Integer = 0): T; static; inline;
  public
    class procedure UnsafeFrom<T>(const Value: T; var B: TArray<Byte>; Offset: Integer = 0); static; {$IFNDEF DELPHIXE4}inline;{$ENDIF}
    class function UnsafeInTo<T>(const B: TArray<Byte>; Offset: Integer = 0): T; static; inline;

    class procedure UnsafeFromVariant(const Value: Variant; var B: TArray<Byte>; Offset: Integer = 0); static; inline;
    class function UnsafeInToVariant(const B: TArray<Byte>; Offset: Integer = 0): Variant; static; inline;
  end;
{$IFEND}

{$IFDEF NEXTGEN}
type
  WideString = UnicodeString;
{$ENDIF}

function DataSetLocateThrough(DataSet: TDataSet; const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
var
  FieldCount: Integer;
  Fields: TObjectList{$IFDEF DELPHIXE3_UP}<TField>{$ENDIF};
  Fld: TField;
  Bookmark: TBookmark;

  function CompareField(var Field: TField; Value: Variant): Boolean;
  var
    S: string;
  begin
    if Field.DataType in [ftString, ftWideString] then
    begin
      if Value = Null then
        Result := Field.IsNull
      else
      begin
        S := Field.AsString;
        if loPartialKey in Options then
          Delete(S, Length(Value) + 1, MaxInt);
        if loCaseInsensitive in Options then
          Result := AnsiSameText(S, Value)
        else
          Result := AnsiSameStr(S, Value);
      end;
    end
    else
      Result := (Field.Value = Value);
  end;

  function CompareRecord: Boolean;
  var
    I: Integer;
  begin
    if FieldCount = 1 then
    begin
      Fld := TField(Fields.First);
      Result := CompareField(Fld, KeyValues)
    end
    else
    begin
      Result := True;
      for I := 0 to FieldCount - 1 do
      begin
        Fld := TField(Fields[I]);
        Result := Result and CompareField(Fld, KeyValues[I]);
      end;
    end;
  end;

begin
  Result := False;
  DataSet.CheckBrowseMode;
  if DataSet.IsEmpty then
    Exit;

  Fields := TObjectList{$IFDEF DELPHIXE3_UP}<TField>{$ENDIF}.Create(False);
  try
    DataSet.GetFieldList(Fields, KeyFields);
    FieldCount := Fields.Count;
    Result := CompareRecord;
    if Result then
      Exit;
    DataSet.DisableControls;
    try
      Bookmark := DataSet.Bookmark;
      try
        DataSet.First;
        while not DataSet.Eof do
        begin
          Result := CompareRecord;
          if Result then
            Break;
          DataSet.Next;
        end;
      finally
        if not Result and DataSet.BookmarkValid(TBookmark(Bookmark)) then
          DataSet.Bookmark := Bookmark;
      end;
    finally
      DataSet.EnableControls;
    end;
  finally
    Fields.Free;
  end;
end;

function FieldListCheckSum(const dataSet: TDataSet): NativeInt;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to dataSet.Fields.Count - 1 do
    Result := Result + (NativeInt(dataSet.Fields[i]) shr (i mod SizeOf(NativeInt)));
end;

{$IF Defined(DELPHIXE3_UP) and not Defined(DELPHIXE8_UP)}
class procedure TDBBitConverter.InternalFromMove<T>(const Value: T;
  var B: TArray<Byte>; Offset: Integer);
begin
  Move(Value, B[Offset], SizeOf(T));
end;

class function TDBBitConverter.InternalIntoMove<T>(const B: TArray<Byte>;
  Offset: Integer): T;
begin
  Move(B[Offset], Result, SizeOf(T));
end;

class procedure TDBBitConverter.UnsafeFrom<T>(const Value: T;
  var B: TArray<Byte>; Offset: Integer);
type
  PT = ^T;
begin
  PT(@B[Offset])^ := Value;
end;

class function TDBBitConverter.UnsafeInTo<T>(const B: TArray<Byte>;
  Offset: Integer): T;
type
  PT = ^T;
begin
  Result := PT(@B[Offset])^;
end;

class procedure TDBBitConverter.UnsafeFromVariant(const Value: Variant;
  var B: TArray<Byte>; Offset: Integer);
begin
  InternalFromMove<Variant>(Value, B, Offset);
end;

class function TDBBitConverter.UnsafeInToVariant(const B: TArray<Byte>;
  Offset: Integer): Variant;
begin
  InternalIntoMove<Variant>(B, Offset);
end;
{$IFEND}


{$REGION 'TCustomVirtualDataSet'}

constructor TCustomVirtualDataSet.Create(AOwner: TComponent);
var
  comparer: IEqualityComparer<string>;
begin
  inherited Create(AOwner);
  fModifiedFields := TCollections.CreateList<TField>;
  fIndexList := TIndexList.Create;

  comparer := TStringComparer.OrdinalIgnoreCase;
  fFieldsCache := TCollections.CreateDictionary<string,TField>(50, comparer);
  fFilterCache := TCollections.CreateDictionary<string,Variant>(50, comparer);
end;

destructor TCustomVirtualDataSet.Destroy;
begin
  fIndexList.Free;
  inherited Destroy;
end;

function TCustomVirtualDataSet.AllocRecordBuffer: TRecordBuffer;
begin
  if not (csDestroying in ComponentState) then
  begin
    Pointer(Result) := AllocMem(fRowBufSize);
    Initialize(PVariantList(Result + SizeOf(TArrayRecInfo))^, Fields.Count);
  end
  else
    Pointer(Result) := nil;
end;

{$IFDEF NEXTGEN}
function TCustomVirtualDataSet.AllocRecBuf: TRecBuf;
begin
  Result := AllocRecordBuffer;
end;
{$ENDIF}

procedure TCustomVirtualDataSet.BindFields(Binding: Boolean);
begin
  inherited BindFields(Binding);
  RebuildFieldCache;
end;

function TCustomVirtualDataSet.BookmarkValid(Bookmark: TBookmark): Boolean;
begin
  Result := Assigned(Bookmark) and Assigned(PObject(Bookmark)^)
    and fIndexList.Contains(PObject(Bookmark)^);
end;

function TCustomVirtualDataSet.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
const
  RetCodes: array[Boolean, Boolean] of ShortInt = ((2, -1), (1, 0));
begin
  Result := RetCodes[Bookmark1 = nil, Bookmark2 = nil];
  if Result = 2 then
    if PNativeUInt(Bookmark1)^ < PNativeUInt(Bookmark2)^ then
      Result := -1
    else if PNativeUInt(Bookmark1)^ > PNativeUInt(Bookmark2)^ then
      Result := 1
    else
      Result := 0;
end;

function TCustomVirtualDataSet.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result := TBlobStream.Create(Field as TBlobField, Mode);
end;

procedure TCustomVirtualDataSet.DataEvent(Event: TDataEvent;
  Info: {$IFDEF DELPHIXE2_UP}NativeInt{$ELSE}LongInt{$ENDIF});
begin
  case Event of
    deLayoutChange:
      if Active and Assigned(Reserved)
        and (FieldListCheckSum(Self) <> NativeInt(Reserved)) then
        Reserved := nil;
  end;
  inherited DataEvent(Event, Info);
end;

procedure TCustomVirtualDataSet.DoDeleteRecord(Index: Integer);
begin
  if Assigned(fOnDeleteRecord) then
    fOnDeleteRecord(Self, Index);
end;

procedure TCustomVirtualDataSet.DoFilterRecord(var Accept: Boolean);
begin
  OnFilterRecord(Self, Accept);
end;

procedure TCustomVirtualDataSet.DoGetFieldValue(Field: TField; Index: Integer;
  var Value: Variant);
begin
  if Assigned(fOnGetFieldValue) then
    fOnGetFieldValue(Self, Field, Index, Value);
end;

procedure TCustomVirtualDataSet.DoOnNewRecord;
begin
  fModifiedFields.Clear;

  if Pointer(fOldValueBuffer) = nil then
    fOldValueBuffer := AllocRecordBuffer
  else
    Finalize(PVariantList(fOldValueBuffer + SizeOf(TArrayRecInfo))^, Fields.Count);

  InitRecord({$IFDEF DELPHIXE4_UP}NativeInt(fOldValueBuffer){$ELSE}fOldValueBuffer{$ENDIF});
  inherited DoOnNewRecord;
end;

procedure TCustomVirtualDataSet.DoPostRecord(Index: Integer; Append: Boolean);
begin
  case State of
    dsEdit:
      if Assigned(fOnUpdateRecord) then
        fOnUpdateRecord(Self, Index);
    dsInsert:
      if Assigned(fOnInsertRecord) then
        fOnInsertRecord(Self, Index);
  end
end;

function TCustomVirtualDataSet.FindField(const FieldName: string): TField;
begin
  if not fFieldsCache.TryGetValue(FieldName, Result) then
    Result := nil;
end;

procedure TCustomVirtualDataSet.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  Finalize(PVariantList(Buffer + SizeOf(TArrayRecInfo))^, Fields.Count);
  FreeMem(Pointer(Buffer));
end;

{$IFDEF NEXTGEN}
procedure TCustomVirtualDataSet.FreeRecBuf(var Buffer: TRecBuf);
begin
  FreeRecordBuffer(Buffer);
end;
{$ENDIF}

function TCustomVirtualDataSet.GetActiveRecBuf(var RecBuf: TRecordBuffer): Boolean;
begin
  Pointer(RecBuf) := nil;
  case State of
    dsBlockRead, dsBrowse:
      if IsEmpty then
        Pointer(RecBuf) := nil
      else
        Pointer(RecBuf) := Pointer(ActiveBuffer);
    dsNewValue, dsInsert, dsEdit:
      Pointer(RecBuf) := Pointer(ActiveBuffer);
    dsCalcFields, dsInternalCalc:
      Pointer(RecBuf) := Pointer(CalcBuffer);
    dsFilter:
      Pointer(RecBuf) := Pointer(fFilterBuffer);
  end;
  Result := Pointer(RecBuf) <> nil;
end;

function TCustomVirtualDataSet.GetBlobFieldData(FieldNo: Integer;
  var Buffer: TBlobByteData): Integer;
begin
  Result := inherited GetBlobFieldData(FieldNo, Buffer);
end;

{$IFDEF DELPHIXE4_UP}
procedure TCustomVirtualDataSet.GetBookmarkData(Buffer: TRecBuf; Data: TBookmark);
begin
  PObject(Data)^ := fIndexList[PArrayRecInfo(Buffer).Index];
end;

procedure TCustomVirtualDataSet.SetBookmarkData(Buffer: TRecBuf; Data: TBookmark);
begin
  if PArrayRecInfo(Buffer).BookmarkFlag in [bfCurrent, bfInserted] then
    PArrayRecInfo(Buffer).Index := fIndexList.IndexOf(PObject(@Data[0])^)
  else
    PArrayRecInfo(Buffer).Index := -1;
end;
{$ENDIF}

{$IFNDEF NEXTGEN}
{$IFDEF DELPHIXE3_UP}
procedure TCustomVirtualDataSet.GetBookmarkData(Buffer: TRecordBuffer; Data: TBookmark);
begin
  PObject(Data)^ := fIndexList[PArrayRecInfo(Buffer).Index];
end;

procedure TCustomVirtualDataSet.SetBookmarkData(Buffer: TRecordBuffer; Data: TBookmark);
begin
  if PArrayRecInfo(Buffer).BookmarkFlag in [bfCurrent, bfInserted] then
    PArrayRecInfo(Buffer).Index := fIndexList.IndexOf(PObject(@Data[0])^)
  else
    PArrayRecInfo(Buffer).Index := -1;
end;
{$ENDIF}

procedure TCustomVirtualDataSet.GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  PObject(Data)^ := fIndexList[PArrayRecInfo(Buffer).Index];
end;

procedure TCustomVirtualDataSet.SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  if PArrayRecInfo(Buffer).BookmarkFlag in [bfCurrent, bfInserted] then
    PArrayRecInfo(Buffer).Index := fIndexList.IndexOf(PObject(Data)^)
  else
    PArrayRecInfo(Buffer).Index := -1;
end;
{$ENDIF}

function TCustomVirtualDataSet.GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;
begin
  Result := PArrayRecInfo(Buffer).BookmarkFlag;
end;

function TCustomVirtualDataSet.GetCanModify: Boolean;
begin
  Result := not fReadOnly;
end;

function TCustomVirtualDataSet.GetFieldData(Field: TField;
  {$IFDEF DELPHIXE4_UP}var{$ENDIF} Buffer: TValueBuffer;
  NativeFormat: Boolean): Boolean;
var
  recBuf: TRecordBuffer;
  Data: Variant;

  {$IFDEF DELPHIXE3_UP}
  procedure CurrToBuffer(const C: Currency);
  var
    buff: TValueBuffer;
  begin
    if NativeFormat then
    begin
      SetLength(buff, SizeOf(Currency));
      TDBBitConverter.UnsafeFrom<Currency>(C, buff);
      DataConvert(Field, buff, Buffer, True)
    end
    else
      TDBBitConverter.UnsafeFrom<Currency>(C, Buffer);
  end;
  {$ELSE}
  procedure CurrToBuffer(const C: Currency);
  begin
    if NativeFormat then
      DataConvert(Field, @C, Buffer, True)
    else
      Currency(Buffer^) := C;
  end;
  {$ENDIF}

  {$IFDEF DELPHIXE3_UP}
  procedure VarToBuffer;
  var
    TempBuff: TValueBuffer;
    PData: Pointer;
  begin
    case Field.DataType of
      ftGuid, ftFixedChar, ftString:
      begin
        PAnsiChar(Buffer)[Field.Size] := #0;
        TempBuff := TEncoding.Default.GetBytes(string(tagVariant(Data).bStrVal));
        Move(TempBuff[0], Buffer[0], Length(TempBuff));
        PAnsiChar(Buffer)[Min(Field.Size, Length(TempBuff))] := #0;
      end;
      ftFixedWideChar, ftWideString:
      begin
        TempBuff := TEncoding.Unicode.GetBytes(tagVariant(Data).bstrVal);
        SetLength(TempBuff, Length(TempBuff) + SizeOf(Char));
        TempBuff[Length(TempBuff) - 2] := 0;
        TempBuff[Length(TempBuff) - 1] := 0;
        Move(TempBuff[0], Buffer[0], Length(TempBuff));
      end;
      ftShortint:
        if tagVariant(Data).vt = VT_UI1 then
          TDBBitConverter.UnsafeFrom<ShortInt>(ShortInt(tagVariant(Data).cVal), Buffer)
        else
          TDBBitConverter.UnsafeFrom<ShortInt>(ShortInt(tagVariant(Data).bVal), Buffer);
      ftByte:
        if tagVariant(Data).vt = VT_UI1 then
          TDBBitConverter.UnsafeFrom<Byte>(Byte(tagVariant(Data).cVal), Buffer)
        else
          TDBBitConverter.UnsafeFrom<Byte>(tagVariant(Data).bVal, Buffer);
      ftSmallint:
        if tagVariant(Data).vt = VT_UI1 then
          TDBBitConverter.UnsafeFrom<SmallInt>(Byte(tagVariant(Data).cVal), Buffer)
        else
          TDBBitConverter.UnsafeFrom<SmallInt>(tagVariant(Data).iVal, Buffer);
      ftWord:
        if tagVariant(Data).vt = VT_UI1 then
          TDBBitConverter.UnsafeFrom<Word>(tagVariant(Data).bVal, Buffer)
        else
          TDBBitConverter.UnsafeFrom<Word>(tagVariant(Data).uiVal, Buffer);
      ftAutoInc, ftInteger:
        TDBBitConverter.UnsafeFrom<Integer>(Data, Buffer);
      ftLongWord:
        TDBBitConverter.UnsafeFrom<Cardinal>(Data, Buffer);
      ftFloat, ftCurrency:
        if tagVariant(Data).vt = VT_R8 then
          TDBBitConverter.UnsafeFrom<Double>(tagVariant(Data).dblVal, Buffer)
        else
          TDBBitConverter.UnsafeFrom<Double>(Data, Buffer);
      ftFMTBCD:
        TDBBitConverter.UnsafeFrom<TBcd>(VarToBcd(Data), Buffer);
      ftBCD:
        if tagVariant(Data).vt = VT_CY then
          CurrToBuffer(tagVariant(Data).cyVal)
        else
          CurrToBuffer(Data);
      ftBoolean:
        TDBBitConverter.UnsafeFrom<WordBool>(tagVariant(Data).vbool, Buffer);
      ftDate, ftTime, ftDateTime:
        if NativeFormat then
        begin
          SetLength(TempBuff, SizeOf(Double));
          TDBBitConverter.UnsafeFrom<Double>(data, TempBuff);
          DataConvert(Field, TempBuff, Buffer, True);
        end
        else
          TDBBitConverter.UnsafeFrom<Double>(tagVariant(Data).date, Buffer);
      ftBytes, ftVarBytes:
        if NativeFormat then
        begin
          PData := VarArrayLock(Data);
          try
            DataConvert(Field, BytesOf(PData, VarArrayLength(Data, 1)), Buffer, True);
          finally
            VarArrayUnlock(Data);
          end;
        end
        else
          TDBBitConverter.UnsafeFromVariant(Data, Buffer);
      ftInterface:
      begin
        TempBuff := BytesOf(@Data, SizeOf(IUnknown));
        Move(TempBuff[0], Buffer[0], SizeOf(IUnknown));
      end;
      ftIDispatch:
      begin
        TempBuff := BytesOf(@Data, SizeOf(IDispatch));
        Move(TempBuff[0], Buffer[0], SizeOf(IDispatch));
      end;
      ftLargeInt:
      begin
        if PDecimal(@Data).sign > 0 then
          TDBBitConverter.UnsafeFrom<Int64>(-1 * PDecimal(@Data).Lo64, Buffer)
        else
          TDBBitConverter.UnsafeFrom<Int64>(PDecimal(@Data).Lo64, Buffer);
      end;
      ftBlob..ftTypedBinary, ftVariant, ftWideMemo: TDBBitConverter.UnsafeFromVariant(Data, Buffer);
    else
      DatabaseErrorFmt(SUnsupportedFieldType, [FieldTypeNames[Field.DataType],
        Field.DisplayName]);
    end;
  end;
  {$ELSE}
  procedure VarToBuffer;
  var
    TempBuff: TArray<Byte>;
  begin
    case Field.DataType of
      ftGuid, ftFixedChar, ftString:
      begin
        PAnsiChar(Buffer)[Field.Size] := #0;
        TempBuff := TEncoding.Default.GetBytes(string(tagVariant(Data).bStrVal));
        Move(TempBuff[0], PByte(Buffer)[0], Length(TempBuff));
        PAnsiChar(Buffer)[Min(Field.Size, Length(TempBuff))] := #0;
      end;
      ftFixedWideChar, ftWideString:
      begin
        TempBuff := TEncoding.Unicode.GetBytes(tagVariant(Data).bstrVal);
        SetLength(TempBuff, Length(TempBuff) + SizeOf(Char));
        TempBuff[Length(TempBuff) - 2] := 0;
        TempBuff[Length(TempBuff) - 1] := 0;
        Move(TempBuff[0], PByte(Buffer)[0], Length(TempBuff));
      end;
      ftShortint:
        if tagVariant(Data).vt = VT_UI1 then
          ShortInt(Buffer^) := ShortInt(tagVariant(Data).cVal)
        else
          ShortInt(Buffer^) := ShortInt(tagVariant(Data).bVal);
      ftByte:
        if tagVariant(Data).vt = VT_UI1 then
          Byte(Buffer^) := Byte(tagVariant(Data).cVal)
        else
          Byte(Buffer^) := tagVariant(Data).bVal;
      ftSmallint:
        if tagVariant(Data).vt = VT_UI1 then
          SmallInt(Buffer^) := Byte(tagVariant(Data).cVal)
        else
          SmallInt(Buffer^) := tagVariant(Data).iVal;
      ftWord:
        if tagVariant(Data).vt = VT_UI1 then
          Word(Buffer^) := tagVariant(Data).bVal
        else
          Word(Buffer^) := tagVariant(Data).uiVal;
      ftAutoInc, ftInteger:
        Integer(Buffer^) := Data;
      ftLongWord:
        Cardinal(Buffer^) := tagVariant(Data).uintVal;
      ftFloat, ftCurrency:
        if tagVariant(Data).vt = VT_R8 then
          Double(Buffer^) := tagVariant(Data).dblVal
        else
          Double(Buffer^) := Data;
      ftFMTBCD:
        TBcd(Buffer^) := VarToBcd(Data);
      ftBCD:
        if tagVariant(Data).vt = VT_CY then
          CurrToBuffer(tagVariant(Data).cyVal)
        else
          CurrToBuffer(Data);
      ftBoolean:
      begin
        VarAsType(Data, VT_BOOL);
        WordBool(Buffer^) := tagVariant(Data).vbool;
      end;
      ftDate, ftTime, ftDateTime:
      begin
        VarAsType(Data, VT_DATE);
        if NativeFormat then
          DataConvert(Field, @date, Buffer, True)
        else
          TOleDate(Buffer^) := Data;
      end;
      ftBytes, ftVarBytes:
        if NativeFormat then
          DataConvert(Field, @Data, Buffer, True)
        else
          OleVariant(Buffer^) := Data;
      ftInterface:
        IUnknown(Buffer^) := Data;
      ftIDispatch:
        IDispatch(Buffer^) := Data;
      ftLargeInt:
        if PDecimal(@Data).sign > 0 then
          LargeInt(Buffer^) := -1 * PDecimal(@Data).Lo64
        else
          LargeInt(Buffer^) := PDecimal(@Data).Lo64;
      ftBlob .. ftTypedBinary, ftVariant, ftWideMemo:
        OleVariant(Buffer^) := Data;
    else
      DatabaseErrorFmt(SUnsupportedFieldType, [FieldTypeNames[Field.DataType],
        Field.DisplayName]);
    end;
  end;
  {$ENDIF}

  procedure RefreshBuffers;
  begin
    Reserved := Pointer(FieldListCheckSum(Self));
    UpdateCursorPos;
    Resync([]);
  end;

  function DataToInt64: Int64;
  begin
    if PDecimal(@Data).sign > 0 then
      Result := -1 * PDecimal(@Data).Lo64
    else
      Result := PDecimal(@Data).Lo64;
  end;

begin
  if not Assigned(Reserved) then
    RefreshBuffers;

  Result := GetActiveRecBuf(recBuf);

  if not Result then
    Exit;

  Data := PVariantList(recBuf + SizeOf(TArrayRecInfo))[Field.Index];

  if VarIsEmpty(Data) then
  begin
    DoGetFieldValue(Field, PArrayRecInfo(recBuf).Index, Data);
    if VarIsEmpty(Data) then
      Data := Null;

    if VarType(Data) = varInt64 then
      PVariantList(recBuf + SizeOf(TArrayRecInfo))[Field.Index] := DataToInt64
    else
      PVariantList(recBuf + SizeOf(TArrayRecInfo))[Field.Index] := Data;
  end;

  Result := not VarIsNull(Data);
  if Result and (Buffer <> nil) then
    VarToBuffer;
end;

function TCustomVirtualDataSet.GetFieldData(Field: TField;
  {$IFDEF DELPHIXE4_UP}var{$ENDIF} Buffer: TValueBuffer): Boolean;
begin
  Result := GetFieldData(Field, Buffer, True);
end;

function TCustomVirtualDataSet.GetIndex: Integer;
var
  recBuf: TRecordBuffer;
begin
  Result := -1;
  CheckActive;
  if GetActiveRecBuf(recBuf) and (PArrayRecInfo(recBuf).BookmarkFlag = bfCurrent) then
    Result := PArrayRecInfo(recBuf).Index;
end;

function TCustomVirtualDataSet.GetRecNo: {$IFDEF DELPHIX_TOKYO_UP}Integer{$ELSE}LongInt{$ENDIF};
var
  recBuf: TRecordBuffer;
begin
  CheckActive;
  Result := -1;
  if GetActiveRecBuf(recBuf) and (PArrayRecInfo(recBuf).BookmarkFlag = bfCurrent) then
    Result := PArrayRecInfo(recBuf).Index + 1;
end;

function TCustomVirtualDataSet.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
var
  accept: Boolean;
  tmp: TDataSetState;
begin
  if not IsFiltered then
    Exit(InternalGetRecord(Buffer, GetMode, DoCheck));

  fFilterBuffer := Buffer;
  tmp := SetTempState(dsFilter);
  try
    Accept := True;
    repeat
      Result := InternalGetRecord(Buffer, GetMode, DoCheck);
      if Result = grOK then
      begin
        DoFilterRecord(Accept);
        if not Accept and (GetMode = gmCurrent) then
          Result := grError;
      end;
    until Accept or (Result <> grOK);
  except
    ApplicationHandleException(Self);
    Result := grError;
  end;
  RestoreState(tmp);
end;

function TCustomVirtualDataSet.GetRecordCount: Integer;
begin
  Result := -1;
  if Assigned(fOnGetRecordCount) then
    fOnGetRecordCount(Self, Result);
end;

function TCustomVirtualDataSet.GetRecordSize: Word;
begin
  Result := SizeOf(TArrayRecInfo);
end;

procedure TCustomVirtualDataSet.InternalAddRecord(
  Buffer: {$IFDEF DELPHIXE3_UP}TRecordBuffer{$ELSE}Pointer{$ENDIF}; Append: Boolean);
begin
  DoPostRecord(Current, Append);
end;

procedure TCustomVirtualDataSet.InternalClose;
begin
  fInternalOpen := False;
  BindFields(False);
  FieldDefs.Updated := False;
  if Pointer(fOldValueBuffer) <> nil then
  begin
    try
      Finalize(PVariantList(fOldValueBuffer + SizeOf(TArrayRecInfo))^, Fields.Count);
      FreeMem(Pointer(fOldValueBuffer));
    finally
      Pointer(fOldValueBuffer) := nil;
    end;
  end;
end;

procedure TCustomVirtualDataSet.InternalCreateFields;
begin
  if {$IF CompilerVersion >= 27}(FieldOptions.AutoCreateMode <> acExclusive)
    or not (lcPersistent in Fields.LifeCycles){$ELSE}DefaultFields{$IFEND} then
    raise EVirtualDataSetException.CreateRes(@SPersistentFieldsRequired);
end;

procedure TCustomVirtualDataSet.InternalDelete;
var
  recBuf: TRecordBuffer;
begin
  if GetActiveRecBuf(recBuf) then
    DoDeleteRecord(PArrayRecInfo(recBuf).Index);
end;

procedure TCustomVirtualDataSet.InternalEdit;
begin
  fModifiedFields.Clear;

  if Pointer(fOldValueBuffer) = nil then
    fOldValueBuffer := AllocRecordBuffer
  else
    Finalize(PVariantList(fOldValueBuffer + SizeOf(TArrayRecInfo))^, Fields.Count);
end;

procedure TCustomVirtualDataSet.InternalFirst;
begin
  fCurrent := -1;
end;

function TCustomVirtualDataSet.InternalGetRecord(Buffer: TRecordBuffer;
  GetMode: TGetMode; DoCheck: Boolean): TGetResult;
var
  recCount: Integer;
begin
  try
    recCount := RecordCount;
    Result := grOK;
    case GetMode of
      gmNext:
      begin
        if fCurrent < recCount then
          Inc(fCurrent);
        if fCurrent >= recCount then
          Result := grEOF;
      end;
      gmPrior:
      begin
        if fCurrent <= 0 then
          fCurrent := -1
        else
          fCurrent := Min(fCurrent - 1, recCount - 1);

        if fCurrent < 0 then
          Result := grBOF;
      end;
      gmCurrent:
      begin
        if fCurrent < 0 then
          Result := grBOF
        else if fCurrent >= recCount then
        begin
          Result := grEOF;
          fCurrent := recCount;
        end;
      end;
    end;

    if Result = grOK then
    begin
      PArrayRecInfo(Buffer).Index := fCurrent;
      PArrayRecInfo(Buffer).BookmarkFlag := bfCurrent;

      Finalize(PVariantList(Buffer + SizeOf(TArrayRecInfo))^, Fields.Count);
      GetCalcFields({$IFDEF DELPHIXE4_UP}NativeInt(Buffer){$ELSE}Buffer{$ENDIF});
    end;
  except
    if DoCheck then
      raise;
    Result := grError;
  end;
end;

procedure TCustomVirtualDataSet.InternalGotoBookmark(
  Bookmark: {$IFDEF DELPHIXE3_UP}TBookmark{$ELSE}Pointer{$ENDIF});
begin
  fCurrent := fIndexList.IndexOf(PObject(Bookmark)^);
end;

procedure TCustomVirtualDataSet.InternalHandleException;
begin
  if Assigned(Classes.ApplicationHandleException) then
    Classes.ApplicationHandleException(Self);
end;

procedure TCustomVirtualDataSet.InternalInitFieldDefs;

  procedure InitFieldDefsFromFields(const fields: TFields;
    const fieldDefs: TFieldDefs);
  var
    i: Integer;
    field: TField;
    fieldDef: TFieldDef;
  begin
    for i := 0 to fields.Count - 1 do
    begin
      field := fields[i];
      if fieldDefs.IndexOf(field.FieldName) = -1 then
      begin
        fieldDef := fieldDefs.AddFieldDef;
        fieldDef.Name := field.FieldName;
        fieldDef.DataType := field.DataType;
        fieldDef.Size := field.Size;
        if field.Required then
          fieldDef.Attributes := [faRequired];
        if field.ReadOnly then
          fieldDef.Attributes := fieldDef.Attributes + [DB.faReadonly];
        if (field.DataType = ftBCD) and (field is TBCDField) then
          fieldDef.Precision := TBCDField(field).Precision;
        if field is TObjectField then
          InitFieldDefsFromFields(TObjectField(field).Fields, fieldDef.ChildDefs);
      end;
    end;
  end;

begin
  FieldDefs.Clear;
  InitFieldDefsFromFields(Fields, FieldDefs);
end;

procedure TCustomVirtualDataSet.InternalInitRecord(Buffer: TRecordBuffer);
var
  i: Integer;
begin
  for i := 0 to Fields.Count - 1 do
    PVariantList(Buffer + SizeOf(TArrayRecInfo))[i] := Null;
end;

procedure TCustomVirtualDataSet.InternalLast;
begin
  fCurrent := RecordCount;
end;

procedure TCustomVirtualDataSet.InternalOpen;
begin
  fInternalOpen := True;
  fCurrent := -1;

  BookmarkSize := SizeOf(Pointer);

  FieldDefs.Updated := False;
  FieldDefs.Update;

  InternalCreateFields;
  Reserved := Pointer(FieldListCheckSum(Self));
  BindFields(True);
  SetRecBufSize;
end;

procedure TCustomVirtualDataSet.InternalPost;
var
  recBuf: TRecordBuffer;
begin
  inherited InternalPost;
  UpdateCursorPos;
  GetActiveRecBuf(recBuf);

  if PArrayRecInfo(recBuf).BookmarkFlag =  bfEOF then
    DoPostRecord(-1, True)
  else
    DoPostRecord(PArrayRecInfo(recBuf).Index, False);
end;

procedure TCustomVirtualDataSet.InternalRefresh;
begin
  fIndexList.Rebuild;
end;

procedure TCustomVirtualDataSet.InternalSetToRecord(Buffer: TRecordBuffer);
begin
  if PArrayRecInfo(Buffer).BookmarkFlag in [bfCurrent, bfInserted] then
    fCurrent := PArrayRecInfo(Buffer).Index;
end;

function TCustomVirtualDataSet.IsCursorOpen: Boolean;
begin
  Result := fInternalOpen;
end;

function TCustomVirtualDataSet.IsFiltered: Boolean;
begin
  Result := Filtered and ((Trim(Filter) <> '') or Assigned(OnFilterRecord));
end;

function TCustomVirtualDataSet.Locate(const KeyFields: string; const KeyValues: Variant;
  Options: TLocateOptions): Boolean;
begin
  DoBeforeScroll;
  Result := DataSetLocateThrough(Self, KeyFields, KeyValues, Options);
  if Result then
  begin
    DataEvent(deDataSetChange, 0);
    DoAfterScroll;
  end;
end;

function TCustomVirtualDataSet.Lookup(const KeyFields: string; const KeyValues: Variant;
  const ResultFields: string): Variant;
begin
  Result := Null;
  if DataSetLocateThrough(Self, KeyFields, KeyValues, []) then
    Result := FieldValues[ResultFields];
end;

procedure TCustomVirtualDataSet.RebuildFieldCache;
var
  i: Integer;
begin
  fFieldsCache.Clear;
  for i := 0 to Fields.Count - 1 do
  begin
    Fields[i].DisplayLabel := FieldDefs[i].DisplayName;
    fFieldsCache.Add(Fields[i].FieldName, Fields[i]);
  end;
end;

procedure TCustomVirtualDataSet.SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag);
begin
  PArrayRecInfo(Buffer).BookmarkFlag := Value;
end;

procedure TCustomVirtualDataSet.SetCurrent(value: Integer);
begin
  fCurrent := value;
end;

procedure TCustomVirtualDataSet.SetFieldData(Field: TField; Buffer: TValueBuffer);
begin
  SetFieldData(Field, Buffer, True);
end;

procedure TCustomVirtualDataSet.SetFieldData(Field: TField; Buffer: TValueBuffer; NativeFormat: Boolean);

  procedure BcdToOleVariant(const Bcd: TBcd; var Data: OleVariant);
  var
    Temp: OleVariant;
  begin
    VarClear(Data);
    Temp := BcdToStr(Bcd);
    VarResultCheck(VariantChangeTypeEx(TVarData(Data), TVarData(Temp), VAR_LOCALE_USER_DEFAULT,
       0, VT_DECIMAL));
  end;

  {$IFDEF DELPHIXE3_UP}
  procedure BufferToVar(var Data: OleVariant);
  var
    LUnknown: IUnknown;
    LDispatch: IDispatch;
    TempBuff: TValueBuffer;
  begin
    case Field.DataType of
{$IFNDEF NEXTGEN}
      ftString, ftFixedChar, ftGuid:
        Data := AnsiString(PAnsiChar(Buffer));
{$ENDIF}
      ftWideString, ftFixedWideChar:
        Data := WideString(PWideChar(Buffer));
      ftAutoInc, ftInteger:
        Data := TDBBitConverter.UnsafeInto<Integer>(Buffer);
      ftLongWord:
        Data := TDBBitConverter.UnsafeInto<Cardinal>(Buffer);
      ftShortint:
        Data := TDBBitConverter.UnsafeInto<ShortInt>(Buffer);
      ftByte:
        Data := TDBBitConverter.UnsafeInto<Byte>(Buffer);
      ftSmallInt:
        Data := TDBBitConverter.UnsafeInto<SmallInt>(Buffer);
      ftWord:
        Data := TDBBitConverter.UnsafeInto<Word>(Buffer);
      ftBoolean:
        Data := TDBBitConverter.UnsafeInto<WordBool>(Buffer);
      ftFloat, ftCurrency:
        Data := TDBBitConverter.UnsafeInto<Double>(Buffer);
      ftBlob, ftMemo, ftGraphic, ftVariant, ftWideMemo:
        Data := TDBBitConverter.UnsafeIntoVariant(Buffer);
      ftInterface:
      begin
        Move(Buffer[0], LUnknown, SizeOf(IUnknown));
        Data := LUnknown;
      end;
      ftIDispatch:
      begin
        Move(Buffer[0], LDispatch, SizeOf(IDispatch));
        Data := LDispatch;
      end;
      ftDate, ftTime, ftDateTime:
        if NativeFormat then
        begin
          SetLength(TempBuff, SizeOf(TVarData(Data).VDate));
          DataConvert(Field, Buffer, TempBuff, False);
          TVarData(Data).VDate := TDBBitConverter.UnsafeInto<Double>(TempBuff);
        end
        else
          Data := TDBBitConverter.UnsafeInto<Double>(Buffer);
      ftFMTBCD:
        BcdToOleVariant(TDBBitConverter.UnsafeInto<TBcd>(Buffer), Data);
      ftBCD:
        if NativeFormat then
        begin
          SetLength(TempBuff, SizeOf(TVarData(Data).VCurrency));
          DataConvert(Field, Buffer, TempBuff, False);
          TVarData(Data).VCurrency := TDBBitConverter.UnsafeInto<Currency>(TempBuff);
        end
        else
          Data := TDBBitConverter.UnsafeInto<Currency>(Buffer);
      ftBytes, ftVarBytes:
        if NativeFormat then
        begin
          TempBuff := BytesOf(@Data, SizeOf(Variant));
          DataConvert(Field, Buffer, TempBuff, False);
          Data := TDBBitConverter.UnsafeIntoVariant(TempBuff);
        end
        else
          Data := TDBBitConverter.UnsafeIntoVariant(Buffer);
      ftLargeInt:
        Data := TDBBitConverter.UnsafeInto<Int64>(Buffer);
    else
      DatabaseErrorFmt(SUnsupportedFieldType, [FieldTypeNames[Field.DataType],
        Field.DisplayName]);
    end;
  end;
  {$ELSE}
  procedure BufferToVar(var Data: OleVariant);
  begin
    case Field.DataType of
      ftString, ftFixedChar, ftGuid:
        Data := AnsiString(PAnsiChar(Buffer));
      ftWideString, ftFixedWideChar:
        Data := WideString(PWideChar(Buffer));
      ftAutoInc, ftInteger:
        Data := Integer(Buffer^);
      ftLongWord:
        Data := Cardinal(Buffer^);
      ftShortint:
        Data := ShortInt(Buffer^);
      ftByte:
        Data := Byte(Buffer^);
      ftSmallInt:
        Data := SmallInt(Buffer^);
      ftWord:
        Data := Word(Buffer^);
      ftBoolean:
        Data := WordBool(Buffer^);
      ftFloat, ftCurrency:
        Data := Double(Buffer^);
      ftBlob, ftMemo, ftGraphic, ftVariant, ftWideMemo:
        Data := Variant(Buffer^);
      ftInterface:
        Data := IUnknown(Buffer^);
      ftIDispatch:
        Data := IDispatch(Buffer^);
      ftDate, ftTime, ftDateTime:
        if NativeFormat then
          DataConvert(Field, Buffer, @TVarData(Data).VDate, False) else
          Data := TDateTime(Buffer^);
      ftFMTBCD:
        BcdToOleVariant(TBcd(Buffer^), Data);
      ftBCD:
        if NativeFormat then
          DataConvert(Field, Buffer, @TVarData(Data).VCurrency, False) else
          Data := Currency(Buffer^);
      ftBytes, ftVarBytes:
        if NativeFormat then
          DataConvert(Field, Buffer, @Data, False) else
          Data := OleVariant(Buffer^);
      ftLargeInt:
        Data := LargeInt(Buffer^);
    else
      DatabaseErrorFmt(SUnsupportedFieldType, [FieldTypeNames[Field.DataType],
        Field.DisplayName]);
    end;
  end;
  {$ENDIF}

var
  data: OleVariant;
  recBuf: TRecordBuffer;
begin
  if not (State in dsWriteModes) then
    DatabaseError(SNotEditing, Self);

  GetActiveRecBuf(recBuf);

  if Buffer = nil then
    data := Null
  else
    BufferToVar(data);

  if Field.FieldNo > 0 then
  begin
    if ReadOnly and not(State in [dsSetKey, dsFilter]) then
      DatabaseErrorFmt(SFieldReadOnly, [Field.DisplayName]);

    Field.Validate(Buffer);

    if not fModifiedFields.Contains(Field) then
    begin
      PVariantList(fOldValueBuffer + SizeOf(TArrayRecInfo))[Field.Index] := Field.OldValue;
      fModifiedFields.Add(Field);
    end;
  end;

  PVariantList(recBuf + SizeOf(TArrayRecInfo))[Field.Index] := data;

  if not (State in [dsCalcFields, dsInternalCalc, dsFilter, dsNewValue]) then
    DataEvent(deFieldChange, NativeInt(Field));
end;

procedure TCustomVirtualDataSet.SetFieldData(const field: TField;
  const buffer: Variant);
var
  recBuf: TRecordBuffer;
begin
  if not (State in dsWriteModes) then
    DatabaseError(SNotEditing, Self);

  GetActiveRecBuf(recBuf);

  if field.FieldNo > 0 then
  begin
    if ReadOnly and not(State in [dsSetKey, dsFilter]) then
      DatabaseErrorFmt(SFieldReadOnly, [field.DisplayName]);

    if not fModifiedFields.Contains(field) then
    begin
      PVariantList(fOldValueBuffer + SizeOf(TArrayRecInfo))[field.Index] := field.OldValue;
      fModifiedFields.Add(field);
    end;
  end;

  PVariantList(recBuf + SizeOf(TArrayRecInfo))[field.Index] := buffer;

  if not (State in [dsCalcFields, dsInternalCalc, dsFilter, dsNewValue]) then
    DataEvent(deFieldChange, NativeInt(field));
end;

procedure TCustomVirtualDataSet.SetFiltered(Value: Boolean);
begin
  if Filtered <> Value then
    if Active then
    begin
      CheckBrowseMode;
      inherited SetFiltered(Value);
      if Value then
      begin
        UpdateFilter;
        First;
      end
      else
      begin
        UpdateFilter;
        Resync([]);
        First;
      end;
    end
    else
      inherited SetFiltered(Value);
end;

procedure TCustomVirtualDataSet.SetIndex(const value: Integer);
begin
  Guard.CheckIndex(RecordCount, Value);

  fCurrent := value;
end;

procedure TCustomVirtualDataSet.SetRecBufSize;
begin
  fRowBufSize := SizeOf(TArrayRecInfo) + (Fields.Count * SizeOf(Variant));
end;

procedure TCustomVirtualDataSet.SetRecNo(Value: Integer);
begin
  CheckBrowseMode;
  Value := Min(max(Value, 1), RecordCount);

  if RecNo <> Value then
  begin
    DoBeforeScroll;
    fCurrent := Value - 1;
    Resync([rmCenter]);
    DoAfterScroll;
  end;
end;

{$ENDREGION}


{$REGION 'TBlobStream'}

constructor TCustomVirtualDataSet.TBlobStream.Create(Field: TBlobField;
  Mode: TBlobStreamMode);
begin
  inherited Create;
  fField := Field;
  fFieldNo := fField.FieldNo - 1;
  fDataSet := fField.Dataset as TCustomVirtualDataSet;
  fFieldData := Null;
  fData := Null;
  if Mode <> bmRead then
  begin
    if fField.ReadOnly then
      DatabaseErrorFmt(SFieldReadOnly, [fField.DisplayName], fDataSet);
    if not (fDataSet.State in [dsEdit, dsInsert]) then
      DatabaseError(SNotEditing, fDataSet);
  end;
  if Mode = bmWrite then
    Truncate
  else
    ReadBlobData;
end;

destructor TCustomVirtualDataSet.TBlobStream.Destroy;
begin
  if fModified then
    fDataSet.SetFieldData(fField, fData);
  inherited Destroy;
end;

procedure TCustomVirtualDataSet.TBlobStream.ReadBlobData;
var
  buffer: TBytes;
  recBuf: TRecordBuffer;
begin
  fFieldData := Null;
  // Pass empty buffer here to not copy any data
  // but ensure the record holds valid data.
  if fDataSet.GetFieldData(fField, buffer, True) then
    if fDataSet.GetActiveRecBuf(recBuf) then
      fFieldData := PVariantList(recBuf + SizeOf(TArrayRecInfo))[fField.Index];
  if not VarIsNull(fFieldData) then
  begin
    if VarType(fFieldData) = varOleStr then
    begin
      if fField.BlobType = ftWideMemo then
        Size := Length(WideString(fFieldData)) * SizeOf(WideChar)
      else
      begin
        { Convert OleStr into a pascal string (format used by TBlobField) }
{$IFNDEF NEXTGEN}
        fFieldData := AnsiString(fFieldData);
{$ELSE}
        fFieldData := VarToStr(fFieldData);
{$ENDIF}
        Size := Length(fFieldData);
      end;
    end
    else
      Size := VarArrayLength(fFieldData, 1);
    fFieldData := Null;
  end;
end;

function TCustomVirtualDataSet.TBlobStream.Realloc(
  var NewCapacity: LongInt): Pointer;

  procedure VarAlloc(var V: Variant; Field: TFieldType);
  var
    W: WideString;
{$IFNDEF NEXTGEN}
    S: AnsiString;
{$ELSE}
    S: string;
{$ENDIF}
  begin
    if Field = ftMemo then
    begin
      if not VarIsNull(V) then
{$IFNDEF NEXTGEN}
        S := AnsiString(V);
{$ELSE}
        S := VarToStr(V);
{$ENDIF}
      SetLength(S, NewCapacity);
      V := S;
    end
    else if Field = ftWideMemo then
    begin
      if not VarIsNull(V) then
        W := WideString(V);
      SetLength(W, NewCapacity div 2);
      V := W;
    end
    else
    begin
      if VarIsClear(V) or VarIsNull(V) then
        V := VarArrayCreate([0, NewCapacity - 1], varByte)
      else
        VarArrayRedim(V, NewCapacity - 1);
    end;
  end;

begin
  Result := Memory;
  if NewCapacity <> Capacity then
  begin
    if VarIsArray(fData) then
      VarArrayUnlock(fData);
    if NewCapacity = 0 then
    begin
      fData := Null;
      Result := nil;
    end
    else
    begin
      if VarIsNull(fFieldData) then
        VarAlloc(fData, fField.DataType)
      else
        fData := fFieldData;
      if VarIsArray(fData) then
        Result := VarArrayLock(fData)
      else
        Result := TVarData(fData).VString;
    end;
  end;
end;

procedure TCustomVirtualDataSet.TBlobStream.Truncate;
begin
  Clear;
  fModified := True;
end;

function TCustomVirtualDataSet.TBlobStream.Write(const Buffer;
  Count: LongInt): LongInt;
begin
  Result := inherited Write(Buffer, Count);
  fModified := True;
end;

{$ENDREGION}


end.
