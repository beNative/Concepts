{
  Copyright (C) 2013-2015 Tim Sinaeve tim.sinaeve@gmail.com

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

{
  References;
    - Datasets without databases by Marco Cantú (http://edn.embarcadero.com/article/20587)
    - Gerald Nunn's TGXBaseDataset
    - http://stackoverflow.com/questions/9533760/how-can-i-get-a-dataset-of-in-memory-objects
    - ported code to enable 64bit support
}

unit DDuce.Components.VirtualDataSet;

{$I ..\DDuce.inc}

interface

uses
  System.Classes, System.SysUtils, System.Variants, System.Rtti,
  System.Generics.Collections,
  Vcl.Forms,
  Data.DB, Data.DbConsts,

  DDuce.Logger;

{$REGION 'Documentation'}
{
Notes: A record Buffer is in the following format
           - physical record Buffer
               (offset 0, length RecordSize)

//           - calculated fields Buffer
//               (offset dsCalcFldOfs, length CalcFieldSize)
//           - bookmark data
//               (offset dsBookmarkOfs, length BookmarkSize)
           - TRecordInfo data
               (offset dsRecordInfoOfs, length sizeof(TDataSetRecordInfo))
        A key Buffer is in the following format
           - physical record Buffer
               (offset 0, length RecordSize)
           - TKeyRecordInfo data
               (offset btKeyInfoOfs, length sizeof(TKeyRecordInfo))
        TDataSet maintains an array of record Buffers.
        TffTable maintains an array of key Buffers, one for each of
          the TffKeyEditType enum values

1.0

2.0 support for nested tables

2.1 calculated field support

}

{
  Internal Data layout:
+---------------+-----------------------+------------------+----------------------+
|PHYSICAL DATA  | CALCULATED FIELDS     |Bookmark arrays   | VARIABLE LENGTH PTRS |
|FRecordSize    | FCalcRecordSize bytes |FBookmarkArraySize| FVarLengthRecordSize |
+---------------+-----------------------+------------------+----------------------+
^               ^                       ^                  ^
0                       StartBookmarks     StartVarLength

Blobsfields in the internal buffer are pointers to the blob data.
}

{

+-----------------------------------------------------------
|                  DATASET RECORD BUFFER
|                    FRecordBufferSize
+---------------+--------------------+----------------------
|PHYSICAL DATA  | Record Info        |  Calculated fields
| FRecordSize   | TRecordInfo bytes  |
+---------------+--------------------+----------------------
^               ^                    ^
0               FRecordInfoOffset    FCalcFieldsOffset

a DataSet maintains a list (TList) of recordbuffers, holding the physical data

}
{
sources:
Marco Cantu : http://www.marcocantu.com/code/md6htm/MdDataPack.htm

}
{$ENDREGION}

type
  TCustomVirtualDataset = class;
  TVirtualDataset       = class;

{$IF CompilerVersion <= 21} // remove?
  TValueBuffer  = Pointer;
  NativeInt     = LongInt;
{$IFEND}
  EVirtualDatasetError = class(Exception);
  PVariantList         = ^TVariantList;
  TVariantList         = array [0 .. 0] of OleVariant;

  TDeleteRecordEvent = procedure(
    Sender : TCustomVirtualDataset;
    Index  : Integer
  ) of object;

  TGetRecordCountEvent = procedure(
        Sender : TCustomVirtualDataset;
    var Count  : Integer
  ) of object;

  TGetFieldValueEvent = procedure(
        Sender : TCustomVirtualDataset;
        Field  : TField;
        Index  : Integer;
    var Value  : TValue
  ) of object;

  TPostDataEvent = procedure(
    Sender : TCustomVirtualDataset;
    Index  : Integer
  ) of object;

  TLocateEvent = procedure(
          Sender    : TCustomVirtualDataset;
    const KeyFields : string;
    const KeyValues : Variant;
          Options   : TLocateOptions;
      var Index     : Integer
  ) of object;

  TLookupValueEvent = procedure(
          Sender       : TCustomVirtualDataset;
    const KeyFields    : string;
    const KeyValues    : Variant;
    const ResultFields : string;
      var Value: Variant
  ) of object;

  PRecordInfo = ^TRecordInfo;
  TRecordInfo = record
    Bookmark     : Integer;
    BookmarkFlag : TBookmarkFlag;
  end;

  { TBlobStream }

  TBlobStream = class(TMemoryStream)
  private
    FField     : TBlobField;
    FDataSet   : TCustomVirtualDataset;
    FBuffer    : TRecordBuffer;
    FFieldNo   : Integer;
    FModified  : Boolean;
    FData      : Variant;
    FFieldData : Variant;

  protected
    procedure ReadBlobData;
    function Realloc(var NewCapacity: Longint): Pointer; override;

  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
    destructor Destroy; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure Truncate;
  end;

  TVirtualMasterDataLink = class(TMasterDataLink)
  protected
    procedure ActiveChanged; override;
  end;

  TCustomVirtualDataset = class(TDataSet)
  strict private
    FInternalOpen     : Boolean;
    FCurrentRecord    : Integer;      // current record (0 to FRecordCount - 1)
    FFilterBuffer     : TRecordBuffer;
    FOldValueBuffer   : TRecordBuffer;
    FReadOnly         : Boolean;
    FRecordBufferSize : Integer;      // data + housekeeping (TRecordInfo)

    FMasterDataLink : TVirtualMasterDataLink;
    FModifiedFields : TList<TField>;
    {$IFDEF DELPHIXE3_UP}
    FReserved: Pointer;
    {$ENDIF}

    FOnDeleteRecord   : TDeleteRecordEvent;
    FOnGetFieldValue  : TGetFieldValueEvent;
    FOnGetRecordCount : TGetRecordCountEvent;
    FOnPostData       : TPostDataEvent;
    FOnLocate         : TLocateEvent;
    FOnLookupValue    : TLookupValueEvent;

    procedure DateTimeToNative(
      ADataType : TFieldType;
      AData     : TDateTime;
      ABuffer   : Pointer
    );

    function GetMasterSource: TDataSource;
    function GetTopIndex: Integer;
    function GetTopRecNo: Integer;
    procedure SetTopIndex(Value: Integer);
    procedure SetMasterSource(Value: TDataSource);

  protected
    // event dispatch methods
    procedure DoDeleteRecord(AIndex: Integer); virtual;
    procedure DoGetFieldValue(
          AField : TField;
          AIndex : Integer;
      var AValue : TValue
    ); virtual;
    procedure DoPostData(AIndex: Integer); virtual;

    function InternalGetRecord(
      ABuffer  : TRecordBuffer;
      AGetMode : TGetMode;
      ADoCheck : Boolean
    ): TGetResult; virtual;

    procedure MasterChanged(Sender: TObject); virtual;
    procedure MasterDisabled(Sender: TObject); virtual;

    function GetActiveRecBuf(var ARecBuf: TRecordBuffer): Boolean;

    procedure InternalSetFieldData(AField: TField; ABuffer: Pointer;
      ANativeFormat: Boolean); virtual;

    procedure LoadFieldDefsFromFields(
      AFields    : TFields;
      AFieldDefs : TFieldDefs
    ); virtual;

    procedure VariantToBuffer(
          AField        : TField;
          AVariant      : Variant;
      out ABuffer       : Pointer;
          ANativeFormat : Boolean = True
    );
    procedure BufferToVariant(
          AField        : TField;
          ABuffer       : Pointer;
      out AVariant      : Variant;
          ANativeFormat : Boolean = True
    );

    // Standard overrides
    function GetCanModify: Boolean; override;
    function GetRecNo: Longint; override;
    function GetRecordCount: Integer; override;
    procedure DataEvent(Event: TDataEvent; Info: NativeInt); override;
    procedure DoOnNewRecord; override;
    procedure InternalEdit; override;
    procedure SetRecNo(Value: Integer); override;

    // Abstract overrides
    function AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    {$IFDEF DELPHIXE5_UP}
    procedure GetBookmarkData(Buffer: TRecBuf; Data: TBookmark); overload; override;
    {$ENDIF}
    {$IFNDEF NEXTGEN}
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); overload; override;
    {$ENDIF}
    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override;

    // abstract methods required for all datasets
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
      DoCheck: Boolean): TGetResult; override;
    procedure InternalClose; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    function IsCursorOpen: Boolean; override;

    function GetRecordSize: Word; override;
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    procedure InternalCreateFields; virtual;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
{$IF CompilerVersion > 21}
    procedure InternalGotoBookmark(Bookmark: TBookmark); override;
{$ELSE}
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
{$IFEND}
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;
    procedure InternalLast; override;
    procedure InternalPost; override;
    procedure InternalSetToRecord(Buffer: TRecordBuffer); override;
    procedure SetBookmarkFlag(Buffer: TRecordBuffer;
      Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;

    procedure SetFieldData(
      Field        : TField;
      Buffer       : TValueBuffer;
      NativeFormat : Boolean
    ); overload; override;

    property ModifiedFields: TList<TField>
      read FModifiedFields;

    property RecordBufferSize: Integer
      read FRecordBufferSize write FRecordBufferSize;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    { Standard public overrides }
    function BookmarkValid(Bookmark: TBookmark): Boolean; override;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark)
      : Integer; override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode)
      : TStream; override;
    function GetBlobFieldData(FieldNo: Integer; var Buffer: TBlobByteData)
      : Integer; override;
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
    function Lookup(const KeyFields: string; const KeyValues: Variant;
      const ResultFields: string): Variant; override;

    function GetFieldData(
      Field: TField;
      var Buffer: TValueBuffer
    ): Boolean; override;

    property MasterDataLink: TVirtualMasterDataLink
      read FMasterDataLink;

    property MasterSource: TDataSource
      read GetMasterSource write SetMasterSource;

    property ReadOnly: Boolean
      read FReadOnly write FReadOnly default False;

    property TopIndex: Integer
      read GetTopIndex write SetTopIndex;

    property TopRecNo: Integer
      read GetTopRecNo;

    property OnDeleteRecord : TDeleteRecordEvent
      read FOnDeleteRecord write FOnDeleteRecord;

    property OnGetFieldValue: TGetFieldValueEvent
      read FOnGetFieldValue write FOnGetFieldValue;

    property OnGetRecordCount : TGetRecordCountEvent
      read FOnGetRecordCount write FOnGetRecordCount;

    property OnLocate: TLocateEvent
      read FOnLocate write FOnLocate;

    property OnLookupValue : TLookupValueEvent
      read FOnLookupValue write FOnLookupValue;

    property OnPostData: TPostDataEvent
      read FOnPostData write FOnPostData;
  end;

  TVirtualDataset = class(TCustomVirtualDataset)
  published
    property Active;
    property Filtered;
    property ReadOnly;
    property MasterSource;

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
    property OnDeleteRecord;
    property OnEditError;
    property OnFilterRecord;
    property OnGetFieldValue;
    property OnGetRecordCount;
    property OnNewRecord;
    property OnLookupValue;
    property OnLocate;
    property OnPostData;
    property OnPostError;
  end;

procedure VirtualDatasetError(
  const AMessage : string;
        ADataset : TCustomVirtualDataset = nil
);

procedure VirtualDatasetErrorFmt(
  const AMessage : string;
  const AArgs    : array of const;
        ADataset : TCustomVirtualDataset = nil
);

implementation

uses
  System.Math, System.WideStrUtils,
  WinApi.ActiveX, WinApi.Windows,
  Data.FmtBcd;

resourcestring
  SUnsupportedFieldType = 'Unsupported field type (%s) in field %s.';
  SPersistentFieldsRequired =
    'Virtual dataset can only be used with persistent fields.';
  SIndexOutOfRange = 'Index out of range';

{$REGION 'interfaced routines'}
procedure VirtualDatasetError(const AMessage: string;
  ADataset: TCustomVirtualDataset = nil);
begin
  if Assigned(ADataset) then
    raise EVirtualDatasetError.Create(Format('%s: %s', [ADataset.Name, AMessage]))
  else
    raise EVirtualDatasetError.Create(AMessage);
end;

procedure VirtualDatasetErrorFmt(const AMessage: string;
  const AArgs: array of const; ADataset: TCustomVirtualDataset = nil);
begin
  VirtualDatasetError(Format(AMessage, AArgs), ADataset);
end;
{$ENDREGION}

{$REGION 'non-interfaced routines'}
function FieldListCheckSum(Dataset: TDataSet): Integer;
var
  I: Integer;
begin
  Result   := 0;
  for I    := 0 to Dataset.Fields.Count - 1 do
    Result := Result + (Integer(Dataset.Fields[I]) shr (I mod 16));
end;
{$ENDREGION}

{$REGION 'TBlobStream'}
constructor TBlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode);
begin
  FField     := Field;
  FFieldNo   := FField.FieldNo - 1;
  FDataSet   := FField.Dataset as TCustomVirtualDataset;
  FFieldData := Null;
  FData      := Null;
  if not FDataSet.GetActiveRecBuf(FBuffer) then
    Exit;
  if Mode <> bmRead then
  begin
    if FField.ReadOnly then
      DatabaseErrorFmt(SFieldReadOnly, [FField.DisplayName], FDataSet);
    if not(FDataSet.State in [dsEdit, dsInsert]) then
      DatabaseError(SNotEditing, FDataSet);
  end;
  if Mode = bmWrite then
    Truncate
  else
    ReadBlobData;
end;

destructor TBlobStream.Destroy;
begin
  if FModified then
    try
      FDataSet.SetFieldData(FField, @FData);
      FField.Modified := True;
      FDataSet.DataEvent(deFieldChange, Longint(FField));
    except
      ApplicationHandleException(Self);
    end;
  inherited Destroy;
end;

procedure TBlobStream.ReadBlobData;
begin
  FDataSet.GetFieldData(FField, @FFieldData, True);
  if not VarIsNull(FFieldData) then
  begin
    if VarType(FFieldData) = varOleStr then
    begin
      if FField.BlobType = ftWideMemo then
        Size := Length(WideString(FFieldData)) * SizeOf(widechar)
      else
      begin
        { Convert OleStr into a pascal string (format used by TBlobField) }
        FFieldData := AnsiString(FFieldData);
        Size       := Length(FFieldData);
      end;
    end
    else
      Size     := VarArrayHighBound(FFieldData, 1) + 1;
    FFieldData := Null;
  end;
end;

function TBlobStream.Realloc(var NewCapacity: Longint): Pointer;

  procedure VarAlloc(var V: Variant; Field: TFieldType);
  var
    W: WideString;
    S: AnsiString;
  begin
    if Field = ftMemo then
    begin
      if not VarIsNull(V) then
        S := AnsiString(V);
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
    if VarIsArray(FData) then
      VarArrayUnlock(FData);
    if NewCapacity = 0 then
    begin
      FData  := Null;
      Result := nil;
    end
    else
    begin
      if VarIsNull(FFieldData) then
        VarAlloc(FData, FField.DataType)
      else
        FData := FFieldData;
      if VarIsArray(FData) then
        Result := VarArrayLock(FData)
      else
        Result := TVarData(FData).VString;
    end;
  end;
end;

function TBlobStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result    := inherited write(Buffer, Count);
  FModified := True;
end;

procedure TBlobStream.Truncate;
begin
  Clear;
  FModified := True;
end;
{$ENDREGION}

{$REGION 'TVirtualMasterDataLink'}
procedure TVirtualMasterDataLink.ActiveChanged;
begin
  if Dataset = nil then
    Exit;

  // Fake a field.
  if Fields.Count = 0 then
    Fields.Add(TField.Create(Dataset));

  if Dataset.Active and not(csDestroying in Dataset.ComponentState) then
    if Active then
    begin
      if Assigned(OnMasterChange) then
        OnMasterChange(Self);
    end
    else if Assigned(OnMasterDisable) then
      OnMasterDisable(Self);
end;
{$ENDREGION}

{$REGION 'TCustomVirtualDataset'}
{$REGION 'construction and destruction'}
procedure TCustomVirtualDataset.AfterConstruction;
begin
  inherited;
  FInternalOpen                  := False;
  FReadOnly                      := False;
  FModifiedFields                := TList<TField>.Create;
  FMasterDataLink                := TVirtualMasterDataLink.Create(Self);
  MasterDataLink.OnMasterChange  := MasterChanged;
  MasterDataLink.OnMasterDisable := MasterDisabled;
end;

procedure TCustomVirtualDataset.BeforeDestruction;
begin
  FModifiedFields.Free;
  FMasterDataLink.Free;
  inherited;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TCustomVirtualDataset.GetRecordCount: Integer;
begin
  Result := -1;
  if Assigned(FOnGetRecordCount) then
    FOnGetRecordCount(Self, Result);
end;

function TCustomVirtualDataset.GetRecordSize: Word;
begin
  Result := SizeOf(TRecordInfo);
end;

function TCustomVirtualDataset.GetTopIndex: Integer;
begin
  if BufferCount = 0 then
    Result := -1
  else
    Result := PRecordInfo(Buffers[0])^.Bookmark;
end;

procedure TCustomVirtualDataset.SetTopIndex(Value: Integer);
begin
  ClearBuffers;
  FCurrentRecord := Value;

  if GetRecord(Buffers[0], gmCurrent, True) = grOK then
  // Only fetch next records when Eof and Bof are false
  begin
    ActivateBuffers;
    GetNextRecords;
  end;
  DataEvent(deDataSetChange, 0);
end;

function TCustomVirtualDataset.GetTopRecNo: Integer;
begin
  Result := TopIndex + 1;
end;

function TCustomVirtualDataset.GetRecNo: Longint;
var
  RecBuf: TRecordBuffer;
begin
  Logger.EnterMethod(Self, 'GetRecNo');
  CheckActive;
  Result := -1;
  if GetActiveRecBuf(RecBuf) and (PRecordInfo(RecBuf)^.BookmarkFlag = bfCurrent) then
    Result := PRecordInfo(RecBuf)^.Bookmark + 1;
  Logger.ExitMethod(Self, 'GetRecNo');
end;

procedure TCustomVirtualDataset.SetRecNo(Value: Integer);
begin
  CheckBrowseMode;
  Value := Min(Max(Value, 1), RecordCount);
  if RecNo <> Value then
  begin
    DoBeforeScroll;
    FCurrentRecord := Value - 1;
    Resync([rmCenter]);
    DoAfterScroll;
  end;
end;

procedure TCustomVirtualDataset.SetMasterSource(Value: TDataSource);
begin
  if IsLinkedTo(Value) then
    DatabaseError(SCircularDataLink, Self);
  MasterDataLink.DataSource := Value;
end;

function TCustomVirtualDataset.GetCanModify: Boolean;
begin
  Result := not FReadOnly;
end;
{$ENDREGION}

function TCustomVirtualDataset.AllocRecordBuffer: TRecordBuffer;
begin
  if not(csDestroying in ComponentState) then
  begin
    Result := AllocMem(FRecordBufferSize);
    Initialize(PVariantList(Result + SizeOf(TRecordInfo))^, Fields.Count);
  end
  else
    Result := nil;
end;

function TCustomVirtualDataset.BookmarkValid(Bookmark: TBookmark): Boolean;
begin
  if Assigned(Bookmark) and (PInteger(Bookmark)^ >= 0) and
    (PInteger(Bookmark)^ < RecordCount) then
    Result := True
  else
    Result := False;
end;

procedure TCustomVirtualDataset.BufferToVariant(AField: TField;
  ABuffer: Pointer; out AVariant: Variant; ANativeFormat: Boolean);
begin
  case AField.DataType of
    ftInterface:
      AVariant := IInterface(ABuffer^);
    ftIDispatch:
      AVariant := IDispatch(ABuffer^);
    ftVariant:
      AVariant := Variant(ABuffer^);
    ftString, ftFixedChar, ftGuid:
      AVariant := AnsiString(PAnsiChar(ABuffer));
    ftWideString, ftFixedWideChar:
      AVariant := WideString(PWideChar(ABuffer));
    ftAutoInc, ftInteger:
      AVariant := Integer(ABuffer^);
    ftSmallint:
      AVariant := SmallInt(ABuffer^);
    ftWord:
      AVariant := Word(ABuffer^);
    ftBoolean:
      AVariant := WordBool(ABuffer^);
    ftFloat, ftCurrency:
      AVariant := Double(ABuffer^);
    ftBlob, ftMemo, ftGraphic, ftWideMemo:
      AVariant := Variant(ABuffer^);
    ftDate, ftTime, ftDateTime:
      if ANativeFormat then
        DataConvert(AField, ABuffer, @TVarData(AVariant).VDate, False)
      else
        AVariant := TDateTime(ABuffer^);
    ftBCD:
      if ANativeFormat then
        DataConvert(AField, ABuffer, @TVarData(AVariant).VCurrency, False)
      else
        AVariant := Currency(ABuffer^);
    ftBytes, ftVarBytes:
      if ANativeFormat then
        DataConvert(AField, ABuffer, @AVariant, False)
      else
        AVariant := Variant(ABuffer^);
    {$IFNDEF CPUX64}
    ftLargeInt:
      begin
        TVarData(AVariant).VType := VT_DECIMAL;
        Decimal(AVariant).Lo64   := Int64(ABuffer^);
      end;
    {$ENDIF}
    ftLongWord:
      begin
        AVariant := LongWord(ABuffer^);
      end;
  else
    DatabaseErrorFmt(SUnsupportedFieldType, [FieldTypeNames[AField.DataType],
        AField.DisplayName]);
  end;
end;

function TCustomVirtualDataset.CompareBookmarks(Bookmark1, Bookmark2: TBookmark)
  : Integer;
const
  RetCodes: array [Boolean, Boolean] of ShortInt = ((2, -1), (1, 0));

begin
  Result := RetCodes[Bookmark1 = nil, Bookmark2 = nil];
  if Result = 2 then
  begin
    if PInteger(Bookmark1)^ < PInteger(Bookmark2)^ then
      Result := -1
    else if PInteger(Bookmark1)^ > PInteger(Bookmark2)^ then
      Result := 1
    else
      Result := 0;
  end;
end;

function TCustomVirtualDataset.CreateBlobStream(Field: TField;
  Mode: TBlobStreamMode): TStream;
begin
  Result := TBlobStream.Create(Field as TBlobField, Mode);
end;

procedure TCustomVirtualDataset.DataEvent(Event: TDataEvent; Info: NativeInt);
begin
  case Event of
    deLayoutChange:
      if Active and Assigned(FReserved) and
        (FieldListCheckSum(Self) <> Integer(FReserved)) then
        FReserved := nil;
  end;
  inherited;
end;

{$REGION 'event dispatch methods'}
procedure TCustomVirtualDataset.DoDeleteRecord(AIndex: Integer);
begin
  if Assigned(FOnDeleteRecord) then
    FOnDeleteRecord(Self, AIndex);
end;

procedure TCustomVirtualDataset.DoGetFieldValue(AField: TField; AIndex: Integer;
  var AValue: TValue);
begin
  if Assigned(FOnGetFieldValue) then
    FOnGetFieldValue(Self, AField, AIndex, AValue);
end;

procedure TCustomVirtualDataset.DoOnNewRecord;
begin
  FModifiedFields.Clear;
  if FOldValueBuffer = nil then
    FOldValueBuffer := AllocRecordBuffer
  else
    Finalize(PVariantList(FOldValueBuffer + SizeOf(TRecordInfo))^,
      Fields.Count);
  InitRecord(FOldValueBuffer);
  inherited DoOnNewRecord;
end;

procedure TCustomVirtualDataset.DoPostData(AIndex: Integer);
begin
  if Assigned(FOnPostData) then
    FOnPostData(Self, AIndex);
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TCustomVirtualDataset.DateTimeToNative(ADataType: TFieldType;
  AData: TDateTime; ABuffer: Pointer);
var
  TimeStamp: TTimeStamp;
begin
  TimeStamp := DateTimeToTimeStamp(AData);
  case ADataType of
    ftDate: Integer(ABuffer^) := TimeStamp.Date;
    ftTime: Integer(ABuffer^) := TimeStamp.Time;
  else
    TDateTime(ABuffer^) := TimeStampToMSecs(TimeStamp);
  end;
end;
{$ENDREGION}

procedure TCustomVirtualDataset.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  Finalize(PVariantList(Buffer + SizeOf(TRecordInfo))^, Fields.Count);
  FreeMem(Buffer);
end;

function TCustomVirtualDataset.GetActiveRecBuf(var ARecBuf: TRecordBuffer)
  : Boolean;
begin
  Logger.EnterMethod(Self, 'GetActiveRecBuf');
  ARecBuf := nil;
  case State of
    dsBlockRead, dsBrowse:
      if IsEmpty then
        ARecBuf := nil
      else
        ARecBuf := PByte(ActiveBuffer);

    dsEdit, dsInsert, dsNewValue:
      ARecBuf := PByte(ActiveBuffer);

    dsCalcFields, dsInternalCalc:
      ARecBuf := PByte(CalcBuffer);

    dsFilter:
      ARecBuf := FFilterBuffer;
  end;
  Result := ARecBuf <> nil;
  Logger.ExitMethod(Self, 'GetActiveRecBuf');
end;

function TCustomVirtualDataset.GetBlobFieldData(FieldNo: Integer;
  var Buffer: TBlobByteData): Integer;
begin
  Result := inherited GetBlobFieldData(FieldNo, Buffer);
end;

procedure TCustomVirtualDataset.GetBookmarkData(Buffer: TRecBuf; Data: TBookmark);
begin
  //PObject(Data)^ := IndexList.GetModel(PArrayRecInfo(Buffer)^.Index).AsObject;
end;

procedure TCustomVirtualDataset.GetBookmarkData(Buffer: TRecordBuffer;
  Data: Pointer);
begin
  Logger.EnterMethod(Self, 'GetBookmarkData');
  PInteger(Data)^ := PRecordInfo(Buffer)^.Bookmark;
  Logger.ExitMethod(Self, 'GetBookmarkData');
end;

function TCustomVirtualDataset.GetBookmarkFlag(Buffer: TRecordBuffer)
  : TBookmarkFlag;
begin
  Logger.EnterMethod(Self, 'GetBookmarkFlag');
  Result := PRecordInfo(Buffer)^.BookmarkFlag;
  Logger.ExitMethod(Self, 'GetBookmarkFlag');
end;

procedure TCustomVirtualDataset.VariantToBuffer(AField: TField; AVariant: Variant;
  out ABuffer: Pointer; ANativeFormat: Boolean);

  procedure CurrToBuffer(const C: Currency);
  begin
    if ANativeFormat then
      DataConvert(AField, @C, ABuffer, True)
    else
      Currency(ABuffer^) := C;
  end;

var
  Size: Integer;
begin
  Logger.EnterMethod(Self, 'VariantToBuffer');

  case AField.DataType of
    ftGuid, ftFixedChar, ftString:
      begin
        PAnsiChar(ABuffer)[AField.Size] := #0;
        if VarType(AVariant) = varString then
        begin
          Size                     := Min(Length(AVariant), AField.Size);
          PAnsiChar(ABuffer)[Size] := #0;
          Move(PChar(string(AVariant))^, PChar(ABuffer)^, Size);
        end
        else
        begin
          Size := Length(AVariant);
          if Size = 0 then
            PAnsiChar(ABuffer)[0] := #0
          else
            WideCharToMultiByte(0, 0, tagVariant(AVariant).bStrVal,
              Size + 1, ABuffer,
              AField.Size, nil, nil);
        end;
      end;
    ftFixedWideChar, ftWideString:
      begin
        if tagVariant(AVariant).bStrVal = nil then
          PWideChar(ABuffer)[0] := #0
        else
          WStrCopy(ABuffer, tagVariant(AVariant).bStrVal);
      end;
    ftSmallint:
      begin
        if tagVariant(AVariant).vt = VT_UI1 then
          SmallInt(ABuffer^) := Byte(tagVariant(AVariant).cVal)
        else
          SmallInt(ABuffer^) := tagVariant(AVariant).iVal;
      end;
    ftWord:
      begin
        if tagVariant(AVariant).vt = VT_UI1 then
          Word(ABuffer^) := tagVariant(AVariant).bVal
        else
          Word(ABuffer^) := tagVariant(AVariant).uiVal;
      end;
    ftAutoInc, ftInteger:
      begin
        Integer(ABuffer^) := AVariant;
      end;
    ftFloat, ftCurrency:
      begin
        if tagVariant(AVariant).vt = VT_R8 then
          Double(ABuffer^) := tagVariant(AVariant).dblVal
        else
          Double(ABuffer^) := AVariant;
      end;
    ftFMTBCD:
      TBcd(ABuffer^) := VarToBcd(AVariant);
    ftBCD:
      if tagVariant(AVariant).vt = VT_CY then
        CurrToBuffer(tagVariant(AVariant).cyVal)
      else
        CurrToBuffer(AVariant);
    ftBoolean:
      begin
        VarAsType(AVariant, VT_BOOL);
        WordBool(ABuffer^) := tagVariant(AVariant).vbool;
      end;
    ftDate, ftTime, ftDateTime:
      begin
        DateTimeToNative(AField.DataType, AVariant, ABuffer);
      end;
    ftBytes, ftVarBytes:
      if ANativeFormat then
        DataConvert(AField, @AVariant, ABuffer, True)
      else
        OleVariant(ABuffer^) := AVariant;
    ftInterface:
      IUnknown(ABuffer^) := AVariant;
    ftIDispatch:
      IDispatch(ABuffer^) := AVariant;
    ftLargeInt: ;
//  TS: not portable!
//      if Decimal(AVariant).sign > 0 then
//        LargeInt(ABuffer^) := -1 * Decimal(AVariant).Lo64
//      else
//        LargeInt(ABuffer^) := Decimal(AVariant).Lo64;
//    ftBlob .. ftTypedBinary, ftVariant, ftWideMemo:
//      OleVariant(ABuffer^) := AVariant;
  else
    DatabaseErrorFmt(SUnsupportedFieldType, [FieldTypeNames[AField.DataType],
        AField.DisplayName]);
  end;

  Logger.ExitMethod(Self, 'VariantToBuffer');
end;

function TCustomVirtualDataset.GetMasterSource: TDataSource;
begin
  if Assigned(MasterDataLink) then
    Result := MasterDataLink.DataSource
  else
    Result := nil;
end;

function TCustomVirtualDataset.GetRecord(Buffer: TRecordBuffer;
  GetMode: TGetMode; DoCheck: Boolean): TGetResult;
var
  Accept    : Boolean;
  SaveState : TDataSetState;
begin
  Logger.EnterMethod(Self, 'GetRecord');
  if Filtered and Assigned(OnFilterRecord) then
  begin
    FFilterBuffer := Buffer;
    SaveState     := SetTempState(dsFilter);
    try
      Accept := True;
      repeat
        Result := InternalGetRecord(Buffer, GetMode, DoCheck);
        if Result = grOK then
        begin
          OnFilterRecord(Self, Accept);
          if not Accept and (GetMode = gmCurrent) then
            Result := grError;
        end;
      until Accept or (Result <> grOK);
    except
      Application.HandleException(Self);
      Result := grError;
    end;
    RestoreState(SaveState);
  end
  else
    Result := InternalGetRecord(Buffer, GetMode, DoCheck);
  Logger.ExitMethod(Self, 'GetRecord');
end;

function TCustomVirtualDataset.InternalGetRecord(ABuffer: TRecordBuffer;
  AGetMode: TGetMode; ADoCheck: Boolean): TGetResult;
var
  iRecCount: Integer;
begin
  Logger.EnterMethod(Self, 'InternalGetRecord');
  try
    Result := grOK;
    case AGetMode of
      gmNext:
        begin
          iRecCount := RecordCount;
          if FCurrentRecord < iRecCount then
            Inc(FCurrentRecord);
          if FCurrentRecord >= iRecCount then
            Result := grEOF;
        end;
      gmPrior:
        begin
          if FCurrentRecord <= 0 then
            FCurrentRecord := -1
          else
          begin
            iRecCount := RecordCount;
            FCurrentRecord  := Min(FCurrentRecord - 1, iRecCount - 1);
          end;
          if FCurrentRecord < 0 then
            Result := grBOF;
        end;
      gmCurrent:
        begin
          iRecCount := RecordCount;
          if FCurrentRecord < 0 then
            Result := grBOF
          else if FCurrentRecord >= iRecCount then
            Result := grEOF;
        end;
    end;

    if Result = grOK then
    begin
      with PRecordInfo(ABuffer)^ do
      begin
        Bookmark     := FCurrentRecord;
        BookmarkFlag := bfCurrent;
      end;
      Finalize(PVariantList(ABuffer + SizeOf(TRecordInfo))^, Fields.Count);
      GetCalcFields(ABuffer);
    end;
  except
    if ADoCheck then
      raise;
    Result := grError;
  end;
  Logger.Watch('CurrentRecord', FCurrentRecord);
  Logger.ExitMethod(Self, 'InternalGetRecord');
end;

{$IF CompilerVersion > 21}
procedure TCustomVirtualDataset.InternalGotoBookmark(Bookmark: TBookmark);
{$ELSE}
procedure TCustomVirtualDataset.InternalGotoBookmark(Bookmark: Pointer);
{$IFEND}
begin
  FCurrentRecord := PInteger(Bookmark)^;
end;

procedure TCustomVirtualDataset.InternalAddRecord(Buffer: Pointer;
  Append: Boolean);
begin
end;

procedure TCustomVirtualDataset.InternalClose;
begin
  FInternalOpen := False;
  BindFields(False);
  FieldDefs.Updated := False;
  if FOldValueBuffer <> nil then
  begin
    try
      Finalize(PVariantList(FOldValueBuffer + SizeOf(TRecordInfo))^,
        Fields.Count);
      FreeMem(FOldValueBuffer);
    finally
      FOldValueBuffer := nil;
    end;
  end;
end;

procedure TCustomVirtualDataset.InternalCreateFields;
begin
  // TCustomVirtualDataset requires persistent fields to be defined
  if DefaultFields then
    VirtualDatasetError(SPersistentFieldsRequired, Self);
end;

procedure TCustomVirtualDataset.InternalDelete;
var
  RecBuf: TRecordBuffer;

begin
  GetActiveRecBuf(RecBuf);
  DoDeleteRecord(PRecordInfo(RecBuf)^.Bookmark);
end;

procedure TCustomVirtualDataset.InternalEdit;
begin
  FModifiedFields.Clear;

  if FOldValueBuffer = nil then
    FOldValueBuffer := AllocRecordBuffer
  else
    Finalize(PVariantList(FOldValueBuffer + SizeOf(TRecordInfo))^,
      Fields.Count);
end;

procedure TCustomVirtualDataset.InternalFirst;
begin
  FCurrentRecord := -1;
end;

procedure TCustomVirtualDataset.InternalHandleException;
begin
  Application.HandleException(Self);
end;

procedure TCustomVirtualDataset.InternalInitFieldDefs;
begin
  FieldDefs.Clear;
  LoadFieldDefsFromFields(Fields, FieldDefs);
end;

procedure TCustomVirtualDataset.InternalInitRecord(Buffer: TRecordBuffer);
var
  I: Integer;
begin
  Logger.EnterMethod(Self, 'InternalInitRecord');
  for I := 0 to Fields.Count - 1 do
    PVariantList(Buffer + SizeOf(TRecordInfo))[I] := 0;
  Logger.ExitMethod(Self, 'InternalInitRecord');
end;

procedure TCustomVirtualDataset.InternalLast;
begin
  FCurrentRecord := RecordCount;
end;

procedure TCustomVirtualDataset.InternalOpen;
begin
  Logger.EnterMethod(Self, 'InternalOpen');
  FInternalOpen := True;
  InternalFirst;
  BookmarkSize := SizeOf(Integer);
  FieldDefs.Updated := False;
  FieldDefs.Update;
  InternalCreateFields;
  FReserved := Pointer(FieldListCheckSum(Self));
  BindFields(True);
  FRecordBufferSize := SizeOf(TRecordInfo) + (Fields.Count * SizeOf(Variant));
  Logger.ExitMethod(Self, 'InternalOpen');
end;

procedure TCustomVirtualDataset.InternalPost;
var
  RecBuf: TRecordBuffer;
begin
  UpdateCursorPos;
  GetActiveRecBuf(RecBuf);
  if PRecordInfo(RecBuf)^.BookmarkFlag = bfEof then
    DoPostData(-1)
  else
    DoPostData(PRecordInfo(RecBuf)^.Bookmark);
end;

procedure TCustomVirtualDataset.InternalSetToRecord(Buffer: TRecordBuffer);
begin
  Logger.EnterMethod(Self, 'InternalSetToRecord');
  if PRecordInfo(Buffer)^.BookmarkFlag in [bfCurrent, bfInserted] then
  begin
    FCurrentRecord := PRecordInfo(Buffer)^.Bookmark;
  end;
  Logger.ExitMethod(Self, 'InternalSetToRecord');
end;

function TCustomVirtualDataset.IsCursorOpen: Boolean;
begin
  Result := FInternalOpen;
end;

procedure TCustomVirtualDataset.LoadFieldDefsFromFields(AFields: TFields;
  AFieldDefs: TFieldDefs);
var
  F  : TField;
  FD : TFieldDef;
begin
  for F in AFields do
  begin
    if AFieldDefs.IndexOf(F.FieldName) = -1 then
    begin
      FD          := AFieldDefs.AddFieldDef;
      FD.Name     := F.FieldName;
      FD.DataType := F.DataType;
      FD.Size     := F.Size;
      if F.Required then
        FD.Attributes := [faRequired];
      if ReadOnly then
        FD.Attributes := FD.Attributes + [faReadonly];
      if (F.DataType = ftBCD) and (F is TBCDField) then
        FD.Precision := TBCDField(F).Precision;
      if F is TObjectField then
        LoadFieldDefsFromFields(TObjectField(F).Fields, FD.ChildDefs);
    end;
  end;
end;

function TCustomVirtualDataset.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
var
  P: Integer;
begin
  if Assigned(FOnLocate) then
  begin
    P := -1;
    FOnLocate(Self, KeyFields, KeyValues, Options, P);
    Result := P <> -1;
    if Result and (P <> FCurrentRecord) then
    begin
      DoBeforeScroll;
      FCurrentRecord := P;
      Resync([rmCenter]);
      DoAfterScroll;
    end;
  end
  else
    Result := False;
end;

function TCustomVirtualDataset.Lookup(const KeyFields: string;
  const KeyValues: Variant; const ResultFields: string): Variant;
begin
  if Assigned(FOnLookupValue) then
  begin
    Result := Null;
    FOnLookupValue(Self, KeyFields, KeyValues, ResultFields, Result);
  end
  else
    Result := inherited Lookup(KeyFields, KeyValues, ResultFields);
end;

procedure TCustomVirtualDataset.MasterChanged(Sender: TObject);
begin
  if not Active then
    Exit;
  InternalFirst;
  Resync([]);
end;

procedure TCustomVirtualDataset.MasterDisabled(Sender: TObject);
begin
  if not Active then
    Exit;
  // Suggestion from Roman Linde
  // Do not reset cursor position because:
  // Second problem is with "MasterDisabled". Procedure executes when I call
  // "Enable controls" changing active record earlier set with "Locate".
  // I want to locate record with disabled controls and enabling controls should
  // not change active record?
  // FCurrent := -1;
  Resync([]);
end;

procedure TCustomVirtualDataset.SetBookmarkFlag(Buffer: TRecordBuffer;
  Value: TBookmarkFlag);
begin
  PRecordInfo(Buffer)^.BookmarkFlag := Value;
end;

procedure TCustomVirtualDataset.SetBookmarkData(Buffer: TRecordBuffer;
  Data: Pointer);
begin
  if PRecordInfo(Buffer)^.BookmarkFlag in [bfCurrent, bfInserted] then
    PRecordInfo(Buffer)^.Bookmark := PInteger(Data)^
  else
    PRecordInfo(Buffer)^.Bookmark := -1;
end;

procedure TCustomVirtualDataset.SetFieldData(Field: TField;
  Buffer: TValueBuffer; NativeFormat: Boolean);
begin
  Logger.EnterMethod(Self, 'SetFieldData');
  InternalSetFieldData(Field, Buffer, NativeFormat);
  Logger.ExitMethod(Self, 'SetFieldData');
end;

procedure TCustomVirtualDataset.InternalSetFieldData(AField: TField;
  ABuffer: Pointer; ANativeFormat: Boolean);
var
  Data  : Variant;
  RecBuf: TRecordBuffer;
begin
  with AField do
  begin
    if not(State in dsWriteModes) then
      DatabaseError(SNotEditing, Self);
    GetActiveRecBuf(RecBuf);

    if FieldNo > 0 then
    begin
      if readonly and not(State in [dsSetKey, dsFilter]) then
        DatabaseErrorFmt(SFieldReadOnly, [DisplayName]);

      Validate(ABuffer);

      if FModifiedFields.IndexOf(AField) = -1 then
      begin
        PVariantList(FOldValueBuffer + SizeOf(TRecordInfo))[AField.Index] :=
          AField.Value;
        FModifiedFields.Add(AField);
      end;
    end;

    if ABuffer = nil then
      Data := Null
    else
      BufferToVariant(AField, ABuffer, Data);

    PVariantList(RecBuf + SizeOf(TRecordInfo))[AField.Index] := Data;

    if not(State in [dsCalcFields, dsInternalCalc, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, Longint(AField));
  end;
end;

function TCustomVirtualDataset.GetFieldData(Field: TField;
   var Buffer: TValueBuffer): Boolean;
var
  RecBuf: TRecordBuffer;
  Data  : TValue;
  V     : Variant;
  Value : Variant;

  procedure RefreshBuffers;
  begin
    FReserved := Pointer(FieldListCheckSum(Self));
    UpdateCursorPos;
    Resync([]);
  end;

begin
  Logger.EnterMethod(Self, 'GetFieldData');
  if not Assigned(FReserved) then
    RefreshBuffers;
  if (State = dsOldValue) and (FModifiedFields.IndexOf(Field) <> -1) then
  // Requesting the old value of a modified field
  begin
    Result := True;
    RecBuf := FOldValueBuffer;
  end
  else
    Result := GetActiveRecBuf(RecBuf);

  if not Result then
    Exit;

  V := PVariantList(RecBuf + SizeOf(TRecordInfo))^[Field.Index];
  Data := TValue.FromVariant(V);

  // if data hasn't been loaded yet, then get data from dataset.
  if Data.IsEmpty then
  begin
    DoGetFieldValue(Field, PRecordInfo(RecBuf)^.Bookmark, Data);
    if Data.IsType<Boolean> then
      Value := Data.AsBoolean
    else
      Value := Data.AsVariant;

    PVariantList(RecBuf + SizeOf(TRecordInfo))[Field.Index] := Value
  end;

  Result := not Data.IsEmpty;
  if Result and (Buffer <> nil) then
  begin
    if Data.IsType<Boolean> then
      Value := Data.AsBoolean
    else
      Value := Data.AsVariant;
    //VariantToBuffer(Field, Data.AsVariant, Buffer, NativeFormat);
    VariantToBuffer(Field, Value, Pointer(Buffer), False);   {TODO -oTS -cGeneral : take a look at NativeFormat }

  end;
  Logger.ExitMethod(Self, 'GetFieldData');
end;
{$ENDREGION}

end.

{
The following documentation is taken from the fcl-db package and provides a
good understanding of TDataSet internals.

Contents
========

+ General remarks
+ Fields system
+ The buffers
+ Dataset implementation
+ Scalable Datasets.

===============
General remarks
===============

- All fields and descendents implemented.
- No calculated fields.
- No persistent fields; this must be added later.

=============
Fields system
=============

Buffers are completely handled by the Dataset. Fields don't handle
their own buffers. Only during validation, the FValueBuffer of the
field is used.

This allows the dataset to allocate a number of buffers for the current
record and the N next records. (getnextrecords/getpriorrecords method)

This means that all field mechanisms MUST pass through GetData/SetData,
since FValueBuffer is only valid during validation.

===========
The Buffers
===========

A buffer contains all the data for 1 record of the dataset, and also
the bookmark information. (bookmarkinformation is REQUIRED)

The dataset allocates by default 'DefaultBufferCount+1' records(buffers)
This constant can be changed, at the beginning of dataset.inc;
if you know you'll be working with big datasets, you can
increase this constant.

The buffers are stored as pchars in the FBuffers array;
The following constants are userd when handling this array:

FBuffercount : The number of buffers allocated, minus one.
FRecordCount : The number of buffers that is actually filled in.
FActiveBuffer : The index of the active record in TDataset
FCurrentRecord : The index of the supposedly active record in the underlaying
                 dataset (ie. the index in the last call to SetToInternalRecord)
                 call CursopPosChanged to reset FCurrentRecord if the active
                 record in the underlaying dataset has changed

So the following picture follows from this:

+---------------+
|  0            |
+---------------+
|  1            |
+---------------+
|               |
   ...
|               |
+---------------+
| FActivebuffer |
+---------------+
|               |
    ...
|               |
+---------------+
|FRecordCount-1 |
+---------------+
|               |
  ...
|               |
+---------------+
| FBufferCount  |
+---------------+

The array is zero based.

The following methods are used to manipulate the array:

GetNextRecords: Tries to fill up the entire array, going forward
GetPriorRecords: tries to fill up the entire array, going backward
GetNextRecord: gets the next record. Shifts the array if FrecordCount=BufferCount-1
GetPriorRecord: gets the previous record. Shifts the array if FrecordCount=BufferCount-1

For the last 2 methods: the underlying record pointer must be on the
last/first record in the dataset, or it will go wrong.

resync tries to refresh the array from the underlying dataset; it uses the
bookmarks for that.

=======================
Dataset implementations
=======================

TDataset does most of the work associated with fields, buffers and
navigating/editing/adding/removing records of some source of data.
There are, however, some methods that need to be filled in so that
a real TDataset can be implemented.

In order to have a working Dataset, the following Methods  need to be
overridden in order to make a dataset descendant:

function AllocRecordBuffer: PChar; virtual; abstract;
-----------------------------------------------------

Must allocate enough memory to store a complete record in the dataset.
Optionally, this buffer must contain enough memory to store bookmarkdata.
The descendent must be able to construct a bookmark from this buffer.

procedure FreeRecordBuffer(var Buffer: PChar); virtual; abstract;
-----------------------------------------------------------------

Must free the memory allocated in the AllocRecordBuffer call.

procedure GetBookmarkData(Buffer: PChar; Data: Pointer); virtual; abstract;
---------------------------------------------------------------------------

Puts the bookmarkdata for Buffer into the area pointed to by Data.

function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; virtual; abstract;
--------------------------------------------------------------------------

Returns the bookmarkflag associated with Buffer.

function GetFieldData(Field: TField; Buffer: Pointer): Boolean; virtual; abstract;
----------------------------------------------------------------------------------

Puts the data for field Field from the active buffer into Buffer.
This is called whenever a field value is demanded, so it must be
efficient.

function GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; virtual; abstract;
-----------------------------------------------------------------------------------

This method must do 3 things:
1) Get the record data for the next/current/previous record, depending
   on the GetMode value. It should return
    grOK    if all was OK.
    grBOF   if the previous record was requested, and we are at the start.
    grEOF   if the next record was requested, and we are at the end.
    grError if an error occurred.

2) If DoCheck is True, and the result is grError, then an exception must be
    raised.

3) It should initialize bookmark data for this record with flag 'bfCurrent'
   This data can be stored in the bufer, if space was allocated for it with
   AllocRecordBuffer.

function GetRecordSize: Word; virtual; abstract;
------------------------------------------------

Should return the record size; this includes ONLY the data portion
of teh buffer; it excludes any bookmark or housekeeping info you may
have put in the buffer.

procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); virtual; abstract;
---------------------------------------------------------------------------------

Adds a record to the dataset. The record's data is in Buffer and Append
indicates whether the record should be appended (True) or Inserted (False).
Note that for SQL based datasets, this has no meaning.

procedure InternalClose; virtual; abstract;
-------------------------------------------

Closes the dataset. Any resources allocated in InternalOpen should be freed
here.

procedure InternalDelete; virtual; abstract;
--------------------------------------------

Deletes the current Record.

procedure InternalFirst; virtual; abstract;
-------------------------------------------

This is called when 'First' is called; After this method, getrecord
should return 'grBOF' if the previous record is requested, and it should
return the next record if the next record is requested.

procedure InternalGotoBookmark(ABookmark: Pointer); virtual; abstract;
----------------------------------------------------------------------

Set the record position on the position that is associated with the
ABookMark data. The ABookMark data is the data that is acquired through
the GetBookMarkData call, and should be kept for each record.

procedure InternalHandleException; virtual; abstract;
-----------------------------------------------------

Not needed yet. Just implement an empty call.

procedure InternalInitFieldDefs; virtual; abstract;
---------------------------------------------------

This method should be called from InternalOpen, and should
initialize FieldDef definitions for all fields in a record.
It should add these definitions to the FFielddefs object.

procedure InternalInitRecord(Buffer: PChar); virtual; abstract;
---------------------------------------------------------------

This method is called to initialize a field buffer when the dataset
is put into edit or append mode. Mostly,you'll want to zero out the
buffer.

procedure InternalLast; virtual; abstract;
------------------------------------------

This is called when 'Last' is called; After this method, getrecord
should return 'grEOF' if the next record is requested, and it should
return the last record if the previous record is requested.

procedure InternalOpen; virtual; abstract;
------------------------------------------

Open the dataset. You must call internalinitfielddefs;
if DefaultFields is True, then you must call CreateFields,
which will create the necessary TFields from the fielddefs.

procedure InternalPost; virtual; abstract;
------------------------------------------

Post the data in the active buffer to the underlying dataset.

procedure InternalSetToRecord(Buffer: PChar); virtual; abstract;
----------------------------------------------------------------

Set the current record to the record in Buffer; if bookmark data
is specified in this buffer, that data can be used to determine which
record this should be.

function IsCursorOpen: Boolean; virtual; abstract;
--------------------------------------------------

This function should return True if data is available, even if the dataset
is not active.

procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); virtual; abstract;
----------------------------------------------------------------------------------

Set the bookmarkflag 'Value' on the data in Buffer.

procedure SetBookmarkData(Buffer: PChar; Data: Pointer); virtual; abstract;
---------------------------------------------------------------------------

Move the bookmarkdata in 'Data' to the bookmarkdata associated with Buffer

procedure SetFieldData(Field: TField; Buffer: Pointer); virtual; abstract;
--------------------------------------------------------------------------

Move the data in associated with Field from Buffer to the activebuffer.

=================
Scalable datasets
=================

In order to have Scalable database access, the concept of TDatabase and
TDBDataset is introduced. The idea is that, in a visual IDE, the change
from one database to another is achieved by simply removing one TDatabase
descendent (Say, TMySqlDatabase) with another (Say, TPostGreSQLDatabase)
and that the Datasets remain untouched.

In order to make this possible, the following scheme is used:

when a TDBdataset descendant is put on Active, it requests a TRecordSet
from the TDatabase. The TRecordSet is an abstract object that should be
implemented together with each database. The TDBDataset then uses the
TRecordSet to navigate through the records and edit/add/modify them.
The TDBdataset implements the abstract methods of Tdataset in order to
achive this.

There will be 2 descendants of TDBdataset: TTable and TQuery; both will
implement the last abstract methods of TDataset in order to achieve a
complete TDataset implementation.

TDBDataset implements most of the initialization of fields, so the
implementation of TRecordSet will be as bare bones as possible.

What is needed:
---------------

Some properties describing the data:

FieldCount : Number of fields in a record;
FieldTypes[Index] : Types of the fields (TFieldType), zero based.
FieldNames[Index] : Names of the fields. Zero based.
FieldSizes[index] : Size of the fields, zero based.
BookmarkSize        : Size of a bookmark.

Some properties with the data content:

FieldBuffers[Index] : Buffers containing the actual data of the current record.
                      (Nil if the field is empty)
                      This data should be of size indicated FieldSizes, and
                      in a format that matches the fieldtype.
BookMarkBuffer      : Buffer with the current bookmark.

Some methods
------------

OpenRecordSet : Opens the recordset; it should initialize the FieldCount
                and FieldTypes, FieldNames, and FieldSizes array data.

CloseRecordSet : Do whatever is needed to close the recordset.

GotoBookMark : go to the record described by the bookmark. Returns True
               if successfull, false if not.

Next  : Goto the next record. Returns true or false
Prior : Goto previous record. Returns true or false
First : Goto the first record. Returns True or false
Last  : Goto the last record. Returns True or False

AppendBuffer : Append a buffer to the records.
}
