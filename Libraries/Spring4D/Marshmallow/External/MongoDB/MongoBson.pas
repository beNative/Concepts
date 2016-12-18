{
     Copyright 2009-2011 10gen Inc.
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
{ This unit implements BSON, a binary JSON-like document format.
  It is used to represent documents in MongoDB and also for network traffic.
  See http://www.mongodb.org/display/DOCS/BSON }
unit MongoBson;

interface

const
  IID_IBSONDocument : TGUID = '{AF9DB8BC-E775-4AC6-8E87-117B2E5FAC9F}';

type
  TBson = class;
  IBsonDocument = interface;



  TIntegerArray = array of Integer;
  TDoubleArray = array of Double;
  TBooleanArray = array of Boolean;
  TStringArray = array of string;
  { A value of TBsonType indicates the type of the data associated
    with a field within a BSON document. }
  TBsonType = (
    bsonEOO = 0,
    bsonDOUBLE = 1,
    bsonSTRING = 2,
    bsonOBJECT = 3,
    bsonARRAY = 4,
    bsonBINDATA = 5,
    bsonUNDEFINED = 6,
    bsonOID = 7,
    bsonBOOL = 8,
    bsonDATE = 9,
    bsonNULL = 10,
    bsonREGEX = 11,
    bsonDBREF = 12, (* Deprecated. *)
    bsonCODE = 13,
    bsonSYMBOL = 14,
    bsonCODEWSCOPE = 15,
    bsonINT = 16,
    bsonTIMESTAMP = 17,
    bsonLONG = 18);
  TBsonIterator = class;
  { A TBsonOID is used to store BSON Object IDs.
    See http://www.mongodb.org/display/DOCS/Object+IDs }
  TBsonOID = class(TObject)
  public
    //var
    //private
      { the oid data }
    value: array[0..11] of Byte;
    { Generate an Object ID }
  public
    constructor Create(); overload;
    { Create an ObjectID from a 24-digit hex string }
    constructor Create(s: string); overload;
    { Create an Object ID from a TBsonIterator pointing to an oid field }
    constructor Create(i: TBsonIterator); overload;
    { Convert this Object ID to a 24-digit hex string }
    function AsString(): string;
  end;
  { A TBsonCodeWScope is used to hold javascript code and its associated scope.
    See TBsonIterator.getCodeWScope() }
  TBsonCodeWScope = class(TObject)
  public
    //var
    //private
    code: string;
    scope: IBsonDocument;
    { Create a TBsonCodeWScope from a javascript string and a TBson scope }
  public
    constructor Create(code_: string; scope_: IBsonDocument); overload;
    { Create a TBsonCodeWScope from a TBSonIterator pointing to a
      CODEWSCOPE field. }
    constructor Create(i: TBsonIterator); overload;
  end;
  { A TBsonRegex is used to hold a regular expression string and its options.
    See TBsonIterator.getRegex(). }
  TBsonRegex = class(TObject)
  public
    //var
    //private
    pattern: string;
    options: string;
    { Create a TBsonRegex from reqular expression and options strings }
  public
    constructor Create(pattern_: string; options_: string); overload;
    { Create a TBsonRegex from a TBsonIterator pointing to a REGEX field }
    constructor Create(i: TBsonIterator); overload;
  end;
  { A TBsonTimestamp is used to hold a TDateTime and an increment value.
    See http://www.mongodb.org/display/DOCS/Timestamp+data+type and
    TBsonIterator.getTimestamp() }
  TBsonTimestamp = class(TObject)
  public
    //var
    //private
    time: TDateTime;
    increment: Integer;
    { Create a TBsonTimestamp from a TDateTime and an increment }
  public
    constructor Create(time_: TDateTime; increment_: Integer); overload;
    { Create a TBSonTimestamp from a TBsonIterator pointing to a TIMESTAMP
      field. }
    constructor Create(i: TBsonIterator); overload;
  end;
  { A TBsonBinary is used to hold the contents of BINDATA fields.
    See TBsonIterator.getBinary() }
  TBsonBinary = class(TObject)
    //var
  public
    { Pointer to data allocated on the heap with GetMem }
    data: Pointer;
    { The length of the data in bytes }
    len: Integer;
    { The subtype of the BINDATA (usually 0) }
    kind: Integer;
    { Create a TBsonBinary from a pointer and a length.  The data
      is copied to the heap.  kind is initialized to 0 }
  public
    constructor Create(p: Pointer; length: Integer); overload;
    { Create a TBsonBinary from a TBsonIterator pointing to a BINDATA
      field. }
    constructor Create(i: TBsonIterator); overload;
    { Destroys the TBsonBinary and releases its memory with FreeMem() }
    destructor Destroy(); override;
  end;
  { A TBsonBuffer is used to build a BSON document by appending the
    names and values of fields.  Call finish() when done to convert
    the buffer to a TBson which can be used in database operations.
    Example: @longcode(#
      var
        bb : TBsonBuffer;
        b  : TBson;
      begin
        bb := TBsonBuffer.Create();
        bb.append('name', 'Joe');
        bb.append('age', 33);
        bb.append('city', 'Boston');
        b := bb.finish();
      end;
    #) }
  TBsonBuffer = class(TObject)
  //public
    private
      //var
    handle: Pointer;
  public
    { Create an empty TBsonBuffer ready to have fields appended. }
    constructor Create(); overload;
    constructor Create(AHandle: Pointer); overload;
    { Append a ansiString (inner convert to pansichar)}
    function append(name: string; value: AnsiString): Boolean; overload;
    { Append a string (PAnsiChar) to the buffer }
    function append(name: string; value: PAnsiChar): Boolean; overload;
    { Append an Integer to the buffer }
    function append(name: string; value: Integer): Boolean; overload;
    { Append an Int64 to the buffer }
    function append(name: string; value: Int64): Boolean; overload;
    { Append a Double to the buffer }
    function appendDouble(name: string; value: Double): Boolean; //BUG 不能和其他重载？ overload;
    { Append a TDateTime to the buffer; converted to 64-bit POSIX time }
    function append(name: string; value: TDateTime): Boolean; overload;
    { Append a Boolean to the buffer }
    function append(name: string; value: Boolean): Boolean; overload;
    { Append an Object ID to the buffer }
    function append(name: string; value: TBsonOID): Boolean; overload;
    { Append a CODEWSCOPE to the buffer }
    function append(name: string; value: TBsonCodeWScope): Boolean; overload;
    { Append a REGEX to the buffer }
    function append(name: string; value: TBsonRegex): Boolean; overload;
    { Append a TIMESTAMP to the buffer }
    function append(name: string; value: TBsonTimestamp): Boolean; overload;
    { Append BINDATA to the buffer }
    function append(name: string; value: TBsonBinary): Boolean; overload;
    { Append a TBson document as a subobject }
    function append(name: string; value: IBsonDocument): Boolean; overload;
    { Generic version of append.  Calls one of the other append functions
      if the type contained in the variant is supported. }
    function append(name: string; value: OleVariant): Boolean; overload;
    { Append an array of Integers }
    function appendArray(name: string; value: array of Integer): Boolean; overload;
    { Append an array of Doubles }
    function appendArray(name: string; value: array of Double): Boolean; overload;
    { Append an array of Booleans }
    function appendArray(name: string; value: array of Boolean): Boolean; overload;
    { Append an array of strings }
    function appendArray(name: string; value: array of string): Boolean; overload;

    function appendArray(name: string; value: array of IBsonDocument): Boolean; overload;

    function appendArray(name: string; value: array of Variant): Boolean; overload;
    { Append a NULL field to the buffer }
    function appendNull(name: string): Boolean;
    { Append an UNDEFINED field to the buffer }
    function appendUndefined(name: string): Boolean;
    { Append javascript code to the buffer }
    function appendCode(name: string; value: PAnsiChar): Boolean;
    { Append a SYMBOL to the buffer }
    function appendSymbol(name: string; value: PAnsiChar): Boolean;
    { Alternate way to append BINDATA directly without first creating a
      TBsonBinary value }
    function appendBinary(name: string; kind: Integer; data: Pointer; length: Integer): Boolean;
    { Indicate that you will be appending more fields as a subobject }
    function startObject(name: string): Boolean;
    { Indicate that you will be appending more fields as an array }
    function startArray(name: string): Boolean;
    { Indicate that a subobject or array is done. }
    function finishObject(): Boolean;
    { Return the current size of the BSON document you are building }
    function size(): Integer;
    { Call this when finished appending fields to the buffer to turn it into
      a TBson for network transport. }
    function finish(): IBsonDocument;
    { Destroy this TBsonBuffer.  Releases external resources. }
    destructor Destroy(); override;
  end;

  

  IBsonDocument = interface
    ['{AF9DB8BC-E775-4AC6-8E87-117B2E5FAC9F}']
    function size(): Integer;
    { Get a TBsonIterator that points to the first field of this BSON }
    function iterator(): TBsonIterator;
    { Get a TBsonIterator that points to the field with the given name.
      If name is not found, nil is returned. }
    function find(name: string): TBsonIterator;
    { Get the value of a field given its name.  This function does not support
      all BSON field types.  Use find() and one of the 'get' functions of
      TBsonIterator to retrieve special values. }
    function value(const name: string): OleVariant;
    { Display this BSON document on the console.  subobjects and arrays are
      appropriately indented. }
    procedure display();

    function getHandle(): Pointer;

    procedure setItem(const Key: string; Value: OleVariant);

    property Item[const Key: string]: OleVariant read value write setItem; default;
  end;

  
  { A TBson holds a BSON document.  BSON is a binary, JSON-like document format.
    It is used to represent documents in MongoDB and also for network traffic.
    See http://www.mongodb.org/display/DOCS/BSON   }
  TBson = class(TInterfacedObject, IBsonDocument)
    { Pointer to externally managed data.  User code should not modify this.
      It is public only because the MongoDB and GridFS units must access it. }
    //var
  private
    FIterator: TBsonIterator;
  public
    handle: Pointer;
    { Return the size of this BSON document in bytes }
    function getHandle(): Pointer;
  public
    function size(): Integer;
    { Get a TBsonIterator that points to the first field of this BSON }
    function iterator(): TBsonIterator;
    { Get a TBsonIterator that points to the field with the given name.
      If name is not found, nil is returned. }
    function find(name: string): TBsonIterator;
    { Get the value of a field given its name.  This function does not support
      all BSON field types.  Use find() and one of the 'get' functions of
      TBsonIterator to retrieve special values. }
    function value(const name: string): OleVariant;
    { Display this BSON document on the console.  subobjects and arrays are
      appropriately indented. }
    procedure display();

    procedure setItem(const Key: string; Value: OleVariant);

    { Create a TBson given a pointer to externally managed data describing
      the document.  User code should not instantiate TBson directly.  Use
      TBsonBuffer and finish() to create BSON documents. }
    constructor Create(); overload;
    constructor Create(AHandle: Pointer); overload;
    { Destroy this TBson.  Releases the externally managed data. }
    destructor Destroy; override;
  end;

  IBsonIterator = interface
    ['{DC20D1AA-3FA3-4D11-8789-4F019CB8958D}']
    function kind(): TBsonType;
    { Return the key (or name) of the field pointed to by this iterator. }
    function key(): string;
    { Step to the first or next field of a TBson document.  Returns True
      if there is a next field; otherwise, returns false at the end of the
      document (or subobject).
      Example: @longcode(#
        iter := b.iterator;
        while i.next() do
           if i.kind = bsonNULL then
              WriteLn(i.key, ' is a NULL field.');
      #) }
    function next(): Boolean;
    { Get the value of the field pointed to by this iterator.  This function
      does not support all BSON field types and will throw an exception for
      those it does not.  Use one of the 'get' functions to extract one of these
      special types. }
    function value(): Variant;
  end;
  
  { TBsonIterators are used to step through the fields of a TBson document. }
  TBsonIterator = class(TInterfacedObject, IBsonIterator)
  //public
  private
       { Pointer to externally managed data. }
       //var
    handle: Pointer;
  protected
    function GetValueFromIteratorObject(AIterator: TBsonIterator = nil): Variant; virtual;
    function GetValueFromIteratorArray(AIterator: TBsonIterator = nil): Variant; virtual;
  public
    { Return the TBsonType of the field pointed to by this iterator. }
    function kind(): TBsonType;
    { Return the key (or name) of the field pointed to by this iterator. }
    function key(): string;
    { Step to the first or next field of a TBson document.  Returns True
      if there is a next field; otherwise, returns false at the end of the
      document (or subobject).
      Example: @longcode(#
        iter := b.iterator;
        while i.next() do
           if i.kind = bsonNULL then
              WriteLn(i.key, ' is a NULL field.');
      #) }
    function next(): Boolean;
    { Get the value of the field pointed to by this iterator.  This function
      does not support all BSON field types and will throw an exception for
      those it does not.  Use one of the 'get' functions to extract one of these
      special types. }
    function value(): Variant;
    { Get an TBsonIterator pointing to the first field of a subobject or array.
      kind() must be bsonOBJECT or bsonARRAY. }
    function subiterator(): TBsonIterator;
    { Get an Object ID from the field pointed to by this iterator. }
    function getOID(): TBsonOID;
    { Get a TBsonCodeWScope object for a CODEWSCOPE field pointed to by this
      iterator. }
    function getCodeWScope(): TBsonCodeWScope;
    { Get a TBsonRegex for a REGEX field }
    function getRegex(): TBsonRegex;
    { Get a TBsonTimestamp object for a TIMESTAMP field pointed to by this
      iterator. }
    function getTimestamp(): TBsonTimestamp;
    { Get a TBsonBinary object for the BINDATA field pointed to by this
      iterator. }
    function getBinary(): TBsonBinary;
    { Get an array of Integers.  This iterator must point to ARRAY field
      which has each component type as Integer }
    function getIntegerArray(): TIntegerArray;
    { Get an array of Doubles.  This iterator must point to ARRAY field
      which has each component type as Double }
    function getDoubleArray(): TDoubleArray;
    { Get an array of strings.  This iterator must point to ARRAY field
      which has each component type as string }
    function getStringArray(): TStringArray;
    { Get an array of Booleans.  This iterator must point to ARRAY field
      which has each component type as Boolean }
    function getBooleanArray(): TBooleanArray;
    { Internal usage only.  Create an uninitialized TBsonIterator }
    constructor Create(); overload;
    { Create a TBsonIterator that points to the first field of the given
      TBson }
    constructor Create(b: IBsonDocument); overload;
    { Destroy this TBsonIterator.  Releases external resources. }
    destructor Destroy; override;
  end;

  { An empty BSON document }

  (* The idea for this shorthand way to build a BSON
     document from an array of variants came from Stijn Sanders
     and his TMongoWire, located here:
     https://github.com/stijnsanders/TMongoWire
     Subobjects are started with '{' and ended with '}'
     Example: @longcode(#
       var b : TBson;
       begin
         b := BSON(['name', 'Albert', 'age', 64,
                     'address', '{',
                        'street', '109 Vine Street',
                        'city', 'New Haven',
                        '}' ]);
  #) *)
function bsonEmpty(): IBsonDocument;
function BSON(x: array of OleVariant): IBsonDocument;
function JsonToBson(const AJson: string): IBsonDocument;
{ Convert a byte to a 2-digit hex string }
function ByteToHex(InByte: Byte): string;
{ Convert an Int64 to a Double.  Some loss of precision may occur. }
function Int64toDouble(i64: int64): double;
cdecl; external 'mongoc.dll' name 'bson_int64_to_double';

implementation

uses
  SysUtils,
  Variants,
  DateUtils,
  bsonUtils,
  uInt64OleVariant
  ,superobject
  ,Spring.Persistence.Core.EmbeddedEntity
  ,Spring.Persistence.Core.Interfaces
  ;
{$IF not declared(UTF8ToWideString)}

function UTF8ToWideString(const s: UTF8string): widestring;
begin
  result := UTF8Decode(s);
end;
{$IFEND}

function bsonEmpty(): IBsonDocument;
begin
  Result := BSON([]);
end;

procedure set_bson_err_handler(err_handler: Pointer); cdecl; external 'mongoc.dll';

function bson_create(): Pointer; cdecl; external 'mongoc.dll';

procedure bson_init(b: Pointer); cdecl; external 'mongoc.dll';

procedure bson_destroy(b: Pointer); cdecl; external 'mongoc.dll';

procedure bson_dispose(b: Pointer); cdecl; external 'mongoc.dll';

procedure bson_copy(dest: Pointer; src: Pointer); cdecl; external 'mongoc.dll';

function bson_finish(b: Pointer): Integer; cdecl; external 'mongoc.dll';

procedure bson_oid_gen(oid: Pointer); cdecl; external 'mongoc.dll';

procedure bson_oid_to_string(oid: Pointer; s: PAnsiChar); cdecl; external 'mongoc.dll';

procedure bson_oid_from_string(oid: Pointer; s: PAnsiChar); cdecl; external 'mongoc.dll';

function bson_append_string(b: Pointer; name: PAnsiChar; value: PAnsiChar): Integer;
  cdecl; external 'mongoc.dll';

function bson_append_code(b: Pointer; name: PAnsiChar; value: PAnsiChar): Integer;
  cdecl; external 'mongoc.dll';

function bson_append_symbol(b: Pointer; name: PAnsiChar; value: PAnsiChar): Integer;
  cdecl; external 'mongoc.dll';

function bson_append_int(b: Pointer; name: PAnsiChar; value: Integer): Integer;
  cdecl; external 'mongoc.dll';

function bson_append_long(b: Pointer; name: PAnsiChar; value: Int64): Integer;
  cdecl; external 'mongoc.dll';

function bson_append_double(b: Pointer; name: PAnsiChar; value: Double): Integer;
  cdecl; external 'mongoc.dll';

function bson_append_date(b: Pointer; name: PAnsiChar; value: Int64): Integer;
  cdecl; external 'mongoc.dll';

function bson_append_bool(b: Pointer; name: PAnsiChar; value: Boolean): Integer;
  cdecl; external 'mongoc.dll';

function bson_append_null(b: Pointer; name: PAnsiChar): Integer;
  cdecl; external 'mongoc.dll';

function bson_append_undefined(b: Pointer; name: PAnsiChar): Integer;
  cdecl; external 'mongoc.dll';

function bson_append_start_object(b: Pointer; name: PAnsiChar): Integer;
  cdecl; external 'mongoc.dll';

function bson_append_start_array(b: Pointer; name: PAnsiChar): Integer;
  cdecl; external 'mongoc.dll';

function bson_append_finish_object(b: Pointer): Integer; cdecl; external 'mongoc.dll';

function bson_append_oid(b: Pointer; name: PAnsiChar; oid: Pointer): Integer; cdecl; external 'mongoc.dll';

function bson_append_code_w_scope(b: Pointer; name: PAnsiChar; code: PAnsiChar; scope: Pointer): Integer;
  cdecl; external 'mongoc.dll';

function bson_append_regex(b: Pointer; name: PAnsiChar; pattern: PAnsiChar; options: PAnsiChar): Integer;
  cdecl; external 'mongoc.dll';

function bson_append_timestamp2(b: Pointer; name: PAnsiChar; time: Integer; increment: Integer): Integer;
  cdecl; external 'mongoc.dll';

function bson_append_binary(b: Pointer; name: PAnsiChar; kind: Byte; data: Pointer; len: Integer): Integer;
  cdecl; external 'mongoc.dll';

function bson_append_bson(b: Pointer; name: PAnsiChar; value: Pointer): Integer;
  cdecl; external 'mongoc.dll';

function bson_buffer_size(b: Pointer): Integer; cdecl; external 'mongoc.dll';

function bson_size(b: Pointer): Integer; cdecl; external 'mongoc.dll';

function bson_iterator_create(): Pointer; cdecl; external 'mongoc.dll';

procedure bson_iterator_dispose(i: Pointer); cdecl; external 'mongoc.dll';

procedure bson_iterator_init(i: Pointer; b: Pointer); cdecl; external 'mongoc.dll';

function bson_find(i: Pointer; b: Pointer; name: PAnsiChar): TBsonType;
  cdecl; external 'mongoc.dll';

function bson_iterator_type(i: Pointer): TBsonType; cdecl; external 'mongoc.dll';

function bson_iterator_next(i: Pointer): TBsonType; cdecl; external 'mongoc.dll';

function bson_iterator_key(i: Pointer): PAnsiChar; cdecl; external 'mongoc.dll';

function bson_iterator_double(i: Pointer): Double; cdecl; external 'mongoc.dll';

function bson_iterator_long(i: Pointer): Int64; cdecl; external 'mongoc.dll';

function bson_iterator_int(i: Pointer): Integer; cdecl; external 'mongoc.dll';

function bson_iterator_bool(i: Pointer): Boolean; cdecl; external 'mongoc.dll';

function bson_iterator_string(i: Pointer): PAnsiChar; cdecl; external 'mongoc.dll';

function bson_iterator_date(i: Pointer): Int64; cdecl; external 'mongoc.dll';

procedure bson_iterator_subiterator(i: Pointer; sub: Pointer);
  cdecl; external 'mongoc.dll';

function bson_iterator_oid(i: Pointer): Pointer; cdecl; external 'mongoc.dll';

function bson_iterator_code(i: Pointer): PAnsiChar; cdecl; external 'mongoc.dll';

procedure bson_iterator_code_scope(i: Pointer; b: Pointer); cdecl; external 'mongoc.dll';

function bson_iterator_regex(i: Pointer): PAnsiChar; cdecl; external 'mongoc.dll';

function bson_iterator_regex_opts(i: Pointer): PAnsiChar; cdecl; external 'mongoc.dll';

function bson_iterator_timestamp_time(i: Pointer): Integer; cdecl; external 'mongoc.dll';

function bson_iterator_timestamp_increment(i: Pointer): Integer; cdecl; external 'mongoc.dll';

function bson_iterator_bin_len(i: Pointer): Integer; cdecl; external 'mongoc.dll';

function bson_iterator_bin_type(i: Pointer): Byte; cdecl; external 'mongoc.dll';

function bson_iterator_bin_data(i: Pointer): Pointer; cdecl; external 'mongoc.dll';

constructor TBsonOID.Create();
begin
  bson_oid_gen(@value);
end;

constructor TBsonOID.Create(s: string);
begin
  if length(s) <> 24 then
    raise Exception.Create('Expected a 24 digit hex string');
  bson_oid_from_string(@value, PAnsiChar(AnsiString(s)));
end;

constructor TBsonOID.Create(i: TBsonIterator);
var
  p: PByte;
begin
  p := bson_iterator_oid(i.handle);
  Move(p^, value, 12);
end;

function TBsonOID.AsString(): string;
var
  buf: array[0..24] of AnsiChar;
begin
  bson_oid_to_string(@value, @buf);
  Result := string(buf);
end;

constructor TBsonIterator.Create();
begin
  inherited Create();
  handle := bson_iterator_create();
end;

constructor TBsonIterator.Create(b: IBsonDocument);
begin
  inherited Create();
  handle := bson_iterator_create();
  bson_iterator_init(handle, b.getHandle);
end;

destructor TBsonIterator.Destroy;
begin
  bson_iterator_dispose(handle);
end;

function TBsonIterator.kind(): TBsonType;
begin
  Result := bson_iterator_type(handle);
end;

function TBsonIterator.next(): Boolean;
begin
  Result := bson_iterator_next(handle) <> bsonEOO;
end;

function TBsonIterator.key(): string;
begin
  Result := string(UTF8ToWideString(bson_iterator_key(handle)));
end;

function TBsonIterator.GetValueFromIteratorArray(AIterator: TBsonIterator): Variant;
var
  LEmbeddedInterfaceEntity: IDBResultset;
  LEntity: TEmbeddedArrayEntity;
  LIter: TBsonIterator;
begin
  LEntity := TEmbeddedArrayEntity.Create;
  if AIterator = nil then
    LIter := subiterator()
  else
    LIter := AIterator;

  try
    while LIter.next do
    begin
      case LIter.kind of
        bsonOBJECT:
        begin
          LEntity.AddValue(LIter.key, GetValueFromIteratorObject(LIter.subiterator));
        end;
        bsonARRAY: LEntity.AddValue(LIter.key, GetValueFromIteratorArray(LIter.subiterator));
        else
        begin
          //add simple value
          LEntity.AddValue(LIter.key, LIter.value());
        end;
      end;
    end;
  finally
    LIter.Free;
  end;    
  LEmbeddedInterfaceEntity := LEntity;
  Result := LEmbeddedInterfaceEntity;
end;

function TBsonIterator.GetValueFromIteratorObject(AIterator: TBsonIterator): Variant;
var
  LEmbeddedInterfaceEntity: IDBResultset;
  LEntity: TEmbeddedObjectEntity;
  LIter: TBsonIterator;
begin
  LEntity := TEmbeddedObjectEntity.Create;
  if AIterator = nil then
    LIter := subiterator()
  else
    LIter := AIterator;
  try
    while LIter.next do
    begin
      case LIter.kind of
        bsonOBJECT:
        begin
          LEntity.AddValue(LIter.key, GetValueFromIteratorObject(LIter.subiterator));
        end;
        bsonARRAY: LEntity.AddValue(LIter.key, GetValueFromIteratorArray(LIter.subiterator));
        else
        begin
          //add simple value
          LEntity.AddValue(LIter.key, LIter.value());
        end;  
      end;
    end;

    LEmbeddedInterfaceEntity := LEntity;
    Result := LEmbeddedInterfaceEntity;
  finally
    LIter.Free;
  end;
end;

function TBsonIterator.value(): Variant;
var
  k: TBsonType;
  d: TDateTime;
begin
  k := kind();
  case k of
    bsonEOO, bsonNULL: Result := Null;
    bsonDOUBLE: Result := bson_iterator_double(handle);
    bsonSTRING, bsonCODE, bsonSYMBOL:
      Result := string(UTF8ToWideString(bson_iterator_string(handle)));
    bsonINT: Result := bson_iterator_int(handle);
    bsonBOOL: Result := bson_iterator_bool(handle);
    bsonOBJECT:
    begin
      Result := GetValueFromIteratorObject();
    end;
    bsonARRAY: Result := GetValueFromIteratorArray();
    bsonDATE:
      begin
        d := Int64toDouble(bson_iterator_date(handle)) / (1000 * 24 * 60 * 60) + 25569;
        Result := d;
      end;
    bsonLONG: Result := bson_iterator_long(handle);
  else
    raise Exception.Create('BsonType (' + IntToStr(Ord(k)) + ') not supported by TBsonIterator.value');
  end;
end;

function TBsonIterator.getOID(): TBsonOID;
begin
  Result := TBsonOID.Create(Self);
end;

function TBsonIterator.getCodeWScope(): TBsonCodeWScope;
begin
  Result := TBsonCodeWScope.Create(Self);
end;

function TBsonIterator.getRegex(): TBsonRegex;
begin
  Result := TBsonRegex.Create(Self);
end;

function TBsonIterator.getTimestamp(): TBsonTimestamp;
begin
  Result := TBsonTimestamp.Create(Self);
end;

function TBsonIterator.getBinary(): TBsonBinary;
begin
  Result := TBsonBinary.Create(Self);
end;

function TBsonIterator.subiterator(): TBsonIterator;
var
  i: TBsonIterator;
begin
  i := TBsonIterator.Create();
  bson_iterator_subiterator(handle, i.handle);
  Result := i;
end;

function TBsonIterator.getIntegerArray(): TIntegerArray;
var
  i: TBsonIterator;
  j, count: Integer;
begin
  if kind() <> bsonArray then
    raise Exception.Create('Iterator does not point to an array');
  i := subiterator();
  count := 0;
  while i.next() do
  begin
    if i.kind() <> bsonINT then
      raise Exception.Create('Array component is not an Integer');
    inc(count);
  end;
  i := subiterator;
  j := 0;
  SetLength(Result, count);
  while i.next() do
  begin
    Result[j] := i.value();
    inc(j);
  end;
end;

function TBsonIterator.getDoubleArray(): TDoubleArray;
var
  i: TBsonIterator;
  j, count: Integer;
begin
  if kind() <> bsonArray then
    raise Exception.Create('Iterator does not point to an array');
  i := subiterator();
  count := 0;
  while i.next() do
  begin
    if i.kind() <> bsonDOUBLE then
      raise Exception.Create('Array component is not a Double');
    inc(count);
  end;
  i := subiterator;
  j := 0;
  SetLength(Result, count);
  while i.next() do
  begin
    Result[j] := i.value();
    inc(j);
  end;
end;

function TBsonIterator.getStringArray(): TStringArray;
var
  i: TBsonIterator;
  j, count: Integer;
begin
  if kind() <> bsonArray then
    raise Exception.Create('Iterator does not point to an array');
  i := subiterator();
  count := 0;
  while i.next() do
  begin
    if i.kind() <> bsonSTRING then
      raise Exception.Create('Array component is not a string');
    inc(count);
  end;
  i := subiterator;
  j := 0;
  SetLength(Result, count);
  while i.next() do
  begin
    Result[j] := string( i.value());
    inc(j);
  end;
end;

function TBsonIterator.getBooleanArray(): TBooleanArray;
var
  i: TBsonIterator;
  j, count: Integer;
begin
  if kind() <> bsonArray then
    raise Exception.Create('Iterator does not point to an array');
  i := subiterator();
  count := 0;
  while i.next() do
  begin
    if i.kind() <> bsonBOOL then
      raise Exception.Create('Array component is not a Boolean');
    inc(count);
  end;
  i := subiterator;
  j := 0;
  SetLength(Result, count);
  while i.next() do
  begin
    Result[j] := i.value();
    inc(j);
  end;
end;

function TBson.value(const name: string): OleVariant;
var
  i: TBsonIterator;
begin
  i := find(name);
  if i = nil then
    Result := Null
  else
  begin
    Result := i.value;
  end;
end;

function TBson.iterator(): TBsonIterator;
begin
  if not Assigned(FIterator) then
    FIterator := TBsonIterator.Create(Self);
  Result := FIterator;
end;

constructor TBsonBuffer.Create();
begin
  inherited Create();
  handle := bson_create();
  bson_init(handle);
end;

constructor TBsonBuffer.Create(AHandle: Pointer);
begin
  handle := AHandle;
end;

destructor TBsonBuffer.Destroy();
begin
  bson_destroy(handle);
  bson_dispose(handle);
  inherited Destroy();
end;

function TBsonBuffer.append(name: string; value: AnsiString): Boolean;
begin
  if handle = nil then
    raise Exception.Create('BsonBuffer already finished');
  Result := (bson_append_string(handle, PAnsiChar(System.UTF8Encode(name)), PAnsiChar(value)) = 0);
end;

function TBsonBuffer.append(name: string; value: PAnsiChar): Boolean;
begin
  if handle = nil then
    raise Exception.Create('BsonBuffer already finished');
  Result := (bson_append_string(handle, PAnsiChar(System.UTF8Encode(name)), value) = 0);
end;

function TBsonBuffer.appendCode(name: string; value: PAnsiChar): Boolean;
begin
  if handle = nil then
    raise Exception.Create('BsonBuffer already finished');
  Result := (bson_append_code(handle, PAnsiChar(System.UTF8Encode(name)), value) = 0);
end;

function TBsonBuffer.appendSymbol(name: string; value: PAnsiChar): Boolean;
begin
  if handle = nil then
    raise Exception.Create('BsonBuffer already finished');
  Result := (bson_append_symbol(handle, PAnsiChar(System.UTF8Encode(name)), value) = 0);
end;

function TBsonBuffer.append(name: string; value: Integer): Boolean;
begin
  if handle = nil then
    raise Exception.Create('BsonBuffer already finished');
  Result := (bson_append_int(handle, PAnsiChar(System.UTF8Encode(name)), value) = 0);
end;

function TBsonBuffer.append(name: string; value: Int64): Boolean;
begin
  if handle = nil then
    raise Exception.Create('BsonBuffer already finished');
  Result := (bson_append_long(handle, PAnsiChar(System.UTF8Encode(name)), value) = 0);
end;

function TBsonBuffer.appendDouble(name: string; value: Double): Boolean;
begin
  if handle = nil then
    raise Exception.Create('BsonBuffer already finished');
  Result := (bson_append_double(handle, PAnsiChar(System.UTF8Encode(name)), value) = 0);
end;

function TBsonBuffer.append(name: string; value: TDateTime): Boolean;
begin
  if handle = nil then
    raise Exception.Create('BsonBuffer already finished');
  Result := (bson_append_date(handle, PAnsiChar(System.UTF8Encode(name)), Trunc((value - 25569) * 1000 * 60 * 60 * 24))
    = 0);
end;

function TBsonBuffer.append(name: string; value: Boolean): Boolean;
begin
  if handle = nil then
    raise Exception.Create('BsonBuffer already finished');
  Result := (bson_append_bool(handle, PAnsiChar(System.UTF8Encode(name)), value) = 0);
end;

function TBsonBuffer.append(name: string; value: TBsonOID): Boolean;
begin
  if handle = nil then
    raise Exception.Create('BsonBuffer already finished');
  Result := (bson_append_oid(handle, PAnsiChar(System.UTF8Encode(name)), @value.value) = 0);
end;

function TBsonBuffer.append(name: string; value: TBsonCodeWScope): Boolean;
begin
  if handle = nil then
    raise Exception.Create('BsonBuffer already finished');
  Result := (bson_append_code_w_scope(handle, PAnsiChar(System.UTF8Encode(name)),
    PAnsiChar(System.UTF8Encode(value.code)), value.scope.getHandle) = 0);
end;

function TBsonBuffer.append(name: string; value: TBsonRegex): Boolean;
begin
  if handle = nil then
    raise Exception.Create('BsonBuffer already finished');
  Result := (bson_append_regex(handle, PAnsiChar(System.UTF8Encode(name)), PAnsiChar(System.UTF8Encode(value.pattern)),
    PAnsiChar(System.UTF8Encode(value.options))) = 0);
end;

function TBsonBuffer.append(name: string; value: TBsonTimestamp): Boolean;
begin
  if handle = nil then
    raise Exception.Create('BsonBuffer already finished');
  Result := (bson_append_timestamp2(handle, PAnsiChar(System.UTF8Encode(name)), Trunc((value.time - 25569) * 60 * 60 *
    24), value.increment) = 0);
end;

function TBsonBuffer.append(name: string; value: TBsonBinary): Boolean;
begin
  if handle = nil then
    raise Exception.Create('BsonBuffer already finished');
  Result := (bson_append_binary(handle, PAnsiChar(System.UTF8Encode(name)), value.kind, value.data, value.len) = 0);
end;

function TBsonBuffer.append(name: string; value: OleVariant): Boolean;
var
  d: double;
  LIntf: IInterface;
begin
  case VarType(value) of
    varNull: Result := appendNull(name);
    varInteger: Result := append(name, Integer(value));
    varSingle, varDouble, varCurrency:
      begin
        d := value;
        Result := append(name, OleVarToDouble(d));
      end;
    varDate: Result := append(name, FormatDateTime('yyyy-MM-dd', value)); //
    varInt64: Result := append(name, OleVarToInt64(value)); //BUG 不能超过 2^52-1
    varBoolean: Result := append(name, Boolean(value));
    varOleStr: Result := append(name, PAnsiChar(System.UTF8Encode(value)));
    varUnknown:
    begin
      LIntf := value;
      Result := append(name, LIntf as IBsonDocument);
    end;
  else
    raise Exception.Create('TBson.append(variant): type not supported (' + IntToStr(VarType(value)) + ')');
  end;
end;

function TBsonBuffer.appendNull(name: string): Boolean;
begin
  if handle = nil then
    raise Exception.Create('BsonBuffer already finished');
  Result := (bson_append_null(handle, PAnsiChar(System.UTF8Encode(name))) = 0);
end;

function TBsonBuffer.appendUndefined(name: string): Boolean;
begin
  if handle = nil then
    raise Exception.Create('BsonBuffer already finished');
  Result := (bson_append_undefined(handle, PAnsiChar(System.UTF8Encode(name))) = 0);
end;



function TBsonBuffer.appendBinary(name: string; kind: Integer; data: Pointer; length: Integer): Boolean;
begin
  if handle = nil then
    raise Exception.Create('BsonBuffer already finished');
  Result := (bson_append_binary(handle, PAnsiChar(System.UTF8Encode(name)), kind, data, length) = 0);
end;

function TBsonBuffer.append(name: string; value: IBsonDocument): Boolean;
begin
  Result := (bson_append_bson(handle, PAnsiChar(System.UTF8Encode(name)), value.getHandle) = 0);
end;

function TBsonBuffer.appendArray(name: string; value: array of Integer): Boolean;
var
  success: Boolean;
  i, len: Integer;
begin
  success := (bson_append_start_array(handle, PAnsiChar(System.UTF8Encode(name))) = 0);
  len := Length(value);
  i := 0;
  while success and (i < len) do
  begin
    success := (bson_append_int(handle, PAnsiChar(AnsiString(IntToStr(i))), value[i]) = 0);
    inc(i);
  end;
  if success then
    success := (bson_append_finish_object(handle) = 0);
  Result := success;
end;

function TBsonBuffer.appendArray(name: string; value: array of Double): Boolean;
var
  success: Boolean;
  i, len: Integer;
begin
  success := (bson_append_start_array(handle, PAnsiChar(System.UTF8Encode(name))) = 0);
  len := Length(value);
  i := 0;
  while success and (i < len) do
  begin
    success := (bson_append_double(handle, PAnsiChar(AnsiString(IntToStr(i))), value[i]) = 0);
    inc(i);
  end;
  if success then
    success := (bson_append_finish_object(handle) = 0);
  Result := success;
end;

function TBsonBuffer.appendArray(name: string; value: array of Boolean): Boolean;
var
  success: Boolean;
  i, len: Integer;
begin
  success := (bson_append_start_array(handle, PAnsiChar(System.UTF8Encode(name))) = 0);
  len := Length(value);
  i := 0;
  while success and (i < len) do
  begin
    success := (bson_append_bool(handle, PAnsiChar(AnsiString(IntToStr(i))), value[i]) = 0);
    inc(i);
  end;
  if success then
    success := (bson_append_finish_object(handle) = 0);
  Result := success;
end;

function TBsonBuffer.appendArray(name: string; value: array of string): Boolean;
var
  success: Boolean;
  i, len: Integer;
begin
  success := (bson_append_start_array(handle, PAnsiChar(System.UTF8Encode(name))) = 0);
  len := Length(value);
  i := 0;
  while success and (i < len) do
  begin
    success := (bson_append_string(handle, PAnsiChar(AnsiString(IntToStr(i))), PAnsiChar(System.UTF8Encode(value[i])))
      = 0);
    inc(i);
  end;
  if success then
    success := (bson_append_finish_object(handle) = 0);
  Result := success;
end;

function TBsonBuffer.appendArray(name: string;
  value: array of IBsonDocument): Boolean;
var
  success: Boolean;
  i, len: Integer;
begin
  success := (bson_append_start_array(handle, PAnsiChar(System.UTF8Encode(name))) = 0);
  len := Length(value);
  i := 0;
  while success and (i < len) do
  begin
    success := (bson_append_bson(handle, PAnsiChar(AnsiString(IntToStr(i))), value[i].getHandle)
      = 0);
    inc(i);
  end;
  if success then
    success := (bson_append_finish_object(handle) = 0);
  Result := success;
end;

function TBsonBuffer.appendArray(name: string;
  value: array of Variant): Boolean;
var
  i: Integer;
  isObject: Boolean;
  success: Boolean;
  len: Integer;


  function GetBsonArray(): TArray<IBsonDocument>;
  var
    LIntf: IInterface;
    i: Integer;
  begin
    SetLength(Result, Length(value));
    for i := Low(value) to High(value) do
    begin
      LIntf := value[i];
      Result[i] := LIntf as IBsonDocument;
    end;
  end;


begin
  isObject := False;
  for i := Low(value) to High(value) do
  begin
    if (VarType( value[i]) = varUnknown) then
    begin
      isObject := True;
      Break;
    end;
  end;

  if isObject then
  begin
    Result := appendArray(name, GetBsonArray());
  end
  else
  begin
    success := (bson_append_start_array(handle, PAnsiChar(System.UTF8Encode(name))) = 0);
    len := Length(value);
    i := 0;
    while success and (i < len) do
    begin
      case VarType(value[i]) of
        varString, varUString, varStrArg, varOleStr:
          success := (bson_append_string(handle, PAnsiChar(AnsiString(IntToStr(i))), PAnsiChar(System.UTF8Encode(value[i])))
            = 0);
        varInteger, varLongWord, varWord, varByte, varShortInt, varSmallint:
          success := (bson_append_int(handle, PAnsiChar(AnsiString(IntToStr(i))), value[i]) = 0);
        varInt64, varUInt64:
          success := (bson_append_long(handle, PAnsiChar(AnsiString(IntToStr(i))), value[i]) = 0);
        varDouble, varSingle, varCurrency:
          success := (bson_append_double(handle, PAnsiChar(AnsiString(IntToStr(i))), value[i]) = 0);
        varNull:
          success := (bson_append_null(handle, PAnsiChar(AnsiString(IntToStr(i)))) = 0);
        varBoolean:
          success := (bson_append_bool(handle, PAnsiChar(AnsiString(IntToStr(i))), value[i]) = 0);
        varDate:
          success := (bson_append_date(handle, PAnsiChar(AnsiString(IntToStr(i))), DateTimeToUnix(value[i])) = 0);
        else
          success := (bson_append_undefined(handle, PAnsiChar(AnsiString(IntToStr(i)))) = 0);
      end;
      inc(i);
    end;
    if success then
      success := (bson_append_finish_object(handle) = 0);
    Result := success;
  end;
end;

function TBsonBuffer.startObject(name: string): Boolean;
begin
  if handle = nil then
    raise Exception.Create('BsonBuffer already finished');
  Result := (bson_append_start_object(handle, PAnsiChar(System.UTF8Encode(name))) = 0);
end;

function TBsonBuffer.startArray(name: string): Boolean;
begin
  if handle = nil then
    raise Exception.Create('BsonBuffer already finished');
  Result := (bson_append_start_array(handle, PAnsiChar(System.UTF8Encode(name))) = 0);
end;

function TBsonBuffer.finishObject(): Boolean;
begin
  if handle = nil then
    raise Exception.Create('BsonBuffer already finished');
  Result := (bson_append_finish_object(handle) = 0);
end;

function TBsonBuffer.size(): Integer;
begin
  if handle = nil then
    raise Exception.Create('BsonBuffer already finished');
  Result := bson_buffer_size(handle);
end;

function TBsonBuffer.finish(): IBsonDocument;
begin
  if handle = nil then
    raise Exception.Create('BsonBuffer already finished');
  if bson_finish(handle) = 0 then
  begin
    Result := TBson.Create(handle);
    handle := nil;
  end
  else
    Result := nil;
end;

constructor TBson.Create();
begin
  inherited Create;
  handle := bson_create();
end;

constructor TBson.Create(AHandle: Pointer);
begin
  handle := AHandle;
end;

destructor TBson.Destroy();
begin
  if Assigned(FIterator) then
    FIterator.Free;
  bson_destroy(handle);
  bson_dispose(handle);
  handle := nil;
  inherited Destroy();
end;

procedure TBson.setItem(const Key: string; Value: OleVariant);
var
  LBuffer: TBsonBuffer;
begin
  LBuffer := TBsonBuffer.Create(handle);
  try
    LBuffer.append(Key, Value);
  finally
    LBuffer.Free;
  end;
end;

function TBson.size(): Integer;
begin
  Result := bson_size(handle);
end;

function TBson.find(name: string): TBsonIterator;
begin
  FIterator := iterator;
  Result := FIterator;
  if bson_find(Result.handle, handle, PAnsiChar(System.UTF8Encode(name))) = bsonEOO then
  begin
    Result := nil;
  end;
end;

procedure _display(i: TBsonIterator; depth: Integer);
var
  t: TBsonType;
  j, k: Integer;
  cws: TBsonCodeWScope;
  regex: TBsonRegex;
  ts: TBsonTimestamp;
  bin: TBsonBinary;
  p: PByte;
begin
  while i.next() do
  begin
    t := i.kind();
    if t = bsonEOO then
      break;
    for j := 1 to depth do
      Write('    ');
    Write(i.key, ' (', Ord(t), ') : ');
    case t of
      bsonDOUBLE,
        bsonSTRING, bsonSYMBOL, bsonCODE,
        bsonBOOL, bsonDATE, bsonINT, bsonLONG:
        Write(i.value);
      bsonUNDEFINED:
        Write('UNDEFINED');
      bsonNULL:
        Write('NULL');
      bsonOBJECT, bsonARRAY:
        begin
          Writeln;
          _display(i.subiterator, depth + 1);
        end;
      bsonOID: write(i.getOID().AsString());
      bsonCODEWSCOPE:
        begin
          Write('CODEWSCOPE ');
          cws := i.getCodeWScope();
          WriteLn(cws.code);
          _display(cws.scope.iterator, depth + 1);
        end;
      bsonREGEX:
        begin
          regex := i.getRegex();
          write(regex.pattern, ', ', regex.options);
        end;
      bsonTIMESTAMP:
        begin
          ts := i.getTimestamp();
          write(DateTimeToStr(ts.time), ' (', ts.increment, ')');
        end;
      bsonBINDATA:
        begin
          bin := i.getBinary();
          Write('BINARY (', bin.kind, ')');
          p := bin.data;
          for j := 0 to bin.len - 1 do
          begin
            if j and 15 = 0 then
            begin
              WriteLn;
              for k := 1 to depth + 1 do
                Write('    ');
            end;
            write(ByteToHex(p^), ' ');
            Inc(p);
          end;
        end;
    else
      Write('UNKNOWN');
    end;
    Writeln;
  end;
end;

function TBson.getHandle(): Pointer;
begin
  Result := handle;
end;

procedure TBson.display();
begin
  if Self = nil then
    WriteLn('nil BSON')
  else
    _display(iterator, 0);
end;

constructor TBsonCodeWScope.Create(code_: string; scope_: IBsonDocument);
begin
  code := code_;
  scope := scope_;
end;

constructor TBsonCodeWScope.Create(i: TBsonIterator);
var
  b, c: Pointer;
begin
  code := string(bson_iterator_code(i.handle));
  b := bson_create();
  bson_iterator_code_scope(i.handle, b);
  scope := TBson.Create();
  c := scope.getHandle();
  bson_copy(c, b);    
  bson_dispose(b);
end;

constructor TBsonRegex.Create(pattern_: string; options_: string);
begin
  pattern := pattern_;
  options := options_;
end;

constructor TBsonRegex.Create(i: TBsonIterator);
begin
  pattern := string(bson_iterator_regex(i.handle));
  options := string(bson_iterator_regex_opts(i.handle));
end;

constructor TBsonTimestamp.Create(time_: TDateTime; increment_: Integer);
begin
  time := time_;
  increment := increment_;
end;

constructor TBsonTimestamp.Create(i: TBsonIterator);
begin
  time := bson_iterator_timestamp_time(i.handle) / (60.0 * 60 * 24) + 25569;
  increment := bson_iterator_timestamp_increment(i.handle);
end;

constructor TBsonBinary.Create(p: Pointer; length: Integer);
begin
  GetMem(data, length);
  Move(p^, data^, length);
  kind := 0;
end;

constructor TBsonBinary.Create(i: TBsonIterator);
var
  p: Pointer;
begin
  kind := bson_iterator_bin_type(i.handle);
  len := bson_iterator_bin_len(i.handle);
  p := bson_iterator_bin_data(i.handle);
  GetMem(data, len);
  Move(p^, data^, len);
end;

destructor TBsonBinary.Destroy;
begin
  FreeMem(data);
end;

function ByteToHex(InByte: Byte): string;
const
  digits: array[0..15] of Char = '0123456789ABCDEF';
begin
  result := digits[InByte shr 4] + digits[InByte and $0F];
end;

function BsonFromString(const AJson: string): IBsonDocument;
var
  LBuffer: TBsonBuffer;
begin
  LBuffer := TBsonBuffer.Create;
  try
    LBuffer.appendUndefined(AJson);
    Result := LBuffer.finish;
  finally
    LBuffer.Free;
  end;
end;

function BSON(x: array of OleVariant): IBsonDocument;
var
  len: Integer;
  i: Integer;
  bb: TBsonBuffer;
  depth: Integer;
  key: string;
  value: string;
begin
  bb := TBsonBuffer.Create();
  try
    len := Length(x);
    i := 0;
    depth := 0;
    while i < len do
    begin
      key := VarToStr(x[i]);
      if key = '}' then
      begin
        if depth = 0 then
          raise Exception.Create('BSON(): unexpected "}"');
        bb.finishObject();
        dec(depth);
      end
      else
      begin
        inc(i);
        if i = Len then
          raise Exception.Create('BSON(): expected value for ' + key);
        if (VarType(x[i]) = varUnknown) then
        begin
          bb.append(key, x[i]);
        end
        else
        begin
          value := VarToStr(x[i]);
          if value = '{' then
          begin
            bb.startObject(key);
            inc(depth);
          end
          else
            bb.append(key, x[i]);
        end;
      end;
      inc(i);
    end;
    if depth > 0 then
      raise Exception.Create('BSON: open subobject');
    Result := bb.finish();
  finally
    bb.Free;
  end;
end;

function JsonToBson(const AJson: string): IBsonDocument;
var
  LSuperObject: ISuperObject;
  LBuff: TBsonBuffer;
  LSuperTable: TSuperTableString;
  LSuperArray: TSuperArray;
  LIter: TSuperAvlEntry;
  i: Integer;
  LArrayValues: array of Variant;
  LType: TSuperType;
begin
  Result := bsonEmpty;
  LBuff := TBsonBuffer.Create;
  try
    LSuperObject := SO(AJson);

    if LSuperObject.IsType(stObject) then
    begin
      LSuperTable := LSuperObject.AsObject;
      for LIter in LSuperTable do
      begin
        case LIter.Value.DataType of
          stNull: LBuff.appendNull(LIter.Name);
          stBoolean: LBuff.append(LIter.Name, LIter.Value.AsBoolean);
          stDouble: LBuff.appendDouble( LIter.Name, LIter.Value.AsDouble) ;
          stCurrency: LBuff.append( LIter.Name, LIter.Value.AsCurrency);
          stInt: LBuff.append( LIter.Name, LIter.Value.AsInteger);
          stObject:
          begin
            LBuff.append( LIter.Name, JsonToBson(LIter.Value.AsJSon()));
          end;
          stArray:
          begin
            LSuperArray := LIter.Value.AsArray;
            SetLength(LArrayValues, LSuperArray.Length);

            for i := 0 to LSuperArray.Length - 1 do
            begin
              LType := LSuperArray.O[i].DataType;
              case LType of
                stNull: LArrayValues[i] := Null;
                stBoolean: LArrayValues[i] := LSuperArray.O[i].AsBoolean;
                stDouble: LArrayValues[i] := LSuperArray.O[i].AsDouble;
                stCurrency: LArrayValues[i] := LSuperArray.O[i].AsCurrency;
                stInt: LArrayValues[i] := LSuperArray.O[i].AsInteger;
                stObject, stArray: LArrayValues[i] := JsonToBson(LSuperArray.O[i].AsJSon());
                stString, stMethod: LArrayValues[i] := LSuperArray.O[i].AsString;
              end;
            end;
            LBuff.appendArray(LIter.Name, LArrayValues);
          end;
          stString: LBuff.append(LIter.Name, AnsiString(UTF8Encode(LIter.Value.AsString)));
          //stMethod: LBuff.appendCode(LIter.Name, LIter.Value.AsString);
        end;
      end;
    end;
    Result := LBuff.finish;
  finally
    LBuff.Free;
  end; 
end;

procedure err_handler(msg: PAnsiChar);
begin
  raise Exception.Create(string(msg));
end;




initialization
//  bsonEmpty := BSON([]);
  set_bson_err_handler(Addr(err_handler));
end.


