unit bsonUtils;

interface

uses SysUtils, MongoBson;

function JsonToBson(const jsonData:WideString):IBSONDocument;
procedure JsonIntoBson(const jsonData:WideString;doc:IBSONDocument); overload;
procedure JsonIntoBson(const jsonData:WideString; doc:IBSONDocument;
  var EndIndex:integer); overload;

type
  EJsonDecodeException=class(Exception);
  EJsonEncodeException=class(Exception);

implementation

uses Classes, Variants;



function JsonToBson(const jsonData:WideString):IBSONDocument;
begin
  Result:=bsonEmpty;
  JsonIntoBson(jsonData,Result);
end;

procedure JsonIntoBson(const jsonData:WideString;doc:IBSONDocument);
var
  i,l:integer;
begin
  JsonIntoBson(jsonData,doc,i);
  //check only whitespace left
  l:=Length(jsonData);
  while (i<=l) and (jsonData[i]<=' ') do inc(i);
  if (i<=l) then raise EJsonDecodeException.Create(
    'JSON with unexpected data past end #'+IntToStr(i));
end;

procedure JsonIntoBson(const jsonData:WideString; doc:IBSONDocument;
  var EndIndex:integer);
var
  i,l:integer;
  function SkipWhiteSpace:WideChar;
  begin
    while (i<=l) and (jsonData[i]<=' ') do inc(i);
    if i<=l then Result:=jsonData[i] else Result:=#0;
  end;
  function ExVicinity:WideString;
  const
    VicinityExtent=8;
  begin
    if i<=VicinityExtent then
      Result:=#13#10'(#'+IntToStr(i)+')"'+Copy(jsonData,1,i-1)+
        ' >>> '+jsonData[i]+' <<< '+Copy(jsonData,i+1,VicinityExtent)+'"'
    else
      Result:=#13#10'(#'+IntToStr(i)+')"...'+Copy(jsonData,i-VicinityExtent-1,VicinityExtent)+
        ' >>> '+jsonData[i]+' <<< '+Copy(jsonData,i+1,VicinityExtent)+'"';
  end;
  procedure Expect(c:WideChar;const msg:string);
  begin
    while (i<=l) and (jsonData[i]<=' ') do inc(i);
    if (i>l) or (jsonData[i]<>c) then
      raise EJsonDecodeException.Create(msg+ExVicinity);
    inc(i);
  end;
  procedure GetStringIndexes(var i1,i2:integer);
  begin
    inc(i);
    i1:=i;
    while (i<=l) and (jsonData[i]<>'"') do
     begin
      if jsonData[i]='\' then inc(i);//just skip all to skip any '"'
      inc(i);
     end;
    i2:=i;
    inc(i);
  end;
  function GetStringValue(i1,i2:integer):WideString;
  var
    ii,di,u,v,w:integer;
  begin
    //assert jsonData[i1-1]='"'
    //assert jsonData[i2]='"';
    SetLength(Result,i2-i1);
    ii:=1;
    di:=i1;
    while di<i2 do
     begin
      //assert ii<=Length(Result);
      if jsonData[di]='\' then
       begin
        inc(di);
        case char(jsonData[di]) of
          '"','\','/':Result[ii]:=jsonData[di];
          'b':Result[ii]:=#8;
          't':Result[ii]:=#9;
          'n':Result[ii]:=#10;
          'f':Result[ii]:=#12;
          'r':Result[ii]:=#13;
          'u':
           begin
            w:=0;
            for u:=0 to 3 do
             begin
              inc(di);
              v:=word(jsonData[di]);
              case v of
                $30..$39:w:=(w shl 4) or (v and $F);
                $41..$5A,$61..$7A:w:=(w shl 4) or ((v and $1F)+9);
                else raise EJsonDecodeException.Create(
                  'JSON Invalid espace sequence'+ExVicinity);
              end;
             end;
            Result[ii]:=WideChar(w);
           end;
          else raise EJsonDecodeException.Create(
            'JSON Unknown escape sequence'+ExVicinity);
        end;
       end
      else
        Result[ii]:=jsonData[di];
      inc(di);
      inc(ii);
     end;
    SetLength(Result,ii-1);
  end;
const
  stackGrowStep=$20;//not too much, not too little (?)
  arrGrowStep=$20;
var
  InObjectOrArray:boolean;
  k1,k2,v1,v2,a1,ai,al:integer;
  d:IBSONDocument;
  a:array of OleVariant;
  at:TVarType;
  procedure SetValue(v:OleVariant);
  begin
    if InObjectOrArray then
      d[GetStringValue(k1,k2)]:=v
    else
     begin
      if ai=al then
       begin
        inc(al,arrGrowStep);//not too much, not too little (?)
        SetLength(a,al);
       end;
      a[ai]:=v;
      //assert (VarType(v) and varArray)=0
      //detect same type elements array
      if at=varEmpty then at:=VarType(v) else
        case at of
          //TODO: what with signed/unsigned mixed?
          varSmallint://i2
            if not(VarType(v) in [varSmallint,
              varShortInt,varByte]) then at:=varVariant;
          varInteger://i4
            if not(VarType(v) in [varSmallint,
              varInteger,varShortInt,varByte,varWord]) then at:=varVariant;
          varWord:
            if not(VarType(v) in [varSmallint,
              varByte,varWord]) then at:=varVariant;
          varLongWord:
            if not(VarType(v) in [varSmallint,
              varShortInt,varByte,varWord,varLongWord]) then at:=varVariant;
          varInt64:
            if not(VarType(v) in [varSmallint,varInteger,varShortInt,
              varByte,varWord,varLongWord,varInt64]) then at:=varVariant;
          varVariant:;//Already creating an VarArray of variants
          //TODO: more?
          else if at<>VarType(v) then at:=varVariant;
        end;
      inc(ai);
     end;
  end;
  function GetArrayValue:OleVariant;
  var
    ii,jj:integer;
  begin
    if at=varEmpty then at:=varVariant;//empty array!
    Result:=VarArrayCreate([0,ai-a1-1],at);
    ii:=a1;
    jj:=0;
    while ii<ai do
     begin
      Result[jj]:=a[ii];
      VarClear(a[ii]);
      inc(ii);
      inc(jj);
     end;
    ai:=a1;
  end;
var
  firstItem,b:boolean;
  stack:array of record
    k1,k2:integer;
    d:IBSONDocument;
  end;
  stackIndex,stackSize:integer;
  ods:char;
  key:WideString;
  v:OleVariant;
  v64:int64;
begin
  //doc.Clear;? let caller decide.
  i:=1;
  l:=Length(jsonData);
  //object starts
  Expect('{','JSON doesn''t define an object, "{" expected.');
  stackSize:=0;
  stackIndex:=0;
  ai:=0;
  al:=0;
  InObjectOrArray:=true;
  firstItem:=true;
  ods:=FormatSettings.DecimalSeparator;
  try
    FormatSettings.DecimalSeparator:='.';
    d:=doc;
    //main loop over key/values and nested objects/arrays
    while (i<=l) and (stackIndex<>-1) do
     begin
      if firstItem then firstItem:=false else
        Expect(',','JSON element not delimited by comma');
      if InObjectOrArray and (SkipWhiteSpace<>'}') then
       begin
        //key string
        {$IFDEF BSINUTILS_JSON_STRICT}
        Expect('"','JSON key string not enclosed in double quotes');
        GetStringIndexes(k1,k2);
        {$ELSE}
        if SkipWhiteSpace='"' then GetStringIndexes(k1,k2) else
         begin
          k1:=i;
          while (i<=l) and (jsonData[i]>' ') and
            (jsonData[i]<>':') and (jsonData[i]<>'"') do inc(i);
          k2:=i;
         end;
        {$ENDIF}
        Expect(':','JSON key, value not separated by colon');
       end;
      //value
      case char(SkipWhiteSpace) of
        '{','['://object or array
         begin
          b:=InObjectOrArray;
          if jsonData[i]='{' then
           begin
            //an object starts
            if InObjectOrArray then
             begin
              key:=GetStringValue(k1,k2);
              v:=d[key];//re-use existing?
              if not(VarType(v) in [varDispatch,varUnknown]) or
                (IUnknown(v).QueryInterface(IID_IBSONDocument,d)<>S_OK) then
               begin
                v:=bsonEmpty;
                d[key]:=v;
               end;
             end
            else
             begin
              //TODO: re-use BSON docs in array?
              if ai=al then
               begin
                inc(al,arrGrowStep);//not too much, not too little (?)
                SetLength(a,al);
               end;
              v:=bsonEmpty;
              a[ai]:=v;
              //detect same type elements array
              if at=varEmpty then at:=varUnknown else
                if at<>varUnknown then at:=varVariant;
              inc(ai);
             end;
            InObjectOrArray:=true;
           end
          else
            InObjectOrArray:=false;
          inc(i);
          //push onto stack
          if stackIndex=stackSize then
           begin
            inc(stackSize,stackGrowStep);
            SetLength(stack,stackSize);
           end;
          if b then //was InObjectOrArray?
           begin
            stack[stackIndex].k1:=k1;
            stack[stackIndex].k2:=k2;
            stack[stackIndex].d:=d;
           end
          else
           begin
            stack[stackIndex].k1:=a1;
            stack[stackIndex].k2:=at;
            stack[stackIndex].d:=nil;
           end;
          inc(stackIndex);
          firstItem:=true;
          if InObjectOrArray then
            d:=IUnknown(v) as IBSONDocument
          else
           begin
            a1:=ai;
            at:=varEmpty;//used to detect same type elements array
           end;
         end;

        '}',']':;//empty object or array, drop into close array below

        '"'://string
         begin
          GetStringIndexes(v1,v2);
          SetValue(GetStringValue(v1,v2));
         end;

        '0'..'9','-'://number
         begin
          b:=jsonData[i]='-';
          v1:=i;
          if b then inc(i);
          v64:=0;
          while (i<=l) and (CharInSet(char(jsonData[i]), ['0'..'9'])) do
           begin
            v64:=v64*10+(word(jsonData[i]) and $F);//TODO: detect overflow
            inc(i);
           end;
          if CharInSet(char(jsonData[i]), ['.','e','E']) then
           begin
            //float
            inc(i);
            while (i<=l) and (CharInSet(char(jsonData[i]),
              ['0'..'9','-','+','e','E'])) do inc(i);
            //try except EConvertError?
            SetValue(StrToFloat(Copy(jsonData,v1,i-v1)));
           end
          else
           begin
            //integer
            if v64>=$80000000 then //int64
              if b then SetValue(-v64) else SetValue(v64)
            else if v64>=$80 then //int32
              if b then SetValue(-integer(v64)) else SetValue(integer(v64))
            else //int8
              if b then SetValue(-SmallInt(v64)) else SetValue(SmallInt(v64));
           end;
         end;

        't'://true
         begin
          inc(i);
          Expect('r','JSON true misspelled');
          Expect('u','JSON true misspelled');
          Expect('e','JSON true misspelled');
          SetValue(true);
         end;
        'f'://false
         begin
          inc(i);
          Expect('a','JSON false misspelled');
          Expect('l','JSON false misspelled');
          Expect('s','JSON false misspelled');
          Expect('e','JSON false misspelled');
          SetValue(false);
         end;
        'n'://null
         begin
          inc(i);
          Expect('u','JSON null misspelled');
          Expect('l','JSON null misspelled');
          Expect('l','JSON null misspelled');
          SetValue(Null);
         end;

        else raise EJsonDecodeException.Create(
          'JSON Unrecognized value type'+ExVicinity);
      end;
      if not firstItem then
       begin
        b:=true;
        while b do
         begin
          v:=Null;
          if InObjectOrArray then
            b:=SkipWhiteSpace='}'
          else
            if SkipWhiteSpace=']' then
              v:=GetArrayValue
            else
              b:=false;
          if b then
           begin
            inc(i);
            //pop from stack
            if stackIndex=0 then
             begin
              EndIndex:=i;
              dec(stackIndex);
              b:=false;
             end
            else
             begin
              dec(stackIndex);
              if stack[stackIndex].d=nil then
               begin
                a1:=stack[stackIndex].k1;
                at:=stack[stackIndex].k2;
                InObjectOrArray:=false;
               end
              else
               begin
                d:=stack[stackIndex].d;
                k1:=stack[stackIndex].k1;
                k2:=stack[stackIndex].k2;
                stack[stackIndex].d:=nil;
                InObjectOrArray:=true;
               end;
             end;
            //set array
            if not VarIsNull(v) then SetValue(v);
           end;
         end;
       end;
     end;
    if stackIndex<>-1 then raise EJsonDecodeException.Create(
      'JSON with '+IntToStr(stackIndex+1)+' objects or arrays not closed');
  finally
    FormatSettings.DecimalSeparator:=ods;
  end;
end;

end.
