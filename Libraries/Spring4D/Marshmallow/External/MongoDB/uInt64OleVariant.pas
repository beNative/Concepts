{
delphi7不支持olevariant转换到int64
所以要用转换到double类型保存的方式,
另外一种方式就是自己写函数实现补丁
}

unit uInt64OleVariant;

interface

uses ActiveX;

function Int64ToOleVar( value: Int64 ): OleVariant;
function  OleVarToInt64( value:OleVariant ): Int64 ;

function OleVarToDouble(value:OleVariant):Double;
function DoubleToOleVar(value:Double):OleVariant;

implementation

//Download by thtp://www.codefans.net

//将64位整数转换到olevariant
function Int64ToOleVar( value: Int64 ): OleVariant;
begin
  TVarData(Result).VType := vt_i8;
  TVarData(Result).VInt64:= value;
end;

//将olevariant转换到64位整数
function  OleVarToInt64( value:OleVariant ): Int64 ;
begin
  Result := 0;
  if TvarData(value).VType = VT_I8 then
  result := TVardata(value).VInt64;
end;

function OleVarToDouble(value:OleVariant):Double;
begin
  Result := 0;
 if TvarData(value).VType = VT_R8 then
  result := TVardata(value).VDouble;
end;

function DoubleToOleVar(value:Double):OleVariant;
begin
  TVarData(Result).VType := VT_R8;
  TVarData(Result).VDouble := value;
end;

end.
