(*
  Copyright (c) 2011, Stefan Glienke
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  - Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.
  - Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.
  - Neither the name of this library nor the names of its contributors may be
    used to endorse or promote products derived from this software without
    specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
*)

unit DSharp.Core.Expressions;

interface

uses
  Classes,
  Rtti,
  SysUtils;

type
  TCompiledExpression = TFunc<TValue>;
  TCompiledExpressionArray = TArray<TCompiledExpression>;
  TConvertFunc = reference to function(const Value: TValue): TValue;

  IExpression = interface
    ['{B6748486-F9E5-40E3-92E7-9DC2C5BFB3F7}']
    function Compile: TCompiledExpression;
    function Execute: TValue;
    function ToString: string;
  end;

  TExpressionArray = TArray<IExpression>;

  IConstantExpression = interface(IExpression)
    ['{4138745A-9BC4-4BF5-9C7D-992664AEDEB7}']
    function GetValue: TValue;
    property Value: TValue read GetValue;
  end;

  IDefaultExpression = interface(IConstantExpression)
    ['{B583DD48-1CC5-4990-A3B2-ABDC7DFCD873}']
  end;

  IValueExpression = interface(IConstantExpression)
    ['{C428D2D2-1BCF-4CC1-B7C6-71989E6055EA}']
    procedure SetValue(const Value: TValue);
    property Value: TValue read GetValue write SetValue;
  end;

  IParameterExpression = interface(IValueExpression)
    ['{F76C9ED9-DB2E-4068-A8C7-662027DC6300}']
    function GetName: string;
    property Name: string read GetName;
  end;

  IUnaryExpression = interface(IExpression)
    ['{B31C60DD-0049-4499-9051-AA6895C3C676}']
    function GetExpression: IExpression;
    property Expression: IExpression read GetExpression;
  end;

  IBinaryExpression = interface(IExpression)
    ['{50331506-E22A-4A82-880E-DA0F75055634}']
    function GetLeft: IExpression;
    function GetRight: IExpression;
    property Left: IExpression read GetLeft;
    property Right: IExpression read GetRight;
  end;

  IConditionalExpression = interface(IExpression)
    ['{F6D4F83A-0830-42D1-A23F-3C18B1D72F64}']
    function GetIfFalse: IExpression;
    function GetIfTrue: IExpression;
    function GetTest: IExpression;
    property IfFalse: IExpression read GetIfFalse;
    property IfTrue: IExpression read GetIfTrue;
    property Test: IExpression read GetTest;
  end;

  IBlockExpression = interface(IExpression)
    ['{9C9D1F1E-1F99-4671-933A-DDC0266C1091}']
    function GetExpressions: TExpressionArray;
    property Expressions: TExpressionArray read GetExpressions;
  end;

  IMemberExpression = interface(IParameterExpression)
    ['{88F4CEFF-7C9B-4815-BD77-9BBDEDFA3693}']
    function GetExpression: IExpression;
    function GetInstance: TValue;
    function GetMember: TRttiMember;
    procedure SetInstance(const Value: TValue);
    property Expression: IExpression read GetExpression;
    property Instance: TValue read GetInstance write SetInstance;
    property Member: TRttiMember read GetMember;
  end;

  Expression = record
  public
    class function Add(Left, Right: IExpression): IBinaryExpression; static;
    class function AddAssign(Left, Right: IExpression): IBinaryExpression; static;
    class function Assign(Left, Right: IExpression): IBinaryExpression; static;
    class function Block(Expressions: array of IExpression): IBlockExpression; static;
    class function Break: IExpression; static;
    class function Condition(Test, IfTrue, IfFalse: IExpression): IConditionalExpression; static;
    class function Constant<T>(Value: T): IConstantExpression; static;
    class function Convert(Left, Right: IExpression; ConvertFunc: TConvertFunc = nil): IBinaryExpression; static;
    class function Default(TypeInfo: Pointer = nil): IDefaultExpression; static;
    class function Divide(Left, Right: IExpression): IBinaryExpression; static;
    class function DivideAssign(Left, Right: IExpression): IBinaryExpression; static;
    class function Empty: IDefaultExpression; static;
    class function Equal(Left, Right: IExpression): IBinaryExpression; static;
//    class function FieldAccess(Expression: IExpression; const Name: string): IMemberExpression; static;
    class function GreaterThan(Left, Right: IExpression): IBinaryExpression; static;
    class function GreaterThanOrEqual(Left, Right: IExpression): IBinaryExpression; static;
    class function IfThen(Test, IfTrue: IExpression): IConditionalExpression; static;
    class function IfThenElse(Test, IfTrue, IfFalse: IExpression): IConditionalExpression; static;
    class function IsFalse(Expression: IExpression): IUnaryExpression; static;
    class function IsTrue(Expression: IExpression): IUnaryExpression; static;
    class function LessThan(Left, Right: IExpression): IBinaryExpression; static;
    class function LessThanOrEqual(Left, Right: IExpression): IBinaryExpression; static;
    class function LogicalAnd(Left, Right: IExpression): IBinaryExpression; static;
    class function LogicalAndAssign(Left, Right: IExpression): IBinaryExpression; static;
    class function LogicalExclusiveOr(Left, Right: IExpression): IBinaryExpression; static;
    class function LogicalExclusiveOrAssign(Left, Right: IExpression): IBinaryExpression; static;
    class function LogicalOr(Left, Right: IExpression): IBinaryExpression; static;
    class function LogicalOrAssign(Left, Right: IExpression): IBinaryExpression; static;
    class function LogicalNot(Expression: IExpression): IUnaryExpression; static;
    class function LogicalNotAssign(Expression: IExpression): IBinaryExpression; static;
    class function Loop(Expression: IExpression): IUnaryExpression; static;
    class function MethodCall(Expression: IExpression; const Name: string;
      const Arguments: array of IExpression): IMemberExpression; static;
    class function Modulo(Left, Right: IExpression): IBinaryExpression; static;
    class function ModuloAssign(Left, Right: IExpression): IBinaryExpression; static;
    class function Multiply(Left, Right: IExpression): IBinaryExpression; static;
    class function MultiplyAssign(Left, Right: IExpression): IBinaryExpression; static;
    class function Negate(Expression: IExpression): IUnaryExpression; static;
    class function Parameter(const Name: string = ''): IParameterExpression; static;
    class function PropertyAccess(Expression: IExpression; const Name: string): IMemberExpression; static;
    class function Subtract(Left, Right: IExpression): IBinaryExpression; static;
    class function SubtractAssign(Left, Right: IExpression): IBinaryExpression; static;
    class function Variable(const Name: string = ''): IParameterExpression; static;
  end;

  TBooleanExpression = record
  private
    FExpression: IExpression;
  public
    constructor Create(Expression: IExpression);
    class operator Equal(const Left, Right: TBooleanExpression): TBooleanExpression;
    class operator LogicalAnd(const Left, Right: TBooleanExpression): TBooleanExpression;
    class operator LogicalOr(const Left, Right: TBooleanExpression): TBooleanExpression;
    class operator LogicalNot(const Value: TBooleanExpression): TBooleanExpression;
    class operator LogicalXor(const Left, Right: TBooleanExpression): TBooleanExpression;
    class operator Negative(const Value: TBooleanExpression): TBooleanExpression;
    class operator NotEqual(const Left, Right: TBooleanExpression): TBooleanExpression;
    class operator Implicit(const Value: Boolean): TBooleanExpression;
    class operator Implicit(const Value: TBooleanExpression): Boolean;
    class operator Implicit(const Value: TBooleanExpression): TFunc<Boolean>;
    class operator Implicit(const Value: TBooleanExpression): TFunc<TObject, Boolean>;
    class operator Implicit(const Value: TBooleanExpression): IExpression;
    function Compile: TCompiledExpression;
    function Execute: TValue;
    function ToString: string;
  end;

  TValueExpression = record
  private
    FExpression: IExpression;
  public
    class operator Add(const Left, Right: TValueExpression): TValueExpression;
    class operator Subtract(const Left, Right: TValueExpression): TValueExpression;
    class operator Multiply(const Left, Right: TValueExpression): TValueExpression;
    class operator IntDivide(const Left, Right: TValueExpression): TValueExpression;
    class operator Divide(const Left, Right: TValueExpression): TValueExpression;
    class operator Modulus(const Left, Right: TValueExpression): TValueExpression;
    class operator Negative(const Value: TValueExpression): TValueExpression;
    class operator Equal(const Left, Right: TValueExpression): TBooleanExpression;
    class operator NotEqual(const Left, Right: TValueExpression): TBooleanExpression;
    class operator GreaterThan(const Left, Right: TValueExpression): TBooleanExpression;
    class operator GreaterThanOrEqual(const Left, Right: TValueExpression): TBooleanExpression;
    class operator LessThan(const Left, Right: TValueExpression): TBooleanExpression;
    class operator LessThanOrEqual(const Left, Right: TValueExpression): TBooleanExpression;
    class operator Implicit(const Value: Extended): TValueExpression;
    class operator Implicit(const Value: Integer): TValueExpression;
    class operator Implicit(const Value: TBooleanExpression): TValueExpression;
    class operator Implicit(const Value: Variant): TValueExpression;
    class operator Implicit(const Value: string): TValueExpression;
    class operator Implicit(const Value: TValueExpression): Extended;
    class operator Implicit(const Value: TValueExpression): Int64;
    class operator Implicit(const Value: TValueExpression): IExpression;
    function Compile: TCompiledExpression;
    function Execute: TValue;
    function ToString: string;
  end;

  TExpression = class abstract(TInterfacedObject, IExpression)
  protected
    function CompileExpression: TCompiledExpression; virtual; abstract;
  public
    function Compile: TCompiledExpression;
    function Execute: TValue;
  end;

  TConstantExpression = class(TExpression, IConstantExpression)
  private
    FValue: TValue;
    function GetValue: TValue;
  protected
    function CompileExpression: TCompiledExpression; override;
  public
    constructor Create(const Value: TObject); overload;
    constructor Create(const Value: TValue); overload;
    constructor Create(const Value: Variant); overload;
    class function Create<T>(const Value: T): TConstantExpression; overload;
    function ToString: string; override;
    property Value: TValue read GetValue;
  end;

  TDefaultExpression = class(TConstantExpression, IDefaultExpression)
  public
    constructor Create(TypeInfo: Pointer);
  end;

  TParameterExpression = class(TConstantExpression, IParameterExpression, IValueExpression)
  private
    FName: string;
    function GetName: string;
    procedure SetValue(const Value: TValue);
  public
    constructor Create(const AName: string);
    function ToString: string; override;
    property Name: string read GetName;
  end;

  TComponentExpression = class(TConstantExpression)
  protected
    function CompileExpression: TCompiledExpression; override;
  public
    constructor Create(const Value: TComponent); overload;
    function ToString: string; override;
  end;

  TUnaryExpression = class(TExpression, IUnaryExpression)
  private
    FExpression: IExpression;
    function GetExpression: IExpression;
  protected
    function CompileExpression: TCompiledExpression; override;
  public
    constructor Create(Expression: IExpression);
    property Expression: IExpression read GetExpression;
  end;

  TNegativeExpression = class(TUnaryExpression)
  protected
    function CompileExpression: TCompiledExpression; override;
  public
    function ToString: string; override;
  end;

  TBinaryExpression = class abstract(TExpression, IBinaryExpression)
  private
    FLeft: IExpression;
    FRight: IExpression;
    function GetLeft: IExpression;
    function GetRight: IExpression;
  protected
    function BuildString(const OperatorString: string): string;
  public
    constructor Create(Left, Right: IExpression);
    property Left: IExpression read GetLeft;
    property Right: IExpression read GetRight;
  end;

  TBinaryExpressionClass = class of TBinaryExpression;

  TAdditionExpression = class(TBinaryExpression)
  protected
    function CompileExpression: TCompiledExpression; override;
  public
    function ToString: string; override;
  end;

  TSubtractionExpression = class(TBinaryExpression)
  protected
    function CompileExpression: TCompiledExpression; override;
  public
    function ToString: string; override;
  end;

  TMultiplicationExpression = class(TBinaryExpression)
  protected
    function CompileExpression: TCompiledExpression; override;
  public
    function ToString: string; override;
  end;

  TDivisionExpression = class(TBinaryExpression)
  protected
    function CompileExpression: TCompiledExpression; override;
  public
    function ToString: string; override;
  end;

  TIntegerDivisionExpression = class(TBinaryExpression)
  protected
    function CompileExpression: TCompiledExpression; override;
  public
    function ToString: string; override;
  end;

  TModulusExpression = class(TBinaryExpression)
  protected
    function CompileExpression: TCompiledExpression; override;
  public
    function ToString: string; override;
  end;

  TLogicalAndExpression = class(TBinaryExpression)
  protected
    function CompileExpression: TCompiledExpression; override;
  public
    function ToString: string; override;
  end;

  TLogicalExclusiveOrExpression = class(TBinaryExpression)
  protected
    function CompileExpression: TCompiledExpression; override;
  public
    function ToString: string; override;
  end;

  TLogicalOrExpression = class(TBinaryExpression)
  protected
    function CompileExpression: TCompiledExpression; override;
  public
    function ToString: string; override;
  end;

  TLogicalNotExpression = class(TUnaryExpression)
  protected
    function CompileExpression: TCompiledExpression; override;
  public
    function ToString: string; override;
  end;

  TEqualExpression = class(TBinaryExpression)
  protected
    function CompileExpression: TCompiledExpression; override;
  public
    function ToString: string; override;
  end;

  TNotEqualExpression = class(TBinaryExpression)
  protected
    function CompileExpression: TCompiledExpression; override;
  public
    function ToString: string; override;
  end;

  TLessThanExpression = class(TBinaryExpression)
  protected
    function CompileExpression: TCompiledExpression; override;
  public
    function ToString: string; override;
  end;

  TLessThanOrEqualExpression = class(TBinaryExpression)
  protected
    function CompileExpression: TCompiledExpression; override;
  public
    function ToString: string; override;
  end;

  TGreaterThanExpression = class(TBinaryExpression)
  protected
    function CompileExpression: TCompiledExpression; override;
  public
    function ToString: string; override;
  end;

  TGreaterThanOrEqualExpression = class(TBinaryExpression)
  protected
    function CompileExpression: TCompiledExpression; override;
  public
    function ToString: string; override;
  end;

  TAssignExpression = class(TBinaryExpression)
  protected
    function CompileExpression: TCompiledExpression; override;
  public
    function ToString: string; override;
  end;

  TConvertExpression = class(TAssignExpression)
  private
    FConvertFunc: TConvertFunc;
  protected
    function CompileExpression: TCompiledExpression; override;
  public
    constructor Create(Left, Right: IExpression; ConvertFunc: TConvertFunc = nil);
  end;

  TConditionalExpression = class(TExpression, IConditionalExpression)
  private
    FTest: IExpression;
    FIfTrue: IExpression;
    FIfFalse: IExpression;
    function GetIfFalse: IExpression;
    function GetIfTrue: IExpression;
    function GetTest: IExpression;
  protected
    function CompileExpression: TCompiledExpression; override;
  public
    constructor Create(ATest, AIfTrue, AIfFalse: IExpression);
    function ToString: string; override;
    property IfTrue: IExpression read GetIfTrue;
    property IfFalse: IExpression read GetIfFalse;
    property Test: IExpression read GetTest;
  end;

  TBlockExpression = class(TExpression, IBlockExpression)
  private
    FExpressions: TExpressionArray;
    function GetExpressions: TExpressionArray;
  protected
    function CompileExpression: TCompiledExpression; override;
  public
    constructor Create(Expressions: array of IExpression);
    function ToString: string; override;
    property Expressions: TExpressionArray read GetExpressions;
  end;

  TMemberExpression = class abstract(TExpression, IMemberExpression)
  private
    FExpression: IExpression;
    FName: string;
    function GetExpression: IExpression;
    function GetInstance: TValue;
    function GetName: string;
    procedure SetInstance(const Value: TValue);
  protected
    function GetMember: TRttiMember; virtual;
    function GetValue: TValue; virtual;
    procedure SetValue(const Value: TValue); virtual;
  public
    property Expression: IExpression read FExpression;
    property Member: TRttiMember read GetMember;
    property Name: string read GetName;
    property Value: TValue read GetValue write SetValue;
  end;

  TPropertyExpression = class(TMemberExpression, IValueExpression)
  private
    FIndex: Integer;
    FProperty: TRttiProperty;
    FPropertyName: string;
    function GetIndexedValue(const Instance: TValue): TValue;
    procedure SetIndexedValue(const Instance, Value: TValue);
  protected
    function CompileExpression: TCompiledExpression; override;
    function GetMember: TRttiMember; override;
    function GetValue: TValue; override;
    procedure SetValue(const Value: TValue); override;
  public
    constructor Create(AExpression: IExpression; const APropertyName: string); overload;
    constructor Create(AComponent: TComponent; const APropertyName: string); overload;
    constructor Create(AObject: TObject; const APropertyName: string); overload;
    function ToString: string; override;
  end;

  TMethodExpression = class(TMemberExpression)
  private
    FParameters: TExpressionArray;
    function CompileArguments: TCompiledExpressionArray;
    function GetMethod: TRttiMethod;
  protected
    function CompileExpression: TCompiledExpression; override;
    function GetMember: TRttiMember; override;
  public
    constructor Create(AExpression: IExpression; const AMethodName: string;
      const AParameters: array of IExpression); overload;
    constructor Create(AComponent: TComponent; const AMethodName: string;
      const AParameters: array of IExpression); overload;
    constructor Create(AObject: TObject; const AMethodName: string;
      const AParameters: array of IExpression); overload;
    function ToString: string; override;
  end;

  TLoopExpression = class(TUnaryExpression)
  protected
    function CompileExpression: TCompiledExpression; override;
  public
    function ToString: string; override;
  end;

  TBreakExpression = class(TExpression)
  protected
    function CompileExpression: TCompiledExpression; override;
  public
    function ToString: string; override;
  end;

//function AsBoolean(const Value: Boolean): TBooleanExpression;
//function ExpressionParams: TList<IValueExpression>;
//function ExpressionStack: TStack<IExpression>;
//procedure ForceStack;

implementation

uses
  DSharp.Core.Reflection,
  Generics.Collections,
  Spring.Collections,
  Spring,
  Spring.Reflection,
  StrUtils,
  TypInfo;

type
  EBreak = class(EAbort);

type
  ExpressionManager = record
  class threadvar
    FForceStack: Boolean;
    FIndentLevel: Integer;
    FParams: TList<IValueExpression>;
    FStack: TStack<IExpression>;
  class var
    FManagedObjects: TList<TObject>;
    FUseParentheses: Boolean;
    class function GetForceStack: Boolean; static;
    class function GetIndentLevel: Integer; static;
    class function GetParams: TList<IValueExpression>; static;
    class function GetStack: TStack<IExpression>; static;
  public
    class constructor Create;
    class destructor Destroy;

    class procedure Indent; static;
    class procedure Unindent; static;

    class property ForceStack: Boolean read GetForceStack;
    class property IndentLevel: Integer read GetIndentLevel;
    class property Params: TList<IValueExpression> read GetParams;
    class property Stack: TStack<IExpression> read GetStack;
    class property UseParentheses: Boolean read FUseParentheses write FUseParentheses;
  end;

//function AsBoolean(const Value: Boolean): TBooleanExpression;
//begin
//  Result := Value;
//end;
//
function ExpressionParams: TList<IValueExpression>;
begin
  Result := ExpressionManager.Params;
end;
//
function ExpressionStack: TStack<IExpression>;
begin
  Result := ExpressionManager.Stack;
end;
//
//procedure ForceStack;
//begin
//  ExpressionManager.FForceStack := True;
//end;

function GetIndentation: string;
begin
  Result := DupeString('  ', ExpressionManager.IndentLevel);
end;

{ ExpressionManager }

class constructor ExpressionManager.Create;
begin
  FManagedObjects := TObjectList<TObject>.Create();
end;

class destructor ExpressionManager.Destroy;
begin
  FManagedObjects.Free;
end;

class function ExpressionManager.GetForceStack: Boolean;
begin
  Result := FForceStack;
  FForceStack := False;
end;

class function ExpressionManager.GetIndentLevel: Integer;
begin
  Result := FIndentLevel;
end;

class function ExpressionManager.GetParams: TList<IValueExpression>;
begin
  if not Assigned(FParams) then
  begin
    FParams := TList<IValueExpression>.Create();
    FParams.Add(TParameterExpression.Create('Arg1'));
    FParams.Add(TParameterExpression.Create('Arg2'));
    FParams.Add(TParameterExpression.Create('Arg3'));
    FParams.Add(TParameterExpression.Create('Arg4'));
    FManagedObjects.Add(FParams);
  end;
  Result := FParams;
end;

class function ExpressionManager.GetStack: TStack<IExpression>;
begin
  if not Assigned(FStack) then
  begin
    FStack := TStack<IExpression>.Create();
    FManagedObjects.Add(FStack);
  end;
  Result := FStack;
end;

class procedure ExpressionManager.Indent;
begin
  Inc(FIndentLevel);
end;

class procedure ExpressionManager.Unindent;
begin
  Dec(FIndentLevel);
end;

{ Expression }

class function Expression.Add(Left, Right: IExpression): IBinaryExpression;
begin
  Result := TAdditionExpression.Create(Left, Right);
end;

class function Expression.AddAssign(Left, Right: IExpression): IBinaryExpression;
begin
  Result := Assign(Left, Add(Left, Right));
end;

class function Expression.Assign(Left, Right: IExpression): IBinaryExpression;
begin
  Result := TAssignExpression.Create(Left, Right);
end;

class function Expression.Block(Expressions: array of IExpression): IBlockExpression;
begin
  Result := TBlockExpression.Create(Expressions);
end;

class function Expression.Break: IExpression;
begin
  Result := TBreakExpression.Create();
end;

class function Expression.Condition(Test, IfTrue, IfFalse: IExpression): IConditionalExpression;
begin
  Result := TConditionalExpression.Create(Test, IfTrue, IfFalse);
end;

class function Expression.Constant<T>(Value: T): IConstantExpression;
begin
  Result := TConstantExpression.Create<T>(Value);
end;

class function Expression.Convert(Left, Right: IExpression; ConvertFunc: TConvertFunc = nil): IBinaryExpression;
begin
  Result := TConvertExpression.Create(Left, Right, ConvertFunc);
end;

class function Expression.Default(TypeInfo: Pointer): IDefaultExpression;
begin
  Result := TDefaultExpression.Create(TypeInfo);
end;

class function Expression.Divide(Left, Right: IExpression): IBinaryExpression;
begin
  Result := TDivisionExpression.Create(Left, Right);
end;

class function Expression.DivideAssign(Left, Right: IExpression): IBinaryExpression;
begin
  Result := Assign(Left, Divide(Left, Right));
end;

class function Expression.Empty: IDefaultExpression;
begin
  Result := Default();
end;

class function Expression.Equal(Left, Right: IExpression): IBinaryExpression;
begin
  Result := TEqualExpression.Create(Left, Right);
end;

//class function Expression.FieldAccess(Expression: IExpression; const Name: string): IMemberExpression;
//begin
//  Result := nil;
//end;

class function Expression.GreaterThan(Left, Right: IExpression): IBinaryExpression;
begin
  Result := TGreaterThanExpression.Create(Left, Right);
end;

class function Expression.GreaterThanOrEqual(Left, Right: IExpression): IBinaryExpression;
begin
  Result := TGreaterThanOrEqualExpression.Create(Left, Right);
end;

class function Expression.IfThen(Test, IfTrue: IExpression): IConditionalExpression;
begin
  Result := TConditionalExpression.Create(Test, IfTrue, Default());
end;

class function Expression.IfThenElse(Test, IfTrue, IfFalse: IExpression): IConditionalExpression;
begin
  Result := TConditionalExpression.Create(Test, IfTrue, IfFalse);
end;

class function Expression.IsFalse(Expression: IExpression): IUnaryExpression;
begin
  Result := TLogicalNotExpression.Create(Expression);
end;

class function Expression.IsTrue(Expression: IExpression): IUnaryExpression;
begin
  Result := TUnaryExpression.Create(Expression);
end;

class function Expression.LessThan(Left, Right: IExpression): IBinaryExpression;
begin
  Result := TLessThanExpression.Create(Left, Right);
end;

class function Expression.LessThanOrEqual(Left, Right: IExpression): IBinaryExpression;
begin
  Result := TLessThanOrEqualExpression.Create(Left, Right);
end;

class function Expression.LogicalAnd(Left, Right: IExpression): IBinaryExpression;
begin
  Result := TLogicalAndExpression.Create(Left, Right);
end;

class function Expression.LogicalAndAssign(Left, Right: IExpression): IBinaryExpression;
begin
  Result := Assign(Left, LogicalAnd(Left, Right));
end;

class function Expression.LogicalExclusiveOr(Left, Right: IExpression): IBinaryExpression;
begin
  Result := TLogicalExclusiveOrExpression.Create(Left, Right);
end;

class function Expression.LogicalExclusiveOrAssign(Left, Right: IExpression): IBinaryExpression;
begin
  Result := Assign(Left, LogicalExclusiveOr(Left, Right));
end;

class function Expression.LogicalNot(Expression: IExpression): IUnaryExpression;
begin
  Result := TLogicalNotExpression.Create(Expression);
end;

class function Expression.LogicalNotAssign(Expression: IExpression): IBinaryExpression;
begin
  Result := Assign(Expression, LogicalNot(Expression));
end;

class function Expression.LogicalOr(Left, Right: IExpression): IBinaryExpression;
begin
  Result := TLogicalOrExpression.Create(Left, Right);
end;

class function Expression.LogicalOrAssign(Left, Right: IExpression): IBinaryExpression;
begin
  Result := Assign(Left, LogicalOr(Left, Right));
end;

class function Expression.Loop(Expression: IExpression): IUnaryExpression;
begin
  Result := TLoopExpression.Create(Expression);
end;

class function Expression.MethodCall(Expression: IExpression; const Name: string;
  const Arguments: array of IExpression): IMemberExpression;
begin
  Result := TMethodExpression.Create(Expression, Name, Arguments);
end;

class function Expression.Modulo(Left, Right: IExpression): IBinaryExpression;
begin
  Result := TModulusExpression.Create(Left, Right);
end;

class function Expression.ModuloAssign(Left, Right: IExpression): IBinaryExpression;
begin
  Result := Assign(Left, Modulo(Left, Right));
end;

class function Expression.Multiply(Left, Right: IExpression): IBinaryExpression;
begin
  Result := TMultiplicationExpression.Create(Left, Right);
end;

class function Expression.MultiplyAssign(Left, Right: IExpression): IBinaryExpression;
begin
  Result := Assign(Left, Multiply(Left, Right));
end;

class function Expression.Negate(Expression: IExpression): IUnaryExpression;
begin
  Result := TNegativeExpression.Create(Expression);
end;

class function Expression.Parameter(const Name: string): IParameterExpression;
begin
  Result := TParameterExpression.Create(Name);
end;

class function Expression.PropertyAccess(Expression: IExpression; const Name: string): IMemberExpression;
begin
  Result := TPropertyExpression.Create(Expression, Name);
end;

class function Expression.Subtract(Left, Right: IExpression): IBinaryExpression;
begin
  Result := TSubtractionExpression.Create(Left, Right);
end;

class function Expression.SubtractAssign(Left, Right: IExpression): IBinaryExpression;
begin
  Result := Assign(Left, Subtract(Left, Right));
end;

class function Expression.Variable(const Name: string): IParameterExpression;
begin
  Result := TParameterExpression.Create(Name);
end;

{ TBooleanExpression }

constructor TBooleanExpression.Create(Expression: IExpression);
begin
  FExpression := Expression;
  ExpressionStack.Push(FExpression);
end;

class operator TBooleanExpression.Equal(const Left,
  Right: TBooleanExpression): TBooleanExpression;
begin
  Result.FExpression := TEqualExpression.Create(Left.FExpression, Right.FExpression);
  ExpressionStack.Clear();
  ExpressionStack.Push(Result.FExpression);
end;

class operator TBooleanExpression.Implicit(
  const Value: TBooleanExpression): TFunc<Boolean>;
var
  Expression: IExpression;
  Delegate: TCompiledExpression;
begin
  Expression := ExpressionStack.Pop();

  Delegate := Expression.Compile();
  Result :=
    function: Boolean
    begin
      if Assigned(Expression) then;

      Result := Delegate().AsBoolean;
    end;
end;

class operator TBooleanExpression.Implicit(
  const Value: TBooleanExpression): TFunc<TObject, Boolean>;
var
  Expression: IExpression;
  Param1: IValueExpression;
  Delegate: TCompiledExpression;
begin
  Expression := ExpressionStack.Pop();
  Param1 := ExpressionParams[0];

  Delegate := Expression.Compile();
  Result :=
    function(Arg1: TObject): Boolean
    begin
      if Assigned(Expression) then;

      Param1.SetValue(Arg1);
      Result := Delegate().AsBoolean;
    end;
end;

class operator TBooleanExpression.Implicit(const Value: Boolean): TBooleanExpression;
begin
  if ExpressionManager.ForceStack then
    Result.FExpression := ExpressionStack.Pop()
  else
    Result.FExpression := TConstantExpression.Create<Boolean>(Value);
  ExpressionStack.Push(Result.FExpression);
end;

class operator TBooleanExpression.Implicit(const Value: TBooleanExpression): Boolean;
var
  Delegate: TCompiledExpression;
begin
  ExpressionStack.Pop();
  Delegate := Value.Compile();
  Result := Delegate().AsBoolean;
end;

class operator TBooleanExpression.LogicalAnd(const Left,
  Right: TBooleanExpression): TBooleanExpression;
begin
  Result.FExpression := TLogicalAndExpression.Create(Left.FExpression, Right.FExpression);
  ExpressionStack.Clear();
  ExpressionStack.Push(Result.FExpression);
end;

class operator TBooleanExpression.LogicalNot(
  const Value: TBooleanExpression): TBooleanExpression;
begin
  Result.FExpression := TLogicalNotExpression.Create(Value.FExpression);
  ExpressionStack.Clear();
  ExpressionStack.Push(Result.FExpression);
end;

class operator TBooleanExpression.LogicalOr(const Left,
  Right: TBooleanExpression): TBooleanExpression;
begin
  Result.FExpression := TLogicalOrExpression.Create(Left.FExpression, Right.FExpression);
  ExpressionStack.Clear();
  ExpressionStack.Push(Result.FExpression);
end;

class operator TBooleanExpression.LogicalXor(const Left,
  Right: TBooleanExpression): TBooleanExpression;
begin
  Result.FExpression := TLogicalExclusiveOrExpression.Create(Left.FExpression, Right.FExpression);
  ExpressionStack.Clear();
  ExpressionStack.Push(Result.FExpression);
end;

class operator TBooleanExpression.Negative(
  const Value: TBooleanExpression): TBooleanExpression;
begin
  Result.FExpression := TNegativeExpression.Create(Value.FExpression);
  ExpressionStack.Clear();
  ExpressionStack.Push(Result.FExpression);
end;

class operator TBooleanExpression.NotEqual(const Left,
  Right: TBooleanExpression): TBooleanExpression;
begin
  Result.FExpression := TNotEqualExpression.Create(Left.FExpression, Right.FExpression);
  ExpressionStack.Clear();
  ExpressionStack.Push(Result.FExpression);
end;

function TBooleanExpression.Compile: TCompiledExpression;
begin
  Result := FExpression.Compile;
end;

function TBooleanExpression.Execute: TValue;
begin
  Result := FExpression.Execute;
end;

function TBooleanExpression.ToString: string;
begin
  Result := FExpression.ToString;
end;

class operator TBooleanExpression.Implicit(
  const Value: TBooleanExpression): IExpression;
begin
  Result := Value.FExpression;
end;

{ TValueExpression }

class operator TValueExpression.Add(const Left, Right: TValueExpression): TValueExpression;
begin
  Result.FExpression := TAdditionExpression.Create(Left.FExpression, Right.FExpression);
  ExpressionStack.Clear();
  ExpressionStack.Push(Result.FExpression);
end;

class operator TValueExpression.Divide(const Left,
  Right: TValueExpression): TValueExpression;
begin
  Result.FExpression := TDivisionExpression.Create(Left.FExpression, Right.FExpression);
  ExpressionStack.Clear();
  ExpressionStack.Push(Result.FExpression);
end;

class operator TValueExpression.Equal(const Left, Right: TValueExpression): TBooleanExpression;
begin
  Result.FExpression := TEqualExpression.Create(Left.FExpression, Right.FExpression);
  ExpressionStack.Clear();
  ExpressionStack.Push(Result.FExpression);
end;

class operator TValueExpression.GreaterThan(const Left,
  Right: TValueExpression): TBooleanExpression;
begin
  Result.FExpression := TGreaterThanExpression.Create(Left.FExpression, Right.FExpression);
  ExpressionStack.Clear();
  ExpressionStack.Push(Result.FExpression);
end;

class operator TValueExpression.GreaterThanOrEqual(const Left,
  Right: TValueExpression): TBooleanExpression;
begin
  Result.FExpression := TGreaterThanOrEqualExpression.Create(Left.FExpression, Right.FExpression);
  ExpressionStack.Clear();
  ExpressionStack.Push(Result.FExpression);
end;

class operator TValueExpression.Implicit(const Value: Extended): TValueExpression;
begin
  Result.FExpression := TConstantExpression.Create<Extended>(Value);
  ExpressionStack.Push(Result.FExpression);
end;

class operator TValueExpression.Implicit(const Value: Integer): TValueExpression;
begin
  Result.FExpression := TConstantExpression.Create<Integer>(Value);
  ExpressionStack.Push(Result.FExpression);
end;

class operator TValueExpression.Implicit(const Value: string): TValueExpression;
begin
  Result.FExpression := TConstantExpression.Create<string>(Value);
  ExpressionStack.Push(Result.FExpression);
end;

class operator TValueExpression.Implicit(const Value: TValueExpression): Extended;
var
  Delegate: TCompiledExpression;
begin
  ExpressionStack.Pop();
  Delegate := Value.Compile();
  Result := Delegate().AsExtended;
end;

class operator TValueExpression.Implicit(const Value: TValueExpression): Int64;
var
  Delegate: TCompiledExpression;
begin
  ExpressionStack.Pop();
  Delegate := Value.Compile();
  Result := Trunc(Delegate().AsExtended);
end;

class operator TValueExpression.Implicit(const Value: Variant): TValueExpression;
begin
  case TVarData(Value).VType of
    varByRef or varVariant:
    begin
      Result.FExpression := IExpression(TVarData(Value).VPointer);
      Result.FExpression._Release;
    end;
  else
    if ExpressionStack.Count > 0 then
    begin
      Result.FExpression := ExpressionStack.Pop();
    end
    else
    begin
      Result.FExpression := TConstantExpression.Create(Value);
    end;
  end;
  ExpressionStack.Push(Result.FExpression);
end;

class operator TValueExpression.Implicit(const Value: TBooleanExpression): TValueExpression;
begin
  Result.FExpression := Value.FExpression;
  ExpressionStack.Pop;
  ExpressionStack.Push(Result.FExpression);
end;

class operator TValueExpression.IntDivide(const Left,
  Right: TValueExpression): TValueExpression;
begin
  Result.FExpression := TIntegerDivisionExpression.Create(Left.FExpression, Right.FExpression);
  ExpressionStack.Clear();
  ExpressionStack.Push(Result.FExpression);
end;

class operator TValueExpression.LessThan(const Left,
  Right: TValueExpression): TBooleanExpression;
begin
  Result.FExpression := TLessThanExpression.Create(Left.FExpression, Right.FExpression);
  ExpressionStack.Clear();
  ExpressionStack.Push(Result.FExpression);
end;

class operator TValueExpression.LessThanOrEqual(const Left,
  Right: TValueExpression): TBooleanExpression;
begin
  Result.FExpression := TLessThanOrEqualExpression.Create(Left.FExpression, Right.FExpression);
  ExpressionStack.Clear();
  ExpressionStack.Push(Result.FExpression);
end;

class operator TValueExpression.Modulus(const Left, Right: TValueExpression): TValueExpression;
begin
  Result.FExpression := TModulusExpression.Create(Left.FExpression, Right.FExpression);
  ExpressionStack.Clear();
  ExpressionStack.Push(Result.FExpression);
end;

class operator TValueExpression.Multiply(const Left,
  Right: TValueExpression): TValueExpression;
begin
  Result.FExpression := TMultiplicationExpression.Create(Left.FExpression, Right.FExpression);
  ExpressionStack.Clear();
  ExpressionStack.Push(Result.FExpression);
end;

class operator TValueExpression.Negative(const Value: TValueExpression): TValueExpression;
begin
  Result.FExpression := TNegativeExpression.Create(Value.FExpression);
  ExpressionStack.Pop();
  ExpressionStack.Push(Result.FExpression);
end;

class operator TValueExpression.NotEqual(const Left,
  Right: TValueExpression): TBooleanExpression;
begin
  Result.FExpression := TNotEqualExpression.Create(Left.FExpression, Right.FExpression);
  ExpressionStack.Clear();
  ExpressionStack.Push(Result.FExpression);
end;

class operator TValueExpression.Subtract(const Left,
  Right: TValueExpression): TValueExpression;
begin
  Result.FExpression := TSubtractionExpression.Create(Left.FExpression, Right.FExpression);
  ExpressionStack.Clear();
  ExpressionStack.Push(Result.FExpression);
end;

function TValueExpression.Compile: TCompiledExpression;
begin
  if Assigned(FExpression) then
  begin
    Result := FExpression.Compile();
  end
  else
  begin
    Result :=
      function: TValue
      begin
        Result := TValue.Empty
      end;
  end;
end;

function TValueExpression.Execute: TValue;
begin
  if Assigned(FExpression) then
  begin
    Result := FExpression.Execute();
  end
  else
  begin
    Result := TValue.Empty;
  end;
end;

function TValueExpression.ToString: string;
begin
  if Assigned(FExpression) then
  begin
    Result := FExpression.ToString;
  end
  else
  begin
    Result := '(empty)';
  end;
end;

class operator TValueExpression.Implicit(
  const Value: TValueExpression): IExpression;
begin
  Result := Value.FExpression;
end;

{ TExpression }

function TExpression.Compile: TCompiledExpression;
begin
  Result := CompileExpression();
end;

function TExpression.Execute: TValue;
var
  Delegate: TCompiledExpression;
begin
  Delegate := CompileExpression();
  Result := Delegate();
end;

{ TConstantExpression }

constructor TConstantExpression.Create(const Value: TObject);
begin
  FValue := TValue.From<TObject>(Value);
end;

constructor TConstantExpression.Create(const Value: TValue);
begin
  FValue := Value;
end;

constructor TConstantExpression.Create(const Value: Variant);
begin
  FValue := TValue.FromVariant(Value);
end;

class function TConstantExpression.Create<T>(
  const Value: T): TConstantExpression;
begin
  Result := TConstantExpression.Create(TValue.From<T>(Value));
end;

function TConstantExpression.CompileExpression: TCompiledExpression;
var
  Expression: IConstantExpression;
begin
  Expression := Self;

  Result :=
    function: TValue
    begin
      Result := Expression.Value;
    end;
end;

function TConstantExpression.GetValue: TValue;
begin
  Result := FValue;
end;

function TConstantExpression.ToString: string;
begin
  if FValue.IsType<string> then
    Result := '"' + FValue.ToString + '"'
  else
  if FValue.IsObject and (FValue.AsObject is TComponent) then
    Result := FValue.AsType<TComponent>.Name
  else
    Result := FValue.ToString;
end;

{ TDefaultExpression }

constructor TDefaultExpression.Create(TypeInfo: Pointer);
begin
  //inherited Create(TValue.From(nil, TypeInfo));
end;

{ TParameterExpression }

constructor TParameterExpression.Create(const AName: string);
begin
  FName := AName;
end;

function TParameterExpression.GetName: string;
begin
  Result := FName;
end;

procedure TParameterExpression.SetValue(const Value: TValue);
begin
  FValue := Value;
end;

function TParameterExpression.ToString: string;
begin
  Result := FName;
end;

{ TComponentExpression }

constructor TComponentExpression.Create(const Value: TComponent);
begin
  inherited Create(Value);
end;

function TComponentExpression.CompileExpression: TCompiledExpression;
var
  Expression: IConstantExpression;
begin
  Expression := Self;

  Result :=
    function: TValue
    begin
      Result := Expression.Value.AsType<TComponent>;
    end;
end;

function TComponentExpression.ToString: string;
begin
  Result := FValue.AsType<TComponent>.Name;
end;

{ TUnaryExpression }

constructor TUnaryExpression.Create(Expression: IExpression);
begin
  FExpression := Expression;
end;

function TUnaryExpression.CompileExpression: TCompiledExpression;
begin
  Result := FExpression.Compile();
end;

function TUnaryExpression.GetExpression: IExpression;
begin
  Result := FExpression;
end;

{ TNegativeExpression }

function TNegativeExpression.CompileExpression: TCompiledExpression;
var
  Expression: IExpression;
  Delegate: TCompiledExpression;
begin
  Expression := Self;
  Delegate := FExpression.Compile();

  Result :=
    function: TValue
    var
      LValue: TValue;
    begin
      if Assigned(Expression) then;

      LValue := Delegate();

      if LValue.IsOrdinal  then
      begin
        Result := TValue.From<Int64>(LValue.AsOrdinal * -1);
      end;
      if LValue.IsFloat then
      begin
        Result := TValue.From<Extended>(LValue.AsExtended * -1);
      end;
    end;
end;

function TNegativeExpression.ToString: string;
begin
  Result := '(-' + FExpression.ToString + ')';
end;

{ TBinaryExpression }

constructor TBinaryExpression.Create(Left, Right: IExpression);
begin
  FLeft := Left;
  FRight := Right;
end;

function TBinaryExpression.BuildString(const OperatorString: string): string;
begin
  Result := FLeft.ToString + ' ' + OperatorString + ' ' + FRight.ToString;
  if ExpressionManager.UseParentheses then
    Result := '(' + Result + ')';
end;

function TBinaryExpression.GetLeft: IExpression;
begin
  Result := FLeft;
end;

function TBinaryExpression.GetRight: IExpression;
begin
  Result := FRight;
end;

{ TAdditionExpression }

function TAdditionExpression.CompileExpression: TCompiledExpression;
var
  Expression: IBinaryExpression;
  LeftDelegate, RightDelegate: TCompiledExpression;
begin
  Expression := Self;
  LeftDelegate := FLeft.Compile();
  RightDelegate := FRight.Compile();

  Result :=
    function: TValue
    var
      LeftValue, RightValue: TValue;
    begin
      if Assigned(Expression) then;

      LeftValue := LeftDelegate();
      RightValue := RightDelegate();

      if LeftValue.IsOrdinal and RightValue.IsOrdinal then
      begin
        Result := TValue.From<Int64>(LeftValue.AsOrdinal + RightValue.AsOrdinal);
      end else
      if LeftValue.IsFloat and RightValue.IsFloat then
      begin
        Result := TValue.From<Extended>(LeftValue.AsExtended + RightValue.AsExtended);
      end else
      begin
        Result := TValue.From<string>(LeftValue.ToString + RightValue.ToString);
      end;
    end;
end;

function TAdditionExpression.ToString: string;
begin
  Result := BuildString('+');
end;

{ TSubtractionExpression }

function TSubtractionExpression.CompileExpression: TCompiledExpression;
var
  Expression: IBinaryExpression;
  LeftDelegate, RightDelegate: TCompiledExpression;
begin
  Expression := Self;
  LeftDelegate := FLeft.Compile();
  RightDelegate := FRight.Compile();

  Result :=
    function: TValue
    var
      LeftValue, RightValue: TValue;
    begin
      if Assigned(Expression) then;

      LeftValue := LeftDelegate();
      RightValue := RightDelegate();

      if LeftValue.IsOrdinal and RightValue.IsOrdinal then
      begin
        Result := TValue.From<Int64>(LeftValue.AsOrdinal - RightValue.AsOrdinal);
      end;
      if LeftValue.IsFloat and RightValue.IsFloat then
      begin
        Result := TValue.From<Extended>(LeftValue.AsExtended - RightValue.AsExtended);
      end;
    end;
end;

function TSubtractionExpression.ToString: string;
begin
  Result := BuildString('-');
end;

{ TMultiplicationExpression }

function TMultiplicationExpression.CompileExpression: TCompiledExpression;
var
  Expression: IBinaryExpression;
  LeftDelegate, RightDelegate: TCompiledExpression;
begin
  Expression := Self;
  LeftDelegate := FLeft.Compile();
  RightDelegate := FRight.Compile();

  Result :=
    function: TValue
    var
      LeftValue, RightValue: TValue;
    begin
      if Assigned(Expression) then;

      LeftValue := LeftDelegate();
      RightValue := RightDelegate();

      if LeftValue.IsOrdinal and RightValue.IsOrdinal then
      begin
        Result := TValue.From<Int64>(LeftValue.AsOrdinal * RightValue.AsOrdinal);
      end;
      if LeftValue.IsFloat and RightValue.IsFloat then
      begin
        Result := TValue.From<Extended>(LeftValue.AsExtended * RightValue.AsExtended);
      end;
    end;
end;

function TMultiplicationExpression.ToString: string;
begin
  Result := BuildString('*');
end;

{ TDivisionExpression }

function TDivisionExpression.CompileExpression: TCompiledExpression;
var
  Expression: IBinaryExpression;
  LeftDelegate, RightDelegate: TCompiledExpression;
begin
  Expression := Self;
  LeftDelegate := FLeft.Compile();
  RightDelegate := FRight.Compile();

  Result :=
    function: TValue
    var
      LeftValue, RightValue: TValue;
    begin
      if Assigned(Expression) then;

      LeftValue := LeftDelegate();
      RightValue := RightDelegate();

      if LeftValue.IsNumeric and RightValue.IsNumeric then
      begin
        Result := TValue.From<Extended>(LeftValue.AsExtended / RightValue.AsExtended);
      end;
    end;
end;

function TDivisionExpression.ToString: string;
begin
  Result := BuildString('/');
end;

{ TIntegerDivisionExpression }

function TIntegerDivisionExpression.CompileExpression: TCompiledExpression;
var
  Expression: IBinaryExpression;
  LeftDelegate, RightDelegate: TCompiledExpression;
begin
  Expression := Self;
  LeftDelegate := FLeft.Compile();
  RightDelegate := FRight.Compile();

  Result :=
    function: TValue
    var
      LeftValue, RightValue: TValue;
    begin
      if Assigned(Expression) then;

      LeftValue := LeftDelegate();
      RightValue := RightDelegate();

      if LeftValue.IsOrdinal and RightValue.IsOrdinal then
      begin
        Result := TValue.From<Int64>(LeftValue.AsOrdinal div RightValue.AsOrdinal);
      end;
    end;
end;

function TIntegerDivisionExpression.ToString: string;
begin
  Result := BuildString('div');
end;

{ TModulusExpression }

function TModulusExpression.CompileExpression: TCompiledExpression;
var
  Expression: IBinaryExpression;
  LeftDelegate, RightDelegate: TCompiledExpression;
begin
  Expression := Self;
  LeftDelegate := FLeft.Compile();
  RightDelegate := FRight.Compile();

  Result :=
    function: TValue
    var
      LeftValue, RightValue: TValue;
    begin
      if Assigned(Expression) then;

      LeftValue := LeftDelegate();
      RightValue := RightDelegate();

      if LeftValue.IsOrdinal and RightValue.IsOrdinal then
      begin
        Result := TValue.From<Int64>(LeftValue.AsOrdinal mod RightValue.AsOrdinal);
      end;
    end;
end;

function TModulusExpression.ToString: string;
begin
  Result := BuildString('mod');
end;

{ TLogicalAndExpression }

function TLogicalAndExpression.CompileExpression: TCompiledExpression;
var
  Expression: IBinaryExpression;
  LeftDelegate, RightDelegate: TCompiledExpression;
begin
  Expression := Self;
  LeftDelegate := FLeft.Compile();
  RightDelegate := FRight.Compile();

  Result :=
    function: TValue
    var
      LeftValue, RightValue: TValue;
    begin
      if Assigned(Expression) then;

      LeftValue := LeftDelegate();
      RightValue := RightDelegate();

      Result := TValue.From<Boolean>(LeftValue.AsBoolean and RightValue.AsBoolean);
    end;
end;

function TLogicalAndExpression.ToString: string;
begin
  Result := BuildString('and');
end;

{ TLogicalExclusiveOrExpression }

function TLogicalExclusiveOrExpression.CompileExpression: TCompiledExpression;
var
  Expression: IBinaryExpression;
  LeftDelegate, RightDelegate: TCompiledExpression;
begin
  Expression := Self;
  LeftDelegate := FLeft.Compile();
  RightDelegate := FRight.Compile();

  Result :=
    function: TValue
    var
      LeftValue, RightValue: TValue;
    begin
      if Assigned(Expression) then;

      LeftValue := LeftDelegate();
      RightValue := RightDelegate();

      Result := TValue.From<Boolean>(LeftValue.AsBoolean() xor RightValue.AsBoolean());
    end;
end;

function TLogicalExclusiveOrExpression.ToString: string;
begin
  Result := BuildString('xor');
end;

{ TLogicalOrExpression }

function TLogicalOrExpression.CompileExpression: TCompiledExpression;
var
  Expression: IBinaryExpression;
  LeftDelegate, RightDelegate: TCompiledExpression;
begin
  Expression := Self;
  LeftDelegate := FLeft.Compile();
  RightDelegate := FRight.Compile();

  Result :=
    function: TValue
    var
      LeftValue, RightValue: TValue;
    begin
      if Assigned(Expression) then;

      LeftValue := LeftDelegate();
      RightValue := RightDelegate();

      Result := TValue.From<Boolean>(LeftValue.AsBoolean or RightValue.AsBoolean);
    end;
end;

function TLogicalOrExpression.ToString: string;
begin
  Result := BuildString('or');
end;

{ TEqualExpression }

function TEqualExpression.CompileExpression: TCompiledExpression;
var
  Expression: IBinaryExpression;
  LeftDelegate, RightDelegate: TCompiledExpression;
begin
  Expression := Self;
  LeftDelegate := FLeft.Compile();
  RightDelegate := FRight.Compile();

  Result :=
    function: TValue
    var
      LeftValue, RightValue: TValue;
    begin
      if Assigned(Expression) then;

      LeftValue := LeftDelegate();
      RightValue := RightDelegate();

      Result := LeftValue.Equals(RightValue);
    end;
end;

function TEqualExpression.ToString: string;
begin
  Result := BuildString('=');
end;

{ TNotEqualExpression }

function TNotEqualExpression.CompileExpression: TCompiledExpression;
var
  Expression: IBinaryExpression;
  LeftDelegate, RightDelegate: TCompiledExpression;
begin
  Expression := Self;
  LeftDelegate := FLeft.Compile();
  RightDelegate := FRight.Compile();

  Result :=
    function: TValue
    var
      LeftValue, RightValue: TValue;
    begin
      if Assigned(Expression) then;

      LeftValue := LeftDelegate();
      RightValue := RightDelegate();

      if LeftValue.IsNumeric and RightValue.IsNumeric then
      begin
        Result := TValue.From<Boolean>(LeftValue.AsExtended <> RightValue.AsExtended);
      end
      else
      begin
        Result := TValue.From<Boolean>(LeftValue.ToString <> RightValue.ToString);
      end;
    end;
end;

function TNotEqualExpression.ToString: string;
begin
  Result := BuildString('<>');
end;

{ TLessThanExpression }

function TLessThanExpression.CompileExpression: TCompiledExpression;
var
  Expression: IBinaryExpression;
  LeftDelegate, RightDelegate: TCompiledExpression;
begin
  Expression := Self;
  LeftDelegate := FLeft.Compile();
  RightDelegate := FRight.Compile();

  Result :=
    function: TValue
    var
      LeftValue, RightValue: TValue;
    begin
      if Assigned(Expression) then;

      LeftValue := LeftDelegate();
      RightValue := RightDelegate();

      if LeftValue.IsNumeric and RightValue.IsNumeric then
      begin
        Result := TValue.From<Boolean>(LeftValue.AsExtended < RightValue.AsExtended);
      end
      else
      begin
        Result := TValue.From<Boolean>(LeftValue.ToString < RightValue.ToString);
      end;
    end;
end;

function TLessThanExpression.ToString: string;
begin
  Result := BuildString('<');
end;

{ TLessThanOrEqualExpression }

function TLessThanOrEqualExpression.CompileExpression: TCompiledExpression;
var
  Expression: IBinaryExpression;
  LeftDelegate, RightDelegate: TCompiledExpression;
begin
  Expression := Self;
  LeftDelegate := FLeft.Compile();
  RightDelegate := FRight.Compile();

  Result :=
    function: TValue
    var
      LeftValue, RightValue: TValue;
    begin
      if Assigned(Expression) then;

      LeftValue := LeftDelegate();
      RightValue := RightDelegate();

      if LeftValue.IsNumeric and RightValue.IsNumeric then
      begin
        Result := TValue.From<Boolean>(LeftValue.AsExtended <= RightValue.AsExtended);
      end
      else
      begin
        Result := TValue.From<Boolean>(LeftValue.ToString <= RightValue.ToString);
      end;
    end;
end;

function TLessThanOrEqualExpression.ToString: string;
begin
  Result := BuildString('<=');
end;

{ TGreaterThanExpression }

function TGreaterThanExpression.CompileExpression: TCompiledExpression;
var
  Expression: IBinaryExpression;
  LeftDelegate, RightDelegate: TCompiledExpression;
begin
  Expression := Self;
  LeftDelegate := FLeft.Compile();
  RightDelegate := FRight.Compile();

  Result :=
    function: TValue
    var
      LeftValue, RightValue: TValue;
    begin
      if Assigned(Expression) then;

      LeftValue := LeftDelegate();
      RightValue := RightDelegate();

      if LeftValue.IsNumeric and RightValue.IsNumeric then
      begin
        Result := TValue.From<Boolean>(LeftValue.AsExtended > RightValue.AsExtended);
      end
      else
      begin
        Result := TValue.From<Boolean>(LeftValue.ToString > RightValue.ToString);
      end;
    end;
end;

function TGreaterThanExpression.ToString: string;
begin
  Result := BuildString('>');
end;

{ TGreaterThanOrEqualExpression }

function TGreaterThanOrEqualExpression.CompileExpression: TCompiledExpression;
var
  Expression: IBinaryExpression;
  LeftDelegate, RightDelegate: TCompiledExpression;
begin
  Expression := Self;
  LeftDelegate := FLeft.Compile();
  RightDelegate := FRight.Compile();

  Result :=
    function: TValue
    var
      LeftValue, RightValue: TValue;
    begin
      if Assigned(Expression) then;

      LeftValue := LeftDelegate();
      RightValue := RightDelegate();

      if LeftValue.IsNumeric and RightValue.IsNumeric then
      begin
        Result := TValue.From<Boolean>(LeftValue.AsExtended >= RightValue.AsExtended);
      end
      else
      begin
        Result := TValue.From<Boolean>(LeftValue.ToString >= RightValue.ToString);
      end;
    end;
end;

function TGreaterThanOrEqualExpression.ToString: string;
begin
  Result := BuildString('>=');
end;

{ TMemberExpression }

function TMemberExpression.GetExpression: IExpression;
begin
  Result := FExpression;
end;

function TMemberExpression.GetInstance: TValue;
var
  Delegate: TCompiledExpression;
begin
  if Assigned(FExpression) then
  begin
    Delegate := FExpression.Compile();
    Result := Delegate();
  end
  else
  begin
    Result := nil;
  end;
end;

function TMemberExpression.GetMember: TRttiMember;
begin
  Result := nil;
end;

function TMemberExpression.GetName: string;
begin
  Result := FName;
end;

function TMemberExpression.GetValue: TValue;
begin
  Result := Execute();
end;

procedure TMemberExpression.SetInstance(const Value: TValue);
var
  LParameterExpression: IParameterExpression;
  LMemberExpression: IMemberExpression;
begin
  if Supports(FExpression, IParameterExpression, LParameterExpression) then
  begin
    LParameterExpression.Value := Value;
  end else
  if Supports(FExpression, IMemberExpression, LMemberExpression) then
  begin
    LMemberExpression.Instance := Value;
  end
end;

procedure TMemberExpression.SetValue(const Value: TValue);
begin
  // nothing to do
end;

{ TPropertyExpression }

constructor TPropertyExpression.Create(AExpression: IExpression;
  const APropertyName: string);
var
  LObjectName: string;
  LPropertyName: string;
  LPos: Integer;

  function FindPropertyExtension: TRttiPropertyExtension;
  begin
    LPropertyName := APropertyName;
    repeat
      LPos := PosEx('[', LPropertyName, LastDelimiter('.', LPropertyName));
      if LPos > 0 then
      begin
        FIndex := StrToIntDef(Copy(LPropertyName, Succ(LPos),
          Pos(']', LPropertyName) - Succ(LPos)), -1);
        LPropertyName := LeftStr(LPropertyName, Pred(LPos));
      end;
      Result := TRttiPropertyExtension.FindByName(LPropertyName);
      if not Assigned(Result) then
      begin
        LPropertyName := Copy(LPropertyName, 1, LastDelimiter('.', LPropertyName) - 1);
      end;
    until Assigned(Result) or not ContainsText(LPropertyName, '.');

    LPos := FindDelimiter('.', Name, Length(LPropertyName));
    if LPos > 0 then
    begin
      LObjectName := LPropertyName;
      if FIndex > -1 then
      begin
        LObjectName := LObjectName +  '[' + IntToStr(FIndex) + ']';
      end;
      LPropertyName := Copy(Name, Succ(LPos));
    end;
  end;

begin
  FExpression := AExpression;
  FIndex := -1;
  FName := APropertyName;

  FProperty := FindPropertyExtension;

  if Assigned(FProperty) then
  begin
    if LObjectName <> '' then
    begin
      FExpression := TPropertyExpression.Create(FExpression, LObjectName);
      FProperty := nil;
      FIndex := -1;
      FPropertyName := LPropertyName;
    end
    else
    begin
      FPropertyName := FProperty.Name;
    end;
  end
  else
  begin
    LPropertyName := APropertyName;
    while ContainsText(LPropertyName, '.') do
    begin
      LObjectName := LeftStr(LPropertyName, Pred(Pos('.', LPropertyName)));
      FExpression := TPropertyExpression.Create(FExpression, LObjectName);
      LPropertyName := RightStr(LPropertyName, Length(LPropertyName) - Pos('.', LPropertyName));
    end;
    LPos := Pos('[', LPropertyName);
    if LPos > 0 then
    begin
      FIndex := StrToIntDef(Copy(LPropertyName, Succ(LPos),
        Pos(']', LPropertyName) - Succ(LPos)), -1);
      FPropertyName := LeftStr(LPropertyName, Pred(LPos));
    end
    else
    begin
      FPropertyName := LPropertyName;
    end;
  end;
end;

constructor TPropertyExpression.Create(AComponent: TComponent;
  const APropertyName: string);
var
  LExpression: IExpression;
begin
  LExpression := TComponentExpression.Create(AComponent);
  Create(LExpression, APropertyName);
end;

constructor TPropertyExpression.Create(AObject: TObject;
  const APropertyName: string);
var
  LExpression: IExpression;
begin
  LExpression := TConstantExpression.Create(AObject);
  Create(LExpression, APropertyName);
end;

function TPropertyExpression.CompileExpression: TCompiledExpression;
var
  Expression: IMemberExpression;
begin
  Expression := Self;

  Result :=
    function: TValue
    begin
      Result := Expression.Value;
    end;
end;

function TPropertyExpression.GetIndexedValue(const Instance: TValue): TValue;
var
  LListObject: TObject;
  LObject: TObject;
  LListInterface: IInterface;
  LTypeInfo: Pointer;
begin
  Result := TValue.Empty;

  if FIndex > -1 then
  begin
    if Instance.IsObject then
    begin
      LListObject := Instance.AsObject;
      Result := TValue.Empty;
      LObject := nil;

      if Assigned(LListObject) then
      begin
        if LListObject.InheritsFrom(TStrings) and (TStrings(LListObject).Count > FIndex) then
        begin
          Result := TStrings(LListObject).Strings[FIndex];
        end else
        if LListObject.InheritsFrom(TCollection) and (TCollection(LListObject).Count > FIndex) then
        begin
          LObject := TCollection(LListObject).Items[FIndex];
        end else
        if LListObject.InheritsFrom(TList) and (TList(LListObject).Count > FIndex) then
        begin
          LObject := TList(LListObject).Items[FIndex];
        end else
        if IsClassCovariantTo(LListObject.ClassType, TList<TObject>)
          and (TList<TObject>(LListObject).Count > FIndex) then
        begin
          LObject := TList<TObject>(LListObject).Items[FIndex];
        end else

        // hack to put the interface into an object reference to carry it around
        if IsClassCovariantTo(LListObject.ClassType, TList<IInterface>)
          and (TList<TObject>(LListObject).Count > FIndex) then
        begin
          LObject := TList<IInterface>(LListObject).Items[FIndex] as TObject;
        end;

        if Assigned(LObject) then
        begin
          Result := TValue.From(LObject);
        end;
      end
    end else
    if Instance.IsInterface then
    begin
      LListInterface := Instance.AsInterface;
      LTypeInfo := Instance.TypeInfo;
      Result := TValue.Empty;
      LObject := nil;

      if Assigned(LListInterface) then
      begin
        if IsTypeCovariantTo(LTypeInfo, TypeInfo(IList<TObject>))
          and (IList<TObject>(LListInterface).Count > FIndex) then
        begin
          LObject := IList<TObject>(LListInterface).Items[FIndex];
        end else

        // hack to put the interface into an object reference to carry it around
        if IsTypeCovariantTo(LTypeInfo, TypeInfo(IList<IInterface>))
          and (IList<TObject>(LListInterface).Count > FIndex) then
        begin
          LObject := IList<IInterface>(LListInterface).Items[FIndex] as TObject;
        end;

        if Assigned(LObject) then
        begin
          Result := TValue.From(LObject);
        end;
      end;
    end;
  end;
end;

function TPropertyExpression.GetMember: TRttiMember;
var
  LInstance: TValue;
  LType: TRttiType;
begin
  Result := nil;
  if Assigned(FProperty) then
  begin
    Result := FProperty;
  end
  else
  begin
    LInstance := GetInstance();
    LType := GetRttiType(LInstance.TypeInfo);
    if Assigned(LType) then
    begin
      LType.TryGetMember(FPropertyName, Result);
    end;
  end;
end;

type
  PPVtable = ^PVtable;
  PVtable = ^TVtable;
  TVtable = array[0..MaxInt div SizeOf(Pointer) - 1] of Pointer;

function TPropertyExpression.GetValue: TValue;
var
  LField: TRttiField;
  LInstance: TValue;
  LMethod: TRttiMethod;
  LProperty: TRttiProperty;
  LResult: TMethod;
  LType: TRttiType;

  function TryReadInterfaceProperty: Boolean;
  var
    LObject: TObject;
  begin
    Result := LType.IsInterface;
    if Result then
    begin
      LObject := LInstance.ToObject;
      Result := Assigned(LObject)
        and LObject.GetType.TryGetProperty(FPropertyName, LProperty)
        and LProperty.IsReadable;
    end;
  end;

begin
  Result := TValue.Empty;
  LInstance := GetInstance();
  LType := GetRttiType(LInstance.TypeInfo);
  if Assigned(LType) then
  begin
    LProperty := FProperty;
    if Assigned(LProperty) or LType.TryGetProperty(FPropertyName, LProperty) then
    begin
      if LProperty.IsReadable then
      begin
        Result := LProperty.GetValue(LInstance.AsObject);
      end;
    end else
    if TryReadInterfaceProperty then
    begin
      Result := LProperty.GetValue(LInstance.ToObject);
    end else
    if LType.TryGetField(FPropertyName, LField) then
    begin
      //Result := LField.GetValue(LInstance.AsPointer);
    end else
    if LType.TryGetMethod(FPropertyName, LMethod) then
    begin
      if Assigned(LMethod.ReturnType) and (LMethod.Parameters.Count = 0) then
      begin
        if LMethod.IsClassMethod then
        begin
          Result := LMethod.Invoke(LInstance.AsObject.ClassType, []);
        end
        else
        begin
          Result := LMethod.Invoke(LInstance.AsObject, []);
        end;
      end else
      begin
        if LMethod.IsClassMethod then
        begin
          LResult.Code := LMethod.CodeAddress;
          LResult.Data := LInstance.AsObject.ClassType;
          Result := TValue.From(LResult);
        end
        else
        begin
          if LMethod.IsStatic then
          begin
            LResult.Code := LMethod.CodeAddress;
          end
          else
          begin
            case LMethod.DispatchKind of
              dkVtable: LResult.Code := PVtable(LInstance.AsObject.ClassType)^[LMethod.VirtualIndex];
              dkDynamic: LResult.Code := GetDynaMethod(LInstance.AsObject.ClassType, LMethod.VirtualIndex);
            else
              LResult.Code := LMethod.CodeAddress;
            end;
          end;
          LResult.Data := LInstance.AsType<Pointer>;
          Result := TValue.From(LResult);
        end;
      end;
    end;

    if FIndex > -1 then
    begin
      Result := GetIndexedValue(Result);
    end;
  end;
end;

procedure TPropertyExpression.SetIndexedValue(const Instance, Value: TValue);
var
  LListObject: TObject;
  LListInterface: IInterface;
  LTypeInfo: Pointer;
begin
  if FIndex > -1 then
  begin
    if Instance.IsObject then
    begin
      LListObject := Instance.AsObject;

      if Assigned(LListObject) then
      begin
        if LListObject.InheritsFrom(TStrings) and (TStrings(LListObject).Count > FIndex) then
        begin
          TStrings(LListObject).Strings[FIndex] := Value.ToString;
        end else
        if LListObject.InheritsFrom(TCollection) and (TCollection(LListObject).Count > FIndex) then
        begin
          TCollection(LListObject).Items[FIndex] := Value.AsType<TCollectionItem>;
        end else
        if LListObject.InheritsFrom(TList) and (TList(LListObject).Count > FIndex) then
        begin
          TList(LListObject).Items[FIndex] := Value.AsType<Pointer>;
        end else
        if IsClassCovariantTo(LListObject.ClassType, TList<TObject>)
          and (TList<TObject>(LListObject).Count > FIndex) then
        begin
          TList<TObject>(LListObject).Items[FIndex] := Value.AsObject;
        end else

        // hack to put the interface into an object reference to carry it around
        if IsClassCovariantTo(LListObject.ClassType, TList<IInterface>)
          and (TList<TObject>(LListObject).Count > FIndex) then
        begin
          TList<IInterface>(LListObject).Items[FIndex] := Value.AsInterface;
        end;
      end
    end else
    if Instance.IsInterface then
    begin
      LListInterface := Instance.AsInterface;
      LTypeInfo := Instance.TypeInfo;

      if Assigned(LListInterface) then
      begin
        if IsTypeCovariantTo(LTypeInfo, TypeInfo(IList<TObject>))
          and (IList<TObject>(LListInterface).Count > FIndex) then
        begin
          IList<TObject>(LListInterface).Items[FIndex] := Value.AsObject;
        end else

        if IsTypeCovariantTo(LTypeInfo, TypeInfo(IList<IInterface>))
          and (IList<TObject>(LListInterface).Count > FIndex) then
        begin
          IList<IInterface>(LListInterface).Items[FIndex] := Value.AsInterface;
        end;
      end;
    end;
  end;
end;

procedure TPropertyExpression.SetValue(const Value: TValue);
var
  LField: TRttiField;
  LInstance: TValue;
  LProperty: TRttiProperty;
  LType: TRttiType;
  LValue: TValue;
begin
  LInstance := GetInstance();
  LValue := Value;
{$IFDEF VER210}
  if LValue.IsEmpty then
  begin
    TValue.Make(nil, Member.RttiType.Handle, LValue);
  end;
{$ENDIF}
  LType := GetRttiType(LInstance.TypeInfo);
  if Assigned(LType) then
  begin
    LProperty := FProperty;
    if Assigned(LProperty) or LType.TryGetProperty(FPropertyName, LProperty) then
    begin
      if FIndex = -1 then
      begin
        if LProperty.IsWritable then
        begin
          LProperty.SetValue(LInstance.AsType<Pointer>, LValue);
        end;
      end
      else
      begin
        SetIndexedValue(LProperty.GetValue(LInstance.AsType<Pointer>), LValue);
      end;
    end else
    if LType.TryGetField(FPropertyName, LField) then
    begin
      LField.SetValue(LInstance.AsType<Pointer>, LValue);
    end;
  end;
end;

function TPropertyExpression.ToString: string;
begin
  Result := FExpression.ToString() + '.' + FPropertyName;
end;

{ TMethodExpression }

constructor TMethodExpression.Create(AExpression: IExpression;
  const AMethodName: string; const AParameters: array of IExpression);
begin
  FExpression := AExpression;
  FName := AMethodName;
  FParameters := TArray.Copy<IExpression>(AParameters);
end;

function TMethodExpression.CompileExpression: TCompiledExpression;
var
  Expression: IMemberExpression;
  ArgumentDelegates: TCompiledExpressionArray;
begin
  Expression := Self;
  ArgumentDelegates := CompileArguments();

  Result :=
    function: TValue
    var
      LMethod: TRttiMethod;
      LInstance: TValue;
      LArguments: TArray<TValue>;
      i: Integer;
    begin
      if Assigned(Expression) then;

      Result := TValue.Empty;
      LMethod := GetMethod();
      if Assigned(LMethod) then
      begin
        LInstance := GetInstance();
        SetLength(LArguments, Length(ArgumentDelegates));
        for i := Low(ArgumentDelegates) to High(ArgumentDelegates) do
          LArguments[i] := ArgumentDelegates[i]();
        if LMethod.IsClassMethod then
        begin
          Result := LMethod.Invoke(LInstance.AsObject.ClassType, LArguments);
        end
        else
        begin
          Result := LMethod.Invoke(LInstance, LArguments);
        end;
      end;
    end;
end;

constructor TMethodExpression.Create(AComponent: TComponent;
  const AMethodName: string; const AParameters: array of IExpression);
var
  LExpression: IExpression;
begin
  LExpression := TComponentExpression.Create(AComponent);
  Create(LExpression, AMethodName, AParameters);
end;

constructor TMethodExpression.Create(AObject: TObject;
  const AMethodName: string; const AParameters: array of IExpression);
var
  LExpression: IExpression;
begin
  LExpression := TConstantExpression.Create(AObject);
  Create(LExpression, AMethodName, AParameters);
end;

function TMethodExpression.CompileArguments: TCompiledExpressionArray;
var
  i: Integer;
begin
  SetLength(Result, Length(FParameters));
  for i := 0 to Pred(Length(FParameters)) do
  begin
    Result[i] := FParameters[i].Compile();
  end;
end;

function TMethodExpression.GetMember: TRttiMember;
begin
  Result := GetMethod();
end;

function TMethodExpression.GetMethod: TRttiMethod;
var
  LInstance: TValue;
  LType: TRttiType;
begin
  Result := nil;
  LInstance := GetInstance();
  LType := GetRttiType(LInstance.TypeInfo);
  if Assigned(LType) then
  begin
    Result := LType.GetMethod(FName);
  end;
end;

function TMethodExpression.ToString: string;
var
  i: Integer;
begin
  Result := FExpression.ToString() + '.' + FName + '(';
  for i := 0 to Pred(Length(FParameters)) do
  begin
    if i < Pred(Length(FParameters)) then
    begin
      Result := Result + FParameters[i].ToString() + ', ';
    end
    else
    begin
      Result := Result + FParameters[i].ToString() + ')';
    end;
  end;
end;

{ TAssignExpression }

function TAssignExpression.CompileExpression: TCompiledExpression;
var
  Expression: IBinaryExpression;
  LeftDelegate, RightDelegate: TCompiledExpression;
  LeftExpr: IValueExpression;
begin
  if Supports(FLeft, IValueExpression, LeftExpr) then
  begin
    Expression := Self;
    LeftDelegate := LeftExpr.Compile();
    RightDelegate := FRight.Compile();

    Result :=
      function: TValue
      begin
        if Assigned(Expression) then;

        LeftExpr.Value := RightDelegate();
        Result := LeftDelegate();
      end;
  end
  else
    raise EArgumentException.Create('Left side cannot be assigned to');
end;

function TAssignExpression.ToString: string;
begin
  Result := FLeft.ToString + ' := ' + FRight.ToString + ';';
end;

{ TConvertExpression }

constructor TConvertExpression.Create(Left, Right: IExpression;
  ConvertFunc: TConvertFunc);
begin
  inherited Create(Left, Right);
  FConvertFunc := ConvertFunc;
end;

function TConvertExpression.CompileExpression: TCompiledExpression;
var
  Expression: IBinaryExpression;
  LeftDelegate, RightDelegate: TCompiledExpression;
  LeftExpr: IValueExpression;
  ConvertFunc: TConvertFunc;
begin
  if Supports(Left, IValueExpression, LeftExpr) then
  begin
    Expression := Self;
    LeftDelegate := LeftExpr.Compile();
    RightDelegate := Right.Compile();
    ConvertFunc := FConvertFunc;

    Result :=
      function: TValue
      var
        LeftValue, RightValue: TValue;
        MemberExpr: IMemberExpression;
      begin
        if Assigned(Expression) then;

        RightValue := RightDelegate();
        if Assigned(ConvertFunc) then
        begin
          LeftValue := ConvertFunc(RightValue);
        end
        else
        begin
          if Supports(LeftExpr, IMemberExpression, MemberExpr) then
          begin
            RightValue.TryConvert(MemberExpr.Member.GetType.Handle, LeftValue);
          end
          else
          begin
            RightValue.TryConvert(LeftExpr.Value.TypeInfo, LeftValue);
          end;
        end;
        LeftExpr.Value := LeftValue;
        Result := LeftDelegate();
      end;
  end
  else
  begin
    Result :=
      function: TValue
      begin
        raise EArgumentException.Create('Left side cannot be assigned to');
      end;
  end;
end;

{ TBlockExpression }

constructor TBlockExpression.Create(Expressions: array of IExpression);
begin
  FExpressions := TArray.Copy<IExpression>(Expressions);
end;

function TBlockExpression.CompileExpression: TCompiledExpression;
var
  Expression: IBlockExpression;
  Delegates: TCompiledExpressionArray;
  i: Integer;
begin
  Expression := Self;
  SetLength(Delegates, Length(FExpressions));

  for i := 0 to Pred(Length(FExpressions)) do
  begin
    Delegates[i] := FExpressions[i].Compile();
  end;

  Result :=
    function: TValue
    var
     i: Integer;
    begin
      if Assigned(Expression) then;

      Result := TValue.Empty;
      for i := 0 to High(Delegates) do
      begin
        Result := Delegates[i]();
      end;
    end;
end;

function TBlockExpression.GetExpressions: TExpressionArray;
begin
  Result := FExpressions;
end;

function TBlockExpression.ToString: string;
var
  i: Integer;
begin
  Result := 'begin' + sLineBreak;
  ExpressionManager.Indent();
  for i := 0 to Pred(Length(FExpressions)) do
  begin
    Result := Result + GetIndentation + FExpressions[i].ToString() + sLineBreak;
  end;
  ExpressionManager.Unindent();
  Result := Result + GetIndentation + 'end';
end;

{ TConditionalExpression }

constructor TConditionalExpression.Create(ATest, AIfTrue, AIfFalse: IExpression);
begin
  FTest := ATest;
  FIfTrue := AIfTrue;
  FIfFalse := AIfFalse;
end;

function TConditionalExpression.CompileExpression: TCompiledExpression;
var
  Expression: IConditionalExpression;
  TestDelegate, IfTrueDelegate, IfFalseDelegate: TCompiledExpression;
begin
  Expression := Self;
  TestDelegate := FTest.Compile();
  IfTrueDelegate := FIfTrue.Compile();
  IfFalseDelegate := FIfFalse.Compile();

  Result :=
    function: TValue
    begin
      if Assigned(Expression) then;

      if TestDelegate().AsBoolean then
      begin
        Result := IfTrueDelegate();
      end
      else
      begin
        Result := IfFalseDelegate();
      end;
    end;
end;

function TConditionalExpression.GetIfFalse: IExpression;
begin
  Result := FIfFalse;
end;

function TConditionalExpression.GetIfTrue: IExpression;
begin
  Result := FIfTrue;
end;

function TConditionalExpression.GetTest: IExpression;
begin
  Result := FTest;
end;

function TConditionalExpression.ToString: string;
begin
  Result := 'if ' + FTest.ToString + ' then' + sLineBreak;
  if not Supports(FIfTrue, IBlockExpression) then
    ExpressionManager.Indent();
  Result := Result + GetIndentation + FIfTrue.ToString;
  if not Supports(FIfTrue, IBlockExpression) then
    ExpressionManager.Unindent();
  if not Supports(FIfFalse, IDefaultExpression) then
  begin
    Result := Result + sLineBreak + GetIndentation + 'else' + sLineBreak;
    if not Supports(FIfFalse, IBlockExpression) then
      ExpressionManager.Indent();
    Result := Result + GetIndentation + FIfFalse.ToString;
    if not Supports(FIfFalse, IBlockExpression) then
      ExpressionManager.Unindent();
  end;
end;

{ TLogicalNotExpression }

function TLogicalNotExpression.CompileExpression: TCompiledExpression;
var
  Expression: IUnaryExpression;
  Delegate: TCompiledExpression;
begin
  Expression := Self;
  Delegate := FExpression.Compile();

  Result :=
    function: TValue
    begin
      if Assigned(Expression) then;

      Result := not Delegate().AsBoolean;
    end;
end;

function TLogicalNotExpression.ToString: string;
begin
  Result := 'not ' + FExpression.ToString;
end;

{ TBreakExpression }

function TBreakExpression.CompileExpression: TCompiledExpression;
var
  Expression: IExpression;
begin
  Expression := Self;

  Result :=
    function: TValue
    begin
      if Assigned(Expression) then;

      raise EBreak.Create('');
    end;
end;

function TBreakExpression.ToString: string;
begin
  Result := 'Break;';
end;

{ TLoopExpression }

function TLoopExpression.CompileExpression: TCompiledExpression;
var
  Expression: IUnaryExpression;
  Delegate: TCompiledExpression;
begin
  Expression := Self;
  Delegate := FExpression.Compile();

  Result :=
    function: TValue
    begin
      if Assigned(Expression) then;

      try
        repeat
          Result := Delegate();
        until False;
      except
        on EBreak do;
      end;
    end;
end;

function TLoopExpression.ToString: string;
begin
  Result := 'repeat' + sLineBreak;
  if not Supports(FExpression, IBlockExpression) then
    ExpressionManager.Indent();
  Result := Result + GetIndentation + FExpression.ToString + sLineBreak;
  if not Supports(FExpression, IBlockExpression) then
    ExpressionManager.Unindent();
  Result := Result + GetIndentation + 'until False';
end;

end.
