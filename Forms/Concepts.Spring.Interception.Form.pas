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

{$I Concepts.inc}

unit Concepts.Spring.Interception.Form;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.ExtCtrls;

type
  {$M+}
  IStartable = interface
  ['{F5C72DEF-9CF8-4CFB-AFD4-94C0AA4BCE04}']
    procedure Start;
  end;

  IStoppable = interface
  ['{84E7C5CA-0235-4007-9EF4-08453C951B59}']
    procedure Stop;
  end;

  IMovable = interface
  ['{6DC07540-31D9-4D67-940D-06F49BBB886F}']
    procedure Move;
  end;

  TTestObject = class(TInterfacedObject, IStartable, IStoppable)
  public
    procedure Start; virtual;
    procedure Stop; virtual;
  end;

  TMoveObject = class(TInterfacedObject, IMovable)
  public
    procedure Move; virtual;
  end;

  TfrmSpringInterception = class(TForm)
    {$REGION 'designer controls'}
    aclMain   : TActionList;
    actStart  : TAction;
    actStop   : TAction;
    actMove   : TAction;
    btnStart  : TButton;
    btnStop   : TButton;
    btnMove   : TButton;
    pnlHeader : TPanel;
    lblHeader : TLabel;
    {$ENDREGION}

    {$REGION 'action handlers'}
    procedure actStartExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure actMoveExecute(Sender: TObject);
    {$ENDREGION}

  private
    FTest : IInterface;

  public
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

uses
  Spring.Interception;

{$REGION 'construction and destruction'}
procedure TfrmSpringInterception.AfterConstruction;
var
  LOptions : TProxyGenerationOptions;
begin
  inherited AfterConstruction;
  LOptions := TProxyGenerationOptions.Default;
  LOptions.AddMixinInstance(TMoveObject.Create);
  //FTestObject := TTestObject.Create;
  //FTestObject := TProxyGenerator.CreateClassProxy(TTestObject, LOptions, []) as TTestObject;

  FTest := TProxyGenerator.CreateClassProxy<TTestObject>(LOptions, []);
//  FTest := TProxyGenerator.CreateInterfaceProxyWithTarget<IMovable>(
//    TTestObject.Create,
//    LOptions,
//    []
//  );


//  TProxyGenerator.
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmSpringInterception.actMoveExecute(Sender: TObject);
var
  LMove: IMovable;
begin
  if Supports(FTest, IMovable, LMove) then
    LMove.Move;
end;

procedure TfrmSpringInterception.actStartExecute(Sender: TObject);
begin
  (FTest as IStartable).Start;
end;

procedure TfrmSpringInterception.actStopExecute(Sender: TObject);
begin
  (FTest as IStoppable).Stop;
end;
{$ENDREGION}

{$REGION 'TTestObject'}
procedure TTestObject.Start;
begin
  ShowMessage('Start');
end;

procedure TTestObject.Stop;
begin
  ShowMessage('Stop');
end;
{$ENDREGION}

{$REGION 'TMoveObject'}
procedure TMoveObject.Move;
begin
  ShowMessage('Moving');
end;
{$ENDREGION}

end.


(*
program Project1;

{$AppType Console}

{$R *.res}

uses
  System.SysUtils,
  Spring,
  Spring.Container,
  Spring.Interception;

type
  IThingy = interface (IInvokable)
    ['{FD337CC6-03EB-4384-A027-E993AB687BF0}']
    function GetValue: String;
    procedure SetValue(const Value: String);

    property Value: String read GetValue write SetValue;
  end;

  TThingy = class (TInterfacedObject, IThingy)
  strict private
    FValue: String;

    function GetValue: String;
    procedure SetValue(const Value: String);
  end;

{ TThingy }

function TThingy.GetValue: String;
begin
  Result := FValue;
end;

procedure TThingy.SetValue(const Value: String);
begin
  FValue := Value;
end;

type
  TClassInterceptor = class(TInterfacedObject, IInterceptor)
    procedure Intercept(const Invocation: IInvocation);
  end;

  TInstanceInterceptor = class(TInterfacedObject, IInterceptor)
  private
    class var InstanceCount: Integer;
    var FNo: Integer;
    procedure Intercept(const Invocation: IInvocation);
  public
    constructor Create;
  end;

{ Main }

procedure Main;
var
  Thingy1: IThingy;
  Thingy2: IThingy;
begin
  GlobalContainer.RegisterType<TClassInterceptor,TClassInterceptor>.AsSingleton;
  GlobalContainer.RegisterType<TInstanceInterceptor>('instance');
  GlobalContainer.RegisterType<IThingy, TThingy>.InterceptedBy<TClassInterceptor>.InterceptedBy('instance');
  GlobalContainer.Build;

  Thingy1 := GlobalContainer.Resolve<IThingy>;
  Thingy2 := GlobalContainer.Resolve<IThingy>;

  Thingy1.Value := 'Value 1';
  Thingy2.Value := 'Value 2';

  WriteLn(Format('Thingy1.Value: %s', [Thingy1.Value]));
  WriteLn(Format('Thingy2.Value: %s', [Thingy2.Value]));
end;

procedure TClassInterceptor.Intercept(const Invocation: IInvocation);
begin
  Invocation.Proceed;

  if Invocation.Method.Name = 'GetValue' then
    Invocation.Result := TValue.From<String>(Invocation.Result.AsString + ' intercepted by class aspect');
end;

constructor TInstanceInterceptor.Create;
begin
  Inc(InstanceCount);
  FNo := InstanceCount;
end;

procedure TInstanceInterceptor.Intercept(const Invocation: IInvocation);
begin
  Invocation.Proceed;

  if Invocation.Method.Name = 'GetValue' then
    Invocation.Result := TValue.From<String>(Invocation.Result.AsString + ' intercepted by instance aspect ' + IntToStr(FNo));
end;

begin
  try
    Main;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;
  if DebugHook <> 0 then
  begin
    WriteLn('Press enter...');
    ReadLn;
  end;
end.
*)
