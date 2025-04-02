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

unit Concepts.Spring.ClassProxy.Form;

{ Demonstrates how to intercept methods using Spring4D. }

{
  REMARKS:
    - only virtual methods can be intercepted with a classproxy. Even dynamic
      methods are not supported.
    - an interfaceproxy supports interception on all implemented methods. The
      interface needs to descend from IInvokable or be compiled with $M+.
}

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList, Vcl.StdCtrls,

  Spring.Interception;

type
  INumber = interface(IInvokable)
  ['{E9BC5C76-59FC-434B-B792-3495D36395C9}']
    function One: Integer;
    function Two: Integer;
    function Three: Integer;

  end;

  TNumber = class(TInterfacedObject, INumber)
  public
    function One: Integer; virtual;
    function Two: Integer; dynamic;
    function Three: Integer;
    class function Four: Integer; virtual;

  end;

  TInterceptor = class(TInterfacedObject, IInterceptor)
  public
    procedure Intercept(const AInvocation: IInvocation);

  end;

  TfrmClassProxy = class(TForm)
    {$REGION 'designer controls'}
    aclMain                      : TActionList;
    btnAddFormProxy              : TButton;
    btnReleaseFormProxy          : TButton;
    actCallClassProxyMethods     : TAction;
    actCallInterfaceProxyMethods : TAction;
    {$ENDREGION}

    procedure actCallClassProxyMethodsExecute(Sender: TObject);
    procedure actCallInterfaceProxyMethodsExecute(Sender: TObject);

  private
    FInterfaceProxy : INumber;
    FClassProxy     : TNumber;
    FNumber         : INumber;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

implementation

uses
  System.Rtti,

  DDuce.Logger;

{$R *.dfm}

{$REGION 'construction and destruction'}
procedure TfrmClassProxy.AfterConstruction;
begin
  inherited AfterConstruction;

  { Only virtual methods can be intercepted }
  FClassProxy := TProxyGenerator.CreateClassProxy<TNumber>(
    [TInterceptor.Create]
  );

  //FClassProxy := TProxyGenerator.CreateClassProxy()

  { All implemented methods of INumber can be intercepted }
  FNumber := TNumber.Create;
  FInterfaceProxy := TProxyGenerator.CreateInterfaceProxyWithTarget<INumber>(
    FNumber,
    [TInterceptor.Create]
  )
end;

procedure TfrmClassProxy.BeforeDestruction;
begin
  FreeAndNil(FClassProxy);
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmClassProxy.actCallClassProxyMethodsExecute(Sender: TObject);
begin
   FClassProxy.One;   // only this call will be intercepted
   FClassProxy.Two;
   FClassProxy.Three;
   FClassProxy.Four;
end;

procedure TfrmClassProxy.actCallInterfaceProxyMethodsExecute(Sender: TObject);
begin
  FInterfaceProxy.One;
  FInterfaceProxy.Two;
  FInterfaceProxy.Three;
end;
{$ENDREGION}

{$REGION 'TInterceptor'}
procedure TInterceptor.Intercept(const AInvocation: IInvocation);
begin
  Logger.Track(Self, 'Intercept');
  Logger.Track(Self, AInvocation.Method.Name);
  Logger.Send('AInvocation.Arguments', TValue.From(AInvocation.Arguments));
  //Logger.SendInterface('AInvocation', AInvocation);

  Logger.SendObject('AInvocation.Method', AInvocation.Method);
  Logger.Send('AInvocation.Result', AInvocation.Result);
  Logger.SendObject('AInvocation.Target', AInvocation.Target.AsObject);


  Logger.Info('AInvocation.Proceed');
  AInvocation.Proceed;
  Logger.Send('AInvocation.Result', AInvocation.Result);
end;
{$ENDREGION}

{$REGION 'TNumber'}
function TNumber.One: Integer;
begin
  Result := 1;
end;

function TNumber.Two: Integer;
begin
  Result := 2;
end;

function TNumber.Three: Integer;
begin
  Result := 3;
end;

class function TNumber.Four: Integer;
begin
  Result := 4;
end;
{$ENDREGION}

end.
