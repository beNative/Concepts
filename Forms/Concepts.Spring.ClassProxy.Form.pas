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

unit Concepts.Spring.ClassProxy.Form;

interface

{
  REMARKS:
    - only virtual methods can be intercepted with a classprocy. Even dynamic
      methods are not supported.
    - an interfaceproxy supports interception on all implemented methods. The
      interface needs to descend from IInvokable or be compiled with $M+.
}

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
    function One: Integer; virtual;
    function Two: Integer; dynamic;
    function Three: Integer;
  end;

  TInterceptor = class(TInterfacedObject, IInterceptor)
  public
    procedure Intercept(const invocation: IInvocation);
  end;

  TfrmClassProxy = class(TForm)
    aclMain                      : TActionList;
    btnAddFormProxy              : TButton;
    btnReleaseFormProxy          : TButton;
    actCallClassProxyMethods     : TAction;
    actCallInterfaceProxyMethods : TAction;

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
   FClassProxy.One;
   FClassProxy.Two;
   FClassProxy.Three;
end;

procedure TfrmClassProxy.actCallInterfaceProxyMethodsExecute(Sender: TObject);
begin
  FInterfaceProxy.One;
  FInterfaceProxy.Two;
  FInterfaceProxy.Three;
end;
{$ENDREGION}

{$REGION 'TInterceptor'}
procedure TInterceptor.Intercept(const invocation: IInvocation);
begin
  invocation.Proceed;
  ShowMessage(invocation.Result.ToString);
  Logger.Send('method', invocation.Method.Name);
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
{$ENDREGION}

end.
