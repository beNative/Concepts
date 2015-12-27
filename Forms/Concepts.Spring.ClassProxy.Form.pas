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

{$I Concepts.inc}

unit Concepts.Spring.ClassProxy.Form;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList, Vcl.StdCtrls,

  Spring.Interception.ClassProxy, Spring.Interception;

type
  TfrmClassProxy = class(TForm)
    aclMain             : TActionList;
    actAddFormProxy     : TAction;
    actReleaseFormProxy : TAction;
    lblHooked: TLabel;
    btnAddFormProxy: TButton;
    btnReleaseFormProxy: TButton;
    actShowMessage: TAction;
    btnShowMessage: TButton;

    procedure actAddFormProxyExecute(Sender: TObject);
    procedure actReleaseFormProxyExecute(Sender: TObject);
    procedure actShowMessageExecute(Sender: TObject);
  private
    FProxy : TObject;
  protected
    procedure UpdateActions; override;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

type
  { Represents the method of the type we want to intercept }
  IDoCreate = interface(IInvokable)
  ['{B9D91F15-A83E-4FB4-B0A7-706975A5985B}']
    procedure DoCreate;
  end;

  TDoCreateInterceptor = class(TInterfacedObject, IInterceptor, IDoCreate)
  public
    procedure Intercept(const invocation: IInvocation);
    procedure DoCreate;
  end;

  TDoCreateObject = class(TInterfacedObject, IDoCreate)
    procedure DoCreate; virtual;
  end;

implementation

{$R *.dfm}

var
  GString : string =  'not hooked';

{$REGION 'construction and destruction'}
procedure TfrmClassProxy.AfterConstruction;
begin
  inherited AfterConstruction;
//  FProxy := TProxyGenerator.CreateClassProxy(TDoCreateObject,
//    [TypeInfo(IDoCreate)],
//    [TDoCreateInterceptor.Create]
//  );

  FProxy := TProxyGenerator.CreateClassProxy<TDoCreateObject>(
    [TDoCreateInterceptor.Create]
  );

//  var
//  proxy: TObject;
//begin
//  proxy := TProxyGenerator.CreateClassProxy(
//    TEnsurePartnerStatusRule,
//    [TypeInfo(ISupportsInvalidation)],
//    [TInvalidationInterceptor.Create]);
//  try
//    CheckTrue(Supports(proxy, ISupportsInvalidation));
//  finally
//    proxy.Free;
//  end;

end;
procedure TfrmClassProxy.BeforeDestruction;
begin
  FreeAndNil(FProxy);
  inherited BeforeDestruction;
end;

procedure TfrmClassProxy.UpdateActions;
begin
  inherited UpdateActions;
  lblHooked.Caption := GString;
end;

{$ENDREGION}


{$REGION 'action handlers'}
procedure TfrmClassProxy.actAddFormProxyExecute(Sender: TObject);
begin
  //
end;

procedure TfrmClassProxy.actReleaseFormProxyExecute(Sender: TObject);
begin
//
end;

procedure TfrmClassProxy.actShowMessageExecute(Sender: TObject);
//var
//  F : TForm;
var
  DCO: TDoCreateObject;
begin
//  F := TForm.Create(Self);
//  F.Show;
  DCO := TDoCreateObject.Create;
  DCO.DoCreate;
  DCO.Free;

end;

{$ENDREGION}



{ TDoCreateInterceptor }

procedure TDoCreateInterceptor.DoCreate;
begin
  GString := 'Hooked';
end;

procedure TDoCreateInterceptor.Intercept(const invocation: IInvocation);
begin
  GString := 'Hooked';
  invocation.Proceed;
end;

{ TDoCreateObject }

procedure TDoCreateObject.DoCreate;
begin
  //
end;

end.
