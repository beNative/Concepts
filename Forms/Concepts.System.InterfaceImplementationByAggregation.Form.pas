{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Concepts.System.InterfaceImplementationByAggregation.Form;

{ Form demonstrating how an object can implement an interface by delegating
  the implementation to an object property.
  Why this object property is not a TInterfacedObject is explained in detail.

  Commented sections are to demonstrate what happens if the inner object holds
  a reference to the outer object (and why it is not a good idea).
}

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ActnList, Vcl.ExtCtrls;

type
  IInnerInterface = interface
  ['{2FD72733-DC03-4C0C-9674-F1ACE43A04D9}']
    procedure InnerMethod;
  end;

  IOuterInterface = interface
  ['{39007719-79BE-4320-A83A-D99A8A81618D}']
    procedure OuterMethod;
  end;

  { This simple class descends from TObject and does not implement
    IInnerInterface directly. An instance of this object will be used to
    let an outer object implement IInnerInterface through the 'implements'
    syntax. }
  TInnerObject = class
  public
    procedure InnerMethod;

  end;

  TOuterObject = class(TInterfacedObject, IOuterInterface, IInnerInterface)
  private
    FInnerObject: TInnerObject;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure OuterMethod;

    { This is the place where the magic happens. With this property declaration
      we tell the compiler to look in the TInnerObject instance for the
      implementation of IInnerInterface.

      REMARKS:
      (1) Being a direct TObject descendant, TInnerObject does not implement the
          following IInterface methods:
            - function QueryInterface(const IID: TGUID; out Obj): HResult;
            - function _AddRef: Integer; stdcall;
            - function _Release: Integer; stdcall;
          In this case this will compile because the interface is referenced
          through the outer object which does implement those methods.
      (2) It would be wrong to make TInnerObject a TInterfacedObject, because
          then the reference count of the inner object would be incorrectly
          updated because the interface is referenced through the outer object.
      (3) As an alternative to this setup, the so calles COM aggregation
          pattern can be used. In this case the inner object descends from
          TAggregatedObject, which provides a special implementation of the 3
          IInterface methods in which the reference count of the outer object
          (= the TAggregatedObject Controller property) is updated.
    }
    property InnerObject: TInnerObject
      read FInnerObject implements IInnerInterface;
  end;

type
  TfrmInterfaceImplementationByAggregation = class(TForm)
    {$REGION 'designer controls'}
    aclMain        : TActionList;
    actInnerMethod : TAction;
    actOuterMethod : TAction;
    btnInnerMethod : TButton;
    btnOuterMethod : TButton;
    pnlHeader      : TPanel;
    lblHeader      : TLabel;
    {$ENDREGION}

    procedure actInnerMethodExecute(Sender: TObject);
    procedure actOuterMethodExecute(Sender: TObject);

  private
    FInstance: IInterface;

  public
    procedure AfterConstruction; override;

  end;

implementation

{$R *.dfm}

{$REGION 'TOuterObject'}
{$REGION 'construction and destruction'}
procedure TOuterObject.AfterConstruction;
begin
  inherited AfterConstruction;
  FInnerObject := TInnerObject.Create;
end;

procedure TOuterObject.BeforeDestruction;
begin
  FInnerObject.Free;
  inherited BeforeDestruction;
end;
{$ENDREGION}

procedure TOuterObject.OuterMethod;
begin
  ShowMessage('IOuterInterface.OuterMethod called from TOuterObject instance.');
end;
{$ENDREGION}

{$REGION 'TInnerObject'}
procedure TInnerObject.InnerMethod;
begin
  ShowMessage('IInnerInterface.InnerMethod called from TOuterObject instance.');
end;
{$ENDREGION}

{$REGION 'TfrmInterfaceImplementationByAggregation'}
{$REGION 'construction and destruction'}
procedure TfrmInterfaceImplementationByAggregation.AfterConstruction;
begin
  inherited AfterConstruction;
  FInstance := TOuterObject.Create;
end;
{$ENDREGION}

{$REGION 'action handlers'}
procedure TfrmInterfaceImplementationByAggregation.actInnerMethodExecute(
  Sender: TObject);
begin
  (FInstance as IInnerInterface).InnerMethod;
end;

procedure TfrmInterfaceImplementationByAggregation.actOuterMethodExecute(
  Sender: TObject);
begin
  (FInstance as IOuterInterface).OuterMethod;
end;
{$ENDREGION}
{$ENDREGION}

end.
