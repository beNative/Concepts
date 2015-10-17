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

unit System.InterfacedComponent;

interface

uses

  System.Classes, System.Win.VCLCom;

{ TODO: does not work yet! }

type
  TDummyVCLComObject = class(TInterfacedObject, IVCLComObject)
  public
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
    function SafeCallException(ExceptObject: TObject;
      ExceptAddr: Pointer): HResult; override;
    procedure FreeOnRelease;
  end;


//begin
//  v := TDummyVCLComObject.Create;
//  CreateVCLComObjectProc := @DoCreateVCLComObject;
//  c := TComponent.Create(nil);
//  if c.ComObject = nil then
//    halt(1);
//  c.Free;
//  v := nil;
//end.


type
  TComponentHelper = class helper for TComponent
  type
    TOwner = class(TDummyVCLComObject, IVCLComObject, IInterface)
    private
      FOwnedInstance: TObject;
    public
      procedure AfterConstruction; override;
      constructor Create(const instance: TComponent);
      destructor Destroy; override;




    end;

  public


    procedure EnableRefCount;


  end;

implementation

var
  c: TComponent;
  v: IVCLComObject;

procedure DoCreateVCLComObject(Component: TComponent);
begin
  Component.VCLComObject := Pointer(V);
end;

{ TDummyVCLComObject }

procedure TDummyVCLComObject.FreeOnRelease;
begin


end;

function TDummyVCLComObject.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDummyVCLComObject.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin

  Result := E_NOTIMPL;
end;

function TDummyVCLComObject.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Count := 0;
  Result := E_NOTIMPL;
end;

function TDummyVCLComObject.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDummyVCLComObject.SafeCallException(ExceptObject: TObject;
  ExceptAddr: Pointer): HResult;
begin
  Result := E_UNEXPECTED;
end;


procedure TComponentHelper.EnableRefCount;
begin
  Assert(not Assigned(Self.VCLComObject));
  Assert(not Assigned(Self.Owner));

  Self.FVCLComObject := TOwner.Create(Self);

end;

constructor TComponentHelper.TOwner.Create(
  const instance: TComponent);
begin

  FOwnedInstance := instance;
  inherited Create;
end;

destructor TComponentHelper.TOwner.Destroy;
begin
  (FOwnedInstance as TComponent).VCLComObject := nil;
  FOwnedInstance.Free;
  inherited;
end;






procedure TComponentHelper.TOwner.AfterConstruction;
begin
  inherited;
  // assigning to FOwnerInterface will increment this to 0
  FRefCount := -1;
end;


initialization
  v := TDummyVCLComObject.Create;
  CreateVCLComObjectProc := @DoCreateVCLComObject;

  c := TComponent.Create(nil);
  if c.ComObject = nil then
    halt(1);



finalization
  c.Free;
  v := nil;

end.
