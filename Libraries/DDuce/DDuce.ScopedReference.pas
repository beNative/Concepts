{
  Copyright (C) 2013-2017 Tim Sinaeve tim.sinaeve@gmail.com

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

unit DDuce.ScopedReference;

{
  Implementation of a scoped reference type (smart pointer).

  A scoped reference performs automatic freeing for enclosed objects. Once an
  object is stored in a scoped reference, it is guaranteed to be released when
  the reference goes out of scope.

  Original concept based on DeHL library by Ciobanu Alexandru. You may obtain a
  copy of this library at http://code.google.com/p/delphilhlplib/

  This simplified version only supports a fully managed instance and does not
  allow for shared or weak references to the enclosed object.

  In addition to the original implementation it features automatic instantiation
  of the enclosed object, so the managed object does not need to be created nor
  destroyed as both operations will be handled by the record instance.

  Optionally a factory function for the reference can be specified. If not
  specified the instance will be created using the default constructor of the
  object type.

  In the example 3 different objects are created to show their classname.

  The code below illustrates the traditional object creation and the equivalent
  version using scoped references.

  1. Standard way (using classic boilerplate code)
  ------------------------------------------------
      procedure ShowClassNames;
      var
        O: TObject;
        P: TPersistent;
        L: TList;
      begin
        O := TObject.Create;
        try
          P := TPersistent.Create;
          try
            L := TList.Create;
            try
              ShowMessage(O.ClassName);
              ShowMessage(P.ClassName);
              ShowMessage(L.ClassName);
            finally
              L.Free;
            end;
          finally
            P.Free;
          end;
        finally
          O.Free;
        end;
      end;

  2. Using scoped references (automatic bookkeeping of the enclosed objects)
  --------------------------------------------------------------------------
      procedure ShowClassNames;
      var
        O: Scoped<TObject>;
        P: Scoped<TPersistent>;
        L: Scoped<TList>;
      begin
        ShowMessage(O.Ref.ClassName);
        ShowMessage(P.Ref.ClassName);
        ShowMessage(L.Ref.ClassName);
      end;

  Optionally you can provide a delegate with the construction code for the
  wrapped type.
  When not provided the default parameterless constructor of the type is used.

  Example using an anonymous method as a delegate to construct the scoped type:

      procedure CreateScopedComponent;
      var
        C: Scoped<TComponent>;
      begin
        C.Create(
          function
          begin
            Result := TComponent.Create(Self);
            Result.Name := 'MyComponent';
          end
        );
        ShowMessage(C.Ref.Name);
      end;
}

{$I DDuce.inc}

interface

uses
  System.SysUtils;

type
  Scoped<T: class, constructor> = record
  private type
    IGuard = interface
      function GetInstance: T;

      property Instance: T
        read GetInstance;
    end;

    TGuard = class(TInterfacedObject, IGuard)
    private
      FInstance: T;
      function GetInstance: T;

    public
      constructor Create(const AInstance: T);
      destructor Destroy; override;

      property Instance: T
        read GetInstance;
    end;

  private
    { FGuard is an interface variable that will be <> nil when an instance is
      created (Unlike normal object reference variables interface variables in
      records are always initialized to nil.) }
    FGuard: IGuard;

    function GetRef: T; inline;

  public
    constructor Create(const ARefFactory: TFunc<T>);
    class operator Implicit(const AScoped: Scoped<T>): T; inline; static;
    procedure Reset;

    property Ref: T
      read GetRef;
  end;

implementation

{ TGuard<T> }

constructor Scoped<T>.TGuard.Create(const AInstance: T);
begin
  inherited Create;
  FInstance := AInstance;
end;

destructor Scoped<T>.TGuard.Destroy;
begin
  FInstance.Free;
  inherited;
end;

function Scoped<T>.TGuard.GetInstance: T;
begin
  Result := FInstance;
end;

{ Scoped<T> }

constructor Scoped<T>.Create(const ARefFactory: TFunc<T>);
begin
  Reset;
  FGuard := TGuard.Create(ARefFactory());
end;

function Scoped<T>.GetRef: T;
begin
  if FGuard = nil then
    FGuard := TGuard.Create(T.Create);
  Result := FGuard.Instance
end;

class operator Scoped<T>.Implicit(const AScoped: Scoped<T>): T;
begin
  Result := AScoped.GetRef;
end;

procedure Scoped<T>.Reset;
begin
  FGuard := nil;
end;

end.
