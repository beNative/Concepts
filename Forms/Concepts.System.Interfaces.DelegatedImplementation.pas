{
  Copyright (C) 2013-2021 Tim Sinaeve tim.sinaeve@gmail.com

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

unit Concepts.System.Interfaces.DelegatedImplementation;

interface

uses
  System.Classes,

  Concepts.System.Interfaces.Interfaces,
  Concepts.System.Interfaces.InterfacedObject;

type
  TInterfacedObject =
    Concepts.System.Interfaces.InterfacedObject.TMyInterfacedObject;

type
  { This simple class descends from TObject and does not implement
    IInnerInterface directly. An instance of this object will be used to
    let an outer object implement IInnerInterface through the 'implements'
    syntax. }
  TInnerObject = class(TObject)
 // TInnerObject = class(TInterfacedObject, IInnerInterface)
  public
    procedure InnerMethod;

  end;

  TOuterObject = class(TInterfacedObject, IOuterInterface, IInnerInterface)
  private
    FInnerObject    : TInnerObject;
    //FInnerInterface : IInnerInterface;

    //function GetInnerObject: IInnerInterface;

    property InnerObject: TInnerObject
      read FInnerObject implements IInnerInterface;

  public
    procedure BeforeDestruction; override;

    constructor Create(
      AIsRefCounted     : Boolean;
      AOnAddRef         : TRefCountEvent = nil;
      AOnRelease        : TRefCountEvent = nil;
      AOnQueryInterface : TNotifyEvent = nil;
      AOnDestroy        : TNotifyEvent = nil
    ); override;

    //procedure IInnerInterface.InnerMethod = MyInnerMethod;
    procedure MyInnerMethod;

    procedure OuterMethod;
  end;

implementation

uses
  Vcl.Dialogs;

{ TInnerObject }

procedure TInnerObject.InnerMethod;
begin
  ShowMessage('TInnerObject.InnerMethod called.');
end;

{ TOuterObject }

procedure TOuterObject.BeforeDestruction;
begin
  FInnerObject.Free;
  inherited BeforeDestruction;
end;

constructor TOuterObject.Create(AIsRefCounted: Boolean; AOnAddRef,
  AOnRelease: TRefCountEvent; AOnQueryInterface, AOnDestroy: TNotifyEvent);
begin
  inherited Create(AIsRefCounted, AOnAddRef, AOnRelease, AOnQueryInterface, AOnDestroy);
  FInnerObject := TInnerObject.Create;
  //FInnerInterface := TInnerObject.Create(AIsRefCounted, AOnAddRef, AOnRelease, AOnQueryInterface, AOnDestroy);
end;

//function TOuterObject.GetInnerObject: IInnerInterface;
//begin
//  Result := FInnerInterface;
//end;

procedure TOuterObject.MyInnerMethod;
begin
  ShowMessage('TOuterObject.MyInnerMethod called.');
end;

procedure TOuterObject.OuterMethod;
begin
  ShowMessage('TOuterObject.OuterMethod called.');
end;

end.
