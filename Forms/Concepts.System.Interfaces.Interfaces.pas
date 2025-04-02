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

unit Concepts.System.Interfaces.Interfaces;

interface

const
  IRunnable_IID  = '{47F16423-1E0B-41C7-A9DA-925AE066BA41}';
  IRunnable_GUID : TGUID = IRunnable_IID;

type
  TRefCountEvent = procedure(
    Sender    : TObject;
    ARefCount : Integer
  ) of object;
  IInterface2 = interface;

  ITrackRefCount = interface
    ['{CFAEC497-C115-4BC9-B954-9528B772E0A4}']
    {$REGION 'property access methods'}
    function GetOnAddRef: TRefCountEvent;
    function GetOnRelease: TRefCountEvent;
    procedure SetOnAddRef(const Value: TRefCountEvent);
    procedure SetOnRelease(const Value: TRefCountEvent);
    function GetRefCount: Integer;
    function GetIsRefCounted: Boolean;
    procedure SetIsRefCounted(const Value: Boolean);
    {$ENDREGION}

    property RefCount: Integer
      read GetRefCount;

    property IsRefCounted : Boolean
      read GetIsRefCounted write SetIsRefCounted;

    property OnAddRef : TRefCountEvent
      read GetOnAddRef write SetOnAddRef;

    property OnRelease : TRefCountEvent
      read GetOnRelease write SetOnRelease;
  end;

  IRunnable = interface
  [IRunnable_IID]
    procedure Run(ARaiseException: Boolean = False);
  end;

  IInnerInterface = interface
  ['{F5D6310C-1F21-40D2-847C-1E1D7FA8632D}']
    procedure InnerMethod;
  end;

  IOuterInterface = interface
  ['{C5AB5C8E-052D-4BC8-B751-4162BB36995F}']
    procedure OuterMethod;
  end;

  IGoStop = interface(IInvokable)
  ['{DAA28BA6-2243-41BE-BC61-2A548999753A}']
    procedure Go(AInteger: Integer = 5);
    procedure Stop(const AString: string = 'TEST');
  end;

  IInterface1 = interface(ITrackRefCount)
  ['{A96617CE-FE65-4FD3-A990-C52B5A259D21}']
    function GetInterfaceReference: IInterface2;
    procedure SetInterfaceReference(const AValue: IInterface2);
    property InterfaceReference : IInterface2
      read GetInterfaceReference write SetInterfaceReference;
  end;

  IInterface2 = interface(ITrackRefCount)
  ['{21C833E1-35BE-4A76-82EA-11F9D618A799}']
    function GetInterfaceReference: IInterface1;
    procedure SetInterfaceReference(const AValue: IInterface1);
    property InterfaceReference : IInterface1
      read GetInterfaceReference write SetInterfaceReference;
  end;

implementation

end.


