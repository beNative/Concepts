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

unit Concepts.Types.ThreadSafe;

{ Wraps an instance of the specified type for thread safe access from
  multiple concurrent running threads of execution.

  Restricted only to stack allocated types. Objects will not be threadsafe
  as we don't lock its allocated data. }

interface

type
  IThreadSafe<T> = interface
  ['{10E11630-C0D8-4A1C-AC55-3239B29762BB}']
    function GetData: T;
    procedure SetData(const Value: T);

    property Data: T
      read GetData write SetData;
  end;

type
  ThreadSafe<T> = record
    strict private type
    TThreadSafe = class(TInterfacedObject, IThreadSafe<T>)
      strict private
        FData  : T;
        FEmpty : Boolean;

        function GetData: T;
        procedure SetData(const Value: T);

      public
        procedure AfterConstruction; override;

        property Data: T
          read GetData write SetData;
    end;

  public
    class function Create: IThreadSafe<T>; static; inline;
  end;

implementation

{ ThreadSafe<T>.TThreadSafe }

procedure ThreadSafe<T>.TThreadSafe.AfterConstruction;
begin
  inherited;
  FEmpty := True;
end;

function ThreadSafe<T>.TThreadSafe.GetData: T;
begin
  TMonitor.Enter(Self); //Since we are exposed only as an interface, Self
  try                   //is private, and therefore, OK to lock against.
    while FEmpty do
      TMonitor.Wait(Self, INFINITE);
    Result := FData;
    FEmpty := True;
    TMonitor.PulseAll(Self);
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure ThreadSafe<T>.TThreadSafe.SetData(const Value: T);
begin
  TMonitor.Enter(Self);
  try
    while not FEmpty do
      TMonitor.Wait(Self, INFINITE);
    FEmpty := False;
    FData := Value;
    TMonitor.PulseAll(Self);
  finally
    TMonitor.Exit(Self);
  end;
end;

{ ThreadSafe<T> }

class function ThreadSafe<T>.Create: IThreadSafe<T>;
begin
  Result := ThreadSafe<T>.TThreadSafe.Create;
end;

end.
