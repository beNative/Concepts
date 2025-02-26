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

unit Concepts.Spring.MultiCastEvents.Data;

{ Test class with multicast TNotifyEvent support. }

interface

uses
  System.Classes,

  Spring;

type
  TPosition = class
  private
    FOnChange : Event<TNotifyEvent>;
    FPosition : Integer;

    {$REGION 'property access methods'}
    function GetOnChange: IEvent<TNotifyEvent>;
    function GetPosition: Integer;
    procedure SetPosition(const Value: Integer);
    {$ENDREGION}

    procedure DoChange;

  public
    property Position: Integer
      read GetPosition write SetPosition;

    property OnChange : IEvent<TNotifyEvent>
      read GetOnChange;
  end;

implementation

{$REGION 'property access methods'}
function TPosition.GetOnChange: IEvent<TNotifyEvent>;
begin
  Result := FOnChange;
end;

function TPosition.GetPosition: Integer;
begin
  Result := FPosition;
end;

procedure TPosition.SetPosition(const Value: Integer);
begin
  if Value <> Position then
  begin
    FPosition := Value;
    DoChange;
  end;
end;
{$ENDREGION}

{$REGION 'event dispatch methods'}
{ Just trigger our multicast notify event. }

procedure TPosition.DoChange;
begin
  FOnChange.Invoke(Self);
end;
{$ENDREGION}

end.
