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

unit Concepts.Spring.MultiCastEvents.Data;

{ Test component with multicast TNotifyEvent support. }

interface

uses
  System.Classes,

  Spring;

type
  TTestComponent = class(TComponent)
  private
    FOnChange: Event<TNotifyEvent>;
    function GetOnChange: IEvent<TNotifyEvent>;

  public
    procedure Change;

    property OnChange : IEvent<TNotifyEvent>
      read GetOnChange;
  end;

implementation

{$REGION 'property access methods'}
function TTestComponent.GetOnChange: IEvent<TNotifyEvent>;
begin
  Result := FOnChange;
end;
{$ENDREGION}

{$REGION 'public methods'}
{ Just trigger our multicast notify event. }

procedure TTestComponent.Change;
begin
  FOnChange.Invoke(Self);
end;
{$ENDREGION}

end.
