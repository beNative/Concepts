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

unit DDuce.Logger.Channels.Base;

{$I DDuce.inc}

interface

uses
  DDuce.Logger.Interfaces;

type
  TCustomLogChannel = class(TInterfacedObject, ILogChannel)
  strict private
    FActive: Boolean;

  strict protected
    function GetActive: Boolean; virtual;
    procedure SetActive(const Value: Boolean); virtual;

  public
    procedure Clear; virtual; abstract;
    procedure Write(const AMsg: TLogMessage); virtual; abstract;

    property Active: Boolean
      read GetActive write SetActive;
  end;

implementation

{$REGION 'property access methods'}
function TCustomLogChannel.GetActive: Boolean;
begin
  Result := FActive;
end;

procedure TCustomLogChannel.SetActive(const Value: Boolean);
begin
  FActive := Value;
end;
{$ENDREGION}

end.
