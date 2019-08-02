{
  Copyright (C) 2013-2019 Tim Sinaeve tim.sinaeve@gmail.com

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

{$I DDuce.inc}

unit DDuce.Factories.LiveBindings;

interface

uses
  System.Classes,
  Vcl.Controls, Vcl.StdCtrls,
  Data.Bind.Components;

type
  TBindingsFactory = class sealed
    class function CreateBindScope(
      ASource       : TObject;
      AOwner        : TComponent;
      AAutoActivate : Boolean = True
    ): TBindScope; static;

    class function CreateBindExpression(
      ASourceComponent         : TComponent;  // bindscope
      const ASourceExpression  : string;
      AControlComponent        : TComponent;
      const AControlExpression : string;
      ADirection               : TExpressionDirection = dirBidirectional;
      AOwner                   : TComponent = nil // take sourcecomponent as owner
    ): TBindExpression; static;

    class function CreateEditBinding(
      ASourceComponent        : TComponent;
      const ASourceExpression : string;
      AEdit                   : TCustomEdit;
      ADirection              : TExpressionDirection = dirBidirectional;
      AOwner                  : TComponent = nil
    ): TBindExpression; static;
  end;

implementation

class function TBindingsFactory.CreateBindExpression(
  ASourceComponent: TComponent; const ASourceExpression: string;
  AControlComponent: TComponent; const AControlExpression: string;
  ADirection: TExpressionDirection; AOwner: TComponent): TBindExpression;
var
  BE : TBindExpression;
begin
  if not Assigned(AOwner) then
    AOwner := ASourceComponent;
  BE := TBindExpression.Create(AOwner);
  BE.NotifyOutputs     := True;
  BE.SourceComponent   := ASourceComponent;
  BE.SourceExpression  := ASourceExpression;
  BE.ControlComponent  := AControlComponent;
  BE.ControlExpression := AControlExpression;
  BE.Direction         := ADirection;
  Result := BE;
end;

class function TBindingsFactory.CreateBindScope(ASource: TObject;
  AOwner: TComponent; AAutoActivate: Boolean): TBindScope;
var
  BS : TBindScope;
begin
  BS := TBindScope.Create(AOwner);
  BS.AutoActivate := AAutoActivate;
  BS.DataObject   := ASource;
  Result := BS;
end;

class function TBindingsFactory.CreateEditBinding(ASourceComponent: TComponent;
  const ASourceExpression: string; AEdit: TCustomEdit;
  ADirection: TExpressionDirection; AOwner: TComponent): TBindExpression;
begin
  Result := TBindingsFactory.CreateBindExpression(
    ASourceComponent,
    ASourceExpression,
    AEdit,
    'Text',
    ADirection,
    AOwner
  );
end;

end.
