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


{ NOT USED! }

unit Concepts.RTTEye.Data;

interface

uses
  System.Rtti,

  Spring, Spring.Reflection, Spring.Collections;

type
  TParameter = class
  private
    FRttiParameter: TRttiParameter;

  protected
    function GetName: string;

  public
    constructor Create(AParameter: TRttiParameter);

    property RttiParameter: TRttiParameter
      read FRttiParameter;

    property Name: string
      read GetName;
  end;

  TMemberData = class
  private
    FRttiMember : TRttiMember;
    FParameters : IList<TParameter>;

  protected
    function GetRttiMember: TRttiMember;
    function GetParameters: IObjectList;
    function GetName: string; virtual;

  public
    constructor Create(AMember: TRttiMember); virtual;

    property RttiMember: TRttiMember
      read GetRttiMember;

    property Parameters: IObjectList
      read GetParameters;

    property Name: string
      read GetName;
  end;

  TMethodData = class(TMemberData)
  protected
    function GetRttiMethod: TRttiMethod;
    function GetName: string; override;

  public
    constructor Create(AMember: TRttiMember); override;

    property RttiMethod: TRttiMethod
      read GetRttiMethod;

    property Parameters: IObjectList
      read GetParameters;

  end;

  TTypeData = class
  private
    FMembers  : IList<TMemberData>;
    FRttiType : TRttiType;

    function GetName: string;
    function GetMembers: IObjectList;

  public
    constructor Create(AType : TRttiType);

    property Name: string
      read GetName;

    property RttiType: TRttiType
      read FRttiType;

    property Members: IObjectList
      read GetMembers;

  end;

  TReflectionData = class(TObject)
  private
    FTypes      : IList<TTypeData>;
    FReflection : TType;
    FFilter     : string;

    function GetName: string;
    procedure SetFilter(const Value: string);
    function GetTypes: IObjectList;

  public
    procedure AfterConstruction; override;

    property Types: IObjectList
      read GetTypes;

    property Filter: string
      read FFilter write SetFilter;

    property Name: string
      read GetName;

   end;

implementation

uses
  System.SysUtils;

{ TReflectionData }

procedure TReflectionData.AfterConstruction;
begin
  inherited AfterConstruction;
  FTypes := TCollections.CreateObjectList<TTypeData>;
end;

function TReflectionData.GetName: string;
begin
//
end;

function TReflectionData.GetTypes: IObjectList;
begin
  Result := FTypes as IObjectList;
end;

procedure TReflectionData.SetFilter(const Value: string);
var
  R: TRttiType;
begin
  if Value <> Filter then
  begin
    FFilter := Value;
    FTypes.Clear;
    for R in FReflection.Types.Where(
      function(const AArg: TRttiType): Boolean
      begin
        Result := AArg.IsPublicType and  AArg.QualifiedName.Contains(Value);
      end
    ) do
    begin
      FTypes.Add(TTypeData.Create(R));
    end;
  end;
end;

{ TTypeData }

constructor TTypeData.Create(AType: TRttiType);
var
 M  : TRttiMethod;
 F  : TRttiField;
 P  : TRttiProperty;
 IP : TRttiIndexedProperty;
begin
  inherited Create;
  FRttiType := AType;
  FMembers := TCollections.CreateObjectList<TMemberData>;
  for F in FRttiType.Fields do
  begin
    FMembers.Add(TMemberData.Create(F));
  end;
  for P in FRttiType.Properties do
  begin
    FMembers.Add(TMemberData.Create(P));
  end;
  for M in FRttiType.Methods do
  begin
    FMembers.Add(TMethodData.Create(M));
  end;
  for IP in FRttiType.GetIndexedProperties do
  begin
    FMembers.Add(TMemberData.Create(IP));
  end;


end;


function TTypeData.GetMembers: IObjectList;
begin
  Result := FMembers as IObjectList;
end;

function TTypeData.GetName: string;
begin
  Result := FRttiType.ToString;
end;

{ TMemberData }

constructor TMemberData.Create(AMember: TRttiMember);
var
  P : TRttiParameter;
begin
  inherited Create;
  FRttiMember := AMember;
  FParameters := TCollections.CreateObjectList<TParameter>;

  if FRttiMember.IsMethod then
  for P in TRttiMethod(FRttiMember).Parameters do
  begin
    FParameters.Add(TParameter.Create(P));
  end;
end;

function TMemberData.GetName: string;
begin
  Result := FRttiMember.ToString;
end;

function TMemberData.GetParameters: IObjectList;
begin
  Result := FParameters as IObjectList;
end;

function TMemberData.GetRttiMember: TRttiMember;
begin
  Result := FRttiMember;
end;

{ TMethodData }

constructor TMethodData.Create(AMember: TRttiMember);
begin
  inherited Create(AMember);
end;

function TMethodData.GetName: string;
begin
  Result := RttiMethod.ToString;
end;

function TMethodData.GetRttiMethod: TRttiMethod;
begin
  Result := RttiMember as TRttiMethod;
end;

{ TParameter }

constructor TParameter.Create(AParameter: TRttiParameter);
begin
  inherited Create;
  FRttiParameter := AParameter;
end;

function TParameter.GetName: string;
begin
  Result := FRttiParameter.ToString;
  //FRttiParameter.Name + ': ' + FRttiParameter.ParamType.ToString;
end;

end.
