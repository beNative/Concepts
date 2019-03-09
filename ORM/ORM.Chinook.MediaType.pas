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

unit ORM.Chinook.MediaType;

interface

uses
  System.Classes,

  Spring,
  Spring.Persistence.Mapping.Attributes, Spring.Persistence.Core.Graphics;

type
  [Entity]
  [Table('MediaType', '')]
  TMediaType = class(TPersistent)
  private
    FMediaTypeId : Integer;
    FName        : Nullable<string>;

  published
    [Column('MediaTypeId', [cpRequired, cpPrimaryKey, cpNotNull], 9, 0)]
    property MediaTypeId: Integer
      read FMediaTypeId write FMediaTypeId;

    [Column('Name', [], 120)]
    property Name: Nullable<string>
      read FName write FName;
  end;

implementation


end.
