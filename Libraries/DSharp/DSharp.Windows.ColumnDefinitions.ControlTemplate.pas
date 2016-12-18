(*
  Copyright (c) 2011-2012, Stefan Glienke
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  - Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.
  - Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.
  - Neither the name of this library nor the names of its contributors may be
    used to endorse or promote products derived from this software without
    specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
*)

unit DSharp.Windows.ColumnDefinitions.ControlTemplate;

interface

uses
  DSharp.Core.DataTemplates,
  DSharp.Windows.ColumnDefinitions,
  DSharp.Windows.ControlTemplates,
  Rtti;

type
  TColumnDefinitionsControlTemplate = class(TControlTemplate)
  protected
    FColumnDefinitions: IColumnDefinitions;
  public
    constructor Create(AColumnDefinitions: IColumnDefinitions);

    function CustomDraw(const Item: TObject; const ColumnIndex: Integer;
      TargetCanvas: TCanvas; CellRect: TRect; ImageList: TCustomImageList;
      DrawMode: TDrawMode; IsSelected: Boolean): Boolean; override;
    function GetHint(const Item: TObject; const ColumnIndex: Integer): string; override;
    function GetImageIndex(const Item: TObject;
      const ColumnIndex: Integer): Integer; override;
    function GetItemTemplate(const Item: TObject): IDataTemplate; override;

    function GetText(const Item: TObject; const ColumnIndex: Integer): string; override;
    function GetValue(const Item: TObject; const ColumnIndex: Integer): TValue; override;
    procedure SetValue(const Item: TObject; const ColumnIndex: Integer;
      const Value: TValue); override;

    property ColumnDefinitions: IColumnDefinitions
      read FColumnDefinitions write FColumnDefinitions;
  end;

implementation

uses
  DSharp.Core.Expressions;

{ TColumnDefinitionsControlTemplate }

constructor TColumnDefinitionsControlTemplate.Create(
  AColumnDefinitions: IColumnDefinitions);
begin
  inherited Create;
  FColumnDefinitions := AColumnDefinitions;
end;

function TColumnDefinitionsControlTemplate.CustomDraw(const Item: TObject;
  const ColumnIndex: Integer; TargetCanvas: TCanvas; CellRect: TRect;
  ImageList: TCustomImageList; DrawMode: TDrawMode; IsSelected: Boolean): Boolean;
begin
  if Assigned(Item) and Assigned(FColumnDefinitions)
    and (ColumnIndex < FColumnDefinitions.Count) and (ColumnIndex > -1) then
  begin
    if Assigned(FColumnDefinitions[ColumnIndex].OnCustomDraw) then
    begin
      Result := FColumnDefinitions[ColumnIndex].OnCustomDraw(FColumnDefinitions.Owner,
        FColumnDefinitions[ColumnIndex], Item,
        TargetCanvas, CellRect, ImageList, DrawMode, IsSelected);
    end
    else
    begin
      Result := inherited;
    end;
  end
  else
  begin
    Result := inherited;
  end;
end;

function TColumnDefinitionsControlTemplate.GetHint(const Item: TObject;
  const ColumnIndex: Integer): string;
var
  LColumnDefinition: TColumnDefinition;
begin
  if Assigned(Item) and Assigned(FColumnDefinitions)
    and (ColumnIndex < FColumnDefinitions.Count) and (ColumnIndex > -1) then
  begin
    LColumnDefinition := FColumnDefinitions[ColumnIndex];

    if Assigned(LColumnDefinition.OnGetHint) then
    begin
      Result := LColumnDefinition.OnGetHint(
        FColumnDefinitions.Owner, LColumnDefinition, Item);
    end else
    if Assigned(LColumnDefinition.HintPropertyExpression) then
    begin
      LColumnDefinition.HintPropertyExpression.Instance := Item;
      Result := LColumnDefinition.HintPropertyExpression.Value.ToString;
    end else
    begin
      Result := inherited;
    end;
  end
  else
  begin
    Result := inherited;
  end;
end;

function TColumnDefinitionsControlTemplate.GetImageIndex(const Item: TObject;
  const ColumnIndex: Integer): Integer;
var
  LColumnDefinition: TColumnDefinition;
begin
  if Assigned(Item) and Assigned(FColumnDefinitions)
    and (ColumnIndex < FColumnDefinitions.Count) and (ColumnIndex > -1) then
  begin
    LColumnDefinition := FColumnDefinitions[ColumnIndex];

    if Assigned(LColumnDefinition.OnGetImageIndex) then
    begin
      Result := LColumnDefinition.OnGetImageIndex(
        FColumnDefinitions.Owner, LColumnDefinition, Item);
      Result := Result + LColumnDefinition.ImageIndexOffset;
    end else
    if Assigned(LColumnDefinition.ImageIndexPropertyExpression) then
    begin
      LColumnDefinition.ImageIndexPropertyExpression.Instance := Item;
      Result := LColumnDefinition.ImageIndexPropertyExpression.Value.AsOrdinal;
      Result := Result + LColumnDefinition.ImageIndexOffset;
    end else
    begin
      Result := inherited;
    end;
  end
  else
  begin
    Result := inherited;
  end;
end;

function TColumnDefinitionsControlTemplate.GetItemTemplate(
  const Item: TObject): IDataTemplate;
begin
  Result := inherited;

  if not Assigned(Result) then
  begin
    Result := Self;
  end;
end;

function TColumnDefinitionsControlTemplate.GetText(const Item: TObject;
  const ColumnIndex: Integer): string;
begin
  if Assigned(Item) and Assigned(FColumnDefinitions)
    and (ColumnIndex < FColumnDefinitions.Count) and (ColumnIndex > -1)
    and (FColumnDefinitions[ColumnIndex].ColumnType <> ctText) then
  begin
    Result := '';
  end
  else
  begin
    Result := inherited;
  end;
end;

function TColumnDefinitionsControlTemplate.GetValue(const Item: TObject;
  const ColumnIndex: Integer): TValue;
var
  LColumnDefinition: TColumnDefinition;
begin
  if Assigned(Item) and Assigned(FColumnDefinitions)
    and (ColumnIndex < FColumnDefinitions.Count) and (ColumnIndex > -1) then
  begin
    LColumnDefinition := FColumnDefinitions[ColumnIndex];

    if Assigned(LColumnDefinition.OnGetText) then
    begin
      Result := LColumnDefinition.OnGetText(
        FColumnDefinitions.Owner, LColumnDefinition, Item);
    end else
    if Assigned(LColumnDefinition.ValuePropertyExpression) then
    begin
      LColumnDefinition.ValuePropertyExpression.Instance := Item;
      Result := LColumnDefinition.ValuePropertyExpression.Value;
    end else
    begin
      Result := TValue.Empty;
    end;
  end
  else
  begin
    Result := inherited;
  end;
end;

procedure TColumnDefinitionsControlTemplate.SetValue(const Item: TObject;
  const ColumnIndex: Integer; const Value: TValue);
var
  LColumnDefinition: TColumnDefinition;
begin
  if Assigned(Item) and Assigned(FColumnDefinitions)
    and (ColumnIndex < FColumnDefinitions.Count) and (ColumnIndex > -1) then
  begin
    LColumnDefinition := FColumnDefinitions[ColumnIndex];

    if Assigned(LColumnDefinition.OnSetText) then
    begin
      LColumnDefinition.OnSetText(
        FColumnDefinitions.Owner, LColumnDefinition, Item, Value.ToString);
    end else
    if Assigned(LColumnDefinition.ValuePropertyExpression) then
    begin
      LColumnDefinition.ValuePropertyExpression.Instance := Item;
      LColumnDefinition.ValuePropertyExpression.Value := Value;
    end;
  end;
end;

end.
