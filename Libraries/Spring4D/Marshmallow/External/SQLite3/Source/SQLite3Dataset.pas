(*
* Copyright (c) 2011, Linas Naginionis
* Contacts: lnaginionis@gmail.com or support@soundvibe.net
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the <organization> nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)
unit SQLite3Dataset;

interface

uses
  SQLiteTable3, DBClient, Classes, DB, SysUtils, Generics.Collections;

type
   /// <summary>
  /// UpdateSQL component for specifying custom insert, update, delete or refresh statements
  ///  to TSQLiteDataset
  /// </summary>
  TSQLiteUpdateSQL = class(TComponent)
  private
    FInsertSQL: TStrings;
    FDeleteSQL: TStrings;
    FModifySQL: TStrings;
    FRefreshSQL: TStrings;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DeleteSQL: TStrings read FDeleteSQL write FDeleteSQL;
    property InsertSQL: TStrings read FInsertSQL write FInsertSQL;
    property ModifySQL: TStrings read FModifySQL write FModifySQL;
    property RefreshSQL: TStrings read FRefreshSQL write FRefreshSQL;
  end;

  T3<TKey, T, TValue> = record
    Key: TKey;
    Second: T;
    Value: TValue;
    constructor Create(const AKey: TKey; const ASecond: T; const AValue: TValue);
  end;

  /// <summary>
  /// Dataset based on TClientDataset for using SQLite with visual controls
  /// </summary>
  TSQLiteDataset = class(TClientDataset)
  private
    FUpdateSQL: TSQLiteUpdateSQL;
    FDB: TSQLiteDatabase;
    FStmtUpd: TSQLitePreparedStatement;
    FStmtIns: TSQLitePreparedStatement;
    FStmtDel: TSQLitePreparedStatement;
    FAutoIncField: TField;
    FAutoIncFieldName: string;
    FIniting: Boolean;
  protected
    function DoInitFields(AStmt: TSQLitePreparedStatement): TSQLiteUniTable;
    procedure DoInternalOpen(tbl: TSQLiteUniTable);
    procedure DoInternalRefreshRecord(); virtual;
    procedure SetActive(Value: Boolean); override;
    procedure GetPair(Astmt: TSQLitePreparedStatement; AList: TList<T3<string, Boolean, TField>>);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ApplyUpdates(MaxErrors: Integer; UseTrans: Boolean = True): Integer; reintroduce; overload;
    /// <summary>
    /// Commit changes to the database
    /// </summary>
    /// <returns></returns>
    function ApplyUpdates(): Integer; reintroduce; overload;
    property AutoIncField: TField read FAutoIncField;
    procedure RefreshRecord(); reintroduce;
  published
    /// <summary>
    /// Auto increment primary key fieldname value. Needed to get inserted new row's primary key value
    /// </summary>
    property AutoIncFieldName: string read FAutoIncFieldName write FAutoIncFieldName;
    property Database: TSQLiteDatabase read FDB write FDB;
    /// <summary>
    /// UpdateSQl component where update statements are defined
    /// </summary>
    property UpdateSQL: TSQLiteUpdateSQL read FUpdateSQL write FUpdateSQL;
  end;


implementation

uses
  StrUtils;

{ TSQLiteDataset }

procedure TSQLiteDataset.GetPair(Astmt: TSQLitePreparedStatement; AList: TList<T3<string, Boolean, TField>>);
var
  i, iPos: Integer;
  fld: TField;
  sField: string;
  A3: T3<string, Boolean, TField>;
begin
  for i := 1 to Astmt.BindParameterCount do
  begin
    A3.Key := Astmt.BindParameterName(i);
    A3.Second := False;
   // sField := A3.Key;

    if Length(A3.Key) > 0 then
    begin
      //trim first param letter
      sField := Copy(A3.Key, 2, Length(A3.Key)-1);
    end
    else
    begin
      raise ESQLiteException.Create('Incorrect parameter name: ' + A3.Key);
    end;

    fld := FindField(sField);
    if not Assigned(fld) then
    begin
      //parse param name
      iPos := PosEx('_', sField);
      if (iPos = 4) then   //correct format - OLD_ID
      begin
        A3.Second := SameText(Copy(sField, 1, 3), 'OLD');

        if A3.Second then
        begin
          fld := FieldByName('OLD_' + Copy(sField, 5, Length(sField)-4));
        end;
      end;
    end;

    if Assigned(fld) then
    begin
      A3.Value := fld;
      AList.Add(A3);
    end
    else
    begin
      raise ESQLiteException.Create(Format('Fieldname %S does not exist', []));
    end;
  end;
end;

procedure TSQLiteDataset.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if AComponent = Database then
      Database := nil
    else if AComponent = UpdateSQL then
      UpdateSQL := nil;
  end;
end;

function TSQLiteDataset.ApplyUpdates(MaxErrors: Integer; UseTrans: Boolean): Integer;
var
  OrigFilter: TUpdateStatusSet;
  bkm: TBookmark;
  errCount: Integer;
  dicUpd, dicIns, dicDel: TList<T3<string, Boolean, TField>>;
  uStatus: TUpdateStatus;
  oldRecNo: Integer;
  {$REGION 'Doc'}
  /// <summary>
  /// Applies updates to Sqlite
  /// </summary>
  /// <param name="AUpdateStatus"></param>
  /// <returns></returns>
  {$ENDREGION}
  function DoApplyUpdate(AUpdateStatus: TUpdateStatus): Boolean;
  var
    A3: T3<string, Boolean, TField>;
    i: Integer;
    dic: TList<T3<string, Boolean, TField>>;
    stmt: TSQLitePreparedStatement;
    iKey: Int64;
  begin
    Result := False;

    //prepare values
    case AUpdateStatus of
      usModified:
      begin
        dic := dicUpd;
        stmt := FStmtUpd;
      end;
      usInserted:
      begin
        dic := dicIns;
        stmt := FStmtIns;
      end;
      usDeleted:
      begin
        dic := dicDel;
        stmt := FStmtDel;
      end
      else
      begin
        Exit;
      end;
    end;

    for i := 1 to dic.Count do
    begin
      A3 := dic[i-1];

      if A3.Second then
        stmt.SetParamVariant(i, A3.Value.OldValue)
      else
        stmt.SetParamVariant(i, A3.Value.Value);
    end;

    try
      Result := stmt.ExecSQL;
      if AUpdateStatus = usInserted then
      begin
        if Assigned(FAutoIncField) then
        begin
          iKey := FDB.GetLastInsertRowID;

          if iKey <> 0 then
          begin
            Edit;

            FAutoIncField.Value := iKey;

            Post;
          end;


        end;


      end;
    except
      Result := False;
    end;
    
  end;

  procedure FillParams();
  begin
    GetPair(FStmtUpd, dicUpd);
    GetPair(FStmtIns, dicIns);
    GetPair(FStmtDel, dicDel);  
  end;

begin
  Result := 0;
  errCount := 0;

  if Self.State in [dsEdit, dsInsert] then
    Post;

  if (ChangeCount < 1) and not (Assigned(FUpdateSQL)) and not (Assigned(FDB)) then
    Exit;

  FAutoIncField := FindField(FAutoIncFieldName);

  bkm := Bookmark;

  OrigFilter := StatusFilter;
  DisableControls;
  oldRecNo := RecNo;

  FStmtUpd := TSQLitePreparedStatement.Create(FDB, UpdateSQL.FModifySQL.Text);
  FStmtIns := TSQLitePreparedStatement.Create(FDB, UpdateSQL.FInsertSQL.Text);
  FStmtDel := TSQLitePreparedStatement.Create(FDB, UpdateSQL.FDeleteSQL.Text);
  //lists for storing prepared fields
  //key = (pair left = ParamName, pair second = isOldValue)
  //value = TField in the dataset
  dicUpd := TList<T3<string, Boolean, TField>>.Create();
  dicIns := TList<T3<string, Boolean, TField>>.Create();
  dicDel := TList<T3<string, Boolean, TField>>.Create();

  if UseTrans then
    FDB.BeginTransaction;
  try
    FillParams;

    //StatusFilter := [usInserted, usDeleted, usModified];
    //logically should be  [usInserted, usDeleted, usModified] but then bug appears when trying to edit first column value
    StatusFilter := [];

    First;

    while not Eof do
    begin
      uStatus := UpdateStatus;
      if uStatus = usUnmodified then
      begin
        Next;
        Continue;
      end;

      if not DoApplyUpdate(uStatus) then
        Inc(errCount)
      else
        Inc(Result);

      Next;
    end;
    //check for deletions
    StatusFilter := [usDeleted];

    First;

    while not Eof do
    begin
      uStatus := UpdateStatus;
      if uStatus = usUnmodified then
      begin
        Next;
        Continue;
      end;

      if not DoApplyUpdate(uStatus) then
        Inc(errCount)
      else
        Inc(Result);

      Next;
    end;

    MergeChangeLog;

  finally
    FStmtUpd.Free;
    FStmtIns.Free;
    FStmtDel.Free;
    dicUpd.Free;
    dicIns.Free;
    dicDel.Free;

    if (UseTrans) then
    begin
      if (errCount <= MaxErrors) or (MaxErrors = 0) then
        FDB.Commit
      else
        FDB.Rollback;
    end;

    StatusFilter := OrigFilter;

    try
      if BookmarkValid(bkm) then
        Bookmark := bkm
      else
      begin
        RecNo := oldRecNo;
      end;
    except
      //spawn
    end;
    EnableControls;
  end;
end;

function TSQLiteDataset.ApplyUpdates: Integer;
begin
  Result := ApplyUpdates(0, True);
end;

constructor TSQLiteDataset.Create(AOwner: TComponent);
begin
  inherited;
  FUpdateSQL := nil;
  FDB := nil;
  FStmtUpd := nil;
  FStmtIns := nil;
  FStmtDel := nil;
  FAutoIncField := nil;
  FAutoIncFieldName := '';
  FIniting := False;
end;

destructor TSQLiteDataset.Destroy;
begin
  FUpdateSQL := nil;
  FDB := nil;
  FAutoIncField := nil;
  FAutoIncFieldName := '';
  inherited;
end;


{$HINTS OFF}
function TSQLiteDataset.DoInitFields(AStmt: TSQLitePreparedStatement): TSQLiteUniTable;
var
  i: Integer;
  fType: TFieldType;
begin
  Result := nil;
  DisableControls;
  FIniting := True;
  try
    for i := 1 to Self.Params.Count do
    begin
      AStmt.SetParamVariant(i, Self.Params[i-1].Value);
    end;

    Result := AStmt.ExecQuery;
    //define DS
    FieldDefs.Clear;
    for i := 0 to Result.FieldCount - 1 do
    begin
      fType := SQLiteDataTypeToDelphiFieldType(Result.Fields[i].FieldType);
      case fType of
        ftString, ftWideString:
        begin
          {TODO -oLinas -cGeneral : make property to choose default string field size}
          FieldDefs.Add(Result.Fields[i].Name, fType, 50);
        end
        else
        begin
          FieldDefs.Add(Result.Fields[i].Name, fType);
        end;
      end;

    end;

    CreateDataSet;

  finally
    EnableControls;
    FIniting := False;
  end;
end;
{$HINTS ON}

procedure TSQLiteDataset.DoInternalOpen(tbl: TSQLiteUniTable);
var
  i: Integer;
begin
  if not Assigned(tbl) and (FIniting) then
    Exit;

  DisableControls;
  try
    while not tbl.EOF do
    begin
      Append;

      for i := 0 to tbl.FieldCount - 1 do
      begin
        Fields[i].Value := tbl.Fields[i].Value;
      end;

      Post;

      tbl.Next;
    end;

    First;

  finally
    EnableControls;
  end;
end;

procedure TSQLiteDataset.DoInternalRefreshRecord;
var
  stmt: TSQLitePreparedStatement;
  tbl: TSQLiteUniTable;
  i: Integer;
  AList: TList<T3<string, Boolean, TField>>;
  A3: T3<string, Boolean, TField>;
begin
  stmt := TSQLitePreparedStatement.Create(FDB, FUpdateSQL.RefreshSQL.Text);
  AList := TList<T3<string, Boolean, TField>>.Create;
  try
    GetPair(stmt, AList);

    for i := 1 to AList.Count do
    begin
      A3 := AList[i-1];

      if A3.Second then
        stmt.SetParamVariant(i, A3.Value.OldValue)
      else
        stmt.SetParamVariant(i, A3.Value.Value);
    end;

    tbl := stmt.ExecQuery;
    try
      if not tbl.EOF then
      begin
        Edit;
        for i := 0 to tbl.FieldCount - 1 do
        begin
          FieldByName(tbl.Fields[i].Name).Value := tbl.Fields[i].Value;
        end;

        Post;
      end
      else
      begin
        //record deleted
        Delete;
      end;
    finally
      tbl.Free;
    end;

  finally
    stmt.Free;
    AList.Free;
  end;
end;

procedure TSQLiteDataset.RefreshRecord;
begin
  CheckActive;

  if State in [dsInsert] then
    Exit;

  if State in [dsEdit] then
    Post;

  UpdateCursorPos;

  DoInternalRefreshRecord;
end;

procedure TSQLiteDataset.SetActive(Value: Boolean);
var
  tbl: TSQLiteUniTable;
  stmt : TSQLitePreparedStatement;
  oldLogChanges: Boolean;
begin
  if (Active <> Value) and not (csReading in ComponentState) and not (FIniting) then
  begin
    tbl := nil;
    stmt := nil;
    if Value then
    begin
      stmt := TSQLitePreparedStatement.Create(FDB, CommandText);
      tbl := DoInitFields(stmt);
    end
    else
    begin
      Self.CancelUpdates;
    end;



    inherited;

    if Value then
    begin
      oldLogChanges := LogChanges;
      LogChanges := False;
      try
        DoInternalOpen(tbl);
      finally
        LogChanges := oldLogChanges;
      end;
     // MergeChangeLog;  //slows down drastically!
    end;

    if Assigned(tbl) then
      tbl.Free;

    if Assigned(stmt) then
      stmt.Free;
  end;
end;

{ TSQLiteUpdateSQL }

constructor TSQLiteUpdateSQL.Create(AOwner: TComponent);
begin
  inherited;
  FDeleteSQL := TStringList.Create;
  FInsertSQL := TStringList.Create;
  FModifySQL := TStringList.Create;
  FRefreshSQL := TStringList.Create;
end;

destructor TSQLiteUpdateSQL.Destroy;
begin
  FDeleteSQL.Free;
  FInsertSQL.Free;
  FModifySQL.Free;
  FRefreshSQL.Free;
  inherited;
end;

{ TPair<TKey, TKey, TValue> }

constructor T3<TKey, T, TValue>.Create(const AKey: TKey; const ASecond: T; const AValue: TValue);
begin
  Key := AKey;
  Second := ASecond;
  Value := AValue;
end;

end.
