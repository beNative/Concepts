unit BCCommon.Diff;

{ Original version: Angus Johnson for Delphi 7 - Delphi 2009. http://angusj.com/delphi/ }

interface

uses
  System.Classes;

const
  MAX_DIAGONAL = $FFFFFF;

type
  P8Bits = PByte;
  PDiags = ^TDiags;
  TDiags = array [-MAX_DIAGONAL .. MAX_DIAGONAL] of Integer;

  PIntArray = ^TIntArray;
  TIntArray = array [0 .. MAXINT div SizeOf(Integer) - 1] of Integer;
  PChrArray = ^TChrArray;
  TChrArray = array [0 .. MAXINT div SizeOf(Integer) - 1] of Char;

  TChangeKind = (ckNone, ckAdd, ckDelete, ckModify);

  PCompareRec = ^TCompareRec;

  TCompareRec = record
    Kind: TChangeKind;
    OldIndex1, OldIndex2: Integer;
    case Boolean of
      False:
        (Chr1, Chr2: Char);
      True:
        (Int1, Int2: Integer);
  end;

  PDiffVars = ^TDiffVars;

  TDiffVars = record
    Offset1: Integer;
    Offset2: Integer;
    Len1: Integer;
    Len2: Integer;
  end;

  TDiffStats = record
    Matches: Integer;
    Adds: Integer;
    Deletes: Integer;
    Modifies: Integer;
  end;

  TDiff = class(TPersistent)
  private
    FCancelled: Boolean;
    FChrs1: PChrArray;
    FChrs2: PChrArray;
    FCompareInts: Boolean;
    FCompareList: TList;
    FDiagB: PDiags;
    FDiagBufferB: Pointer;
    FDiagBufferF: Pointer;
    FDiagF: PDiags;
    FDiffList: TList;
    FDiffStats: TDiffStats;
    FExecuting: Boolean;
    FInts1: TPointerList;
    FInts2: TPointerList;
    FLastCompareRec: TCompareRec;
    function GetCompare(AIndex: Integer): TCompareRec;
    function GetCompareCount: Integer;
    function PopDiff: Boolean;
    function SnakeChrB(k, Offset1, Offset2, Len1, Len2: Integer): Boolean;
    function SnakeChrF(k, Offset1, Offset2, Len1, Len2: Integer): Boolean;
    function SnakeIntB(k, Offset1, Offset2, Len1, Len2: Integer): Boolean;
    function SnakeIntF(k, Offset1, Offset2, Len1, Len2: Integer): Boolean;
    procedure AddChangeChr(Offset1, Range: Integer; ChangeKind: TChangeKind);
    procedure AddChangeInt(Offset1, Range: Integer; ChangeKind: TChangeKind);
    procedure DiffChr(Offset1, Offset2, Len1, Len2: Integer);
    procedure DiffInt(Offset1, Offset2, Len1, Len2: Integer);
    procedure InitDiagArrays(Len1, Len2: Integer);
    procedure PushDiff(Offset1, Offset2, Len1, Len2: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    function Execute(PChrs1, PChrs2: PChar; Len1, Len2: Integer): Boolean; overload;
    function Execute(PInts1, PInts2: TPointerList; Len1, Len2: Integer): Boolean; overload;
    procedure Cancel;
    procedure Clear;
    property Cancelled: Boolean read FCancelled;
    property Compares[Index: Integer]: TCompareRec read GetCompare; default;
    property Count: Integer read GetCompareCount;
    property DiffStats: TDiffStats read FDiffStats;
  end;

implementation

uses
  Vcl.Forms;

constructor TDiff.Create;
begin
  inherited;
  FCompareList := TList.Create;
  FDiffList := TList.Create;
end;

destructor TDiff.Destroy;
begin
  Clear;
  FCompareList.Free;
  FDiffList.Free;
  inherited;
end;

function TDiff.Execute(PInts1, PInts2: TPointerList; Len1, Len2: Integer): Boolean;
var
  i, Len1Minus1: Integer;
begin
  Result := not FExecuting;
  if not Result then
    Exit;
  FCancelled := False;
  FExecuting := True;
  try
    Clear;

    Len1Minus1 := Len1 - 1;
    FCompareList.Capacity := Len1 + Len2;
    FCompareInts := True;

    GetMem(FDiagBufferF, SizeOf(Integer) * (Len1 + Len2 + 3));
    GetMem(FDiagBufferB, SizeOf(Integer) * (Len1 + Len2 + 3));

    FInts1 := PInts1;
    FInts2 := PInts2;
    try
      PushDiff(0, 0, Len1, Len2);
      while PopDiff do;
    finally
      FreeMem(FDiagBufferF);
      FreeMem(FDiagBufferB);
    end;

    if FCancelled then
    begin
      Result := False;
      Clear;
      Exit;
    end;

    for i := 1 to Count - 1 do
      with PCompareRec(FCompareList[i])^ do
        if (Kind = ckModify) and (Int1 = Int2) then
        begin
          Kind := ckNone;
          Dec(FDiffStats.Modifies);
          Inc(FDiffStats.Matches);
        end;

    with FLastCompareRec do
      AddChangeInt(OldIndex1, Len1Minus1 - OldIndex1, ckNone);
  finally
    FExecuting := False;
  end;
end;

function TDiff.Execute(PChrs1, PChrs2: PChar; Len1, Len2: Integer): Boolean;
var
  i, Len1Minus1: Integer;
begin
  Result := not FExecuting;
  if not Result then
    Exit;
  FCancelled := False;
  FExecuting := True;
  try
    Clear;

    Len1Minus1 := Len1 - 1;
    FCompareList.Capacity := Len1 + Len2;
    FDiffList.Capacity := 1024;
    FCompareInts := False;

    GetMem(FDiagBufferF, SizeOf(Integer) * (Len1 + Len2 + 3));
    GetMem(FDiagBufferB, SizeOf(Integer) * (Len1 + Len2 + 3));
    FChrs1 := Pointer(PChrs1);
    FChrs2 := Pointer(PChrs2);
    try
      PushDiff(0, 0, Len1, Len2);
      while PopDiff do;
    finally
      FreeMem(FDiagBufferF);
      FreeMem(FDiagBufferB);
    end;

    if FCancelled then
    begin
      Result := False;
      Clear;
      Exit;
    end;

    for i := 1 to Count - 1 do
      with PCompareRec(FCompareList[i])^ do
        if (Kind = ckModify) and (Chr1 = Chr2) then
        begin
          Kind := ckNone;
          Dec(FDiffStats.Modifies);
          Inc(FDiffStats.Matches);
        end;

    with FLastCompareRec do
      AddChangeChr(OldIndex1, Len1Minus1 - OldIndex1, ckNone);
  finally
    FExecuting := False;
  end;
end;

procedure TDiff.PushDiff(Offset1, Offset2, Len1, Len2: Integer);
var
  LDiffVars: PDiffVars;
begin
  New(LDiffVars);
  LDiffVars.Offset1 := Offset1;
  LDiffVars.Offset2 := Offset2;
  LDiffVars.Len1 := Len1;
  LDiffVars.Len2 := Len2;
  FDiffList.Add(LDiffVars);
end;

function TDiff.PopDiff: Boolean;
var
  LDiffVars: PDiffVars;
  LIndex: Integer;
begin
  LIndex := FDiffList.Count - 1;
  Result := LIndex >= 0;
  if not Result then
    Exit;
  LDiffVars := PDiffVars(FDiffList[LIndex]);
  with LDiffVars^ do
    if FCompareInts then
      DiffInt(Offset1, Offset2, Len1, Len2)
    else
      DiffChr(Offset1, Offset2, Len1, Len2);
  Dispose(LDiffVars);
  FDiffList.Delete(LIndex);
end;

procedure TDiff.InitDiagArrays(Len1, Len2: Integer);
var
  i: Integer;
begin
  // assumes that top and bottom matches have been excluded
  P8Bits(FDiagF) := P8Bits(FDiagBufferF) - SizeOf(Integer) * (MAX_DIAGONAL - (Len1 + 1));
  for i := -(Len1 + 1) to (Len2 + 1) do
    FDiagF[i] := -MAXINT;
  FDiagF[1] := -1;

  P8Bits(FDiagB) := P8Bits(FDiagBufferB) - SizeOf(Integer) * (MAX_DIAGONAL - (Len1 + 1));
  for i := -(Len1 + 1) to (Len2 + 1) do
    FDiagB[i] := MAXINT;
  FDiagB[Len2 - Len1 + 1] := Len2;
end;

procedure TDiff.DiffInt(Offset1, Offset2, Len1, Len2: Integer);
var
  p, k, Delta: Integer;
begin
  // trim matching bottoms ...
  while (Len1 > 0) and (Len2 > 0) and (FInts1[Offset1] = FInts2[Offset2]) do
  begin
    Inc(Offset1);
    Inc(Offset2);
    Dec(Len1);
    Dec(Len2);
  end;
  // trim matching tops ...
  while (Len1 > 0) and (Len2 > 0) and (FInts1[Offset1 + Len1 - 1] = FInts2[Offset2 + Len2 - 1]) do
  begin
    Dec(Len1);
    Dec(Len2);
  end;

  // stop diff'ing if minimal conditions reached ...
  if Len1 = 0 then
  begin
    AddChangeInt(Offset1, Len2, ckAdd);
    Exit;
  end
  else
  if Len2 = 0 then
  begin
    AddChangeInt(Offset1, Len1, ckDelete);
    Exit;
  end
  else
  if (Len1 = 1) and (Len2 = 1) then
  begin
    AddChangeInt(Offset1, 1, ckDelete);
    AddChangeInt(Offset1, 1, ckAdd);
    Exit;
  end;

  p := -1;
  Delta := Len2 - Len1;
  InitDiagArrays(Len1, Len2);
  if Delta < 0 then
    repeat
      Inc(p);
      if p mod 1024 = 1023 then
      begin
        Application.ProcessMessages;
        if FCancelled then
          Exit;
      end;
      for k := p downto Delta + 1 do
        if SnakeIntF(k, Offset1, Offset2, Len1, Len2) then
          Exit;
      for k := -p + Delta to Delta - 1 do
        if SnakeIntF(k, Offset1, Offset2, Len1, Len2) then
          Exit;
      for k := Delta - p to -1 do
        if SnakeIntB(k, Offset1, Offset2, Len1, Len2) then
          Exit;
      for k := p downto 1 do
        if SnakeIntB(k, Offset1, Offset2, Len1, Len2) then
          Exit;
      if SnakeIntF(Delta, Offset1, Offset2, Len1, Len2) then
        Exit;
      if SnakeIntB(0, Offset1, Offset2, Len1, Len2) then
        Exit;
    until (False)
  else
    repeat
      Inc(p);
      if p mod 1024 = 1023 then
      begin
        Application.ProcessMessages;
        if FCancelled then
          Exit;
      end;
      for k := -p to Delta - 1 do
        if SnakeIntF(k, Offset1, Offset2, Len1, Len2) then
          Exit;
      for k := p + Delta downto Delta + 1 do
        if SnakeIntF(k, Offset1, Offset2, Len1, Len2) then
          Exit;
      for k := Delta + p downto 1 do
        if SnakeIntB(k, Offset1, Offset2, Len1, Len2) then
          Exit;
      for k := -p to -1 do
        if SnakeIntB(k, Offset1, Offset2, Len1, Len2) then
          Exit;
      if SnakeIntF(Delta, Offset1, Offset2, Len1, Len2) then
        Exit;
      if SnakeIntB(0, Offset1, Offset2, Len1, Len2) then
        Exit;
    until False;
end;

procedure TDiff.DiffChr(Offset1, Offset2, Len1, Len2: Integer);
var
  p, k, Delta: Integer;
begin
  // trim matching bottoms ...
  while (Len1 > 0) and (Len2 > 0) and (FChrs1[Offset1] = FChrs2[Offset2]) do
  begin
    Inc(Offset1);
    Inc(Offset2);
    Dec(Len1);
    Dec(Len2);
  end;
  // trim matching tops ...
  while (Len1 > 0) and (Len2 > 0) and (FChrs1[Offset1 + Len1 - 1] = FChrs2[Offset2 + Len2 - 1]) do
  begin
    Dec(Len1);
    Dec(Len2);
  end;

  if Len1 = 0 then
  begin
    AddChangeChr(Offset1, Len2, ckAdd);
    Exit;
  end
  else
  if Len2 = 0 then
  begin
    AddChangeChr(Offset1, Len1, ckDelete);
    Exit;
  end
  else
  if (Len1 = 1) and (Len2 = 1) then
  begin
    AddChangeChr(Offset1, 1, ckDelete);
    AddChangeChr(Offset1, 1, ckAdd);
    Exit;
  end;

  p := -1;
  Delta := Len2 - Len1;
  InitDiagArrays(Len1, Len2);
  if Delta < 0 then
    repeat
      Inc(p);
      if p mod 1024 = 1023 then
      begin
        Application.ProcessMessages;
        if FCancelled then
          Exit;
      end;
      for k := p downto Delta + 1 do
        if SnakeChrF(k, Offset1, Offset2, Len1, Len2) then
          Exit;
      for k := -p + Delta to Delta - 1 do
        if SnakeChrF(k, Offset1, Offset2, Len1, Len2) then
          Exit;
      for k := Delta - p to -1 do
        if SnakeChrB(k, Offset1, Offset2, Len1, Len2) then
          Exit;
      for k := p downto 1 do
        if SnakeChrB(k, Offset1, Offset2, Len1, Len2) then
          Exit;
      if SnakeChrF(Delta, Offset1, Offset2, Len1, Len2) then
        Exit;
      if SnakeChrB(0, Offset1, Offset2, Len1, Len2) then
        Exit;
    until (False)
  else
    repeat
      Inc(p);
      if p mod 1024 = 1023 then
      begin
        Application.ProcessMessages;
        if FCancelled then
          Exit;
      end;
      for k := -p to Delta - 1 do
        if SnakeChrF(k, Offset1, Offset2, Len1, Len2) then
          Exit;
      for k := p + Delta downto Delta + 1 do
        if SnakeChrF(k, Offset1, Offset2, Len1, Len2) then
          Exit;
      for k := Delta + p downto 1 do
        if SnakeChrB(k, Offset1, Offset2, Len1, Len2) then
          Exit;
      for k := -p to -1 do
        if SnakeChrB(k, Offset1, Offset2, Len1, Len2) then
          Exit;
      if SnakeChrF(Delta, Offset1, Offset2, Len1, Len2) then
        Exit;
      if SnakeChrB(0, Offset1, Offset2, Len1, Len2) then
        Exit;
    until (False);
end;

function TDiff.SnakeChrF(k, Offset1, Offset2, Len1, Len2: Integer): Boolean;
var
  x, y: Integer;
begin
  if FDiagF[k + 1] > FDiagF[k - 1] then
    y := FDiagF[k + 1]
  else
    y := FDiagF[k - 1] + 1;
  x := y - k;
  while (x < Len1 - 1) and (y < Len2 - 1) and (FChrs1[Offset1 + x + 1] = FChrs2[Offset2 + y + 1]) do
  begin
    Inc(x);
    Inc(y);
  end;
  FDiagF[k] := y;
  Result := (FDiagF[k] >= FDiagB[k]);
  if not Result then
    Exit;

  Inc(x);
  Inc(y);
  PushDiff(Offset1 + x, Offset2 + y, Len1 - x, Len2 - y);
  PushDiff(Offset1, Offset2, x, y);
end;

function TDiff.SnakeChrB(k, Offset1, Offset2, Len1, Len2: Integer): Boolean;
var
  x, y: Integer;
begin
  if FDiagB[k - 1] < FDiagB[k + 1] then
    y := FDiagB[k - 1]
  else
    y := FDiagB[k + 1] - 1;
  x := y - k;
  while (x >= 0) and (y >= 0) and (FChrs1[Offset1 + x] = FChrs2[Offset2 + y]) do
  begin
    Dec(x);
    Dec(y);
  end;
  FDiagB[k] := y;

  Result := FDiagB[k] <= FDiagF[k];
  if not Result then
    Exit;

  Inc(x);
  Inc(y);
  PushDiff(Offset1 + x, Offset2 + y, Len1 - x, Len2 - y);
  PushDiff(Offset1, Offset2, x, y);
end;

function TDiff.SnakeIntF(k, Offset1, Offset2, Len1, Len2: Integer): Boolean;
var
  x, y: Integer;
begin
  if FDiagF[k + 1] > FDiagF[k - 1] then
    y := FDiagF[k + 1]
  else
    y := FDiagF[k - 1] + 1;
  x := y - k;
  while (x < Len1 - 1) and (y < Len2 - 1) and (FInts1[Offset1 + x + 1] = FInts2[Offset2 + y + 1]) do
  begin
    Inc(x);
    Inc(y);
  end;
  FDiagF[k] := y;
  Result := FDiagF[k] >= FDiagB[k];
  if not Result then
    Exit;

  Inc(x);
  Inc(y);
  PushDiff(Offset1 + x, Offset2 + y, Len1 - x, Len2 - y);
  PushDiff(Offset1, Offset2, x, y);
end;

function TDiff.SnakeIntB(k, Offset1, Offset2, Len1, Len2: Integer): Boolean;
var
  x, y: Integer;
begin
  if FDiagB[k - 1] < FDiagB[k + 1] then
    y := FDiagB[k - 1]
  else
    y := FDiagB[k + 1] - 1;
  x := y - k;
  while (x >= 0) and (y >= 0) and (FInts1[Offset1 + x] = FInts2[Offset2 + y]) do
  begin
    Dec(x);
    Dec(y);
  end;
  FDiagB[k] := y;
  Result := FDiagB[k] <= FDiagF[k];
  if not Result then
    Exit;

  Inc(x);
  Inc(y);
  PushDiff(Offset1 + x, Offset2 + y, Len1 - x, Len2 - y);
  PushDiff(Offset1, Offset2, x, y);
end;

procedure TDiff.AddChangeChr(Offset1, Range: Integer; ChangeKind: TChangeKind);
var
  i, j: Integer;
  compareRec: PCompareRec;
begin
  // first, add any unchanged items into this list ...
  while (FLastCompareRec.OldIndex1 < Offset1 - 1) do
  begin
    with FLastCompareRec do
    begin
      Kind := ckNone;
      Inc(OldIndex1);
      Inc(OldIndex2);
      Chr1 := FChrs1[OldIndex1];
      Chr2 := FChrs2[OldIndex2];
    end;
    New(compareRec);
    compareRec^ := FLastCompareRec;
    FCompareList.Add(compareRec);
    Inc(FDiffStats.Matches);
  end;

  case ChangeKind of
    ckNone:
      for i := 1 to Range do
      begin
        with FLastCompareRec do
        begin
          Kind := ckNone;
          Inc(OldIndex1);
          Inc(OldIndex2);
          Chr1 := FChrs1[OldIndex1];
          Chr2 := FChrs2[OldIndex2];
        end;
        New(compareRec);
        compareRec^ := FLastCompareRec;
        FCompareList.Add(compareRec);
        Inc(FDiffStats.Matches);
      end;
    ckAdd:
      begin
        for i := 1 to Range do
        begin
          with FLastCompareRec do
          begin
            if Kind = ckDelete then
            begin
              j := FCompareList.Count - 1;
              while (j > 0) and (PCompareRec(FCompareList[j - 1]).Kind = ckDelete) do
                Dec(j);
              PCompareRec(FCompareList[j]).Kind := ckModify;
              Dec(FDiffStats.Deletes);
              Inc(FDiffStats.Modifies);
              Inc(FLastCompareRec.OldIndex2);
              PCompareRec(FCompareList[j]).OldIndex2 := FLastCompareRec.OldIndex2;
              PCompareRec(FCompareList[j]).Chr2 := FChrs2[OldIndex2];
              if j = FCompareList.Count - 1 then
                FLastCompareRec.Kind := ckModify;
              Continue;
            end;

            Kind := ckAdd;
            Chr1 := #0;
            Inc(OldIndex2);
            Chr2 := FChrs2[OldIndex2]; // ie what we added
          end;
          New(compareRec);
          compareRec^ := FLastCompareRec;
          FCompareList.Add(compareRec);
          Inc(FDiffStats.Adds);
        end;
      end;
    ckDelete:
      for i := 1 to Range do
      begin
        with FLastCompareRec do
        begin

          // check if a range of deletes are following a range of adds
          // and convert them to modifies ...
          if Kind = ckAdd then
          begin
            j := FCompareList.Count - 1;
            while (j > 0) and (PCompareRec(FCompareList[j - 1]).Kind = ckAdd) do
              Dec(j);
            PCompareRec(FCompareList[j]).Kind := ckModify;
            Dec(FDiffStats.Adds);
            Inc(FDiffStats.Modifies);
            Inc(FLastCompareRec.OldIndex1);
            PCompareRec(FCompareList[j]).OldIndex1 := FLastCompareRec.OldIndex1;
            PCompareRec(FCompareList[j]).Chr1 := FChrs1[OldIndex1];
            if j = FCompareList.Count - 1 then
              FLastCompareRec.Kind := ckModify;
            Continue;
          end;

          Kind := ckDelete;
          Chr2 := #0;
          Inc(OldIndex1);
          Chr1 := FChrs1[OldIndex1]; // ie what we deleted
        end;
        New(compareRec);
        compareRec^ := FLastCompareRec;
        FCompareList.Add(compareRec);
        Inc(FDiffStats.Deletes);
      end;
  end;
end;

procedure TDiff.AddChangeInt(Offset1, Range: Integer; ChangeKind: TChangeKind);
var
  i, j: Integer;
  compareRec: PCompareRec;
begin
  // first, add any unchanged items into this list ...
  while (FLastCompareRec.OldIndex1 < Offset1 - 1) do
  begin
    with FLastCompareRec do
    begin
      Kind := ckNone;
      Inc(OldIndex1);
      Inc(OldIndex2);
      Int1 := Integer(FInts1[OldIndex1]);
      Int2 := Integer(FInts2[OldIndex2]);
    end;
    New(compareRec);
    compareRec^ := FLastCompareRec;
    FCompareList.Add(compareRec);
    Inc(FDiffStats.Matches);
  end;

  case ChangeKind of
    ckNone:
      for i := 1 to Range do
      begin
        with FLastCompareRec do
        begin
          Kind := ckNone;
          Inc(OldIndex1);
          Inc(OldIndex2);
          Int1 := Integer(FInts1[OldIndex1]);
          Int2 := Integer(FInts2[OldIndex2]);
        end;
        New(compareRec);
        compareRec^ := FLastCompareRec;
        FCompareList.Add(compareRec);
        Inc(FDiffStats.Matches);
      end;
    ckAdd:
      for i := 1 to Range do
      begin
        with FLastCompareRec do
        begin

          // check if a range of adds are following a range of deletes
          // and convert them to modifies ...
          if Kind = ckDelete then
          begin
            j := FCompareList.Count - 1;
            while (j > 0) and (PCompareRec(FCompareList[j - 1]).Kind = ckDelete) do
              Dec(j);
            PCompareRec(FCompareList[j]).Kind := ckModify;
            Dec(FDiffStats.Deletes);
            Inc(FDiffStats.Modifies);
            Inc(FLastCompareRec.OldIndex2);
            PCompareRec(FCompareList[j]).OldIndex2 := FLastCompareRec.OldIndex2;
            PCompareRec(FCompareList[j]).Int2 := Integer(FInts2[OldIndex2]);
            if j = FCompareList.Count - 1 then
              FLastCompareRec.Kind := ckModify;
            Continue;
          end;

          Kind := ckAdd;
          Int1 := $0;
          Inc(OldIndex2);
          Int2 := Integer(FInts2[OldIndex2]); // ie what we added
        end;
        New(compareRec);
        compareRec^ := FLastCompareRec;
        FCompareList.Add(compareRec);
        Inc(FDiffStats.Adds);
      end;
    ckDelete:
      for i := 1 to Range do
      begin
        with FLastCompareRec do
        begin

          // check if a range of deletes are following a range of adds
          // and convert them to modifies ...
          if Kind = ckAdd then
          begin
            j := FCompareList.Count - 1;
            while (j > 0) and (PCompareRec(FCompareList[j - 1]).Kind = ckAdd) do
              Dec(j);
            PCompareRec(FCompareList[j]).Kind := ckModify;
            Dec(FDiffStats.Adds);
            Inc(FDiffStats.Modifies);
            Inc(FLastCompareRec.OldIndex1);
            PCompareRec(FCompareList[j]).OldIndex1 := FLastCompareRec.OldIndex1;
            PCompareRec(FCompareList[j]).Int1 := Integer(FInts1[OldIndex1]);
            if j = FCompareList.Count - 1 then
              FLastCompareRec.Kind := ckModify;
            Continue;
          end;

          Kind := ckDelete;
          Int2 := $0;
          Inc(OldIndex1);
          Int1 := Integer(FInts1[OldIndex1]); // ie what we deleted
        end;
        New(compareRec);
        compareRec^ := FLastCompareRec;
        FCompareList.Add(compareRec);
        Inc(FDiffStats.Deletes);
      end;
  end;
end;

procedure TDiff.Clear;
var
  i: Integer;
begin
  for i := 0 to FCompareList.Count - 1 do
    Dispose(PCompareRec(FCompareList[i]));
  FCompareList.Clear;
  FLastCompareRec.Kind := ckNone;
  FLastCompareRec.OldIndex1 := -1;
  FLastCompareRec.OldIndex2 := -1;
  FDiffStats.Matches := 0;
  FDiffStats.Adds := 0;
  FDiffStats.Deletes := 0;
  FDiffStats.Modifies := 0;
  FInts1 := nil;
  FInts2 := nil;
  FChrs1 := nil;
  FChrs2 := nil;
end;

function TDiff.GetCompareCount: Integer;
begin
  Result := FCompareList.Count;
end;

function TDiff.GetCompare(AIndex: Integer): TCompareRec;
begin
  Result := PCompareRec(FCompareList[AIndex])^;
end;

procedure TDiff.Cancel;
begin
  FCancelled := True;
end;

end.
