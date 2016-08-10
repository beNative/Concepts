unit BCEditor.KeyboardHandler;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, BCEditor.Types;

type
  TBCEditorMethodList = class
  strict private
    FData: TList;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): TMethod;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(AHandler: TMethod);
    procedure Remove(AHandler: TMethod);
    property Count: Integer read GetCount;
    property Items[Aindex: Integer]: TMethod read GetItem; default;
  end;

  TBCEditorKeyboardHandler = class(TObject)
  strict private
    FInKeyDown: Boolean;
    FInKeyPress: Boolean;
    FInKeyUp: Boolean;
    FInMouseCursor: Boolean;
    FInMouseDown: Boolean;
    FInMouseUp: Boolean;
    FKeyDownChain: TBCEditorMethodList;
    FKeyPressChain: TBCEditorMethodList;
    FKeyUpChain: TBCEditorMethodList;
    FMouseCursorChain: TBCEditorMethodList;
    FMouseDownChain: TBCEditorMethodList;
    FMouseUpChain: TBCEditorMethodList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddKeyDownHandler(AHandler: TKeyEvent);
    procedure AddKeyPressHandler(AHandler: TBCEditorKeyPressWEvent);
    procedure AddKeyUpHandler(AHandler: TKeyEvent);
    procedure AddMouseCursorHandler(AHandler: TBCEditorMouseCursorEvent);
    procedure AddMouseDownHandler(AHandler: TMouseEvent);
    procedure AddMouseUpHandler(AHandler: TMouseEvent);
    procedure ExecuteKeyDown(ASender: TObject; var Key: Word; Shift: TShiftState);
    procedure ExecuteKeyPress(ASender: TObject; var Key: Char);
    procedure ExecuteKeyUp(ASender: TObject; var Key: Word; Shift: TShiftState);
    procedure ExecuteMouseCursor(ASender: TObject; const ALineCharPos: TBCEditorTextPosition; var ACursor: TCursor);
    procedure ExecuteMouseDown(ASender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ExecuteMouseUp(ASender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure RemoveKeyDownHandler(AHandler: TKeyEvent);
    procedure RemoveKeyPressHandler(AHandler: TBCEditorKeyPressWEvent);
    procedure RemoveKeyUpHandler(AHandler: TKeyEvent);
    procedure RemoveMouseCursorHandler(AHandler: TBCEditorMouseCursorEvent);
    procedure RemoveMouseDownHandler(AHandler: TMouseEvent);
    procedure RemoveMouseUpHandler(AHandler: TMouseEvent);
  end;

implementation

uses
  BCEditor.Consts;

{ TBCEditorMethodList }

constructor TBCEditorMethodList.Create;
begin
  inherited;

  FData := TList.Create;
end;

destructor TBCEditorMethodList.Destroy;
begin
  FData.Free;

  inherited;
end;

function TBCEditorMethodList.GetCount: Integer;
begin
  Result := FData.Count div 2;
end;

function TBCEditorMethodList.GetItem(AIndex: Integer): TMethod;
begin
  AIndex := AIndex * 2;
  Result.Data := FData[AIndex];
  Result.Code := FData[AIndex + 1];
end;

procedure TBCEditorMethodList.Add(AHandler: TMethod);
begin
  FData.Add(AHandler.Data);
  FData.Add(AHandler.Code);
end;

procedure TBCEditorMethodList.Remove(AHandler: TMethod);
var
  i: Integer;
begin
  i := FData.Count - 2;
  while i >= 0 do
  begin
    if (FData.List[i] = AHandler.Data) and (FData.List[i + 1] = AHandler.Code) then
    begin
      FData.Delete(i);
      FData.Delete(i);
      Exit;
    end;
    Dec(i, 2);
  end;
end;

{ TBCEditorKeyboardHandler }

constructor TBCEditorKeyboardHandler.Create;
begin
  inherited;

  FKeyDownChain := TBCEditorMethodList.Create;
  FKeyUpChain := TBCEditorMethodList.Create;
  FKeyPressChain := TBCEditorMethodList.Create;
  FMouseDownChain := TBCEditorMethodList.Create;
  FMouseUpChain := TBCEditorMethodList.Create;
  FMouseCursorChain := TBCEditorMethodList.Create;
end;

destructor TBCEditorKeyboardHandler.Destroy;
begin
  FKeyPressChain.Free;
  FKeyDownChain.Free;
  FKeyUpChain.Free;
  FMouseDownChain.Free;
  FMouseUpChain.Free;
  FMouseCursorChain.Free;

  inherited Destroy;
end;

procedure TBCEditorKeyboardHandler.AddKeyDownHandler(AHandler: TKeyEvent);
begin
  FKeyDownChain.Add(TMethod(AHandler));
end;

procedure TBCEditorKeyboardHandler.AddKeyUpHandler(AHandler: TKeyEvent);
begin
  FKeyUpChain.Add(TMethod(AHandler));
end;

procedure TBCEditorKeyboardHandler.AddKeyPressHandler(AHandler: TBCEditorKeyPressWEvent);
begin
  FKeyPressChain.Add(TMethod(AHandler));
end;

procedure TBCEditorKeyboardHandler.AddMouseDownHandler(AHandler: TMouseEvent);
begin
  FMouseDownChain.Add(TMethod(AHandler));
end;

procedure TBCEditorKeyboardHandler.AddMouseUpHandler(AHandler: TMouseEvent);
begin
  FMouseUpChain.Add(TMethod(AHandler));
end;

procedure TBCEditorKeyboardHandler.AddMouseCursorHandler(AHandler: TBCEditorMouseCursorEvent);
begin
  FMouseCursorChain.Add(TMethod(AHandler));
end;

procedure TBCEditorKeyboardHandler.ExecuteKeyDown(ASender: TObject; var Key: Word; Shift: TShiftState);
var
  i: Integer;
begin
  if FInKeyDown then
    Exit;
  FInKeyDown := True;
  try
    with FKeyDownChain do
    begin
      for i := Count - 1 downto 0 do
      begin
        TKeyEvent(Items[i])(ASender, Key, Shift);
        if Key = 0 then
        begin
          FInKeyDown := False;
          Exit;
        end;
      end;
    end;
  finally
    FInKeyDown := False;
  end;
end;

procedure TBCEditorKeyboardHandler.ExecuteKeyUp(ASender: TObject; var Key: Word; Shift: TShiftState);
var
  i: Integer;
begin
  if FInKeyUp then
    Exit;
  FInKeyUp := True;
  try
    with FKeyUpChain do
    begin
      for i := Count - 1 downto 0 do
      begin
        TKeyEvent(Items[i])(ASender, Key, Shift);
        if Key = 0 then
        begin
          FInKeyUp := False;
          Exit;
        end;
      end;
    end;
  finally
    FInKeyUp := False;
  end;
end;

procedure TBCEditorKeyboardHandler.ExecuteKeyPress(ASender: TObject; var Key: Char);
var
  i: Integer;
begin
  if FInKeyPress then
    Exit;
  FInKeyPress := True;
  try
    with FKeyPressChain do
    begin
      for i := Count - 1 downto 0 do
      begin
        TBCEditorKeyPressWEvent(Items[i])(ASender, Key);
        if Key = BCEDITOR_NONE_CHAR then
        begin
          FInKeyPress := False;
          Exit;
        end;
      end;
    end;
  finally
    FInKeyPress := False;
  end;
end;

procedure TBCEditorKeyboardHandler.ExecuteMouseDown(ASender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  i: Integer;
begin
  if FInMouseDown then
    Exit;
  FInMouseDown := True;
  try
    for i := FMouseDownChain.Count - 1 downto 0 do
      TMouseEvent(FMouseDownChain[i])(ASender, Button, Shift, X, Y);
  finally
    FInMouseDown := False;
  end;
end;

procedure TBCEditorKeyboardHandler.ExecuteMouseUp(ASender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  i: Integer;
begin
  if FInMouseUp then
    Exit;
  FInMouseUp := True;
  try
    for i := FMouseUpChain.Count - 1 downto 0 do
      TMouseEvent(FMouseUpChain[i])(ASender, Button, Shift, X, Y);
  finally
    FInMouseUp := False;
  end;
end;

procedure TBCEditorKeyboardHandler.ExecuteMouseCursor(ASender: TObject; const ALineCharPos: TBCEditorTextPosition;
  var ACursor: TCursor);
var
  i: Integer;
begin
  if FInMouseCursor then
    Exit;
  FInMouseCursor := True;
  try
    for i := FMouseCursorChain.Count - 1 downto 0 do
      TBCEditorMouseCursorEvent(FMouseCursorChain[i])(ASender, ALineCharPos, ACursor);
  finally
    FInMouseCursor := False;
  end;
end;

procedure TBCEditorKeyboardHandler.RemoveKeyDownHandler(AHandler: TKeyEvent);
begin
  FKeyDownChain.Remove(TMethod(AHandler));
end;

procedure TBCEditorKeyboardHandler.RemoveKeyUpHandler(AHandler: TKeyEvent);
begin
  FKeyUpChain.Remove(TMethod(AHandler));
end;

procedure TBCEditorKeyboardHandler.RemoveKeyPressHandler(AHandler: TBCEditorKeyPressWEvent);
begin
  FKeyPressChain.Remove(TMethod(AHandler));
end;

procedure TBCEditorKeyboardHandler.RemoveMouseDownHandler(AHandler: TMouseEvent);
begin
  FMouseDownChain.Remove(TMethod(AHandler));
end;

procedure TBCEditorKeyboardHandler.RemoveMouseUpHandler(AHandler: TMouseEvent);
begin
  FMouseUpChain.Remove(TMethod(AHandler));
end;

procedure TBCEditorKeyboardHandler.RemoveMouseCursorHandler(AHandler: TBCEditorMouseCursorEvent);
begin
  FMouseCursorChain.Remove(TMethod(AHandler));
end;

end.
