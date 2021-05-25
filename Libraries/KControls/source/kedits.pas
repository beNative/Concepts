{ @abstract(This file is part of the KControls component suite for Delphi and Lazarus.)
  @author(Tomas Krysl)

  Copyright (c) 2020 Tomas Krysl<BR><BR>

  <B>License:</B><BR>
  This code is licensed under BSD 3-Clause Clear License, see file License.txt or https://spdx.org/licenses/BSD-3-Clause-Clear.html.
}

unit kedits; // lowercase name because of Lazarus/Linux

{$include kcontrols.inc}
{$WEAKPACKAGEUNIT ON}

interface

uses
  {$IFDEF FPC}
    LCLType, LCLIntf, LMessages, LCLProc, LResources,
  {$ELSE}
    Windows, Messages,
  {$ENDIF}
    SysUtils, Classes, Controls, Forms, Graphics, StdCtrls, ComCtrls, Dialogs,
    KFunctions, KControls, KDialogs, KLog
  {$IFDEF USE_THEMES}
    , Themes
   {$IFNDEF FPC}
    , UxTheme
   {$ENDIF}
  {$ENDIF}
    ;

const
  KM_NE_UPDATEUPDOWN = KM_BASE + 101;

type
  TKLabelPosition = (
    lpAbove,
    lpBelow,
    lpLeft,
    lpRight
  );

  TKNumberEditAcceptedFormat = (
    neafAscii,
    neafBin,
    neafDec,
    neafFloat,
    neafHex,
    neafOct
  );

  TKNumberEditAcceptedFormats = set of TKNumberEditAcceptedFormat;

  TKNumberEditDisplayedFormat = (
    nedfAsInput,
    nedfAscii,
    nedfBin,
    nedfDec,
    nedfFloat,
    nedfHex,
    nedfOct
  );

  TKNumberEditHexPrefix = (
    nehpC,
    nehpPascal
  );

  TKNumberEditOption = (
    neoKeepEmpty,
    neoLowerCase,
    neoUnsigned,
    neoUseLabel,
    neoUsePrefix,
    neoUseUpDown,
    neoWarning,
    neoClampToMinMax
  );

  TKNumberEditOptions = set of TKNumberEditOption;

const
  DefaultNumberEditOptions = [neoLowerCase, neoUseLabel, neoUsePrefix,
    neoUseUpDown, neoWarning, neoClampToMinMax];

type

  { TKNumberValue }

  TKNumberValue = class
  private
    FIVal: Int64;
    FFVal: Extended;
    FHasInt: Boolean;
    function GetFVal: Extended;
    function GetIVal: Int64;
    function GetUIVal: UInt64;
    procedure SetHasInt(const Value: Boolean);
    procedure SetIVal(const AValue: Int64);
    procedure SetFVal(const AValue: Extended);
    procedure SetUIVal(const AValue: UInt64);
  public
    constructor CreateEmpty;
    constructor CreateI(const AValue: Int64);
    constructor CreateF(const AValue: Extended);
    procedure Assign(const AValue: TKNumberValue);
    procedure Clear(AHasIntState: Boolean);
    function Clamp(const AMinimum, AMaximum: TKNumberValue; ASigned: Boolean = True): Boolean;
    function EqualsTo(const AValue: TKNumberValue): Boolean;
    function GreaterThan(const AValue: TKNumberValue; ASigned: Boolean = True): Boolean;
    function LowerThan(const AValue: TKNumberValue; ASigned: Boolean = True): Boolean;
    property IVal: Int64 read GetIVal write SetIVal;
    property FVal: Extended read GetFVal write SetFVal;
    property HasInt: Boolean read FHasInt write SetHasInt;
    property UIVal: UInt64 read GetUIVal write SetUIVal;
  end;

  { TKCustomNumberEdit }

  TKCustomNumberEdit = class(TCustomEdit)
  private
    FAcceptedFormats: TKNumberEditAcceptedFormats;
    FCustomSuffix: string;
    FDecimalSeparator: Char;
    FDisplayedFormat: TKNumberEditDisplayedFormat;
  {$IFDEF FPC}
    FFlat: Boolean;
  {$ENDIF}
    FFixedWidth: Integer;
    FHexPrefix: TKNumberEditHexPrefix;
    FLabel: TLabel;
    FLabelPosition: TKLabelPosition;
    FLabelSpacing: Cardinal;
    FLastInputFormat: TKNumberEditDisplayedFormat;
    FLog: TKLog;
    FMax: TKNumberValue;
    FMin: TKNumberValue;
    FOptions: TKNumberEditOptions;
    FPrecision: Integer;
    FRealUpDownStep: Extended;
    FUpdateUpDown: Boolean;
    FUpDown: TUpDown;
    FUpdownChanging: Boolean;
    FUpDownStep: Extended;
    FValue: TKNumberValue;
    FWarningColor: TColor;
    FOnUpDownChange: TNotifyEvent;
    procedure CMEnabledChanged(var Msg: TLMessage); message CM_ENABLEDCHANGED;
    procedure CMVisibleChanged(var Msg: TLMessage); message CM_VISIBLECHANGED;
    procedure CMBiDiModeChanged(var Msg: TLMessage); message CM_BIDIMODECHANGED;
    procedure KMNEUpdateUpDown(var Msg: TLMessage); message KM_NE_UPDATEUPDOWN;
    procedure WMPaste(var Msg: TLMPaste); message LM_PASTE;
    procedure WMKillFocus(var Msg: TLMKillFocus); message LM_KILLFOCUS;
    procedure WMMove(var Msg: TLMMove); message LM_MOVE;
    procedure WMSetFocus(var Msg: TLMSetFocus); message LM_SETFOCUS;
    procedure WMSize(var Msg: TLMSize); message LM_SIZE;
  protected
    procedure Change; override;
  {$IFDEF FPC}
    procedure CreateWnd; override;
    procedure DoOnChangeBounds; override;
  {$ENDIF}
    procedure DoWarning(AValue: TKNumberValue); virtual;
    function GetCaption: TCaption; virtual;
    procedure GetFormat(AText: string; var Fmt: TKNumberEditDisplayedFormat; AValue: TKNumberValue); virtual;
    function GetMax: Extended; virtual;
    function GetMaxAsInt: Int64;
    function GetMin: Extended; virtual;
    function GetMinAsInt: Int64; virtual;
    procedure GetPrefixSuffix(Format: TKNumberEditDisplayedFormat; out Prefix, Suffix: string); virtual;
    function GetRealSelStart: Integer; virtual;
    function GetRealSelLength: Integer; virtual;
    function GetSigned: Boolean; virtual;
    function GetValue: Extended; virtual;
    function GetValueAsInt: Int64; virtual;
    function GetValueAsText: string; virtual;
    function InspectInputChar(Key: Char): Char; virtual;
    function IsCaptionStored: Boolean; virtual;
    function IsCustomSuffixStored: Boolean; virtual;
    function IsMaxStored: Boolean; virtual;
    function IsMinStored: Boolean; virtual;
    function IsUpDownStepStored: Boolean; virtual;
    function IsValueStored: Boolean; virtual;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SafeSetFocus; virtual;
    procedure SetAcceptedFormats(AValue: TKNumberEditAcceptedFormats); virtual;
    procedure SetCaption(const AValue: TCaption); virtual;
    procedure SetCustomSuffix(const AValue: string); virtual;
    procedure SetDecimalSeparator(Value: Char); virtual;
    procedure SetDisplayedFormat(AValue: TKNumberEditDisplayedFormat); virtual;
    procedure SetFixedWidth(AValue: Integer); virtual;
  {$IFDEF FPC}
    procedure SetFlat(Value: Boolean); virtual;
  {$ENDIF}
    function SetFormat(AValue: TKNumberValue): string; virtual;
    procedure SetHexPrefix(AValue: TKNumberEditHexPrefix); virtual;
    procedure SetLabelPosition(Value: TKLabelPosition); virtual;
    procedure SetLabelSpacing(Value: Cardinal); virtual;
    procedure SetMax(AMax: Extended); virtual;
    procedure SetMaxAsInt(AMax: Int64); virtual;
    procedure SetMin(AMin: Extended); virtual;
    procedure SetMinAsInt(AMin: Int64); virtual;
    procedure SetName(const Value: TComponentName); override;
    procedure SetOptions(AValue: TKNumberEditOptions); virtual;
    procedure SetParent(AParent: TWinControl); override;
    procedure SetPrecision(AValue: Integer); virtual;
    procedure SetUpDownStep(AValue: Extended); virtual;
    procedure SetValue(AValue: Extended); virtual;
    procedure SetValueAsInt(AValue: Int64); virtual;
    procedure SetValueAsText(const AValue: string); virtual;
    procedure TextToValue; virtual;
    procedure UpdateFormats; virtual;
    procedure UpdateLabel; virtual;
    procedure UpdateMaxMin; virtual;
    procedure UpdateUpDown(AValue: TKNumberValue); virtual;
    procedure UpdateUpDownPos; virtual;
    procedure UpDownChange; virtual;
    procedure UpDownChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: {$IFDEF COMPILER19_UP}Integer{$ELSE}SmallInt{$ENDIF}; Direction: TUpDownDirection);
    procedure ValueToText; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Empty: Boolean; virtual;
    procedure Validate; virtual;
    property AcceptedFormats: TKNumberEditAcceptedFormats read FAcceptedFormats write SetAcceptedFormats default [neafDec];
    property Caption: TCaption read GetCaption write SetCaption stored IsCaptionStored;
    property CustomSuffix: string read FCustomSuffix write SetCustomSuffix stored IsCustomSuffixStored;
    property DecimalSeparator: Char read FDecimalSeparator write SetDecimalSeparator;
    property DisplayedFormat: TKNumberEditDisplayedFormat read FDisplayedFormat write SetDisplayedFormat default nedfAsInput;
    property FixedWidth: Integer read FFixedWIdth write SetFixedWidth default 0;
    property HexPrefix: TKNumberEditHexPrefix read FHexPrefix write SetHexPrefix default nehpC;
    property LabelPosition: TKLabelPosition read FLabelPosition write SetLabelPosition default lpAbove;
    property LabelSpacing: Cardinal read FLabelSpacing write SetLabelSpacing default 3;
    property LastInputFormat: TKNumberEditDisplayedFormat read FLastInputFormat write FLastInputFormat;
    property Log: TKLog read FLog write FLog;
    property Max: Extended read GetMax write SetMax stored IsMaxStored;
    property MaxAsInt: Int64 read GetMaxAsInt write SetMaxAsInt;
    property Min: Extended read GetMin write SetMin stored IsMinStored;
    property MinAsInt: Int64 read GetMinAsInt write SetMinAsInt;
    property Options: TKNumberEditOptions read FOptions write SetOptions default DefaultNumberEditOptions;
    property Precision: Integer read FPrecision write SetPrecision default 2;
    property Signed: Boolean read GetSigned;
    property UpDownStep: Extended read FUpDownStep write SetUpDownStep stored IsUpDownStepStored;
    property Value: Extended read GetValue write SetValue stored IsValueStored;
    property ValueAsInt: Int64 read GetValueAsInt write SetValueAsInt;
    property ValueAsText: string read GetValueAsText write SetValueAsText;
    property WarningColor: TColor read FWarningColor write FWarningColor default clRed;
    property OnUpDownChange: TNotifyEvent read FOnUpDownChange write FOnUpDownChange;
  end;

  { TKNumberEdit }

  TKNumberEdit = class(TKCustomNumberEdit)
  published
    property AcceptedFormats;
    property Caption;
    property CustomSuffix;
    property DecimalSeparator;
    property DisplayedFormat;
    property FixedWidth;
    property HexPrefix;
    property LabelPosition;
    property LabelSpacing;
    property Log;
    property Max;
    property MaxAsInt;
    property Min;
    property MinAsInt;
    property Options;
    property Precision;
    property UpDownStep;
    property Value;
    property ValueAsInt;
    property WarningColor;
    property OnUpDownChange;

    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    {$IFDEF FPC}
      { Specifies the same as Ctl3D in Delphi. }
      property Flat: Boolean read FFlat write SetFlat default False;
    {$ELSE}
      { Inherited property - see Delphi help. }
      property Ctl3D;
    {$ENDIF}
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
  {$IFDEF COMPILER9_UP}
    property OnMouseEnter;
    property OnMouseLeave;
  {$ENDIF}
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;


  TKFileNameEditButtonStyle = (fbNone, fbButton, fbBitBtn, fbSpeedBtn, fbUser);

  TKFileNameEditButtonAlign = (fbaRight, fbaLeft, fbaLeftDown, fbaRightDown);

  TKFileNameEditOption = (foFolderOnly, foSaveDialog, foAlwaysInitialDir, foAddToList,
    foCheckPath, foCorrectPath, foPathMustExist, foAddInitialDir,
    foCheckWithInitialDir, foWarning);

  TKFileNameEditOptions = set of TKFileNameEditOption;

  TKFileNameEditDlgProperties = class(TPersistent)
  private
    FInitialDir: TFolder;
    FDefaultExt: string;
    FFilter: string;
    FFilterIndex: Integer;
    FOpenOptions: TOpenOptions;
    FBrowseOptions: TKBrowseFolderOptions;
    FBrowseDlgLabel: string;
    function IsOpenOptionsStored: Boolean;
    function IsBrowseOptionsStored: Boolean;
  protected
  public
    constructor Create;
  published
    property BrowseDlgLabel: string read FBrowseDlgLabel write FBrowseDlgLabel;
    property BrowseOptions: TKBrowseFolderOptions read FBrowseOptions write FBrowseOptions stored IsBrowseOptionsStored;
    property DefaultExt: string read FDefaultExt write FDefaultExt;
    property Filter: string read FFilter write FFilter stored True;
    property FilterIndex: Integer read FFilterIndex write FFilterIndex default 1;
    property InitialDir: TFolder read FInitialDir write FInitialDir;
    property OpenOptions: TOpenOptions read FOpenOptions write FOpenOptions stored IsOpenOptionsStored;
  end;

  { TKFileNameEdit }

  TKFileNameEdit = class(TCustomComboBox)
  private
    FButton: TControl;
    FButtonAlign: TKFileNameEditButtonAlign;
    FButtonStyle: TKFileNameEditButtonStyle;
    FButtonText: TCaption;
    FButtonWidth: Integer;
    FButtonDist: Integer;
  {$IFDEF FPC}
    FFlat: Boolean;
  {$ENDIF}
    FLog: TKLog;
    FOptions: TKFileNameEditOptions;
    FWarningColor: TColor;
    FBtnOnClick: TNotifyEvent;
    FDlgProperties: TKFileNameEditDlgProperties;
    function GetFileName: TFileName;
    procedure SetFileName(const Value: TFileName);
    procedure SetButton(Value: TControl);
    function IsButtonStored: Boolean;
    procedure SetButtonAlign(Value: TKFileNameEditButtonAlign);
    procedure SetButtonStyle(Value: TKFileNameEditButtonStyle);
    procedure SetButtonText(const Value: TCaption);
    function IsButtonTextStored: Boolean;
    procedure SetButtonWidth(Value: Integer);
    procedure SetButtonDist(Value: Integer);
    function GetWholeWidth: Integer;
    function GetWholeLeft: Integer;
  {$IFDEF FPC}
    procedure SetFlat(Value: Boolean);
  {$ENDIF}
    procedure SetWholeWidth(Value: Integer);
    procedure SetWholeLeft(const Value: Integer);
    procedure SetOptions(Value: TKFileNameEditOptions);
    procedure UpdateButton;
    procedure CMEnabledChanged(var Msg: TLMessage); message CM_ENABLEDCHANGED;
    procedure CMVisibleChanged(var Msg: TLMessage); message CM_VISIBLECHANGED;
    procedure CMBiDiModeChanged(var Msg: TLMessage); message CM_BIDIMODECHANGED;
    procedure WMMove(var Msg: TLMMove); message LM_MOVE;
    procedure WMSize(var Msg: TLMSize); message LM_SIZE;
  protected
    procedure ButtonClick(Sender: TObject); virtual;
    procedure ButtonExit(Sender: TObject); virtual;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DropDown; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property Style; {Must be published before Items}
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Button: TControl read FButton write SetButton stored IsButtonStored;
    property ButtonAlign: TKFileNameEditButtonAlign read FButtonAlign write SetButtonAlign default fbaRight;
    property ButtonDist: Integer read FButtonDist write SetButtonDist default 8;
    property ButtonStyle: TKFileNameEditButtonStyle read FButtonStyle write SetButtonStyle default fbButton;
    property ButtonText: TCaption read FButtonText write SetButtonText stored IsButtonTextStored;
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth default 70;
    property CharCase;
    property Color;
    property Constraints;
    {$IFDEF FPC}
      { Specifies the same as Ctl3D in Delphi. }
      property Flat: Boolean read FFlat write SetFlat default False;
    {$ELSE}
      { Inherited property - see Delphi help. }
      property Ctl3D;
    {$ENDIF}
    property DlgProperties: TKFileNameEditDlgProperties read FDlgProperties;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property FileName: TFileName read GetFileName write SetFileName;
    property Font;
    property ItemHeight;
    property Log: TKLog read FLog write FLog;
    property MaxLength;
    property Options: TKFileNameEditOptions read FOptions write SetOptions default
      [foAddToList, foCheckPath, foWarning];
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Visible;
    property WarningColor: TColor read FWarningColor write FWarningColor default clRed;
    property WholeLeft: Integer read GetWholeLeft write SetWholeLeft stored False;
    property WholeWidth: Integer read GetWholeWidth write SetWholeWidth stored False;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
  {$IFDEF COMPILER9_UP}
    property OnMouseEnter;
    property OnMouseLeave;
  {$ENDIF}
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property Items; { Must be published after OnMeasureItem }
  end;

function CorrectPath(const S, InitialDir: TFileName; Options: TKFileNameEditOptions;
  out Err: Boolean; Log: TKLog): TFileName;

function CorrectSubDirName(var S: string; out Warn: Boolean; Log: TKLog): Boolean;

implementation

uses
  ClipBrd, Buttons, Math, Types, TypInfo, KMessageBox, KRes;

const
  IdStartCharSet = ['_', 'a'..'z', 'A'..'Z'];
  IdCharSet = ['0'..'9'] + IdStartCharSet;
  SubDirIllegalCharSet = [#0..#31, '<', '>', ':', '"', '/', '\', '|', '*', '?'];

function CorrectPath(const S, InitialDir: TFileName; Options: TKFileNameEditOptions;
  out Err: Boolean; Log: TKLog): TFileName;

  function FindCharFromPos(const S: string; AChar: Char; ALen: Integer; var APos: Integer): Boolean;
  var
    Bk: Integer;
  begin
    {ignore multiple backslashes}
    while (APos < ALen) and (S[APos] = AChar) do Inc(APos);
    Bk := APos;
    while (Bk < ALen) and (S[Bk] <> AChar) do Inc(Bk);
    Result := Bk < ALen;
  end;

var
  I, Len, K: Integer;
  B0, B, B1, B2, Warn, DirAdded: Boolean;
  T, T1: string;
begin
  T := S; Err := False; DirAdded := False;
  Len := Length(T); K := 1;
  B0 := False; B := False; B1 := False; B2 := False;
  for I := 1 to Len do if T[I] = '/' then T[I] := '\';
  if Len > 0 then
  begin
    {check drive}
    if CharInSetEx(UpCase(T[1]), ['A'..'Z']) and (T[2] = ':') then
    begin
      if (T[3] <> '\') then
      begin
        Insert('\', T, 3);
        Inc(Len);
      end;
      K := 4;
    end
    else if (S[1] = '.') and (S[2] = '\') then  //check current dir
      K := 3
    else if (S[1] = '.') and (S[2] = '.') and (S[3] = '\') then //check parent dir
      K := 4
   { else  //check protocol - not enabled
    begin
      I := 0;
      while not (S[I] in [':', '\']) do Inc(I);
      if (I <> 0) and (S[I] = ':') then
      begin
        T1 := LowerCase(Copy(S, 1, I - 1));
        if (T1 = 'http') or (T1 = 'ftp') or (T1 = 'gopher') or (T1 = 'mailto') or
          (T1 = 'nntp') then
          K := Length(T1) + 2;
      end;
    end};
    if K = 1 then
    begin
      if Options * [foCorrectPath, foAddInitialDir] <> [] then
        while CharInSetEx(T[1], SubDirIllegalCharSet) do Delete(T, 1, 1);
      if foAddInitialDir in Options then
      begin
        if InitialDir[Length(InitialDir)] = '\' then
          T := Format('%s%s', [InitialDir, T])
        else
          T := Format('%s\%s', [InitialDir, T]);
        Len := Length(T);
        K := Length(InitialDir) + 2;
        B0 := True;
        if Assigned(Log) then
          Log.Log(lgNote, Format(sEDCurrentDirAdded, [S]));
        DirAdded := True;
      end;
    end;
    {check subdirectories}
    while (K < Len) and (FindCharFromPos(T, '\', Len, K) or (foFolderOnly in Options)) do
    begin
      {check or correct next subdirectory}
      if K < Len then
      begin
        I := K;
        while (I < Len) and (T[I] <> '\') do Inc(I);
        if T[I] <> '\' then Inc(I);
        T1 := Copy(T, K, I - K);
        if CorrectSubDirName(T1, Warn, nil) then
        begin
          if Warn then B := True;
          if (foCorrectPath in Options) or not Warn then
          begin
            Delete(T, K, I - K);
            Insert(T1, T, K);
            Len := Length(T);
            Inc(K, Length(T1) + 1);
          end else
            Inc(K, I - K + 1);
        end else
          Inc(K, Length(T1) + 1);
      end;
    end;
    {check file name}
    if not (foFolderOnly in Options) then
    begin
      if K < Len then
      begin
        T1 := Copy(T, K, Len - K + 1);
        if CorrectSubDirName(T1, Warn, nil) then
        begin
          if Warn then B := True;
          if (foCorrectPath in Options) or not Warn then
          begin
            Delete(T, K, Len - K);
            T := T + T1;
          end;
        end;
      end else
        B1 := True;
    end;
    {check for path presence}
    if foPathMustExist in Options then
      if foFolderOnly in Options then
      begin
        if not DirectoryExists(T) then
          B2 := True;
      end else
        if not B1 then
        begin
          if not (foCheckWithInitialDir in Options) or DirAdded then
            T1 := T
          else
            if (InitialDir = '') or (ExtractFilePath(T) <> '') then
              T1 := T
            else if InitialDir[Length(InitialDir)] = '\' then
              T1 := InitialDir + T
            else
              T1 := Format('%s\%s', [InitialDir, T]);
          if not FileExists(T1) then
            B2 := True;
        end;
    {log errors}
    if Assigned(Log) then
    begin
      if B then
        if foFolderOnly in Options then
        begin
          if foCorrectPath in Options then
            Log.Log(lgInputError, Format(sEDBadDirCorr, [S, T]))
          else if foCheckPath in Options then
            Log.Log(lgWarning, Format(sEDBadDir, [T]));
        end else
          if foCorrectPath in Options then
            Log.Log(lgInputError, Format(sEDBadPathCorr, [S, T]))
          else if foCheckPath in Options then
            Log.Log(lgWarning, Format(sEDBadPath, [T]));
      if B1 and (Options * [foCheckPath, foCorrectPath] <> []) then
        Log.Log(lgWarning, sEDMissingFileName);
      if B2 then
        if foFolderOnly in Options then
          Log.Log(lgWarning, Format(sEDNoExistingDir, [T]))
        else
          Log.Log(lgWarning, Format(sEDNoExistingPath, [T]));
    end;
  end;
  Err := B0 or B or B1 or B2;
  Result := T;
end;

function CorrectSubDirName(var S: string; out Warn: Boolean; Log: TKLog): Boolean;

  function IsSpecialName(const S: string): Boolean;
  var
    T: string;
  begin
    if S[4] = '.' then T := Copy(S, 1, 3) else T := S;
    Result := (T = 'AUX') or (T = 'PRN') or (T = 'CON');
  end;

var
  I, Len: Integer;
  T: string;
begin
  Result := False;
  Warn := True;
  T := S;
  if Length(S) > 0 then
  begin
    if CharInSetEx(S[1], SubDirIllegalCharSet) then
    begin
      S[1] := '_';
      Result := True;
    end;
    I := 2;
    Len := Length(S);
    while (I <= Len) do
    begin
      if CharInSetEx(S[I], SubDirIllegalCharSet) then
      begin
        Delete(S, I, 1);
        Dec(Len);
        Dec(I);
        Result := True;
      end;
      Inc(I);
    end;
    if IsSpecialName(UpperCase(S)) then
    begin
      S := '_' + S;
      Inc(Len);
      Result := True;
    end;
    while S[Len] = '.' do
    begin
      Delete(S, Len, 1);
      Dec(Len);
      Result := True;
      Warn := False;
    end;
    if S = '.' then
    begin
      S[1] := '_';
      Result := True;
    end;
  end else
  begin
    S := '_';
    Result := True;
  end;
  if Result and Warn and Assigned(Log) then
    Log.Log(lgInputError, Format(sEDBadSubDirName, [T, S]));
end;

{ TKNumberValue }

procedure TKNumberValue.Clear(AHasIntState: Boolean);
begin
  if AHasIntState then
    IVal := 0
  else
    FVal := 0;
end;

constructor TKNumberValue.CreateEmpty;
begin
  IVal := 0;
end;

constructor TKNumberValue.CreateF(const AValue: Extended);
begin
  FVal := AValue;
end;

constructor TKNumberValue.CreateI(const AValue: Int64);
begin
  IVal := AValue;
end;

procedure TKNumberValue.Assign(const AValue: TKNumberValue);
begin
  FFVal := AValue.FFVal;
  FIVal := AValue.FIVal;
  FHasInt := AValue.FHasInt;
end;

function TKNumberValue.Clamp(const AMinimum, AMaximum: TKNumberValue; ASigned: Boolean): Boolean;
begin
  Result := False;
  if LowerThan(AMinimum, ASigned) then
  begin
    Assign(AMinimum);
    Result := True;
  end
  else if GreaterThan(AMaximum, ASigned) then
  begin
    Assign(AMaximum);
    Result := True;
  end;
end;

function TKNumberValue.EqualsTo(const AValue: TKNumberValue): Boolean;
begin
  if FHasInt then
    Result := IVal = AValue.IVal
  else
    Result := FVal = AValue.FVal;
end;

function TKNumberValue.GetFVal: Extended;
begin
  if FHasInt then
    Result := FIVal
  else
    Result := FFVal
end;

function TKNumberValue.GetIVal: Int64;
const
  cMaxInt64F =  9.223372036854775807E+18;
  cMinInt64F = -9.223372036854775808E+18;
{$IF DEFINED(FPC) OR DEFINED(COMPILER12_UP)} // maybe incorrect version, I don't know which Delphi version does not complain anymore
  cMaxInt64 =  9223372036854775807;
  cMinInt64 = -9223372036854775808;
{$IFEND}
begin
  if FHasInt then
    Result := FIVal
  else
  begin
    try
      try
        Result := Round(FFVal)
      except
        // try to clamp the value to Int64 limits
        // this requires the Extended type with sufficient precision, at least 10 bytes
        // might be not accurate when Extended is mapped to Double etc.
{$IF DEFINED(FPC) OR DEFINED(COMPILER12_UP)}
        if FFVal > cMaxInt64F then
          Result := cMaxInt64
        else if FFVal < cMinInt64F then
          Result := cMinInt64
        else
{$IFEND}
          Result := 0;
      end;
    except
      Result := 0;
    end;
  end;
end;

function TKNumberValue.GetUIVal: UInt64;
const
  cMaxUInt64F = 1.8446744073709551615E+19;
  cMinUInt64F =                     0E+01;
// maybe incorrect version, I don't know which Delphi version does not complain anymore
{$IF DEFINED(FPC) OR DEFINED(COMPILER12_UP)}
  cMaxUInt64 = 18446744073709551615;
  cMinUInt64 =                    0;
{$IFEND}
begin
  if FHasInt then
    Result := UInt64(FIVal)
  else
  begin
    try
      try
        Result := Round(FFVal)
      except
        // try to clamp the value to UInt64 limits
        // this requires the Extended type with sufficient precision, at least 10 bytes
        // might be not accurate when Extended is mapped to Double etc.
{$IF DEFINED(FPC) OR DEFINED(COMPILER12_UP)}
        if FFVal > cMaxUInt64F then
          Result := cMaxUInt64
        else if FFVal < cMinUInt64F then
          Result := cMinUInt64
        else
{$IFEND}
          Result := 0;
      end;
    except
      Result := 0;
    end;
  end;
end;

function TKNumberValue.GreaterThan(const AValue: TKNumberValue; ASigned: Boolean): Boolean;
begin
  if FHasInt then
  begin
    if ASigned then
      Result := IVal > AValue.IVal
    else
      Result := UIVal > AValue.UIVal
  end else
    Result := FVal > AValue.FVal;
end;

function TKNumberValue.LowerThan(const AValue: TKNumberValue; ASigned: Boolean): Boolean;
begin
  if FHasInt then
  begin
    if ASigned then
      Result := IVal < AValue.IVal
    else
      Result := UIVal < AValue.UIVal
  end else
    Result := FVal < AValue.FVal;
end;

procedure TKNumberValue.SetFVal(const AValue: Extended);
begin
  FFVal := AValue;
  FHasInt := False;
end;

procedure TKNumberValue.SetHasInt(const Value: Boolean);
begin
  if Value <> HasInt then
  begin
    if Value then
      IVal := IVal
    else
      FVal := FVal;
  end;
end;

procedure TKNumberValue.SetIVal(const AValue: Int64);
begin
  FIVal := AValue;
  FHasInt := True;
end;

procedure TKNumberValue.SetUIVal(const AValue: UInt64);
begin
  FIVal := Int64(AValue);
  FHasInt := True;
end;

{ TKNumberEdit }

constructor TKCustomNumberEdit.Create(AOwner: TComponent);
begin
  inherited;
  FMin := TKNumberValue.CreateI(0);
  FMax := TKNumberValue.CreateI(1000);
  FValue := TKNumberValue.CreateI(0);
  Text := '';
  FWarningColor := clRed;
  FOptions := [neoLowerCase, neoUseLabel, neoUsePrefix, neoUseUpDown, neoWarning, neoClampToMinMax];
  FAcceptedFormats := [neafDec];
  FDecimalSeparator := GetFormatSettings.DecimalSeparator;
  FDisplayedFormat := nedfAsInput;
  FLastInputFormat := nedfDec;
  FFixedWidth := 0;
  FPrecision := 2;
  FCustomSuffix := '';
  FLabelPosition := lpAbove;
  FLabelSpacing := 3;
  FLog := nil;
  FUpDown := TUpDown.Create(Self);
  FUpDown.TabStop := False;
  FUpDown.OnChangingEx := UpDownChangingEx;
  FUpDownStep := 1;
  FUpdownChanging := False;
  FUpdateUpDown := True;
  FLabel := TLabel.Create(Self);
  FLabel.FocusControl := Self;
  FOnUpDownChange := nil;
end;

destructor TKCustomNumberEdit.Destroy;
begin
  FMin.Free;
  FMax.Free;
  FValue.Free;
  inherited;
end;

procedure TKCustomNumberEdit.Change;
begin
  inherited;
  TextToValue;
  UpdateUpDown(FValue);
end;

{$IFDEF FPC}
procedure TKCustomNumberEdit.CreateWnd;
begin
  inherited;
  UpdateUpDownPos;
  UpdateLabel;
end;

procedure TKCustomNumberEdit.DoOnChangeBounds;
begin
  inherited;
  UpdateUpDownPos;
  UpdateLabel;
end;
{$ENDIF}

procedure TKCustomNumberEdit.DoWarning(AValue: TKNumberValue);
var
  Fmt: TKNumberEditDisplayedFormat;
begin
  if (ComponentState * [csLoading, csDesigning] = []) and HasParent then
  begin
    if neoWarning in FOptions then Font.Color := FWarningColor;
    if Assigned(FLog) then
    begin
      if FDisplayedFormat = nedfAsInput then
        Fmt := FLastInputFormat
      else
        Fmt := FDisplayedFormat;
      case Fmt of
        nedfDec: FLog.Log(lgInputError, Format(sEDBadIntValueAsStr,
          [SetFormat(FMin), SetFormat(FMax), SetFormat(AValue)]));
        nedfFloat: FLog.Log(lgInputError, Format(sEDBadFloatValueAsStr,
          [SetFormat(FMin), SetFormat(FMax), SetFormat(AValue)]));
        nedfHex: FLog.Log(lgInputError, Format(sEDBadHexValueAsStr,
          [SetFormat(FMin), SetFormat(FMax), SetFormat(AValue)]));
      end;
    end;
  end;
end;

function TKCustomNumberEdit.Empty: Boolean;
begin
  Result := (Text = '') or (Text = '-');
end;

function TKCustomNumberEdit.GetCaption: TCaption;
begin
  Result := FLabel.Caption;
end;

procedure TKCustomNumberEdit.GetFormat(AText: string; var Fmt: TKNumberEditDisplayedFormat; AValue: TKNumberValue);
var
  I: Int64;
  D: Extended;
  Code: Integer;
  W: Byte;
  K: Integer;
begin
  AValue.Clear(True);
  if AText = '' then Exit;
  if FCustomSuffix <> '' then
  begin
    K := Pos(FCustomSuffix, AText);
    if (K > 0) and (K = Length(AText) - Length(FCustomSuffix) + 1) then
      Delete(AText, K, Length(CustomSuffix));
    while (AText <> '') and (AText[Length(AText)] = ' ') do
      SetLength(AText, Length(AText) - 1);
  end;
  if AText = '' then Exit;
  // decimal integer - most probable
  if neafDec in FAcceptedFormats then
  begin
    I := DecStrToInt(AText, Code);
    if (Code = 0) then
    begin
      Fmt := nedfDec;
      AValue.IVal := I;
      Exit;
    end;
  end;
  // hexadecimal integer
  if neafHex in FAcceptedFormats then
  begin
    if FFixedWidth > 0 then W := FFixedWidth else W := 8;  // 32 bit
    I := HexStrToInt(AText, W, Signed, Code);
    if (Code = 0) then
    begin
      Fmt := nedfHex;
      AValue.IVal := I;
      Exit;
    end;
  end;
  // binary integer
  if neafBin in FAcceptedFormats then
  begin
    if FFixedWidth > 0 then W := FFixedWidth else W := 16; // 16 bit
    I := BinStrToInt(AText, W, Signed, Code);
    if (Code = 0) then
    begin
      Fmt := nedfBin;
      AValue.IVal := I;
      Exit;
    end;
  end;
  // octal integer
  if neafOct in FAcceptedFormats then
  begin
    I := OctStrToInt(AText, Code);
    if (Code = 0) then
    begin
      Fmt := nedfBin;
      AValue.IVal := I;
      Exit;
    end;
  end;
  // double - custom suffix only
  if neafFloat in FAcceptedFormats then
  begin
    K := Pos('.', AText);
    if K = 0 then K := Pos(',', AText);
    if K = 0 then K := Pos(DecimalSeparator, AText);
    if K > 0 then AText[K] := '.';
    Val(AText, D, Code);
    if (Code = 0) then
    begin
      Fmt := nedfFloat;
      AValue.FVal := D;
      Exit;
    end;
  end;
  // ascii - least probable
  if neafAscii in FAcceptedFormats then
  begin
    if FFixedWidth > 0 then W := FFixedWidth else W := 4;  // 32 bit
    AValue.IVal := AsciiToInt(AText, W);
    Fmt := nedfAscii;
  end;
end;

function TKCustomNumberEdit.GetMax: Extended;
begin
  Result := FMax.FVal;
end;

function TKCustomNumberEdit.GetMaxAsInt: Int64;
begin
  Result := FMax.IVal;
end;

function TKCustomNumberEdit.GetMin: Extended;
begin
  Result := FMin.FVal;
end;

function TKCustomNumberEdit.GetMinAsInt: Int64;
begin
  Result := FMin.IVal;
end;

procedure TKCustomNumberEdit.GetPrefixSuffix(Format: TKNumberEditDisplayedFormat; out Prefix, Suffix: string);
begin
  Prefix := '';
  Suffix := '';
  case Format of
    nedfBin: if neoLowerCase in FOptions then Suffix := 'b' else Suffix := 'B';
    nedfHex:
      if neoUsePrefix in FOptions then
        case FHexPrefix of
          nehpPascal: Prefix := '$';
          nehpC: Prefix := '0x';
        end
      else
        if neoLowerCase in FOptions then Suffix := 'h' else Suffix := 'H';
    nedfOct: if neoLowerCase in FOptions then Suffix := 'o' else Suffix := 'O';
  end;
end;

function TKCustomNumberEdit.GetRealSelLength: Integer;
begin
  if Sellength >= 0 then
    Result := SelLength
  else
    Result := -SelLength;
end;

function TKCustomNumberEdit.GetRealSelStart: Integer;
begin
  if Sellength >= 0 then
    Result := SelStart
  else
    Result := SelStart - SelLength;
end;

function TKCustomNumberEdit.GetSigned: Boolean;
begin
  Result := not (neoUnsigned in FOptions);
end;

function TKCustomNumberEdit.GetValue: Extended;
begin
  TextToValue;
  Result := FValue.FVal;
end;

function TKCustomNumberEdit.GetValueAsInt: Int64;
begin
  TextToValue;
  Result := FValue.IVal;
end;

function TKCustomNumberEdit.GetValueAsText: string;
begin
  TextToValue;
  Result := SetFormat(FValue);
end;

function TKCustomNumberEdit.InspectInputChar(Key: Char): Char;
var
  S: string;
  KeyDec, KeyHex, KeyBin, KeyOct, KeyFLoat, KeySuffix: Char;
begin
  S := Copy(Text, 1, GetRealSelStart) + Copy(Text, GetRealSelStart + GetRealSelLength + 1, Length(Text));
  if neafAscii in FAcceptedFormats then
    Result := Key
  else
  begin
    Result := #0;
    if neafDec in FAcceptedFormats then
    begin
      KeyDec := Key;
      if CharInSetEx(KeyDec, ['0'..'9','-',#8]) then
      begin
        if (KeyDec = '-') and (SelStart <> 0) then
          KeyDec := #0;
        if (Pos('0', S) = 1) and (neafOct in FAcceptedFormats) then
          KeyDec := #0;
      end else
        KeyDec := #0;
    end else
      KeyDec := #0;
    if neafHex in FAcceptedFormats then
    begin
      KeyHex := Key;
      if CharInSetEx(KeyHex, ['0'..'9', 'a'..'f', 'A'..'F', #8, 'x', 'X', 'h', 'H']) then
      begin
        if CharInSetEx(KeyHex, ['x', 'X']) and ((SelStart > 1) or
          (Pos('x', S) <> 0) or (Pos('X', S) <> 0)) then
          KeyHex := #0;
        if CharInSetEx(KeyHex, ['h', 'H']) and ((SelStart < Length(S)) or
          (Pos('h', S) <> 0) or (Pos('H', S) <> 0)) then
          KeyHex := #0;
        if neoLowerCase in FOptions then
        begin
          if CharInSetEx(KeyHex, ['A'..'F', 'X']) then Inc(KeyHex, Ord('a') - Ord('A'));
        end else
          if CharInSetEx(KeyHex, ['a'..'f', 'x']) then Inc(KeyHex, Ord('A') - Ord('a'));
      end else
        KeyHex := #0;
    end else
      KeyHex := #0;
    if neafBin in FAcceptedFormats then
    begin
      KeyBin := Key;
      if CharInSetEx(KeyBin, ['0'..'1', 'b', 'B', #8]) then
      begin
        if CharInSetEx(KeyBin, ['b', 'B']) and ((SelStart < Length(S)) or
          (Pos('b', S) <> 0) or (Pos('B', S) <> 0)) then
          KeyBin := #0;
      end else
        KeyBin := #0;
    end else
      KeyBin := #0;
    if neafOct in FAcceptedFormats then
    begin
      KeyOct := Key;
      if CharInSetEx(KeyOct, ['0'..'7', #8]) then
      begin
        if (Pos('0', S) > 1) then
          KeyOct := #0;
      end else
        KeyOct := #0;
    end else
      KeyOct := #0;
    if neafFloat in FAcceptedFormats then
    begin
      KeyFloat := Key;
      if CharInSetEx(KeyFLoat, ['0'..'9','-', '.', ',', 'e', 'E', DecimalSeparator, #8]) then
      begin
        if (KeyFloat = '-') and (SelStart <> 0) then KeyFloat := #0;
        if CharInSetEx(KeyFLoat, ['.', ',', DecimalSeparator]) and
          ((Pos('.', S) <> 0) or (Pos(',', S) <> 0) or (Pos(DecimalSeparator, S) <> 0)) then
          KeyFloat := #0;
      end else
        KeyFloat := #0;
    end else
      KeyFLoat := #0;
    if FCustomSuffix <> '' then
    begin
      if (Pos(Key, FCustomSuffix) <> 0) or (Key = ' ') then
        KeySuffix := Key
      else
        KeySuffix := #0;
    end else
      KeySuffix := #0;
    if KeyFloat <> #0 then Result := KeyFLoat;
    if KeyBin <> #0 then Result := KeyBin;
    if KeyHex <> #0 then Result := KeyHex;
    if KeyDec <> #0 then Result := KeyDec;
    if KeyOct <> #0 then Result := KeyOct;
    if KeySuffix <> #0 then Result := KeySuffix;
  end;
end;

function TKCustomNumberEdit.IsCaptionStored: Boolean;
begin
  Result := FLabel.Caption <> Name;
end;

function TKCustomNumberEdit.IsCustomSuffixStored: Boolean;
begin
  Result := FCustomSuffix <> '';
end;

function TKCustomNumberEdit.IsMaxStored: Boolean;
begin
  Result := FMax.IVal <> 1000;
end;

function TKCustomNumberEdit.IsMinStored: Boolean;
begin
  Result := FMin.IVal <> 0;
end;

function TKCustomNumberEdit.IsUpDownStepStored: Boolean;
begin
  Result := FUpDownStep <> 1;
end;

function TKCustomNumberEdit.IsValueStored: Boolean;
begin
  Result := GetValue <> 0;
end;

procedure TKCustomNumberEdit.KeyPress(var Key: Char);
begin
  inherited;
  if Key >= #32 then
  begin
    Key := InspectInputChar(Key);
    if Key <> #0 then
      Font.Color := clWindowText;
  end
end;

procedure TKCustomNumberEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FUpDown then
      FUpDown := nil
    else if AComponent = FLabel then
      FLabel := nil;
end;

procedure TKCustomNumberEdit.SafeSetFocus;
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  if (Form <> nil) and Form.Visible and Form.Enabled and Visible and Enabled then
    Form.ActiveControl := Self;
end;

procedure TKCustomNumberEdit.SetAcceptedFormats(AValue: TKNumberEditAcceptedFormats);
begin
  if AValue <> FAcceptedFormats then
  begin
    TextToValue;
    FAcceptedFormats := AValue;
    UpdateFormats;
    UpdateMaxMin;
    ValueToText;
  end;
end;

procedure TKCustomNumberEdit.SetCaption(const AValue: TCaption);
begin
  FLabel.SetTextBuf(PChar(AValue));
end;

procedure TKCustomNumberEdit.SetCustomSuffix(const AValue: string);
begin
  if AValue <> FCustomSuffix then
  begin
    TextToValue;
    FCustomSuffix := AValue;
    ValueToText;
  end;
end;

procedure TKCustomNumberEdit.SetDecimalSeparator(Value: Char);
begin
  if Value <> FDecimalSeparator then
  begin
    FDecimalSeparator := Value;
    SetValue(GetValue);
  end;
end;

procedure TKCustomNumberEdit.SetDisplayedFormat(AValue: TKNumberEditDisplayedFormat);
begin
  if FDisplayedFormat <> AValue then
  begin
    TextToValue;
    FDisplayedFormat := AValue;
    UpdateFormats;
    UpdateMaxMin;
    ValueToText;
  end;
end;

procedure TKCustomNumberEdit.SetFixedWidth(AValue: Integer);
begin
  if FFixedWidth <> AValue then
  begin
    TextToValue;
    FFixedWidth := AValue;
    ValueToText;
  end;
end;

{$IFDEF FPC}
procedure TKCustomNumberEdit.SetFlat(Value: Boolean);
begin
  if Value <> FFlat then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;
{$ENDIF}

function TKCustomNumberEdit.SetFormat(AValue: TKNumberValue): string;
var
  Prefix, Suffix: string;
  A: ShortString;
  W: Byte;
  J: Integer;
  F, G: Extended;
  Fmt: TKNumberEditDisplayedFormat;
begin
  Result := '';
  if FDisplayedFormat = nedfAsInput then
  begin
    if Frac(AValue.FVal) <> 0 then
      Fmt := nedfFloat
    else
      Fmt := FLastInputFormat;
  end else
    Fmt := FDisplayedFormat;
  GetPrefixSuffix(Fmt, Prefix, Suffix);
  case Fmt of
    nedfAscii:
    begin
      if FFixedWidth > 0 then W := FFixedWidth else W := 4;
      Result := IntToAscii(AValue.IVal, W);
    end;
    nedfBin:
    begin
      if FFixedWidth > 0 then W := FFixedWidth else W := 16;
      Result := IntToBinStr(AValue.IVal, W, Suffix);
    end;
    nedfDec:
    begin
      if Signed then
        Result := IntToDecStr(AValue.IVal, FFixedWidth)
      else
        Result := UIntToDecStr(AValue.IVal, FFixedWidth)
    end;
    nedfFloat:
      begin
        if FPrecision < 0 then
        begin
          Result := FloatToStrF(AValue.FVal, ffGeneral, 15, 15);
        end
        else if FPrecision > 0 then
        begin
          Str(AValue.FVal:FFixedWidth:FPrecision, A);
          Result := string(A);
        end else
        begin
          // determine number of valid decimal digits
          W := 0;
          F := AValue.FVal;
          G := Frac(F);
          while not (IsZero(G, 1E-10) or IsZero(1 - G, 1E-10)) do
          begin
            F := F * 10;
            G := Frac(F);
            Inc(W);
          end;
          Str(AValue.FVal:FFixedWidth:W, A);
          Result := string(A);
        end;
        J := Pos('.', Result);
        if J = 0 then J := Pos(',', Result);
        if J > 0 then Result[J] := FDecimalSeparator;
      end;
    nedfHex:
    begin
      if FFixedWidth > 0 then W := FFixedWidth else W := 8;
      Result := IntToHexStr(AValue.IVal, W, Prefix, Suffix, neoLowerCase in FOptions);
    end;
    nedfOct:
    begin
      Result := IntToOctStr(AValue.IVal);
    end;
  end;
  if (Result <> '') and (FCustomSuffix <> '') then
    Result := Result + ' ' + FCustomSuffix;
end;

procedure TKCustomNumberEdit.SetHexPrefix(AValue: TKNumberEditHexPrefix);
begin
  if FHexPrefix <> AValue then
  begin
    TextToValue;
    FHexPrefix := AValue;
    ValueToText;
  end;
end;

procedure TKCustomNumberEdit.SetLabelPosition(Value: TKLabelPosition);
begin
  if Value <> FLabelPosition then
  begin
    FLabelPosition := Value;
    UpdateLabel;
  end;
end;

procedure TKCustomNumberEdit.SetLabelSpacing(Value: Cardinal);
begin
  if Value < 1 then Value := 1;
  if Value <> FLabelSpacing then
  begin
    FLabelSpacing := Value;
    UpdateLabel;
  end;
end;

procedure TKCustomNumberEdit.SetMax(AMax: Extended);
begin
  if AMax <> FMax.FVal then
  begin
    TextToValue;
    FMax.FVal := AMax;
    UpdateMaxMin;
    ValueToText;
  end;
end;

procedure TKCustomNumberEdit.SetMaxAsInt(AMax: Int64);
begin
  if AMax <> FMax.IVal then
  begin
    TextToValue;
    FMax.IVal := AMax;
    UpdateMaxMin;
    ValueToText;
  end;
end;

procedure TKCustomNumberEdit.SetMin(AMin: Extended);
begin
  if AMin <> FMin.FVal then
  begin
    TextToValue;
    FMin.FVal := AMin;
    UpdateMaxMin;
    ValueToText;
  end;
end;

procedure TKCustomNumberEdit.SetMinAsInt(AMin: Int64);
begin
  if AMin <> FMin.IVal then
  begin
    TextToValue;
    FMin.IVal := AMin;
    UpdateMaxMin;
    ValueToText;
  end;
end;

procedure TKCustomNumberEdit.SetName(const Value: TComponentName);
var
  S: string;
begin
  S := Name;
  inherited;
  if (Text = S) or (Text = Value) or (FLabel.Caption = S) then
  begin
    if (Text = S) or (Text = Value) then
      Text := '0';
    if (FLabel <> nil) and (csSetCaption in ControlStyle) then
      FLabel.SetTextBuf(PChar(Name));
  end;
end;

procedure TKCustomNumberEdit.SetOptions(AValue: TKNumberEditOptions);
begin
  if FOptions <> AValue then
  begin
    TextToValue;
    FOptions := AValue;
    UpdateLabel;
    UpdateMaxMin;
    ValueToText;
  end;
end;

procedure TKCustomNumberEdit.SetParent(AParent: TWinControl);
begin
  inherited;
  UpdateUpDown(FValue);
  UpdateLabel;
end;

procedure TKCustomNumberEdit.SetPrecision(AValue: Integer);
begin
  if FPrecision <> AValue then
  begin
    TextToValue;
    FPrecision := AValue;
    ValueToText;
  end;
end;

procedure TKCustomNumberEdit.SetUpDownStep(AValue: Extended);
begin
  if FUpDownStep <> AValue then
  begin
    TextToValue;
    FUpDownStep := AValue;
    ValueToText;
  end;
end;

procedure TKCustomNumberEdit.SetValue(AValue: Extended);
var
  Warn: Boolean;
begin
  Font.Color := clWindowText;
  FValue.FVal := AValue;
  if neoClampToMinMax in FOptions then
    Warn := FValue.Clamp(FMin, FMax, Signed)
  else
    Warn := False;
  ValueToText;
  UpdateUpDown(FValue);
  if Warn then
    DoWarning(FValue);
end;

procedure TKCustomNumberEdit.SetValueAsInt(AValue: Int64);
var
  Warn: Boolean;
begin
  Font.Color := clWindowText;
  FValue.IVal := AValue;
  if neoClampToMinMax in FOptions then
    Warn := FValue.Clamp(FMin, FMax, Signed)
  else
    Warn := False;
  ValueToText;
  UpdateUpDown(FValue);
  if Warn then
    DoWarning(FValue);
end;

procedure TKCustomNumberEdit.SetValueAsText(const AValue: string);
var
  Fmt: TKNumberEditDisplayedFormat;
  Warn: Boolean;
begin
  Font.Color := clWindowText;
  Fmt := nedfAsInput;
  GetFormat(AValue, Fmt, FValue);
  if neoClampToMinMax in FOptions then
    Warn := FValue.Clamp(FMin, FMax, Signed)
  else
    Warn := False;
  ValueToText;
  UpdateUpDown(FValue);
  if Warn then
    DoWarning(FValue);
end;

procedure TKCustomNumberEdit.TextToValue;
begin
  GetFormat(Text, FLastInputFormat, FValue);
  if neoClampToMinMax in FOptions then
    if FValue.Clamp(FMin, FMax, Signed) then
      DoWarning(FValue);
end;

procedure TKCustomNumberEdit.UpdateFormats;
var
  Fmt: TKNumberEditDisplayedFormat;
  Fmts: set of TKNumberEditDisplayedFormat;
begin
  if FAcceptedFormats = [] then
    FAcceptedFormats := [neafDec];
  Fmts := [];
  Fmt := nedfAsInput;
  if (neafAscii in FAcceptedFormats) then begin Include(Fmts, nedfAscii); Fmt := nedfAscii end;
  if (neafBin in FAcceptedFormats) then begin Include(Fmts, nedfBin); Fmt := nedfBin end;
  if (neafOct in FAcceptedFormats) then begin Include(Fmts, nedfOct); Fmt := nedfOct end;
  if (neafFloat in FAcceptedFormats) then begin Include(Fmts, nedfFloat); Fmt := nedfFloat end;
  if (neafHex in FAcceptedFormats) then begin Include(Fmts, nedfHex); Fmt := nedfHex end;
  if (neafDec in FAcceptedFormats) then begin Include(Fmts, nedfDec); Fmt := nedfDec end;
  if not (FDisplayedFormat in Fmts) then
  begin
    FDisplayedFormat := nedfAsInput;
    FLastInputFormat := Fmt;
  end;
end;

procedure TKCustomNumberEdit.UpdateLabel;
var
  P: TPoint;
begin
 if FLabel <> nil then
  if neoUseLabel in FOptions then
  begin
    case FLabelPosition of
      lpAbove: P := Point(Left, Top - FLabel.Height - Integer(FLabelSpacing));
      lpBelow: P := Point(Left, Top + Height + Integer(FLabelSpacing));
      lpLeft: P := Point(Left - Math.Max(Integer(FLabelSpacing), FLabel.Width + 3), Top + (Height - FLabel.Height) div 2);
      lpRight: P := Point(Left + Width + Integer(FLabelSpacing), Top + (Height - FLabel.Height) div 2);
    end;
    FLabel.Left := P.X;
    FLabel.Top := P.Y;
    FLabel.Parent := Parent
  end else
    FLabel.Parent := nil;
end;

procedure TKCustomNumberEdit.UpdateMaxMin;
begin
  try
    if (neafHex in FAcceptedFormats) or (FDisplayedFormat = nedfHex) then
    begin
      if Signed then
      begin
        FMin.IVal := KFunctions.MinMax(FMin.IVal, Low(Integer), High(Integer));
        FMax.IVal := KFunctions.MinMax(FMax.IVal, Low(Integer), High(Integer));
        if FMax.LowerThan(FMin, True) then
          FMax.Assign(FMin);
      end else
      begin
        FMin.IVal := KFunctions.MinMax(FMin.IVal, 0, High(LongWord));
        FMax.IVal := KFunctions.MinMax(FMax.IVal, 0, High(LongWord));
        if FMax.LowerThan(FMin, False) then
          FMax.Assign(FMin);
      end;
    end else
    begin
      if FMax.LowerThan(FMin, Signed) then
        FMax.Assign(FMin);
    end;
  except
    FMin.IVal := 0;
    FMax.IVal := 1000;
  end;
end;

procedure TKCustomNumberEdit.UpdateUpDown(AValue: TKNumberValue);
var
  Fmt: TKNumberEditDisplayedFormat;
  AbsMax, D, PP: Extended;
begin
  if FUpdateUpdown and (FUpDown <> nil) then
    if neoUseUpDown in FOptions then
    begin
      AbsMax := Math.Max(Abs(FMax.FVal), Abs(FMin.FVal));
      if FDisplayedFormat = nedfAsInput then
        Fmt := FLastInputFormat
      else
        Fmt := FDisplayedFormat;
      D := 1;
      case Fmt of
        nedfDec: D := MinMax(FUpDownStep, 1, Math.Max(AbsMax / 10, 1));
        nedfHex: D := MinMax(FUpDownStep, 1, Math.Max(AbsMax / 16, 1));
        nedfOct: D := MinMax(FUpDownStep, 1, Math.Max(AbsMax / 8, 1));
        nedfBin: D := MinMax(FUpDownStep, 1, Math.Max(AbsMax / 2, 1));
        nedfFloat:
        begin
          PP := IntPower(10, FPrecision);
          D := MinMax(FUpDownStep * PP, 1, Math.Max(AbsMax * PP / 10, 1)) / PP;
        end;
      end;
      // UpDown min, max and position are ShortInt! (ough)
      // - must increase the order accordingly if absolute maximum number has more digits
      while AbsMax / D > 30000 do
        case Fmt of
          nedfDec, nedfFloat: D := D * 10;
          nedfHex: D := D * 16;
          nedfOct: D := D * 8;
          nedfBin: D := D * 2;
        end;
      FUpdownChanging := True;
      try
        FUpDown.Min := Trunc(FMin.FVal / D);
        FUpDown.Max := Trunc(FMax.FVal / D);
        FUpDown.Position := Trunc(AValue.FVal / D);
        FUpDown.Parent := Parent;
        FRealUpDownStep := D;
      finally
        FUpdownChanging := False;
      end;
    end else
      FUpDown.Parent := nil;
end;

procedure TKCustomNumberEdit.UpdateUpDownPos;
begin
  if FUpDown <> nil then
    FUpDown.SetBounds(Left + Width, Top, FUpDown.Width, Height);
end;

procedure TKCustomNumberEdit.UpDownChange;
begin
  if Assigned(FOnUpDownChange) then FOnUpDownChange(Self);
end;

procedure TKCustomNumberEdit.UpDownChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue:
  {$IFDEF COMPILER19_UP}Integer{$ELSE}SmallInt{$ENDIF}; Direction: TUpDownDirection);
var
  V: Extended;
begin
  if (neoUseUpDown in FOptions) and (FUpDown <> nil) and not FUpdownChanging then
  begin
    SafeSetFocus;
    Font.Color := clWindowText;
    FUpdateUpDown := False;
    V := MinMax(NewValue * FRealUpDownStep, FMin.FVal, FMax.FVal);
    if V <> Value then
    begin
      if (DisplayedFormat = nedfAsInput) and (neafDec in AcceptedFormats) and (Frac(V) = 0) then
        LastInputFormat := nedfDec;
      Value := V;
      UpDownChange;
      FUpdateUpDown := True;
      PostMessage(Handle, KM_NE_UPDATEUPDOWN, 0, 0);
    end;
  end;
end;

procedure TKCustomNumberEdit.Validate;
var
  Fmt: TKNumberEditDisplayedFormat;
begin
  if Empty and (neoKeepEmpty in FOptions) then
    Exit;
  Fmt := nedfAsInput;
  GetFormat(Text, Fmt, FValue);
  if (Fmt = nedfAsInput) and (neoClampToMinMax in FOptions) then
    FValue.Clamp(FMin, FMax, Signed)
  else
    FLastInputFormat := Fmt;
  Text := SetFormat(FValue);
  if (Fmt = nedfAsInput) and (ComponentState * [csLoading, csDesigning] = []) and HasParent then
  begin
    if neoWarning in FOptions then Font.Color := FWarningColor;
    if Assigned(FLog) then FLog.Log(lgInputError, sEDFormatNotAccepted);
  end;
end;

procedure TKCustomNumberEdit.ValueToText;
begin
  Text := SetFormat(FValue);
end;

procedure TKCustomNumberEdit.KMNEUpdateUpDown(var Msg: TLMessage);
begin
  TextToValue;
  UpdateUpDown(FValue);
end;

procedure TKCustomNumberEdit.CMBiDiModeChanged(var Msg: TLMessage);
begin
  inherited;
  if FLabel <> nil then FLabel.BiDiMode := BidiMode;
end;

procedure TKCustomNumberEdit.CMEnabledChanged(var Msg: TLMessage);
begin
  inherited;
  if FLabel <> nil then FLabel.Enabled := Enabled;
  if FUpDown <> nil then FUpDown.Enabled := Enabled;
end;

procedure TKCustomNumberEdit.CMVisibleChanged(var Msg: TLMessage);
begin
  inherited;
  if FLabel <> nil then FLabel.Visible := Visible;
  if FUpDown <> nil then FUpDown.Visible := Visible;
end;

procedure TKCustomNumberEdit.WMKillFocus(var Msg: TLMKillFocus);
begin
  inherited;
  Validate;
end;

procedure TKCustomNumberEdit.WMSetFocus(var Msg: TLMSetFocus);
begin
  inherited;
  Font.Color := clWindowText;
end;

procedure TKCustomNumberEdit.WMMove(var Msg: TLMMove);
begin
  inherited;
  UpdateUpDownPos;
  UpdateLabel;
end;

procedure TKCustomNumberEdit.WMPaste(var Msg: TLMPaste);
var
  S: string;
  I: Integer;
begin
  if ClipBoard.HasFormat(CF_TEXT) then
  begin
    S := ClipBoard.AsText;
    for I := 1 to Length(S) do
      if (InspectInputChar(S[I]) = #0) and not (csDesigning in ComponentState) then
      begin
        Font.Color := WarningColor;
        if Assigned(FLog) then FLog.Log(lgError, sEDClipboardFmtNotAccepted);
        SelLength := 0;
        Exit;
      end;
    Font.Color := clWindowText;
    inherited;
  end;
end;

procedure TKCustomNumberEdit.WMSize(var Msg: TLMSize);
begin
  inherited;
  UpdateUpDownPos;
  UpdateLabel;
end;

{ TKFileNameEditDlgProperties }

constructor TKFileNameEditDlgProperties.Create;
begin
  FInitialDir := '';
  FDefaultExt := '';
  FFilter := sEDAllFiles;
  FFilterIndex := 1;
  FOpenOptions := [ofHideReadOnly, ofEnableSizing];
  FBrowseOptions := [bfReturnOnlyFSDirs, bfDontGoBelowDomain];
  FBrowseDlgLabel := '';
end;

function TKFileNameEditDlgProperties.IsBrowseOptionsStored: Boolean;
begin
  Result := FBrowseOptions <> [bfSetFolder, bfReturnOnlyFSDirs, bfDontGoBelowDomain];
end;

function TKFileNameEditDlgProperties.IsOpenOptionsStored: Boolean;
begin
  Result := FOpenOptions <> [ofHideReadOnly, ofEnableSizing];
end;

{ TKFileNameEdit }

constructor TKFileNameEdit.Create(AOwner: TComponent);
begin
  inherited;
  FButton := nil;
  FButtonStyle := fbNone;
  FButtonAlign := fbaRight;
  FButtonText := sEDBrowse;
  FButtonWidth := 75;
  FButtonDist := 8;
  SetButtonStyle(fbButton);
  FOptions := [foAddToList, foCheckPath, foWarning];
  FWarningColor  := clRed;
  ControlStyle := ControlStyle - [csSetCaption];
  FDlgProperties := TKFileNameEditDlgProperties.Create;
  FLog := nil;
end;

destructor TKFileNameEdit.Destroy;
begin
  FDlgProperties.Free;
  inherited;
end;

procedure TKFileNameEdit.ButtonClick(Sender: TObject);
var
  OD: TOpenDialog;
  SD: TSaveDialog;
  BF: TKBrowseFolderDialog;
begin
  if foFolderOnly in FOptions then
  begin
    BF := TKBrowseFolderDialog.Create(Self);
    try
      if (Text = '') or (foAlwaysInitialDir in FOptions) then
        BF.Folder := FDlgProperties.InitialDir
      else
        BF.Folder := Text;
      BF.LabelText := FDlgProperties.BrowseDlgLabel;
      BF.Options := FDlgProperties.BrowseOptions;
      if BF.Execute then
      begin
        Text := BF.Folder;
        Change;
        if foAddToList in FOptions then
          Items.Insert(0, Text);
        FDlgProperties.InitialDir := ExtractFilePath(Text);
        Font.Color := clWindowText;
      end;
    finally
      BF.Free;
    end;
  end else if foSaveDialog in FOptions then
  begin
    SD := TSaveDialog.Create(Self);
    try
      if (Text = '') or (foAlwaysInitialDir in FOptions) then
        SD.InitialDir := FDlgProperties.InitialDir
      else
        SD.InitialDir := ExtractFilePath(Text);
      SD.DefaultExt := FDlgProperties.DefaultExt;
      SD.Filter := FDlgProperties.Filter;
      SD.FilterIndex := FDlgProperties.FilterIndex;
      SD.Options := FDlgProperties.OpenOptions;
      if SD.Execute then
      begin
        Text := SD.FileName;
        Change;
        if foAddToList in FOptions then
          Items.Insert(0, Text);
        FDlgProperties.InitialDir := ExtractFilePath(Text);
        Font.Color := clWindowText;
      end;
    finally
      SD.Free;
    end;
  end else
  begin
    OD := TOpenDialog.Create(Self);
    try
      if (Text = '') or (foAlwaysInitialDir in FOptions) then
        OD.InitialDir := FDlgProperties.InitialDir
      else
        OD.InitialDir := ExtractFilePath(Text);
      OD.DefaultExt := FDlgProperties.DefaultExt;
      OD.Filter := FDlgProperties.Filter;
      OD.FilterIndex := FDlgProperties.FilterIndex;
      OD.Options := FDlgProperties.OpenOptions;
      if OD.Execute then
      begin
        Text := OD.FileName;
        Change;
        if foAddToList in FOptions then
          Items.Insert(0, Text);
        FDlgProperties.InitialDir := ExtractFilePath(Text);
        Font.Color := clWindowText;
      end;
    finally
      OD.Free;
    end;
  end;
  if Assigned(FBtnOnClick) then FBtnOnClick(Sender);
end;

procedure TKFileNameEdit.ButtonExit(Sender: TObject);
begin
  DoExit;
end;

procedure TKFileNameEdit.CMBiDiModeChanged(var Msg: TLMessage);
begin
  inherited;
  {switch the button position};
  UpdateButton;
end;

procedure TKFileNameEdit.CMEnabledChanged(var Msg: TLMessage);
begin
  inherited;
  if Assigned(FButton) then FButton.Enabled := Enabled;
end;

procedure TKFileNameEdit.CMVisibleChanged(var Msg: TLMessage);
begin
  inherited;
  if Assigned(FButton) then FButton.Visible := Visible;
end;

procedure TKFileNameEdit.DoEnter;
begin
  Font.Color := clWindowText;
  inherited;
end;

procedure TKFileNameEdit.DoExit;
var
  B: Boolean;
  H: HWnd;
begin
  inherited;
  if FButton is TWinControl then H := TWinControl(FButton).Handle else H := 0;
  if (GetFocus <> H) and (ComponentState * [csLoading, csDesigning] = []) then
  begin
    Text := CorrectPath(Text, FDlgProperties.InitialDir, FOptions, B, FLog);
    if B then
    begin
      if foWarning in FOptions then Font.Color := FWarningColor
    end else
      if foAddToList in FOptions then
        if (Items.IndexOf(Text) < 0) and (Text <> '') then
          Items.Insert(0, Text);
  end;
end;

function TKFileNameEdit.GetFileName: TFileName;
begin
  Result := Text;
end;

procedure TKFileNameEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key = VK_RETURN then Key := 0;
end;

procedure TKFileNameEdit.DropDown;
var
  I: Integer;
begin
  // clear empty items if > 1
  if Items.Count > 1 then
    for I := 0 to Items.Count - 1 do
       if Items[I] = '' then
         Items.Delete(I);
  inherited;
end;

procedure TKFileNameEdit.SetButton(Value: TControl);
var
  PI: PPropInfo;
  N: TNotifyEvent;
begin
  if (FButtonStyle = fbUser) and (Value <> Self) then
  begin
    FButton.Free;
    FBtnOnClick := nil;
    FButton := Value;
    if Assigned(FButton) then
    begin
      PI := GetPropInfo(FButton, 'OnClick');
      if PI <> nil then
      begin
        FBtnOnClick := TNotifyEvent(GetMethodProp(FButton, PI));
        N := ButtonClick;
        SetMethodProp(FButton, PI, TMethod(N));
        UpdateButton;
        FButton.Parent := Parent;
      end;
      FButton.FreeNotification(Self);
    end;
  end;
end;

procedure TKFileNameEdit.SetButtonAlign(Value: TKFileNameEditButtonAlign);
begin
 if Value <> FButtonAlign then
 begin
   FButtonAlign := Value;
   UpdateButton;
 end;
end;

procedure TKFileNameEdit.SetButtonStyle(Value: TKFileNameEditButtonStyle);
begin
  if FButtonStyle <> Value then
  begin
    if FButtonStyle <> fbUser then FButton.Free;  
    FButtonStyle := Value;
    FButton := nil;
    FBtnOnClick := nil;
    case Value of
      fbButton:
      begin
        FButton := TButton.Create(Self);
        try
          (FButton as TButton).OnClick := ButtonClick;
        except
        end;
      end;
      fbBitBtn:
      begin
        FButton := TBitBtn.Create(Self);
        try
          with FButton as TBitBtn do
          begin
          {$IFDEF FPC}
            Glyph.LoadFromLazarusResource('OPENDIR');
          {$ELSE}
            Glyph.LoadFromResourceName(HInstance, 'OPENDIR');
          {$ENDIF}
            Glyph.Transparent := True;
            OnClick := ButtonClick;
          end;
        except
        end;
      end;
      fbSpeedBtn:
      begin
        FButton := TSpeedButton.Create(Self);
        try
          with FButton as TSpeedButton do
          begin
          {$IFDEF FPC}
            Glyph.LoadFromLazarusResource('OPENDIR');
          {$ELSE}
            Glyph.LoadFromResourceName(HInstance, 'OPENDIR');
          {$ENDIF}
            Glyph.Transparent := True;
            OnClick := ButtonClick;
          end;
        except
        end;
      end;
    end;
    UpdateButton;
    if Assigned(FButton) then
    begin
      FButton.Name := '_internal_';
      FButton.Parent := Parent;
    end;
  end;
end;

procedure TKFileNameEdit.SetButtonText(const Value: TCaption);
begin
  if Value <> FButtonText then
  begin
    FButtonText := Value;
    UpdateButton;
  end;
end;

procedure TKFileNameEdit.SetButtonWidth(Value: Integer);
begin
  if Value <> FButtonWidth then
  begin
    FButtonWidth := Value;
    UpdateButton;
  end;
end;

procedure TKFileNameEdit.SetButtonDist(Value: Integer);
begin
  if Value <> FButtonDist then
  begin
    FButtonDist := Value;
    UpdateButton;
  end;
end;

procedure TKFileNameEdit.SetFileName(const Value: TFileName);
var
  B: Boolean;
begin
  if Value <> Text then
  begin
    if ComponentState * [csLoading, csDesigning] = [] then
    begin
      Text := CorrectPath(Value, FDlgProperties.InitialDir, FOptions, B, FLog);
      if not (csDesigning in ComponentState) then
      begin
        if B then
        begin
          if foWarning in FOptions then Font.Color := FWarningColor
        end else
        begin
          if foWarning in FOptions then Font.Color := clWindowText;
          if foAddToList in FOptions then
            if (Items.IndexOf(Text) < 0) and (Text <> '') then
              Items.Insert(0, Text);
        end;
        Change;
      end;
    end
    else
      Text := Value;        
  end;
end;

procedure TKFileNameEdit.SetOptions(Value: TKFileNameEditOptions);
begin
  if Value <> FOptions then
    FOptions := Value;
end;

procedure TKFileNameEdit.SetParent(AParent: TWinControl);
begin
  inherited;
  if Assigned(FButton) then
  begin
    if Parent <> nil then
      UpdateButton;
    FButton.Parent := AParent;
  end;
end;

procedure TKFileNameEdit.WMMove(var Msg: TLMMove);
begin
  inherited;
  UpdateButton;
end;

procedure TKFileNameEdit.UpdateButton;

  procedure SetButtonPos(ALeft, ADown: Boolean);
  begin
    if ALeft then
      if ADown then
      begin
        FButton.Left := Left;
        FButton.Top := Top + Height + FButtonDist;
      end else
      begin
        FButton.Left := Left - FButton.Width - FButtonDist;
        FButton.Top := Top;
        FButton.Height := Height;
      end
    else
      if ADown then
      begin
        FButton.Left := Left + Width - FButton.Width;
        FButton.Top := Top + Height + FButtonDist;
      end else
      begin
        FButton.Left := Left + Width + FButtonDist;
        FButton.Top := Top;
        FButton.Height := Height;
      end;
 end;

var
  M: TNotifyEvent;
begin
  if Assigned(FButton) then
  begin
    if FButtonText <> '&' then
      FButton.SetTextBuf(PChar(FButtonText))
    else
      FButton.SetTextBuf('');
    FButton.Width := FButtonWidth;
    if IsPublishedProp(FButton, 'OnExit') then
    try
      SetOrdProp(FButton, 'TabStop', Integer(True));
      SetOrdProp(FButton, 'TabOrder', TabOrder + 1);
      M := ButtonExit;
      SetMethodProp(FButton, 'OnExit', TMethod(M));
    except
    end;
    if BiDiMode in [bdLeftToRight, bdRightToLeftReadingOnly] then
      if FButtonAlign in [fbaLeft, fbaLeftDown] then
        SetButtonPos(True, FButtonAlign = fbaLeftDown)
      else
        SetButtonPos(False, FButtonAlign = fbaRightDown)
    else
      if FButtonAlign in [fbaLeft, fbaLeftDown] then
        SetButtonPos(False, FButtonAlign = fbaLeftDown)
      else
        SetButtonPos(False, FButtonAlign = fbaRightDown);
  end;
end;

procedure TKFileNameEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FButton) and (Operation = opRemove) then
    FButton := nil;
end;

procedure TKFileNameEdit.WMSize(var Msg: TLMSize);
begin
  inherited;
  UpdateButton;
end;

function TKFileNameEdit.IsButtonStored: Boolean;
begin
  Result := (FButton <> nil) and (FButton.Name <> '_internal_');
end;

function TKFileNameEdit.IsButtonTextStored: Boolean;
begin
  Result := FButtonText <> sEDBrowse;
end;

function TKFileNameEdit.GetWholeLeft: Integer;
begin
  if Assigned(FButton) then
    Result := Min(Left, FButton.Left)
  else
    Result := Left;
end;

{$IFDEF FPC}
procedure TKFileNameEdit.SetFlat(Value: Boolean);
begin
  if Value <> FFlat then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;
{$ENDIF}

procedure TKFileNameEdit.SetWholeLeft(const Value: Integer);
var
  L: Integer;
begin
  L := WholeLeft;
  if L <> Value then
  begin
    if Assigned(FButton) then
      FButton.Left := FButton.Left + Value - L;
    Left := Left + Value - L;
  end;
end;

function TKFileNameEdit.GetWholeWidth: Integer;
begin
  if Assigned(FButton) then
    Result := Max(Left + Width, FButton.Left + FButton.Width) - WholeLeft
  else
    Result := Width;
end;

procedure TKFileNameEdit.SetWholeWidth(Value: Integer);
var
  W: Integer;
begin
  W := WholeWidth;
  if W <> Value then
  begin
    if Assigned(FButton) then
    begin
      if BiDiMode in [bdLeftToRight, bdRightToLeftReadingOnly] then
      begin
        case FButtonAlign of
          fbaLeft: Width := Max(Value - Left + FButton.Left, 5);
          fbaRight: Width := Max(Value - (FButton.Left + FButton.Width - Left - Width), 5);
        else
          Width := Max(Value, FButton.Width + 5);
        end;
      end else
      begin
        case FButtonAlign of
          fbaRight: Width := Max(Value - Left + FButton.Left, 5);
          fbaLeft: Width := Max(Value - (FButton.Left + FButton.Width - Left - Width), 5);
        else
          Width := Max(Value, FButton.Width + 5);
        end;
      end;
      UpdateButton;
    end else
      Width := Value;
  end;
end;

{$IFDEF FPC}
initialization
  {$i kedits.lrs}
{$ELSE}
  {$R kedits.res}
{$ENDIF}
end.
