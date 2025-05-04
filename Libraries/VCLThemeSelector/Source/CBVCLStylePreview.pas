{******************************************************************************}
{                                                                              }
{       CBVCLStylePreview: to paint a Preview of a VCL Style                   }
{       based on: VCLStylePreview Vcl.Styles.Ext                               }
{       https://github.com/RRUZ/vcl-styles-utils/                              }
{                                                                              }
{       Copyright (c) 2020-2024 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{                                                                              }
{       https://github.com/EtheaDev/VCLThemeSelector                           }
{                                                                              }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}
unit CBVCLStylePreview;

interface

Uses
  System.Classes,
  System.Sysutils,
  System.Generics.Collections,
  System.Types,
  Winapi.Windows,
  Vcl.Styles,
  Vcl.Themes,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.ExtCtrls;

const
  PREVIEW_HEIGHT = 190; //At 96 DPI
  PREVIEW_WIDTH = 300;  //At 96 DPI

type
  TCBVclStylesPreview = class(TCustomControl)
  private
    FCustomStyle: TCustomStyleServices;
    FIcon: HICON;
    FCaption: TCaption;
    FRegion : HRGN;
    FEditRequiredColor, FEditReadonlyColor: TColor;
    FMenu1Caption, FMenu2Caption, FMenu3Caption, FMenu4Caption: string;
    FTextEditCaption, FRequiredEditCaption, FReadOnlyEditCaption: string;
    FButtonNormalCaption, FButtonHotCaption, FButtonPressedCaption, FButtonDisabledCaption: string;
    FCheckBoxCaption: string;
    FScale: Double;
  protected
    procedure Paint; override;
    {$IF CompilerVersion >= 31}
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    {$ELSE}
    procedure ChangeScale(M, D: Integer); override;
    {$IFEND}
  public
    procedure SetCaptions(const ACaptions: string);
    procedure SetEditColors(const ARequiredColor, AReadonlyColor: TColor);
    property Icon:HICON read FIcon Write FIcon;
    property CustomStyle: TCustomStyleServices read FCustomStyle Write FCustomStyle;
    property Caption : TCaption read FCaption write FCaption;
    constructor Create(AControl: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TVclStylePreview }

{$IF CompilerVersion >= 31}
procedure TCBVclStylesPreview.ChangeScale(M, D: Integer; isDpiChange: Boolean);
{$ELSE}
procedure TCBVclStylesPreview.ChangeScale(M, D: Integer);
{$IFEND}
begin
  inherited;
  FScale := FScale * (M / D);
  Invalidate;
end;

constructor TCBVclStylesPreview.Create(AControl: TComponent);
begin
  inherited;
  FScale := 1;
  FRegion := 0;
  FCustomStyle := nil;
  FCaption := '';
  FIcon := 0;
  FMenu1Caption := 'File';
  FMenu2Caption := 'Edit';
  FMenu3Caption := 'View';
  FMenu4Caption := 'Help';
  FTextEditCaption := 'Text edit';
  FRequiredEditCaption := 'Required';
  FReadOnlyEditCaption := 'ReadOnly';
  FButtonNormalCaption := 'Normal';
  FButtonHotCaption := 'Hot';
  FButtonPressedCaption := 'Pressed';
  FButtonDisabledCaption := 'Disabled';
  FCheckBoxCaption := 'Check';
  FEditRequiredColor := clDefault;
  FEditReadonlyColor := clDefault;
end;

destructor TCBVclStylesPreview.Destroy;
begin
  if FRegion <> 0 then
  begin
    DeleteObject(FRegion);
    FRegion := 0;
  end;
  inherited;
end;

procedure TCBVclStylesPreview.Paint;
var
  //i: Integer;
  FBitmap: TBitmap;
  LDetails, CaptionDetails, IconDetails: TThemedElementDetails;
  IconRect, BorderRect, CaptionRect, ButtonRect, TextRect: TRect;
  CaptionBitmap : TBitmap;
  ThemeTextColor: TColor;
  ARect, LRect: TRect;
  LRegion: HRgn;
  {$ifdef Compiler33_Plus}LDPI: Integer;{$endif}

    function GetBorderSize: TRect;
    var
      Size: TSize;
      Details: TThemedElementDetails;
      Detail: TThemedWindow;
    begin
      Result  := Rect(0, 0, 0, 0);
      Detail  := twCaptionActive;
      Details := CustomStyle.GetElementDetails(Detail);
      CustomStyle.GetElementSize(0, Details, esActual, Size);
      Result.Top := Size.cy;
      Detail := twFrameLeftActive;
      Details := CustomStyle.GetElementDetails(Detail);
      CustomStyle.GetElementSize(0, Details, esActual, Size);
      Result.Left := Size.cx;
      Detail := twFrameRightActive;
      Details := CustomStyle.GetElementDetails(Detail);
      CustomStyle.GetElementSize(0, Details, esActual, Size);
      Result.Right := Size.cx;
      Detail := twFrameBottomActive;
      Details := CustomStyle.GetElementDetails(Detail);
      CustomStyle.GetElementSize(0, Details, esActual, Size);
      Result.Bottom := Size.cy;
    end;

    function RectVCenter(var R: TRect; Bounds: TRect): TRect;
    begin
      OffsetRect(R, -R.Left, -R.Top);
      OffsetRect(R, 0, (Bounds.Height - R.Height) div 2);
      OffsetRect(R, Bounds.Left, Bounds.Top);
      Result := R;
    end;

    procedure DrawCaptionButton(const AButtonTheme: TThemedWindow);
    begin
      LDetails := CustomStyle.GetElementDetails(AButtonTheme);
      CaptionRect.Bottom := Round(CaptionRect.Bottom);
      if CustomStyle.GetElementContentRect(0, LDetails, CaptionRect, ButtonRect) then
        CustomStyle.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, ButtonRect);
    end;

    procedure DrawButton(const AButtonTheme: TThemedButton;
      const ACaption: string; const ALeft, ALine: Integer);
    begin
      LDetails := CustomStyle.GetElementDetails(AButtonTheme);
      ButtonRect.Left := Round(BorderRect.Left + 5 + (ALeft * FScale));
      ButtonRect.Top := Round(ARect.Height - (45 * FScale) - ((2 - ALine) * FScale * 30));
      if AButtonTheme = tbCheckBoxCheckedNormal then
        ButtonRect.Width := Round(25 * FScale)
      else
        ButtonRect.Width := Round(75 * FScale);
      ButtonRect.Height := Round(25 * FScale);
      CustomStyle.DrawElement(FBitmap.Canvas.Handle, LDetails, ButtonRect);
      CustomStyle.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
      if CustomStyle.name = 'Windows' then
      begin
        ButtonRect.Top := ButtonRect.Top + 5;
        ButtonRect.Bottom := ButtonRect.Bottom + 5;
      end;
      if AButtonTheme = tbCheckBoxCheckedNormal then
      begin
        ButtonRect.Left := ButtonRect.Left + Round(25 * FScale);
        CustomStyle.DrawText(FBitmap.Canvas.Handle, LDetails, ACaption, ButtonRect,
          TTextFormatFlags(DT_VCENTER or DT_LEFT), ThemeTextColor)
      end
      else
        CustomStyle.DrawText(FBitmap.Canvas.Handle, LDetails, ACaption, ButtonRect,
          TTextFormatFlags(DT_VCENTER or DT_CENTER), ThemeTextColor);
    end;

    procedure DrawEdit(const ACaption: string; const ALeft: Integer;
      const AColor: TColor = clDefault);
    var
      LControlRect: TRect;
    begin
      //Draw Edit
      LDetails := CustomStyle.GetElementDetails(teEditTextNormal);
      ButtonRect.Left := Round(ALeft * FScale);
      ButtonRect.Top := Round(ARect.Height - (105 * FScale));
      ButtonRect.Width := Round(80 * FScale);
      ButtonRect.Height := Round(25 * FScale);
      CustomStyle.DrawElement(FBitmap.Canvas.Handle, LDetails, ButtonRect);
      if AColor <> clDefault then
      begin
        FBitmap.Canvas.Brush.Color := AColor;
        LControlRect := ButtonRect;
        LControlRect.Left := LControlRect.Left + 2;
        LControlRect.Right := LControlRect.Right - 2;
        LControlRect.Top := LControlRect.Top + 2;
        LControlRect.Bottom := LControlRect.Bottom - 2;
        FBitmap.Canvas.FillRect(LControlRect);
      end;
      ButtonRect.Left := ButtonRect.Left + 5;
      //Draw text into Edit
      CustomStyle.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
      CustomStyle.DrawText(FBitmap.Canvas.Handle, LDetails, ACaption, ButtonRect,
        TTextFormatFlags(DT_VCENTER or DT_LEFT), ThemeTextColor);
    end;
begin
  if FCustomStyle = nil then Exit;

  FBitmap := TBitmap.Create;
  try
    FBitmap.PixelFormat := pf32bit;
    FBitmap.Canvas.Font.Height := Muldiv(FBitmap.Canvas.Font.Height,
      Round(96*FScale), Screen.PixelsPerInch);
    {$ifdef Compiler33_Plus}LDPI := Round(96 / screen.pixelsperinch * fscale * 96);{$endif}

    BorderRect := GetBorderSize;
    ARect := ClientRect;
    CaptionBitmap := TBitmap.Create;
    try
      CaptionBitmap.SetSize(ARect.Width, BorderRect.Top);

      FBitmap.Width := ClientRect.Width;
      FBitmap.Height := ClientRect.Height;

      //Draw background
      LDetails.Element := teWindow;
      LDetails.Part := 0;
      CustomStyle.DrawElement(FBitmap.Canvas.Handle, LDetails, ARect);

      //Draw caption border
      CaptionRect := Rect(0, 0, Round(CaptionBitmap.Width * FScale), Round(CaptionBitmap.Height * FScale));
      LDetails := CustomStyle.GetElementDetails(twCaptionActive);

      LRegion := FRegion;
      try
        CustomStyle.GetElementRegion(LDetails, ARect, FRegion);
        SetWindowRgn(Handle, FRegion, True);
      finally
        if LRegion <> 0 then
          DeleteObject(LRegion);
      end;

      CustomStyle.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, CaptionRect);
      TextRect := CaptionRect;
      CaptionDetails := LDetails;

      //Draw icon
      IconDetails := CustomStyle.GetElementDetails(twSysButtonNormal);
      if not CustomStyle.GetElementContentRect(0, IconDetails, CaptionRect, ButtonRect) then
        ButtonRect := Rect(0, 0, 0, 0);
      IconRect := Rect(0, 0, GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON));
      RectVCenter(IconRect, ButtonRect);

      if (ButtonRect.Width > 0) and (FIcon <> 0) then
        DrawIconEx(CaptionBitmap.Canvas.Handle, IconRect.Left, IconRect.Top, FIcon, 0, 0, 0, 0, DI_NORMAL);
      Inc(TextRect.Left, ButtonRect.Width + 5);

      //Draw buttons
      if CustomStyle.Name <> 'Windows' then
      begin
        //Close button
        DrawCaptionButton(twCloseButtonNormal);
        //Maximize button
        DrawCaptionButton(twMaxButtonNormal);
        //Minimize button
        DrawCaptionButton(twMinButtonNormal);
        //Help button
        DrawCaptionButton(twHelpButtonNormal);
      end;

      if ButtonRect.Left > 0 then
        TextRect.Right := ButtonRect.Left;

      //Draw text "Preview"
      CustomStyle.DrawText(CaptionBitmap.Canvas.Handle, CaptionDetails,
        FCaption, TextRect, [tfLeft, tfSingleLine, tfVerticalCenter], clNone{$ifdef Compiler33_Plus}, LDPI{$endif});

      //Draw caption
      FBitmap.Canvas.Draw(0, 0, CaptionBitmap);
    finally
      CaptionBitmap.Free;
    end;

    //Draw left border
    CaptionRect := Rect(0, BorderRect.Top, BorderRect.Left, ARect.Height - BorderRect.Bottom);
    LDetails := CustomStyle.GetElementDetails(twFrameLeftActive);
    if CaptionRect.Bottom - CaptionRect.Top > 0 then
      CustomStyle.DrawElement(FBitmap.Canvas.Handle, LDetails, CaptionRect);

    //Draw right border
    CaptionRect := Rect(ARect.Width - BorderRect.Right, BorderRect.Top, ARect.Width, ARect.Height - BorderRect.Bottom);
    LDetails := CustomStyle.GetElementDetails(twFrameRightActive);
    CustomStyle.DrawElement(FBitmap.Canvas.Handle, LDetails, CaptionRect);

    //Draw Bottom border
    CaptionRect := Rect(0, ARect.Height - BorderRect.Bottom, ARect.Width, ARect.Height);
    LDetails := CustomStyle.GetElementDetails(twFrameBottomActive);
    CustomStyle.DrawElement(FBitmap.Canvas.Handle, LDetails, CaptionRect);

    //Draw Main Menu
    LDetails:= CustomStyle.GetElementDetails(tmMenuBarBackgroundActive);
    LRect := Rect(BorderRect.Left, BorderRect.Top+1, ARect.Width-BorderRect.Left,BorderRect.Top+1+Round(20*FScale));
    CustomStyle.DrawElement(FBitmap.Canvas.Handle, LDetails, LRect);

    LDetails := CustomStyle.GetElementDetails(tmMenuBarItemNormal);
    CustomStyle.GetElementColor(LDetails, ecTextColor, ThemeTextColor);

    CaptionRect := Rect(LRect.Left+10,LRect.Top+3, Round(LRect.Right*FScale), Round(LRect.Bottom*FScale));
    CustomStyle.DrawText(FBitmap.Canvas.Handle, LDetails, FMenu1Caption, CaptionRect, TTextFormatFlags(DT_LEFT), ThemeTextColor);
    CaptionRect := Rect(Round(CaptionRect.Left+Length(FMenu1Caption)*8*FScale), LRect.Top+3, LRect.Right , LRect.Bottom);
    CustomStyle.DrawText(FBitmap.Canvas.Handle, LDetails, FMenu2Caption, CaptionRect, TTextFormatFlags(DT_LEFT), ThemeTextColor);
    CaptionRect := Rect(Round(CaptionRect.Left+Length(FMenu2Caption)*8*FScale), LRect.Top+3, LRect.Right , LRect.Bottom);
    CustomStyle.DrawText(FBitmap.Canvas.Handle, LDetails, FMenu3Caption, CaptionRect, TTextFormatFlags(DT_LEFT), ThemeTextColor);
    CaptionRect := Rect(Round(CaptionRect.Left+Length(FMenu3Caption)*8*FScale), LRect.Top+3, LRect.Right , LRect.Bottom);
    CustomStyle.DrawText(FBitmap.Canvas.Handle, LDetails, FMenu4Caption, CaptionRect, TTextFormatFlags(DT_LEFT), ThemeTextColor);

    //Draw ToolButtons
  (*
    for i := 1 to 3 do
    begin
      LDetails := CustomStyle.GetElementDetails(ttbButtonNormal);
      ButtonRect.Left := BorderRect.Left + 5 +((i - 1) * 76);
      ButtonRect.Top := LRect.Top + 30;
      ButtonRect.Width := 75;
      ButtonRect.Height := 20;
      CustomStyle.DrawElement(FBitmap.Canvas.Handle, LDetails, ButtonRect);

      CustomStyle.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
      CustomStyle.DrawText(FBitmap.Canvas.Handle, LDetails, 'ToolButton'+IntToStr(i), ButtonRect, TTextFormatFlags(DT_VCENTER or DT_CENTER), ThemeTextColor);
    end;
  *)

    DrawEdit(FTextEditCaption, BorderRect.Left + 5, clDefault);
    if FEditRequiredColor <> clDefault then
      DrawEdit(FRequiredEditCaption, BorderRect.Left + 5 + 85, FEditRequiredColor);
    if FEditReadonlyColor <> clDefault then
      DrawEdit(FReadOnlyEditCaption, BorderRect.Left + 5 + 85 + 85, FEditReadonlyColor);

    //Draw Normal Button
    DrawButton(tbPushButtonNormal, FButtonNormalCaption, 0, 1);

    //Draw Hot Button
    DrawButton(tbPushButtonHot, FButtonHotCaption, 80, 1);

    //Draw Pressed Button
    DrawButton(tbPushButtonPressed, FButtonPressedCaption, 0, 2);

    //Draw Disabled Button
    DrawButton(tbPushButtonDisabled, FButtonDisabledCaption, 80, 2);

    //Draw CheckBox
    DrawButton(tbCheckBoxCheckedNormal, FCheckBoxCaption, 160, 2);

    Canvas.Draw(0, 0, FBitmap);
  finally
    FBitmap.Free;
  end;
end;

procedure TCBVclStylesPreview.SetEditColors(const ARequiredColor, AReadonlyColor: TColor);
begin
  FEditRequiredColor := ARequiredColor;
  FEditReadOnlyColor := AReadonlyColor;
end;

procedure TCBVclStylesPreview.SetCaptions(const ACaptions: string);
var
  LCaptions: TStringList;
begin
  LCaptions := TStringList.Create;
  LCaptions.Text := ACaptions;
  try
    if LCaptions.Count  > 0 then //File
      FMenu1Caption := LCaptions.Strings[0];

    if LCaptions.Count  > 1 then //Edit
      FMenu2Caption := LCaptions.Strings[1];

    if LCaptions.Count  > 2 then //View
      FMenu3Caption := LCaptions.Strings[2];

    if LCaptions.Count  > 3 then //Help
      FMenu4Caption := LCaptions.Strings[3];

    if LCaptions.Count  > 4 then //Text editor
      FTextEditCaption := LCaptions.Strings[4];

    if LCaptions.Count  > 5 then //Normal
      FButtonNormalCaption := LCaptions.Strings[5];

    if LCaptions.Count  > 6 then //Hot
      FButtonHotCaption := LCaptions.Strings[6];

    if LCaptions.Count  > 7 then //Pressed
      FButtonPressedCaption := LCaptions.Strings[7];

    if LCaptions.Count  > 8 then //Disabled
      FButtonDisabledCaption := LCaptions.Strings[8];

    if LCaptions.Count  > 9 then //Required
      FRequiredEditCaption := LCaptions.Strings[9];

    if LCaptions.Count  > 10 then //Readonly
      FReadonlyEditCaption := LCaptions.Strings[10];

    if LCaptions.Count  > 11 then //CheckBox
      FCheckBoxCaption := LCaptions.Strings[11];
  finally
    LCaptions.Free;
  end;
end;

end.
