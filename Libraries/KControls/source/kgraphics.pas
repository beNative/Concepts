{ @abstract(This file is part of the KControls component suite for Delphi and Lazarus.)
  @author(Tomas Krysl)

  Copyright (c) 2020 Tomas Krysl<BR><BR>

  <B>License:</B><BR>
  This code is licensed under BSD 3-Clause Clear License, see file License.txt or https://spdx.org/licenses/BSD-3-Clause-Clear.html.
}

unit kgraphics; // lowercase name because of Lazarus/Linux

{$include kcontrols.inc}
{$IFNDEF REGISTER_PICTURE_FORMATS}
 {$WEAKPACKAGEUNIT ON}
{$ENDIF}

interface

uses
{$IFDEF FPC}
 // use the LCL interface support whenever possible
 {$IFDEF MSWINDOWS}
  Windows,
 {$ENDIF}
  GraphType, IntfGraphics, FPImage, LCLType, LCLIntf, LMessages, LResources,
{$ELSE}
  Windows, Messages, JPeg,
 {$IFDEF USE_PNG_SUPPORT}
  PngImage,
 {$ENDIF}
{$ENDIF}
  Classes, Forms, Graphics, Controls, Types, KFunctions, KControls
{$IFDEF USE_THEMES}
  , Themes
 {$IFNDEF FPC}
  , UxTheme
 {$ENDIF}
{$ENDIF}
  ;

const
  { PNG Support }
  PNGHeader = #137'PNG'#13#10#26#10;
  MNGHeader = #138'MNG'#13#10#26#10;

  { Default value for the @link(TKSizingGrips.GripColor) property. }
  cSizingGripColor = clNavy;
  { Default value for the @link(TKSizingGrips.GripSize) property. }
  cSizingGripSize = 8;
  { Default value for the @link(TKSizingGrips.MidGripConstraint) property. }
  cSizingMidGripConstraint = (3 * cSizingGripSize + 10);

type
  { Declares possible values for the Style parameter of the @link(BrightColor) function. }
  TKBrightMode = (
    { The Color will be brightened with Percent of its entire luminosity range. }
    bsAbsolute,
    { The Color will be brightened with Percent of its current luminosity value. }
    bsOfBottom,
    { The Color will be brightened with Percent of the difference of its entire
      luminosity range and current luminosity value. }
    bsOfTop
  );

  { Declares RGB + Alpha channel color description allowing both to
    access single channels and the whole color item. }
  TKColorRec = packed record
    case Integer of
      0: (R, G, B, A: Byte);
      1: (Value: Cardinal);
  end;

  { Pointer to TKColorRec. }
  PKColorRec = ^TKColorRec;

  { Dynamic array for TKColorRec. }
  TKColorRecs = array[0..MaxInt div SizeOf(TKColorRec) - 1] of TKColorRec;
  { Dynamic array for TKColorRecs. }
  PKColorRecs = ^TKColorRecs;
  { Dynamic array for TKColorRec. }
  TKDynColorRecs = array of TKColorRec;

  { String type for @link(ImageByType) function. }
  TKImageHeaderString = string[10];

{$IFDEF USE_PNG_SUPPORT}
 {$IFDEF FPC}
   { @exclude }
  TKPngImage = TPortableNetworkGraphic;
 {$ELSE}
  {$IFDEF COMPILER12_UP}
   { @exclude }
  TKPngImage = TPngImage;
  {$ELSE}
   { @exclude }
  TKPngImage = TPngObject;
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

  TKJpegImage = TJpegImage;

  { Declares possible values for the Attributes parameter in the @link(DrawAlignedText) function. }
  TKTextAttribute = (
    { Bounding rectangle is calculated. No text is drawn. }
    taCalcRect,
    { Text will be clipped within the given rectangle. }
    taClip,
    { Text will be drawn with end ellipsis if it does not fit within given width. }
    taEndEllipsis,
    { Given rectangle will be filled. }
    taFillRect,
    { Only the text within given rectangle will be filled. }
    taFillText,
    { Include padding created by aligns in the @link(TKTextBox.PointToIndex) calculation. }
    taIncludePadding,
    { Text will be drawn as multi-line text if it contains carriage returns and line feeds. }
    taLineBreak,
    { Text will be drawn with path ellipsis if it does not fit within given width. }
    taPathEllipsis,
    { Text line(s) will be broken between words if they don't fit within given width. }
    taWordBreak,
    { Text line(s) will be broken if they don't fit within col width. }
    taWrapText, //JR:20091229
    { White spaces will be trimmed at the beginning or end of text lines. }
    taTrimWhiteSpaces,
    { Text will be drawn with start ellipsis if it does not fit within given width. }
    taStartEllipsis
  );

  { Set type for @link(TKTextAttribute) enumeration. }
  TKTextAttributes = set of TKTextAttribute;

  { Declares possible values for the HAlign parameter in the @link(DrawAlignedText) function. }
  TKHAlign = (
    { Text is aligned to the left border of a cell rectangle. }
    halLeft,
    { Text is horizontally centered within the cell rectangle. }
    halCenter,
    { Text is aligned to the right border of a cell rectangle. }
    halRight,
    { Text is aligned to the left and right border of a cell rectangle. }
    halJustify
  );

  { Declares possible values for the StretchMode parameter in the @link(ExcludeShapeFromBaseRect) function. }
  TKStretchMode = (
    { Shape is not stretched. }
    stmNone,
    { Shape is zoomed out. }
    stmZoomOutOnly,
    { Shape is zoomed in. }
    stmZoomInOnly,
    { Shape is zoomed arbitrary. }
    stmZoom
  );

  { For backward compatibility. }
  TKTextHAlign = TKHAlign;

  { Declares possible values for the VAlign parameter in the @link(DrawAlignedText) function. }
  TKVAlign = (
    { Text is aligned to the upper border of a cell rectangle. }
    valTop,
    { Text is vertically centered within the cell rectangle. }
    valCenter,
    { Text is aligned to the lower border of a cell rectangle. }
    valBottom
  );

  { For backward compatibility. }
  TKTextVAlign = TKVAlign;

  { Declares possible values for the AStates parameter in the @link(DrawButtonFrame) function. }
  TKButtonDrawState = (
    { Use OS themes/styles to draw button. }
    bsUseThemes,
    { Draw disabled button. }
    bsDisabled,
    { Draw pressed button. }
    bsPressed,
    { Draw normal focused button. }
    bsFocused,
    { Draw normal hot button. }
    bsHot
  );

  { Set of TKButtonState values. }
  TKButtonDrawStates = set of TKButtonDrawState;

  { Contains common properties for all KCOntrols TGraphic descendants. }
  TKGraphic = class(TGraphic)
  protected
    FDescription: string;
    FFileFilter: string;
  public
    { Creates the instance. }
    constructor Create; override;
    { Gives description for design time loader. }
    property Description: string read FDescription;
    { Gives file filter for design time loader. }
    property FileFilter: string read FFileFilter;
  end;

  { A simple platform independent encapsulation for a 32bpp bitmap with
    alpha channel with the ability to modify it's pixels directly. }

  { TKAlphaBitmap }

  TKAlphaBitmap = class(TKGraphic)
  private
    FAutoMirror: Boolean;
    FCanvas: TCanvas;
    FDirectCopy: Boolean;
    FHandle: HBITMAP;
    FHeight: Integer;
  {$IFNDEF MSWINDOWS}
    FImage: TLazIntfImage; // Lazarus only
    FMaskHandle: HBITMAP;
  {$ENDIF}
    FOldBitmap: HBITMAP;
    FPixels: PKColorRecs;
    FPixelsChanged: Boolean;
    FUpdateLock: Integer;
    FWidth: Integer;
    function GetScanLine(Index: Integer): PKColorRecs;
    function GetHandle: HBITMAP;
    function GetHasAlpha: Boolean;
    function GetPixel(X, Y: Integer): TKColorRec;
    procedure SetPixel(X, Y: Integer; Value: TKColorRec);
  protected
    { Calls OnChanged event. }
    procedure Changed(Sender: TObject); override;
    { Paints itself to ACanvas at location ARect. }
    procedure Draw(ACanvas: TCanvas; const ARect: TRect); override;
    { Returns True if bitmap is empty. }
    function GetEmpty: Boolean; override;
    { Returns the bitmap height. }
    function GetHeight: Integer; override;
    { Returns True. Treat alpha bitmap as transparent because of the
      possible alpha channel. }
    function GetTransparent: Boolean; override;
    { Returns the bitmap width. }
    function GetWidth: Integer; override;
    { Specifies new bitmap height. }
    procedure SetHeight(Value: Integer); override;
    { Specifies new bitmap width. }
    procedure SetWidth(Value: Integer); override;
    { Does nothing. Bitmap is never transparent. }
    procedure SetTransparent(Value: Boolean); override;
  public
    { Creates the instance. }
    constructor Create; override;
    { Creates the instance from application resources. For Lazarus 'BMP' type is
      taken, for Delphi RT_RCDATA is taken. }
    constructor CreateFromRes(const ResName: string);
    { Destroys the instance. }
    destructor Destroy; override;
    { Paints alpha bitmap onto Canvas at position given by X, Y. The alpha bitmap
      is combined with the background already drawn on Canvas using alpha channel
      stored in the alpha bitmap. }
    procedure AlphaDrawTo(ACanvas: TCanvas; X, Y: Integer);
    { Paints alpha bitmap onto Canvas at position given by ARect. The alpha bitmap
      is combined with the background already drawn on Canvas using alpha channel
      stored in the alpha bitmap. }
    procedure AlphaStretchDrawTo(ACanvas: TCanvas; const ARect: TRect);
    { Fills the alpha channel with Alpha. If the optional IfEmpty parameter is True,
      the alpha channel won't be modified unless it has zero value for all pixels. }
    procedure AlphaFill(Alpha: Byte; IfEmpty: Boolean = False); overload;
    { Fills the alpha channel according to given parameters. Currently it is used
      internally by @link(TKDragWindow). }
    procedure AlphaFill(Alpha: Byte; BlendColor: TColor; Gradient, Translucent: Boolean); overload;
    { Fills the alpha channel with AAlpha for pixels with AColor. }
    procedure AlphaFillOnColorMatch(AColor: TColor; AAlpha: Byte);
    { Modifies the alpha channel with Percent of its current value. If the optional IfEmpty parameter is True,
      the alpha channel will be set to percent of full scale if it has zero value. }
    procedure AlphaFillPercent(Percent: Integer; IfEmpty: Boolean);
    { Copies shareable properties of another instance into this instance of TKAlphaBitmap. }
    procedure Assign(Source: TPersistent); override;
    { Copies shareable properties of this instance into another instance of TKAlphaBitmap. }
    procedure AssignTo(Dest: TPersistent); override;
    { Brightens the image by given percent and bright mode. }
    procedure Brighten(APercent: Single; AMode: TKBrightMode = bsAbsolute);
    { Clears the image. }
    procedure Clear; {$IFDEF FPC}override;{$ENDIF}
    { Combines the pixel at given location with the given color. }
    procedure CombinePixel(X, Y: Integer; Color: TKColorRec);
    { Takes dimensions and pixels from AGraphic. }
    procedure CopyFrom(AGraphic: TGraphic);
    { Takes dimensions and pixels from ABitmap. }
    procedure CopyFromAlphaBitmap(ABitmap: TKAlphaBitmap);
    { Takes diemnsions and pixels from APngImage. }
    procedure CopyFromJpeg(AJpegImage: TJPEGImage);
  {$IFDEF USE_PNG_SUPPORT}
    { Takes diemnsions and pixels from APngImage. }
    procedure CopyFromPng(APngImage: TKPngImage);
  {$ENDIF}
    { Takes 90°-rotated dimensions and pixels from ABitmap. }
    procedure CopyFromRotated(ABitmap: TKAlphaBitmap);
    { Takes portion from AGraphic at position X and Y. }
    procedure CopyFromXY(X, Y: Integer; AGraphic: TGraphic);
    { Takes portion from ABitmap at position X and Y. }
    procedure CopyFromXYAlphaBitmap(X, Y: Integer; ABitmap: TKAlphaBitmap);
    { Takes portion from AJpegImage at position X and Y. }
    procedure CopyFromXYJpeg(X, Y: Integer; AJpegImage: TJPEGImage);
  {$IFDEF USE_PNG_SUPPORT}
    { Takes portion from APngImage at position X and Y. }
    procedure CopyFromXYPng(X, Y: Integer; APngImage: TKPngImage);
    { Takes diemnsions and pixels from APngImage. }
    procedure CopyToPng(APngImage: TKPngImage);
  {$ENDIF}
    { Copies a location specified by ARect from ACanvas to bitmap. }
    procedure DrawFrom(ACanvas: TCanvas; const ARect: TRect); overload;
    { Copies a portion of AGraphic to bitmap at position X and Y. }
    procedure DrawFrom(AGraphic: TGraphic; X, Y: Integer); overload;
    { Calls @link(TKAlphaBitmap.Draw). }
    procedure DrawTo(ACanvas: TCanvas; const ARect: TRect);
    { Fill with Color. }
    procedure Fill(Color: TKColorRec);
    { Convert to grayscale. }
    procedure GrayScale;
  {$IFNDEF FPC}
    { Does nothing. }
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); override;
  {$ENDIF}
    { Loads the bitmap from a file. Overriden to support PNG as well. }
    procedure LoadFromFile(const Filename: string); override;
    { Loads the bitmap from bitmap and mask handles. }
    procedure LoadFromHandles(ABitmap, AMask: HBITMAP);
    { Loads the bitmap from another TGraphic instance. }
    procedure LoadFromGraphic(Image: TGraphic); virtual;
    { Loads the bitmap from a stream. }
    procedure LoadFromStream(Stream: TStream); override;
    { Locks calls to @link(TKAlphaBitmap.Changed). }
    procedure LockUpdate; virtual;
    { Mirrors the bitmap pixels horizontally. }
    procedure MirrorHorz;
    { Mirrors the bitmap pixels vertically. }
    procedure MirrorVert;
  {$IFNDEF FPC}
    { Does nothing. }
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
      var APalette: HPALETTE); override;
  {$ENDIF}
    { Saves the bitmap to a stream. }
    procedure SaveToStream(Stream: TStream); override;
    { Specifies the bitmap size. }
    procedure SetSize(AWidth, AHeight: Integer); {$IFNDEF FPC} reintroduce;{$ENDIF}
    { Unlocks calls to @link(TKAlphaBitmap.Changed). }
    procedure UnlockUpdate; virtual;
    { Updates the bitmap handle from bitmap pixels. }
    procedure UpdateHandle; dynamic;
    { Updates the pixels from bitmap handle. }
    procedure UpdatePixels; dynamic;
    { Automatically mirrors the bitmap vertically for Linux hosts, when reading/writing from/to a stream. }
    property AutoMirror: Boolean read FAutoMirror write FAutoMirror default True;
    { Returns the bitmap memory canvas. }
    property Canvas: TCanvas read FCanvas;
    { Temporary flag. Use when copying data directly from another TGraphic to TKAlphaBitmap. }
    property DirectCopy: Boolean read FDirectCopy write FDirectCopy;
    { Returns the bitmap handle. }
    property Handle: HBITMAP read GetHandle;
    { Returns true if alpha channel is nonzero for at least one pixel. }
    property HasAlpha: Boolean read GetHasAlpha;
    { Specifies the pixel color. Does range checking. }
    property Pixel[X, Y: Integer]: TKColorRec read GetPixel write SetPixel;
    { Returns the pointer to bitmap pixels. }
    property Pixels: PKColorRecs read FPixels;
    { Set this property to True if you have modified the bitmap pixels. }
    property PixelsChanged: Boolean read FPixelsChanged write FPixelsChanged;
    { Returns the pointer to a bitmap scan line. }
    property ScanLine[Index: Integer]: PKColorRecs read GetScanLine;
  end;

{$IFDEF MSWINDOWS}
  { A simple encapsulation for a Windows or Enhanced metafile. It runs only under Windows and does not use shared images.
    However, it is possible to release metafile handles on assigning to another TKMetafile. }
  TKMetafile = class(TGraphic)
  private
    FCopyOnAssign: Boolean;
    FEmfHandle: HENHMETAFILE;
    FEnhanced: Boolean;
    FWmfHandle: HMETAFILE;
    procedure SetEMFHandle(const Value: HENHMETAFILE);
    procedure SetEnhanced(const Value: Boolean);
    procedure SetWMFHandle(const Value: HMETAFILE);
  protected
    FRequiredHeight,
    FRequiredWidth: Integer;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetTransparent: Boolean; override;
    function GetWidth: Integer; override;
    procedure SetHeight(Value: Integer); override;
    procedure SetWidth(Value: Integer); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; virtual;
    procedure LoadFromStream(Stream: TStream); override;
    procedure Release(out AWmfHandle: HMETAFILE; out AEmfHandle: HENHMETAFILE);
    procedure SaveToStream(Stream: TStream); override;
    property CopyOnAssign: Boolean read FCopyOnAssign write FCopyOnAssign;
    property EMFHandle: HENHMETAFILE read FEMFHandle write SetEMFHandle;
    property Enhanced: Boolean read FEnhanced write SetEnhanced;
    property WMFHandle: HMETAFILE read FWMFHandle write SetWMFHandle;
  end;
{$ENDIF}

  { Declares possible values for the AFunction parameter in the @link(TKTextBox.Process) function. }
  TKTextBoxFunction = (
    { Measure text box contents. }
    tbfMeasure,
    { Get text index for coordinates stored in FPoint. }
    tbfGetIndex,
    { Get boundary rectangle for index stored in FIndex. }
    tbfGetRect,
    { Draw text box contents. }
    tbfDraw
  );

  { Implements advanced text block rendering. Formerly implemented as @link(DrawAlignedText) function. }
  TKTextBox = class(TObject)
  private
    FAttributes: TKTextAttributes;
    FBackColor: TColor;
    FHAlign: TKHAlign;
    FHPadding: Integer;
    FSelBkgnd: TColor;
    FSelColor: TColor;
    FSelEnd: Integer;
    FSelStart: Integer;
    FSpacesForTab: Integer;
    FText: TKString;
    FVAlign: TKVAlign;
    FVPadding: Integer;
    procedure SetText(const AText: TKString);
  protected
    FCanvas: TCanvas;
    FClipRect: TRect;
    FFontHeight: Integer;
    FHasTabs: Boolean;
    FIndex: Integer;
    FCalcRect: TRect;
    function GetHorzPos(ATextWidth: Integer): Integer;
    function GetVertPos: Integer; virtual;
    procedure Initialize(ACanvas: TCanvas; const ARect: TRect); virtual;
    procedure Process(Y: Integer; AFunction: TKTextBoxFunction);
    procedure TextTrim(const AText: TKString; var AStart, ALen: Integer); virtual;
  public
    constructor Create;
    procedure Draw(ACanvas: TCanvas; const ARect: TRect); virtual;
    function IndexToRect(ACanvas: TCanvas; const ARect: TRect; AIndex: Integer): TRect; virtual;
    procedure Measure(ACanvas: TCanvas; const ARect: TRect; var AWidth, AHeight: Integer); virtual;
    function PointToIndex(ACanvas: TCanvas; const ARect: TRect; APoint: TPoint): Integer; virtual;
    class function TextExtent(ACanvas: TCanvas; const AText: TKString;
       AStart, ALen: Integer; AExpandTabs: Boolean = False; ASpacesForTab: Integer = 2): TSize;
    class procedure TextOutput(ACanvas: TCanvas; X, Y: Integer;
      const AText: TKString; AStart, ALen: Integer;
      AExpandTabs: Boolean = False; ASpacesForTab: Integer = 2);
    property Attributes: TKTextAttributes read FAttributes write FAttributes;
    property BackColor: TColor read FBackColor write FBackColor;
    property HAlign: TKHAlign read FHAlign write FHAlign;
    property HPadding: Integer read FHPadding write FHPadding;
    property SelBkgnd: TColor read FSelBkgnd write FSelBkgnd;
    property SelColor: TColor read FSelColor write FSelColor;
    property SelEnd: Integer read FSelEnd write FSelEnd;
    property SelStart: Integer read FSelStart write FSelStart;
    property SpacesForTab: Integer read FSpacesForTab write FSpacesForTab;
    property Text: TKString read FText write SetText;
    property VAlign: TKVAlign read FVAlign write FVAlign;
    property VPadding: Integer read FVPadding write FVPadding;
  end;

{$IFDEF MSWINDOWS}
  TUpdateLayeredWindowProc = function(Handle: THandle; hdcDest: HDC; pptDst: PPoint;
    _psize: PSize; hdcSrc: HDC; pptSrc: PPoint; crKey: COLORREF; pblend: PBLENDFUNCTION;
    dwFlags: DWORD): Boolean; stdcall;
{$ENDIF}

  { @abstract(Encapsulates the drag window)
    Drag window is top level window used for dragging with mouse. It displays
    some portion of associated control. It can be translucent under Windows. }
  TKDragWindow = class(TObject)
  private
    FActive: Boolean;
    FAlphaEffects: Boolean;
    FBitmap: TKAlphaBitmap;
    FBitmapFilled: Boolean;
    FControl: TCustomControl;
    FGradient: Boolean;
    FInitialPos: TPoint;
    FLayered: Boolean;
    FMasterAlpha: Byte;
    FRect: TRect;
  {$IFDEF MSWINDOWS}
    FBlend: TBlendFunction;
    FUpdateLayeredWindow: TUpdateLayeredWindowProc;
    FWindow: HWND;
  {$ELSE}
    FDragForm: TCustomForm;
  {$ENDIF}
  public
    { Creates the instance. }
    constructor Create;
    { Destroys the instance. }
    destructor Destroy; override;
    { Shows the drag window on screen. Takes a rectangular part as set by ARect from
      IniCtrl's Canvas and displays it at position InitialPos. MasterAlpha and
      Gradient are used to premaster the copied image with a specific fading effect. }
    procedure Init(IniCtrl: TCustomControl; const ARect: TRect;
      const AInitialPos: TPoint; AMasterAlpha: Byte; AGradient: Boolean);
    { Moves the drag window to a new location. }
    procedure Move(ARect: PRect; const ACurrentPos: TPoint; AShowAlways: Boolean);
    { Hides the drag window. }
    procedure Hide;
    { Returns True if the drag window is shown. }
    property Active: Boolean read FActive;
    { Returns the pointer to the bitmap that holds the copied control image. }
    property Bitmap: TKAlphaBitmap read FBitmap;
    { Returns True if the control already copied itself to the bitmap. }
    property BitmapFilled: Boolean read FBitmapFilled;
  end;

  { @abstract(Base class for KControls hints)
    This class extends the standard THintWindow class. It adds functionality
    common to all hints used in KControls. }

  { TKHintWindow }

  TKHintWindow = class(THintWindow)
  private
    FExtent: TPoint;
    procedure WMEraseBkGnd(var Msg: TLMessage); message LM_ERASEBKGND;
  public
    { Creates the instance. }
    constructor Create(AOwner: TComponent); override;
    { Shows the hint at given position. This is an IDE independent implementation. }
    procedure ShowAt(const Origin: TPoint);
    { Hides the hint. }
    procedure Hide;
    { Returns the extent of the hint. }
    property Extent: TPoint read FExtent;
  end;

  { @abstract(Hint window to display formatted text)
    This class implements the textual hint window. The text is displayed . }
  TKTextHint = class(TKHintWindow)
  private
    FText: TKString;
    procedure SetText(const Value: TKString);
  protected
    { Overriden method. Paints the hint. }
    procedure Paint; override;
  public
    { Creates the instance. }
    constructor Create(AOwner: TComponent); override;
    { Text to show in the hint. }
    property Text: TKString read FText write SetText;
  end;

  { @abstract(Hint window to display graphic)
    This class implements the hint window to show an image. }
  TKGraphicHint = class(TKHintWindow)
  private
    FGraphic: TGraphic;
    procedure SetGraphic(const Value: TGraphic);
  protected
    { Overriden method. Paints the hint. }
    procedure Paint; override;
  public
    { Creates the instance. }
    constructor Create(AOwner: TComponent); override;
    { Image to show in the hint. }
    property Graphic: TGraphic read FGraphic write SetGraphic;
  end;

  TKSizingGripPosition = (
    sgpNone,
    sgpLeft,
    sgpRight,
    sgpTop,
    sgpBottom,
    sgpTopLeft,
    sgpTopRight,
    sgpBottomLeft,
    sgpBottomRight
  );

  TKSizingGrips = class
  private
    FBoundsRect: TRect;
    FGripColor: TColor;
    FGripSize: Integer;
    FMidGripConstraint: Integer;
  protected
    function GripRect(APosition: TKSizingGripPosition): TRect;
  public
    constructor Create;
    class procedure ClsAffectRect(APosition: TKSizingGripPosition;
      ADX, ADY: Integer; var ARect: TRect);
    procedure DrawTo(ACanvas: TCanvas);
    function HitTest(const APoint: TPoint): TKSizingGripPosition;
    function CursorAt(const APoint: TPoint): TCursor;
    function CursorFor(APosition: TKSizingGripPosition): TCursor;
    property BoundsRect: TRect read FBoundsRect write FBoundsRect;
    property GripColor: TColor read FGripColor write FGripColor default cSizingGripColor;
    property GripSize: Integer read FGripSize write FGripSize default cSizingGripSize;
    property MidGripConstraint: Integer read FMidGripConstraint write FMidGripConstraint default cSizingMidGripConstraint;
  end;

{ Draws Src to Dest with per pixel weighting by alpha channel saved in Src. }
procedure BlendLine(Src, Dest: PKColorRecs; Count: Integer);

{ Calculates a brighter color of given color based on the HSL color space.
  <UL>
  <LH>Parameters:</LH>
  <LI><I>Color</I> - input color.</LI>
  <LI><I>Percent</I> - percentage of luminosity to bright the color (0 to 1).</LI>
  <LI><I>Mode</I> - identifies how the Percent parameter should be interpreted.</LI>
  </UL> }
function BrightColor(Color: TColor; Percent: Single; Mode: TKBrightMode = bsAbsolute): TColor;

{ Returns current canvas window/wiewport scaling. }
procedure CanvasGetScale(ACanvas: TCanvas; out MulX, MulY, DivX, DivY: Integer);

{ Selects the default window/wiewport scaling to given canvas for both axes. }
procedure CanvasResetScale(ACanvas: TCanvas);

{ Returns True if the ACanvas's device context has been mapped to anything else
  than MM_TEXT. }
function CanvasScaled(ACanvas: TCanvas): Boolean;

{ Selects the window/wiewport scaling to given canvas for both axes. }
procedure CanvasSetScale(ACanvas: TCanvas; MulX, MulY, DivX, DivY: Integer);

{ Selects the wiewport offset to given canvas for both axes. }
procedure CanvasSetOffset(ACanvas: TCanvas; OfsX, OfsY: Integer);

{ Converts TKColorRec to TColor. }
function ColorRecToColor(Color: TKColorRec): TColor;

{ Converts TColor to TKColorRec. }
function ColorToColorRec(Color: TColor): TKColorRec;

{$IFDEF FPC}
{ Converts TKColorRec to TFPColor. }
function ColorRecToFPColor(Color: TKColorRec): TFPColor;

{ Converts TFPColor to TKColorRec. }
function FPColorToColorRec(Color: TFPColor): TKColorRec;
{$ENDIF}

{ Makes a grayscale representation of the given color. }
function ColorToGrayScale(Color: TColor): TColor;

{ Returns True if properties of the two brushes are equal. }
function CompareBrushes(ABrush1, ABrush2: TBrush): Boolean;

{ Returns True if properties of the two fonts are equal. }
function CompareFonts(AFont1, AFont2: TFont): Boolean;

{ Calls BitBlt. }
procedure CopyBitmap(DestDC: HDC; DestRect: TRect; SrcDC: HDC; SrcX, SrcY: Integer);

{ Creates an empty point. }
function CreateEmptyPoint: TPoint;

{ Creates an empty point. }
function CreateEmptyPoint64: TKPoint64;

{ Creates an empty rectangle. }
function CreateEmptyRect: TRect;

{ Creates an empty rectangle. }
function CreateEmptyRect64: TKRect64;

{ Creates an empty rectangular region. }
function CreateEmptyRgn: HRGN;

{ Draws Text to the Canvas at location given by ARect.
  This function is here for backward compatibility.
  HAlign and VAlign specify horizontal resp. vertical alignment of the text
  within ARect. HPadding and VPadding specify horizontal (both on left and right side)
  and vertical (both on top and bottom side) padding of the Text from ARect.
  BackColor specifies the fill color for brush gaps if a non solid Brush
  is defined in Canvas. Attributes specift various text output attributes. }
procedure DrawAlignedText(Canvas: TCanvas; var ARect: TRect;
  HAlign: TKHAlign; VAlign: TKVAlign; HPadding, VPadding: Integer;
  const AText: TKString;
  BackColor: TColor = clWhite; Attributes: TKTextAttributes = []);

{ Draws standard button frame }
procedure DrawButtonFrame(ACanvas: TCanvas; const ARect: TRect;
  AStates: TKButtonDrawStates);

{ Simulates WinAPI DrawEdge with customizable colors. }
procedure DrawEdges(Canvas: TCanvas; const R: TRect; HighlightColor,
  ShadowColor: TColor; Flags: Cardinal);

{ Draws a rectangle to Canvas. The rectangle coordinates are given by Rect.
  The rectangle is filled by Brush. If Brush is not solid, its gaps are filled
  with BackColor. If BackColor is clNone these gaps are not filled and the Brush
  appears transparent. }
procedure DrawFilledRectangle(Canvas: TCanvas; const ARect: TRect;
  BackColor: TColor);

{ Fills rectangle with linear gradient. Parameters should be self explaining. }
procedure DrawGradientRect(Canvas: TCanvas; const ARect: TRect;
  AStartColor, AEndColor: TColor; AColorStep: Integer; AHorizontal: Boolean);

{ This helper function excludes a rectangular area occupied by a shape from
  BaseRect and calculates the shape area rectangles Bounds and Interior.
  The shape area is specified by the shape extent (ShapeWidth and ShapeHeight),
  padding (HPadding and VPadding) and stretching mode (StretchMode).
  The returned Bounds includes (possibly stretched) shape + padding,
  and Interior includes only the (possibly stretched) shape.
  HAlign specifies the horizontal alignment of shape area within BaseRect.
  VAlign specifies the vertical alignment of shape area within BaseRect.
  The shape area is always excluded horizontally from BaseRect, as needed by cell
  data calculations in KGrid. }
procedure ExcludeShapeFromBaseRect(var BaseRect: TRect; ShapeWidth, ShapeHeight: Integer;
  HAlign: TKHAlign; VAlign: TKVAlign; HPadding, VPadding: Integer;
  StretchMode: TKStretchMode; out Bounds, Interior: TRect);

{ Selects ARect into device context. Returns previous clipping region. }
function ExtSelectClipRect(DC: HDC; ARect: TRect; Mode: Integer; var PrevRgn: HRGN): Boolean;

{ Selects ARect into device context. Combines with CurRgn and
  returns previous clipping region. Both regions have to be created first. }
function ExtSelectClipRectEx(DC: HDC; ARect: TRect; Mode: Integer; CurRgn, PrevRgn: HRGN): Boolean;

{ Fills the area specified by the difference Boundary - Interior on ACanvas with current Brush.
  If Brush is not solid, its gaps are filled with BackColor. If BackColor is
  clNone these gaps are not filled and the Brush appears transparent. }
procedure FillAroundRect(ACanvas: TCanvas; const Boundary, Interior: TRect; BackColor: TColor);

{ Determine the height (ascent + descent) of the font currently selected into given DC. }
function GetFontHeight(DC: HDC): Integer;

{ Determine the ascent of the font currently selected into given DC. }
function GetFontAscent(DC: HDC): Integer;

{ Determine the descent of the font currently selected into given DC. }
function GetFontDescent(DC: HDC): Integer;

{ Determine average character size for given DC. }
function GetAveCharSize(DC: HDC): TSize;

{ Determine checkbox frame size. }
function GetCheckBoxSize: TSize;

{ Try to determine image DPI. }
function GetImageDPI(AGraphic: Tgraphic): TPoint;

{ Raises an exception if GDI resource has not been created. }
function GDICheck(Value: Integer): Integer;

{ Returns horizontal position of shape within ABoundary according to AAlignment. Shape has size defined by AShapeSize. }
function HorizontalShapePosition(AAlignment: TKHAlign; const ABoundary: TRect; const AShapeSize: TPoint): Integer;

{ Creates a TGraphic instance according to the image file header.
  Currently supported images are BMP, PNG, MNG, JPG, ICO. }
function ImageByType(const Header: TKImageHeaderString): TGraphic;

{ Calls the IntersectClipRect function. }
function IntersectClipRectIndirect(DC: HDC; ARect: TRect): Boolean;

{ Determines if given color has lightness > 0.5. }
function IsBrightColor(Color: TColor): Boolean;

{ Loads a custom mouse cursor. }
procedure LoadCustomCursor(Cursor: TCursor; const ResName: string);

{ Loads graphic from resource. }
procedure LoadGraphicFromResource(Graphic: TGraphic; const ResName: string; ResType: PChar);

{ Loads picture from clipboard. Clipboard should have CF_PICTURE format. }
procedure LoadPictureFromClipboard(APicture: TPicture; APreferredFormat: TKClipboardFormat);

{ Builds a TKColorRec structure. }
function MakeColorRec(R, G, B, A: Byte): TKColorRec; overload;

{ Builds a TKColorRec structure. }
function MakeColorRec(Value: LongWord): TKColorRec; overload;

{ Returns a pixel format that matches Bpp. }
function PixelFormatFromBpp(Bpp: Cardinal): TPixelFormat;

{ In Lazarus this WinAPI function is missing. }
function RectInRegion(Rgn: HRGN; ARect: TRect): Boolean;

{ Creates the region and copies the device context's current region into it. }
function RgnCreateAndGet(DC: HDC): HRGN;

{ Selects the region into given device context and deletes the region. }
procedure RgnSelectAndDelete(DC: HDC; Rgn: HRGN);

{ Paints rectangle with rounded corners. }
procedure RoundRectangle(ACanvas: TCanvas; const ARect: TRect; AXRadius, AYRadius: Integer);

{ Paints an image so that it fits in ARect. Performs double buffering and fills
  the background with current brush for mapped device contexts. }
procedure SafeStretchDraw(ACanvas: TCanvas; ARect: TRect; AGraphic: TGraphic; ABackColor: TColor = clWhite);

{ Selects ARect as new clipping region into the device context. }
procedure SelectClipRect(DC: HDC; const ARect: TRect);

{ Calls StretchBlt. }
procedure StretchBitmap(DestDC: HDC; DestRect: TRect; SrcDC: HDC; SrcRect: TRect);

{ Swaps the color format from RGB to BGR and vice versa. }
function SwitchRGBToBGR(Value: TColor): TColor;

{ Subtracts the current device context offset from ARect. }
procedure TranslateRectToDevice(DC: HDC; var ARect: TRect);

{ Returns vertical position of shape within ABoundary according to AAlignment. Shape has size defined by AShapeSize. }
function VerticalShapePosition(AAlignment: TKVAlign; const ABoundary: TRect; const AShapeSize: TPoint): Integer;

implementation

uses
  ClipBrd, Math, SysUtils, KRes;

procedure BlendLine(Src, Dest: PKColorRecs; Count: Integer);
var
  I: Integer;
  R, G, B, A1, A2: Integer;
begin
  // without assembler
  for I := 0 to Count - 1 do
  begin
    A1 := Src[I].A;
    A2 := 255 - A1;
    Inc(A1);
    Inc(A2);
    R := Src[I].R * A1 + Dest[I].R * A2;
    G := Src[I].G * A1 + Dest[I].G * A2;
    B := Src[I].B * A1 + Dest[I].B * A2;
    Dest[I].R := R shr 8;
    Dest[I].G := G shr 8;
    Dest[I].B := B shr 8;
  end;
end;

function CalcLightness(Color: TColor): Single;
var
  X: TKColorRec;
begin
  X := ColorToColorRec(Color);
  Result := (X.R + X.G + X.B) / (3 * 256);
end;

function BrightColor(Color: TColor; Percent: Single; Mode: TKBrightMode): TColor;
var
  L, Tmp: Single;

  function Func1(Value: Single): Single;
  begin
    Result := Value * (L + Percent) / L;
  end;

  function Func2(Value: Single): Single;
  begin
    Result := 1 - (0.5 - Tmp) * (1 - Value) / (1 - L);
    { this is the shorter form of
      Value := 1 - 0.5 * (1 - Value) / (1 - L) ; // get color with L = 0.5
      Result := 1 - (0.5 - Tmp) * (1 - Value) / 0.5; // get corresponding color
    }
  end;

  function Rd(Value: Single): Byte;
  begin
    Result := Min(Integer(Round(Value * 255)), 512);
  end;

var
  R, G, B, Cmax, Cmin: Single;
  X: TKColorRec;
begin
  X := ColorToColorRec(Color);
  R := X.R / 255;
  G := X.G / 255;
  B := X.B / 255;
  Cmax := Max(R, Max(G, B));
  Cmin := Min(R, Min(G, B));
  L := (Cmax + Cmin) / 2;
  if L < 1 then
  begin
    case Mode of
      bsOfBottom: Percent := L * Percent;
      bsOfTop: Percent := (1 - L) * Percent;
    end;
    Percent := Min(Percent, 1 - L);
    if L = 0 then
    begin
      // zero length singularity
      R := R + Percent; G := G + Percent; B := B + Percent;
    end else
    begin
      Tmp := L + Percent - 0.5;
      // lumination below 0.5
      if L < 0.5 then
      begin
        // if L + Percent is >= 0.5, get color with L = 0.5
        Percent := Min(Percent, 0.5 - L);
        R := Func1(R); G := Func1(G); B := Func1(B);
        L := 0.5;
      end;
      // lumination above 0.5
      if Tmp > 0 then
      begin
        R := Func2(R); G := Func2(G); B := Func2(B);
      end;
    end;
    X.R := Rd(R);
    X.G := Rd(G);
    X.B := Rd(B);
  end;
  Result := X.Value;
end;

procedure CanvasGetScale(ACanvas: TCanvas; out MulX, MulY, DivX, DivY: Integer);
{$IFDEF USE_DC_MAPPING}
var
  WindowExt, ViewPortExt: TSize;
{$ENDIF}
begin
{$IFDEF USE_DC_MAPPING}
  if Boolean(GetWindowExtEx(ACanvas.Handle, {$IFDEF FPC}@{$ENDIF}WindowExt)) and
    Boolean(GetViewPortExtEx(ACanvas.Handle, {$IFDEF FPC}@{$ENDIF}ViewPortExt)) then
  begin
    DivX := WindowExt.cx; DivY := WindowExt.cy;
    MulX := ViewPortExt.cx; MulY := ViewPortExt.cy;
  end else
{$ENDIF}
  begin
    MulX := 1; DivX := 1;
    MulY := 1; DivY := 1;
  end;
end;

procedure CanvasResetScale(ACanvas: TCanvas);
begin
{$IFDEF USE_DC_MAPPING}
  SetMapMode(ACanvas.Handle, MM_TEXT);
{$ENDIF}
end;

function CanvasScaled(ACanvas: TCanvas): Boolean;
begin
{$IFDEF USE_DC_MAPPING}
  Result := not (GetMapMode(ACanvas.Handle) in [0, MM_TEXT]);
{$ELSE}
  Result := False;
{$ENDIF}
end;

procedure CanvasSetScale(ACanvas: TCanvas; MulX, MulY, DivX, DivY: Integer);
begin
{$IFDEF USE_DC_MAPPING}
  SetMapMode(ACanvas.Handle, MM_ANISOTROPIC);
  SetWindowExtEx(ACanvas.Handle, DivX, DivY, nil);
  SetViewPortExtEx(ACanvas.Handle, MulX, MulY, nil);
{$ELSE}
  {$WARNING 'Device context window/viewport transformations not working!'}
{$ENDIF}
end;

procedure CanvasSetOffset(ACanvas: TCanvas; OfsX, OfsY: Integer);
begin
{$IFDEF USE_DC_MAPPING}
  SetMapMode(ACanvas.Handle, MM_ANISOTROPIC);
  SetViewPortOrgEx(ACanvas.Handle, OfsX, OfsY, nil);
{$ENDIF}  
end;

function ColorToGrayScale(Color: TColor): TColor;
var
  GreyValue: Integer;
  X: TKColorRec;
begin
  X := ColorToColorRec(Color);
  GreyValue := (Integer(21) * X.R + Integer(72) * X.G + Integer(7) * X.B) div 100;
  X.R := GreyValue;
  X.G := GreyValue;
  X.B := GreyValue;
  Result := X.Value;
end;

function ColorRecToColor(Color: TKColorRec): TColor;
begin
  Result := Color.Value and $FFFFFF;
end;

function ColorToColorRec(Color: TColor): TKColorRec;
begin
  Result.Value := ColorToRGB(Color);
end;

{$IFDEF FPC}
function ColorRecToFPColor(Color: TKColorRec): TFPColor;
begin
  Result.Red := Color.R;
  Result.Red := Result.Red + (Result.Red shl 8);
  Result.Green := Color.G;
  Result.Green := Result.Green + (Result.Green shl 8);
  Result.Blue := Color.B;
  Result.Blue := Result.Blue + (Result.Blue shl 8);
  Result.Alpha := Color.A;
  Result.Alpha := Result.Alpha + (Result.Alpha shl 8);
end;

function FPColorToColorRec(Color: TFPColor): TKColorRec;
begin
  Result.R := Color.Red shr 8;
  Result.G := Color.Green shr 8;
  Result.B := Color.Blue shr 8;
  Result.A := Color.Alpha shr 8;
end;
{$ENDIF}

function CompareBrushes(ABrush1, ABrush2: TBrush): Boolean;
begin
  Result :=
    (ABrush1.Color = ABrush2.Color) and
    (ABrush1.Style = ABrush2.Style);
end;

function CompareFonts(AFont1, AFont2: TFont): Boolean;
begin
  Result :=
    (AFont1.Charset = AFont2.Charset) and
    (AFont1.Color = AFont2.Color) and
    (AFont1.Name = AFont2.Name) and
    (AFont1.Pitch = AFont2.Pitch) and
    (AFont1.Size = AFont2.Size) and
    (AFont1.Style = AFont2.Style);
end;

procedure CopyBitmap(DestDC: HDC; DestRect: TRect; SrcDC: HDC; SrcX, SrcY: Integer);
begin
  {$IFDEF MSWINDOWS}Windows.{$ENDIF}BitBlt(DestDC,
    DestRect.Left, DestRect.Top, DestRect.Right - DestRect.Left, DestRect.Bottom - DestRect.Top,
    SrcDC, 0, 0, SRCCOPY);
end;

function CreateEmptyPoint: TPoint;
begin
  Result := Point(0,0);
end;

function CreateEmptyPoint64: TKPoint64;
begin
  Result := Point64(0,0);
end;

function CreateEmptyRect: TRect;
begin
  Result := Rect(0,0,0,0);
end;

function CreateEmptyRect64: TKRect64;
begin
  Result := Rect64(0,0,0,0);
end;

function CreateEmptyRgn: HRGN;
begin
  Result := CreateRectRgn(0,0,0,0);
end;

procedure DrawAlignedText(Canvas: TCanvas; var ARect: TRect;
  HAlign: TKHAlign; VAlign: TKVAlign; HPadding, VPadding: Integer;
  const AText: TKString;
  BackColor: TColor; Attributes: TKTextAttributes);
var
  TextBox: TKTextBox;
  Width, Height: Integer;
begin
  TextBox := TKTextBox.Create;
  try
    TextBox.Attributes := Attributes;
    TextBox.BackColor := BackColor;
    TextBox.HAlign := HAlign;
    TextBox.HPadding := HPadding;
    TextBox.Text := AText;
    TextBox.VAlign := VAlign;
    TextBox.VPadding := VPadding;
    if taCalcRect in Attributes then
    begin
      TextBox.Measure(Canvas, ARect, Width, Height);
      ARect.Right := ARect.Left + Width;
      ARect.Bottom := ARect.Top + Height;
    end else
      TextBox.Draw(Canvas, ARect);
  finally
    TextBox.Free;
  end;
end;

procedure DrawButtonFrame(ACanvas: TCanvas; const ARect: TRect;
  AStates: TKButtonDrawStates);
var
  BM: TBitmap;
  TmpCanvas: TCanvas;
  TmpRect: TRect;
  ButtonState: Integer;
{$IFDEF USE_THEMES}
  ButtonTheme: TThemedButton;
{$ENDIF}
begin
  // a LOT of tweaking here...
{$IF DEFINED(MSWINDOWS) OR DEFINED(LCLQT) } // GTK2 cannot strech and paint on bitmap canvas, grrr..
  if CanvasScaled(ACanvas) {$IFDEF MSWINDOWS}and (bsUseThemes in AStates){$ENDIF} then
  begin
    BM := TBitmap.Create;
    BM.Width := ARect.Right - ARect.Left;
    BM.Height := ARect.Bottom - ARect.Top;
    BM.Canvas.Brush.Assign(ACanvas.Brush);
    TmpRect := Rect(0, 0, BM.Width, BM.Height);
    BM.Canvas.FillRect(TmpRect);
    TmpCanvas := BM.Canvas;
  end else
{$IFEND}
  begin
    BM := nil;
    TmpRect := ARect;
    TmpCanvas := ACanvas;
  end;
  try
  {$IFDEF USE_THEMES}
    if bsUseThemes in AStates then
    begin
      if bsDisabled in AStates then
        ButtonTheme := tbPushButtonDisabled
      else if bsPressed in AStates then
        ButtonTheme := tbPushButtonPressed
      else if bsHot in AStates then
        ButtonTheme := tbPushButtonHot
      else if bsFocused in AStates then
        ButtonTheme := tbPushButtonDefaulted
      else
        ButtonTheme := tbPushButtonNormal;
      ThemeServices.DrawElement(TmpCanvas.Handle, ThemeServices.GetElementDetails(ButtonTheme), TmpRect);
    end else
  {$ENDIF}
    begin
      ButtonState := DFCS_BUTTONPUSH;
      if bsDisabled in AStates then
        ButtonState := ButtonState or DFCS_INACTIVE
      else if bsPressed in AStates then
        ButtonState := ButtonState or DFCS_PUSHED
      else if bsHot in AStates then
        ButtonState := ButtonState or DFCS_HOT;
      DrawFrameControl(TmpCanvas.Handle, TmpRect, DFC_BUTTON, ButtonState);
    end;
    if BM <> nil then
      ACanvas.Draw(ARect.Left, ARect.Top, BM);
  finally
    BM.Free;
  end;
end;

procedure DrawEdges(Canvas: TCanvas; const R: TRect; HighlightColor,
  ShadowColor: TColor; Flags: Cardinal);
begin
  with Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := HighlightColor;
    if Flags and BF_LEFT <> 0 then
      FillRect(Rect(R.Left, R.Top + 1, R.Left + 1, R.Bottom));
    if Flags and BF_TOP <> 0 then
      FillRect(Rect(R.Left, R.Top, R.Right, R.Top + 1));
    Brush.Color := ShadowColor;
    if Flags and BF_RIGHT <> 0 then
      FillRect(Rect(R.Right - 1, R.Top + 1, R.Right, R.Bottom));
    if Flags and BF_BOTTOM <> 0 then
      FillRect(Rect(R.Left + 1, R.Bottom - 1, R.Right - 1, R.Bottom));
  end;
end;

procedure DrawFilledRectangle(Canvas: TCanvas; const ARect: TRect; BackColor: TColor);
var
  DC: HDC;
begin
  DC := Canvas.Handle;
  if BackColor <> clNone then
  begin
    SetBkMode(DC, OPAQUE);
    SetBkColor(DC, ColorToRGB(BackColor));
  end;
  FillRect(DC, ARect, Canvas.Brush.Handle);
end;

procedure DrawGradientRect(Canvas: TCanvas; const ARect: TRect;
  AStartColor, AEndColor: TColor; AColorStep: Integer; AHorizontal: Boolean);
var
  J, OldJ, Extent, Num: Integer;
  L: Byte;
  CS, CE: TKColorRec;
  RCnt, GCnt, BCnt: Longint;
  RInc, GInc, BInc: Longint;
  B: Boolean;
  R: TRect;

  function NumToRGB(Num: Cardinal): TKColorRec;
  begin
    Result.R := Byte(Num shr 16);
    Result.G := Byte(Num shr 8);
    Result.B := Byte(Num);
  end;

  function RGBToNum(Col: TKColorRec): Cardinal;
  begin
    Result := Cardinal(Col.R) shl 16 + Cardinal(Col.G) shl 8 + Col.B;
  end;

begin
  with Canvas do
  begin
    if AHorizontal then
      Extent := ARect.Right - ARect.Left - 1
    else
      Extent := ARect.Bottom - ARect.Top - 1;
    Num := Max(Extent div AColorStep, 1);
    CS := NumToRGB(AStartColor);
    CE := NumToRGB(AEndColor);
    // colors per pixel
    RInc := (Integer(CE.R - CS.R) shl 16) div Extent;
    GInc := (Integer(CE.G - CS.G) shl 16) div Extent;
    Binc := (Integer(CE.B - CS.B) shl 16) div Extent;
    // start colors
    RCnt := CS.R shl 16;
    GCnt := CS.G shl 16;
    BCnt := CS.B shl 16;
    // drawing bar
    Brush.Color := RGBToNum(CS);
    OldJ := 0;
    B := False;
    for J := 0 to Extent do
    begin
      Inc(RCnt, RInc);
      L := Byte(RCnt shr 16);
      if L <> CS.R then
      begin
        CS.R := L;
        B := True;
      end;
      Inc(GCnt, GInc);
      L := Byte(GCnt shr 16);
      if L <> CS.G then
      begin
        CS.G := L;
        B := True;
      end;
      Inc(BCnt, BInc);
      L := Byte(BCnt shr 16);
      if L <> CS.B then
      begin
        CS.B := L;
        B := True;
      end;
      if B and (J mod Num = 0) then
      begin
        if AHorizontal then
          R := Rect(ARect.Left + OldJ, ARect.Top, ARect.Left + J, ARect.Bottom)
        else
          R := Rect(ARect.Left, ARect.Top + OldJ, ARect.Right, ARect.Top + J);
        FillRect(R);
        Brush.Color := RGBToNum(CS);
        OldJ := J;
        B := False;
      end;
    end;
  end;
end;

procedure ExcludeShapeFromBaseRect(var BaseRect: TRect; ShapeWidth, ShapeHeight: Integer;
  HAlign: TKHAlign; VAlign: TKVAlign; HPadding, VPadding: Integer;
  StretchMode: TKStretchMode; out Bounds, Interior: TRect);
var
  MaxHeight, MaxWidth, StretchHeight, StretchWidth: Integer;
  RatioX, RatioY: Single;
begin
  MaxHeight := BaseRect.Bottom - BaseRect.Top - 2 * VPadding;
  MaxWidth := BaseRect.Right - BaseRect.Left - HPadding;
  if ((MaxWidth <> ShapeWidth) or (MaxHeight <> ShapeHeight)) and (
    (StretchMode = stmZoom) or
    (StretchMode = stmZoomInOnly) and (MaxWidth >= ShapeWidth) and (MaxHeight >= ShapeHeight) or
    (StretchMode = stmZoomOutOnly) and ((MaxWidth < ShapeWidth) or (MaxHeight < ShapeHeight))
    ) then
  begin
    RatioX := MaxWidth / ShapeWidth;
    RatioY := MaxHeight / ShapeHeight;
    if RatioY >= RatioX then
    begin
      StretchWidth := MaxWidth;
      StretchHeight := ShapeHeight * StretchWidth div ShapeWidth;
    end else
    begin
      StretchHeight := MaxHeight;
      StretchWidth := ShapeWidth * StretchHeight div ShapeHeight;
    end;
  end else
  begin
    StretchHeight := ShapeHeight;
    StretchWidth := ShapeWidth;
  end;
  Bounds := BaseRect;
  Interior := BaseRect;
  case HAlign of
    halCenter:
    begin
      BaseRect.Right := BaseRect.Left; // BaseRect empty, no space for next item!
      // Bounds remains unchanged
      Inc(Interior.Left, HPadding + (MaxWidth - StretchWidth) div 2);
    end;
    halRight:
    begin
      Dec(BaseRect.Right, StretchWidth + HPadding);
      Bounds.Left := BaseRect.Right;
      // Bounds.Right remains unchanged
      Interior.Left := BaseRect.Right;
    end;
  else
    Inc(BaseRect.Left, StretchWidth + HPadding);
    // Bounds.Left remains unchanged
    Bounds.Right := BaseRect.Left;
    Inc(Interior.Left, HPadding);
  end;
  Interior.Right := Interior.Left + StretchWidth;
  case VAlign of
    valCenter: Inc(Interior.Top, VPadding + (MaxHeight - StretchHeight) div 2);
    valBottom: Interior.Top := BaseRect.Bottom - VPadding - StretchHeight;
  else
    Inc(Interior.Top, VPadding);
  end;
  Interior.Bottom := Interior.Top + StretchHeight;
end;

function ExtSelectClipRect(DC: HDC; ARect: TRect; Mode: Integer; var PrevRgn: HRGN): Boolean;
var
  TmpRgn: HRGN;
begin
  TmpRgn := CreateEmptyRgn;
  try
    Result := ExtSelectClipRectEx(DC, ARect, Mode, TmpRgn, PrevRgn)
  finally
    DeleteObject(TmpRgn);
  end;
end;

function ExtSelectClipRectEx(DC: HDC; ARect: TRect; Mode: Integer; CurRgn, PrevRgn: HRGN): Boolean;
var
  RectRgn: HRGN;
//  R1, R2: TRect;
begin
  RectRgn := CreateRectRgnIndirect(ARect);
  try
//    GetRgnBox(PrevRgn, R1); // debug line
//    GetRgnBox(RectRgn, R2); // debug line
    Result := CombineRgn(CurRgn, PrevRgn, RectRgn, Mode) <> NULLREGION;
    if Result then
      SelectClipRgn(DC, CurRgn)
  finally
    DeleteObject(RectRgn);
  end;
end;

procedure FillAroundRect(ACanvas: TCanvas; const Boundary, Interior: TRect; BackColor: TColor);
var
  R: TRect;
begin
  R := Rect(Boundary.Left, Boundary.Top, Boundary.Right, Interior.Top);
  if not IsRectEmpty(R) then DrawFilledRectangle(ACanvas, R, BackColor);
  R := Rect(Boundary.Left, Interior.Top, Interior.Left, Interior.Bottom);
  if not IsRectEmpty(R) then DrawFilledRectangle(ACanvas, R, BackColor);
  R := Rect(Interior.Right, Interior.Top, Boundary.Right, Interior.Bottom);
  if not IsRectEmpty(R) then DrawFilledRectangle(ACanvas, R, BackColor);
  R := Rect(Boundary.Left, Interior.Bottom, Boundary.Right, Boundary.Bottom);
  if not IsRectEmpty(R) then DrawFilledRectangle(ACanvas, R, BackColor);
end;

function GetFontHeight(DC: HDC): Integer;
var
  TM: TTextMetric;
begin
  FillChar(TM, SizeOf(TTextMetric), 0);
  GetTextMetrics(DC, TM);
  Result := TM.tmHeight;
end;

function GetFontAscent(DC: HDC): Integer;
var
  TM: TTextMetric;
begin
  FillChar(TM, SizeOf(TTextMetric), 0);
  GetTextMetrics(DC, TM);
  Result := TM.tmAscent;
end;

function GetFontDescent(DC: HDC): Integer;
var
  TM: TTextMetric;
begin
  FillChar(TM, SizeOf(TTextMetric), 0);
  GetTextMetrics(DC, TM);
  Result := TM.tmDescent;
end;

function GetAveCharSize(DC: HDC): TSize;
var
  TM: TTextMetric;
const
  Buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
begin
  FillChar(TM, SizeOf(TTextMetric), 0);
  GetTextMetrics(DC, TM);
  GetTextExtentPoint32(DC, Buffer, 52, Result);
  Result.cx := (Result.cx div 26 + 1) div 2; //div uses trunc rounding; we want arithmetic rounding
  Result.cy := tm.tmHeight;
end;

function GetCheckBoxSize: TSize;
{$IFDEF MSWINDOWS}
var
  Bm: HBITMAP;
  BmSize: BITMAP;
{$ENDIF}
{$IFDEF FPC}
{$ELSE}
var
  Theme: HTHEME;
{$ENDIF}
begin
  Result.cx := 0;
  Result.cy := 0;
{$IFDEF USE_THEMES}
  if ThemeServices.ThemesEnabled and ThemeServices.ThemesAvailable then
  begin
  {$IFDEF FPC}
     Result := ThemeServices.GetDetailSize(ThemeServices.GetElementDetails(tbCheckBoxCheckedNormal));
     Exit;
  {$ELSE}
     Theme := ThemeServices.Theme[teButton];
     if GetThemePartSize(Theme, 0, BP_CHECKBOX, CBS_CHECKEDNORMAL, nil, TS_TRUE, Result) = S_OK then
       Exit;
  {$ENDIF}
  end;
{$ENDIF}

{$IFDEF MSWINDOWS}
  Bm := LoadBitmap(0, PChar(OBM_CHECKBOXES));
  if Bm <> 0 then
    try
      if GetObject(Bm, SizeOf(BmSize), @BmSize) = SizeOf(BmSize) then
        begin
          Result.cx := BmSize.bmWidth div 4;
          Result.cy := BmSize.bmHeight div 3;
        end;
    finally
      DeleteObject(Bm);
    end;
{$ELSE}
  // just guessed here
  Result.cx := 13;
  Result.cy := 13;
{$ENDIF}
end;

function GetImageDPI(AGraphic: Tgraphic): TPoint;

  procedure GetDPIFromJPeg(AJPeg: TJPegImage);
  const
    cInchesPerCM = (1 / 2.54);
    cBufferSize = 50;
  var
    MS: TMemoryStream;
    Index: Integer;
    Buffer: AnsiString;
    resUnits: Byte;
    xResolution: Word;
    yResolution: Word;
  begin
    // seek for XDensity and YDensity fields in JPEG header
    MS := TMemoryStream.Create;
    try
      AJPeg.SaveToStream(MS);
      MS.Seek(0, soFromBeginning);
      SetLength(Buffer, cBufferSize);
      MS.Read(Buffer[1], cBufferSize);
      Index := Pos(AnsiString('JFIF'+#$00), Buffer);
      if Index > 0 then
      begin
        MS.Seek(Index + 6, soFromBeginning);
        MS.Read(resUnits, 1);
        MS.Read(xResolution, 2);
        MS.Read(yResolution, 2);
        xResolution := Swap(xResolution);
        yResolution := Swap(yResolution);
        case resUnits of
          1: // dots per inch
          begin
            Result.X := xResolution;
            Result.Y := yResolution;
          end;
          2: // dots per cm
          begin
            Result.X := Round(xResolution / cInchesPerCM);
            Result.Y := Round(yResolution / cInchesPerCM);
          end;
        else
          Result.X := 96;
          Result.Y := MulDiv(96, yResolution, xResolution);
        end;
      end;
    finally
      MS.Free;
    end;
  end;

{$IFDEF USE_PNG_SUPPORT}
  procedure GetDPIFromPng(APng: TKPngImage);
  const
    cInchesPerMeter = (100 / 2.54);
{$IFDEF FPC}
  type
    TPngChunkCode = array[0..3] of AnsiChar;
    TPngChunkHdr = packed record
      clength: LongWord;
      ctype: TPngChunkCode;
    end;
  var
    MS: TMemoryStream;
    CLen, PPUnitX, PPUnitY: Cardinal;
    CHdr: TPngChunkHdr;
    CPHYsData: array[0..8] of Byte;
{$ELSE}
  var
    Chunk: TChunk;
{$ENDIF}
  begin
{$IFDEF FPC}
    MS := TMemoryStream.Create;
    try
      APng.SaveToStream(MS);
      MS.Seek(8, soFromBeginning); // skip PNG header
      while MS.Position < MS.Size do
      begin
        // traverse the PNG chunks until pHYs chunk is found
        MS.Read(CHdr, SizeOf(CHdr));
        CLen := SwapEndian(CHdr.clength); // suppose little endian
        if CHdr.ctype = 'pHYs' then
        begin
          MS.Read(CPHYsData, 9); // pHYs chunk is always 9 bytes long
          if CPHYsData[8] = 1 then // dots per meter
          begin
            PPUnitX := SwapEndian(PCardinal(@CPHYsData[0])^); // suppose little endian
            PPUnitY := SwapEndian(PCardinal(@CPHYsData[4])^); // suppose little endian
            Result.X := Round(PPUnitX / cInchesPerMeter);
            Result.Y := Round(PPUnitY / cInchesPerMeter);
          end;
          Exit;
        end else
          MS.Seek(CLen + SizeOf(LongWord), soFromCurrent);
      end;
    finally
      MS.Free;
    end;
{$ELSE}
    // in Delphi we have the pHYs chunk directly accessible
    Chunk := APng.Chunks.FindChunk(TChunkpHYs);
    if Assigned(Chunk) then
    begin
      if (TChunkPhys(Chunk).UnitType = utMeter) then
      begin
        Result.X := Round(TChunkPhys(Chunk).PPUnitX / cInchesPerMeter);
        Result.Y := Round(TChunkPhys(Chunk).PPUnitY / cInchesPerMeter);
      end;
    end
{$ENDIF}
  end;
{$ENDIF}

begin
  Result := Point(96, 96); // for unimplemented image types set screen dpi
  if AGraphic is TJPegImage then
    GetDPIFromJPeg(TJpegImage(AGraphic))
{$IFDEF USE_PNG_SUPPORT}
  else if AGraphic is TKPngImage then
    GetDPIFromPng(TKPngImage(AGraphic));
{$ENDIF}
end;

function GDICheck(Value: Integer): Integer;
begin
  if Value = 0 then
    raise EOutOfResources.Create(sGDIError);
  Result := Value;
end;

function HorizontalShapePosition(AAlignment: TKHAlign; const ABoundary: TRect; const AShapeSize: TPoint): Integer;
begin
  case AAlignment of
    halCenter: Result := ABoundary.Left + (ABoundary.Right - ABoundary.Left - AShapeSize.X) div 2;
    halRight: Result := ABoundary.Right - AShapeSize.X;
  else
    Result := ABoundary.Left;
  end;
end;

function ImageByType(const Header: TKImageHeaderString): TGraphic;
begin
  if Pos('BM', {$IFDEF COMPILER12_UP}string{$ENDIF}(Header)) = 1 then
    Result := TBitmap.Create
{$IFDEF USE_PNG_SUPPORT }
  else if (Pos(#$89'PNG', {$IFDEF COMPILER12_UP}string{$ENDIF}(Header)) = 1) or
    (Pos(#$8A'MNG', {$IFDEF COMPILER12_UP}string{$ENDIF}(Header)) = 1) then
    Result := TKPngImage.Create
{$ENDIF }
  else if (Pos(#$FF#$D8, {$IFDEF COMPILER12_UP}string{$ENDIF}(Header)) = 1) then
    Result := TJPegImage.Create
  else if (Pos(#$00#$00, {$IFDEF COMPILER12_UP}string{$ENDIF}(Header)) = 1) then
    Result := TIcon.Create
  else
    Result := nil;
end;

function IntersectClipRectIndirect(DC: HDC; ARect: TRect): Boolean;
begin
  with ARect do
    Result := IntersectClipRect(DC, Left, Top, Right, Bottom) <> NULLREGION;
end;

function IsBrightColor(Color: TColor): Boolean;
begin
  Result := CalcLightness(Color) > 0.5;
end;

function MakeColorRec(R, G, B, A: Byte): TKColorRec;
begin
  Result.R := R;
  Result.G := G;
  Result.B := B;
  Result.A := A;
end;

function MakeColorRec(Value: LongWord): TKColorRec;
begin
  Result.Value := Value;
end;

procedure LoadCustomCursor(Cursor: TCursor; const ResName: string);
begin
  Screen.Cursors[Cursor] :=
  {$IFDEF FPC}
    LoadCursorFromLazarusResource(ResName);
  {$ELSE}
    LoadCursor(HInstance, PChar(ResName));
  {$ENDIF}
end;

procedure LoadGraphicFromResource(Graphic: TGraphic; const ResName: string; ResType: PChar);
{$IFNDEF FPC}
var
  Stream: TResourceStream;
{$ENDIF}
begin
  if Graphic <> nil then
  try
  {$IFDEF FPC}
    try
      Graphic.LoadFromResourceName(HInstance, ResName);
    except
      Graphic.LoadFromLazarusResource(ResName);
    end;
  {$ELSE}
    Stream := TResourceStream.Create(HInstance, ResName, ResType);
    try
      Graphic.LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  {$ENDIF}
  except
    Error(sErrGraphicsLoadFromResource);
  end;
end;

procedure LoadPictureFromClipboard(APicture: TPicture; APreferredFormat: TKClipboardFormat);
begin
  try
  {$IFDEF FPC}
    APicture.LoadFromClipboardFormat(APreferredFormat);
  {$ELSE}
    APicture.LoadFromClipboardFormat(APreferredFormat, Clipboard.GetAsHandle(APreferredFormat), 0);
  {$ENDIF}
  except
    APicture.Assign(ClipBoard);
  end;
end;

function PixelFormatFromBpp(Bpp: Cardinal): TPixelFormat;
begin
  case Bpp of
    1: Result := pf1bit;
    2..4: Result := pf4bit;
    5..8: Result := pf8bit;
    9..16: Result := pf16bit;
  else
    Result := pf32bit;
  end;
end;

function RectInRegion(Rgn: HRGN; ARect: TRect): Boolean;
{$IFDEF FPC}
var
  RectRgn, TmpRgn: HRGN;
{$ENDIF}
begin
{$IFDEF FPC}
  RectRgn := CreateRectRgnIndirect(ARect);
  try
    TmpRgn := CreateEmptyRgn;
    try
      Result := CombineRgn(TmpRgn, RectRgn, Rgn, RGN_AND) <> NULLREGION;
    finally
      DeleteObject(TmpRgn);
    end;
  finally
    DeleteObject(RectRgn);
  end;
{$ELSE}
  Result := Windows.RectInRegion(Rgn, ARect);
{$ENDIF}
end;

function RgnCreateAndGet(DC: HDC): HRGN;
//var
//  R: TRect;
begin
  Result := CreateEmptyRgn;
  GetClipRgn(DC, Result);
//  GetRgnBox(Result, R); // debug line
end;

procedure RgnSelectAndDelete(DC: HDC; Rgn: HRGN);
begin
  SelectClipRgn(DC, Rgn);
  DeleteObject(Rgn);
end;

procedure RoundRectangle(ACanvas: TCanvas; const ARect: TRect; AXRadius, AYRadius: Integer);
begin
{$IF DEFINED(COMPILER12_UP) OR DEFINED(FPC)}
  ACanvas.RoundRect(ARect, AXRadius, AYRadius)
{$ELSE}
  ACanvas.RoundRect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, AXRadius, AYRadius)
{$IFEND}
end;

procedure SafeStretchDraw(ACanvas: TCanvas; ARect: TRect; AGraphic: TGraphic; ABackColor: TColor);
{$IFDEF MSWINDOWS}
{var
  BM: TBitmap;
  W, H, MulX, MulY, DivX, DivY: Integer;
  R: TRect;}
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  // tk: I cannot see problem with StretchBlt anymore, perhaps it was in old Windows XP?
  // Even if so, following implementation is buggy:

  {if AGraphic.Transparent then
  begin
    // WinAPI StretchBlt function does not read properly from screen buffer
    // so we have to append double buffering
    CanvasGetScale(ACanvas, MulX, MulY, DivX, DivY);
    W := MulDiv(ARect.Right - ARect.Left, MulX, DivX);
    H := MulDiv(ARect.Bottom - ARect.Top, MulY, DivY);
    BM := TBitmap.Create;
    try
      BM.Width := W;
      BM.Height := H;
      BM.Canvas.Brush := ACanvas.Brush;
      R := Rect(0, 0, W, H);
      DrawFilledRectangle(BM.Canvas, R, ABackColor);
      BM.Canvas.StretchDraw(R, AGraphic);
      ACanvas.StretchDraw(ARect, BM);
    finally
      BM.Free;
    end;
  end else}
{$ENDIF}
    ACanvas.StretchDraw(ARect, AGraphic);
end;

procedure SelectClipRect(DC: HDC; const ARect: TRect);
var
  Rgn: HRGN;
begin
  Rgn := CreateRectRgnIndirect(ARect);
  try
    SelectClipRgn(DC, Rgn);
  finally
    DeleteObject(Rgn);
  end;
end;

procedure StretchBitmap(DestDC: HDC; DestRect: TRect; SrcDC: HDC; SrcRect: TRect);
begin
{$IFDEF MSWINDOWS}
  SetStretchBltMode(DestDC, HALFTONE);
{$ENDIF}
  {$IFDEF MSWINDOWS}Windows.{$ENDIF}StretchBlt(DestDC,
    DestRect.Left, DestRect.Top, DestRect.Right - DestRect.Left, DestRect.Bottom - DestRect.Top,
    SrcDC, SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top,
    SRCCOPY);
end;

procedure SwapBR(var ColorRec: TKColorRec);
var
  Tmp: Byte;
begin
  Tmp := ColorRec.R;
  ColorRec.R := ColorRec.B;
  ColorRec.B := Tmp;
end;

function SwitchRGBToBGR(Value: TColor): TColor;
var
  B: Byte;
begin
  Result := Value;
  B := PKColorRec(@Value).B;
  PKColorRec(@Result).B := PKColorRec(@Result).R;
  PKColorRec(@Result).R := B;
end;

procedure TranslateRectToDevice(DC: HDC; var ARect: TRect);
var
  WindowOrg, ViewportOrg: TPoint;
{$IFDEF USE_DC_MAPPING}
 {$IFNDEF LCLQT}
  WindowExt, ViewportExt: TSize;
 {$ENDIF}
{$ENDIF}
begin
  if Boolean(GetWindowOrgEx(DC, {$IFDEF FPC}@{$ENDIF}WindowOrg)) then
    KFunctions.OffsetRect(ARect, -WindowOrg.X, -WindowOrg.Y);
{$IFDEF USE_DC_MAPPING}
  {$IFNDEF LCLQT}
  if not (GetMapMode(DC) in [0, MM_TEXT]) and
    Boolean(GetWindowExtEx(DC, {$IFDEF FPC}@{$ENDIF}WindowExt)) and
    Boolean(GetViewportExtEx(DC, {$IFDEF FPC}@{$ENDIF}ViewportExt)) then
  begin
    ARect.Left := MulDiv(ARect.Left, ViewportExt.cx, WindowExt.cx);
    ARect.Right := MulDiv(ARect.Right, ViewportExt.cx, WindowExt.cx);
    ARect.Top := MulDiv(ARect.Top, ViewportExt.cy, WindowExt.cy);
    ARect.Bottom := MulDiv(ARect.Bottom, ViewportExt.cy, WindowExt.cy);
  end;
  if Boolean(GetViewPortOrgEx(DC, {$IFDEF FPC}@{$ENDIF}ViewportOrg)) then
    KFunctions.OffsetRect(ARect, ViewportOrg);
  {$ENDIF}
{$ENDIF}
end;

function VerticalShapePosition(AAlignment: TKVAlign; const ABoundary: TRect; const AShapeSize: TPoint): Integer;
begin
  case AAlignment of
    valCenter: Result := ABoundary.Top + (ABoundary.Bottom - ABoundary.Top - AShapeSize.Y) div 2;
    valBottom: Result := ABoundary.Bottom - AShapeSize.Y;
  else
    Result := ABoundary.Top;
  end;
end;

{ TKGraphic }

constructor TKGraphic.Create;
begin
  inherited;
  FDescription := '';
  FFileFilter := '';
end;

{ TKAlphaBitmap }

constructor TKAlphaBitmap.Create;
begin
  inherited;
  FCanvas := TCanvas.Create;
  FCanvas.Handle := CreateCompatibleDC(0);
  FUpdateLock := 0;
  FAutoMirror := True;
  FDescription := 'KControls alpha bitmap';
  FDirectCopy := False;
  FFileFilter := '*.bma;*.bmp;*.png;*.jpg';
  FHandle := 0;
{$IFNDEF MSWINDOWS}
  FImage := TLazIntfImage.Create(0, 0);
{$ENDIF}
  FHeight := 0;
  FOldBitmap := 0;
  FPixels := nil;
  FPixelsChanged := False;
  FWidth := 0;
end;

constructor TKAlphaBitmap.CreateFromRes(const ResName: string);
var
  Stream: {$IFDEF FPC}TLazarusResourceStream{$ELSE}TResourceStream{$ENDIF};
begin
  Create;
  try
  {$IFDEF FPC}
    Stream := TLazarusResourceStream.Create(LowerCase(ResName), 'BMP');
  {$ELSE}
    Stream := TResourceStream.Create(HInstance, ResName, RT_RCDATA);
  {$ENDIF}
    try
      LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  except
    Error(sErrGraphicsLoadFromResource);
  end;
end;

destructor TKAlphaBitmap.Destroy;
var
  DC: HDC;
begin
  LockUpdate;
  SetSize(0, 0);
{$IFNDEF MSWINDOWS}
  FImage.Free;
{$ENDIF}
  DC := FCanvas.Handle;
  FCanvas.Handle := 0;
  DeleteDC(DC);
  FCanvas.Free;
  inherited;
end;

procedure TKAlphaBitmap.AlphaDrawTo(ACanvas: TCanvas; X, Y: Integer);
begin
  AlphaStretchDrawTo(ACanvas, Rect(X, Y, X + FWidth, Y + FHeight));
end;

procedure TKAlphaBitmap.AlphaFill(Alpha: Byte; IfEmpty: Boolean);
var
  I: Integer;
  LocHasAlpha: Boolean;
begin
  LocHasAlpha := False;
  if IfEmpty then
    LocHasAlpha := HasAlpha;
  if not LocHasAlpha then
  begin
    LockUpdate;
    try
      for I := 0 to FWidth * FHeight - 1 do
        FPixels[I].A := Alpha;
    finally
      UnlockUpdate;
    end;
  end;
end;

procedure TKAlphaBitmap.AlphaFillOnColorMatch(AColor: TColor; AAlpha: Byte);
var
  I: Integer;
  CS: TKColorRec;
begin
  LockUpdate;
  try
    CS := ColorToColorRec(AColor);
    SwapBR(CS);
    for I := 0 to FWidth * FHeight - 1 do
      if (FPixels[I].R = CS.R) and (FPixels[I].G = CS.G) and (FPixels[I].B = CS.B) then
        FPixels[I].A := AAlpha;
  finally
    UnlockUpdate;
  end;
end;

procedure TKAlphaBitmap.AlphaFillPercent(Percent: Integer; IfEmpty: Boolean);
var
  I: Integer;
begin
  LockUpdate;
  try
    for I := 0 to FWidth * FHeight - 1 do
      if FPixels[I].A <> 0 then
        FPixels[I].A := Percent * FPixels[I].A div 100
      else if IfEmpty then
        FPixels[I].A := Percent * 255 div 100;
  finally
    UnlockUpdate;
  end;
end;

procedure TKAlphaBitmap.AlphaFill(Alpha: Byte; BlendColor: TColor; Gradient, Translucent: Boolean);
var
  I, J, A1, A2, AR, AG, AB, HAlpha: Integer;
  HStep, HSum, VStep, VSum: Single;
  Scan: PKColorRecs;
  CS: TKColorRec;
begin
  LockUpdate;
  try
    VSum := 0; VStep := 0;
    HSum := 0; HStep := 0;
    if Gradient then
    begin
      VStep := Alpha / FHeight;
      VSum := Alpha;
    end;
    CS := ColorToColorRec(BlendColor);
  {$IFNDEF MSWINDOWS}
    for I := 0 to FHeight - 1 do
  {$ELSE}
    for I := FHeight - 1 downto 0 do
  {$ENDIF}
    begin
      Scan := ScanLine[I];
      HAlpha := Alpha;
      if Gradient then
      begin
        HStep := HAlpha / FWidth;
        HSum := HAlpha;
      end;
      for J := 0 to FWidth - 1 do with Scan[J] do
      begin
        A1 := HAlpha;
        A2 := 255 - HAlpha;
        AR := R * A1 + CS.R * A2;
        AG := G * A1 + CS.G * A2;
        AB := B * A1 + CS.B * A2;
        R := AR shr 8;
        G := AG shr 8;
        B := AB shr 8;
        if Translucent then
          A := HAlpha
        else
          A := 255;
        if Gradient then
        begin
          HAlpha := Round(HSum);
          HSum := HSum - HStep;
        end;
      end;
      if Gradient then
      begin
        Alpha := Round(VSum);
        VSum := VSum - VStep;
      end;
    end;
  finally
    UnlockUpdate;
  end;
end;

procedure TKAlphaBitmap.AlphaStretchDrawTo(ACanvas: TCanvas;
  const ARect: TRect);
{$IF DEFINED(MSWINDOWS) OR DEFINED(LCLGTK) OR DEFINED(LCLGTK2)}
var
  I: Integer;
  Tmp: TKAlphaBitmap;
  Ps, Pd: PKColorRecs;
{$IFEND}
begin
{$IF DEFINED(MSWINDOWS) OR DEFINED(LCLGTK) OR DEFINED(LCLGTK2)}
  Tmp := TKAlphaBitmap.Create;
  try
    Tmp.SetSize(FWidth, FHeight);
    Tmp.Fill(MakeColorRec(255, 255, 255, 0));
    Tmp.DrawFrom(ACanvas, ARect);
    for I := 0 to FHeight - 1 do
    begin
      Ps := ScanLine[I];
      Pd := Tmp.ScanLine[I];
      BlendLine(Ps, Pd, FWidth);
    end;
    Tmp.PixelsChanged := True;
    Tmp.DrawTo(ACanvas, ARect);
  finally
    Tmp.Free;
  end;
{$ELSE}
  DrawTo(ACanvas, ARect);
{$IFEND}
end;

procedure TKAlphaBitmap.Assign(Source: TPersistent);
begin
  if Source = nil then
    SetSize(0, 0)
  else if Source is TKAlphaBitmap then
    TKAlphaBitmap(Source).AssignTo(Self)
  else if Source is TGraphic then
    LoadFromGraphic(TGraphic(Source))
  else
    inherited;
end;

procedure TKAlphaBitmap.AssignTo(Dest: TPersistent);
begin
  if Dest is TKAlphaBitmap then with TKAlphaBitmap(Dest) do
  begin
    AutoMirror := Self.AutoMirror;
    DirectCopy := Self.DirectCopy;
    CopyFrom(Self);
  end
end;

procedure TKAlphaBitmap.Clear;
var
  I: Integer;
begin
  LockUpdate;
  try
    for I := 0 to FWidth * FHeight - 1 do
      FPixels[I].Value := 0;
  finally
    UnlockUpdate;
  end;
end;

procedure TKAlphaBitmap.Changed(Sender: TObject);
begin
  inherited;
  FPixelsChanged := True;
end;

procedure TKAlphaBitmap.CombinePixel(X, Y: Integer; Color: TKColorRec);
var
  Index, A1, A2, AR, AG, AB: Integer;
begin
  if (X >= 0) and (X < FWidth) and (Y >= 0) and (Y < FHeight) then
  begin
    LockUpdate;
    try
      SwapBR(Color);
    {$IFDEF MSWINDOWS}
      Index := (FHeight - Y - 1) * FWidth + X;
    {$ELSE}
      Index := Y * FWidth + X;
    {$ENDIF}
      A2 := Color.A;
      if A2 = 255 then
        FPixels[Index] := Color
      else if A2 <> 0 then
      begin
        A1 := 255 - Color.A;
        AR := FPixels[Index].R * A1 + Color.R * A2;
        AG := FPixels[Index].G * A1 + Color.G * A2;
        AB := FPixels[Index].B * A1 + Color.B * A2;
        FPixels[Index].R := AR shr 8;
        FPixels[Index].G := AG shr 8;
        FPixels[Index].B := AB shr 8;
        FPixels[Index].A := 255;
      end;
    finally
      UnlockUpdate;
    end;
  end;
end;

procedure TKAlphaBitmap.CopyFrom(AGraphic: TGraphic);
begin
  if AGraphic is TKAlphaBitmap then
    CopyFromAlphaBitmap(AGraphic as TKAlphaBitmap)
{$IFDEF USE_PNG_SUPPORT}
  else if AGraphic is TKPngImage then
    CopyFromPng(AGraphic as TKPngImage)
{$ENDIF}
  else if AGraphic is TJpegImage then
    CopyFromJpeg(AGraphic as TJpegImage)
  else
    DrawFrom(AGraphic, 0, 0);
end;

procedure TKAlphaBitmap.CopyFromAlphaBitmap(ABitmap: TKAlphaBitmap);
var
  I, Size: Integer;
begin
  LockUpdate;
  try
    SetSize(ABitmap.Width, ABitmap.Height);
    Size := FWidth * SizeOf(TKColorRec);
    for I := 0 to FHeight - 1 do
      Move(ABitmap.ScanLine[I]^, ScanLine[I]^, Size);
  finally
    UnlockUpdate;
  end;
end;

procedure TKAlphaBitmap.CopyFromJpeg(AJpegImage: TJPEGImage);
{$IFDEF FPC}
var
  I, J: Integer;
  C: TKColorRec;
  IM: TLazIntfImage;
  FC: TFPColor;
{$ENDIF}
begin
  LockUpdate;
  try
    SetSize(AJpegImage.Width, AJpegImage.Height);
  {$IFDEF FPC}
    IM := AJpegImage.CreateIntfImage;
    try
      for I := 0 to AJpegImage.Width - 1 do
      begin
        for J := 0 to AJpegImage.Height - 1 do
        begin
          FC := IM.Colors[I, J];
          C := FPColorToColorRec(FC);
          Pixel[I, J] := C;
        end;
      end;
    finally
      IM.Free;
    end;
  {$ELSE}
    // no access to JPEG pixels under Delphi
    DrawFrom(AJpegImage, 0, 0);
    DirectCopy := not HasAlpha;
    AlphaFill(255, True);
  {$ENDIF}
  finally
    UnlockUpdate;
  end;
end;

{$IFDEF USE_PNG_SUPPORT}
procedure TKAlphaBitmap.CopyFromPng(APngImage: TKPngImage);
var
  I, J: Integer;
  C: TKColorRec;
{$IFDEF FPC}
  IM: TLazIntfImage;
  FC: TFPColor;
{$ENDIF}
begin
  LockUpdate;
  try
    SetSize(APngImage.Width, APngImage.Height);
  {$IFDEF FPC}
    IM := APngImage.CreateIntfImage;
    try
  {$ENDIF}
      for I := 0 to APngImage.Width - 1 do
      begin
        for J := 0 to APngImage.Height - 1 do
        begin
        {$IFDEF FPC}
          FC := IM.Colors[I, J];
          C := FPColorToColorRec(FC);
        {$ELSE}
          C.Value := APngImage.Pixels[I, J];
          if APngImage.AlphaScanline[J] <> nil then
            C.A := APngImage.AlphaScanline[J][I]
          else
            C.A := 255;
        {$ENDIF}
          Pixel[I, J] := C;
        end;
      end;
  {$IFDEF FPC}
    finally
      IM.Free;
    end;
  {$ENDIF}
  finally
    UnlockUpdate;
  end;
end;
{$ENDIF}

procedure TKAlphaBitmap.CopyFromRotated(ABitmap: TKAlphaBitmap);
var
  I, J: Integer;
  SrcScan, DstScan: PKColorRecs;
begin
  LockUpdate;
  try
    SetSize(ABitmap.Height, ABitmap.Width);
    for J := 0 to ABitmap.Height - 1 do
    begin
      SrcScan := ABitmap.ScanLine[J];
      for I := 0 to ABitmap.Width - 1 do
      begin
        DstScan := ScanLine[ABitmap.Width - I - 1];
        DstScan[J] := SrcScan[I];
      end;
    end;
  finally
    UnlockUpdate;
  end;
end;

procedure TKAlphaBitmap.CopyFromXY(X, Y: Integer; AGraphic: TGraphic);
begin
  if AGraphic is TKAlphaBitmap then
    CopyFromXYAlphaBitmap(X, Y, AGraphic as TKAlphaBitmap)
{$IFDEF USE_PNG_SUPPORT}
  else if AGraphic is TKPngImage then
    CopyFromXYPng(X, Y, AGraphic as TKPngImage)
{$ENDIF}
  else if AGraphic is TJpegImage then
    CopyFromXYJpeg(X, Y, AGraphic as TJpegImage)
  else
    DrawFrom(AGraphic, X, Y);
end;

procedure TKAlphaBitmap.CopyFromXYAlphaBitmap(X, Y: Integer; ABitmap: TKAlphaBitmap);
var
  I, J: Integer;
begin
  LockUpdate;
  try
    for I := X to X + ABitmap.Width - 1 do
      for J := Y to Y + ABitmap.Height - 1 do
        if (I >= 0) and (I < FWidth) and (J >= 0) and (J < FHeight) then
          Pixels[J * FWidth + I] := ABitmap.Pixels[(J - Y) * ABitmap.Width + (I - X)];
  finally
    UnlockUpdate;
  end;
end;

procedure TKAlphaBitmap.CopyFromXYJpeg(X, Y: Integer; AJpegImage: TJPEGImage);
{$IFDEF FPC}
var
  I, J: Integer;
  C: TKColorRec;
  IM: TLazIntfImage;
  FC: TFPColor;
{$ENDIF}
begin
  LockUpdate;
  try
    {$IFDEF FPC}
      IM := AJpegImage.CreateIntfImage;
      try
        for I := X to X + AJpegImage.Width - 1 do
        begin
          for J := Y to Y + AJpegImage.Height - 1 do
          begin
            if (I >= 0) and (I < FWidth) and (J >= 0) and (J < FHeight) then
            begin
              FC := IM.Colors[I - X, J - Y];
              C := FPColorToColorRec(FC);
              Pixel[I, J] := C;
            end;
          end;
        end;
      finally
        IM.Free;
      end;
    {$ELSE}
      // no access to JPEG pixels under Delphi
      DrawFrom(AJpegImage, X, Y);
      DirectCopy := not HasAlpha;
      AlphaFill(255, True);
    {$ENDIF}
  finally
    UnlockUpdate;
  end;
end;

{$IFDEF USE_PNG_SUPPORT}
procedure TKAlphaBitmap.CopyFromXYPng(X, Y: Integer; APngImage: TKPngImage);
var
  I, J: Integer;
  C: TKColorRec;
{$IFDEF FPC}
  IM: TLazIntfImage;
  FC: TFPColor;
{$ENDIF}
begin
  LockUpdate;
  try
    {$IFDEF FPC}
      IM := APngImage.CreateIntfImage;
      try
    {$ENDIF}
        for I := X to X + APngImage.Width - 1 do
        begin
          for J := Y to Y + APngImage.Height - 1 do
          begin
            if (I >= 0) and (I < FWidth) and (J >= 0) and (J < FHeight) then
            begin
            {$IFDEF FPC}
              FC := IM.Colors[I - X, J - Y];
              C := FPColorToColorRec(FC);
            {$ELSE}
              C.Value := APngImage.Pixels[I - X, J - Y];
              if APngImage.AlphaScanline[J - Y] <> nil then
                C.A := APngImage.AlphaScanline[J - Y][I - X]
              else
                C.A := 255;
            {$ENDIF}
              Pixel[I, J] := C;
            end;
          end;
        end;
    {$IFDEF FPC}
      finally
        IM.Free;
      end;
    {$ENDIF}
  finally
    UnlockUpdate;
  end;
end;

procedure TKAlphaBitmap.CopyToPng(APngImage: TKPngImage);
var
  I, J: Integer;
  C: TKColorRec;
{$IFDEF FPC}
  IM: TLazIntfImage;
  FC: TFPColor;
{$ELSE}
  IM: TKPngImage;
{$ENDIF}
begin
  UpdatePixels;
{$IFDEF FPC}
  IM := TLazIntfImage.Create(0, 0, [riqfRGB, riqfAlpha]);
{$ELSE}
  IM := TKPngImage.CreateBlank(COLOR_RGBALPHA, 8, FWidth, FHeight);
{$ENDIF}
  try
  {$IFDEF FPC}
    IM.SetSize(FWidth, FHeight);
  {$ENDIF}
    for I := 0 to FWidth - 1 do
    begin
      for J := 0 to FHeight - 1 do
      begin
        C := Pixel[I, J];
      {$IFDEF FPC}
        FC := ColorRecToFPColor(C);
        IM.Colors[I, J] := FC;
      {$ELSE}
        IM.Pixels[I, J] := C.Value;
        if IM.AlphaScanline[J] <> nil then
          IM.AlphaScanline[J][I] := C.A;
      {$ENDIF}
      end;
    end;
  {$IFDEF FPC}
    APngImage.LoadFromIntfImage(IM);
  {$ELSE}
    APngImage.Assign(IM);
  {$ENDIF}
  finally
    IM.Free;
  end;
end;
{$ENDIF}

procedure TKAlphaBitmap.Draw(ACanvas: TCanvas; const ARect: TRect);
begin
  if FDirectCopy then
    DrawTo(ACanvas, ARect)
  else
    AlphaStretchDrawTo(ACanvas, ARect);
end;

procedure TKAlphaBitmap.DrawFrom(ACanvas: TCanvas; const ARect: TRect);
begin
  if not Empty then
  begin
    if not CanvasScaled(ACanvas) then
      StretchBitmap(FCanvas.Handle, Rect(0, 0, FWidth, FHeight), ACanvas.Handle, ARect)
    else
    begin
      FCanvas.Brush := ACanvas.Brush;
      DrawFilledRectangle(FCanvas, Rect(0, 0, FWidth, FHeight),
        {$IFDEF MSWINDOWS}GetBkColor(ACanvas.Handle){$ELSE}clWindow{$ENDIF});
    end;
    UpdatePixels;
  end;
end;

procedure TKAlphaBitmap.DrawFrom(AGraphic: TGraphic; X, Y: Integer);
begin
  if not Empty then
  begin
    UpdateHandle;
    FCanvas.Draw(X, Y, AGraphic);
    UpdatePixels;
  end;
end;

procedure TKAlphaBitmap.DrawTo(ACanvas: TCanvas; const ARect: TRect);
begin
  if not Empty then
  begin
    UpdateHandle;
    StretchBitmap(ACanvas.Handle, ARect, FCanvas.Handle, Rect(0, 0, FWidth, FHeight))
  end;
end;

procedure TKAlphaBitmap.Fill(Color: TKColorRec);
var
  I: Integer;
begin
  LockUpdate;
  try
    for I := 0 to FWidth * FHeight - 1 do
      FPixels[I].Value := Color.Value;
  finally
    UnlockUpdate;
  end;
end;

function TKAlphaBitmap.GetEmpty: Boolean;
begin
  Result := (FWidth = 0) and (FHeight = 0);
end;

function TKAlphaBitmap.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TKAlphaBitmap.GetPixel(X, Y: Integer): TKColorRec;
begin
  if (X >= 0) and (X < FWidth) and (Y >= 0) and (Y < FHeight) then
  begin
  {$IFDEF MSWINDOWS}
    Result := FPixels[(FHeight - Y - 1) * FWidth + X];
  {$ELSE}
    Result := FPixels[Y * FWidth + X];
  {$ENDIF}
    SwapBR(Result);
  end else
    Result := MakeColorRec(0,0,0,0);
end;

function TKAlphaBitmap.GetTransparent: Boolean;
begin
  Result := True;
end;

function TKAlphaBitmap.GetScanLine(Index: Integer): PKColorRecs;
begin
  // no checks here
  Result := @FPixels[Index * FWidth];
end;

function TKAlphaBitmap.GetHandle: HBITMAP;
begin
  Result := FHandle;
end;

function TKAlphaBitmap.GetHasAlpha: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FWidth * FHeight - 1 do
    if FPixels[I].A <> 0 then
    begin
      Result := True;
      Break;
    end;
end;

function TKAlphaBitmap.GetWidth: Integer;
begin
  Result := FWidth;
end;

procedure TKAlphaBitmap.GrayScale;
var
  I, Average: Integer;
begin
  LockUpdate;
  try
    for I := 0 to FWidth * FHeight - 1 do
    begin
      // R and B are swapped
      Average := (Integer(7) * FPixels[I].R + Integer(72) * FPixels[I].G + Integer(21) * FPixels[I].B) div 100;
      FPixels[I].R := Average;
      FPixels[I].G := Average;
      FPixels[I].B := Average;
    end;
  finally
    UnlockUpdate;
  end;
end;

procedure TKAlphaBitmap.Brighten(APercent: Single; AMode: TKBrightMode);
var
  I: Integer;
  X: TKColorRec;
begin
  LockUpdate;
  try
    for I := 0 to FWidth * FHeight - 1 do
    begin
      X.Value := BrightColor(ColorRecToColor(FPixels[I]), APercent, AMode);
      FPixels[I].R := X.R;
      FPixels[I].G := X.G;
      FPixels[I].B := X.B;
    end;
  finally
    UnlockUpdate;
  end;
end;

{$IFNDEF FPC}
procedure TKAlphaBitmap.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
begin
  // does nothing
end;
{$ENDIF}

procedure TKAlphaBitmap.LoadFromFile(const Filename: string);
var
  IM: TPicture;
begin
  IM := TPicture.Create;
  try
    IM.LoadFromFile(FileName);
    LoadFromGraphic(IM.Graphic);
  finally
    IM.Free;
  end;
end;

procedure TKAlphaBitmap.LoadFromHandles(ABitmap, AMask: HBITMAP);
begin
  // todo
end;

procedure TKAlphaBitmap.LoadFromGraphic(Image: TGraphic);
begin
  LockUpdate;
  try
    SetSize(Image.Width, Image.Height);
  {$IFDEF MSWINDOWS}
    Canvas.Draw(0, 0, Image);
  {$ELSE}
    if Image is TRasterImage then
      FImage.Assign(TRasterImage(Image).CreateIntfImage);
  {$ENDIF}
    // if bitmap has no alpha channel, create full opacity
    AlphaFill($FF, True);
  finally
    UnlockUpdate;
  end;
end;

procedure TKAlphaBitmap.LoadFromStream(Stream: TStream);
var
  BF: TBitmapFileHeader;
  BI: TBitmapInfoHeader;
begin
  Stream.Read(BF, SizeOf(TBitmapFileHeader));
  if BF.bfType = $4D42 then
  begin
    Stream.Read(BI, SizeOf(TBitmapInfoHeader));
    if BI.biBitCount = 32 then
    begin
      LockUpdate;
      try
        SetSize(BI.biWidth, BI.biHeight);
        Stream.Read(FPixels^, BI.biSizeImage);
        // if bitmap has no alpha channel, create full opacity
        AlphaFill($FF, True);
      {$IFnDEF MSWINDOWS}
        if FAutoMirror then
          MirrorVert;
      {$ENDIF}
      finally
        UnlockUpdate;
      end;
    end;
  end;
end;

procedure TKAlphaBitmap.LockUpdate;
begin
  Inc(FUpdateLock);
end;

procedure TKAlphaBitmap.MirrorHorz;
var
  I, J, Index: Integer;
  SrcScan: PKColorRecs;
  Buf: TKColorRec;
begin
  LockUpdate;
  try
    for I := 0 to FHeight - 1 do
    begin
      SrcScan := ScanLine[I];
      Index := FWidth - 1;
      for J := 0 to (FWidth shr 1) - 1 do
      begin
        Buf := SrcScan[Index];
        SrcScan[Index] := SrcScan[J];
        SrcScan[J] := Buf;
        Dec(Index);
      end;
    end;
  finally
    UnlockUpdate;
  end;
end;

procedure TKAlphaBitmap.MirrorVert;
var
  I, Size, Index: Integer;
  SrcScan, DstScan: PKColorRecs;
  Buf: PKColorRec;
begin
  LockUpdate;
  try
    Size:= FWidth * SizeOf(TKColorRec);
    Index := FHeight - 1;
    GetMem(Buf, Size);
    try
      for I := 0 to (FHeight shr 1) - 1 do
      begin
        SrcScan := ScanLine[I];
        DstScan := ScanLine[Index];
        Move(SrcScan^, Buf^, Size);
        Move(DstScan^, SrcScan^, Size);
        Move(Buf^, DstScan^, Size);
        Dec(Index);
      end;
    finally
      FreeMem(Buf);
    end;
  finally
    UnlockUpdate;
  end;
end;

{$IFNDEF FPC}
procedure TKAlphaBitmap.SaveToClipboardFormat(var AFormat: Word;
  var AData: THandle; var APalette: HPALETTE);
begin
  // does nothing
end;
{$ENDIF}

procedure TKAlphaBitmap.SaveToStream(Stream: TStream);
var
  Size: Integer;
  BF: TBitmapFileHeader;
  BI: TBitmapInfoHeader;
begin
{$IFnDEF MSWINDOWS}
  if FAutoMirror then
    MirrorVert;
{$ENDIF}
  Size := FWidth * FHeight * 4;
  FillChar(BF, SizeOf(TBitmapFileHeader), 0);
  BF.bfType := $4D42;
  BF.bfSize := SizeOf(TBitmapFileHeader) + SizeOf(TBitmapInfoHeader) + Size;
  BF.bfOffBits := SizeOf(TBitmapFileHeader) + SizeOf(TBitmapInfoHeader);
  Stream.Write(BF, SizeOf(TBitmapFileHeader));
  FillChar(BI, SizeOf(TBitmapInfoHeader), 0);
  BI.biSize := SizeOf(TBitmapInfoHeader);
  BI.biWidth := FWidth;
  BI.biHeight := FHeight;
  BI.biPlanes := 1;
  BI.biBitCount := 32;
  BI.biCompression := BI_RGB;
  BI.biSizeImage := Size;
  Stream.Write(BI, SizeOf(TBitmapInfoHeader));
  Stream.Write(FPixels^, Size);
{$IFnDEF MSWINDOWS}
  if FAutoMirror then
    MirrorVert;
{$ENDIF}
end;

procedure TKAlphaBitmap.SetHeight(Value: Integer);
begin
  SetSize(FWidth, Value);
end;

procedure TKAlphaBitmap.SetPixel(X, Y: Integer; Value: TKColorRec);
begin
  if (X >= 0) and (X < FWidth) and (Y >= 0) and (Y < FHeight) then
  begin
    LockUpdate;
    try
      SwapBR(Value);
    {$IFDEF MSWINDOWS}
      FPixels[(FHeight - Y - 1) * FWidth + X] := Value;
    {$ELSE}
      FPixels[Y * FWidth + X] := Value;
    {$ENDIF}
    finally
      UnlockUpdate;
    end;
  end;
end;

procedure TKAlphaBitmap.SetSize(AWidth, AHeight: Integer);
var
{$IFNDEF MSWINDOWS}
  ImgFormatDescription: TRawImageDescription;
{$ELSE}
  BI: TBitmapInfoHeader;
{$ENDIF}
begin
  AWidth := Max(AWidth, 0);
  AHeight := Max(AHeight, 0);
  if (AWidth <> FWidth) or (AHeight <> FHeight) then
  begin
    LockUpdate;
    try
      FWidth := AWidth;
      FHeight := AHeight;
      if FHandle <> 0 then
      begin
        SelectObject(FCanvas.Handle, FOldBitmap);
        DeleteObject(FHandle);
        FHandle := 0;
      {$IFNDEF MSWINDOWS}
        DeleteObject(FMaskHandle);
        FMaskHandle := 0;
      {$ENDIF}
      end;
    {$IFNDEF MSWINDOWS}
      FImage.SetSize(0, 0);
    {$ENDIF}
      FPixels := nil;
      if (FWidth <> 0) and (FHeight <> 0) then
      begin
      {$IFNDEF MSWINDOWS}
        ImgFormatDescription.Init_BPP32_B8G8R8A8_BIO_TTB(FWidth,FHeight);
        FImage.DataDescription := ImgFormatDescription;
        FPixelsChanged := True;
        UpdateHandle;
      {$ELSE}
        FillChar(BI, SizeOf(TBitmapInfoHeader), 0);
        BI.biSize := SizeOf(TBitmapInfoHeader);
        BI.biWidth := FWidth;
        BI.biHeight := FHeight;
        BI.biPlanes := 1;
        BI.biBitCount := 32;
        BI.biCompression := BI_RGB;
        FHandle := GDICheck(CreateDIBSection(FCanvas.Handle, PBitmapInfo(@BI)^, DIB_RGB_COLORS, Pointer(FPixels), 0, 0));
        FOldBitmap := SelectObject(FCanvas.Handle, FHandle);
      {$ENDIF}
      end;
    finally
      UnlockUpdate;
    end;
  end;
end;

procedure TKAlphaBitmap.SetWidth(Value: Integer);
begin
  SetSize(Value, FWidth);
end;

procedure TKAlphaBitmap.SetTransparent(Value: Boolean);
begin
  // does nothing
end;

procedure TKAlphaBitmap.UnlockUpdate;
begin
  if FUpdateLock > 0 then
  begin
    Dec(FUpdateLock);
    if FUpdateLock = 0 then
      Changed(Self);
  end;
end;

procedure TKAlphaBitmap.UpdateHandle;
begin
{$IFNDEF MSWINDOWS}
  if FPixelsChanged then
  begin
    PixelsChanged := False;
    if FHandle <> 0 then
    begin
      DeleteObject(FMaskHandle);
      DeleteObject(SelectObject(FCanvas.Handle, FOldBitmap));
    end;
    FImage.CreateBitmaps(FHandle, FMaskHandle, False);
    FOldBitmap := SelectObject(FCanvas.Handle, FHandle);
    FPixels := PKColorRecs(FImage.PixelData);
  end;
{$ENDIF}
end;

procedure TKAlphaBitmap.UpdatePixels;
begin
{$IFNDEF MSWINDOWS}
  FImage.LoadFromDevice(FCanvas.Handle);
  FPixelsChanged := True;
  UpdateHandle;
{$ENDIF}
end;

{ TKMetafile }

{$IFDEF MSWINDOWS}

constructor TKMetafile.Create;
begin
  inherited;
  FCopyOnAssign := True;
  FEmfHandle := 0;
  FRequiredHeight := 0;
  FRequiredWidth := 0;
  FWmfHandle := 0;
end;

destructor TKMetafile.Destroy;
begin
  Clear;
  inherited;
end;

procedure TKMetafile.Assign(Source: TPersistent);
var
  Stream: TMemoryStream;
begin
  if Source is TKMetafile then
  begin
    Clear;
    FEnhanced := TKMetafile(Source).Enhanced;
    if TKMetafile(Source).CopyOnAssign then
    begin
      Stream := TMemoryStream.Create;
      try
        TKMetafile(Source).SaveToStream(Stream);
        Stream.Seek(0, soFromBeginning);
        LoadFromStream(Stream);
      finally
        Stream.Free;
      end;
    end else
    begin
      // here, the source loses the images!
      TKMetafile(Source).Release(FWmfHandle, FEmfHandle);
    end;
    FRequiredHeight := TKMetafile(Source).Height;
    FRequiredWidth := TKMetafile(Source).Width;
  end;
end;

procedure TKMetafile.Clear;
begin
  if FWmfHandle <> 0 then
  begin
    DeleteMetafile(FWmfHandle);
    FWmfHandle := 0;
  end;
  if FEmfHandle <> 0 then
  begin
    DeleteEnhMetafile(FEmfHandle);
    FEmfHandle := 0;
  end;
end;

procedure TKMetafile.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  BM: TKAlphaBitmap;
begin
  inherited;
  if FWMfHandle <> 0 then
  begin
    if FRequiredWidth * FRequiredHeight > 0 then
    begin
      BM := TKAlphaBitmap.Create;
      try
        BM.DirectCopy := True;
        BM.SetSize(FRequiredWidth, FRequiredHeight);
        BM.Fill(MakeColorRec(255,255,255,255));
        PlayMetafile(BM.Canvas.Handle, FWmfHandle);
        BM.DirectCopy := False;
        BM.DrawTo(ACanvas, Rect);
      finally
        BM.Free;
      end;
    end;
  end
  else if FEMFHandle <> 0 then
  begin
    PlayEnhMetafile(ACanvas.Handle, FEmfHandle, Rect);
  end;
end;

function TKMetafile.GetEmpty: Boolean;
begin
  Result := (FWmfHandle = 0) and (FEmfHandle = 0);
end;

function TKMetafile.GetHeight: Integer;
begin
  Result := FRequiredHeight;
end;

function TKMetafile.GetTransparent: Boolean;
begin
  Result := False;
end;

function TKMetafile.GetWidth: Integer;
begin
  Result := FRequiredWidth;
end;

procedure TKMetafile.LoadFromStream(Stream: TStream);
var
  S: AnsiString;
  EHDR: TEnhMetaheader;
  MFP: TMetaFilePict;
begin
  SetLength(S, Stream.Size - Stream.Position);
  if S <> '' then
  begin
    Stream.Read(EHDR, SizeOf(TEnhMetaHeader));
    Stream.Seek(-SizeOf(TEnhMetaHeader), soFromCurrent);
    Stream.Read(S[1], Length(S));
    if FEnhanced and (EHDR.iType = EMR_HEADER) then
    begin
      FEmfHandle := SetEnhMetafileBits(Length(S), @S[1]);
      FRequiredWidth := EHDR.rclBounds.Right - EHDR.rclBounds.Left;
      FRequiredHeight := EHDR.rclBounds.Bottom - EHDR.rclBounds.Top;
    end else
    begin
      FWmfHandle := SetMetafileBitsEx(Length(S), @S[1]);
      if FWmfHandle <> 0 then
      begin
        // obtain width and height
        with MFP do
        begin
          MM := MM_ANISOTROPIC;
          xExt := 0;
          yExt := 0;
          hmf := 0;
        end;
        FEmfHandle := SetWinMetaFileBits(Length(S), @S[1], 0, MFP);
        if FEmfHandle <> 0 then
        begin
          if GetEnhMetaFileHeader(FEmfHandle, SizeOf(TEnhMetaHeader), @EHDR) > 0 then
          begin
            FRequiredWidth := EHDR.rclBounds.Right - EHDR.rclBounds.Left;
            FRequiredHeight := EHDR.rclBounds.Bottom - EHDR.rclBounds.Top;
          end;
          DeleteEnhMetafile(FEmfHandle);
          FEmfHandle := 0;
        end;
      end;
    end;
  end;
end;

procedure TKMetafile.Release(out AWmfHandle: HMETAFILE; out AEmfHandle: HENHMETAFILE);
begin
  AWmfHandle := FWmfHandle;
  FWmfHandle := 0;
  AEmfHandle := FEmfHandle;
  FEmfHandle := 0;
end;

procedure TKMetafile.SaveToStream(Stream: TStream);
var
  S: AnsiString;
  Size: Integer;
begin
  S := '';
  if FWmfHandle <> 0 then
  begin
    Size := GetMetaFileBitsEx(FWmfHandle, 0, nil);
    if Size > 0 then
    begin
      SetLength(S, Size);
      GetMetafileBitsEx(FWmfHandle, Size, @S[1]);
    end;
  end
  else if FEmfHandle <> 0 then
  begin
    Size := GetEnhMetaFileBits(FEmfHandle, 0, nil);
    if Size > 0 then
    begin
      SetLength(S, Size);
      GetEnhMetafileBits(FEmfHandle, Size, @S[1]);
    end;
  end;
  if S <> '' then
    Stream.Write(S[1], Length(S));
end;

procedure TKMetafile.SetEMFHandle(const Value: HENHMETAFILE);
begin
  Clear;
  FEMFHandle := Value;
end;

procedure TKMetafile.SetEnhanced(const Value: Boolean);
begin
  FEnhanced := Value;
end;

procedure TKMetafile.SetHeight(Value: Integer);
begin
  FRequiredHeight := Value;
end;

procedure TKMetafile.SetWidth(Value: Integer);
begin
  FRequiredWidth := Value;
end;

procedure TKMetafile.SetWMFHandle(const Value: HMETAFILE);
begin
  Clear;
  FWMFHandle := Value;
end;

{$ENDIF}

{ TKTextBox }

constructor TKTextBox.Create;
begin
  inherited;
  FAttributes := [];
  FBackColor := clWhite;
  FHAlign := halLeft;
  FHasTabs := False;
  FHPadding := 0;
  FSelBkgnd := clHighlight;
  FSelColor := clHighlightText;
  FSelEnd := 0;
  FSelStart := 0;
  FSpacesForTab := 8;
  FText := '';
  FVAlign := valCenter;
  FVPadding := 0;
end;

procedure TKTextBox.Draw(ACanvas: TCanvas; const ARect: TRect);
var
  Y: Integer;
  TmpRect: TRect;
  PrevRgn: HRGN;
begin
  if not IsRectEmpty(ARect) then
  begin
    if taFillRect in Attributes then
      DrawFilledRectangle(ACanvas, ARect, BackColor);
    if FText <> '' then
    begin
      Initialize(ACanvas, ARect);
      if not IsRectEmpty(FClipRect) then
      begin
        Y := GetVertPos;
        TmpRect := FClipRect;
        if taClip in Attributes then
        begin
          TranslateRectToDevice(ACanvas.Handle, TmpRect);
          PrevRgn := RgnCreateAndGet(ACanvas.Handle);
          try
            if ExtSelectClipRect(ACanvas.Handle, TmpRect, RGN_AND, PrevRgn) then
            begin
              if not (taFillText in Attributes) then
                SetBkMode(ACanvas.Handle, TRANSPARENT);
              Process(Y, tbfDraw);
            end;
          finally
            RgnSelectAndDelete(ACanvas.Handle, PrevRgn);
          end;
        end else
        begin
          if not (taFillText in Attributes) then
            SetBkMode(ACanvas.Handle, TRANSPARENT);
          Process(Y, tbfDraw);
        end;
      end;
    end;
  end;
end;

function TKTextBox.GetHorzPos(ATextWidth: Integer): Integer;
begin
  case HAlign of
    halCenter:
      Result := Max(FClipRect.Left, (FClipRect.Left + FClipRect.Right - ATextWidth) div 2);
    halRight:
      Result := FClipRect.Right - ATextWidth;
  else
    Result := FClipRect.Left;
  end;
end;

function TKTextBox.GetVertPos: Integer;
begin
  case VAlign of
    valCenter:
    begin
      Process(0, tbfMeasure);
      Result := Max(FClipRect.Top, (FClipRect.Bottom + FClipRect.Top - FCalcRect.Top) div 2);
    end;
    valBottom:
    begin
      Process(0, tbfMeasure);
      Result := FClipRect.Bottom - FCalcRect.Top;
    end
  else
    Result := FClipRect.Top;
  end;
end;

function TKTextBox.IndexToRect(ACanvas: TCanvas; const ARect: TRect; AIndex: Integer): TRect;
var
  Y: Integer;
begin
  Initialize(ACanvas, ARect);
  Y := GetVertPos;
  FIndex := AIndex;
  Process(Y, tbfGetRect);
  Result := FCalcRect;
end;

procedure TKTextBox.Initialize(ACanvas: TCanvas; const ARect: TRect);
begin
  FCanvas := ACanvas;
  FClipRect := ARect;
  InflateRect(FClipRect, -HPadding, -VPadding);
  FFontHeight := GetFontHeight(FCanvas.Handle);
end;

procedure TKTextBox.Measure(ACanvas: TCanvas; const ARect: TRect; var AWidth, AHeight: Integer);
begin
  Initialize(ACanvas, ARect);
  Process(0, tbfMeasure);
  AWidth := FCalcRect.Left;
  AHeight := FCalcRect.Top;
end;

procedure TKTextBox.Process(Y: Integer; AFunction: TKTextBoxFunction);
var
  StartEllipsis, EndEllipsis, PathEllipsis: Boolean;
  Width, EllipsisWidth: Integer;
  NormalColor, NormalBkgnd: TColor;

  procedure Measure(AStart, ALen: Integer);
  begin
    FCalcRect.Left := Max(FCalcRect.Left, TextExtent(FCanvas, FText, AStart, ALen, FHasTabs, FSpacesForTab).cx);
  end;

  procedure GetIndex(Y: Integer; AStart, ALen: Integer);
  var
    Index, NewIndex, X, Width: Integer;
  begin
    if FIndex < 0 then
    begin
      if not (taIncludePadding in Attributes) and (Y <= FCalcRect.Top) and (FCalcRect.Top < Y + FFontHeight) or
        (taIncludePadding in Attributes) and (
          (AStart = 1) and (FClipRect.Top <= FCalcRect.Top) and (FCalcRect.Top < Y + FFontHeight) or
          (AStart + ALen = Length(FText) + 1) and (Y <= FCalcRect.Top) and (FCalcRect.Top < FClipRect.Bottom)
        ) then
      begin
        Width := TextExtent(FCanvas, FText, AStart, ALen, FHasTabs, FSpacesForTab).cx;
        X := GetHorzPos(Width);
        if not (taIncludePadding in Attributes) and (X <= FCalcRect.Left) and (FCalcRect.Left < X + Width) or
          (taIncludePadding in Attributes) and (FClipRect.Left <= FCalcRect.Left) and (FCalcRect.Left < FClipRect.Right) then
        begin
          Index := AStart;
          while (FIndex < 0) and (Index <= AStart + ALen - 1) do
          begin
            NewIndex := StrNextCharIndex(FText, Index);
            Inc(X, TextExtent(FCanvas, FText, Index, NewIndex - Index, FHasTabs, FSpacesForTab).cx);
            if FCalcRect.Left < X then
              FIndex := Index;
            Index := NewIndex;
          end;
          if (taIncludePadding in Attributes) and (FIndex < 0) and (FCalcRect.Left < FClipRect.Right) then
            FIndex := Index;
        end;
      end;
    end;
  end;

  procedure GetRect(Y: Integer; AStart, ALen: Integer);
  var
    Index, NewIndex, X, Width: Integer;
  begin
    if (FIndex >= AStart) and (FIndex <= ALen) then
    begin
      Index := AStart;
      Width := TextExtent(FCanvas, FText, AStart, ALen, FHasTabs, FSpacesForTab).cx;
      X := GetHorzPos(Width);
      while Index < FIndex do
      begin
        NewIndex := StrNextCharIndex(FText, Index);
        Inc(X, TextExtent(FCanvas, FText, Index, NewIndex - Index, FHasTabs, FSpacesForTab).cx);
        Index := NewIndex;
      end;
      NewIndex := StrNextCharIndex(FText, Index);
      FCalcRect := Rect(X, Y, X + TextExtent(FCanvas, FText, Index, NewIndex - Index, FHasTabs, FSpacesForTab).cx, Y + FFontHeight);
    end
  end;

  procedure Draw(Y: Integer; AStart, ALen: Integer);
  var
    DrawEllipsis, DrawFileName, SetNormalColors, SetSelectionColors: Boolean;
    AWidth, Index, NewIndex, SlashPos, FileNameLen, EllipsisMaxX, X: Integer;
    S: TKString;
  begin
    if (Y >= FClipRect.Top - FFontHeight) and (Y <= FClipRect.Bottom) then
    begin
      DrawEllipsis := False;
      DrawFileName := False;
      SlashPos := 0;
      FileNameLen := 0;
      if (StartEllipsis or EndEllipsis or PathEllipsis) and (ALen > 1) then
      begin
        AWidth := TextExtent(FCanvas, FText, AStart, ALen, FHasTabs, FSpacesForTab).cx;
        if AWidth > Width then
        begin
          AWidth := 0;
          Index := AStart;
          if EndEllipsis or StartEllipsis then
          begin
            EllipsisMaxX := Width - EllipsisWidth;
            if EndEllipsis then
            begin
              while Index <= AStart + ALen - 1 do
              begin
                NewIndex := StrNextCharIndex(FText, Index);
                Inc(AWidth, TextExtent(FCanvas, FText, Index, NewIndex - Index, FHasTabs, FSpacesForTab).cx);
                if (AWidth >= EllipsisMaxX) and (Index > AStart) then
                  Break
                else
                  Index := NewIndex;
              end;
              ALen := Index - AStart;
            end else
            begin
              Index := AStart + ALen - 1;
              while Index > AStart do
              begin
                NewIndex := StrPreviousCharIndex(FText, Index);
                Inc(AWidth, TextExtent(FCanvas, FText, Index, Index - NewIndex, FHasTabs, FSpacesForTab).cx);
                if AWidth >= EllipsisMaxX then
                  Break
                else
                  Index := NewIndex;
              end;
              if Index = AStart + ALen - 1 then
              begin
                AStart := Index;
                ALen := 1;
              end else
              begin
                Dec(ALen, Index - AStart);
                AStart := Index + 1;
              end;
            end;
            DrawEllipsis := True;
          end
          else if PathEllipsis then
          begin
            SlashPos := AStart + ALen - 1;
            while (SlashPos > 0) and not CharInSetEx(FText[SlashPos], ['/', '\']) do
              Dec(SlashPos);
            Dec(SlashPos);
            if SlashPos > 0 then
            begin
              DrawEllipsis := True;
              DrawFileName := True;
              FileNameLen := AStart + ALen - SlashPos;
              EllipsisMaxX := Width - TextExtent(FCanvas, FText, SlashPos, FileNameLen, FHasTabs, FSpacesForTab).cx - EllipsisWidth;
              while (Index <= SlashPos) do
              begin
                NewIndex := StrNextCharIndex(FText, Index);
                Inc(AWidth, TextExtent(FCanvas, FText, Index, NewIndex - Index, FHasTabs, FSpacesForTab).cx);
                if AWidth >= EllipsisMaxX then
                  Break
                else
                  Index := NewIndex;
              end;
              ALen := Index - AStart;
            end;
          end;
        end;
      end;
      if DrawEllipsis then
      begin
        if DrawFileName then
          S := Copy(FText, AStart, ALen) + cEllipsis + Copy(FText, AStart + SlashPos, FileNameLen)
        else if EndEllipsis then
          S := Copy(FText, AStart, ALen) + cEllipsis
        else
          S := cEllipsis + Copy(FText, AStart, ALen);
        AStart := 1;
        ALen := Length(S);
      end else
        S := FText;
      X := GetHorzPos(TextExtent(FCanvas, S, AStart, ALen, FHasTabs, FSpacesForTab).cx);
      if DrawEllipsis or (SelStart = SelEnd) then
        TextOutput(FCanvas, X, Y, S, AStart, ALen, FHasTabs, FSpacesForTab)
      else
      begin
        AWidth := 0;
        Index := AStart;
        SlashPos := Index; // reuse
        while (Index <= AStart + ALen) do
        begin
          DrawFileName := False;  // reuse
          SetNormalColors := False;
          SetSelectionColors := False;
          if Index = SelStart then
          begin
            DrawFileName := True;
            SetSelectionColors := True;
          end
          else if Index = SelEnd then
          begin
            DrawFileName := True;
            SetNormalColors := True;
          end
          else if Index = AStart + ALen then
          begin
            DrawFileName := True;
          end;
          if DrawFileName then
          begin
            if Index > SlashPos then
            begin
              if SetNormalColors then
                DrawFilledRectangle(FCanvas, Rect(X, Y, X + AWidth, Y + FFontHeight), FBackColor);
              if not (taFillText in Attributes) then
                SetBkMode(FCanvas.Handle, TRANSPARENT);
              TextOutput(FCanvas, X, Y, S, SlashPos, Index - SlashPos, FHasTabs, FSpacesForTab);
            end;
            Inc(X, AWidth);
            AWidth := 0;
            SlashPos := Index;
          end;
          if Index < AStart + ALen then
          begin
            NewIndex := StrNextCharIndex(FText, Index);
            Inc(AWidth, TextExtent(FCanvas, FText, Index, NewIndex - Index, FHasTabs, FSpacesForTab).cx);
            Index := NewIndex;
          end else
            Inc(Index);
          if SetNormalColors then
          begin
            FCanvas.Font.Color := NormalColor;
            FCanvas.Brush.Color := NormalBkgnd;
          end
          else if SetSelectionColors then
          begin
            FCanvas.Font.Color := SelColor;
            FCanvas.Brush.Color := SelBkgnd;
          end;
        end;
      end;
    end;
  end;

var
  I, Index, TextLen, LineBegin, LineBreaks, Vert, TrimStart, TrimLen: Integer;
  WordBreak, LineBreak, WhiteSpace, PrevWhiteSpace, FirstWord,
  WasLineBreak, WrapText: Boolean;
  Size: TSize;
begin
  case AFunction of
    tbfMeasure: FCalcRect := CreateEmptyRect;
    tbfGetIndex: FIndex := -1;
    tbfGetRect: FCalcRect := CreateEmptyRect;
    tbfDraw: ;
  end;
  Vert := Y;
  if FText <> '' then
  begin
    LineBegin := 1;
    LineBreaks := 0;
    TextLen := Length(FText);
    Width := FClipRect.Right - FClipRect.Left;
    WordBreak := taWordBreak in Attributes;
    LineBreak := taLineBreak in Attributes;
    WrapText := taWrapText in Attributes; //JR:20091229
    if AFunction = tbfDraw then
    begin
      StartEllipsis := taStartEllipsis in Attributes;
      EndEllipsis := taEndEllipsis in Attributes;
      PathEllipsis := taPathEllipsis in Attributes;
      EllipsisWidth := TextExtent(FCanvas, cEllipsis, 1, Length(cEllipsis)).cx;
      NormalColor := FCanvas.Font.Color;
      NormalBkgnd := FCanvas.Brush.Color;
    end;
    if WordBreak or LineBreak then
    begin
      I := LineBegin;
      Index := LineBegin;
      WhiteSpace := True;
      FirstWord := True;
      WasLineBreak := False;
      while I <= TextLen + 1 do
      begin
        PrevWhiteSpace := WhiteSpace;
        WhiteSpace := CharInSetEx(FText[I], cWordBreaks + cLineBreaks);
        if (not PrevWhiteSpace and WhiteSpace and (I > LineBegin))
          or (not PrevWhiteSpace and WrapText and (I > LineBegin)) then //JR:20091229
        begin
          if (WordBreak or WrapText) and (LineBreaks = 0) and not FirstWord then
          begin
            TrimStart := LineBegin;
            TrimLen := I - LineBegin;
            TextTrim(FText, TrimStart, TrimLen);
            Size := TextExtent(FCanvas, FText, TrimStart, TrimLen, FHasTabs, FSpacesForTab);
            if Size.cx > Width then
              Inc(LineBreaks);
          end;
          if LineBreaks > 0 then
          begin
            if Index > LineBegin then
            begin
              TrimStart := LineBegin;
              TrimLen := Index - LineBegin;
              TextTrim(FText, TrimStart, TrimLen);
              case AFunction of
                tbfMeasure: Measure(TrimStart, TrimLen);
                tbfGetIndex: GetIndex(Vert, TrimStart, TrimLen);
                tbfGetRect: GetRect(Vert, TrimStart, TrimLen);
                tbfDraw: Draw(Vert, TrimStart, TrimLen);
              end;
              LineBegin := Index;
            end;
            Inc(Vert, FFontHeight * LineBreaks);
            LineBreaks := 0;
          end;
          Index := I;
          FirstWord := False;
        end;
        if LineBreak then
          if CharInSetEx(FText[I], cLineBreaks) then
          begin
            if not WasLineBreak then
            begin
              Inc(LineBreaks);
              WasLineBreak := True;
            end;
          end else
            WasLineBreak := False;
        Inc(I);
      end;
    end;
    if LineBegin <= TextLen then
    begin
      TrimStart := LineBegin;
      TrimLen := TextLen - LineBegin + 1;
      TextTrim(FText, TrimStart, TrimLen);
      case AFunction of
        tbfMeasure: Measure(TrimStart, TrimLen);
        tbfGetIndex: GetIndex(Vert, TrimStart, TrimLen);
        tbfGetRect: GetRect(Vert, TrimStart, TrimLen);
        tbfDraw: Draw(Vert, TrimStart, TrimLen)
      end;
      Inc(Vert, FFontHeight * (1 + LineBreaks));
    end;
  end;
  case AFunction of
    tbfMeasure:
    begin
      if FText = '' then
        FCalcRect.Top := FFontHeight
      else
        FCalcRect.Top := Vert - Y;
    end;
    tbfGetIndex: ;
    tbfGetRect: if FText = '' then
    begin
      I := GetHorzPos(0);
      FCalcRect := Rect(I, Y, I, Y + FFontHeight);
    end;
    tbfDraw: ;
  end;
end;

procedure TKTextBox.SetText(const AText: TKString);
begin
  if AText <> FText then
  begin
    FText := AText;
    FHasTabs := Pos(cTAB, FText) > 0;
  end;
end;

function TKTextBox.PointToIndex(ACanvas: TCanvas; const ARect: TRect; APoint: TPoint): Integer;
var
  Y: Integer;
begin
  Initialize(ACanvas, ARect);
  Y := GetVertPos;
  FCalcRect.TopLeft := APoint;
  Process(Y, tbfGetIndex);
  Result := FIndex;
end;

class function TKTextBox.TextExtent(ACanvas: TCanvas; const AText: TKString;
  AStart, ALen: Integer; AExpandTabs: Boolean; ASpacesForTab: Integer): TSize;
var
  S: TKString;
  TextPtr: PKText;
begin
  S := '';
  if AExpandTabs then
  begin
    S := Copy(AText, AStart, ALen);
    ConvertTabsToSpaces(S, ASpacesForTab);
    TextPtr := @S[1];
    ALen := Length(S);
  end else
    TextPtr := @AText[AStart];
{$IFDEF STRING_IS_UNICODE}
 {$IFDEF FPC}
  {$IFDEF USE_CANVAS_METHODS}
  if not AExpandTabs then
    S := Copy(AText, AStart, ALen);
  Result := ACanvas.TextExtent(S); // little slower but more secure in Lazarus
  {$ELSE}
  GetTextExtentPoint32(ACanvas.Handle, TextPtr, ALen, Result);
  {$ENDIF}
 {$ELSE}
  GetTextExtentPoint32(ACanvas.Handle, TextPtr, ALen, Result);
 {$ENDIF}
{$ELSE}
  GetTextExtentPoint32W(ACanvas.Handle, TextPtr, ALen, Result);
{$ENDIF}
end;

class procedure TKTextBox.TextOutput(ACanvas: TCanvas; X, Y: Integer;
  const AText: TKString; AStart, ALen: Integer; AExpandTabs: Boolean; ASpacesForTab: Integer);
var
  S: TKString;
  TextPtr: PKText;
begin
  if AExpandTabs then
  begin
    S := Copy(AText, AStart, ALen);
    ConvertTabsToSpaces(S, ASpacesForTab);
    TextPtr := @S[1];
    ALen := Length(S);
  end else
  begin
    TextPtr := @AText[AStart];
    S := AText;
  end;
{$IFDEF STRING_IS_UNICODE}
 {$IFDEF FPC}
  {$IFDEF USE_CANVAS_METHODS}
  if not AExpandTabs then
    S := Copy(S, AStart, ALen);
  ACanvas.TextOut(X, Y, S); // little slower but more secure in Lazarus
  {$ELSE}
  TextOut(ACanvas.Handle, X, Y, TextPtr, ALen);
  {$ENDIF}
 {$ELSE}
  TextOut(ACanvas.Handle, X, Y, TextPtr, ALen);
 {$ENDIF}
{$ELSE}
  TextOutW(ACanvas.Handle, X, Y, TextPtr, ALen);
{$ENDIF}
end;

procedure TKTextBox.TextTrim(const AText: TKString; var AStart, ALen: Integer);
begin
  if taLineBreak in Attributes then
    TrimWhiteSpaces(AText, AStart, ALen, cLineBreaks);
  if taTrimWhiteSpaces in Attributes then
    TrimWhiteSpaces(AText, AStart, ALen, cWordBreaks);
end;

{ TKDragWindow }

{$IFDEF MSWINDOWS}
const
  cLayeredWndClass = 'KControls drag window';

function DragWndProc(Window: HWnd; Msg, WParam, LParam: Longint): Longint; stdcall;
var
  DC: HDC;
  PS: TPaintStruct;
  AWindow: TKDragWindow;
begin
  case Msg of
    WM_PAINT:
    begin
      AWindow := TKDragWindow(GetWindowLong(Window, GWL_USERDATA));
      if (AWindow <> nil) and AWindow.BitmapFilled then
      begin
        if wParam = 0 then
          DC := BeginPaint(Window, PS)
        else
          DC := wParam;
        try
          BitBlt(DC, 0, 0, AWindow.Bitmap.Width, AWindow.Bitmap.Height,
            AWindow.Bitmap.Canvas.Handle, 0, 0, SRCCOPY);
        finally
          if wParam = 0 then EndPaint(Window, PS);
        end;
      end;
      Result := 1;
    end;
  else
    Result := DefWindowProc(Window, Msg, WParam, LParam);
  end;
end;

{$ELSE}

type

  { TKDragForm }

  TKDragForm = class(THintWindow)
  private
    FWindow: TKDragWindow;
    procedure WMEraseBkGnd(var Msg: TLMessage); message LM_ERASEBKGND;
  protected
    procedure Paint; override;
  public
    constructor CreateDragForm(AWindow: TKDragWindow);
  end;

{ TKDragForm }

constructor TKDragForm.CreateDragForm(AWindow: TKDragWindow);
begin
  inherited Create(nil);
  FWindow := AWindow;
  ShowInTaskBar := stNever;
end;

procedure TKDragForm.Paint;
begin
  if FWindow.Active and FWindow.BitmapFilled then
    Canvas.Draw(0, 0, FWindow.FBitmap);
end;

procedure TKDragForm.WMEraseBkGnd(var Msg: TLMessage);
begin
  Msg.Result := 1;
end;

{$ENDIF}

constructor TKDragWindow.Create;
{$IFDEF MSWINDOWS}
var
  Cls: Windows.TWndClass;
  ExStyle: Cardinal;
{$ENDIF}
begin
  inherited;
  FActive := False;
  FBitmap := TKAlphaBitmap.Create;
  FInitialPos := CreateEmptyPoint;
{$IFDEF MSWINDOWS}
  FUpdateLayeredWindow := GetProcAddress(GetModuleHandle('user32.dll'), 'UpdateLayeredWindow');
  FLayered := Assigned(FUpdateLayeredWindow);
  Cls.style := CS_SAVEBITS;
  Cls.lpfnWndProc := @DragWndProc;
  Cls.cbClsExtra := 0;
  Cls.cbWndExtra := 0;
  Cls.hInstance := HInstance;
  Cls.hIcon := 0;
  Cls.hCursor := 0;
  Cls.hbrBackground := 0;
  Cls.lpszMenuName := nil;
  Cls.lpszClassName := cLayeredWndClass;
  Windows.RegisterClass(Cls);
  ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
  if FLayered then
    ExStyle := ExStyle or WS_EX_LAYERED or WS_EX_TRANSPARENT;
  FWindow := CreateWindowEx(ExStyle, cLayeredWndClass, '', WS_POPUP,
    Integer(CW_USEDEFAULT), Integer(CW_USEDEFAULT), Integer(CW_USEDEFAULT),
    Integer(CW_USEDEFAULT), 0, 0, HInstance, nil);
  Windows.SetWindowLong(FWindow, GWL_USERDATA, Integer(Self));
{$ELSE}
  FDragForm := TKDragForm.CreateDragForm(Self);
  FLayered := False;
{$ENDIF}
end;

destructor TKDragWindow.Destroy;
begin
  inherited;
  Hide;
{$IFDEF MSWINDOWS}
  DestroyWindow(FWindow);
  Windows.UnregisterClass(cLayeredWndClass, HInstance);
{$ELSE}
  FDragForm.Free;
{$ENDIF}
  FBitmap.Free;
end;

procedure TKDragWindow.Hide;
begin
  if FActive then
  begin
  {$IFDEF MSWINDOWS}
    ShowWindow(FWindow, SW_HIDE);
  {$ELSE}
    FDragForm.Hide;
  {$ENDIF}
    FActive := False;
  end;
end;

procedure TKDragWindow.Init(IniCtrl: TCustomControl; const ARect: TRect;
  const AInitialPos: TPoint; AMasterAlpha: Byte; AGradient: Boolean);
var
  Org: TPoint;
  W, H: Integer;
  ScreenDC: HDC;
begin
  if not FActive and ((IniCtrl = nil) or (IniCtrl is TKCustomControl)) then
  begin
    FActive := True;
    FBitmapFilled := False;
    FControl := IniCtrl;
    FMasterAlpha := AMasterAlpha;
    FGradient := AGradient;
    FInitialPos := AInitialPos;
    FRect := ARect;
    W := ARect.Right - ARect.Left;
    H := ARect.Bottom - ARect.Top;
    FBitmap.SetSize(W, H);
    ScreenDC := GetDC(0);
    try
      FAlphaEffects := GetDeviceCaps(ScreenDC, BITSPIXEL) >= 15;
      // because alpha blending is not nice elsewhere
    finally
      ReleaseDC(0, ScreenDC);
    end;
    if FControl <> nil then
    begin
      Org := FControl.ClientToScreen(ARect.TopLeft);
      // to be compatible with all LCL widgetsets we must copy the control's part
      // while painting in TKCustomControl.Paint!
      TKCustomControl(FControl).MemoryCanvas := FBitmap.Canvas;
      TKCustomControl(FControl).MemoryCanvasRect := ARect;
      TKCustomControl(FControl).Repaint;
    end else
      Org := ARect.TopLeft;
  {$IFDEF MSWINDOWS}
    if FLayered then with FBlend do
    begin
      BlendOp := AC_SRC_OVER;
      BlendFlags := 0;
      SourceConstantAlpha := 255;
      if FAlphaEffects then
        AlphaFormat := AC_SRC_ALPHA
      else
        AlphaFormat := 0;
    end;
    SetWindowPos(FWindow, HWND_TOP, Org.X, Org.Y, W, H, SWP_NOACTIVATE);
  {$ELSE}
    FDragForm.SetBounds(Org.X, Org.Y, W, H);
  {$ENDIF}
  end;
end;

procedure TKDragWindow.Move(ARect: PRect; const ACurrentPos: TPoint; AShowAlways: Boolean);
var
  R: TRect;
  DX, DY: Integer;
  BlendColor: TColor;
  ChangedPos: Boolean;
{$IFDEF MSWINDOWS}
  ScreenDC: HDC;
  CanvasOrigin: TPoint;
{$ENDIF}
begin
  if FActive then
  begin
    ChangedPos := False;
    DX := ACurrentPos.X - FInitialPos.X;
    DY := ACurrentPos.Y - FInitialPos.Y;
    if (DX <> 0) or (DY <> 0) then
    begin
      FInitialPos := ACurrentPos;
      ChangedPos := True;
    end;
    if ARect <> nil then
      ChangedPos := ChangedPos or not EqualRect(ARect^, FRect);
    if ((FControl = nil) or (TKCustomControl(FControl).MemoryCanvas = nil)) and not FBitmapFilled or (ARect <> nil) then
    begin
      FBitmapFilled := True;
      if ARect <> nil then
        FBitmap.SetSize(ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);
      FBitmap.UpdatePixels;
      if FAlphaEffects then
      begin
        if FLayered then
          BlendColor := clBlack
        else
          BlendColor := clWhite;
        FBitmap.AlphaFill(FMasterAlpha, BlendColor, FGradient, FLayered);
        FBitmap.UpdateHandle;
      end;
    end;
    if ChangedPos or AShowAlways then
    begin
    {$IFDEF MSWINDOWS}
      if ARect <> nil then
        R := ARect^
      else
        GetWindowRect(FWindow, R);
      KFunctions.OffsetRect(R, DX, DY);
      if FLayered then
      begin
        R.Right := FBitmap.Width;
        R.Bottom := FBitmap.Height;
        CanvasOrigin := CreateEmptyPoint;
        ScreenDC := GetDC(0);
        try
          if FUpdateLayeredWindow(FWindow, ScreenDC, @R.TopLeft, PSize(@R.BottomRight),
            FBitmap.Canvas.Handle, @CanvasOrigin, clNone, @FBlend, ULW_ALPHA) then
            if FBitmapFilled then
              ShowWindow(FWindow, SW_SHOWNOACTIVATE);
        finally
          ReleaseDC(0, ScreenDC);
        end;
      end
      else if FBitmapFilled then
        SetWindowPos(FWindow, 0, R.Left, R.Top, 0, 0, SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER or SWP_SHOWWINDOW);
    {$ELSE}
      if ARect <> nil then
        R := ARect^
      else
        R := FDragForm.BoundsRect;
      OffsetRect(R, DX, DY);
      FDragForm.BoundsRect := R;
      if FBitmapFilled then
      begin
        FDragForm.Visible := True;
        if FControl <> nil then
          SetCaptureControl(FControl);
      end;
    {$ENDIF}
    end;
  end;
end;

{ TKHintWindow }

constructor TKHintWindow.Create(AOwner: TComponent);
begin
  inherited;
{$IFDEF FPC}
  ShowInTaskBar := stNever;
{$ENDIF}
  DoubleBuffered := True;
end;

procedure TKHintWindow.ShowAt(const Origin: TPoint);
begin
  ActivateHint(Rect(Origin.X, Origin.Y, Origin.X + FExtent.X + 10, Origin.Y + FExtent.Y + 10), '');
//  ActivateWithBounds(Rect(Origin.X, Origin.Y, Origin.X + FExtent.X + 10, Origin.Y + FExtent.Y + 10), '');
end;

procedure TKHintWindow.Hide;
begin
{$IFDEF FPC}
  inherited Hide;
{$ELSE}
   Self.DestroyHandle;
{$ENDIF}
end;

procedure TKHintWindow.WMEraseBkGnd(var Msg: TLMessage);
begin
  Msg.Result := 1;
end;

{ TKTextHint }

constructor TKTextHint.Create(AOwner: TComponent);
begin
  inherited;
  FText := '';
{$IFDEF FPC}
  Font := Screen.HintFont;
{$ENDIF}
end;

procedure TKTextHint.Paint;
var
  TextBox: TKTextBox;
begin
  Canvas.Font := Font;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := clInfoBk;
  Canvas.FillRect(ClientRect);
  Canvas.Brush.Style := bsClear;
  TextBox := TKTextBox.Create;
  try
    TextBox.Attributes := [taEndEllipsis, taWordBreak, taLineBreak];
    TextBox.HPadding := 5;
    TextBox.VPadding := 5;
    TextBox.Text := FText;
    TextBox.Draw(Canvas, Rect(0, 0, FExtent.X + 10, FExtent.Y + 10));
  finally
     TextBox.Free;
  end;
end;

procedure TKTextHint.SetText(const Value: TKString);
var
  R: TRect;
  TextBox: TKTextBox;
begin
  if Value <> FText then
  begin
    FText := Value;
    R := Rect(0, 0, 300, 0);
    TextBox := TKTextBox.Create;
    try
      TextBox.Attributes := [taWordBreak, taLineBreak];
      TextBox.Text := FText;
      TextBox.Measure(Canvas, R, FExtent.X, FExtent.Y);
    finally
       TextBox.Free;
    end;
  end;
end;

{ TKGraphicHint }

constructor TKGraphicHint.Create(AOwner: TComponent);
begin
  inherited;
  FGraphic := nil;
{$IFDEF FPC}
  ShowInTaskBar := stNever;
{$ENDIF}
  DoubleBuffered := True;
end;

procedure TKGraphicHint.Paint;
begin
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := clInfoBk;
  Canvas.FillRect(ClientRect);
  if Assigned(FGraphic) then
    Canvas.Draw(5, 5, FGraphic)
end;

procedure TKGraphicHint.SetGraphic(const Value: TGraphic);
begin
  if Value <> FGraphic then
  begin
    FGraphic := Value;
    FExtent.X := FGraphic.Width;
    FExtent.Y := FGraphic.Height;
  end;
end;

{ TKSizingGrips }

constructor TKSizingGrips.Create;
begin
  FBoundsRect := CreateEmptyRect;
  FGripColor := cSizingGripColor;
  FGripSize := cSizingGripSize;
  FMidGripConstraint := cSizingMidGripConstraint;
end;

class procedure TKSizingGrips.ClsAffectRect(APosition: TKSizingGripPosition;
  ADX, ADY: Integer; var ARect: TRect);
begin
  case APosition of
    sgpLeft:
      Inc(ARect.Left, ADX);
    sgpRight:
      Inc(ARect.Right, ADX);
    sgpTop:
      Inc(ARect.Top, ADY);
    sgpBottom:
      Inc(ARect.Bottom, ADY);
    sgpTopLeft:
    begin
      Inc(ARect.Left, ADX);
      Inc(ARect.Top, ADY);
    end;
    sgpTopRight:
    begin
      Inc(ARect.Right, ADX);
      Inc(ARect.Top, ADY);
    end;
    sgpBottomLeft:
    begin
      Inc(ARect.Left, ADX);
      Inc(ARect.Bottom, ADY);
    end;
    sgpBottomRight:
    begin
      Inc(ARect.Right, ADX);
      Inc(ARect.Bottom, ADY);
    end;
  else
    KFunctions.OffsetRect(ARect, ADX, ADY);
  end;
end;

function TKSizingGrips.CursorAt(const APoint: TPoint): TCursor;
begin
  Result := CursorFor(HitTest(APoint));
end;

function TKSizingGrips.CursorFor(APosition: TKSizingGripPosition): TCursor;
begin
  case APosition of
    sgpLeft, sgpRight: Result := crSizeWE;
    sgpTop, sgpBottom: Result := crSizeNS;
    sgpTopLeft, sgpBottomRight: Result := crSizeNWSE;
    sgpTopRight, sgpBottomLeft: Result := crSizeNESW;
  else
    Result := crDefault;
  end;
end;

procedure TKSizingGrips.DrawTo(ACanvas: TCanvas);
begin
  with ACanvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := FGripColor;
    // take the corners first
    ACanvas.FillRect(GripRect(sgpTopLeft));
    ACanvas.FillRect(GripRect(sgpTopRight));
    ACanvas.FillRect(GripRect(sgpBottomLeft));
    ACanvas.FillRect(GripRect(sgpBottomRight));
    // take middle grips
    if FBoundsRect.Right - FBoundsRect.Left >= FMidGripConstraint then
    begin
      ACanvas.FillRect(GripRect(sgpTop));
      ACanvas.FillRect(GripRect(sgpBottom));
    end;
    if FBoundsRect.Bottom - FBoundsRect.Top >= FMidGripConstraint then
    begin
      ACanvas.FillRect(GripRect(sgpLeft));
      ACanvas.FillRect(GripRect(sgpRight));
    end;
  end;
end;

function TKSizingGrips.GripRect(APosition: TKSizingGripPosition): TRect;
begin
  case APosition of
    sgpLeft:
    begin
      Result.Left := FBoundsRect.Left;
      Result.Top := FBoundsRect.Top + (FBoundsRect.Bottom - FBoundsRect.Top - FGripSize) div 2;
    end;
    sgpRight:
    begin
      Result.Left := FBoundsRect.Right - FGripSize;
      Result.Top := FBoundsRect.Top + (FBoundsRect.Bottom - FBoundsRect.Top - FGripSize) div 2;
    end;
    sgpTop:
    begin
      Result.Left := FBoundsRect.Left + (FBoundsRect.Right - FBoundsRect.Left - FGripSize) div 2;
      Result.Top := FBoundsRect.Top;
    end;
    sgpBottom:
    begin
      Result.Left := FBoundsRect.Left + (FBoundsRect.Right - FBoundsRect.Left - FGripSize) div 2;
      Result.Top := FBoundsRect.Bottom - FGripSize;
    end;
    sgpTopLeft:
    begin
      Result.Left := FBoundsRect.Left;
      Result.Top := FBoundsRect.Top;
    end;
    sgpTopRight:
    begin
      Result.Left := FBoundsRect.Right - FGripSize;
      Result.Top :=  FBoundsRect.Top;
    end;
    sgpBottomLeft:
    begin
      Result.Left := FBoundsRect.Left;
      Result.Top := FBoundsRect.Bottom - FGripSize;
    end;
    sgpBottomRight:
    begin
      Result.Left := FBoundsRect.Right - FGripSize;
      Result.Top := FBoundsRect.Bottom - FGripSize;
    end
  else
    Result := CreateEmptyRect;
  end;
  if APosition <> sgpNone then
  begin
    Result.Right := Result.Left + FGripSize;
    Result.Bottom := Result.Top + FGripSize;
  end;
end;

function TKSizingGrips.HitTest(const APoint: TPoint): TKSizingGripPosition;
var
  I: TKSizingGripPosition;
  R: TRect;
begin
  Result := sgpNone;
  for I := Low(TKSizingGripPosition) to High(TKSizingGripPosition) do if I <> sgpNone then
  begin
    R := GripRect(I);
    if PtInRect(R, APoint) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

procedure RegisterAlphaBitmap;
begin
  TPicture.RegisterFileFormat('BMA', sGrAlphaBitmap, TKAlphaBitmap);
end;

procedure UnregisterAlphaBitmap;
begin
  TPicture.UnregisterGraphicClass(TKAlphaBitmap);
end;

{$IFDEF REGISTER_PICTURE_FORMATS}
initialization
  RegisterAlphaBitmap;
finalization
  //not necessary, but...
  UnregisterAlphaBitmap;
{$ENDIF}
end.
