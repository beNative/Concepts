unit Spring.Data.ActiveX;

interface

const
  {$EXTERNALSYM VT_EMPTY}
  VT_EMPTY           = 0;   { [V]   [P]  nothing                     }
  {$EXTERNALSYM VT_NULL}
  VT_NULL            = 1;   { [V]        SQL style Null              }
  {$EXTERNALSYM VT_I2}
  VT_I2              = 2;   { [V][T][P]  2 byte signed int           }
  {$EXTERNALSYM VT_I4}
  VT_I4              = 3;   { [V][T][P]  4 byte signed int           }
  {$EXTERNALSYM VT_R4}
  VT_R4              = 4;   { [V][T][P]  4 byte real                 }
  {$EXTERNALSYM VT_R8}
  VT_R8              = 5;   { [V][T][P]  8 byte real                 }
  {$EXTERNALSYM VT_CY}
  VT_CY              = 6;   { [V][T][P]  currency                    }
  {$EXTERNALSYM VT_DATE}
  VT_DATE            = 7;   { [V][T][P]  date                        }
  {$EXTERNALSYM VT_BSTR}
  VT_BSTR            = 8;   { [V][T][P]  binary string               }
  {$EXTERNALSYM VT_DISPATCH}
  VT_DISPATCH        = 9;   { [V][T]     IDispatch FAR*              }
  {$EXTERNALSYM VT_ERROR}
  VT_ERROR           = 10;  { [V][T]     SCODE                       }
  {$EXTERNALSYM VT_BOOL}
  VT_BOOL            = 11;  { [V][T][P]  True=-1, False=0            }
  {$EXTERNALSYM VT_VARIANT}
  VT_VARIANT         = 12;  { [V][T][P]  VARIANT FAR*                }
  {$EXTERNALSYM VT_UNKNOWN}
  VT_UNKNOWN         = 13;  { [V][T]     IUnknown FAR*               }
  {$EXTERNALSYM VT_DECIMAL}
  VT_DECIMAL         = 14;  { [V][T]   [S]  16 byte fixed point      }

  {$EXTERNALSYM VT_I1}
  VT_I1              = 16;  {    [T]     signed char                 }
  {$EXTERNALSYM VT_UI1}
  VT_UI1             = 17;  {    [T]     unsigned char               }
  {$EXTERNALSYM VT_UI2}
  VT_UI2             = 18;  {    [T]     unsigned short              }
  {$EXTERNALSYM VT_UI4}
  VT_UI4             = 19;  {    [T]     unsigned long               }
  {$EXTERNALSYM VT_I8}
  VT_I8              = 20;  {    [T][P]  signed 64-bit int           }
  {$EXTERNALSYM VT_UI8}
  VT_UI8             = 21;  {    [T]     unsigned 64-bit int         }
  {$EXTERNALSYM VT_INT}
  VT_INT             = 22;  {    [T]     signed machine int          }
  {$EXTERNALSYM VT_UINT}
  VT_UINT            = 23;  {    [T]     unsigned machine int        }
  {$EXTERNALSYM VT_VOID}
  VT_VOID            = 24;  {    [T]     C style void                }
  {$EXTERNALSYM VT_HRESULT}
  VT_HRESULT         = 25;  {    [T]                                 }
  {$EXTERNALSYM VT_PTR}
  VT_PTR             = 26;  {    [T]     pointer type                }
  {$EXTERNALSYM VT_SAFEARRAY}
  VT_SAFEARRAY       = 27;  {    [T]     (use VT_ARRAY in VARIANT)   }
  {$EXTERNALSYM VT_CARRAY}
  VT_CARRAY          = 28;  {    [T]     C style array               }
  {$EXTERNALSYM VT_USERDEFINED}
  VT_USERDEFINED     = 29;  {    [T]     user defined type          }
  {$EXTERNALSYM VT_LPSTR}
  VT_LPSTR           = 30;  {    [T][P]  null terminated string      }
  {$EXTERNALSYM VT_LPWSTR}
  VT_LPWSTR          = 31;  {    [T][P]  wide null terminated string }
  {$EXTERNALSYM VT_RECORD}
  VT_RECORD          = 36;  { [V]   [P][S]  user defined type        }
  {$EXTERNALSYM VT_INT_PTR}
  VT_INT_PTR         = 37;  {    [T]     signed machine register size width }
  {$EXTERNALSYM VT_UINT_PTR}
  VT_UINT_PTR        = 38;  {    [T]     unsigned machine register size width }

  {$EXTERNALSYM VT_FILETIME}
  VT_FILETIME        = 64;  {       [P]  FILETIME                    }
  {$EXTERNALSYM VT_BLOB}
  VT_BLOB            = 65;  {       [P]  Length prefixed bytes       }
  {$EXTERNALSYM VT_STREAM}
  VT_STREAM          = 66;  {       [P]  Name of the stream follows  }
  {$EXTERNALSYM VT_STORAGE}
  VT_STORAGE         = 67;  {       [P]  Name of the storage follows }
  {$EXTERNALSYM VT_STREAMED_OBJECT}
  VT_STREAMED_OBJECT = 68;  {       [P]  Stream contains an object   }
  {$EXTERNALSYM VT_STORED_OBJECT}
  VT_STORED_OBJECT   = 69;  {       [P]  Storage contains an object  }
  {$EXTERNALSYM VT_BLOB_OBJECT}
  VT_BLOB_OBJECT     = 70;  {       [P]  Blob contains an object     }
  {$EXTERNALSYM VT_CF}
  VT_CF              = 71;  {       [P]  Clipboard format            }
  {$EXTERNALSYM VT_CLSID}
  VT_CLSID           = 72;  {       [P]  A Class ID                  }

  {$EXTERNALSYM VT_VECTOR}
  VT_VECTOR        = $1000; {       [P]  simple counted array        }
  {$EXTERNALSYM VT_ARRAY}
  VT_ARRAY         = $2000; { [V]        SAFEARRAY*                  }
  {$EXTERNALSYM VT_BYREF}
  VT_BYREF         = $4000; { [V]                                    }
  {$EXTERNALSYM VT_RESERVED}
  VT_RESERVED      = $8000;
  {$EXTERNALSYM VT_ILLEGAL}
  VT_ILLEGAL       = $ffff;
  {$EXTERNALSYM VT_ILLEGALMASKED}
  VT_ILLEGALMASKED = $0fff;
  {$EXTERNALSYM VT_TYPEMASK}
  VT_TYPEMASK      = $0fff;

type
  {$EXTERNALSYM TOleDate}
  TOleDate = Double;
  POleDate = ^TOleDate;

  {$EXTERNALSYM TOleBool}
  TOleBool = WordBool;
  POleBool = ^TOleBool;

  PSafeArrayBound = ^TSafeArrayBound;
  {$EXTERNALSYM tagSAFEARRAYBOUND}
  tagSAFEARRAYBOUND = record
    cElements: Longint;
    lLbound: Longint;
  end;
  TSafeArrayBound = tagSAFEARRAYBOUND;
  {$EXTERNALSYM SAFEARRAYBOUND}
  SAFEARRAYBOUND = TSafeArrayBound;

  LONGLONG = Int64;

  PSafeArray = ^TSafeArray;
  {$EXTERNALSYM tagSAFEARRAY}
  tagSAFEARRAY = record
    cDims: Word;
    fFeatures: Word;
    cbElements: Longint;
    cLocks: Longint;
    pvData: Pointer;
    rgsabound: array[0..0] of TSafeArrayBound;
  end;
  TSafeArray = tagSAFEARRAY;
  {$EXTERNALSYM SAFEARRAY}
  SAFEARRAY = TSafeArray;

  PDecimal = ^TDecimal;
  {$EXTERNALSYM tagDEC}
  tagDEC = packed record
    wReserved: Word;
    case Integer of
      0: (scale, sign: Byte; Hi32: Longint;
      case Integer of
        0: (Lo32, Mid32: Longint);
        1: (Lo64: LONGLONG));
      1: (signscale: Word);
  end;
  TDecimal = tagDEC;
  {$EXTERNALSYM DECIMAL}
  DECIMAL = TDecimal;




  PVariantArg = ^TVariantArg;
  {$EXTERNALSYM tagVARIANT}
  tagVARIANT = record
    vt: TVarType;
    wReserved1: Word;
    wReserved2: Word;
    wReserved3: Word;
    case Integer of
      VT_UI1:                  (bVal: Byte);
      VT_I2:                   (iVal: Smallint);
      VT_I4:                   (lVal: Longint);
      VT_R4:                   (fltVal: Single);
      VT_R8:                   (dblVal: Double);
      VT_BOOL:                 (vbool: TOleBool);
      VT_ERROR:                (scode: HResult);
      VT_CY:                   (cyVal: Currency);
      VT_DATE:                 (date: TOleDate);
      VT_BSTR:                 (bstrVal: PWideChar{WideString});
      VT_UNKNOWN:              (unkVal: Pointer{IUnknown});
      VT_DISPATCH:             (dispVal: Pointer{IDispatch});
      VT_ARRAY:                (parray: PSafeArray);
      VT_BYREF or VT_UI1:      (pbVal: ^Byte);
      VT_BYREF or VT_I2:       (piVal: ^Smallint);
      VT_BYREF or VT_I4:       (plVal: ^Longint);
      VT_BYREF or VT_R4:       (pfltVal: ^Single);
      VT_BYREF or VT_R8:       (pdblVal: ^Double);
      VT_BYREF or VT_BOOL:     (pbool: ^TOleBool);
      VT_BYREF or VT_ERROR:    (pscode: ^HResult);
      VT_BYREF or VT_CY:       (pcyVal: ^Currency);
      VT_BYREF or VT_DATE:     (pdate: ^TOleDate);
{$IFNDEF NEXTGEN}
      VT_BYREF or VT_BSTR:     (pbstrVal: ^WideString);
{$ENDIF}
      VT_BYREF or VT_UNKNOWN:  (punkVal: ^IUnknown);
      VT_BYREF or VT_DISPATCH: (pdispVal: ^IDispatch);
      VT_BYREF or VT_ARRAY:    (pparray: ^PSafeArray);
      VT_BYREF or VT_VARIANT:  (pvarVal: PVariant);
      VT_BYREF:                (byRef: Pointer);
      VT_I1:                   (cVal: {$IFNDEF NEXTGEN}AnsiChar{$ELSE}Byte{$ENDIF});
      VT_UI2:                  (uiVal: Word);
      VT_UI4:                  (ulVal: LongWord);
      VT_I8:                   (llVal: Int64);
      VT_UI8:                  (ullVal: UInt64);
      VT_INT:                  (intVal: Integer);
      VT_UINT:                 (uintVal: LongWord);
      VT_BYREF or VT_DECIMAL:  (pdecVal: PDecimal);
{$IFNDEF NEXTGEN}
      VT_BYREF or VT_I1:       (pcVal: PAnsiChar);
{$ENDIF}
      VT_BYREF or VT_UI2:      (puiVal: PWord);
      VT_BYREF or VT_UI4:      (pulVal: PInteger);
      VT_BYREF or VT_INT:      (pintVal: PInteger);
      VT_BYREF or VT_UINT:     (puintVal: PLongWord);
      VT_BYREF or VT_I8:       (pllVal: ^Int64);
      VT_BYREF or VT_UI8:      (pullVal: ^UInt64);
      VT_RECORD:               (pvRecord: Pointer;
                                pRecInfo: Pointer);
  end;

  TVariantArg = tagVARIANT;
  PVariantArgList = ^TVariantArgList;
  TVariantArgList = array[0..65535] of TVariantArg;

implementation

end.
