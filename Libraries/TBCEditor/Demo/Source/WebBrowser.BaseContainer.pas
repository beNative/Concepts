{
 * This source code accompanies the article "How to customise the TWebBrowser
 * user interface" which can be found at
 * http://www.delphidabbler.com/articles?article=18.
 *
 * The code is merely a proof of concept and is intended only to illustrate the
 * article. It is not designed for use in its current form in finished
 * applications. The code is provided on an "AS IS" basis, WITHOUT WARRANTY OF
 * ANY KIND, either express or implied.
 *
 * $Rev: 60 $
 * $Date: 2010-05-30 03:15:37 +0100 (Sun, 30 May 2010) $
}


{$A8,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$WARN UNSAFE_TYPE OFF}


unit WebBrowser.BaseContainer;

interface


uses
  Windows, ActiveX, SHDocVw, WebBrowser.IntfDocHostUIHandler;

type
  TBaseWBContainer = class(TObject, IUnknown, IOleClientSite, IDocHostUIHandler)
  private
    FHostedBrowser: TWebBrowser;
    // Registration method
    procedure SetBrowserOleClientSite(const Site: IOleClientSite);
  protected
    { IUnknown }
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    { IOleClientSite }
    function SaveObject: HResult; stdcall;
    function GetMoniker(dwAssign: Longint;
      dwWhichMoniker: Longint;
      out mk: IMoniker): HResult; stdcall;
    function GetContainer(
      out container: IOleContainer): HResult; stdcall;
    function ShowObject: HResult; stdcall;
    function OnShowWindow(fShow: BOOL): HResult; stdcall;
    function RequestNewObjectLayout: HResult; stdcall;
    { IDocHostUIHandler }
    function ShowContextMenu(const dwID: DWORD; const ppt: PPOINT;
      const pcmdtReserved: IUnknown; const pdispReserved: IDispatch): HResult;
      stdcall;
    function GetHostInfo(var pInfo: TDocHostUIInfo): HResult; stdcall;
    function ShowUI(const dwID: DWORD;
      const pActiveObject: IOleInPlaceActiveObject;
      const pCommandTarget: IOleCommandTarget; const pFrame: IOleInPlaceFrame;
      const pDoc: IOleInPlaceUIWindow): HResult; stdcall;
    function HideUI: HResult; stdcall;
    function UpdateUI: HResult; stdcall;
    function EnableModeless(const fEnable: BOOL): HResult; stdcall;
    function OnDocWindowActivate(const fActivate: BOOL): HResult; stdcall;
    function OnFrameWindowActivate(const fActivate: BOOL): HResult; stdcall;
    function ResizeBorder(const prcBorder: PRECT;
      const pUIWindow: IOleInPlaceUIWindow; const fFrameWindow: BOOL): HResult;
      stdcall;
    function TranslateAccelerator(const lpMsg: PMSG; const pguidCmdGroup: PGUID;
      const nCmdID: DWORD): HResult; stdcall;
    function GetOptionKeyPath(var pchKey: POLESTR; const dw: DWORD ): HResult;
      stdcall;
    function GetDropTarget(const pDropTarget: IDropTarget;
      out ppDropTarget: IDropTarget): HResult; stdcall;
    function GetExternal(out ppDispatch: IDispatch): HResult; stdcall;
    function TranslateUrl(const dwTranslate: DWORD; const pchURLIn: POLESTR;
      var ppchURLOut: POLESTR): HResult; stdcall;
    function FilterDataObject(const pDO: IDataObject;
      out ppDORet: IDataObject): HResult; stdcall;
  public
    constructor Create(const HostedBrowser: TWebBrowser);
    destructor Destroy; override;
    property HostedBrowser: TWebBrowser read FHostedBrowser;
  end;


implementation

uses
  SysUtils;

{ TBaseWBContainer }

constructor TBaseWBContainer.Create(const HostedBrowser: TWebBrowser);
begin
  Assert(Assigned(HostedBrowser));
  inherited Create;
  FHostedBrowser := HostedBrowser;
  SetBrowserOleClientSite(Self as IOleClientSite);
end;

destructor TBaseWBContainer.Destroy;
begin
  SetBrowserOleClientSite(nil);
  inherited;
end;

function TBaseWBContainer.EnableModeless(const fEnable: BOOL): HResult;
begin
  { Return S_OK to indicate we handled (ignored) OK }
  Result := S_OK;
end;

function TBaseWBContainer.FilterDataObject(const pDO: IDataObject;
  out ppDORet: IDataObject): HResult;
begin
  { Return S_FALSE to show no data object supplied.
    We *must* also set ppDORet to nil }
  ppDORet := nil;
  Result := S_FALSE;
end;

function TBaseWBContainer.GetContainer(
  out container: IOleContainer): HResult;
  {Returns a pointer to the container's IOleContainer
  interface}
begin
  { We do not support IOleContainer.
    However we *must* set container to nil }
  container := nil;
  Result := E_NOINTERFACE;
end;

function TBaseWBContainer.GetDropTarget(const pDropTarget: IDropTarget;
  out ppDropTarget: IDropTarget): HResult;
begin
  { Return E_FAIL since no alternative drop target supplied.
    We *must* also set ppDropTarget to nil }
  ppDropTarget := nil;
  Result := E_FAIL;
end;

function TBaseWBContainer.GetExternal(out ppDispatch: IDispatch): HResult;
begin
  { Return E_FAIL to indicate we failed to supply external object.
    We *must* also set ppDispatch to nil }
  ppDispatch := nil;
  Result := E_FAIL;
end;

function TBaseWBContainer.GetHostInfo(var pInfo: TDocHostUIInfo): HResult;
begin
  { Return S_OK to indicate UI is OK without changes }
  Result := S_OK;
end;

function TBaseWBContainer.GetMoniker(dwAssign, dwWhichMoniker: Integer;
  out mk: IMoniker): HResult;
  {Returns a moniker to an object's client site}
begin
  { We don't support monikers.
    However we *must* set mk to nil }
  mk := nil;
  Result := E_NOTIMPL;
end;

function TBaseWBContainer.GetOptionKeyPath(var pchKey: POLESTR;
  const dw: DWORD): HResult;
begin
  { Return E_FAIL to indicate we failed to override
    default registry settings }
  Result := E_FAIL;
end;

function TBaseWBContainer.HideUI: HResult;
begin
  { Return S_OK to indicate we handled (ignored) OK }
  Result := S_OK;
end;

function TBaseWBContainer.OnDocWindowActivate(
  const fActivate: BOOL): HResult;
begin
  { Return S_OK to indicate we handled (ignored) OK }
  Result := S_OK;
end;

function TBaseWBContainer.OnFrameWindowActivate(
  const fActivate: BOOL): HResult;
begin
  { Return S_OK to indicate we handled (ignored) OK }
  Result := S_OK;
end;

function TBaseWBContainer.OnShowWindow(fShow: BOOL): HResult;
  {Notifies a container when an embedded object's window
  is about to become visible or invisible}
begin
  { Return S_OK to pretend we've responded to this }
  Result := S_OK;
end;

function TBaseWBContainer.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TBaseWBContainer.RequestNewObjectLayout: HResult;
  {Asks container to allocate more or less space for
  displaying an embedded object}
begin
  { We don't support requests for a new layout }
  Result := E_NOTIMPL;
end;

function TBaseWBContainer.ResizeBorder(const prcBorder: PRECT;
  const pUIWindow: IOleInPlaceUIWindow; const fFrameWindow: BOOL): HResult;
begin
  { Return S_FALSE to indicate we did nothing in response }
  Result := S_FALSE;
end;

function TBaseWBContainer.SaveObject: HResult;
  {Saves the object associated with the client site}
begin
  { Return S_OK to pretend we've done this }
  Result := S_OK;
end;

procedure TBaseWBContainer.SetBrowserOleClientSite(
  const Site: IOleClientSite);
var
  OleObj: IOleObject;
begin
  Assert((Site = Self as IOleClientSite) or (Site = nil));
  if not Supports(
    FHostedBrowser.DefaultInterface, IOleObject, OleObj
  ) then
    raise Exception.Create(
      'Browser''s Default interface does not support IOleObject'
    );
  OleObj.SetClientSite(Site);
end;

function TBaseWBContainer.ShowContextMenu(const dwID: DWORD;
  const ppt: PPOINT; const pcmdtReserved: IInterface;
  const pdispReserved: IDispatch): HResult;
begin
  { Return S_FALSE to notify we didn't display a menu and to
  let browser display its own menu }
  Result := S_FALSE
end;

function TBaseWBContainer.ShowObject: HResult;
  {Tells the container to position the object so it is
  visible to the user}
begin
  { Return S_OK to pretend we've done this }
  Result := S_OK;
end;

function TBaseWBContainer.ShowUI(const dwID: DWORD;
  const pActiveObject: IOleInPlaceActiveObject;
  const pCommandTarget: IOleCommandTarget; const pFrame: IOleInPlaceFrame;
  const pDoc: IOleInPlaceUIWindow): HResult;
begin
  { Return S_OK to say we displayed own UI }
  Result := S_OK;
end;

function TBaseWBContainer.TranslateAccelerator(const lpMsg: PMSG;
  const pguidCmdGroup: PGUID; const nCmdID: DWORD): HResult;
begin
  { Return S_FALSE to indicate no accelerators are translated }
  Result := S_FALSE;
end;

function TBaseWBContainer.TranslateUrl(const dwTranslate: DWORD;
  const pchURLIn: POLESTR; var ppchURLOut: POLESTR): HResult;
begin
  { Return E_FAIL to indicate that no translations took place }
  Result := E_FAIL;
end;

function TBaseWBContainer.UpdateUI: HResult;
begin
  { Return S_OK to indicate we handled (ignored) OK }
  Result := S_OK;
end;

function TBaseWBContainer._AddRef: Integer;
begin
  Result := -1;
end;

function TBaseWBContainer._Release: Integer;
begin
  Result := -1;
end;

end.
