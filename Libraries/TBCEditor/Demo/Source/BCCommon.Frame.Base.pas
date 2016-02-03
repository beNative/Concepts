unit BCCommon.Frame.Base;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, sFrameAdapter;

type
  TBCBaseFrame = class(TFrame)
    FrameAdapter: TsFrameAdapter;
  end;

implementation

{$R *.dfm}

end.
