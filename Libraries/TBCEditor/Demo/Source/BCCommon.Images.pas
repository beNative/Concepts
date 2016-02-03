unit BCCommon.Images;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, acAlphaImageList, System.ImageList, Vcl.ImgList;

const
  { small images }
  IMAGE_INDEX_FIND_IN_FILES = 36;
  IMAGE_INDEX_RECORD = 88;
  IMAGE_INDEX_PAUSE = 89;

type
  TImagesDataModule = class(TDataModule)
    ImageList: TsAlphaImageList;
    ImageListSmall: TsAlphaImageList;
  end;

var
  ImagesDataModule: TImagesDataModule;

implementation

uses
  Vcl.Forms;

{$R *.dfm}

initialization

  Application.CreateForm(TImagesDataModule, ImagesDataModule);

end.

