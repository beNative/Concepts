unit MainRunner;

interface

uses
  Mapping.CodeGenerator.DB
  ,SvSerializer
  ,SvSerializerSuperJson
  ;

type
  TMainRunner = class
  private
    FFilename: string;
    FDBLoader: TEntityModelDataLoader;
  public
    constructor Create(); virtual;
    destructor Destroy; override;

    function Execute(AIndex: Integer): string;

    property DBLoader: TEntityModelDataLoader read FDBLoader;
  end;

implementation

uses
  SysUtils
  ,Mapping.CodeGenerator
  ;

{ TMainRunner }

constructor TMainRunner.Create;
begin
  inherited Create;
  FFilename := IncludeTrailingPathDelimiter(GetHomePath) + IncludeTrailingPathDelimiter('Marshmallow') + 'runner.json';
  ForceDirectories(ExtractFileDir(FFilename));
  FDBLoader := TEntityModelDataLoader.Create;
  if FileExists(FFilename) then
    TSvSerializer.DeSerializeObjectFromFilename(FDBLoader, FFilename);
end;

destructor TMainRunner.Destroy;
begin
  TSvSerializer.SerializeObjectToFilename(FDBLoader, FFilename);
  FDBLoader.Free;
  inherited Destroy;
end;

function TMainRunner.Execute(AIndex: Integer): string;
var
  LGenerator: TDelphiUnitCodeGenerator;
begin
  LGenerator := TDelphiUnitCodeGenerator.Create;
  try
    LGenerator.UnitPrefix := DBLoader.UnitPrefix;
    LGenerator.UseNullableTypes := DBLoader.UseNullableTypes;
    Result := LGenerator.Generate(FDBLoader.Entities[AIndex]);
  finally
    LGenerator.Free;
  end;
end;

end.
