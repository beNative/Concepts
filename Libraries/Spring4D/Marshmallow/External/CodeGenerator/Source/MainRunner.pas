unit MainRunner;

interface

uses
  Spring.Persistence.Mapping.CodeGenerator.DB;

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
  SysUtils,
  JsonDataObjects,
  Spring.Persistence.Mapping.CodeGenerator;

{ TMainRunner }

constructor TMainRunner.Create;
var
  json: TJsonObject;
begin
  inherited Create;
  FFilename := IncludeTrailingPathDelimiter(GetHomePath) + IncludeTrailingPathDelimiter('Marshmallow') + 'runner.json';
  ForceDirectories(ExtractFileDir(FFilename));
  FDBLoader := TEntityModelDataLoader.Create;

  if FileExists(FFilename) then
  begin
    json := TJsonObject.ParseFromFile(FFilename) as TJsonObject;
    try
      FDBLoader.DatabaseName := json.S['DatabaseName'];
      FDBLoader.DefaultSchemaName := json.S['DefaultSchemaName'];
      FDBLoader.ConnectionString := json.S['ConnectionString'];
      FDBLoader.OutputDir := json.S['OutputDir'];
      FDBLoader.UnitPrefix := json.S['UnitPrefix'];
      FDBLoader.UseNullableTypes := json.B['UseNullableTypes'];
    finally
      json.Free;
    end;
  end;
end;

destructor TMainRunner.Destroy;
var
  json: TJsonObject;
begin
  json := TJsonObject.Create;
  try
    json.S['DatabaseName'] := FDBLoader.DatabaseName;
    json.S['DefaultSchemaName'] := FDBLoader.DefaultSchemaName;
    json.S['ConnectionString'] := FDBLoader.ConnectionString;
    json.S['OutputDir'] := FDBLoader.OutputDir;
    json.S['UnitPrefix'] := FDBLoader.UnitPrefix;
    json.B['UseNullableTypes'] := FDBLoader.UseNullableTypes;
    json.SaveToFile(FFilename);
  finally
    json.Free;
  end;
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
