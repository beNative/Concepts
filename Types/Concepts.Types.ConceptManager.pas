unit Concepts.Types.ConceptManager;

interface

uses
  System.Classes, System.Generics.Collections,
  Vcl.Forms,

  VirtualTreeView,

  DSharp.Collections;

type
  TConcept = class(TPersistent)
  strict private
    FName      : string;
    FFormClass : TComponentClass;

    function GetSourceFilename: string;

  published
    property Name: string
      read FName write FName;

    property Category: string
      read FCategory write FCategory;

    property FormClass: TComponentClass
      read FFormClass write FFormClass;

    property SourceFilename: string
      read GetSourceFilename;
  end;

type
  TConceptManager = record
    class var
      FList: IList;

    class function Register(
            AFormClass : TComponentClass;
      const AName      : string = '';
      const ACategory  : string = ''
    ): Boolean; static;

    class procedure Execute(AConcept: TObject); static;

    class constructor Create;
    class destructor Destroy;

    class property ItemList: IList
      read FList;

    class function Register(
      AFormClass      : TComponentClass;
      const AName     : string = '';
      const ACategory : string = ''
    ): Boolean; static;

    class procedure Execute(const AName: string); static;

  end;

  ConceptManager = TConceptManager;

implementation

uses
  System.SysUtils, System.StrUtils;

{$REGION 'construction and destruction'}
class constructor TConceptManager.Create;
begin
  FList := TFormList.Create;
end;

class destructor TConceptManager.Destroy;
begin
  FreeAndNil(FList);
end;
class procedure TConceptManager.Execute(AConcept: TObject);
begin

end;

{$ENDREGION}

{$REGION 'public methods'}
class procedure TConceptManager.Execute(const AName: string);
var
  F: TForm;
begin
  F := FList.Items[AName].Create(Application);
  F.Caption := AName;
  F.Position := poScreenCenter;
  F.ShowModal;
end;

class function TConceptManager.Register(AFormClass: TComponentClass;
  const AName, ACategory: string): Boolean;
begin

end;

class function TConceptManager.Register(AFormClass: TFormClass; const AName : string): Boolean;
var
  S: string;
begin
  Result := False;
  S := IfThen(AName = '', AFormClass.ClassName, AName);
  if not FList.ContainsKey(S) then
  begin
    FList.Add(S, AFormClass);
    Result := True;
  end;
end;
{$ENDREGION}

end.

