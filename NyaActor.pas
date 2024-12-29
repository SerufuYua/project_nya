unit NyaActor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CastleTransform, CastleClassUtils;

type
  TNyaActor = class(TCastleTransform)
  protected
    FUrl: String;
    procedure SetUrl(const Value: String);
  public
    constructor Create(AOwner: TComponent); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    property Url: String read FUrl write SetUrl;
  end;

implementation

uses
  CastleComponentSerialize, CastleUtils
  {$ifdef CASTLE_DESIGN_MODE}
  , PropEdits, CastlePropEdits
  {$endif};

constructor TNyaActor.Create(AOwner: TComponent);
begin
  inherited;

  FUrl:= '';
end;

procedure TNyaActor.SetUrl(const value: String);
begin
  if (FUrl = value) then Exit;
  FUrl:= value;

end;

function TNyaActor.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'Url'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TNyaActor, 'Nya Actor');

  {$ifdef CASTLE_DESIGN_MODE}
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaActor, 'URL',
                         TTransformDesignURLPropertyEditor);
  {$endif}
end.

