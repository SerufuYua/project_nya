unit NyaActor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CastleTransform, CastleClassUtils;

type
  TNyaActor = class(TCastleTransform)
  public
    function PropertySections(const PropertyName: String): TPropertySections; override;

  end;

implementation

uses
  CastleComponentSerialize, CastleUtils;

function TNyaActor.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'none'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TNyaActor, 'Nya Actor');
end.

