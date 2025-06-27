unit NyaActorVehicle;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, NyaActor, CastleClassUtils, CastleSceneCore,
  NyaCharaDress, CastleTransform, NyaCastleUtils, CastleScene,
  CastleParticleEmitter,
  NyaSoundControl;

type
  TNyaActorVehicle = class(TNyaActor)
  protected
    procedure SetUrl(const value: String); override;
  public
    const
      DefaultDripping = 0.0;
      DefaultSweating = 0.0;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  end;

implementation

uses
  CastleComponentSerialize, CastleUtils;

constructor TNyaActorVehicle.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TNyaActorVehicle.Destroy;
begin
  inherited;
end;

procedure TNyaActorVehicle.Update(const SecondsPassed: Single;
                                  var RemoveMe: TRemoveType);
begin
  inherited;
end;

procedure TNyaActorVehicle.SetUrl(const value: String);
begin
  inherited;
end;

function TNyaActorVehicle.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'any'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TNyaActorVehicle, 'Nya Actor Vehicle');
end.

