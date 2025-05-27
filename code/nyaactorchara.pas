unit NyaActorChara;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, NyaActor, CastleClassUtils, CastleSceneCore,
  NyaCharaDress, CastleTransform, NyaCastleUtils, CastleScene,
  CastleParticleEmitter,
  NyaSoundControl;

type
  TNyaActorChara = class(TNyaActor)
  protected
    { controllers }
    FEmitterDrip: TCastleParticleEmitter;
    FEffectDrip: TCastleParticleEffect;
    FEmitterSweat: TCastleParticleEmitter;
    FEffectSweat: TCastleParticleEffect;
    FControlJizz: TCastleTransform;
    FEmitterJizz: TCastleParticleEmitter;
  protected
    FDresser: TCharaDresser;
    FDripping: Single;
    FSweating: Single;
    procedure SetActorName(const value: String); override;
    procedure SetDripping(value: Single);
    procedure SetSweating(value: Single);
    procedure ApplyDripping;
    procedure ApplySweating;
    procedure UpdateJizz;
    procedure SetUrl(const value: String); override;
  public
    const
      DefaultDripping = 0.0;
      DefaultSweating = 0.0;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    function Dresser: TCharaDresser;
    procedure RestoreCondition;
    procedure SaveCondition;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    property Dripping: Single read FDripping write SetDripping
             {$ifdef FPC}default DefaultDripping{$endif};
    property Sweating: Single read FSweating write SetSweating
             {$ifdef FPC}default DefaultSweating{$endif};
  end;

implementation

uses
  CastleComponentSerialize, CastleUtils;

constructor TNyaActorChara.Create(AOwner: TComponent);
begin
  inherited;

  FDripping:= DefaultDripping;
  FSweating:= DefaultSweating;
  FDresser:= nil;
end;

destructor TNyaActorChara.Destroy;
begin
  if Assigned(FDresser) then
    FreeAndNil(FDresser);
  inherited;
end;

procedure TNyaActorChara.Update(const SecondsPassed: Single;
                                var RemoveMe: TRemoveType);
begin
  inherited;

  UpdateJizz;
end;

procedure TNyaActorChara.SetUrl(const value: String);
begin
  inherited;

  if Assigned(FDresser) then
    FreeAndNil(FDresser);

  ApplyDripping;
  ApplySweating;

  FEmitterDrip:= FDesign.DesignedComponent('EmitterDrip', False)
                 as TCastleParticleEmitter;
  FEffectDrip:= FDesign.DesignedComponent('EffectDrip', False)
                as TCastleParticleEffect;
  FEmitterSweat:= FDesign.DesignedComponent('EmitterSweat', False)
                  as TCastleParticleEmitter;
  FEffectSweat:= FDesign.DesignedComponent('EffectSweat', False)
                 as TCastleParticleEffect;
  FControlJizz:= FDesign.DesignedComponent('Control_Jizz', False)
                 as TCastleTransform;
  FEmitterJizz:= FDesign.DesignedComponent('EmitterJizz', False)
                 as TCastleParticleEmitter;

  RestoreCondition;
end;

function TNyaActorChara.Dresser: TCharaDresser;
begin
  if NOT Assigned(FDesign) then Exit(nil);

  if NOT Assigned(FDresser) then
    FDresser:= TCharaDresser.Create(FDesign);

  Result:= FDresser;
end;

procedure TNyaActorChara.RestoreCondition;
begin
  if Assigned(Dresser) then
    Dresser.RestoreCondition(ActorName);
end;

procedure TNyaActorChara.SaveCondition;
begin
  if Assigned(Dresser) then
    Dresser.SaveCondition(ActorName);
end;

procedure TNyaActorChara.SetActorName(const value: String);
begin
  if (FActorName = value) then exit;
  FActorName:= value;
  RestoreCondition;
end;

procedure TNyaActorChara.SetDripping(value: Single);
begin
  if (FDripping = value) then Exit;
  FDripping:= value;
  ApplyDripping;
end;

procedure TNyaActorChara.SetSweating(value: Single);
begin
  if (FSweating = value) then Exit;
  FSweating:= value;
  ApplySweating;
end;

procedure TNyaActorChara.ApplyDripping;
begin
  if (Assigned(FEffectDrip) AND Assigned(FEmitterDrip)) then
  begin
    FEmitterDrip.Exists:= (FDripping > 0.05);
    FEffectDrip.MaxParticles:= 1 + round(20.0 * FDripping);
  end;
end;

procedure TNyaActorChara.ApplySweating;
begin
  if (Assigned(FEffectSweat) AND Assigned(FEmitterSweat)) then
  begin
    FEmitterSweat.Exists:= (FSweating > 0.05);
    FEffectSweat.MaxParticles:= 1 + round(5.0 * FSweating);
  end;
end;

procedure TNyaActorChara.UpdateJizz;
begin
  if (Assigned(FControlJizz) AND Assigned(FEmitterJizz)) then
    FEmitterJizz.Exists:= (FControlJizz.Translation.Y > 0.5);
end;

function TNyaActorChara.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'Dripping', 'Sweating'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TNyaActorChara, 'Nya Actor Chara');
end.

