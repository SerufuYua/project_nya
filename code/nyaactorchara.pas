unit NyaActorChara;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, NyaActor, CastleClassUtils, CastleSceneCore,
  CharaDress, CastleTransform, NyaCastleUtils, CastleScene, CastleBehaviors;

type
  TNyaActorChara = class(TNyaActor)
  protected
    FDresser: TCharaDresser;
    FDripping: Single;
    FSweating: Single;
    FSoundStep: TCastleSoundSource;
    procedure SetActorName(const value: String); override;
    procedure SetDripping(value: Single);
    procedure SetSweating(value: Single);
    procedure ApplyDripping;
    procedure ApplySweating;
    procedure UpdateJizz;
    procedure UpdateSound;
    procedure SetSoundStep(value: TCastleSoundSource);
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
    property SoundStep: TCastleSoundSource read FSoundStep write SetSoundStep;
  end;

implementation

uses
  CastleComponentSerialize, CastleUtils, CastleParticleEmitter;

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

  UpdateSound;
  UpdateJizz;
end;

procedure TNyaActorChara.SetUrl(const value: String);
begin
  inherited;

  if Assigned(FDresser) then
    FreeAndNil(FDresser);

  ApplyDripping;
  ApplySweating;

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

procedure TNyaActorChara.SetSoundStep(value: TCastleSoundSource);
begin
  if (FSoundStep = value) then Exit;

  FSoundStep:= value;
end;

procedure TNyaActorChara.ApplyDripping;
var
  EmitterDrip: TCastleParticleEmitter;
  EffectDrip: TCastleParticleEffect;
begin
  if NOT Assigned(FDesign) then Exit;

  EmitterDrip:= FDesign.DesignedComponent('EmitterDrip', False)
                as TCastleParticleEmitter;
  EffectDrip:= FDesign.DesignedComponent('EffectDrip', False)
               as TCastleParticleEffect;

  if (Assigned(EffectDrip) AND Assigned(EmitterDrip)) then
  begin
    EmitterDrip.Exists:= (FDripping > 0.05);
    EffectDrip.MaxParticles:= 1 + round(20.0 * FDripping);
  end;
end;

procedure TNyaActorChara.ApplySweating;
var
  EmitterSweat: TCastleParticleEmitter;
  EffectSweat: TCastleParticleEffect;
begin
  if NOT Assigned(FDesign) then Exit;

  EmitterSweat:= FDesign.DesignedComponent('EmitterSweat', False)
                 as TCastleParticleEmitter;
  EffectSweat:= FDesign.DesignedComponent('EffectSweat', False)
                as TCastleParticleEffect;

  if (Assigned(EffectSweat) AND Assigned(EmitterSweat)) then
  begin
    EmitterSweat.Exists:= (FSweating > 0.05);
    EffectSweat.MaxParticles:= 1 + round(5.0 * FSweating);
  end;
end;

procedure TNyaActorChara.UpdateJizz;
var
  ControlJizz: TCastleTransform;
  EmitterJizz: TCastleParticleEmitter;
begin
  if NOT Assigned(FDesign) then Exit;

  ControlJizz:= FDesign.DesignedComponent('Control_Jizz', False)
                as TCastleTransform;
  EmitterJizz:= FDesign.DesignedComponent('EmitterJizz', False)
                as TCastleParticleEmitter;

  if (Assigned(ControlJizz) AND Assigned(EmitterJizz)) then
    EmitterJizz.Exists:= (ControlJizz.Translation.Y > 0.5);
end;

procedure TNyaActorChara.UpdateSound;
var
  ControlStep: TCastleTransform;
begin
  if NOT Assigned(FDesign) then Exit;

  ControlStep:= FDesign.DesignedComponent('Control_Step', False)
                as TCastleTransform;

  if (Assigned(ControlStep) AND Assigned(SoundStep)) then
    SoundStep.SoundPlaying:= (ControlStep.Translation.Y > 0.1);
end;

function TNyaActorChara.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'Dripping', 'Sweating', 'SoundStep'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TNyaActorChara, 'Nya Actor Chara');
end.

