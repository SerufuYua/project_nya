unit NyaActorChara;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, NyaBaseActor, CastleClassUtils, CastleSceneCore,
  CharaDress, CastleTransform, NyaCastleUtils, CastleScene;

type
  TNyaActorChara = class(TNyaBaseActor)
  protected
    FDresser: TCharaDresser;
    FDripping: Single;
    FSweating: Single;
    procedure SetSpeed(value: Single); override;
    procedure SetDripping(value: Single);
    procedure SetSweating(value: Single);
    procedure SetAutoAnimation(const Value: String); override;
    procedure UpdateJizz;
  public
    const
      DefaultDripping = 0.0;
      DefaultSweating = 0.0;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PrepareResources(const Options: TPrepareResourcesOptions;
                               const Params: TPrepareParams); override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    procedure PlayAnimation(const animationName: String; loop: boolean = true); override;
    procedure PlayAnimation(const Parameters: TPlayAnimationParameters); override;
    procedure StopAnimation(const DisableStopNotification: Boolean = false); override;
    function MainActor: TCastleScene; override; { main actor Body }
    function ActorsList: TCastleScenes;         { Body + Head + Hair}
    function Dresser: TCharaDresser;
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

procedure TNyaActorChara.PrepareResources(const Options: TPrepareResourcesOptions;
                                         const Params: TPrepareParams);
var
  value: Single;
begin
  inherited;

  { Dripping: Single; }
  value:= FDripping;
  FDripping:= value - 1.0;
  Dripping:= value;

  { Sweating: Single; }
  value:= FSweating;
  FSweating:= value - 1.0;
  Sweating:= value;

  Dresser.RestoreCondition(ActorName);
end;

procedure TNyaActorChara.Update(const SecondsPassed: Single;
                                var RemoveMe: TRemoveType);
begin
  inherited;

  UpdateJizz;
end;

procedure TNyaActorChara.SetAutoAnimation(const Value: String);
var
  actor: TCastleScene;
begin
  if (FAutoAnimation = Value) then Exit;
  FAutoAnimation:= Value;

  for actor in ActorsList do
    actor.AutoAnimation:= Value;
end;

procedure TNyaActorChara.PlayAnimation(const animationName: String;
                                       loop: boolean = true);
var
  actor: TCastleScene;
begin
  for actor in ActorsList do
    actor.PlayAnimation(animationName, loop);
end;

procedure TNyaActorChara.PlayAnimation(const Parameters: TPlayAnimationParameters);
var
  actor: TCastleScene;
  body: TCastleScene;
  noEventParam: TPlayAnimationParameters;
begin
  body:= MainActor;
  for actor in ActorsList do
  begin
    if (actor = body) then
      actor.PlayAnimation(Parameters)
    else begin
      noEventParam:= Parameters;
      Parameters.StopNotification:= nil;
      actor.PlayAnimation(noEventParam)
    end;
  end;
end;

procedure TNyaActorChara.StopAnimation(const DisableStopNotification: Boolean);
var
  actor: TCastleScene;
begin
  for actor in ActorsList do
    actor.StopAnimation(DisableStopNotification);
end;

function TNyaActorChara.Dresser: TCharaDresser;
begin
  if NOT Assigned(FDresser) then
    FDresser:= TCharaDresser.Create(self);

  Result:= FDresser;
end;

procedure TNyaActorChara.SaveCondition;
begin
  Dresser.SaveCondition(ActorName);
end;

function TNyaActorChara.MainActor: TCastleScene;
begin
  Result:= DesignedComponent('Body', False) as TCastleScene;
end;

function TNyaActorChara.ActorsList: TCastleScenes;
var
  actor: TCastleScene;
  i: Integer;
begin
  i:= -1;
  Result:= [];
  SetLength(Result, 4);

  actor:= MainActor;
  if Assigned(actor) then
  begin
    i:= i + 1;
    Result[i]:= actor;
  end;

  actor:= DesignedComponent('SceneHead', False) as TCastleScene;
  if Assigned(actor) then
  begin
    i:= i + 1;
    Result[i]:= actor;
  end;

  actor:= DesignedComponent('SceneHair', False) as TCastleScene;
  if Assigned(actor) then
  begin
    i:= i + 1;
    Result[i]:= actor;
  end;

  actor:= DesignedComponent('Controller', False) as TCastleScene;
  if Assigned(actor) then
  begin
    i:= i + 1;
    Result[i]:= actor;
  end;

  SetLength(Result, i + 1);
end;

procedure TNyaActorChara.SetSpeed(value: Single);
var
  bodies: TCastleScenes;
  body: TCastleScene;
begin
  if (FSpeed = value) then Exit;
  FSpeed:= value;

  bodies:= ActorsList;
  for body in bodies do
    if Assigned(body) then
      body.TimePlayingSpeed:= value;
end;

procedure TNyaActorChara.SetDripping(value: Single);
var
  EmitterDrip: TCastleParticleEmitter;
  EffectDrip: TCastleParticleEffect;
begin
  if (FDripping = value) then Exit;

  EmitterDrip:= DesignedComponent('EmitterDrip', False)
                as TCastleParticleEmitter;
  EffectDrip:= DesignedComponent('EffectDrip', False)
               as TCastleParticleEffect;

  if (Assigned(EffectDrip) AND Assigned(EmitterDrip)) then
  begin
    FDripping:= value;
    EmitterDrip.Exists:= (FDripping > 0.05);
    EffectDrip.MaxParticles:= 1 + round(20.0 * FDripping);
  end;
end;

procedure TNyaActorChara.SetSweating(value: Single);
var
  EmitterSweat: TCastleParticleEmitter;
  EffectSweat: TCastleParticleEffect;
begin
  if (FSweating = value) then Exit;

  EmitterSweat:= DesignedComponent('EmitterSweat', False)
                 as TCastleParticleEmitter;
  EffectSweat:= DesignedComponent('EffectSweat', False)
                as TCastleParticleEffect;

  if (Assigned(EffectSweat) AND Assigned(EmitterSweat)) then
  begin
    FSweating:= value;
    EmitterSweat.Exists:= (FSweating > 0.05);
    EffectSweat.MaxParticles:= 1 + round(5.0 * FSweating);
  end;
end;

procedure TNyaActorChara.UpdateJizz;
var
  ControlJizz: TCastleTransform;
  EmitterJizz: TCastleParticleEmitter;
begin
  inherited;

  ControlJizz:= DesignedComponent('Control_Jizz', False)
                as TCastleTransform;
  EmitterJizz:= DesignedComponent('EmitterJizz', False)
                as TCastleParticleEmitter;

  if (Assigned(ControlJizz) AND Assigned(EmitterJizz)) then
    EmitterJizz.Exists:= (ControlJizz.Translation.Y > 0.5);
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

