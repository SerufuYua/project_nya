unit ActorChara;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Generics.Collections,
  CastleVectors, CastleTransform, CastleScene, NyaCastleUtils, CastleColors,
  CastleSceneCore,
  CharaDress, NyaBaseActor,
  CastleParticleEmitter;

type
  TActorChara = class(TNyaBaseActor)
  protected
    FDresser: TCharaDresser;
    FDresseSaver: TDressSaver;
    FControlJizz: TCastleTransform;
    FEmitterJizz: TCastleParticleEmitter;
    FEffectDrip: TCastleParticleEffect;
    FEmitterDrip: TCastleParticleEmitter;
    FEmitterSweat: TCastleParticleEmitter;
    FEffectSweat: TCastleParticleEffect;
    function GetMainBody(): TCastleScene;    { main actor Body }
    function GetActorsList(): TCastleScenes; { Body + Head + Hair}
    procedure SetAutoAnimation(const Value: String);
    function GetAutoAnimation: String;
    procedure SetLightning(enable: Boolean);
    function GetLightning: Boolean;
    procedure SetSelfEmission(value: Single);
    procedure SetDripping(value: Single);
    procedure SetSweating(value: Single);
    function GetColor: TCastleColorRGB;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    procedure SaveCondition;
    procedure PlayAnimation(const animationName: String; loop: boolean = true); override;
    procedure PlayAnimation(const Parameters: TPlayAnimationParameters); override;
    procedure StopAnimation(const DisableStopNotification: Boolean = false); override;
    procedure SetSpeed(value: Single); override;
    function GetDresser(): TCharaDresser;
    property AutoAnimation: String read GetAutoAnimation write SetAutoAnimation;
    property Lightning: Boolean read GetLightning write SetLightning;
    property SelfEmission: Single write SetSelfEmission;
    property PersonalColor: TCastleColorRGB read GetColor;
  end;

  TCharasList = Array of TActorChara;

implementation

uses
  SysUtils, CastleComponentSerialize, X3DTime, X3DNodes;

constructor TActorChara.Create(AOwner: TComponent);
var
  charaBody, charaHead: TCastleScene;
begin
  inherited;

  { juices setting }
{

  charaBody:= ActorRoot.DesignedComponent('Body') as TCastleScene;
  charaHead:= ActorRoot.DesignedComponent('SceneHead') as TCastleScene;}

  { create dresser }
  FDresser:= TCharaDresser.Create(self);
  FDresseSaver:= TDressSaver.Create(FDresser, ActorName);

  { set Anisotropic Filtering for character }
  SetAnisotropicFiltering(charaBody);
  SetAnisotropicFiltering(charaHead);
end;

destructor TActorChara.Destroy;
begin
  if Assigned(FDresseSaver) then
    FreeAndNil(FDresseSaver);
  if Assigned(FDresser) then
    FreeAndNil(FDresser);
  inherited;
end;

procedure TActorChara.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
var
  ControlJizz: TCastleTransform;
  EmitterJizz: TCastleParticleEmitter;
begin
  ControlJizz:= DesignedComponent('Control_Jizz', False)
                as TCastleTransform;
  EmitterJizz:= DesignedComponent('EmitterJizz', False)
                as TCastleParticleEmitter;

  if (Assigned(ControlJizz) AND Assigned(EmitterJizz)) then
    EmitterJizz.Exists:= (ControlJizz.Translation.Y > 0.5);

end;

procedure TActorChara.SaveCondition;
begin
  FDresseSaver.SaveProperties;
end;

function TActorChara.GetDresser(): TCharaDresser;
begin
  Result:= FDresser;
end;

procedure TActorChara.SetAutoAnimation(const Value: String);
var
  actors: TCastleScenes;
  actor: TCastleScene;
begin
  actors:= GetActorsList();
  for actor in actors do
    if Assigned(actor) then
      actor.AutoAnimation:= Value;
end;

function TActorChara.GetAutoAnimation: String;
begin
  Result:= GetMainBody().AutoAnimation;
end;

procedure TActorChara.SetLightning(enable: Boolean);
var
  item: TCastleScene;
begin
  for item in GetAllScenes(self) do
  begin
    item.RenderOptions.Lighting:= enable;
  end;
end;

procedure TActorChara.SetSelfEmission(value: Single);
var
  item: TCastleScene;
begin
  for item in GetAllScenes(self) do
  begin
    SetEmission(item, value, value, value, True);
  end;
end;

procedure TActorChara.SetDripping(value: Single);
begin
  if (Assigned(FEffectDrip) AND Assigned(FEmitterDrip)) then
  begin
    FEmitterDrip.Exists:= (value > 0.05);
    FEffectDrip.MaxParticles:= 1 + round(20.0 * value);
  end;
end;

procedure TActorChara.SetSweating(value: Single);
begin
  if (Assigned(FEffectSweat) AND Assigned(FEmitterSweat)) then
  begin
    FEmitterSweat.Exists:= (value > 0.05);
    FEffectSweat.MaxParticles:= 1 + round(5.0 * value);
  end;
end;

procedure TActorChara.SetSpeed(value: Single);
var
  bodies: TCastleScenes;
  body: TCastleScene;
begin
  bodies:= GetActorsList();
  for body in bodies do
    if Assigned(body) then
      body.TimePlayingSpeed:= value;
end;

function TActorChara.GetColor: TCastleColorRGB;
var
  imageColor: TCastleImageTransform;
begin
  imageColor:= DesignedComponent('PersonalColor', False) as TCastleImageTransform;

  if Assigned(imageColor) then
    Result:= imageColor.Color.RGB
  else
    Result:=  Vector3(1.0, 1.0, 1.0);
end;

function TActorChara.GetLightning: Boolean;
begin
  Result:= GetMainBody().RenderOptions.Lighting;
end;

procedure TActorChara.PlayAnimation(const animationName: String;
                                       loop: boolean);
var
  actors: TCastleScenes;
  actor: TCastleScene;
begin
  actors:= GetActorsList();
  for actor in actors do
    if Assigned(actor) then
      actor.PlayAnimation(animationName, loop);
end;

procedure TActorChara.PlayAnimation(const Parameters: TPlayAnimationParameters);
var
  actors: TCastleScenes;
  actor: TCastleScene;
  noEventParam: TPlayAnimationParameters;
begin
  actors:= GetActorsList();
  for actor in actors do
    if Assigned(actor) then
      if (actor = GetMainBody()) then
        actor.PlayAnimation(Parameters)
      else begin
        noEventParam:= Parameters;
        Parameters.StopNotification:= nil;
        actor.PlayAnimation(noEventParam)
      end;
end;

procedure TActorChara.StopAnimation(const DisableStopNotification: Boolean);
var
  actors: TCastleScenes;
  actor: TCastleScene;
begin
  actors:= GetActorsList();
  for actor in actors do
    if Assigned(actor) then
      actor.StopAnimation(DisableStopNotification);
end;

function TActorChara.GetMainBody(): TCastleScene;
begin
  Result:= DesignedComponent('Body', False) as TCastleScene;
end;

function TActorChara.GetActorsList(): TCastleScenes;
var
  actors: TCastleScenes;
begin
  SetLength(actors, 4);
  actors[0]:= GetMainBody();
  actors[1]:= DesignedComponent('SceneHead', False) as TCastleScene;
  actors[2]:= DesignedComponent('SceneHair', False) as TCastleScene;
  actors[3]:= DesignedComponent('Controller', False) as TCastleScene;

  Result:= actors;
end;

end.

