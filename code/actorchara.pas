unit ActorChara;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Generics.Collections,
  CastleVectors, CastleTransform, CastleScene, MyCastleUtils, CastleColors,
  CastleSceneCore,
  CharaDress, ActorInterfaces,
  CastleParticleEmitter;

type
  TActorChara = class(TInterfacedObject, IActor)
  private
    function GetTrans(): TVector3;
    procedure SetTrans(coord: TVector3);
    function GetRot(): TVector4;
    procedure SetRot(coord: TVector4);
    function GetLightning: Boolean;
    procedure SetLightning(enable: Boolean);
    procedure SetSelfEmission(value: Single);
    function GetColor: TCastleColorRGB;
  public
    constructor Create(actorRoot: TCastleTransformDesign; name: String);
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single);
    procedure SaveCondition;
    procedure PauseAnimation;
    procedure PlayAnimation(const animationName: String; loop: boolean = true);
    procedure PlayAnimation(const Parameters: TPlayAnimationParameters);
    procedure StopAnimation(const DisableStopNotification: Boolean = false);
    procedure SetSpeed(value: Single);
    procedure SetDripping(value: Single);
    function GetDresser(): TCharaDresser;
    property Translation: TVector3 read GetTrans write SetTrans;
    property Rotation: TVector4 read GetRot write SetRot;
    property Lightning: Boolean read GetLightning write SetLightning;
    property SelfEmission: Single write SetSelfEmission;
    property PersonalColor: TCastleColorRGB read GetColor;
  protected
    FActorRoot: TCastleTransformDesign;
    FDresser: TCharaDresser;
    FDresseSaver: TDressSaver;
    FControlJizz: TCastleTransform;
    FEffectJizz: TCastleParticleEmitter;
    EffectDrip: TCastleParticleEmitter;
    ParticleEffectDrip: TCastleParticleEffect;
    FActorName: String;
    function GetMainBody(): TCastleScene;    { main actor Body }
    function GetActorsList(): TCastleScenes; { Body + Head + Hair}
    procedure ActionFaceDefault;
  end;

implementation

uses
  SysUtils, CastleComponentSerialize, X3DTime, X3DNodes;

constructor TActorChara.Create(actorRoot: TCastleTransformDesign; name: String);
var
  charaBody, charaHead: TCastleScene;
begin
  FActorRoot:= actorRoot as TCastleTransformDesign;
  FActorName:= name;

  { juices setting }
  FControlJizz:= FActorRoot.DesignedComponent('Control_Jizz', False)
                 as TCastleTransform;
  FEffectJizz:= FActorRoot.DesignedComponent('EffectJizz', False)
                as TCastleParticleEmitter;
  EffectDrip:= FActorRoot.DesignedComponent('EffectDrip', False)
               as TCastleParticleEmitter;
  ParticleEffectDrip:= FActorRoot.DesignedComponent('ParticleEffectDrip', False)
                       as TCastleParticleEffect;

  charaBody:= FActorRoot.DesignedComponent('Body') as TCastleScene;
  charaHead:= FActorRoot.DesignedComponent('SceneHead') as TCastleScene;

  { create dresser }
  FDresser:= TCharaDresser.Create(FActorRoot);
  FDresseSaver:= TDressSaver.Create(FDresser, FActorName);

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

procedure TActorChara.Update(const SecondsPassed: Single);
begin
  if (Assigned(FControlJizz) AND Assigned(FEffectJizz)) then
    FEffectJizz.Exists:= (FControlJizz.Translation.Y > 0.5);
end;

procedure TActorChara.SaveCondition;
begin
  FDresseSaver.SaveProperties;
end;

procedure TActorChara.PauseAnimation;
var
  bodies: TCastleScenes;
  body: TCastleScene;
begin
  bodies:= GetActorsList();
  for body in bodies do
    if Assigned(body) then
      body.StopAnimation();
end;

procedure TActorChara.ActionFaceDefault;
var
  head: TCastleScene;
begin
  head:= FActorRoot.DesignedComponent('SceneHead') as TCastleScene;
  head.PlayAnimation('Blink', true);
end;

function TActorChara.GetTrans(): TVector3;
begin
  Result:= FActorRoot.Translation;
end;

procedure TActorChara.SetTrans(coord: TVector3);
begin
  FActorRoot.Translation:= coord;
end;

function TActorChara.GetRot(): TVector4;
begin
  Result:= FActorRoot.Rotation;
end;

procedure TActorChara.SetRot(coord: TVector4);
begin
  FActorRoot.Rotation:= coord;
end;

function TActorChara.GetDresser(): TCharaDresser;
begin
  Result:= FDresser;
end;

procedure TActorChara.SetLightning(enable: Boolean);
var
  item: TCastleScene;
  items: TCastleScenes;
begin
  items:= GetAllScenes(FActorRoot);

  for item in items do
  begin
    item.RenderOptions.Lighting:= enable;
  end;
end;

procedure TActorChara.SetSelfEmission(value: Single);
var
  item: TCastleScene;
  items: TCastleScenes;
begin
  items:= GetAllScenes(FActorRoot);

  for item in items do
  begin
    SetEmission(item, value, value, value, True);
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

procedure TActorChara.SetDripping(value: Single);
begin
  if (Assigned(ParticleEffectDrip) AND Assigned(EffectDrip)) then
  begin
    EffectDrip.Exists:= (value > 0.05);
    ParticleEffectDrip.MaxParticles:= round(10.0 * value);
  end;
end;

function TActorChara.GetColor: TCastleColorRGB;
var
  imageColor: TCastleImageTransform;
begin
  imageColor:= FActorRoot.DesignedComponent('PersonalColor', False) as TCastleImageTransform;

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
  ActionFaceDefault;
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
  ActionFaceDefault;
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
  ActionFaceDefault;
  actors:= GetActorsList();
  for actor in actors do
    if Assigned(actor) then
      actor.StopAnimation(DisableStopNotification);
end;

function TActorChara.GetMainBody(): TCastleScene;
begin
  Result:= FActorRoot.DesignedComponent('Body', False) as TCastleScene;
end;

function TActorChara.GetActorsList(): TCastleScenes;
var
  actors: TCastleScenes;
begin
  SetLength(actors, 4);
  actors[0]:= GetMainBody();
  actors[1]:= FActorRoot.DesignedComponent('SceneHead', False) as TCastleScene;
  actors[2]:= FActorRoot.DesignedComponent('SceneHair', False) as TCastleScene;
  actors[3]:= FActorRoot.DesignedComponent('Controller', False) as TCastleScene;

  Result:= actors;
end;

end.

