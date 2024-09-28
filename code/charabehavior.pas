unit CharaBehavior;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Generics.Collections,
  CastleVectors, CastleTransform, CastleScene, MyCastleUtils, CastleColors,
  CharaDress;

type
  TCharaBehavior = class(TCastleBehavior)
  private
    function GetPos(): TVector3;
    procedure SetPos(coord: TVector3);
    function GetLightning: Boolean;
    procedure SetLightning(enable: Boolean);
    procedure SetSelfEmission(value: Single);
    function GetSpeed: Single;
    procedure SetSpeed(value: Single);
    function GetColor: TCastleColorRGB;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ParentAfterAttach(); override;
    procedure Update(const SecondsPassed: Single;
                     var RemoveMe: TRemoveType); override;
    procedure ActionPause;
    procedure ActionStand;
    procedure ActionWalk;
    procedure ActionRun;
    function GetDresser(): TCharaDresser;
    property Pos: TVector3 read GetPos write SetPos;
    property Lightning: Boolean read GetLightning write SetLightning;
    property SelfEmission: Single write SetSelfEmission;
    property Speed: Single read GetSpeed write SetSpeed;
    property PersonalColor: TCastleColorRGB read GetColor;
  protected
    Scene: TCastleTransformDesign;
    Dresser: TCharaDresser;
    procedure PlayAnimation(const animationName: String);
    function GetMainBody(): TCastleScene; { main chara Body }
    function GetActorsList(): TCastleScenes; { Body + Head + Hair}
  end;

implementation

uses
  CastleComponentSerialize, X3DTime, X3DNodes;

constructor TCharaBehavior.Create(AOwner: TComponent);
begin
  inherited;
  { any other initialization }
end;

procedure TCharaBehavior.ParentAfterAttach;
var
  charaBody, charaHead: TCastleScene;
begin
  inherited;
  Scene:= Parent as TCastleTransformDesign;

  charaBody:= Scene.DesignedComponent('Body') as TCastleScene;
  charaHead:= Scene.DesignedComponent('SceneHead') as TCastleScene;

  { create dresser }
  Dresser:= TCharaDresser.Create(Scene);

  { set Anisotropic Filtering for character }
  SetAnisotropicFiltering(charaBody);
  SetAnisotropicFiltering(charaHead);

  { Set Emission }
  SelfEmission:= 0.6;

  { Default action}
  ActionStand;
end;

procedure TCharaBehavior.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  inherited;
  { any other updates }
end;

procedure TCharaBehavior.ActionPause;
var
  bodies: TCastleScenes;
  body: TCastleScene;
begin
  bodies:= GetActorsList();
  for body in bodies do
    if Assigned(body) then
      body.StopAnimation();
end;

procedure TCharaBehavior.ActionStand;
begin
  PlayAnimation('GAME.STAND');
end;

procedure TCharaBehavior.ActionWalk;
begin
  PlayAnimation('GAME.WALK.FORWARD');
end;

procedure TCharaBehavior.ActionRun;
begin
  PlayAnimation('GAME.RUN.FORWARD');
end;

function TCharaBehavior.GetPos(): TVector3;
begin
  Result:= Scene.Translation;
end;

procedure TCharaBehavior.SetPos(coord: TVector3);
begin
  Scene.Translation:= coord;
end;

function TCharaBehavior.GetDresser(): TCharaDresser;
begin
  Result:= Dresser;
end;

procedure TCharaBehavior.SetLightning(enable: Boolean);
var
  item: TCastleScene;
  items: TCastleScenes;
begin
  items:= GetAllScenes(Scene);

  for item in items do
  begin
    item.RenderOptions.Lighting:= enable;
  end;
end;

procedure TCharaBehavior.SetSelfEmission(value: Single);
var
  item: TCastleScene;
  items: TCastleScenes;
begin
  items:= GetAllScenes(Scene);

  for item in items do
  begin
    SetEmission(item, value, value, value, True);
  end;
end;

function TCharaBehavior.GetSpeed: Single;
begin
  Result:= GetMainBody().TimePlayingSpeed;
end;

procedure TCharaBehavior.SetSpeed(value: Single);
var
  bodies: TCastleScenes;
  body: TCastleScene;
begin
  bodies:= GetActorsList();
  for body in bodies do
    if Assigned(body) then
      body.TimePlayingSpeed:= value;
end;

function TCharaBehavior.GetColor: TCastleColorRGB;
var
  imageColor: TCastleImageTransform;
begin
  imageColor:= Scene.DesignedComponent('PersonalColor', False) as TCastleImageTransform;

  if Assigned(imageColor) then
    Result:= imageColor.Color.RGB
  else
    Result:=  Vector3(1.0, 1.0, 1.0);
end;

function TCharaBehavior.GetLightning: Boolean;
begin
  Result:= GetMainBody().RenderOptions.Lighting;
end;

procedure TCharaBehavior.PlayAnimation(const animationName: String);
var
  bodies: TCastleScenes;
  body: TCastleScene;
begin
  bodies:= GetActorsList();
  for body in bodies do
    if Assigned(body) then
      body.PlayAnimation(animationName, true);
end;

function TCharaBehavior.GetMainBody(): TCastleScene;
begin
  Result:= Scene.DesignedComponent('Body') as TCastleScene;
end;

function TCharaBehavior.GetActorsList(): TCastleScenes;
var
  actors: TCastleScenes;
begin
  SetLength(actors, 3);
  actors[0]:= GetMainBody();
  actors[1]:= Scene.DesignedComponent('SceneHead') as TCastleScene;
  actors[2]:= Scene.DesignedComponent('SceneHair') as TCastleScene;

  Result:= actors;
end;

end.

