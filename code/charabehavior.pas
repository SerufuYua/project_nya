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
    function GetRot(): TVector4;
    procedure SetRot(coord: TVector4);
    function GetLightning: Boolean;
    procedure SetLightning(enable: Boolean);
    procedure SetSelfEmission(value: Single);
    function GetSpeed: Single;
    procedure SetSpeed(value: Single);
    function GetColor: TCastleColorRGB;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ParentAfterAttach(); override;
    procedure Update(const SecondsPassed: Single;
                     var RemoveMe: TRemoveType); override;
    procedure SaveCondition;
    procedure ActionPause;
    procedure ActionStand;
    procedure ActionWalk;
    procedure ActionRun;
    procedure PlayAnimation(const animationName: String; loop: boolean = true);
    function GetDresser(): TCharaDresser;
    property Pos: TVector3 read GetPos write SetPos;
    property Rot: TVector4 read GetRot write SetRot;
    property Lightning: Boolean read GetLightning write SetLightning;
    property SelfEmission: Single write SetSelfEmission;
    property Speed: Single read GetSpeed write SetSpeed;
    property PersonalColor: TCastleColorRGB read GetColor;
  protected
    FScene: TCastleTransformDesign;
    FDresser: TCharaDresser;
    FDresseSaver: TDressSaver;
    FCharaName: String;
    function GetMainBody(): TCastleScene; { main chara Body }
    function GetActorsList(): TCastleScenes; { Body + Head + Hair}
    procedure ActionFaceDefault;
  end;

implementation

uses
  SysUtils, CastleComponentSerialize, X3DTime, X3DNodes;

constructor TCharaBehavior.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { any other initialization }
end;

destructor TCharaBehavior.Destroy;
begin
  if Assigned(FDresseSaver) then
    FreeAndNil(FDresseSaver);
  if Assigned(FDresser) then
    FreeAndNil(FDresser);
  inherited;
end;

procedure TCharaBehavior.ParentAfterAttach;
var
  charaBody, charaHead: TCastleScene;
begin
  inherited;
  FScene:= Parent as TCastleTransformDesign;

  charaBody:= FScene.DesignedComponent('Body') as TCastleScene;
  charaHead:= FScene.DesignedComponent('SceneHead') as TCastleScene;

  { create FDresser }
  FDresser:= TCharaDresser.Create(FScene);
  FDresseSaver:= TDressSaver.Create(FDresser, FCharaName);

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

procedure TCharaBehavior.SaveCondition;
begin
  FDresseSaver.SaveProperties;
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
  ActionFaceDefault;
  PlayAnimation('GAME.STAND');
end;

procedure TCharaBehavior.ActionWalk;
begin
  ActionFaceDefault;
  PlayAnimation('GAME.WALK.FORWARD');
end;

procedure TCharaBehavior.ActionRun;
begin
  ActionFaceDefault;
  PlayAnimation('GAME.RUN.FORWARD');
end;

procedure TCharaBehavior.ActionFaceDefault;
var
  head: TCastleScene;
begin
  head:= FScene.DesignedComponent('SceneHead') as TCastleScene;
  head.PlayAnimation('Blink', true);
end;

function TCharaBehavior.GetPos(): TVector3;
begin
  Result:= FScene.Translation;
end;

procedure TCharaBehavior.SetPos(coord: TVector3);
begin
  FScene.Translation:= coord;
end;

function TCharaBehavior.GetRot(): TVector4;
begin
  Result:= FScene.Rotation;
end;

procedure TCharaBehavior.SetRot(coord: TVector4);
begin
  FScene.Rotation:= coord;
end;

function TCharaBehavior.GetDresser(): TCharaDresser;
begin
  Result:= FDresser;
end;

procedure TCharaBehavior.SetLightning(enable: Boolean);
var
  item: TCastleScene;
  items: TCastleScenes;
begin
  items:= GetAllScenes(FScene);

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
  items:= GetAllScenes(FScene);

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
  imageColor:= FScene.DesignedComponent('PersonalColor', False) as TCastleImageTransform;

  if Assigned(imageColor) then
    Result:= imageColor.Color.RGB
  else
    Result:=  Vector3(1.0, 1.0, 1.0);
end;

function TCharaBehavior.GetLightning: Boolean;
begin
  Result:= GetMainBody().RenderOptions.Lighting;
end;

procedure TCharaBehavior.PlayAnimation(const animationName: String;
                                       loop: boolean);
var
  bodies: TCastleScenes;
  body: TCastleScene;
begin
  bodies:= GetActorsList();
  for body in bodies do
    if Assigned(body) then
      body.PlayAnimation(animationName, loop);
end;

function TCharaBehavior.GetMainBody(): TCastleScene;
begin
  Result:= FScene.DesignedComponent('Body') as TCastleScene;
end;

function TCharaBehavior.GetActorsList(): TCastleScenes;
var
  actors: TCastleScenes;
begin
  SetLength(actors, 3);
  actors[0]:= GetMainBody();
  actors[1]:= FScene.DesignedComponent('SceneHead') as TCastleScene;
  actors[2]:= FScene.DesignedComponent('SceneHair') as TCastleScene;

  Result:= actors;
end;

end.

