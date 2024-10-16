unit ActorChara;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Generics.Collections,
  CastleVectors, CastleTransform, CastleScene, MyCastleUtils, CastleColors,
  CharaDress;

type
  TActorChara = class
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
    constructor Create(actorRoot: TCastleTransformDesign);
    destructor Destroy; override;
    procedure SaveCondition;
    procedure PauseAnimation;
    procedure PlayAnimation(const animationName: String; loop: boolean = true);
    function GetDresser(): TCharaDresser;
    property Pos: TVector3 read GetPos write SetPos;
    property Rot: TVector4 read GetRot write SetRot;
    property Lightning: Boolean read GetLightning write SetLightning;
    property SelfEmission: Single write SetSelfEmission;
    property Speed: Single read GetSpeed write SetSpeed;
    property PersonalColor: TCastleColorRGB read GetColor;
  protected
    FRoot: TCastleTransformDesign;
    FDresser: TCharaDresser;
    FDresseSaver: TDressSaver;
    FActorName: String;
    function GetMainBody(): TCastleScene; { main actor Body }
    function GetActorsList(): TCastleScenes; { Body + Head + Hair}
    procedure ActionFaceDefault;
  end;

implementation

uses
  SysUtils, CastleComponentSerialize, X3DTime, X3DNodes;

constructor TActorChara.Create(actorRoot: TCastleTransformDesign);
var
  charaBody, charaHead: TCastleScene;
begin
  FRoot:= actorRoot as TCastleTransformDesign;

  charaBody:= FRoot.DesignedComponent('Body') as TCastleScene;
  charaHead:= FRoot.DesignedComponent('SceneHead') as TCastleScene;

  { create FDresser }
  FDresser:= TCharaDresser.Create(FRoot);
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
  head:= FRoot.DesignedComponent('SceneHead') as TCastleScene;
  head.PlayAnimation('Blink', true);
end;

function TActorChara.GetPos(): TVector3;
begin
  Result:= FRoot.Translation;
end;

procedure TActorChara.SetPos(coord: TVector3);
begin
  FRoot.Translation:= coord;
end;

function TActorChara.GetRot(): TVector4;
begin
  Result:= FRoot.Rotation;
end;

procedure TActorChara.SetRot(coord: TVector4);
begin
  FRoot.Rotation:= coord;
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
  items:= GetAllScenes(FRoot);

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
  items:= GetAllScenes(FRoot);

  for item in items do
  begin
    SetEmission(item, value, value, value, True);
  end;
end;

function TActorChara.GetSpeed: Single;
begin
  Result:= GetMainBody().TimePlayingSpeed;
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
  imageColor:= FRoot.DesignedComponent('PersonalColor', False) as TCastleImageTransform;

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
  bodies: TCastleScenes;
  body: TCastleScene;
begin
  bodies:= GetActorsList();
  for body in bodies do
    if Assigned(body) then
      body.PlayAnimation(animationName, loop);
end;

function TActorChara.GetMainBody(): TCastleScene;
begin
  Result:= FRoot.DesignedComponent('Body') as TCastleScene;
end;

function TActorChara.GetActorsList(): TCastleScenes;
var
  actors: TCastleScenes;
begin
  SetLength(actors, 3);
  actors[0]:= GetMainBody();
  actors[1]:= FRoot.DesignedComponent('SceneHead') as TCastleScene;
  actors[2]:= FRoot.DesignedComponent('SceneHair') as TCastleScene;

  Result:= actors;
end;

end.

