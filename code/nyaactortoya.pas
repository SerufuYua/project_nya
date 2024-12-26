unit NyaActorToyA;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Generics.Collections,
  CastleSceneCore, CastleVectors, CastleTransform, CastleScene, NyaBaseActor,
  StrUtils;

type
  TNyaActorToyA = class(TNyaBaseActor)
  protected
    FRailingUsed: Boolean;
    function GetSpeed: Single; override;
    procedure SetSpeed(value: Single); override;
    procedure SetAutoAnimation(const Value: String); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure PlayAnimation(const animationName: String; loop: boolean = true); override;
    procedure PlayAnimation(const Parameters: TPlayAnimationParameters); override;
    procedure StopAnimation(const DisableStopNotification: Boolean = false); override;
    function MainActor: TCastleScene; override;
    procedure UseRailing(enable: Boolean);
  end;

implementation

uses
  CastleComponentSerialize;

constructor TNyaActorToyA.Create(AOwner: TComponent);
begin
  inherited;

  FRailingUsed:= False;
end;

procedure TNyaActorToyA.SetAutoAnimation(const Value: String);
var
  tool: TCastleScene;
begin
  UseRailing(NOT StartsText('GAME.GIRL_TOYA.PLAY.A2', Value));

  if FAutoAnimation <> Value then
  begin
    FAutoAnimation:= Value;
    tool:= MainActor;
    if Assigned(tool) then
      tool.AutoAnimation:= Value;
  end;
end;

procedure TNyaActorToyA.PlayAnimation(const animationName: String;
                                             loop: boolean = true);
var
  tool: TCastleScene;
begin
  UseRailing(NOT StartsText('GAME.GIRL_TOYA.PLAY.A2', animationName));

  tool:= MainActor;
  if Assigned(tool) then
    tool.PlayAnimation(animationName, loop);
end;

procedure TNyaActorToyA.PlayAnimation(const Parameters: TPlayAnimationParameters);
var
  tool: TCastleScene;
begin
  UseRailing(NOT StartsText('GAME.GIRL_TOYA.PLAY.A2', Parameters.Name));

  tool:= MainActor;
  if Assigned(tool) then
    tool.PlayAnimation(Parameters);
end;

procedure TNyaActorToyA.StopAnimation(const DisableStopNotification: Boolean);
var
  tool: TCastleScene;
begin
  tool:= MainActor;
  if Assigned(tool) then
    tool.StopAnimation(DisableStopNotification);
end;

function TNyaActorToyA.MainActor: TCastleScene;
begin
  Result:= DesignedComponent('ToyA', False) as TCastleScene;
end;

procedure TNyaActorToyA.UseRailing(enable: Boolean);
var
  railing: TCastleScene;
begin
  if (FRailingUsed = enable) then Exit;
  FRailingUsed:= enable;

  railing:= DesignedComponent('Railing') as TCastleScene;

  if enable then
  begin
    railing.Translation:= Vector3(0, 0, 0);
    railing.Rotation:= Vector4(1, 0, 0, 0);
  end else begin
    railing.Translation:= Vector3(-28, 0, -10);
    railing.Rotation:= Vector4(0, 1, 0, -30);
  end;
end;

function TNyaActorToyA.GetSpeed: Single;
var
  tool: TCastleScene;
begin
  tool:= MainActor;
  if Assigned(tool) then
    Result:= tool.TimePlayingSpeed
  else
    Result:= 1.0;
end;

procedure TNyaActorToyA.SetSpeed(value: Single);
var
  tool: TCastleScene;
begin
  tool:= MainActor;
  if Assigned(tool) then
    tool.TimePlayingSpeed:= value;
end;

initialization
  RegisterSerializableComponent(TNyaActorToyA, 'Nya Actor Toy-A');
end.

