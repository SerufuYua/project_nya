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
    function GetCurrentTool(): TCastleScene;
  public
    procedure PlayAnimation(const animationName: String; loop: boolean = true); override;
    procedure PlayAnimation(const Parameters: TPlayAnimationParameters); override;
    procedure StopAnimation(const DisableStopNotification: Boolean = false); override;
    procedure SetSpeed(value: Single); override;
    procedure UseRailing(enable: Boolean);
  end;

implementation

uses
  CastleComponentSerialize;

procedure TNyaActorToyA.PlayAnimation(const animationName: String;
                                             loop: boolean = true);
var
  tool: TCastleScene;
begin
  UseRailing(NOT StartsText('GAME.GIRL_TOYA.PLAY.A2', animationName));

  tool:= GetCurrentTool();
  if Assigned(tool) then
    tool.PlayAnimation(animationName, loop);
end;

procedure TNyaActorToyA.PlayAnimation(const Parameters: TPlayAnimationParameters);
var
  tool: TCastleScene;
begin
  UseRailing(NOT StartsText('GAME.GIRL_TOYA.PLAY.A2', Parameters.Name));

  tool:= GetCurrentTool();
  if Assigned(tool) then
    tool.PlayAnimation(Parameters);
end;

procedure TNyaActorToyA.StopAnimation(const DisableStopNotification: Boolean);
var
  tool: TCastleScene;
begin
  tool:= GetCurrentTool();
  if Assigned(tool) then
    tool.StopAnimation(DisableStopNotification);
end;

function TNyaActorToyA.GetCurrentTool(): TCastleScene;
begin
  Result:= DesignedComponent('ToyA', False) as TCastleScene;
end;

procedure TNyaActorToyA.UseRailing(enable: Boolean);
var
  railing: TCastleScene;
begin
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

procedure TNyaActorToyA.SetSpeed(value: Single);
var
  tool: TCastleScene;
begin
  tool:= GetCurrentTool();
  if Assigned(tool) then
    tool.TimePlayingSpeed:= value;
end;

initialization
  RegisterSerializableComponent(TNyaActorToyA, 'Nya Actor Toy-A');
end.

