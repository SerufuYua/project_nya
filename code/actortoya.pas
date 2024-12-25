unit ActorToyA;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Generics.Collections,
  CastleSceneCore,
  CastleVectors, CastleTransform, CastleScene, NyaBaseActor, CharaDress,
  StrUtils;

type
  TActorToyA = class(TNyaBaseActor)
  protected
    function GetCurrentTool(): TCastleScene;
  public
    procedure PauseAnimation; override;
    procedure PlayAnimation(const animationName: String; loop: boolean = true); override;
    procedure PlayAnimation(const Parameters: TPlayAnimationParameters); override;
    procedure StopAnimation(const DisableStopNotification: Boolean = false); override;
    procedure SetSpeed(value: Single); override;
    procedure UseRailing(enable: Boolean);
  end;

implementation

uses
  CastleComponentSerialize;

procedure TActorToyA.PauseAnimation;
begin
  GetCurrentTool().StopAnimation();
end;

procedure TActorToyA.PlayAnimation(const animationName: String;
                                             loop: boolean = true);
begin
  UseRailing(NOT StartsText('GAME.GIRL_TOYA.PLAY.A2', animationName));
  GetCurrentTool().PlayAnimation(animationName, loop);
end;

procedure TActorToyA.PlayAnimation(const Parameters: TPlayAnimationParameters);
begin
  UseRailing(NOT StartsText('GAME.GIRL_TOYA.PLAY.A2', Parameters.Name));
  GetCurrentTool().PlayAnimation(Parameters);
end;

procedure TActorToyA.StopAnimation(const DisableStopNotification: Boolean);
begin
  GetCurrentTool().StopAnimation(DisableStopNotification);
end;

function TActorToyA.GetCurrentTool(): TCastleScene;
begin
  Result:= DesignedComponent('ToyA') as TCastleScene;
end;

procedure TActorToyA.UseRailing(enable: Boolean);
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

procedure TActorToyA.SetSpeed(value: Single);
begin
  GetCurrentTool().TimePlayingSpeed:= value;
end;

end.

