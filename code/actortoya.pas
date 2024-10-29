unit ActorToyA;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Generics.Collections,
  CastleSceneCore,
  CastleVectors, CastleTransform, CastleScene, ActorInterfaces, CharaDress,
  StrUtils;

type
  TActorToyA = class(TInterfacedObject, IActor)
  public
    constructor Create(actorRoot: TCastleTransformDesign);
    procedure Update(const SecondsPassed: Single);
    procedure PauseAnimation;
    procedure PlayAnimation(const animationName: String; loop: boolean = true);
    procedure PlayAnimation(const Parameters: TPlayAnimationParameters);
    procedure StopAnimation(const DisableStopNotification: Boolean = false);
    procedure SetSpeed(value: Single);
    procedure SetDripping(value: Single);
    function GetDresser(): TCharaDresser;
    procedure UseRailing(enable: Boolean);
  protected
    FActorRoot: TCastleTransformDesign;
    function GetCurrentTool(): TCastleScene;
  end;

implementation

uses
  CastleComponentSerialize;

constructor TActorToyA.Create(actorRoot: TCastleTransformDesign);
begin
  FActorRoot:= actorRoot;
end;

procedure TActorToyA.Update(const SecondsPassed: Single);
begin

end;

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


function TActorToyA.GetDresser(): TCharaDresser;
begin
  Result:= nil;
end;

function TActorToyA.GetCurrentTool(): TCastleScene;
begin
  Result:= FActorRoot.DesignedComponent('ToyA') as TCastleScene;
end;

procedure TActorToyA.UseRailing(enable: Boolean);
var
  railing: TCastleScene;
begin
  railing:= FActorRoot.DesignedComponent('Railing') as TCastleScene;

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

procedure TActorToyA.SetDripping(value: Single);
begin

end;

end.

