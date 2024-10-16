unit ActorToyA;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Generics.Collections,
  CastleVectors, CastleTransform, CastleScene, ActorInterfaces;

type
  TActorToyA = class(TInterfacedObject, IActor)
  private
    function GetSpeed: Single;
    procedure SetSpeed(value: Single);
  public
    constructor Create(actorRoot: TCastleTransformDesign);
    procedure PauseAnimation;
    procedure PlayAnimation(const animationName: String; loop: boolean = true);
    procedure ActionPlayToyA_Idle;
    procedure ActionPlayToyA_A1P1;
    procedure ActionPlayToyA_A2P1;
    property Speed: Single read GetSpeed write SetSpeed;
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
  ActionPlayToyA_Idle;
end;

procedure TActorToyA.PauseAnimation;
begin
  GetCurrentTool().StopAnimation();
end;

procedure TActorToyA.PlayAnimation(const animationName: String;
                                             loop: boolean = true);
begin
  GetCurrentTool().PlayAnimation(animationName, loop);
end;

procedure TActorToyA.ActionPlayToyA_Idle;
begin
  UseRailing(True);
  PlayAnimation('GAME.GIRL_TOYA.PLAY.IDLE', true);
end;

procedure TActorToyA.ActionPlayToyA_A1P1;
begin
  UseRailing(True);
  PlayAnimation('GAME.GIRL_TOYA.PLAY.A1.P1', true);
end;

procedure TActorToyA.ActionPlayToyA_A2P1;
begin
  UseRailing(False);
  PlayAnimation('GAME.GIRL_TOYA.PLAY.A2.P1', true);
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

function TActorToyA.GetSpeed: Single;
begin
  Result:= GetCurrentTool().TimePlayingSpeed;
end;

procedure TActorToyA.SetSpeed(value: Single);
begin
  GetCurrentTool().TimePlayingSpeed:= value;
end;

end.

