unit ActorToyA;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Generics.Collections,
  CastleVectors, CastleTransform, CastleScene, ActorInterfaces, CharaDress,
  StrUtils;

type
  TActorToyA = class(TInterfacedObject, IActor)
  private
    function GetSpeed: Single;
    procedure SetSpeed(value: Single);
  public
    constructor Create(actorRoot: TCastleTransformDesign);
    procedure PauseAnimation;
    procedure PlayAnimation(const animationName: String; loop: boolean = true);
    function GetDresser(): TCharaDresser;
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

function TActorToyA.GetSpeed: Single;
begin
  Result:= GetCurrentTool().TimePlayingSpeed;
end;

procedure TActorToyA.SetSpeed(value: Single);
begin
  GetCurrentTool().TimePlayingSpeed:= value;
end;

end.

