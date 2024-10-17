unit ActorsLogic;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ActorInterfaces, CastleSceneCore, X3DNodes, FadeInOut;

type
  TActorStatus = (Wait, Start, Go, FastGo, Finish, Relax);

  TActorsList = Array[0..1] of IActor;

  TActorsLogic = class
  public
    constructor Create(actorA, actorB: IActor; animationPrefix: String;
                       screenFader: TImageFader);
    procedure SetAction(num: Integer);
    procedure Pause;
    procedure Stop;
    procedure NextPart;
  protected
    FActors: TActorsList;
    FAnimationPrefix: String;
    FActionNum: String;
    FStatus: TActorStatus;
    FScreenFader: TImageFader;
    procedure PlayAnimation(const animationName: String;
                            loop, bottomDress: boolean);
    procedure PlayAnimation(const Parameters: TPlayAnimationParameters;
                            bottomDress: boolean);
    procedure ActionIdle;   { Play Idle Animation.    IN Cycle }
    procedure ActionStart;  { Play Start Animation.   NO Cycle }
    procedure ActionGo;     { Play Go Animation.      IN Cycle }
    procedure ActionFastGo; { Play Fast Go Animation. IN Cycle }
    procedure ActionFinish; { Play Finish Animation.  NO Cycle }
    procedure ActionRelax;  { Play Fast Go Animation. IN Cycle }
    procedure ActionStartStop(const Scene: TCastleSceneCore;
                              const Animation: TTimeSensorNode);
    procedure ActionFinishStop(const Scene: TCastleSceneCore;
                              const Animation: TTimeSensorNode);
  end;

implementation

uses
  CharaDress;

const
  SuffixWait = '.IDLE';
  SuffixStart = '.P0';
  SuffixGo = '.P1';
  SuffixFastGo = '.P2';
  SuffixFinish = '.P3';
  SuffixRelax = '.P4';

constructor TActorsLogic.Create(actorA, actorB: IActor; animationPrefix: String;
                                screenFader: TImageFader);
begin
  FActors[0]:= actorA;
  FActors[1]:= actorB;
  FAnimationPrefix:= animationPrefix;
  FScreenFader:= screenFader;
  FActionNum:= '';
  Stop;
end;

procedure TActorsLogic.SetAction(num: Integer);
begin
  FActionNum:= '.A' + IntToStr(num);
  FStatus:= TActorStatus.Start;
  ActionStart;
end;

procedure TActorsLogic.Pause;
var
  actor: IActor;
begin
  for actor in FActors do
    actor.PauseAnimation;
end;

procedure TActorsLogic.Stop;
begin
  FStatus:= TActorStatus.Wait;
  ActionIdle;
end;

procedure TActorsLogic.NextPart;
begin
  Case FStatus of
  TActorStatus.Start:
    ActionGo;
  TActorStatus.Go:
    ActionFastGo;
  TActorStatus.FastGo:
    ActionFinish;
  TActorStatus.Finish:
    ActionRelax;
  TActorStatus.Relax:
    ActionIdle;
  end;
end;

procedure TActorsLogic.PlayAnimation(const animationName: String;
                                    loop, bottomDress: boolean);
var
  actor: IActor;
  dresser: TCharaDresser;
begin
  for actor in FActors do
  begin
    if NOT bottomDress then
    begin
      dresser:= actor.GetDresser();
      if Assigned(dresser) then
        dresser.WearSuit(TSuits.Bottom, 'none');
    end;
    actor.PlayAnimation(animationName, loop)
  end;
end;

procedure TActorsLogic.PlayAnimation(const Parameters: TPlayAnimationParameters;
                                     bottomDress: boolean);
var
  actor: IActor;
  dresser: TCharaDresser;
begin
  for actor in FActors do
  begin
    if NOT bottomDress then
    begin
      dresser:= actor.GetDresser();
      if Assigned(dresser) then
        dresser.WearSuit(TSuits.Bottom, 'none');
    end;
    actor.PlayAnimation(Parameters)
  end;
end;

procedure TActorsLogic.ActionIdle;
begin
  FStatus:= TActorStatus.Wait;
  FScreenFader.Fade(0.75);
  PlayAnimation(FAnimationPrefix + SuffixWait, True, True);
end;

procedure TActorsLogic.ActionStart;
var
  AnimationParams: TPlayAnimationParameters;
begin
  AnimationParams := TPlayAnimationParameters.Create;
  try
    AnimationParams.Name:= FAnimationPrefix + FActionNum + SuffixStart;
    AnimationParams.StopNotification:= {$ifdef FPC}@{$endif}ActionStartStop;
    AnimationParams.Loop:= False;
    FStatus:= TActorStatus.Start;
    FScreenFader.Fade(0.25);
    PlayAnimation(AnimationParams, False);
  finally FreeAndNil(AnimationParams)
  end;
end;

procedure TActorsLogic.ActionGo;
begin
  FStatus:= TActorStatus.Go;
  FScreenFader.Fade(0.25);
  PlayAnimation(FAnimationPrefix + FActionNum + SuffixGO, True, True);
end;

procedure TActorsLogic.ActionFastGo;
begin
  FStatus:= TActorStatus.FastGo;
  FScreenFader.Fade(0.5);
  PlayAnimation(FAnimationPrefix + FActionNum + SuffixFastGO, True, True);
end;

procedure TActorsLogic.ActionFinish;
var
  AnimationParams: TPlayAnimationParameters;
begin
  AnimationParams := TPlayAnimationParameters.Create;
  try
    AnimationParams.Name:= FAnimationPrefix + FActionNum + SuffixFinish;
    AnimationParams.StopNotification:= {$ifdef FPC}@{$endif}ActionFinishStop;
    AnimationParams.Loop:= False;
    FStatus:= TActorStatus.Finish;
    FScreenFader.Fade(0.25);
    PlayAnimation(AnimationParams, False);
  finally FreeAndNil(AnimationParams)
  end;
end;

procedure TActorsLogic.ActionRelax;
begin
  FStatus:= TActorStatus.Relax;
  FScreenFader.Fade(0.6);
  PlayAnimation(FAnimationPrefix + FActionNum + SuffixRelax, True, True);
end;

procedure TActorsLogic.ActionStartStop(const Scene: TCastleSceneCore;
                                       const Animation: TTimeSensorNode);
begin
  ActionGo;
end;

procedure TActorsLogic.ActionFinishStop(const Scene: TCastleSceneCore;
                                        const Animation: TTimeSensorNode);
begin
  ActionRelax;
end;

end.

