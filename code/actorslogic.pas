unit ActorsLogic;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ActorInterfaces, CastleSceneCore, X3DNodes, FadeInOut;

type
  TActorStatus = (Wait, Start, Go, FastGo, Finish, Relax);

  TActorsList = Array[0..1] of IActor;

  TActorsLogic = class
  protected
    FActors: TActorsList;
    FAnimationPrefix: String;
    FActionNum: String;
    FStatus: TActorStatus;
    FScreenFader: TImageFader;
    FPleasure: Single;
    FTension: Single;
    FSpeed: Single;
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
    procedure SetPleasure(value: Single);
    procedure SetTension(value: Single);
  public
    constructor Create(actorA, actorB: IActor; animationPrefix: String;
                       screenFader: TImageFader);
    procedure Update(const SecondsPassed: Single);
    procedure SetAction(num: Integer);
    procedure SetSpeed(value: Single);
    procedure Pause;
    procedure Stop;
    procedure NextPart;
    property Pleasure: Single read FPleasure write SetPleasure;
    property Tension: Single read FTension write SetTension;
  end;

implementation

uses
  CharaDress, CastleUtils;

const
  SuffixWait = '.IDLE';
  SuffixStart = '.P0';
  SuffixGo = '.P1';
  SuffixFastGo = '.P2';
  SuffixFinish = '.P3';
  SuffixRelax = '.P4';
  ActionCoeff = 0.1;

constructor TActorsLogic.Create(actorA, actorB: IActor; animationPrefix: String;
                                screenFader: TImageFader);
begin
  FActors[0]:= actorA;
  FActors[1]:= actorB;
  FAnimationPrefix:= animationPrefix;
  FScreenFader:= screenFader;
  FActionNum:= '';
  FPleasure:= 0.0;
  FTension:= 0.0;
  FSpeed:= 1.0;
  Stop;
end;

procedure TActorsLogic.Update(const SecondsPassed: Single);
begin
  Case FStatus of
  TActorStatus.Wait:
    begin
      Pleasure:= Pleasure - ActionCoeff * SecondsPassed;
      Tension:= Tension - ActionCoeff * SecondsPassed;
    end;
//  TActorStatus.Start:
  TActorStatus.Go:
    begin
      Pleasure:= Pleasure + FSpeed * ActionCoeff * SecondsPassed;
      Tension:= Tension + 0.5 *  FSpeed *  FSpeed * ActionCoeff * SecondsPassed;
      if ((Pleasure > 0.6) AND (Tension < 0.95)) then
        ActionFastGo
      else if (Pleasure > 0.95) then
        ActionFinish;
    end;
  TActorStatus.FastGo:
    begin
      Pleasure:= Pleasure + 3.0 *  FSpeed * ActionCoeff * SecondsPassed;
      Tension:= Tension + 4.0 *  FSpeed *  FSpeed * ActionCoeff * SecondsPassed;
      if (Tension > 0.95) then
        ActionGo
      else if (Pleasure > 0.95) then
        ActionFinish;
    end;
  TActorStatus.Finish:
    begin
      Pleasure:= 1.0;
    end;
  TActorStatus.Relax:
    begin
      Pleasure:= Pleasure - 0.5 * ActionCoeff * SecondsPassed;
      Tension:= Tension - ActionCoeff * SecondsPassed;
    end;
  end;
end;

procedure TActorsLogic.SetAction(num: Integer);
begin
  FActionNum:= '.A' + IntToStr(num);
  FStatus:= TActorStatus.Start;
  ActionStart;
end;

procedure TActorsLogic.SetSpeed(value: Single);
var
  actor: IActor;
begin
  FSpeed:= value;
  for actor in FActors do
    actor.SetSpeed(value);
end;

procedure TActorsLogic.Pause;
var
  actor: IActor;
begin
  for actor in FActors do
    actor.PauseAnimation;
end;

procedure TActorsLogic.Stop;
var
  actor: IActor;
begin
  for actor in FActors do
    actor.StopAnimation(True);

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

procedure TActorsLogic.SetPleasure(value: Single);
begin
  FPleasure:= Clamped(value, 0.0, 1.0);
end;

procedure TActorsLogic.SetTension(value: Single);
begin
  FTension:= Clamped(value, 0.0, 1.0);
end;

end.

