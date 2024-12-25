unit ActorsLogic;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, NyaBaseActor, CastleSceneCore, CastleColors,
  X3DNodes, NyaFadeEffect, ActorChara;

type
  TActorStatus = (Wait, Start, Go, FastGo, Finish, Relax);

  TActorsList = Array of TNyaBaseActor;

  TActorsLogic = class
  protected
    FActors: TActorsList;
    FAnimationPrefix: String;
    FActionNum: String;
    FStatus: TActorStatus;
    FScreenFader: TNyaFadeEffect;
    FPleasure: Single;
    FTension: Single;
    FSpeed: Single;
    FThresholdFastGo: Single;
    FThresholdFinish: Single;
    FEnableStopAction: Boolean;
    procedure PlayAnimation(const animationName: String;
                            loop, bottomDress: boolean; footDress: boolean);
    procedure PlayAnimation(const Parameters: TPlayAnimationParameters;
                            bottomDress: boolean; footDress: boolean);
    procedure ActionIdle;   { Play Idle Animation.    IN Cycle }
    procedure ActionStart;  { Play Start Animation.   NO Cycle }
    procedure ActionGo;     { Play Go Animation.      IN Cycle }
    procedure ActionFastGo; { Play Fast Go Animation. IN Cycle }
    procedure ActionFinish; { Play Finish Animation.  NO Cycle }
    procedure ActionRelax;  { Play Relax Animation. IN Cycle }
    procedure ActionStartStop(const Scene: TCastleSceneCore;
                              const Animation: TTimeSensorNode);
    procedure ActionFinishStop(const Scene: TCastleSceneCore;
                               const Animation: TTimeSensorNode);
    procedure SetPleasure(value: Single);
    procedure SetTension(value: Single);
    function GetCharas: TCharasList;
    function GetColor: TCastleColorRGB;
    procedure SetSpeed(value: Single);
  public
    const
      DefaultSpeed = 1.0;
      DefaultThresholdFastGo = 0.6;
      DefaultThresholdFinish = 0.95;

    constructor Create(actorA, actorB: TNyaBaseActor;
                       animationPrefix: String;
                       screenFader: TNyaFadeEffect);
    procedure Update(const SecondsPassed: Single);
    procedure SetAction(num: Integer);
    procedure Stop;
    procedure NextPart;
    property Pleasure: Single read FPleasure write SetPleasure;
    property Tension: Single read FTension write SetTension;
    property Charas: TCharasList read GetCharas;
    property CharasColor: TCastleColorRGB read GetColor;
    property Speed: Single read FSpeed write SetSpeed;
    property ThresholdFastGo: Single read FThresholdFastGo write FThresholdFastGo;
    property ThresholdFinish: Single read FThresholdFinish write FThresholdFinish;
  end;

implementation

uses
  CharaDress, CastleUtils, CastleVectors, NyaClassUtils, Math;

type
  TCharaDynamic = {$ifdef FPC}specialize{$endif} TDynamic<TActorChara>;

const
  SuffixWait = '.IDLE';
  SuffixStart = '.P0';
  SuffixGo = '.P1';
  SuffixFastGo = '.P2';
  SuffixFinish = '.P3';
  SuffixRelax = '.P4';
  ActionCoeff = 0.01;
  WithoutPants = 'condom';
  BareFoots = 'none';

constructor TActorsLogic.Create(actorA, actorB: TNyaBaseActor;
                                animationPrefix: String;
                                screenFader: TNyaFadeEffect);
begin
  SetLength(FActors, 2);
  FActors[0]:= actorA;
  FActors[1]:= actorB;
  FAnimationPrefix:= animationPrefix;
  FScreenFader:= screenFader;
  FActionNum:= '';
  FPleasure:= 0.0;
  FTension:= 0.0;
  FEnableStopAction:= False;
  FSpeed:= DefaultSpeed;
  FThresholdFastGo:= DefaultThresholdFastGo;
  FThresholdFinish:= DefaultThresholdFinish;
end;

procedure TActorsLogic.Update(const SecondsPassed: Single);
var
  actor: TNyaBaseActor;
begin
  { Update Pleasure/Tension Statuses }
  Case FStatus of
  TActorStatus.Wait:
    begin
      Pleasure:= Pleasure - 4.0 * ActionCoeff * SecondsPassed;
      Tension:= Tension - ActionCoeff * SecondsPassed;
    end;
//  TActorStatus.Start:
  TActorStatus.Go:
    begin
      Pleasure:= Pleasure + FSpeed * ActionCoeff * SecondsPassed;
      Tension:= Tension + 0.5 *  FSpeed *  FSpeed * ActionCoeff * SecondsPassed;
      if ((Pleasure > FThresholdFastGo) AND (Tension < 0.95)) then
        ActionFastGo
      else if (Pleasure > FThresholdFinish) then
        ActionFinish;
    end;
  TActorStatus.FastGo:
    begin
      Pleasure:= Pleasure + 3.0 *  FSpeed * ActionCoeff * SecondsPassed;
      Tension:= Tension + 4.0 *  FSpeed *  FSpeed * ActionCoeff * SecondsPassed;
      if (Tension > FThresholdFinish) then
        ActionGo
      else if (Pleasure > FThresholdFinish) then
        ActionFinish;
    end;
//  TActorStatus.Finish:
  TActorStatus.Relax:
    begin
      Pleasure:= Pleasure - 0.5 * ActionCoeff * SecondsPassed;
      Tension:= Tension - ActionCoeff * SecondsPassed;
    end;
  end;

  { Update Actors }
  for actor in FActors do
  begin
    actor.Pleasure:= Pleasure;
    actor.Tension:= Tension;
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
  actor: TNyaBaseActor;
begin
  FSpeed:= value;
  for actor in FActors do
    actor.Speed:= value;
end;

procedure TActorsLogic.Stop;
var
  actor: TNyaBaseActor;
begin
  for actor in FActors do
    actor.StopAnimation(True);

  ActionIdle;
end;

procedure TActorsLogic.NextPart;
begin
  Case FStatus of
    TActorStatus.Start:  ActionGo;
    TActorStatus.Go:     ActionFastGo;
    TActorStatus.FastGo: ActionFinish;
    TActorStatus.Finish: ActionRelax;
    TActorStatus.Relax:  ActionIdle;
  end;
end;

procedure TActorsLogic.PlayAnimation(const animationName: String;
                                    loop, bottomDress: boolean;
                                    footDress: boolean);
var
  actor: TNyaBaseActor;
  chara: TActorChara;
  dresser: TCharaDresser;
begin
  for actor in FActors do
  begin
    if (NOT (bottomDress AND footDress)) then
    begin
      chara:= TCharaDynamic.Cast(actor);
      if Assigned(chara) then
      begin
        dresser:= chara.GetDresser();
        begin
          if NOT bottomDress then
            dresser.WearSuit(TSuits.Bottom, WithoutPants);
          if NOT footDress then
            dresser.WearSuit(TSuits.Foots, BareFoots);
        end;
      end;
    end;
    actor.PlayAnimation(animationName, loop)
  end;
end;

procedure TActorsLogic.PlayAnimation(const Parameters: TPlayAnimationParameters;
                                     bottomDress: boolean;
                                     footDress: boolean);
var
  actor: TNyaBaseActor;
  chara: TActorChara;
  dresser: TCharaDresser;
begin
  for actor in FActors do
  begin
    if (NOT (bottomDress AND footDress)) then
    begin
      chara:= TCharaDynamic.Cast(actor);
      if Assigned(chara) then
      begin
        dresser:= chara.GetDresser();
        if Assigned(dresser) then
        begin
          if NOT bottomDress then
            dresser.WearSuit(TSuits.Bottom, WithoutPants);
          if NOT footDress then
            dresser.WearSuit(TSuits.Foots, BareFoots);
        end;
      end;
    end;
    actor.PlayAnimation(Parameters)
  end;
end;

procedure TActorsLogic.ActionIdle;
begin
  FStatus:= TActorStatus.Wait;
  FScreenFader.Fade(0.75);
  PlayAnimation(FAnimationPrefix + SuffixWait, True, True, False);
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
    FEnableStopAction:= True;
    PlayAnimation(AnimationParams, False, True);
  finally FreeAndNil(AnimationParams)
  end;
end;

procedure TActorsLogic.ActionGo;
begin
  FStatus:= TActorStatus.Go;
  FScreenFader.Fade(0.25);
  PlayAnimation(FAnimationPrefix + FActionNum + SuffixGO, True, True, True);
end;

procedure TActorsLogic.ActionFastGo;
begin
  FStatus:= TActorStatus.FastGo;
  FScreenFader.Fade(0.5);
  PlayAnimation(FAnimationPrefix + FActionNum + SuffixFastGO, True, True, True);
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
    FEnableStopAction:= True;
    PlayAnimation(AnimationParams, True, True);
  finally FreeAndNil(AnimationParams)
  end;
end;

procedure TActorsLogic.ActionRelax;
begin
  FStatus:= TActorStatus.Relax;
  FScreenFader.Fade(0.6);
  Pleasure:= Pleasure - 0.5 * Tension;
  PlayAnimation(FAnimationPrefix + FActionNum + SuffixRelax, True, True, True);
end;

procedure TActorsLogic.ActionStartStop(const Scene: TCastleSceneCore;
                                       const Animation: TTimeSensorNode);
begin
  if NOT FEnableStopAction then Exit;
  ActionGo;
  FEnableStopAction:= False;
end;

procedure TActorsLogic.ActionFinishStop(const Scene: TCastleSceneCore;
                                        const Animation: TTimeSensorNode);
begin
  if NOT FEnableStopAction then Exit;
  ActionRelax;
  FEnableStopAction:= False;
end;

procedure TActorsLogic.SetPleasure(value: Single);
begin
  FPleasure:= Clamped(value, 0.0, 1.0);
end;

procedure TActorsLogic.SetTension(value: Single);
begin
  FTension:= Clamped(value, 0.0, 1.0);
end;

function TActorsLogic.GetCharas: TCharasList;
var
  actor: TNyaBaseActor;
  chara: TActorChara;
begin
  Result:= [];

  for actor in FActors do
  begin
    chara:= TCharaDynamic.Cast(actor);
    if Assigned(chara) then
      Insert(chara, Result, 0);
  end;
end;

function TActorsLogic.GetColor: TCastleColorRGB;
var
  actor: TNyaBaseActor;
  chara: TActorChara;
  averColor: TCastleColorRGB;
  count: Integer;
  maxColor: Single;
  maxAverColor: Single;
begin
  averColor:= Vector3(0.0, 0.0, 0.0);
  count:= 0;
  maxColor:= 0.0;
  maxAverColor:= 0.0;

  for actor in FActors do
  begin
    chara:= TCharaDynamic.Cast(actor);
    if Assigned(chara) then
    begin
      averColor:= averColor + chara.PersonalColor;
      maxColor:= max(max(max(chara.PersonalColor.X, chara.PersonalColor.Y),
                 chara.PersonalColor.Y), maxColor);
      count:= count + 1;
    end;
  end;

  if (count > 1) then
  begin
    averColor:= averColor / count;
    maxAverColor:= max(max(averColor.X, averColor.Y), averColor.Y);
    averColor:= maxColor * averColor / maxAverColor;
  end;

  Result:= averColor;
end;

end.

