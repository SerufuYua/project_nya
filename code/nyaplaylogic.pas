unit NyaPlayLogic;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleSceneCore, CastleColors, CastleTimeUtils,
  X3DNodes, NyaFadeEffect, NyaActor, NyaActorChara;

type
  TActorsList = Array of TNyaActor;
  TCharasList = Array of TNyaActorChara;

  TNyaPlayLogic = class
  protected
    type
      TActorStatus = (Wait, Start, Go, FastGo, Finish, Relax);
  protected
    FActors: TActorsList;
    FAnimationsPrefix: String;
    FActionNum: String;
    FStatus: TActorStatus;
    FScreenFader: TNyaFadeEffect;
    FPleasure: Single;
    FTension: Single;
    FSpeed: Single;
    FActionCoeff: Single;
    FThresholdFastGo: Single;
    FThresholdTired: Single;
    FThresholdFinish: Single;
    procedure PlayAnimation(const Parameters: TPlayAnimationParameters;
                            bottomDress, footDress: boolean);
    procedure ActionIdle;   { Play Idle Animation.    IN Cycle }
    procedure ActionStart;  { Play Start Animation.   NO Cycle }
    procedure ActionGo;     { Play Go Animation.      IN Cycle }
    procedure ActionFastGo; { Play Fast Go Animation. IN Cycle }
    procedure ActionFinish; { Play Finish Animation.  NO Cycle }
    procedure ActionRelax;  { Play Relax Animation.   IN Cycle }
    procedure DoActionStartEnded(const Scene: TCastleSceneCore;
                                 const Animation: TTimeSensorNode);
    procedure DoActionFinishEnded(const Scene: TCastleSceneCore;
                                  const Animation: TTimeSensorNode);
    procedure SetActNum(const value: Integer);
    function GetActNum: Integer;
    procedure SetPleasure(const value: Single);
    procedure SetTension(const value: Single);
    procedure SetSpeed(const value: Single);
    function DetermineAnimationsPrefix: String;
    procedure Fade(duration: TFloatTime = 0.5);
  public
    const
      DefaultSpeed = 1.0;
      DefaultActionCoeff = 0.01;
      DefaultPleasure = 0.0;
      DefaultTension = 0.0;
      DefaultActNum = '';
      DefaultThresholdFastGo = 0.6;
      DefaultThresholdTired = 0.85;
      DefaultThresholdFinish = 0.999;

    constructor Create(actors: TActorsList; screenFader: TNyaFadeEffect);
    procedure Update(const SecondsPassed: Single);
    procedure Stop;
    procedure NextPart;
    property ActNum: Integer read GetActNum write SetActNum;
    property Pleasure: Single read FPleasure write SetPleasure;
    property Tension: Single read FTension write SetTension;
    property Speed: Single read FSpeed write SetSpeed;
    property ActionCoeff: Single read FActionCoeff write FActionCoeff;
    property ThresholdFastGo: Single read FThresholdFastGo write FThresholdFastGo;
    property ThresholdFinish: Single read FThresholdFinish write FThresholdFinish;
    function Actors: TActorsList;
    function Charas: TCharasList;
    function CombinedColor: TCastleColorRGB;
  end;

implementation

uses
  NyaCharaDress, CastleUtils, CastleVectors, Math, StrUtils;

const
  SuffixIdle = '.Idle';
  SuffixStart = '.Start';
  SuffixGo = '.Go';
  SuffixFastGo = '.FastGo';
  SuffixFinish = '.Finish';
  SuffixRelax = '.Relax';
  WithoutPants = 'condom';
  BareFoots = 'none';

constructor TNyaPlayLogic.Create(actors: TActorsList;
                                 screenFader: TNyaFadeEffect);
begin
  inherited Create;
  FActors:= actors;
  FAnimationsPrefix:= DetermineAnimationsPrefix;
  FScreenFader:= screenFader;
  FActionNum:= DefaultActNum;
  FPleasure:= DefaultPleasure;
  FTension:= DefaultTension;
  FSpeed:= DefaultSpeed;
  FActionCoeff:= DefaultActionCoeff;
  FThresholdFastGo:= DefaultThresholdFastGo;
  FThresholdTired:= DefaultThresholdTired;
  FThresholdFinish:= DefaultThresholdFinish;
end;

procedure TNyaPlayLogic.Update(const SecondsPassed: Single);
var
  chara: TNyaActorChara;
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

      if ((Pleasure > FThresholdFastGo) AND (Tension <= FThresholdTired)) then
        ActionFastGo
      else if (Pleasure > FThresholdTired) then
        ActionFastGo;
    end;
  TActorStatus.FastGo:
    begin
      Pleasure:= Pleasure + 3.0 *  FSpeed * ActionCoeff * SecondsPassed;
      Tension:= Tension + 4.0 *  FSpeed *  FSpeed * ActionCoeff * SecondsPassed;

      if ((Tension > FThresholdTired) AND (Pleasure < FThresholdTired)) then
        ActionGo
      else if (Pleasure >= FThresholdFinish) then
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
  for chara in Charas do
  begin
    chara.Dripping:= Pleasure;
    chara.Sweating:= Tension;
  end;
end;

procedure TNyaPlayLogic.SetActNum(const value: Integer);
begin
  FActionNum:= '.A' + IntToStr(value);
  FStatus:= TActorStatus.Start;
  ActionStart;
end;

function TNyaPlayLogic.GetActNum: Integer;
var
  numStr: String;
begin
  if (Length(FActionNum) > 2) then
  begin
    numStr:= copy(FActionNum, 3, (Length(FActionNum) - 2));
    Result:= StrToInt(numStr);
  end else
    Result:= 0;
end;

procedure TNyaPlayLogic.SetSpeed(const value: Single);
var
  actor: TNyaActor;
begin
  FSpeed:= value;
  for actor in Actors do
    actor.AnimationSpeed:= value;
end;

function TNyaPlayLogic.DetermineAnimationsPrefix: String;
var
  animationName: String;
  unfinish: Boolean;
  i: Integer;
begin
  Result:= '';
  if (Length(Actors) < 1) then Exit;
  if (Actors[0].AnimationsList.Count < 1) then Exit;
  unfinish:= True;
  i:= 0;

  while unfinish do
  begin
    i:= i + 1;
    Result:= copy(Actors[0].AnimationsList[0], 0, i);
    for animationName in Actors[0].AnimationsList do
    begin
      if NOT StartsText(Result, animationName) then
      begin
        unfinish:= False;
        { remove last symbol }
        Delete(Result, Length(Result), 1);
        { remove dot in the end of line }
        RemoveTrailingChars(Result, ['.']);
        Break;
      end;
    end;
  end;
end;

procedure TNyaPlayLogic.Fade(duration: TFloatTime = 0.5);
begin
  if Assigned(FScreenFader) then
    FScreenFader.Fade(duration);
end;

procedure TNyaPlayLogic.Stop;
var
  actor: TNyaActor;
begin
  for actor in Actors do
    actor.StopAnimation(True);

  ActionIdle;
end;

procedure TNyaPlayLogic.NextPart;
begin
  Case FStatus of
    TActorStatus.Start:  ActionGo;
    TActorStatus.Go:     ActionFastGo;
    TActorStatus.FastGo: ActionFinish;
    TActorStatus.Finish: ActionRelax;
    TActorStatus.Relax:  ActionIdle;
  end;
end;

procedure TNyaPlayLogic.PlayAnimation(const Parameters: TPlayAnimationParameters;
                                      bottomDress, footDress: boolean);
var
  actor: TNyaActor;
  chara: TNyaActorChara;
  dresser: TCharaDresser;
  noEventParam: TPlayAnimationParameters;
begin
  { undress charas }
  if (NOT (bottomDress AND footDress)) then
  begin
    for chara in Charas do
    begin
      dresser:= chara.Dresser;
      if Assigned(dresser) then
      begin
        if NOT bottomDress then
          dresser.DressSuitPart(TSuitPart.Bottom, WithoutPants);
        if NOT footDress then
          dresser.DressSuitPart(TSuitPart.Foots, BareFoots);
      end;
    end;
  end;

  { animate actors }
  noEventParam:= TPlayAnimationParameters.Create;
  noEventParam.Name:= Parameters.Name;
  noEventParam.Loop:= Parameters.Loop;
  noEventParam.Forward:= Parameters.Forward;
  noEventParam.StopNotification:= nil;
  noEventParam.TransitionDuration:= Parameters.TransitionDuration;
  noEventParam.InitialTime:= Parameters.InitialTime;

  for actor in Actors do
  begin
    if (actor = Actors[0]) then
      actor.PlayAnimation(Parameters)
    else
      actor.PlayAnimation(noEventParam);
  end;

  FreeAndNil(noEventParam);
end;

procedure TNyaPlayLogic.ActionIdle;
var
  AnimationParams: TPlayAnimationParameters;
begin
  AnimationParams:= TPlayAnimationParameters.Create;
  try
    AnimationParams.Name:= FAnimationsPrefix + SuffixIdle;
    AnimationParams.StopNotification:= nil;
    AnimationParams.Loop:= True;
    AnimationParams.Forward:= True;
    AnimationParams.TransitionDuration:= 0.0;
    Fade(0.75);
    PlayAnimation(AnimationParams, False, False);
  finally
    FreeAndNil(AnimationParams)
  end;

  FStatus:= TActorStatus.Wait;
end;

procedure TNyaPlayLogic.ActionStart;
var
  AnimationParams: TPlayAnimationParameters;
begin
  AnimationParams:= TPlayAnimationParameters.Create;
  try
    AnimationParams.Name:= FAnimationsPrefix + FActionNum + SuffixStart;
    AnimationParams.StopNotification:= {$ifdef FPC}@{$endif}DoActionStartEnded;
    AnimationParams.Loop:= False;
    AnimationParams.Forward:= True;
    AnimationParams.TransitionDuration:= 0.0;
    Fade(0.25);
    PlayAnimation(AnimationParams, False, True);
  finally
    FreeAndNil(AnimationParams)
  end;

  FStatus:= TActorStatus.Start;
end;

procedure TNyaPlayLogic.ActionGo;
var
  AnimationParams: TPlayAnimationParameters;
begin
  AnimationParams:= TPlayAnimationParameters.Create;
  try
    AnimationParams.Name:= FAnimationsPrefix + FActionNum + SuffixGO;
    AnimationParams.StopNotification:= nil;
    AnimationParams.Loop:= True;
    AnimationParams.Forward:= True;
    AnimationParams.TransitionDuration:= 1.0;
    PlayAnimation(AnimationParams, True, True);
  finally
    FreeAndNil(AnimationParams)
  end;

  FStatus:= TActorStatus.Go;
end;

procedure TNyaPlayLogic.ActionFastGo;
var
  AnimationParams: TPlayAnimationParameters;
begin
  AnimationParams:= TPlayAnimationParameters.Create;
  try
    AnimationParams.Name:= FAnimationsPrefix + FActionNum + SuffixFastGO;
    AnimationParams.StopNotification:= nil;
    AnimationParams.Loop:= True;
    AnimationParams.Forward:= True;
    AnimationParams.TransitionDuration:= 1.0;
    PlayAnimation(AnimationParams, True, True);
  finally
    FreeAndNil(AnimationParams)
  end;

  FStatus:= TActorStatus.FastGo;
end;

procedure TNyaPlayLogic.ActionFinish;
var
  AnimationParams: TPlayAnimationParameters;
begin
  AnimationParams:= TPlayAnimationParameters.Create;
  try
    AnimationParams.Name:= FAnimationsPrefix + FActionNum + SuffixFinish;
    AnimationParams.StopNotification:= {$ifdef FPC}@{$endif}DoActionFinishEnded;
    AnimationParams.Loop:= False;
    AnimationParams.Forward:= True;
    AnimationParams.TransitionDuration:= 0.1;
    PlayAnimation(AnimationParams, True, True);
  finally
    FreeAndNil(AnimationParams)
  end;

  FStatus:= TActorStatus.Finish;
end;

procedure TNyaPlayLogic.ActionRelax;
var
  AnimationParams: TPlayAnimationParameters;
begin
  AnimationParams:= TPlayAnimationParameters.Create;
  try
    AnimationParams.Name:= FAnimationsPrefix + FActionNum + SuffixRelax;
    AnimationParams.StopNotification:= nil;
    AnimationParams.Loop:= True;
    AnimationParams.Forward:= True;
    AnimationParams.TransitionDuration:= 0.0;
    Fade(0.6);
    PlayAnimation(AnimationParams, True, True);
  finally
    FreeAndNil(AnimationParams)
  end;

  FStatus:= TActorStatus.Relax;
  Pleasure:= Pleasure - 0.5 * Tension;
end;

procedure TNyaPlayLogic.DoActionStartEnded(const Scene: TCastleSceneCore;
                                           const Animation: TTimeSensorNode);
begin
  ActionGo;
end;

procedure TNyaPlayLogic.DoActionFinishEnded(const Scene: TCastleSceneCore;
                                            const Animation: TTimeSensorNode);
begin
  ActionRelax;
end;

procedure TNyaPlayLogic.SetPleasure(const value: Single);
begin
  FPleasure:= Clamped(value, 0.0, 1.0);
end;

procedure TNyaPlayLogic.SetTension(const value: Single);
begin
  FTension:= Clamped(value, 0.0, 1.0);
end;

function TNyaPlayLogic.Actors: TActorsList;
begin
  Result:= FActors;
end;

function TNyaPlayLogic.Charas: TCharasList;
var
  actor: TNyaActor;
begin
  Result:= [];

  for actor in Actors do
  begin
    if (actor is TNyaActorChara) then
      Insert((actor as TNyaActorChara), Result, 0);
  end;
end;

function TNyaPlayLogic.CombinedColor: TCastleColorRGB;
var
  chara: TNyaActorChara;
  averColor: TCastleColorRGB;
  count: Integer;
  maxColor: Single;
  maxAverColor: Single;
begin
  averColor:= Vector3(0.0, 0.0, 0.0);
  count:= 0;
  maxColor:= 0.0;
  maxAverColor:= 0.0;

  for chara in Charas do
  begin
    averColor:= averColor + chara.PersonalColor;
    maxColor:= max(max(max(chara.PersonalColor.X, chara.PersonalColor.Y),
               chara.PersonalColor.Y), maxColor);
    count:= count + 1;
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

