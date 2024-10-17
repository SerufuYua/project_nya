unit ActorLogic;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ActorInterfaces;

type
  TActorStatus = (Wait, Start, Go, FastGo, Finish, Relax);

  TActorsList = Array[0..1] of IActor;

  TActorsLogic = class
  public
    constructor Create(actorA, actorB: IActor);
    procedure SetAction(num: Integer);
    procedure PauseAction;
    procedure StopAction;
    procedure NextPartAction;
    procedure ActionPlayToyA_A1P1;
    procedure ActionPlayToyA_A2P1;
  protected
    FActors: TActorsList;
    FActionNum: String;
    FNewStatus: TActorStatus;
    FOldStatus: TActorStatus;
    procedure PlayAnimation(const animationName: String;
                            loop, bottomDress: boolean);
    procedure ActionIdle;
  end;

implementation

uses
  CharaDress;

const
  Prefix = 'GAME.GIRL_TOYA.PLAY';
  SuffixWait = '.IDLE';
  SuffixStart = '.P0';
  SuffixGo = '.P1';
  SuffixFastGo = '.P2';
  SuffixFinish = '.P3';
  SuffixRelax = '.P4';

constructor TActorsLogic.Create(actorA, actorB: IActor);
begin
  FActors[0]:= actorA;
  FActors[1]:= actorB;
  ActionIdle;
end;

procedure TActorsLogic.SetAction(num: Integer);
begin
  FActionNum:= '.A' + IntToStr(num);
  FNewStatus:= Start;
  FOldStatus:= Start;
end;

procedure TActorsLogic.PauseAction;
var
  actor: IActor;
begin
  for actor in FActors do
    actor.PauseAnimation;
end;

procedure TActorsLogic.StopAction;
begin
  FNewStatus:= Wait;
  FOldStatus:= Wait;
  ActionIdle;
end;

procedure TActorsLogic.NextPartAction;
begin

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

procedure TActorsLogic.ActionIdle;
begin
  PlayAnimation(Prefix + SuffixWait, True, True);
end;

procedure TActorsLogic.ActionPlayToyA_A1P1;
begin
  PlayAnimation('GAME.GIRL_TOYA.PLAY.A1.P1', True, False);
end;

procedure TActorsLogic.ActionPlayToyA_A2P1;
begin
  PlayAnimation('GAME.GIRL_TOYA.PLAY.A2.P1', True, False);
end;

end.

