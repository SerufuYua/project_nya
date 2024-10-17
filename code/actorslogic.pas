unit ActorsLogic;

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
    procedure Pause;
    procedure Stop;
    procedure NextPart;
  protected
    FActors: TActorsList;
    FActionNum: String;
    FStatus: TActorStatus;
    procedure PlayAnimation(const animationName: String;
                            loop, bottomDress: boolean);
    procedure ActionIdle;  { Play Idle Animation. IN Cycle }
    procedure ActionStart; { Play Start Animation. NO Cycle }
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
  FActionNum:= '';
  Stop;
end;

procedure TActorsLogic.SetAction(num: Integer);
begin
  FActionNum:= '.A' + IntToStr(num);
  FStatus:= Start;
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
  FStatus:= Wait;
  ActionIdle;
end;

procedure TActorsLogic.NextPart;
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

procedure TActorsLogic.ActionStart;
begin
  PlayAnimation(Prefix + FActionNum + SuffixStart, False, False);
end;

end.

