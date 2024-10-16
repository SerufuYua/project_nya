unit ActorLogic;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ActorInterfaces;

type
  TActorsList = Array[0..1] of IActor;

  TActorLogic = class
  public
    constructor Create(actorA, actorB: IActor);
    procedure ActionPlayToyA_Idle;
    procedure ActionPlayToyA_A1P1;
    procedure ActionPlayToyA_A2P1;
  protected
    FActors: TActorsList;
  end;

implementation

uses
  CharaDress;

constructor TActorLogic.Create(actorA, actorB: IActor);
begin
  FActors[0]:= actorA;
  FActors[1]:= actorB;
end;

procedure TActorLogic.ActionPlayToyA_Idle;
var
  actor: IActor;
  dresser: TCharaDresser;
begin
  for actor in FActors do
  begin
    dresser:= actor.GetDresser();
    if Assigned(dresser) then
      dresser.WearSuit(TSuits.Bottom, 'none');
    actor.PlayAnimation('GAME.GIRL_TOYA.PLAY.IDLE');
  end;
end;

procedure TActorLogic.ActionPlayToyA_A1P1;
var
  actor: IActor;
  dresser: TCharaDresser;
begin
  for actor in FActors do
  begin
    dresser:= actor.GetDresser();
    if Assigned(dresser) then
      dresser.WearSuit(TSuits.Bottom, 'none');
    actor.PlayAnimation('GAME.GIRL_TOYA.PLAY.A1.P1');
  end;
end;

procedure TActorLogic.ActionPlayToyA_A2P1;
var
  actor: IActor;
  dresser: TCharaDresser;
begin
  for actor in FActors do
  begin
    dresser:= actor.GetDresser();
    if Assigned(dresser) then
      dresser.WearSuit(TSuits.Bottom, 'none');
    actor.PlayAnimation('GAME.GIRL_TOYA.PLAY.A2.P1');
  end;
end;

end.

