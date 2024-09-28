unit CharaGirlBehavior;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Generics.Collections,
  CastleVectors, CastleTransform, CastleScene, CharaBehavior, CharaDress;

type
  TCharaGirlBehavior = class(TCharaBehavior)
  public
    procedure ActionIdle;
    procedure ActionPlayA1;
    procedure ActionPlayA2;
    procedure ActionPlayA3;
    procedure ActionPlayA4;
    procedure ActionPlayA5;
    procedure ActionIdleTogether;
    procedure ActionPlayTogetherA1P1;
    procedure ActionPlayTogetherA1P2;
  protected
  end;

implementation

uses
  CastleComponentSerialize;

procedure TCharaGirlBehavior.ActionIdle;
begin
  ActionStand;
  Scene.Translation:= Vector3(0, 0, -30);
end;

procedure TCharaGirlBehavior.ActionPlayA1;
begin
  PlayAnimation('GAME.RU.PLAY.A1');
  WearSuit(TSuits.Bottom, 'none');
  Scene.Translation:= Vector3(0, 0, 0);
end;

procedure TCharaGirlBehavior.ActionPlayA2;
begin
  PlayAnimation('GAME.RU.PLAY.A2');
  WearSuit(TSuits.Bottom, 'none');
  Scene.Translation:= Vector3(0, 0, 0);
end;

procedure TCharaGirlBehavior.ActionPlayA3;
begin
  PlayAnimation('GAME.RU.PLAY.A3');
  WearSuit(TSuits.Bottom, 'none');
  Scene.Translation:= Vector3(0, 0, 0);
end;

procedure TCharaGirlBehavior.ActionPlayA4;
begin
  PlayAnimation('GAME.RU.PLAY.A4');
  WearSuit(TSuits.Bottom, 'none');
  Scene.Translation:= Vector3(0, 0, 0);
end;

procedure TCharaGirlBehavior.ActionPlayA5;
begin
  PlayAnimation('GAME.RU.PLAY.A5');
  WearSuit(TSuits.Bottom, 'none');
  Scene.Translation:= Vector3(0, 0, 0);
end;

procedure TCharaGirlBehavior.ActionIdleTogether;
begin
  ActionStand;
  Scene.Translation:= Vector3(-9, 0, 0);
end;

procedure TCharaGirlBehavior.ActionPlayTogetherA1P1;
begin
  PlayAnimation('GAME.TOGETHER.PLAY.A1.P1');
  WearSuit(TSuits.Bottom, 'none');
  Scene.Translation:= Vector3(0, 0, 0);
end;

procedure TCharaGirlBehavior.ActionPlayTogetherA1P2;
begin
  PlayAnimation('GAME.TOGETHER.PLAY.A1.P2');
  WearSuit(TSuits.Bottom, 'none');
  Scene.Translation:= Vector3(0, 0, 0);
end;

initialization
  RegisterSerializableComponent(TCharaGirlBehavior, 'TCharaGirlBehavior');
end.

