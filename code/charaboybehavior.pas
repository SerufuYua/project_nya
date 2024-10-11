unit CharaBoyBehavior;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Generics.Collections,
  CastleVectors, CastleTransform, CastleScene, CharaBehavior, CharaDress;

type
  TCharaBoyBehavior = class(TCharaBehavior)
  public
    procedure ActionPlayTogether_Idle;
    procedure ActionPlayTogether_A1P1;
    procedure ActionPlayTogether_A1P2;
  end;

implementation

uses
  CastleComponentSerialize;

procedure TCharaBoyBehavior.ActionPlayTogether_Idle;
begin
  ActionStand;
  Scene.Translation:= Vector3(9, 0, 0);
end;

procedure TCharaBoyBehavior.ActionPlayTogether_A1P1;
begin
  PlayAnimation('GAME.TOGETHER.PLAY.A1.P1');
  Dresser.WearSuit(TSuits.Bottom, 'none');
  Scene.Translation:= Vector3(0, 0, 0);
end;

procedure TCharaBoyBehavior.ActionPlayTogether_A1P2;
begin
  PlayAnimation('GAME.TOGETHER.PLAY.A1.P2');
  Dresser.WearSuit(TSuits.Bottom, 'none');
  Scene.Translation:= Vector3(0, 0, 0);
end;

initialization
  RegisterSerializableComponent(TCharaBoyBehavior, 'TCharaBoyBehavior');
end.

