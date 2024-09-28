unit CharaBoyBehavior;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Generics.Collections,
  CastleVectors, CastleTransform, CastleScene, CharaBehavior, CharaDress;

type
  TCharaBoyBehavior = class(TCharaBehavior)
  public
    procedure ParentAfterAttach(); override;
    procedure Update(const SecondsPassed: Single;
                     var RemoveMe: TRemoveType); override;
    procedure ActionIdleTogether;
    procedure ActionPlayTogetherA1P1;
    procedure ActionPlayTogetherA1P2;
  end;

implementation

uses
  CastleComponentSerialize;

procedure TCharaBoyBehavior.ParentAfterAttach;
begin
  inherited;
end;

procedure TCharaBoyBehavior.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  inherited;
end;

procedure TCharaBoyBehavior.ActionIdleTogether;
begin
  ActionStand;
  Scene.Translation:= Vector3(9, 0, 0);
end;

procedure TCharaBoyBehavior.ActionPlayTogetherA1P1;
begin
  PlayAnimation('GAME.TOGETHER.PLAY.A1.P1');
  Dresser.WearSuit(TSuits.Bottom, 'none');
  Scene.Translation:= Vector3(0, 0, 0);
end;

procedure TCharaBoyBehavior.ActionPlayTogetherA1P2;
begin
  PlayAnimation('GAME.TOGETHER.PLAY.A1.P2');
  Dresser.WearSuit(TSuits.Bottom, 'none');
  Scene.Translation:= Vector3(0, 0, 0);
end;

initialization
  RegisterSerializableComponent(TCharaBoyBehavior, 'TCharaBoyBehavior');
end.

