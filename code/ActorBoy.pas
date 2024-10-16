unit ActorBoy;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Generics.Collections,
  CastleVectors, CastleTransform, CastleScene, ActorChara, CharaDress;

type
  TActorBoy = class(TActorChara)
  public
    constructor Create(actorRoot: TCastleTransformDesign);
    procedure ActionPlayTogether_Idle;
    procedure ActionPlayTogether_A1P1;
    procedure ActionPlayTogether_A1P2;
  end;

implementation

uses
  CastleComponentSerialize;

constructor TActorBoy.Create(actorRoot: TCastleTransformDesign);
begin
  FActorName:= 'Boy';
  inherited Create(actorRoot);
end;

procedure TActorBoy.ActionPlayTogether_Idle;
begin
  //ActionStand;
  FRoot.Translation:= Vector3(9, 0, 0);
end;

procedure TActorBoy.ActionPlayTogether_A1P1;
begin
  PlayAnimation('GAME.TOGETHER.PLAY.A1.P1');
  FDresser.WearSuit(TSuits.Bottom, 'none');
  FRoot.Translation:= Vector3(0, 0, 0);
end;

procedure TActorBoy.ActionPlayTogether_A1P2;
begin
  PlayAnimation('GAME.TOGETHER.PLAY.A1.P2');
  FDresser.WearSuit(TSuits.Bottom, 'none');
  FRoot.Translation:= Vector3(0, 0, 0);
end;

end.

