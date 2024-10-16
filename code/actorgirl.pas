unit ActorGirl;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Generics.Collections,
  CastleVectors, CastleTransform, CastleScene, ActorChara, CharaDress;

type
  TActorGirl = class(TActorChara)
  public
    constructor Create(actorRoot: TCastleTransformDesign);
    procedure ActionPlayTogether_Idle;
    procedure ActionPlayTogether_A1P1;
    procedure ActionPlayTogether_A1P2;
  protected
  end;

implementation

uses
  CastleComponentSerialize;

constructor TActorGirl.Create(actorRoot: TCastleTransformDesign);
begin
  FActorName:= 'Girl';
  inherited Create(actorRoot);
end;

procedure TActorGirl.ActionPlayTogether_Idle;
begin
  //ActionStand;
  FActorRoot.Translation:= Vector3(-9, 0, 0);
end;

procedure TActorGirl.ActionPlayTogether_A1P1;
begin
  PlayAnimation('GAME.TOGETHER.PLAY.A1.P1');
  FDresser.WearSuit(TSuits.Bottom, 'none');
  FActorRoot.Translation:= Vector3(0, 0, 0);
end;

procedure TActorGirl.ActionPlayTogether_A1P2;
begin
  PlayAnimation('GAME.TOGETHER.PLAY.A1.P2');
  FDresser.WearSuit(TSuits.Bottom, 'none');
  FActorRoot.Translation:= Vector3(0, 0, 0);
end;

end.

