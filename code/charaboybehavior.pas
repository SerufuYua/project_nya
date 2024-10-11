unit CharaBoyBehavior;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Generics.Collections,
  CastleVectors, CastleTransform, CastleScene, CharaBehavior, CharaDress;

type
  TCharaBoyBehavior = class(TCharaBehavior)
  public
    constructor Create(AOwner: TComponent); override;
    procedure ActionPlayTogether_Idle;
    procedure ActionPlayTogether_A1P1;
    procedure ActionPlayTogether_A1P2;
  end;

implementation

uses
  CastleComponentSerialize;

constructor TCharaBoyBehavior.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCharaName:= 'Boy';
end;

procedure TCharaBoyBehavior.ActionPlayTogether_Idle;
begin
  ActionStand;
  FScene.Translation:= Vector3(9, 0, 0);
end;

procedure TCharaBoyBehavior.ActionPlayTogether_A1P1;
begin
  PlayAnimation('GAME.TOGETHER.PLAY.A1.P1');
  FDresser.WearSuit(TSuits.Bottom, 'none');
  FScene.Translation:= Vector3(0, 0, 0);
end;

procedure TCharaBoyBehavior.ActionPlayTogether_A1P2;
begin
  PlayAnimation('GAME.TOGETHER.PLAY.A1.P2');
  FDresser.WearSuit(TSuits.Bottom, 'none');
  FScene.Translation:= Vector3(0, 0, 0);
end;

initialization
  RegisterSerializableComponent(TCharaBoyBehavior, 'TCharaBoyBehavior');
end.

