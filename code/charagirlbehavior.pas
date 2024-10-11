unit CharaGirlBehavior;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Generics.Collections,
  CastleVectors, CastleTransform, CastleScene, CharaBehavior, CharaDress;

type
  TCharaGirlBehavior = class(TCharaBehavior)
  public
    constructor Create(AOwner: TComponent); override;
    procedure ActionIdle;
    procedure ActionPlayToyA_Idle;
    procedure ActionPlayToyA_A1P1;
    procedure ActionPlayToyA_A2P1;
    procedure ActionPlayTogether_Idle;
    procedure ActionPlayTogether_A1P1;
    procedure ActionPlayTogether_A1P2;
  protected
  end;

implementation

uses
  CastleComponentSerialize;

constructor TCharaGirlBehavior.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCharaName:= 'Girl';
end;

procedure TCharaGirlBehavior.ActionIdle;
begin
  ActionStand;
  FScene.Translation:= Vector3(0, 0, -30);
end;

procedure TCharaGirlBehavior.ActionPlayToyA_Idle;
begin
  PlayAnimation('GAME.GIRL_TOYA.PLAY.IDLE');
  FScene.Translation:= Vector3(0, 0, 0);
end;

procedure TCharaGirlBehavior.ActionPlayToyA_A1P1;
begin
  ActionFaceDefault;
  PlayAnimation('GAME.GIRL_TOYA.PLAY.A1.P1');
  FDresser.WearSuit(TSuits.Bottom, 'none');
  FScene.Translation:= Vector3(0, 0, 0);
end;

procedure TCharaGirlBehavior.ActionPlayToyA_A2P1;
begin
  ActionFaceDefault;
  PlayAnimation('GAME.GIRL_TOYA.PLAY.A2.P1');
  FDresser.WearSuit(TSuits.Bottom, 'none');
  FScene.Translation:= Vector3(0, 0, 0);
end;

procedure TCharaGirlBehavior.ActionPlayTogether_Idle;
begin
  ActionStand;
  FScene.Translation:= Vector3(-9, 0, 0);
end;

procedure TCharaGirlBehavior.ActionPlayTogether_A1P1;
begin
  PlayAnimation('GAME.TOGETHER.PLAY.A1.P1');
  FDresser.WearSuit(TSuits.Bottom, 'none');
  FScene.Translation:= Vector3(0, 0, 0);
end;

procedure TCharaGirlBehavior.ActionPlayTogether_A1P2;
begin
  PlayAnimation('GAME.TOGETHER.PLAY.A1.P2');
  FDresser.WearSuit(TSuits.Bottom, 'none');
  FScene.Translation:= Vector3(0, 0, 0);
end;

initialization
  RegisterSerializableComponent(TCharaGirlBehavior, 'TCharaGirlBehavior');
end.

