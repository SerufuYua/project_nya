unit NyaActorToyA;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Generics.Collections,
  CastleSceneCore, CastleVectors, CastleTransform, CastleScene, NyaActor,
  StrUtils;

type
  TNyaActorToyA = class(TNyaActor)
  protected
    FRailingUsed: Boolean;
    procedure ApplyAutoAnimation; override;
  public
    const
      DefaultRailingUsed = False;

    constructor Create(AOwner: TComponent); override;
    procedure PlayAnimation(const animationName: String; loop: boolean = true); override;
    procedure PlayAnimation(const Parameters: TPlayAnimationParameters); override;
    procedure UseRailing(enable: Boolean);
  end;

implementation

uses
  CastleComponentSerialize;

constructor TNyaActorToyA.Create(AOwner: TComponent);
begin
  inherited;

  FRailingUsed:= DefaultRailingUsed;
end;

procedure TNyaActorToyA.ApplyAutoAnimation;
begin
  UseRailing(NOT StartsText('GAME.GIRL_TOYA.PLAY.A2', FAutoAnimation));

  inherited;
end;

procedure TNyaActorToyA.PlayAnimation(const animationName: String;
                                      loop: boolean = true);
begin
  UseRailing(NOT StartsText('GAME.GIRL_TOYA.PLAY.A2', animationName));

  inherited;
end;

procedure TNyaActorToyA.PlayAnimation(const Parameters: TPlayAnimationParameters);
begin
  UseRailing(NOT StartsText('GAME.GIRL_TOYA.PLAY.A2', Parameters.Name));

  inherited;
end;

procedure TNyaActorToyA.UseRailing(enable: Boolean);
var
  railing: TCastleScene;
begin
  if NOT Assigned(FDesign) then Exit;

  if (FRailingUsed = enable) then Exit;
  FRailingUsed:= enable;

  railing:= FDesign.DesignedComponent('Railing', False) as TCastleScene;
  if NOT Assigned(railing) then Exit;

  if enable then
  begin
    railing.Translation:= Vector3(0, 0, 0);
    railing.Rotation:= Vector4(1, 0, 0, 0);
  end else begin
    railing.Translation:= Vector3(-28, 0, -10);
    railing.Rotation:= Vector4(0, 1, 0, -30);
  end;
end;

initialization
  RegisterSerializableComponent(TNyaActorToyA, 'Nya Actor Toy-A');
end.

