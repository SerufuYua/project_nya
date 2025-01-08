unit NyaActorToyA;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Generics.Collections,
  CastleSceneCore, CastleVectors, CastleTransform, CastleScene,
  CastleClassUtils, NyaActor, StrUtils;

type
  TNyaActorToyA = class(TNyaActor)
  protected
    FUseRailing: Boolean;
    procedure ApplyAutoAnimation; override;
    procedure SetRailing(enable: Boolean);
  public
    const
      DefaultUseRailing = False;

    constructor Create(AOwner: TComponent); override;
    procedure PlayAnimation(const animationName: String; loop: boolean = true); override;
    procedure PlayAnimation(const Parameters: TPlayAnimationParameters); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    property UseRailing: Boolean read FUseRailing write SetRailing
             {$ifdef FPC}default DefaultUseRailing{$endif};
  end;

implementation

uses
  CastleComponentSerialize, CastleUtils;

constructor TNyaActorToyA.Create(AOwner: TComponent);
var
  scene: TCastleScene;
begin
  inherited;

  UseRailing:= DefaultUseRailing;

  { make sure that ToyA is main }
  for scene in FAllScenes do
  begin
    if (scene.Name = 'ToyA') then
    begin
      FMainScene:= scene;
      UpdateAnimationsList;
      Break;
    end;
  end;
end;

procedure TNyaActorToyA.ApplyAutoAnimation;
begin
  UseRailing:= NOT StartsText('GAME.GIRL_TOYA.PLAY.A2', FAutoAnimation);

  inherited;
end;

procedure TNyaActorToyA.PlayAnimation(const animationName: String;
                                      loop: boolean = true);
begin
  UseRailing:= NOT StartsText('GAME.GIRL_TOYA.PLAY.A2', animationName);

  inherited;
end;

procedure TNyaActorToyA.PlayAnimation(const Parameters: TPlayAnimationParameters);
begin
  UseRailing:= NOT StartsText('GAME.GIRL_TOYA.PLAY.A2', Parameters.Name);

  inherited;
end;

procedure TNyaActorToyA.SetRailing(enable: Boolean);
var
  railing: TCastleScene;
begin
  if NOT Assigned(FDesign) then Exit;

  if (FUseRailing = enable) then Exit;
  FUseRailing:= enable;

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

function TNyaActorToyA.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'UseRailing'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TNyaActorToyA, 'Nya Actor Toy-A');
end.

