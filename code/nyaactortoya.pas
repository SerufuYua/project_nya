unit NyaActorToyA;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Generics.Collections,
  CastleSceneCore, CastleVectors, CastleTransform, CastleScene,
  CastleClassUtils, NyaActor, StrUtils;

type
  TNyaActorToyA = class(TNyaActor)
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  CastleComponentSerialize, CastleUtils;

constructor TNyaActorToyA.Create(AOwner: TComponent);
var
  scene: TCastleScene;
begin
  inherited;

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

initialization
  RegisterSerializableComponent(TNyaActorToyA, 'Nya Actor Toy-A');
end.

