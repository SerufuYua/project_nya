unit NyaScene;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CastleScene, X3DNodes, CastleSceneCore, CastleShapes, CastleClassUtils;

type
  TNyaScene = class(TCastleScene)
  protected
    FAnisotropicDegree: Single;
    procedure SetAnisotropicDegree(value: Single);
    procedure ApplyAnisotropicDegree;

    procedure HandleNodeAnisotropic(sceneNode: TX3DNode);
  public
    const
      DefaultAnisotropicDegree = 0.0;

    constructor Create(AOwner: TComponent); override;
    procedure DoGeometryChanged(const Change: TGeometryChange;
                                LocalGeometryShape: TShape); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    property AnisotropicDegree: Single read FAnisotropicDegree write SetAnisotropicDegree
      {$ifdef FPC}default DefaultAnisotropicDegree{$endif};
  end;

implementation

uses
  CastleComponentSerialize, CastleTextureImages, CastleUtils;

constructor TNyaScene.Create(AOwner: TComponent);
begin
  inherited;

  FAnisotropicDegree:= DefaultAnisotropicDegree;
end;

procedure TNyaScene.DoGeometryChanged(const Change: TGeometryChange;
                                      LocalGeometryShape: TShape);
begin
  inherited;

  ApplyAnisotropicDegree;
end;

procedure TNyaScene.SetAnisotropicDegree(value: Single);
begin
  if (FAnisotropicDegree = value) then Exit;
  FAnisotropicDegree:= value;
  ApplyAnisotropicDegree;
end;

procedure TNyaScene.ApplyAnisotropicDegree;
begin
  if Assigned(RootNode) then
    RootNode.EnumerateNodes(TImageTextureNode,
                            {$ifdef FPC}@{$endif}HandleNodeAnisotropic,
                            false);
end;

procedure TNyaScene.HandleNodeAnisotropic(sceneNode: TX3DNode);
var
  ImageTexture: TImageTextureNode;
begin
  ImageTexture:= sceneNode as TImageTextureNode;

  if ImageTexture.IsTextureImage then
    ImageTexture.TextureProperties.AnisotropicDegree:= FAnisotropicDegree;
end;

function TNyaScene.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'AnisotropicDegree'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TNyaScene, 'Nya Scene');
end.

