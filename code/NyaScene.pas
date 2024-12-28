unit NyaScene;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CastleScene, X3DNodes, CastleSceneCore, CastleShapes;

type
  TNyaScene = class(TCastleScene)
  protected
    FAnisotropicDegree: Single;
    FAnisotropicDegreeSaved: Single;
    procedure SetAnisotropicDegree(value: Single);

    procedure HandleNodeAnisotropic(sceneNode: TX3DNode);
  public
    const
      DefaultAnisotropicDegree = 0.0;

    constructor Create(AOwner: TComponent); override;
    procedure DoGeometryChanged(const Change: TGeometryChange;
                                LocalGeometryShape: TShape); override;
  published
    property AnisotropicDegree: Single read FAnisotropicDegree write SetAnisotropicDegree
      {$ifdef FPC}default DefaultAnisotropicDegree{$endif};
  end;

implementation

uses
  CastleComponentSerialize, CastleTextureImages;

constructor TNyaScene.Create(AOwner: TComponent);
begin
  inherited;

  FAnisotropicDegree:= DefaultAnisotropicDegree;
end;

procedure TNyaScene.DoGeometryChanged(const Change: TGeometryChange;
                                      LocalGeometryShape: TShape);
begin
  inherited;

  { AnisotropicDegree: Single; }
  FAnisotropicDegree:= FAnisotropicDegreeSaved - 1.0;
  AnisotropicDegree:= FAnisotropicDegreeSaved;
end;

procedure TNyaScene.SetAnisotropicDegree(value: Single);
begin
  FAnisotropicDegreeSaved:= value;

  if (Assigned(RootNode) AND (FAnisotropicDegree <> FAnisotropicDegreeSaved)) then
  begin
    FAnisotropicDegree:= FAnisotropicDegreeSaved;
    RootNode.EnumerateNodes(TImageTextureNode,
                            {$ifdef FPC}@{$endif}HandleNodeAnisotropic,
                            false);
  end;
end;

procedure TNyaScene.HandleNodeAnisotropic(sceneNode: TX3DNode);
var
  ImageTexture: TImageTextureNode;
begin
  ImageTexture:= sceneNode as TImageTextureNode;

  if ImageTexture.IsTextureImage then
    ImageTexture.TextureProperties.AnisotropicDegree:= FAnisotropicDegree;
end;

initialization
  RegisterSerializableComponent(TNyaScene, 'Nya Scene');
end.

