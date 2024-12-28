unit NyaScene;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CastleScene, X3DNodes, CastleSceneCore, CastleShapes, CastleClassUtils,
  CastleColors;

type
  TNyaScene = class(TCastleScene)
  protected
    FAnisotropicDegree: Single;
    FEmissionColor: TCastleColorRGB;
    FEmissionColorPersistent: TCastleColorRGBPersistent;
    function GetEmissionColorForPersistent: TCastleColorRGB;
    procedure SetEmissionColorForPersistent(const AValue: TCastleColorRGB);
    procedure SetAnisotropicDegree(value: Single);
    procedure SetEmissionColor(const value: TCastleColorRGB);
    procedure ApplyAnisotropicDegree;
    procedure ApplyEmissionColor;
  protected
    procedure HandleNodeAnisotropic(sceneNode: TX3DNode);
    procedure HandleNodeEmissionColor(sceneNode: TX3DNode);
  public
    const
      DefaultAnisotropicDegree = 0.0;
      DefaultEmissionColor: TCastleColorRGB = (X: 0.0; Y: 0.0; Z: 0.0);

    constructor Create(AOwner: TComponent); override;
    procedure DoGeometryChanged(const Change: TGeometryChange;
                                LocalGeometryShape: TShape); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    property EmissionColor: TCastleColorRGB read FEmissionColor write SetEmissionColor;
  published
    property AnisotropicDegree: Single read FAnisotropicDegree write SetAnisotropicDegree
      {$ifdef FPC}default DefaultAnisotropicDegree{$endif};
    property EmissionColorPersistent: TCastleColorRGBPersistent read FEmissionColorPersistent;
  end;

implementation

uses
  CastleComponentSerialize, CastleTextureImages, CastleUtils;

constructor TNyaScene.Create(AOwner: TComponent);
begin
  inherited;

  FAnisotropicDegree:= DefaultAnisotropicDegree;

  { Persistent for EmissionColor }
  FEmissionColorPersistent:= TCastleColorRGBPersistent.Create(nil);
  FEmissionColorPersistent.SetSubComponent(true);
  FEmissionColorPersistent.InternalGetValue:= {$ifdef FPC}@{$endif}GetEmissionColorForPersistent;
  FEmissionColorPersistent.InternalSetValue:= {$ifdef FPC}@{$endif}SetEmissionColorForPersistent;
  FEmissionColorPersistent.InternalDefaultValue:= DefaultEmissionColor; // current value is default
end;

procedure TNyaScene.DoGeometryChanged(const Change: TGeometryChange;
                                      LocalGeometryShape: TShape);
begin
  inherited;

  ApplyAnisotropicDegree;
  ApplyEmissionColor;
end;

procedure TNyaScene.SetAnisotropicDegree(value: Single);
begin
  if (FAnisotropicDegree = value) then Exit;
  FAnisotropicDegree:= value;
  ApplyAnisotropicDegree;
end;

procedure TNyaScene.SetEmissionColor(const value: TCastleColorRGB);
begin
  if TCastleColorRGB.Equals(FEmissionColor, value) then Exit;
  FEmissionColor:= value;
  ApplyEmissionColor;
end;

procedure TNyaScene.ApplyAnisotropicDegree;
begin
  if Assigned(RootNode) then
    RootNode.EnumerateNodes(TImageTextureNode,
                            {$ifdef FPC}@{$endif}HandleNodeAnisotropic,
                            false);
end;

procedure TNyaScene.ApplyEmissionColor;
begin
  if Assigned(RootNode) then
    RootNode.EnumerateNodes(TPhysicalMaterialNode,
                            {$ifdef FPC}@{$endif}HandleNodeEmissionColor,
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

procedure TNyaScene.HandleNodeEmissionColor(sceneNode: TX3DNode);
var
  material: TPhysicalMaterialNode;
begin
  material:= sceneNode as TPhysicalMaterialNode;
  material.EmissiveColor:= FEmissionColor;
end;

function TNyaScene.GetEmissionColorForPersistent: TCastleColorRGB;
begin
  Result:= EmissionColor;
end;

procedure TNyaScene.SetEmissionColorForPersistent(const AValue: TCastleColorRGB);
begin
  EmissionColor:= AValue;
end;

function TNyaScene.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'AnisotropicDegree', 'EmissionColorPersistent'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TNyaScene, 'Nya Scene');
end.

