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
    FEmissionItself: Boolean;
    FEmissionColor: TCastleColorRGB;
    FEmissionColorPersistent: TCastleColorRGBPersistent;
    function GetEmissionColorForPersistent: TCastleColorRGB;
    procedure SetEmissionColorForPersistent(const AValue: TCastleColorRGB);
    procedure SetAnisotropicDegree(value: Single);
    procedure SetEmissionItself(const value: Boolean);
    procedure SetEmissionColor(const value: TCastleColorRGB);
    procedure ApplyAnisotropicDegree;
    procedure ApplyEmissionItself;
    procedure ApplyEmissionColor;
  protected
    procedure HandleNodeAnisotropic(sceneNode: TX3DNode);
    procedure HandleNodeEmissionItself(sceneNode: TX3DNode);
    procedure HandleNodeEmissionColor(sceneNode: TX3DNode);
  public
    const
      DefaultAnisotropicDegree = 0.0;
      DefaultEmissionItself = False;
      DefaultEmissionColor: TCastleColorRGB = (X: 0.0; Y: 0.0; Z: 0.0);

    constructor Create(AOwner: TComponent); override;
{    procedure DoWhenAllReady; override;   }
    function PropertySections(const PropertyName: String): TPropertySections; override;

    property EmissionColor: TCastleColorRGB read FEmissionColor write SetEmissionColor;
  published
    property AnisotropicDegree: Single read FAnisotropicDegree write SetAnisotropicDegree
      {$ifdef FPC}default DefaultAnisotropicDegree{$endif};
    property EmissionItself: Boolean read FEmissionItself write SetEmissionItself
      {$ifdef FPC}default DefaultEmissionItself{$endif};
    property EmissionColorPersistent: TCastleColorRGBPersistent read FEmissionColorPersistent;
  end;

implementation

uses
  CastleComponentSerialize, CastleTextureImages, CastleUtils;

constructor TNyaScene.Create(AOwner: TComponent);
begin
  inherited;

  FAnisotropicDegree:= DefaultAnisotropicDegree;
  FEmissionItself:= DefaultEmissionItself;

  { Persistent for EmissionColor }
  FEmissionColorPersistent:= TCastleColorRGBPersistent.Create(nil);
  FEmissionColorPersistent.SetSubComponent(true);
  FEmissionColorPersistent.InternalGetValue:= {$ifdef FPC}@{$endif}GetEmissionColorForPersistent;
  FEmissionColorPersistent.InternalSetValue:= {$ifdef FPC}@{$endif}SetEmissionColorForPersistent;
  FEmissionColorPersistent.InternalDefaultValue:= DefaultEmissionColor; // current value is default
end;

{procedure TNyaScene.DoWhenAllReady;
begin
  inherited;

  ApplyAnisotropicDegree;
  ApplyEmissionItself;
  ApplyEmissionColor;
end;      }

procedure TNyaScene.SetAnisotropicDegree(value: Single);
begin
  if (FAnisotropicDegree = value) then Exit;
  FAnisotropicDegree:= value;
  ApplyAnisotropicDegree;
end;

procedure TNyaScene.SetEmissionItself(const value: Boolean);
begin
  if (FEmissionItself = value) then Exit;
  FEmissionItself:= value;
  ApplyEmissionItself;
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

procedure TNyaScene.ApplyEmissionItself;
begin
  if Assigned(RootNode) then
    RootNode.EnumerateNodes(TPhysicalMaterialNode,
                            {$ifdef FPC}@{$endif}HandleNodeEmissionItself,
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

procedure TNyaScene.HandleNodeEmissionItself(sceneNode: TX3DNode);
var
  material: TPhysicalMaterialNode;
begin
  material:= sceneNode as TPhysicalMaterialNode;

  if FEmissionItself then
  begin
    material.EmissiveTexture:= material.BaseTexture;
    material.EmissiveTextureMapping:= material.BaseTextureMapping;
  end else
    material.EmissiveTexture:= nil;
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
       'AnisotropicDegree', 'EmissionItself', 'EmissionColorPersistent'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TNyaScene, 'Nya Scene');
end.


