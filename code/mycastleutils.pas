unit MyCastleUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, CastleScene, CastleShapes, CastleTransform, X3DNodes;

type
  TCastleScenes = Array of TCastleScene;
  TShapeNames = Array of String;
  TShapeNodes = Array of TShapeNode;

procedure SetAnisotropicFiltering(const scene: TCastleScene;
                                  degree: Single = 16);
procedure SetEmission(const scene: TCastleScene;
                      r, g, b: Single; toSelf: Boolean);
function GetShapeNamesByNameStart(const scene: TCastleScene;
                                  const NameStartWith: String): TShapeNames;
function GetShapesByNameStart(const scene: TCastleScene;
                              const NameStartWith: String): TShapeNodes;
function GetAllScenes(const scene: TCastleTransformDesign): TCastleScenes;

implementation

uses
  CastleComponentSerialize, CastleTextureImages, CastleColors,
  CastleVectors, CastleClassUtils, sysutils;

type
  TComponents = Array of TComponent;

  TNodeHandler = class
    class procedure HandleNodeAnisotropic(Node: TX3DNode);
    class procedure HandleNodeEmission(Node: TX3DNode);
    class procedure HandleShapeNamesByNameStart(Node: TX3DNode);
    class procedure HandleShapesByNameStart(Node: TX3DNode);
  end;

var
  AnisotropicDegree: Single;
  EmissionColor: TVector3;
  EmissionToSelf: Boolean;
  NameStartPattern: String;
  FoundShapeNames: TShapeNames;
  FoundShapes: TShapeNodes;

class procedure TNodeHandler.HandleNodeAnisotropic(Node: TX3DNode);
var
  ImageTexture: TImageTextureNode;
  TextureProperties: TTexturePropertiesNode;
begin
  ImageTexture:= Node as TImageTextureNode;

  if ImageTexture.IsTextureImage then
  begin
    TextureProperties:= TTexturePropertiesNode.Create;
    TextureProperties.AnisotropicDegree:= AnisotropicDegree;
    TextureProperties.MinificationFilter:= minDefault;
    TextureProperties.MagnificationFilter:= magDefault;
    TextureProperties.BoundaryModeS:= BoolRepeatToBoundaryMode[ImageTexture.RepeatS];
    TextureProperties.BoundaryModeT:= BoolRepeatToBoundaryMode[ImageTexture.RepeatT];
    ImageTexture.TextureProperties:= TextureProperties;
  end;
end;

class procedure TNodeHandler.HandleNodeEmission(Node: TX3DNode);
var
  appearance: TAppearanceNode;
  material: TPhysicalMaterialNode;
begin
  appearance:= Node as TAppearanceNode;
  material:= appearance.Material as TPhysicalMaterialNode;

  material.EmissiveColor:= EmissionColor;
  if EmissionToSelf then
  begin
    material.EmissiveTexture:= material.BaseTexture;
    material.EmissiveTextureMapping:= material.BaseTextureMapping;
  end else
    material.EmissiveTexture:= nil;
end;

class procedure TNodeHandler.HandleShapeNamesByNameStart(Node: TX3DNode);
var
  nodeName: String;
begin
  nodeName:= Node.X3DName;
  if nodeName.StartsWith(NameStartPattern) then
  begin
    SetLength(FoundShapeNames, Length(FoundShapeNames) + 1);
    FoundShapeNames[High(FoundShapeNames)]:= nodeName;
  end;
end;

class procedure TNodeHandler.HandleShapesByNameStart(Node: TX3DNode);
var
  nodeName: String;
  shape: TShapeNode;
begin
  nodeName:= Node.X3DName;
  if nodeName.StartsWith(NameStartPattern) then
  begin
    shape:= Node as TShapeNode;
    SetLength(FoundShapes, Length(FoundShapes) + 1);
    FoundShapes[High(FoundShapes)]:= shape;
  end;
end;


procedure SetAnisotropicFiltering(const scene: TCastleScene; degree: Single);
var
  Node: TX3DRootNode;
begin
  if NOT Assigned(scene) then Exit;

  AnisotropicDegree:= degree;
  Node:= scene.RootNode;
  Node.EnumerateNodes(TImageTextureNode,
    {$ifdef FPC}@{$endif} TNodeHandler {$ifdef FPC}(nil){$endif}.
    HandleNodeAnisotropic, false);
end;

procedure SetEmission(const scene: TCastleScene;
                      r, g, b: Single; toSelf: Boolean);
var
  Node: TX3DRootNode;
begin
  if NOT Assigned(scene) then Exit;

  EmissionColor:= Vector3(r, g, b);
  EmissionToSelf:= toSelf;
  Node:= scene.RootNode;
  Node.EnumerateNodes(TAppearanceNode,
    {$ifdef FPC}@{$endif} TNodeHandler {$ifdef FPC}(nil){$endif}.
    HandleNodeEmission, false);
end;

function GetShapeNamesByNameStart(const scene: TCastleScene;
                                  const NameStartWith: String): TShapeNames;
var
  Node: TX3DRootNode;
begin
  if NOT Assigned(scene) then Exit;
  FoundShapeNames:= [];
  NameStartPattern:= NameStartWith;

  Node:= scene.RootNode;
  Node.EnumerateNodes(TShapeNode,
    {$ifdef FPC}@{$endif} TNodeHandler {$ifdef FPC}(nil){$endif}.
    HandleShapeNamesByNameStart, false);

  Result:= FoundShapeNames;
end;

function GetShapesByNameStart(const scene: TCastleScene;
                              const NameStartWith: String): TShapeNodes;
var
  Node: TX3DRootNode;
begin
  if NOT Assigned(scene) then Exit;
  FoundShapes:= [];
  NameStartPattern:= NameStartWith;

  Node:= scene.RootNode;
  Node.EnumerateNodes(TShapeNode,
    {$ifdef FPC}@{$endif} TNodeHandler {$ifdef FPC}(nil){$endif}.
    HandleShapesByNameStart, false);

  Result:= FoundShapes;
end;


function GetAllScenes(const scene: TCastleTransformDesign): TCastleScenes;
var
  num, numSub, startSub, i, j, start: Integer;
  item: TComponent;
  items: TComponents;
begin
  Result:= [];
  start:= 0;
  items:= [scene];

  // collect all components
  while (start < Length(items)) do
  begin
    num:= Length(items);
    for i:= start to (num - 1) do
    begin
      numSub:= items[i].ComponentCount;
      startSub:= Length(items) - 1;
      SetLength(items, Length(items) + numSub);
      for j:= 0 to (numSub - 1) do
      begin
        item:= items[i].Components[j];
        items[startSub + j + 1]:= item;
      end;
    end;
    start:= num;
  end;

  // get only scenes from all components
  num:= Length(items);
  for i:= 0 to (num - 1) do
  begin
    if (CompareText(items[i].ClassName, TCastleScene.ClassName) = 0) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1]:= items[i] as TCastleScene;
    end;
  end;
end;

end.

