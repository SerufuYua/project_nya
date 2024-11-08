unit MyCastleUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, CastleScene, CastleShapes, CastleTransform,
  CastleControls, CastleUIControls,
  X3DNodes;

type
  TItemCondition = record
    Name: String;
    Visible: boolean;
  end;

  TItemConditions = Array of TItemCondition;
  TCastleScenes = Array of TCastleScene;
  TShapeNodes = Array of TShapeNode;
  TUIRectangles = Array of TCastleRectangleControl;
  TUIImages = Array of TCastleImageControl;

procedure SetAnisotropicFiltering(const scene: TCastleScene;
                                  degree: Single = 16);
procedure SetEmission(const scene: TCastleScene;
                      r, g, b: Single; toSelf: Boolean);
function GetShapeNamesByNameStart(const scene: TCastleScene;
                                  const NameStartWith: String): TItemConditions;
function GetShapesByNameStart(const scene: TCastleScene;
                              const NameStartWith: String): TShapeNodes;
function GetAllScenes(const rootItem: TCastleTransform): TCastleScenes;
function GetSceneNamesByNameStart(const rootScene: TCastleTransformDesign;
                                  const NameStartWith: String): TItemConditions;
function GetAllUIRectangles(const rootItem: TCastleUserInterface): TUIRectangles;
function GetAllUIImages(const rootItem: TCastleUserInterface): TUIImages;

implementation

uses
  CastleComponentSerialize, CastleTextureImages, CastleColors,
  CastleVectors, CastleClassUtils, sysutils;

type
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
  FoundItems: TItemConditions;
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
  shape: TShapeNode;
begin
  nodeName:= Node.X3DName;
  if nodeName.StartsWith(NameStartPattern) then
  begin
    SetLength(FoundItems, Length(FoundItems) + 1);
    FoundItems[High(FoundItems)].Name:= nodeName;

    shape:= Node as TShapeNode;
    FoundItems[High(FoundItems)].Visible:= shape.Visible;
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
                                  const NameStartWith: String): TItemConditions;
var
  Node: TX3DRootNode;
begin
  if NOT Assigned(scene) then Exit;
  FoundItems:= [];
  NameStartPattern:= NameStartWith;

  Node:= scene.RootNode;
  Node.EnumerateNodes(TShapeNode,
    {$ifdef FPC}@{$endif} TNodeHandler {$ifdef FPC}(nil){$endif}.
    HandleShapeNamesByNameStart, false);

  Result:= FoundItems;
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

function GetAllScenes(const rootItem: TCastleTransform): TCastleScenes;
type
  TComponents = Array of TComponent;
var
  num, startSub, i, j, start: Integer;
  item: TComponent;
  items: TComponents;
begin
  Result:= [];
  start:= 0;
  items:= [rootItem];

  // collect all components
  while (start < Length(items)) do
  begin
    num:= Length(items);
    for i:= start to (num - 1) do
    begin
      startSub:= Length(items) - 1;

      j:= 0;
      for item in items[i] do
      begin
        j:= j + 1;
        SetLength(items, Length(items) + 1);
        items[startSub + j]:= item;

        // pick up target
        if (item is TCastleScene) then
        begin
          SetLength(Result, Length(Result) + 1);
          Result[Length(Result) - 1]:= item as TCastleScene;
        end;
      end;
    end;
    start:= num;
  end;
end;

function GetSceneNamesByNameStart(const rootScene: TCastleTransformDesign;
                                  const NameStartWith: String): TItemConditions;
var
  item: TCastleScene;
  itemCondition: TItemCondition;
begin
  Result:= [];

  for item in GetAllScenes(rootScene) do
  begin
    if item.Name.StartsWith(NameStartWith) then
    begin
      SetLength(Result, Length(Result) + 1);
      itemCondition.Name:= item.Name;
      itemCondition.Visible:= item.Visible;
      Result[Length(Result) - 1]:= itemCondition;
    end;
  end;
end;

function GetAllUIRectangles(const rootItem: TCastleUserInterface): TUIRectangles;
type
  TCastleUIs = Array of TCastleUserInterface;
var
  num, startSub, i, j, start: Integer;
  item: TCastleUserInterface;
  items: TCastleUIs;
begin
  Result:= [];
  start:= 0;
  items:= [rootItem];

  // collect all components
  while (start < Length(items)) do
  begin
    num:= Length(items);
    for i:= start to (num - 1) do
    begin
      startSub:= Length(items) - 1;

      j:= 0;
      for item in items[i] do
      begin
        j:= j + 1;
        SetLength(items, Length(items) + 1);
        items[startSub + j]:= item;

        // pick up target
        if (item is TCastleRectangleControl) then
        begin
          SetLength(Result, Length(Result) + 1);
          Result[Length(Result) - 1]:= item as TCastleRectangleControl;
        end;
      end;
    end;
    start:= num;
  end;
end;

function GetAllUIImages(const rootItem: TCastleUserInterface): TUIImages;
type
  TCastleUIs = Array of TCastleUserInterface;
var
  num, startSub, i, j, start: Integer;
  item: TCastleUserInterface;
  items: TCastleUIs;
begin
  Result:= [];
  start:= 0;
  items:= [rootItem];

  // collect all components
  while (start < Length(items)) do
  begin
    num:= Length(items);
    for i:= start to (num - 1) do
    begin
      startSub:= Length(items) - 1;

      j:= 0;
      for item in items[i] do
      begin
        j:= j + 1;
        SetLength(items, Length(items) + 1);
        items[startSub + j]:= item;

        // pick up target
        if (item is TCastleImageControl) then
        begin
          SetLength(Result, Length(Result) + 1);
          Result[Length(Result) - 1]:= item as TCastleImageControl;
        end;
      end;
    end;
    start:= num;
  end;
end;

end.

