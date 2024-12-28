unit NyaCastleUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, CastleScene, CastleShapes, CastleTransform,
  CastleControls, CastleUIControls,
  X3DNodes, CastleKeysMouse;

type
  TItemCondition = record
    Name: String;
    Visible: boolean;
  end;

  TItemConditions = Array of TItemCondition;
  TCastleScenes = Array of TCastleScene;
  TCastleBehaviors = Array of TCastleBehavior;
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
function GetKeyName(key: TKey): string;
function GetAllScenes(const rootItem: TCastleTransform): TCastleScenes;
function GetAllBehavior(const rootItem: TCastleTransform;
                        const BehaviorClass: TCastleBehaviorClass): TCastleBehaviors;
function GetSceneNamesByNameStart(const rootScene: TCastleTransformDesign;
                                  const NameStartWith: String): TItemConditions;
function GetAllUIRectangles(const rootItem: TCastleUserInterface): TUIRectangles;
function GetAllUIImages(const rootItem: TCastleUserInterface): TUIImages;

implementation

uses
  CastleComponentSerialize, CastleTextureImages, CastleColors,
  CastleVectors, Generics.Collections, sysutils, TypInfo;

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
  material: TPhysicalMaterialNode;
begin
  material:= Node as TPhysicalMaterialNode;

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
  condition: TItemCondition;
begin
  nodeName:= Node.X3DName;
  if nodeName.StartsWith(NameStartPattern) then
  begin
    shape:= Node as TShapeNode;
    condition.Name:= nodeName;
    condition.Visible:= shape.Visible;

    Insert(condition, FoundItems, 0);
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
    Insert(shape, FoundShapes, 0);
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
  Node.EnumerateNodes(TPhysicalMaterialNode,
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

function GetKeyName(key: TKey): string;
begin
  Result:= GetEnumName(TypeInfo(TKey), Ord(key));
  Delete(Result, 1, 3);
end;

function GetAllScenes(const rootItem: TCastleTransform): TCastleScenes;
type
  TItemsStack = {$ifdef FPC}specialize{$endif} TObjectStack<TComponent>;
var
  item, child: TComponent;
  items: TItemsStack;
begin
  Result:= [];
  items:= TItemsStack.Create(False);
  items.Push(rootItem);

  { iterate over all elements of tree }
  while (items.Count > 0) do
  begin
    item:= items.Pop;
    for child in item do
      items.Push(child);

    { pick up target }
    if (item is TCastleScene) then
      Insert((item as TCastleScene), Result, 0);
  end;

  FreeAndNil(items);
end;

function GetAllBehavior(const rootItem: TCastleTransform;
                        const BehaviorClass: TCastleBehaviorClass): TCastleBehaviors;
type
  TItemsStack = {$ifdef FPC}specialize{$endif} TObjectStack<TComponent>;
var
  item, child: TComponent;
  items: TItemsStack;
begin
  Result:= [];
  items:= TItemsStack.Create(False);
  items.Push(rootItem);

  { iterate over all elements of tree }
  while (items.Count > 0) do
  begin
    item:= items.Pop;
    for child in item do
      items.Push(child);

    { pick up target }
    if (item is BehaviorClass) then
      Insert((item as BehaviorClass), Result, 0);
  end;

  FreeAndNil(items);
end;

function GetSceneNamesByNameStart(const rootScene: TCastleTransformDesign;
                                  const NameStartWith: String): TItemConditions;
var
  item: TCastleScene;
  condition: TItemCondition;
begin
  Result:= [];

  for item in GetAllScenes(rootScene) do
  begin
    if item.Name.StartsWith(NameStartWith) then
    begin
      condition.Name:= item.Name;
      condition.Visible:= item.Visible;

      Insert(condition, Result, 0);
    end;
  end;
end;

function GetAllUIRectangles(const rootItem: TCastleUserInterface): TUIRectangles;
type
  TItemsStack = {$ifdef FPC}specialize{$endif} TObjectStack<TCastleUserInterface>;
var
  item, child: TCastleUserInterface;
  items: TItemsStack;
begin
  Result:= [];
  items:= TItemsStack.Create(False);
  items.Push(rootItem);

  { iterate over all elements of tree }
  while (items.Count > 0) do
  begin
    item:= items.Pop;
    for child in item do
      items.Push(child);

    { pick up target }
    if (item is TCastleRectangleControl) then
      Insert((item as TCastleRectangleControl), Result, 0);
  end;

    FreeAndNil(items);
end;

function GetAllUIImages(const rootItem: TCastleUserInterface): TUIImages;
type
  TItemsStack = {$ifdef FPC}specialize{$endif} TObjectStack<TCastleUserInterface>;
var
  item, child: TCastleUserInterface;
  items: TItemsStack;
begin
  Result:= [];
  items:= TItemsStack.Create(False);
  items.Push(rootItem);

  { iterate over all elements of tree }
  while (items.Count > 0) do
  begin
    item:= items.Pop;
    for child in item do
      items.Push(child);

    { pick up target }
    if (item is TCastleImageControl) then
      Insert((item as TCastleImageControl), Result, 0);
  end;

  FreeAndNil(items);
end;

end.

