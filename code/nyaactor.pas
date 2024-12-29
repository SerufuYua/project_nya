unit NyaActor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CastleTransform, CastleClassUtils, CastleColors, X3DNodes, CastleScene;

type
  TNyaActor = class(TCastleTransform)
  protected
    FDesign: TCastleTransformDesign;
    FUrl: String;
    FActorName: String;
    FAutoAnimation: String;
    FAnimationSpeed: Single;
    FAnisotropicDegree: Single;
    FEmissionItself: Boolean;
    FEmissionColor: TCastleColorRGB;
    FEmissionColorPersistent: TCastleColorRGBPersistent;
    procedure SetUrl(const Value: String);
    function GetEmissionColorForPersistent: TCastleColorRGB;
    procedure SetEmissionColorForPersistent(const AValue: TCastleColorRGB);
    procedure SetAutoAnimation(const Value: String);
    procedure SetAnimationSpeed(value: Single);
    procedure SetAnisotropicDegree(value: Single);
    procedure SetEmissionItself(const value: Boolean);
    procedure SetEmissionColor(const value: TCastleColorRGB);
    procedure ApplyAutoAnimation;
    procedure ApplyAnimationSpeed;
    procedure ApplyAnisotropicDegree;
    procedure ApplyEmissionItself;
    procedure ApplyEmissionColor;
  protected
    procedure HandleNodeAnisotropic(sceneNode: TX3DNode);
    procedure HandleNodeEmissionItself(sceneNode: TX3DNode);
    procedure HandleNodeEmissionColor(sceneNode: TX3DNode);
  public
    const
      DefaultActorName = 'unknown';
      DefaultAutoAnimation = 'none';
      DefaultAnimationSpeed = 1.0;
      DefaultAnisotropicDegree = 0.0;
      DefaultEmissionItself = False;
      DefaultEmissionColor: TCastleColorRGB = (X: 0.0; Y: 0.0; Z: 0.0);

    constructor Create(AOwner: TComponent); override;
    function MainActor: TCastleScene; virtual;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    property EmissionColor: TCastleColorRGB read FEmissionColor write SetEmissionColor;
  published
    property ActorName: String read FActorName write FActorName;
    property AutoAnimation: String read FAutoAnimation write SetAutoAnimation;
    property AnimationSpeed: Single read FAnimationSpeed write SetAnimationSpeed
      {$ifdef FPC}default DefaultAnimationSpeed{$endif};
    property Url: String read FUrl write SetUrl;
    property AnisotropicDegree: Single read FAnisotropicDegree write SetAnisotropicDegree
      {$ifdef FPC}default DefaultAnisotropicDegree{$endif};
    property EmissionItself: Boolean read FEmissionItself write SetEmissionItself
      {$ifdef FPC}default DefaultEmissionItself{$endif};
    property EmissionColorPersistent: TCastleColorRGBPersistent read FEmissionColorPersistent;
  end;

implementation

uses
  CastleComponentSerialize, CastleUtils, NyaCastleUtils
  {$ifdef CASTLE_DESIGN_MODE}
  , PropEdits, CastlePropEdits, CastleSceneCore
  {$endif};

constructor TNyaActor.Create(AOwner: TComponent);
begin
  inherited;

  FDesign:= nil;
  FUrl:= '';
  FActorName:= DefaultActorName;
  FAutoAnimation:= DefaultAutoAnimation;
  FAnimationSpeed:= DefaultAnimationSpeed;
  FAnisotropicDegree:= DefaultAnisotropicDegree;
  FEmissionItself:= DefaultEmissionItself;

  { Persistent for EmissionColor }
  FEmissionColorPersistent:= TCastleColorRGBPersistent.Create(nil);
  FEmissionColorPersistent.SetSubComponent(true);
  FEmissionColorPersistent.InternalGetValue:= {$ifdef FPC}@{$endif}GetEmissionColorForPersistent;
  FEmissionColorPersistent.InternalSetValue:= {$ifdef FPC}@{$endif}SetEmissionColorForPersistent;
  FEmissionColorPersistent.InternalDefaultValue:= DefaultEmissionColor; // current value is default
end;

procedure TNyaActor.SetUrl(const value: String);
begin
  if (FUrl = value) then Exit;
  FUrl:= value;

  if NOT Assigned(FDesign) then
  begin
    FDesign:= TCastleTransformDesign.Create(Self);
    FDesign.SetTransient;
    Add(FDesign);
  end;

  FDesign.Url:= value;

  ApplyAnisotropicDegree;
  ApplyEmissionItself;
  ApplyEmissionColor;

  ApplyAutoAnimation;
  ApplyAnimationSpeed;
end;

procedure TNyaActor.SetAutoAnimation(const Value: String);
begin
  if (FAutoAnimation = Value) then Exit;
  FAutoAnimation:= Value;
  ApplyAutoAnimation;
end;

procedure TNyaActor.SetAnimationSpeed(value: Single);
begin
  if (FAnimationSpeed = value) then Exit;
  FAnimationSpeed:= value;
  ApplyAnimationSpeed;
end;

procedure TNyaActor.SetAnisotropicDegree(value: Single);
begin
  if (FAnisotropicDegree = value) then Exit;
  FAnisotropicDegree:= value;
  ApplyAnisotropicDegree;
end;

procedure TNyaActor.SetEmissionItself(const value: Boolean);
begin
  if (FEmissionItself = value) then Exit;
  FEmissionItself:= value;
  ApplyEmissionItself;
end;

procedure TNyaActor.SetEmissionColor(const value: TCastleColorRGB);
begin
  if TCastleColorRGB.Equals(FEmissionColor, value) then Exit;
  FEmissionColor:= value;
  ApplyEmissionColor;
end;

procedure TNyaActor.ApplyAutoAnimation;
var
  scene: TCastleScene;
begin
  if NOT Assigned(FDesign) then Exit;

  for scene in GetAllScenes(FDesign) do
    scene.AutoAnimation:= FAutoAnimation;
end;

procedure TNyaActor.ApplyAnimationSpeed;
var
  scene: TCastleScene;
begin
  if NOT Assigned(FDesign) then Exit;

  for scene in GetAllScenes(FDesign) do
    scene.TimePlayingSpeed:= FAnimationSpeed;
end;

procedure TNyaActor.ApplyAnisotropicDegree;
var
  scene: TCastleScene;
begin
  if NOT Assigned(FDesign) then Exit;

  for scene in GetAllScenes(FDesign) do
    if Assigned(scene.RootNode) then
      scene.RootNode.EnumerateNodes(TImageTextureNode,
                                    {$ifdef FPC}@{$endif}HandleNodeAnisotropic,
                                    false);
end;

procedure TNyaActor.ApplyEmissionItself;
var
  scene: TCastleScene;
begin
  if NOT Assigned(FDesign) then Exit;

  for scene in GetAllScenes(FDesign) do
    if Assigned(scene.RootNode) then
      scene.RootNode.EnumerateNodes(TPhysicalMaterialNode,
                                    {$ifdef FPC}@{$endif}HandleNodeEmissionItself,
                                    false);
end;

procedure TNyaActor.ApplyEmissionColor;
var
  scene: TCastleScene;
begin
  if NOT Assigned(FDesign) then Exit;

  for scene in GetAllScenes(FDesign) do
    if Assigned(scene.RootNode) then
      scene.RootNode.EnumerateNodes(TPhysicalMaterialNode,
                                    {$ifdef FPC}@{$endif}HandleNodeEmissionColor,
                                    false);
end;

procedure TNyaActor.HandleNodeAnisotropic(sceneNode: TX3DNode);
var
  ImageTexture: TImageTextureNode;
begin
  ImageTexture:= sceneNode as TImageTextureNode;

  if ImageTexture.IsTextureImage then
    ImageTexture.TextureProperties.AnisotropicDegree:= FAnisotropicDegree;
end;

procedure TNyaActor.HandleNodeEmissionItself(sceneNode: TX3DNode);
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

procedure TNyaActor.HandleNodeEmissionColor(sceneNode: TX3DNode);
var
  material: TPhysicalMaterialNode;
begin
  material:= sceneNode as TPhysicalMaterialNode;
  material.EmissiveColor:= FEmissionColor;
end;

function TNyaActor.GetEmissionColorForPersistent: TCastleColorRGB;
begin
  Result:= EmissionColor;
end;

procedure TNyaActor.SetEmissionColorForPersistent(const AValue: TCastleColorRGB);
begin
  EmissionColor:= AValue;
end;

function TNyaActor.MainActor: TCastleScene;
var
  scenes: TCastleScenes;
begin
  if NOT Assigned(FDesign) then Exit;

  { take only first TCastleScene }
  scenes:= GetAllScenes(FDesign);
  if (Length(scenes) > 0) then
    Result:= scenes[0]
  else
    Result:= nil;
end;

function TNyaActor.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'Url', 'AnisotropicDegree', 'EmissionItself', 'EmissionColorPersistent',
       'AutoAnimation', 'AnimationSpeed', 'ActorName'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

{$ifdef CASTLE_DESIGN_MODE}
type
  { Property editor to select an animation on TNyaSwitch }
  TNyaActorPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

function TNyaActorPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result:= [paMultiSelect, paValueList, paSortList, paRevertable];
end;

procedure TNyaActorPropertyEditor.GetValues(Proc: TGetStrProc);
var
  Nav: TNyaActor;
  Scene: TCastleSceneCore;
  S: String;
begin
  Proc('');
  Nav:= GetComponent(0) as TNyaActor;
  Scene:= Nav.MainActor;
  if Scene <> nil then
    for S in Scene.AnimationsList do
      Proc(S);
end;
{$endif}

initialization
  RegisterSerializableComponent(TNyaActor, 'Nya Actor');

  {$ifdef CASTLE_DESIGN_MODE}
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaActor, 'URL',
                         TTransformDesignURLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaActor, 'AutoAnimation',
                         TNyaActorPropertyEditor);
  {$endif}
end.

