unit NyaActor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CastleTransform, CastleClassUtils, CastleColors, X3DNodes, CastleScene,
  NyaCastleUtils, CastleSceneCore, CastleVectors, NyaMath;

type
  TNyaActorPerson = class(TCastleComponent)
  protected
    FActorName: String;
    FPersonalColor: TCastleColorRGB;
    FPersonalColorPersistent: TCastleColorRGBPersistent;
    function GetPersonalColorForPersistent: TCastleColorRGB;
    procedure SetPersonalColorForPersistent(const AValue: TCastleColorRGB);
    procedure SetActorName(const value: String); virtual;
  public
    const
      DefaultActorName = 'unknown';
      DefaultPersonalColor: TCastleColorRGB = (X: 1.0; Y: 1.0; Z: 1.0);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    property PersonalColor: TCastleColorRGB read FPersonalColor write FPersonalColor;
  published
    property ActorName: String read FActorName write SetActorName;
    property PersonalColorPersistent: TCastleColorRGBPersistent read FPersonalColorPersistent;
  end;

  TNyaActor = class(TCastleTransform)
  protected
    FDesign: TCastleTransformDesign;
    FAllScenes: TCastleScenes;
    FAnimatedScenes: TCastleScenes;
    FMainScene: TCastleScene;
    FMainSceneName: String;
    FAnimationsList: TStrings;
    procedure UpdateMainScene;
    procedure UpdateAnimationsList;
  protected
    FUrl, FUrlPerson: String;
    FActorName: String;
    FPersonalColor: TCastleColorRGB;
    FAutoAnimation: String;
    FAnimationSpeed: Single;
    FAnisotropicDegree: Single;
    FDefaultAnimationTransition: Single;
    FAnimateChildActors: Boolean;
    FLightning: Boolean;
    FForwardVelocity, FForwardShift: Single;
    FVelocityNoiseSuppressor: TNoiseSuppressor;
    FLastPos: TVector3;
    FEmissionItself: Boolean;
    FEmissionColor: TCastleColorRGB;
    FEmissionColorPersistent: TCastleColorRGBPersistent;
    procedure SetUrl(const value: String); virtual;
    procedure SetUrlPerson(const value: String); virtual;
    function GetEmissionColorForPersistent: TCastleColorRGB;
    procedure SetEmissionColorForPersistent(const AValue: TCastleColorRGB);
    procedure SetMainScene(const value: String);
    procedure SetAutoAnimation(const value: String);
    procedure SetAnimationSpeed(value: Single);
    procedure SetAnisotropicDegree(value: Single);
    procedure SetLightning(enable: Boolean);
    procedure SetEmissionItself(const value: Boolean);
    procedure SetEmissionColor(const value: TCastleColorRGB);
    procedure SetDefaultAnimationTransition(value: Single);
    function GetVelocityNoiseSuppressorCount: Integer;
    procedure SetVelocityNoiseSuppressorCount(value: Integer);
    procedure ApplyAutoAnimation; virtual;
    procedure ApplyAnimationSpeed;
    procedure ApplyAnisotropicDegree;
    procedure ApplyLightning;
    procedure ApplyEmissionItself;
    procedure ApplyEmissionColor;
    procedure ApplyDefaultAnimationTransition;
  protected
    procedure HandleNodeAnisotropic(sceneNode: TX3DNode);
    procedure HandleNodeEmissionItself(sceneNode: TX3DNode);
    procedure HandleNodeEmissionColor(sceneNode: TX3DNode);
  public
    const
      DefaultAutoAnimation = 'none';
      DefaultMainSceneName = 'none';
      DefaultAnimationSpeed = 1.0;
      DefaultAnisotropicDegree = 1.0;
      DefaultLightning = True;
      DefaultEmissionItself = False;
      DefaultEmissionColor: TCastleColorRGB = (X: 0.0; Y: 0.0; Z: 0.0);
      DefaultVelocityNoiseSuppressorCount = 8;
      DefaultDefaultAnimationTransition = 0.0;
      DefaultAnimateChildActors = False;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    procedure PlayAnimation(const Parameters: TPlayAnimationParameters); virtual;
    procedure StopAnimation(const disableStopNotification: Boolean = False);
    function Cameras: TCastleCameras;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    property ActorName: String read FActorName;
    property AllScenes: TCastleScenes read FAllScenes;
    property MainScene: TCastleScene read FMainScene;
    property AnimationsList: TStrings read FAnimationsList;
    property EmissionColor: TCastleColorRGB read FEmissionColor write SetEmissionColor;
    property PersonalColor: TCastleColorRGB read FPersonalColor;
    property ForwardVelocity: Single read FForwardVelocity;
    property ForwardShift: Single read FForwardShift;
  published
    property MainSceneName: String read FMainSceneName write SetMainScene;
    property AutoAnimation: String read FAutoAnimation write SetAutoAnimation;
    property AnimationSpeed: Single read FAnimationSpeed write SetAnimationSpeed
             {$ifdef FPC}default DefaultAnimationSpeed{$endif};
    property Url: String read FUrl write SetUrl;
    property UrlPerson: String read FUrlPerson write SetUrlPerson;
    property AnisotropicDegree: Single read FAnisotropicDegree write SetAnisotropicDegree
             {$ifdef FPC}default DefaultAnisotropicDegree{$endif};
    property Lightning: Boolean read FLightning write SetLightning
             {$ifdef FPC}default DefaultLightning{$endif};
    property EmissionItself: Boolean read FEmissionItself write SetEmissionItself
             {$ifdef FPC}default DefaultEmissionItself{$endif};
    property EmissionColorPersistent: TCastleColorRGBPersistent read FEmissionColorPersistent;
    property VelocityNoiseSuppressorCount: Integer read GetVelocityNoiseSuppressorCount write SetVelocityNoiseSuppressorCount
             {$ifdef FPC}default DefaultVelocityNoiseSuppressorCount{$endif};
    property DefaultAnimationTransition: Single
             read FDefaultAnimationTransition write SetDefaultAnimationTransition
             {$ifdef FPC}default DefaultDefaultAnimationTransition{$endif};
    property AnimateChildActors: Boolean read FAnimateChildActors write FAnimateChildActors
             {$ifdef FPC}default DefaultAnimateChildActors{$endif};
  end;

implementation

uses
  CastleComponentSerialize, CastleUtils, NyaVectorMath
  {$ifdef CASTLE_DESIGN_MODE}
  , PropEdits, CastlePropEdits
  {$endif};

{ ========= ------------------------------------------------------------------ }
{ TNyaActorPerson ------------------------------------------------------------ }
{ ========= ------------------------------------------------------------------ }

constructor TNyaActorPerson.Create(AOwner: TComponent);
begin
  inherited;

  FActorName:= DefaultActorName;
  FPersonalColor:= DefaultPersonalColor;

  { Persistent for PersonalColor }
  FPersonalColorPersistent:= TCastleColorRGBPersistent.Create(nil);
  FPersonalColorPersistent.SetSubComponent(true);
  FPersonalColorPersistent.InternalGetValue:= {$ifdef FPC}@{$endif}GetPersonalColorForPersistent;
  FPersonalColorPersistent.InternalSetValue:= {$ifdef FPC}@{$endif}SetPersonalColorForPersistent;
  FPersonalColorPersistent.InternalDefaultValue:= PersonalColor; // current value is default
end;

destructor TNyaActorPerson.Destroy;
begin
  FreeAndNil(FPersonalColorPersistent);
  inherited;
end;

procedure TNyaActorPerson.SetActorName(const value: String);
begin
  if (FActorName = value) then exit;
  FActorName:= value;
end;

function TNyaActorPerson.GetPersonalColorForPersistent: TCastleColorRGB;
begin
  Result:= PersonalColor;
end;

procedure TNyaActorPerson.SetPersonalColorForPersistent(const AValue: TCastleColorRGB);
begin
  PersonalColor:= AValue;
end;

function TNyaActorPerson.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'ActorName', 'PersonalColorPersistent'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

{ ========= ------------------------------------------------------------------ }
{ TNyaActor ------------------------------------------------------------------ }
{ ========= ------------------------------------------------------------------ }

constructor TNyaActor.Create(AOwner: TComponent);
begin
  inherited;

  FDesign:= nil;
  FUrl:= '';
  FUrlPerson:= '';
  FAutoAnimation:= DefaultAutoAnimation;
  FMainSceneName:= DefaultMainSceneName;
  FAnimationSpeed:= DefaultAnimationSpeed;
  FAnisotropicDegree:= DefaultAnisotropicDegree;
  FLightning:= DefaultLightning;
  FEmissionItself:= DefaultEmissionItself;
  FAnimationsList:= TStringList.Create;
  FDefaultAnimationTransition:= DefaultDefaultAnimationTransition;
  FAnimateChildActors:= DefaultAnimateChildActors;

  FPersonalColor:= TNyaActorPerson.DefaultPersonalColor;
  FActorName:= TNyaActorPerson.DefaultActorName;

  FEmissionColor:= DefaultEmissionColor;

  FForwardShift:= 0.0;
  FForwardVelocity:= 0.0;

  FLastPos:= Translation;
  FVelocityNoiseSuppressor:= TNoiseSuppressor.Create;
  FVelocityNoiseSuppressor.CountLimit:= DefaultVelocityNoiseSuppressorCount;

  { Persistent for EmissionColor }
  FEmissionColorPersistent:= TCastleColorRGBPersistent.Create(nil);
  FEmissionColorPersistent.SetSubComponent(true);
  FEmissionColorPersistent.InternalGetValue:= {$ifdef FPC}@{$endif}GetEmissionColorForPersistent;
  FEmissionColorPersistent.InternalSetValue:= {$ifdef FPC}@{$endif}SetEmissionColorForPersistent;
  FEmissionColorPersistent.InternalDefaultValue:= EmissionColor; // current value is default
end;

destructor TNyaActor.Destroy;
begin
  FreeAndNil(FVelocityNoiseSuppressor);
  FreeAndNil(FEmissionColorPersistent);
  FreeAndNil(FAnimationsList);
  inherited;
end;

procedure TNyaActor.Update(const SecondsPassed: Single;
                           var RemoveMe: TRemoveType);
var
  shift: TVector3;
begin
  inherited;

  { calculate real Velocity from Avatar Hierarchy }
  shift:= Translation - FLastPos;
  FForwardShift:= ProjectionVectorAtoBLength(shift, Direction);
  if (SecondsPassed > 0.0) then
    FForwardVelocity:= FForwardShift / SecondsPassed;
  FLastPos:= Translation;

  FVelocityNoiseSuppressor.Update(FForwardVelocity);
  FForwardVelocity:= FVelocityNoiseSuppressor.Value;
end;

procedure TNyaActor.PlayAnimation(const Parameters: TPlayAnimationParameters);
var
  scene: TCastleScene;
  noEventParam: TPlayAnimationParameters;
  child: TComponent;
begin
  noEventParam:= TPlayAnimationParameters.Create;
  noEventParam.Name:= Parameters.Name;
  noEventParam.Loop:= Parameters.Loop;
  noEventParam.Forward:= Parameters.Forward;
  noEventParam.TransitionDuration:= Parameters.TransitionDuration;
  noEventParam.InitialTime:= Parameters.InitialTime;

  for scene in FAnimatedScenes do
  begin
    if (scene = FMainScene) then
      scene.PlayAnimation(Parameters)
    else
      scene.PlayAnimation(noEventParam)
  end;

  if AnimateChildActors then
    for child in self do
      if (child is TNyaActor) then
        (child as TNyaActor).PlayAnimation(noEventParam);

  FreeAndNil(noEventParam);
end;

procedure TNyaActor.StopAnimation(const disableStopNotification: Boolean);
var
  scene: TCastleScene;
  child: TComponent;
begin
  for scene in FAnimatedScenes do
    scene.StopAnimation(disableStopNotification);

  if AnimateChildActors then
    for child in self do
      if (child is TNyaActor) then
        (child as TNyaActor).StopAnimation(disableStopNotification);
end;

function TNyaActor.Cameras: TCastleCameras;
begin
  Result:= GetAllCameras(FDesign);
end;

procedure TNyaActor.UpdateMainScene;
var
  scene: TCastleScene;
begin
  { take first Scene as Main }
  if (Length(FAllScenes) > 0) then
    FMainScene:= FAllScenes[0]
  else
    FMainScene:= nil;

  { try to find user Main Scene }
  for scene in FAllScenes do
  begin
    if (scene.Name = FMainSceneName) then
    begin
      FMainScene:= scene;
      Break;
    end;
  end;

  UpdateAnimationsList;
end;

procedure TNyaActor.UpdateAnimationsList;
var
  animationName: String;
begin
  { fill animations list }
  FAnimationsList.Clear;
  if Assigned(FMainScene) then
  begin
    for animationName in FMainScene.AnimationsList do
      FAnimationsList.Add(animationName);
  end;
end;

procedure TNyaActor.SetUrl(const value: String);
var
  buff: String;
  scene: TCastleScene;
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

  { prepare Scenes list }
  FAllScenes:= GetAllScenes(FDesign);

  { prepare Animated Scenes list }
  FAnimatedScenes:= [];
  for scene in FAllScenes do
    if (scene.AnimationsList.Count > 0) then
      System.Insert(scene, FAnimatedScenes, 0);

  UpdateMainScene;

  { apply all settings }
  ApplyAnisotropicDegree;
  ApplyLightning;
  ApplyEmissionItself;
  ApplyEmissionColor;
  ApplyDefaultAnimationTransition;

  { restore animation }
  buff:= FAutoAnimation;
  AutoAnimation:= DefaultAutoAnimation;
  AutoAnimation:= buff;

  ApplyAnimationSpeed;
end;

procedure TNyaActor.SetUrlPerson(const value: String);
var
  personOwner: TComponent;
  person: TNyaActorPerson;
begin
  FUrlPerson:= value;
  FPersonalColor:= TNyaActorPerson.DefaultPersonalColor;
  FActorName:= TNyaActorPerson.DefaultActorName;

  personOwner:= TComponent.Create(nil);
  try
    person:= ComponentLoad(value, personOwner) as TNyaActorPerson;
    FPersonalColor:= person.PersonalColor;
    FActorName:= person.ActorName;
  finally
    FreeAndNil(personOwner)
  end;
end;

procedure TNyaActor.SetMainScene(const value: String);
begin
  if (FMainSceneName <> value) then
  begin
    FMainSceneName:= value;
    UpdateMainScene;
  end;
end;

procedure TNyaActor.SetAutoAnimation(const value: String);
begin
  if (FAutoAnimation = value) then Exit;
  FAutoAnimation:= value;
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

  { #note : Anisotropic Degree MUST be > 0.0 }
  if (value < 1.0) then
    value:= 1.0;

  FAnisotropicDegree:= value;
  ApplyAnisotropicDegree;
end;

procedure TNyaActor.SetLightning(enable: Boolean);
begin
  if (FLightning = enable) then Exit;
  FLightning:= enable;
  ApplyLightning;
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

procedure TNyaActor.SetDefaultAnimationTransition(value: Single);
begin
  if (FDefaultAnimationTransition = value) then Exit;
  FDefaultAnimationTransition:= value;

  ApplyDefaultAnimationTransition;
end;

function TNyaActor.GetVelocityNoiseSuppressorCount: Integer;
begin
  Result:= FVelocityNoiseSuppressor.CountLimit;
end;

procedure TNyaActor.SetVelocityNoiseSuppressorCount(value: Integer);
begin
  FVelocityNoiseSuppressor.CountLimit:= value;
end;

procedure TNyaActor.ApplyAutoAnimation;
var
  scene: TCastleScene;
  child: TComponent;
  childActor: TNyaActor;
begin
  for scene in FAnimatedScenes do
    scene.AutoAnimation:= FAutoAnimation;

  if AnimateChildActors then
    for child in self do
      if (child is TNyaActor) then
      begin
        childActor:= (child as TNyaActor);
        childActor.AutoAnimation:= FAutoAnimation;
        childActor.DefaultAnimationTransition:= DefaultAnimationTransition;
      end;
end;

procedure TNyaActor.ApplyAnimationSpeed;
var
  scene: TCastleScene;
  child: TComponent;
begin
  for scene in FAnimatedScenes do
    scene.TimePlayingSpeed:= FAnimationSpeed;

  if AnimateChildActors then
    for child in self do
      if (child is TNyaActor) then
        (child as TNyaActor).AnimationSpeed:= FAnimationSpeed;
end;

procedure TNyaActor.ApplyAnisotropicDegree;
var
  scene: TCastleScene;
begin
  for scene in FAllScenes do
    if Assigned(scene.RootNode) then
      scene.RootNode.EnumerateNodes(TImageTextureNode,
                                    {$ifdef FPC}@{$endif}HandleNodeAnisotropic,
                                    false);
end;

procedure TNyaActor.ApplyLightning;
var
  scene: TCastleScene;
begin
  for scene in FAllScenes do
    scene.RenderOptions.Lighting:= FLightning;
end;

procedure TNyaActor.ApplyEmissionItself;
var
  scene: TCastleScene;
begin
  for scene in FAllScenes do
    if (Assigned(scene.RootNode) AND (scene.Tag <> -1)) then
      scene.RootNode.EnumerateNodes(TPhysicalMaterialNode,
                                    {$ifdef FPC}@{$endif}HandleNodeEmissionItself,
                                    false);
end;

procedure TNyaActor.ApplyEmissionColor;
var
  scene: TCastleScene;
begin
  for scene in FAllScenes do
    if (Assigned(scene.RootNode) AND (scene.Tag <> -1)) then
      scene.RootNode.EnumerateNodes(TPhysicalMaterialNode,
                                    {$ifdef FPC}@{$endif}HandleNodeEmissionColor,
                                    false);
end;

procedure TNyaActor.ApplyDefaultAnimationTransition;
var
  scene: TCastleScene;
  child: TComponent;
begin
  for scene in FAllScenes do
    scene.DefaultAnimationTransition:= FDefaultAnimationTransition;

  if AnimateChildActors then
    for child in self do
      if (child is TNyaActor) then
        (child as TNyaActor).DefaultAnimationTransition:= FDefaultAnimationTransition;
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

function TNyaActor.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'Url', 'UrlPerson', 'AnisotropicDegree', 'EmissionItself',
       'EmissionColorPersistent', 'AutoAnimation', 'AnimationSpeed',
       'ActorName', 'Lightning', 'MainSceneName', 'DefaultAnimationTransition',
       'VelocityNoiseSuppressorCount', 'AnimateChildActors'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

{$ifdef CASTLE_DESIGN_MODE}
type
  { Property editor to select Animation }
  TNyaActorPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { Property editor to select MainScene }
  TNyaActorMainSceneEditor = class(TStringPropertyEditor)
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
  S: String;
begin
  Proc('');
  Nav:= GetComponent(0) as TNyaActor;
  for S in Nav.AnimationsList do
    Proc(S);
end;



function TNyaActorMainSceneEditor.GetAttributes: TPropertyAttributes;
begin
  Result:= [paMultiSelect, paValueList, paSortList, paRevertable];
end;

procedure TNyaActorMainSceneEditor.GetValues(Proc: TGetStrProc);
var
  Nav: TNyaActor;
  S: TCastleScene;
begin
  Proc('');
  Nav:= GetComponent(0) as TNyaActor;
  for S in Nav.AllScenes do
    Proc(S.Name);
end;
{$endif}

initialization
  RegisterSerializableComponent(TNyaActor, 'Nya Actor');
  RegisterSerializableComponent(TNyaActorPerson, 'Nya Actor Personality');

  {$ifdef CASTLE_DESIGN_MODE}
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaActor, 'Url',
                         TTransformDesignURLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaActor, 'UrlPerson',
                         TAnyDesignUrlPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaActor, 'AutoAnimation',
                         TNyaActorPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaActor, 'MainSceneName',
                         TNyaActorMainSceneEditor);
  {$endif}
end.

