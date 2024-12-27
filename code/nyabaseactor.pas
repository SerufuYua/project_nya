unit NyaBaseActor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleSceneCore, CastleTransform, CastleVectors,
  CastleColors, CharaDress, CastleClassUtils, CastleScene;

type
  TNyaBaseActor = class(TCastleTransformDesign)
  protected
    FActorName: String;
    FAutoAnimation: String;
    FSpeed: Single;
    FPleasure: Single;
    FTension: Single;
    FSelfEmission: Single;
    FAnisotropicDegree: Single;
    FLightning: Boolean;
    FPersonalColor: TCastleColorRGB;
    FPersonalColorPersistent: TCastleColorRGBPersistent;
    function GetPersonalColorForPersistent: TCastleColorRGB;
    procedure SetPersonalColorForPersistent(const AValue: TCastleColorRGB);
    procedure SetSpeed(value: Single); virtual; abstract;
    procedure SetAutoAnimation(const Value: String); virtual; abstract;
    procedure SetLightning(enable: Boolean);
    procedure SetSelfEmission(value: Single);
    procedure SetAnisotropicDegree(value: Single);
  public
    const
      DefaultActorName = 'unknown';
      DefaultAutoAnimation = 'none';
      DefaultLightning = True;
      DefaultSpeed = 1.0;
      DefaultSelfEmission = 0.0;
      DefaultAnisotropicDegree = 0.0;
      DefaultPersonalColor: TCastleColorRGB = (X: 0.0; Y: 0.0; Z: 0.0);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PrepareResources(const Options: TPrepareResourcesOptions;
                               const Params: TPrepareParams); override;
    procedure PlayAnimation(const animationName: String; loop: boolean = true); virtual; abstract;
    procedure PlayAnimation(const Parameters: TPlayAnimationParameters); virtual; abstract;
    procedure StopAnimation(const DisableStopNotification: Boolean = false); virtual; abstract;
    function MainActor: TCastleScene; virtual; abstract;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    property PersonalColor: TCastleColorRGB read FPersonalColor write FPersonalColor;
  published
    property AutoAnimation: String read FAutoAnimation write SetAutoAnimation;
    property ActorName: String read FActorName write FActorName;
    property Speed: Single read FSpeed write SetSpeed
      {$ifdef FPC}default DefaultSpeed{$endif};
    property Lightning: Boolean read FLightning write SetLightning;
    property SelfEmission: Single read FSelfEmission write SetSelfEmission
      {$ifdef FPC}default DefaultSelfEmission{$endif};
    property AnisotropicDegree: Single read FAnisotropicDegree write SetAnisotropicDegree
      {$ifdef FPC}default DefaultAnisotropicDegree{$endif};
    property PersonalColorPersistent: TCastleColorRGBPersistent read FPersonalColorPersistent;
  end;

implementation

uses
  CastleUtils, NyaCastleUtils
  {$ifdef CASTLE_DESIGN_MODE}
  , PropEdits, ComponentEditors, CastlePropEdits
  {$endif};

constructor TNyaBaseActor.Create(AOwner: TComponent);
begin
  inherited;

  FActorName:= DefaultActorName;
  FAutoAnimation:= DefaultAutoAnimation;
  FSpeed:= DefaultSpeed;
  FLightning:= DefaultLightning;
  FSelfEmission:= DefaultSelfEmission;
  FAnisotropicDegree:= DefaultAnisotropicDegree;

  { Persistent for PersonalColor }
  FPersonalColorPersistent:= TCastleColorRGBPersistent.Create(nil);
  FPersonalColorPersistent.SetSubComponent(true);
  FPersonalColorPersistent.InternalGetValue:= {$ifdef FPC}@{$endif}GetPersonalColorForPersistent;
  FPersonalColorPersistent.InternalSetValue:= {$ifdef FPC}@{$endif}SetPersonalColorForPersistent;
  FPersonalColorPersistent.InternalDefaultValue:= DefaultPersonalColor; // current value is default
end;

destructor TNyaBaseActor.Destroy;
begin
  FreeAndNil(FPersonalColorPersistent);
  inherited;
end;

procedure TNyaBaseActor.PrepareResources(const Options: TPrepareResourcesOptions;
                                         const Params: TPrepareParams);
var
  animBuff: String;
  value: Single;
  enable: Boolean;
begin
  inherited;

  { SelfEmission: Single; }
  value:= FSelfEmission;
  FSelfEmission:= value - 1.0;
  SelfEmission:= value;

  { AnisotropicDegree: Single; }
  value:= FAnisotropicDegree;
  FAnisotropicDegree:= value - 1.0;
  AnisotropicDegree:= value;

  { Speed: Single; }
  value:= FSpeed;
  FSpeed:= value - 1.0;
  Speed:= value;

  { Lightning: Boolean; }
  enable:= FLightning;
  FLightning:= NOT enable;
  Lightning:= enable;

  { AutoAnimation: String; }
  { #note : set animation need to be placed after SelfEmission }
  animBuff:= FAutoAnimation;
  AutoAnimation:= 'none'; { NOT FAutoAnimation }
  AutoAnimation:= animBuff;
end;

procedure TNyaBaseActor.SetLightning(enable: Boolean);
var
  scene: TCastleScene;
begin
  if (FLightning = enable) then Exit;
  FLightning:= enable;

  for scene in GetAllScenes(self) do
    scene.RenderOptions.Lighting:= enable;
end;

procedure TNyaBaseActor.SetSelfEmission(value: Single);
var
  scene: TCastleScene;
begin
  if (FSelfEmission = value) then Exit;
  FSelfEmission:= value;

  for scene in GetAllScenes(self) do
  begin
    SetEmission(scene, value, value, value, True);
  end;
end;

procedure TNyaBaseActor.SetAnisotropicDegree(value: Single);
var
  scene: TCastleScene;
begin
  if (FAnisotropicDegree = value) then Exit;
  FAnisotropicDegree:= value;

  for scene in GetAllScenes(self) do
  begin
    SetAnisotropicFiltering(scene, value);
  end;
end;

function TNyaBaseActor.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'ActorName', 'Speed', 'Pleasure', 'Tension', 'AutoAnimation',
       'Lightning', 'SelfEmission', 'AnisotropicDegree',
       'PersonalColorPersistent'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

function TNyaBaseActor.GetPersonalColorForPersistent: TCastleColorRGB;
begin
  Result:= PersonalColor;
end;

procedure TNyaBaseActor.SetPersonalColorForPersistent(const AValue: TCastleColorRGB);
begin
  PersonalColor:= AValue;
end;

{$ifdef CASTLE_DESIGN_MODE}
type
  { Property editor to select an animation on TNyaSwitch }
  TNyaBaseActorPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

function TNyaBaseActorPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result:= [paMultiSelect, paValueList, paSortList, paRevertable];
end;

procedure TNyaBaseActorPropertyEditor.GetValues(Proc: TGetStrProc);
var
  Nav: TNyaBaseActor;
  Scene: TCastleSceneCore;
  S: String;
begin
  Proc('');
  Nav:= GetComponent(0) as TNyaBaseActor;
  Scene:= Nav.MainActor;
  if Scene <> nil then
    for S in Scene.AnimationsList do
      Proc(S);
end;
{$endif}

initialization
  {$ifdef CASTLE_DESIGN_MODE}
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaBaseActor, 'AutoAnimation',
    TNyaBaseActorPropertyEditor);
  {$endif}
end.

