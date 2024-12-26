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
    FPleasure: Single;
    FTension: Single;
    function GetSpeed: Single; virtual; abstract;
    procedure SetSpeed(value: Single); virtual; abstract;
    procedure SetAutoAnimation(const Value: String); virtual; abstract;
  public
    const
      DefaultActorName = 'unknown';
      DefaultAutoAnimation  = 'none';

    constructor Create(AOwner: TComponent); override;
    procedure PlayAnimation(const animationName: String; loop: boolean = true); virtual; abstract;
    procedure PlayAnimation(const Parameters: TPlayAnimationParameters); virtual; abstract;
    procedure StopAnimation(const DisableStopNotification: Boolean = false); virtual; abstract;
    function MainActor: TCastleScene; virtual; abstract;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    property AutoAnimation: String read FAutoAnimation write SetAutoAnimation;
    property ActorName: String read FActorName write FActorName;
    property Speed: Single read GetSpeed write SetSpeed;
  end;

implementation

uses
  CastleUtils
  {$ifdef CASTLE_DESIGN_MODE}
  , PropEdits, ComponentEditors, CastlePropEdits
  {$endif};

constructor TNyaBaseActor.Create(AOwner: TComponent);
begin
  inherited;

  FActorName:= DefaultActorName;
  FAutoAnimation:= DefaultAutoAnimation;
end;

function TNyaBaseActor.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'ActorName', 'Speed', 'Pleasure', 'Tension', 'AutoAnimation'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
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

