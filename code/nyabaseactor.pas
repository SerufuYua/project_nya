unit NyaBaseActor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleSceneCore, CastleTransform, CastleVectors,
  CastleColors, CharaDress, CastleClassUtils;

type
  TNyaBaseActor = class(TCastleTransformDesign)
  protected
    FActorName: String;
    FPleasure: Single;
    FTension: Single;
    function GetSpeed: Single; virtual; abstract;
    procedure SetSpeed(value: Single); virtual; abstract;
    procedure SetPleasure(value: Single); virtual; abstract;
    procedure SetTension(value: Single); virtual; abstract;
  public
    procedure PlayAnimation(const animationName: String; loop: boolean = true); virtual; abstract;
    procedure PlayAnimation(const Parameters: TPlayAnimationParameters); virtual; abstract;
    procedure StopAnimation(const DisableStopNotification: Boolean = false); virtual; abstract;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    property ActorName: String read FActorName write FActorName;
    property Speed: Single read GetSpeed write SetSpeed;
    property Pleasure: Single read FPleasure write SetPleasure;
    property Tension: Single read FTension write SetTension;
  end;

implementation

uses
  CastleUtils;

function TNyaBaseActor.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'ActorName', 'Speed', 'Pleasure', 'Tension'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

end.

