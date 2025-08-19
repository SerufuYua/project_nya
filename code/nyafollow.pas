unit NyaFollow;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleTransform, CastleClassUtils, CastleScene,
  CastleSceneCore, X3DNodes;

type
  TNyaFollow = class(TCastleBehavior)
  protected
    FFollowSpeed: Single;
    FTarget: TCastleTransform;
    FTargetObserver: TFreeNotificationObserver;
    procedure SetTarget(const Value: TCastleTransform);
    procedure TargetFreeNotification(const Sender: TFreeNotificationObserver);
  public
    const
      DefaultFollowSpeed = 10;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    property Target: TCastleTransform read FTarget write SetTarget;
    property FollowSpeed: Single read FFollowSpeed write FFollowSpeed
      {$ifdef FPC}default DefaultFollowSpeed{$endif};
  end;


implementation

uses
  CastleUtils, CastleComponentSerialize, CastleVectors;

constructor TNyaFollow.Create(AOwner: TComponent);
begin
  inherited;

  FFollowSpeed:= DefaultFollowSpeed;

  FTargetObserver:= TFreeNotificationObserver.Create(Self);
  FTargetObserver.OnFreeNotification:= {$ifdef FPC}@{$endif}TargetFreeNotification;
end;

destructor TNyaFollow.Destroy;
begin
  FreeAndNil(FTargetObserver);
  inherited;
end;

procedure TNyaFollow.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  inherited;

  if Assigned(FTarget) then
    Parent.Translation:= SmoothTowards(Parent.Translation,
                                       FTarget.WorldTranslation,
                                       SecondsPassed, FollowSpeed);
end;

procedure TNyaFollow.SetTarget(const Value: TCastleTransform);
begin
  if (FTarget <> Value) then
  begin
    FTargetObserver.Observed:= Value;
    FTarget:= Value;
  end;
end;

procedure TNyaFollow.TargetFreeNotification(
  const Sender: TFreeNotificationObserver);
begin
  Target:= nil;
end;

function TNyaFollow.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'Target', 'FollowSpeed'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TNyaFollow, ['Nya Follow']);
end.

