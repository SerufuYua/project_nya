unit NyaAnimators;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleTransform, CastleClassUtils, CastleScene,
  CastleSceneCore, CastleVectors, CastleTimeUtils, X3DNodes;

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

TNyaRotate = class(TCastleBehavior)
protected
  FTime: TFloatTime;
  FRotationStep: Single;
  FRotationDirection: TVector3;
  FRotatorPersistent: TCastleVector3Persistent;
  function GetRotatorPersistent: TVector3;
  procedure SetRotatorPersistent(const AValue: TVector3);
  function GetRotator: TVector3;
  procedure SetRotator(const AValue: TVector3);
public
  const
    DefaultRotator: TVector3 = (X: 0; Y: 0; Z: 0);

  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;
  procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  function PropertySections(const PropertyName: String): TPropertySections; override;

  property Rotator: TVector3 read GetRotator write SetRotator;
published
  property RotatorPersistent: TCastleVector3Persistent read FRotatorPersistent;
end;

implementation

uses
  CastleUtils, CastleComponentSerialize;

{ ========= ------------------------------------------------------------------ }
{ TNyaFollow ----------------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

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

{ ========= ------------------------------------------------------------------ }
{ TNyaRotate ----------------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

constructor TNyaRotate.Create(AOwner: TComponent);
begin
  inherited;

  FTime:= 0.0;
  FRotationDirection:= DefaultRotator;
  FRotationStep:= 0.0;

  { Persistent for Rotator }
  FRotatorPersistent:= TCastleVector3Persistent.Create(nil);
  FRotatorPersistent.SetSubComponent(true);
  FRotatorPersistent.InternalGetValue:= {$ifdef FPC}@{$endif}GetRotatorPersistent;
  FRotatorPersistent.InternalSetValue:= {$ifdef FPC}@{$endif}SetRotatorPersistent;
  FRotatorPersistent.InternalDefaultValue:= DefaultRotator; // current value is default
end;

destructor TNyaRotate.Destroy;
begin
  FreeAndNil(FRotatorPersistent);
  inherited;
end;

procedure TNyaRotate.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  inherited;

  FTime:= FTime + SecondsPassed;
  Parent.Rotation:= Vector4(FRotationDirection, FRotationStep * FTime);
end;

function TNyaRotate.GetRotator: TVector3;
begin
  Result:= FRotationDirection * FRotationStep;
end;

procedure TNyaRotate.SetRotator(const AValue: TVector3);
begin
  FRotationDirection:= AValue.Normalize;
  FRotationStep:= AValue.Length;
end;

function TNyaRotate.GetRotatorPersistent: TVector3;
begin
  Result:= Rotator;
end;

procedure TNyaRotate.SetRotatorPersistent(const AValue: TVector3);
begin
  Rotator:= AValue;
end;

function TNyaRotate.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'RotatorPersistent'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TNyaFollow, ['Nya Follow']);
  RegisterSerializableComponent(TNyaRotate, ['Nya Rotate']);
end.

