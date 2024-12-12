unit MyThirdPersonCameraNavigation;

{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, SysUtils, CastleCameras, CastleTransform, CastleClassUtils,
  CastleVectors;

type
  TMyThirdPersonCameraNavigation = class(TCastleMouseLookNavigation)
  protected
    FDistanceToAvatarTarget: Single;
    FDistanceToAvatarTargetMin: Single;
    FDistanceToAvatarTargetMax: Single;
    FFollowSpeed: Single;
    FZoomStep: Single;
    FAvatarTarget: TVector3;
    FAvatarTargetPersistent: TCastleVector3Persistent;
    FAvatarHierarchy: TCastleTransform;
    FAvatarHierarchyFreeObserver: TFreeNotificationObserver;
    procedure AvatarHierarchyFreeNotification(const Sender: TFreeNotificationObserver);
    procedure SetAvatarHierarchy(const Value: TCastleTransform);
    function GetAvatarTargetForPersistent: TVector3;
    procedure SetAvatarTargetForPersistent(const AValue: TVector3);
    procedure SetDistanceToAvatarTarget(const Value: Single);
    procedure SetDistanceToAvatarTargetMin(const Value: Single);
    procedure SetDistanceToAvatarTargetMax(const Value: Single);
  protected
    procedure CalcCamera(const ADir: TVector3; out APos, AUp: TVector3);
    procedure CameraCollision(const CameraDir: TVector3; var CameraPos: TVector3);
    procedure UpdateCamera(const SecondsPassed: Single);
    procedure ProcessMouseLookDelta(const Delta: TVector2); override;
    function Zoom(const Factor: Single): Boolean; override;
  public
    const
      DefaultAvatarTarget: TVector3 = (X: 0; Y: 20; Z: 0);
      DefaultDistanceToAvatarTarget = 40.0;
      DefaultDistanceToAvatarTargetMin = 10.0;
      DefaultDistanceToAvatarTargetMax = 180.0;
      DefaultFollowStep = 10;
      DefaultZoomStep = 5;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single;
                     var HandleInput: Boolean); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    property AvatarTarget: TVector3 read FAvatarTarget write FAvatarTarget;
  published
    property AvatarHierarchy: TCastleTransform read FAvatarHierarchy write SetAvatarHierarchy;
    property AvatarTargetPersistent: TCastleVector3Persistent read FAvatarTargetPersistent;
    property DistanceToAvatarTarget: Single read FDistanceToAvatarTarget write SetDistanceToAvatarTarget
      {$ifdef FPC}default DefaultDistanceToAvatarTarget{$endif};
    property DistanceToAvatarTargetMin: Single read FDistanceToAvatarTargetMin write SetDistanceToAvatarTargetMin
      {$ifdef FPC}default DefaultDistanceToAvatarTargetMin{$endif};
    property DistanceToAvatarTargetMax: Single read FDistanceToAvatarTargetMax write SetDistanceToAvatarTargetMax
      {$ifdef FPC}default DefaultDistanceToAvatarTargetMax{$endif};
    property FollowSpeed: Single read FFollowSpeed write FFollowSpeed
      {$ifdef FPC}default DefaultFollowStep{$endif};
    property ZoomStep: Single read FZoomStep write FZoomStep
      {$ifdef FPC}default DefaultZoomStep{$endif};
  end;

implementation

uses
  CastleComponentSerialize, CastleUtils, Math, CastleQuaternions,
  CastleVectorsInternalSingle;

constructor TMyThirdPersonCameraNavigation.Create(AOwner: TComponent);
begin
  inherited;
  FAvatarTarget:= DefaultAvatarTarget;
  FDistanceToAvatarTarget:= DefaultDistanceToAvatarTarget;
  FDistanceToAvatarTargetMin:= DefaultDistanceToAvatarTargetMin;
  FDistanceToAvatarTargetMax:= DefaultDistanceToAvatarTargetMax;
  FFollowSpeed:= DefaultFollowStep;
  FZoomStep:= DefaultZoomStep;

  FAvatarHierarchyFreeObserver:= TFreeNotificationObserver.Create(Self);
  FAvatarHierarchyFreeObserver.OnFreeNotification:= {$ifdef FPC}@{$endif}AvatarHierarchyFreeNotification;

  { Persistent for AvatarTarget }
  FAvatarTargetPersistent:= TCastleVector3Persistent.Create(nil);
  FAvatarTargetPersistent.SetSubComponent(true);
  FAvatarTargetPersistent.InternalGetValue:= {$ifdef FPC}@{$endif}GetAvatarTargetForPersistent;
  FAvatarTargetPersistent.InternalSetValue:= {$ifdef FPC}@{$endif}SetAvatarTargetForPersistent;
  FAvatarTargetPersistent.InternalDefaultValue:= DefaultAvatarTarget; // current value is default
end;

destructor TMyThirdPersonCameraNavigation.Destroy;
begin
  AvatarHierarchy:= nil;
  FreeAndNil(FAvatarTargetPersistent);
  inherited;
end;

procedure TMyThirdPersonCameraNavigation.Update(const SecondsPassed: Single;
                 var HandleInput: Boolean);
begin
  inherited;
  if NOT Valid then Exit;

  UpdateCamera(SecondsPassed);
end;

procedure TMyThirdPersonCameraNavigation.SetAvatarHierarchy(const Value: TCastleTransform);
begin
  if (FAvatarHierarchy <> Value) then
  begin
    FAvatarHierarchy:= Value;
    FAvatarHierarchyFreeObserver.Observed:= Value;
  end;
end;

function TMyThirdPersonCameraNavigation.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'AvatarHierarchy', 'AvatarTargetPersistent',
       'DistanceToAvatarTarget', 'DistanceToAvatarTargetMin',
       'DistanceToAvatarTargetMax', 'FollowSpeed', 'ZoomStep'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TMyThirdPersonCameraNavigation.CalcCamera(const ADir: TVector3; out APos, AUp: TVector3);
var
  TargetWorldPos: TVector3;
begin
  if NOT Assigned(AvatarHierarchy) then Exit;

  TargetWorldPos:= AvatarHierarchy.WorldTransform.MultPoint(AvatarTarget);
  APos:= TargetWorldPos - ADir * DistanceToAvatarTarget;
  CameraCollision(ADir, APos);
  AUp:= Camera.GravityUp;
end;

procedure TMyThirdPersonCameraNavigation.CameraCollision(const CameraDir: TVector3; var CameraPos: TVector3);
var
  TargetWorldPos: TVector3;
  CollisionDistance: Single;
  SavedPickable: Boolean;
begin
  CollisionDistance:= MaxSingle;
  TargetWorldPos:= AvatarHierarchy.WorldTransform.MultPoint(AvatarTarget);

  SavedPickable := AvatarHierarchy.Pickable;
  AvatarHierarchy.Pickable := false;
  try
    if (AvatarHierarchy.World.WorldRayCast(TargetWorldPos, -CameraDir, CollisionDistance) = nil) then
      CollisionDistance:= MaxSingle;
  finally
    AvatarHierarchy.Pickable := SavedPickable;
  end;

  if PointsDistanceSqr(CameraPos, TargetWorldPos) > Sqr(CollisionDistance) then
    CameraPos:= TargetWorldPos - CameraDir.AdjustToLength(CollisionDistance);
end;

procedure TMyThirdPersonCameraNavigation.UpdateCamera(const SecondsPassed: Single);
var
  CameraPos, CameraPosTarget, CameraDir, CameraUp: TVector3;
begin
  Camera.GetWorldView(CameraPos, CameraDir, CameraUp);
  CameraPosTarget:= CameraPos;
  CalcCamera(CameraDir, CameraPosTarget, CameraUp);
  CameraPos:= SmoothTowards(CameraPos, CameraPosTarget, SecondsPassed, FollowSpeed);
  Camera.SetWorldView(CameraPos, CameraDir, CameraUp);
end;

procedure TMyThirdPersonCameraNavigation.ProcessMouseLookDelta(const Delta: TVector2);
var
  CameraPos, CameraDir, CameraUp: TVector3;
  TurnVertDir, TurnHorizDir, CrossTurnDir: TVector3;
  Rot: TVector2;
begin
  inherited;

  Camera.GetWorldView(CameraPos, CameraDir, CameraUp);
  TurnVertDir:= Camera.GravityUp;

  Rot:= (-Pi/180.0) * Delta;

  { using formula rotate vector v around vector k:
  v_rot = v + (1-cos(angle))(k x (k x v)) + sin(angle)(k x v) }

  CrossTurnDir:= TVector3.CrossProduct(TurnVertDir, CameraDir);
  CameraDir:= CameraDir +
              (1 - cos(Rot.X)) * TVector3.CrossProduct(TurnVertDir, CrossTurnDir) +
              sin(Rot.X) * CrossTurnDir;

  TurnHorizDir:= TVector3.CrossProduct(TurnVertDir, CameraDir);

  CrossTurnDir:= TVector3.CrossProduct(TurnHorizDir, CameraDir);
  CameraDir:= CameraDir +
              (1 - cos(Rot.Y)) * TVector3.CrossProduct(TurnHorizDir, CrossTurnDir) +
              sin(Rot.Y) * CrossTurnDir;

  CalcCamera(CameraDir, CameraPos, CameraUp);

  CameraUp:= TurnVertDir;
  Camera.SetWorldView(CameraPos, CameraDir, CameraUp);
end;

function TMyThirdPersonCameraNavigation.Zoom(const Factor: Single): Boolean;
begin
  Result:= false;
  if NOT Valid then Exit;

  DistanceToAvatarTarget:= DistanceToAvatarTarget - Factor * ZoomStep;

  if (DistanceToAvatarTarget > DistanceToAvatarTargetMax) then
    DistanceToAvatarTarget:= DistanceToAvatarTargetMax
  else if (DistanceToAvatarTarget < DistanceToAvatarTargetMin) then
    DistanceToAvatarTarget:= DistanceToAvatarTargetMin;

  Result:= True;
end;

procedure TMyThirdPersonCameraNavigation.AvatarHierarchyFreeNotification(
  const Sender: TFreeNotificationObserver);
begin
  AvatarHierarchy:= nil;
end;

function TMyThirdPersonCameraNavigation.GetAvatarTargetForPersistent: TVector3;
begin
  Result:= AvatarTarget;
end;

procedure TMyThirdPersonCameraNavigation.SetAvatarTargetForPersistent(const AValue: TVector3);
begin
  AvatarTarget:= AValue;
end;

procedure TMyThirdPersonCameraNavigation.SetDistanceToAvatarTarget(const Value: Single);
begin
  if FDistanceToAvatarTarget <> Value then
    FDistanceToAvatarTarget:= Value;
end;

procedure TMyThirdPersonCameraNavigation.SetDistanceToAvatarTargetMin(const Value: Single);
begin
  if FDistanceToAvatarTargetMin <> Value then
    FDistanceToAvatarTargetMin:= Value;
end;

procedure TMyThirdPersonCameraNavigation.SetDistanceToAvatarTargetMax(const Value: Single);
begin
  if FDistanceToAvatarTargetMax <> Value then
    FDistanceToAvatarTargetMax:= Value;
end;

initialization
  RegisterSerializableComponent(TMyThirdPersonCameraNavigation, ['Navigation', 'My-Third-Person-Camera']);
end.

