unit NyaThirdPersonCameraNavigation;

{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, SysUtils, CastleCameras, CastleTransform, CastleClassUtils,
  CastleVectors, CastleInputs, CastleKeysMouse;

type
  TNyaThirdPersonCameraNavigation = class(TCastleMouseLookNavigation)
  protected
    FFollowRotation: Boolean;
    FLocalCamDir: TVector3;
    FDistanceToAvatarTarget: Single;
    FDistanceToAvatarTargetMin: Single;
    FDistanceToAvatarTargetMax: Single;
    FFollowSpeed: Single;
    FZoomStep: Single;
    FInput_Rotate: TInputShortcut;
    FInput_Shift: TInputShortcut;
    FAvatarTarget: TVector3;
    FAvatarTargetPersistent: TCastleVector3Persistent;
    FLocalCamDirPersistent: TCastleVector3Persistent;
    FAvatarHierarchy: TCastleTransform;
    FAvatarHierarchyFreeObserver: TFreeNotificationObserver;
    procedure AvatarHierarchyFreeNotification(const Sender: TFreeNotificationObserver);
    procedure SetAvatarHierarchy(const Value: TCastleTransform);
    function GetAvatarTargetForPersistent: TVector3;
    procedure SetAvatarTargetForPersistent(const AValue: TVector3);
    function GetLocalCamDirForPersistent: TVector3;
    procedure SetLocalCamDirForPersistent(const AValue: TVector3);
  protected
    procedure CalcCameraPos(const ADir: TVector3; out APos, AUp: TVector3);
    procedure CameraCollision(const CameraDir: TVector3; var CameraPos: TVector3);
    procedure ProcessMouseLookDelta(const Delta: TVector2); override;
    function Zoom(const Factor: Single): Boolean; override;
  public
    const
      DefaultAvatarTarget: TVector3 = (X: 0.0; Y: 1.0; Z: 0.0);
      DefaultLocalCamDir: TVector3 = (X: 0.0; Y: -0.44721; Z: 0.89443);
      DefaultDistanceToAvatarTarget = 1.0;
      DefaultDistanceToAvatarTargetMin = 0.5;
      DefaultDistanceToAvatarTargetMax = 4.0;
      DefaultFollowSpeed = 10;
      DefaultZoomStep = 0.1;
      DefaultFollowRotation = False;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single;
                     var HandleInput: Boolean); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    property AvatarTarget: TVector3 read FAvatarTarget write FAvatarTarget;
    property LocalCamDir: TVector3 read FLocalCamDir write FLocalCamDir;
  public
    property Input_Rotate: TInputShortcut read FInput_Rotate;
    property Input_Shift: TInputShortcut read FInput_Shift;
  published
    property AvatarHierarchy: TCastleTransform read FAvatarHierarchy write SetAvatarHierarchy;
    property AvatarTargetPersistent: TCastleVector3Persistent read FAvatarTargetPersistent;
    property LocalCamDirPersistent: TCastleVector3Persistent read FLocalCamDirPersistent;
    property DistanceToAvatarTarget: Single read FDistanceToAvatarTarget write FDistanceToAvatarTarget
      {$ifdef FPC}default DefaultDistanceToAvatarTarget{$endif};
    property DistanceToAvatarTargetMin: Single
      read FDistanceToAvatarTargetMin write FDistanceToAvatarTargetMin
      {$ifdef FPC}default DefaultDistanceToAvatarTargetMin{$endif};
    property DistanceToAvatarTargetMax: Single
      read FDistanceToAvatarTargetMax write FDistanceToAvatarTargetMax
      {$ifdef FPC}default DefaultDistanceToAvatarTargetMax{$endif};
    property FollowSpeed: Single read FFollowSpeed write FFollowSpeed
      {$ifdef FPC}default DefaultFollowSpeed{$endif};
    property ZoomStep: Single read FZoomStep write FZoomStep
      {$ifdef FPC}default DefaultZoomStep{$endif};
    property FollowRotation: Boolean read FFollowRotation write FFollowRotation
      {$ifdef FPC}default DefaultFollowRotation{$endif};
  end;

implementation

uses
  CastleComponentSerialize, CastleUtils, Math, CastleQuaternions,
  CastleVectorsInternalSingle, NyaVectorMath;

constructor TNyaThirdPersonCameraNavigation.Create(AOwner: TComponent);
begin
  inherited;

  FInput_Rotate                := TInputShortcut.Create(Self);
  FInput_Shift                 := TInputShortcut.Create(Self);

  Input_Rotate                 .Assign(keyNone, keyNone, '', True, buttonRight);
  Input_Shift                  .Assign(keyNone, keyNone, '', True, buttonMiddle);

  Input_Rotate                 .SetSubComponent(true);
  Input_Shift                  .SetSubComponent(true);

  Input_Rotate                 .Name:= 'Input_Rotate';
  Input_Shift                  .Name:= 'Input_Shift';

  FAvatarTarget:= DefaultAvatarTarget;
  FLocalCamDir:= DefaultLocalCamDir;
  FDistanceToAvatarTarget:= DefaultDistanceToAvatarTarget;
  FDistanceToAvatarTargetMin:= DefaultDistanceToAvatarTargetMin;
  FDistanceToAvatarTargetMax:= DefaultDistanceToAvatarTargetMax;
  FFollowSpeed:= DefaultFollowSpeed;
  FZoomStep:= DefaultZoomStep;
  FFollowRotation:= DefaultFollowRotation;

  FAvatarHierarchyFreeObserver:= TFreeNotificationObserver.Create(Self);
  FAvatarHierarchyFreeObserver.OnFreeNotification:= {$ifdef FPC}@{$endif}AvatarHierarchyFreeNotification;

  { Persistent for AvatarTarget }
  FAvatarTargetPersistent:= TCastleVector3Persistent.Create(nil);
  FAvatarTargetPersistent.SetSubComponent(true);
  FAvatarTargetPersistent.InternalGetValue:= {$ifdef FPC}@{$endif}GetAvatarTargetForPersistent;
  FAvatarTargetPersistent.InternalSetValue:= {$ifdef FPC}@{$endif}SetAvatarTargetForPersistent;
  FAvatarTargetPersistent.InternalDefaultValue:= DefaultAvatarTarget; // current value is default

  { Persistent for AvatarTarget }
  FLocalCamDirPersistent:= TCastleVector3Persistent.Create(nil);
  FLocalCamDirPersistent.SetSubComponent(true);
  FLocalCamDirPersistent.InternalGetValue:= {$ifdef FPC}@{$endif}GetLocalCamDirForPersistent;
  FLocalCamDirPersistent.InternalSetValue:= {$ifdef FPC}@{$endif}SetLocalCamDirForPersistent;
  FLocalCamDirPersistent.InternalDefaultValue:= DefaultLocalCamDir; // current value is default
end;

destructor TNyaThirdPersonCameraNavigation.Destroy;
begin
  AvatarHierarchy:= nil;
  FreeAndNil(FAvatarTargetPersistent);
  inherited;
end;

procedure TNyaThirdPersonCameraNavigation.Update(const SecondsPassed: Single;
                                                 var HandleInput: Boolean);
var
  CameraPos, CameraPosTarget, CameraDir, CameraDirTarget, CameraUp: TVector3;
begin
  inherited;
  if NOT Valid then Exit;

  Camera.GetWorldView(CameraPos, CameraDir, CameraUp);

  if FollowRotation then
  begin
    CameraDirTarget:= AvatarHierarchy.WorldTransform.MultDirection(FLocalCamDir);
    CameraDir:= SmoothTowards(CameraDir, CameraDirTarget, SecondsPassed, FollowSpeed);
  end;

  CameraPosTarget:= CameraPos;
  CalcCameraPos(CameraDir, CameraPosTarget, CameraUp);
  CameraPos:= SmoothTowards(CameraPos, CameraPosTarget, SecondsPassed, FollowSpeed);
  Camera.SetWorldView(CameraPos, CameraDir, CameraUp);
end;

procedure TNyaThirdPersonCameraNavigation.CalcCameraPos(const ADir: TVector3;
                                                        out APos, AUp: TVector3);
var
  TargetWorldPos: TVector3;
begin
  if NOT Assigned(AvatarHierarchy) then Exit;

  TargetWorldPos:= AvatarHierarchy.WorldTransform.MultPoint(AvatarTarget);
  APos:= TargetWorldPos - ADir * DistanceToAvatarTarget;
  CameraCollision(ADir, APos);
  AUp:= Camera.GravityUp;
end;

procedure TNyaThirdPersonCameraNavigation.CameraCollision(const CameraDir: TVector3;
                                                          var CameraPos: TVector3);
var
  TargetWorldPos: TVector3;
  Collider: TCastleTransform;
  CollisionDistance: Single;
  SavedPickable: Boolean;
begin
  CollisionDistance:= MaxSingle;
  TargetWorldPos:= AvatarHierarchy.WorldTransform.MultPoint(AvatarTarget);

  SavedPickable:= AvatarHierarchy.Pickable;
  AvatarHierarchy.Pickable:= false;
  try
    Collider:= AvatarHierarchy.World.WorldRayCast(TargetWorldPos, -CameraDir, CollisionDistance);
  finally
    AvatarHierarchy.Pickable:= SavedPickable;
  end;

  if NOT Assigned(Collider) then
    CollisionDistance:= MaxSingle;

  if PointsDistanceSqr(CameraPos, TargetWorldPos) > Sqr(CollisionDistance) then
    CameraPos:= TargetWorldPos - CameraDir.AdjustToLength(CollisionDistance);
end;

procedure TNyaThirdPersonCameraNavigation.ProcessMouseLookDelta(const Delta: TVector2);
var
  CameraPos, CameraDir, CameraUp: TVector3;
  VerticalDir, HorizontalDir: TVector3;
  Rot: TVector2;
begin
  inherited;

  if Input_Rotate.IsPressed(Container) then
  begin
    Camera.GetWorldView(CameraPos, CameraDir, CameraUp);

    Rot:= (-Pi/180.0) * Delta;

    VerticalDir:= Camera.GravityUp;

    if FollowRotation then
    begin
      HorizontalDir:= TVector3.CrossProduct(VerticalDir, FLocalCamDir);
      FLocalCamDir:= TurnVectorAroundVector(FLocalCamDir, VerticalDir, Rot.X);
      FLocalCamDir:= TurnVectorAroundVector(FLocalCamDir, HorizontalDir, Rot.Y);
      CameraDir:= AvatarHierarchy.WorldTransform.MultDirection(FLocalCamDir);
    end else
    begin
      HorizontalDir:= TVector3.CrossProduct(VerticalDir, CameraDir);
      CameraDir:= TurnVectorAroundVector(CameraDir, VerticalDir, Rot.X);
      CameraDir:= TurnVectorAroundVector(CameraDir, HorizontalDir, Rot.Y);
    end;

    CalcCameraPos(CameraDir, CameraPos, CameraUp);
    Camera.SetWorldView(CameraPos, CameraDir, CameraUp);
  end
  else
  if Input_Shift.IsPressed(Container) then
  begin
    FAvatarTarget.Y:= FAvatarTarget.Y + Delta.Y;
    if (FAvatarTarget.Y < 0.0) then
      FAvatarTarget.Y:= 0.0
    else if (FAvatarTarget.Y > AvatarHierarchy.BoundingBox.MaxSize) then
      FAvatarTarget.Y:= AvatarHierarchy.BoundingBox.MaxSize;
  end;
end;

function TNyaThirdPersonCameraNavigation.Zoom(const Factor: Single): Boolean;
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

procedure TNyaThirdPersonCameraNavigation.SetAvatarHierarchy(const Value: TCastleTransform);
begin
  if (FAvatarHierarchy <> Value) then
  begin
    FAvatarHierarchy:= Value;
    FAvatarHierarchyFreeObserver.Observed:= Value;
  end;
end;

function TNyaThirdPersonCameraNavigation.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'AvatarHierarchy', 'AvatarTargetPersistent', 'LocalCamDirPersistent',
       'DistanceToAvatarTarget', 'DistanceToAvatarTargetMin',
       'DistanceToAvatarTargetMax', 'FollowSpeed', 'ZoomStep', 'FollowRotation'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

procedure TNyaThirdPersonCameraNavigation.AvatarHierarchyFreeNotification(
  const Sender: TFreeNotificationObserver);
begin
  AvatarHierarchy:= nil;
end;

function TNyaThirdPersonCameraNavigation.GetAvatarTargetForPersistent: TVector3;
begin
  Result:= AvatarTarget;
end;

procedure TNyaThirdPersonCameraNavigation.SetAvatarTargetForPersistent(const AValue: TVector3);
begin
  AvatarTarget:= AValue;
end;

function TNyaThirdPersonCameraNavigation.GetLocalCamDirForPersistent: TVector3;
begin
  Result:= LocalCamDir;
end;

procedure TNyaThirdPersonCameraNavigation.SetLocalCamDirForPersistent(const AValue: TVector3);
begin
  LocalCamDir:= AValue.Normalize;
end;

initialization
  RegisterSerializableComponent(TNyaThirdPersonCameraNavigation, ['Navigation', 'Nya Third-Person Camera']);
end.

