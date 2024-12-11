unit MyThirdPersonNavigation;

{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, SysUtils, CastleCameras, CastleTransform, CastleClassUtils,
  CastleVectors;

type
  TMyThirdPersonNavigation = class(TCastleMouseLookNavigation)
  protected
    FDistanceToAvatarTarget: Single;
    FFollowSpeed: Single;
    FZoomSpeed: Single;
    FAvatarTarget: TVector3;
    FAvatarTargetPersistent: TCastleVector3Persistent;
    FAvatarHierarchy: TCastleTransform;
    FAvatarHierarchyFreeObserver: TFreeNotificationObserver;
    procedure AvatarHierarchyFreeNotification(const Sender: TFreeNotificationObserver);
    procedure SetAvatarHierarchy(const Value: TCastleTransform);
    function GetAvatarTargetForPersistent: TVector3;
    procedure SetAvatarTargetForPersistent(const AValue: TVector3);
    procedure SetDistanceToAvatarTarget(const Value: Single);
  protected
    procedure CalcCamera(const ADir: TVector3; out APos, AUp: TVector3);
    procedure UpdateCamera(const SecondsPassed: Single);
    procedure ProcessMouseLookDelta(const Delta: TVector2); override;
    function Zoom(const Factor: Single): Boolean; override;
  public
    const
      DefaultAvatarTarget: TVector3 = (X: 0; Y: 20; Z: 0);
      DefaultDistanceToAvatarTarget = 40.0;
      DefaultFollowSpeed = 10;
      DefaultZoomSpeed = 5;

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
    property FollowSpeed: Single read FFollowSpeed write FFollowSpeed
      {$ifdef FPC}default DefaultFollowSpeed{$endif};
    property ZoomSpeed: Single read FZoomSpeed write FZoomSpeed
      {$ifdef FPC}default DefaultZoomSpeed{$endif};
  end;

implementation

uses
  CastleComponentSerialize, CastleUtils, Math, CastleQuaternions,
  CastleVectorsInternalSingle;

constructor TMyThirdPersonNavigation.Create(AOwner: TComponent);
begin
  inherited;
  FAvatarTarget:= DefaultAvatarTarget;
  FDistanceToAvatarTarget:= DefaultDistanceToAvatarTarget;
  FFollowSpeed:= DefaultFollowSpeed;
  FZoomSpeed:= DefaultZoomSpeed;

  FAvatarHierarchyFreeObserver:= TFreeNotificationObserver.Create(Self);
  FAvatarHierarchyFreeObserver.OnFreeNotification:= {$ifdef FPC}@{$endif}AvatarHierarchyFreeNotification;

  { Persistent for AvatarTarget }
  FAvatarTargetPersistent:= TCastleVector3Persistent.Create(nil);
  FAvatarTargetPersistent.SetSubComponent(true);
  FAvatarTargetPersistent.InternalGetValue:= {$ifdef FPC}@{$endif}GetAvatarTargetForPersistent;
  FAvatarTargetPersistent.InternalSetValue:= {$ifdef FPC}@{$endif}SetAvatarTargetForPersistent;
  FAvatarTargetPersistent.InternalDefaultValue:= DefaultAvatarTarget; // current value is default
end;

destructor TMyThirdPersonNavigation.Destroy;
begin
  AvatarHierarchy:= nil;
  FreeAndNil(FAvatarTargetPersistent);
  inherited;
end;

procedure TMyThirdPersonNavigation.Update(const SecondsPassed: Single;
                 var HandleInput: Boolean);
begin
  inherited;
  if NOT Valid then Exit;

  UpdateCamera(SecondsPassed);
end;

procedure TMyThirdPersonNavigation.SetAvatarHierarchy(const Value: TCastleTransform);
begin
  if (FAvatarHierarchy <> Value) then
  begin
    FAvatarHierarchy:= Value;
    FAvatarHierarchyFreeObserver.Observed:= Value;
  end;
end;

function TMyThirdPersonNavigation.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'AvatarHierarchy', 'AvatarTargetPersistent',
       'DistanceToAvatarTarget', 'FollowSpeed', 'ZoomSpeed'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TMyThirdPersonNavigation.CalcCamera(const ADir: TVector3; out APos, AUp: TVector3);
var
  TargetWorldPos: TVector3;
begin
  if NOT Assigned(AvatarHierarchy) then Exit;

  TargetWorldPos:= AvatarHierarchy.WorldTransform.MultPoint(AvatarTarget);
  APos:= TargetWorldPos - ADir * DistanceToAvatarTarget;
  AUp:= Camera.GravityUp;
end;

procedure TMyThirdPersonNavigation.UpdateCamera(const SecondsPassed: Single);
var
  CameraPos, CameraPosTarget, CameraDir, CameraUp: TVector3;
begin
  Camera.GetWorldView(CameraPos, CameraDir, CameraUp);
  CameraPosTarget:= CameraPos;
  CalcCamera(CameraDir, CameraPosTarget, CameraUp);
  CameraPos:= SmoothTowards(CameraPos, CameraPosTarget, SecondsPassed, FollowSpeed);
  Camera.SetWorldView(CameraPos, CameraDir, CameraUp);
end;

procedure TMyThirdPersonNavigation.ProcessMouseLookDelta(const Delta: TVector2);
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

function TMyThirdPersonNavigation.Zoom(const Factor: Single): Boolean;
begin
  Result:= false;
  if NOT Valid then Exit;

  DistanceToAvatarTarget:= DistanceToAvatarTarget - Factor * ZoomSpeed;

  Result:= True;
end;

procedure TMyThirdPersonNavigation.AvatarHierarchyFreeNotification(
  const Sender: TFreeNotificationObserver);
begin
  AvatarHierarchy:= nil;
end;

function TMyThirdPersonNavigation.GetAvatarTargetForPersistent: TVector3;
begin
  Result:= AvatarTarget;
end;

procedure TMyThirdPersonNavigation.SetAvatarTargetForPersistent(const AValue: TVector3);
begin
  AvatarTarget:= AValue;
end;

procedure TMyThirdPersonNavigation.SetDistanceToAvatarTarget(const Value: Single);
begin
  if FDistanceToAvatarTarget <> Value then
  begin
    FDistanceToAvatarTarget:= Value;
  end;
end;

initialization
  RegisterSerializableComponent(TMyThirdPersonNavigation, ['Navigation', 'My-Third-Person']);
end.

