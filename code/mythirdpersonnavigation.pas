unit MyThirdPersonNavigation;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleCameras, CastleTransform, CastleClassUtils,
  CastleVectors;

type
  TMyThirdPersonNavigation = class(TCastleMouseLookNavigation)
  protected
    FDistanceToAvatarTarget: Single;
    FCameraSpeed: Single;
    FAvatarTarget: TVector3;
    FAvatarTargetPersistent: TCastleVector3Persistent;
    FAvatarHierarchy: TCastleTransform;
    FAvatarHierarchyFreeObserver: TFreeNotificationObserver;
    procedure AvatarHierarchyFreeNotification(const Sender: TFreeNotificationObserver);
    procedure SetAvatarHierarchy(const Value: TCastleTransform);
    function GetAvatarTargetForPersistent: TVector3;
    procedure SetAvatarTargetForPersistent(const AValue: TVector3);
    procedure SetDistanceToAvatarTarget(const Value: Single);

    procedure UpdateCamera(const SecondsPassed: Single);
  public
    const
      DefaultAvatarTarget: TVector3 = (X: 0; Y: 20; Z: 0);
      DefaultDistanceToAvatarTarget = 40.0;
      DefaultCameraSpeed = 10;

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
    property CameraSpeed: Single read FCameraSpeed write FCameraSpeed
      {$ifdef FPC}default DefaultCameraSpeed{$endif};
  end;

implementation

uses
  CastleComponentSerialize, CastleUtils, Math;

constructor TMyThirdPersonNavigation.Create(AOwner: TComponent);
begin
  inherited;
  FAvatarTarget:= DefaultAvatarTarget;
  FDistanceToAvatarTarget:= DefaultDistanceToAvatarTarget;
  FCameraSpeed := DefaultCameraSpeed;

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
  if not Valid then Exit;

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
       'DistanceToAvatarTarget'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TMyThirdPersonNavigation.UpdateCamera(const SecondsPassed: Single);
var
  TargetWorldPos: TVector3;
  CameraPos, CameraDir, CameraUp: TVector3;
  CameraPosTarget: TVector3;
begin
  if NOT Assigned(AvatarHierarchy) then Exit;

  TargetWorldPos:= AvatarHierarchy.WorldTransform.MultPoint(AvatarTarget);
  Camera.GetWorldView(CameraPos, CameraDir, CameraUp);
  CameraPosTarget:= TargetWorldPos - CameraDir * DistanceToAvatarTarget;

  CameraPos:= SmoothTowards(CameraPos, CameraPosTarget, SecondsPassed, CameraSpeed);

  Camera.SetWorldView(CameraPos, CameraDir, CameraUp);
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

