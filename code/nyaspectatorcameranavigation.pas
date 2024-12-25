unit NyaSpectatorCameraNavigation;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleCameras, CastleTransform, CastleClassUtils,
  CastleVectors;

type
  TNyaSpectatorCameraNavigation = class(TCastleMouseLookNavigation)
  protected
    FFollowSpeed: Single;
    FLastTargetPos: TVector3;
    FAvatarTarget: TVector3;
    FAvatarTargetPersistent: TCastleVector3Persistent;
    FAvatarHierarchy: TCastleTransform;
    FAvatarHierarchyFreeObserver: TFreeNotificationObserver;
    procedure AvatarHierarchyFreeNotification(const Sender: TFreeNotificationObserver);
    procedure SetAvatarHierarchy(const Value: TCastleTransform);
    function GetAvatarTargetForPersistent: TVector3;
    procedure SetAvatarTargetForPersistent(const AValue: TVector3);
  protected
    procedure CalcCamera(const SecondsPassed: Single;
                         const APos: TVector3; out ADir, AUp: TVector3);
    procedure UpdateCamera(const SecondsPassed: Single);
    function Zoom(const Factor: Single): Boolean; override;
  public
    const
      DefaultAvatarTarget: TVector3 = (X: 0; Y: 20; Z: 0);
      DefaultFollowSpeed = 10;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single;
                     var HandleInput: Boolean); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    property AvatarTarget: TVector3 read FAvatarTarget write FAvatarTarget;
  published
    property AvatarHierarchy: TCastleTransform read FAvatarHierarchy write SetAvatarHierarchy;
    property AvatarTargetPersistent: TCastleVector3Persistent read FAvatarTargetPersistent;
    property FollowSpeed: Single read FFollowSpeed write FFollowSpeed
      {$ifdef FPC}default DefaultFollowSpeed{$endif};
  end;

implementation

uses
  CastleComponentSerialize, CastleUtils, Math, CastleQuaternions,
  CastleVectorsInternalSingle, NyaVectorMath;

constructor TNyaSpectatorCameraNavigation.Create(AOwner: TComponent);
begin
  inherited;
  FAvatarTarget:= DefaultAvatarTarget;
  FFollowSpeed:= DefaultFollowSpeed;

  FLastTargetPos:= TVector3.Zero;

  FAvatarHierarchyFreeObserver:= TFreeNotificationObserver.Create(Self);
  FAvatarHierarchyFreeObserver.OnFreeNotification:= {$ifdef FPC}@{$endif}AvatarHierarchyFreeNotification;

  { Persistent for AvatarTarget }
  FAvatarTargetPersistent:= TCastleVector3Persistent.Create(nil);
  FAvatarTargetPersistent.SetSubComponent(true);
  FAvatarTargetPersistent.InternalGetValue:= {$ifdef FPC}@{$endif}GetAvatarTargetForPersistent;
  FAvatarTargetPersistent.InternalSetValue:= {$ifdef FPC}@{$endif}SetAvatarTargetForPersistent;
  FAvatarTargetPersistent.InternalDefaultValue:= DefaultAvatarTarget; // current value is default

//  ZoomEnabled;
end;

destructor TNyaSpectatorCameraNavigation.Destroy;
begin
  AvatarHierarchy:= nil;
  FreeAndNil(FAvatarTargetPersistent);
  inherited;
end;

procedure TNyaSpectatorCameraNavigation.Update(const SecondsPassed: Single;
                 var HandleInput: Boolean);
begin
  inherited;
  if NOT Valid then Exit;

  UpdateCamera(SecondsPassed);
end;

procedure TNyaSpectatorCameraNavigation.SetAvatarHierarchy(const Value: TCastleTransform);
begin
  if (FAvatarHierarchy <> Value) then
  begin
    FAvatarHierarchy:= Value;
    FAvatarHierarchyFreeObserver.Observed:= Value;
  end;
end;

function TNyaSpectatorCameraNavigation.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'AvatarHierarchy', 'AvatarTargetPersistent',
       'FollowSpeed'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

procedure TNyaSpectatorCameraNavigation.CalcCamera(const SecondsPassed: Single;
                                                  const APos: TVector3;
                                                  out ADir, AUp: TVector3);
var
  TargetWorldPos: TVector3;
begin
  if NOT Assigned(AvatarHierarchy) then Exit;

  TargetWorldPos:= AvatarHierarchy.WorldTransform.MultPoint(AvatarTarget);
  FLastTargetPos:= SmoothTowards(FLastTargetPos, TargetWorldPos, SecondsPassed, FollowSpeed);

  ADir:= FLastTargetPos - APos;
  AUp:= Camera.GravityUp;
end;

procedure TNyaSpectatorCameraNavigation.UpdateCamera(const SecondsPassed: Single);
var
  CameraPos, CameraDir, CameraUp: TVector3;
begin
  Camera.GetWorldView(CameraPos, CameraDir, CameraUp);
  CalcCamera(SecondsPassed, CameraPos, CameraDir, CameraUp);
  Camera.SetWorldView(CameraPos, CameraDir, CameraUp);
end;

function TNyaSpectatorCameraNavigation.Zoom(const Factor: Single): Boolean;
begin
  Result:= False;
end;

procedure TNyaSpectatorCameraNavigation.AvatarHierarchyFreeNotification(
  const Sender: TFreeNotificationObserver);
begin
  AvatarHierarchy:= nil;
end;

function TNyaSpectatorCameraNavigation.GetAvatarTargetForPersistent: TVector3;
begin
  Result:= AvatarTarget;
end;

procedure TNyaSpectatorCameraNavigation.SetAvatarTargetForPersistent(const AValue: TVector3);
begin
  AvatarTarget:= AValue;
end;

initialization
  RegisterSerializableComponent(TNyaSpectatorCameraNavigation, ['Navigation', 'Nya Spectator Camera']);
end.

