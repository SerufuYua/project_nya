unit BaseViewRide;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleTransform, CastleKeysMouse, CastleUIControls,
  NyaVehicleNavigation, BaseViewTravel, NyaActor, NyaActorVehicle,
  NyaBaseNavigation;

type
  TBaseViewRide = class(TBaseViewTravel)
  published
    GroupSpeed: TCastleUserInterface;
  protected
    FGetOffSwitch, FLightSwitch: TKey;
    FTempCollider: TCastleCollider;
    FTempRBody: TCastleRigidBody;
    FTempActorParent: TCastleTransform;
    FVehicle: TNyaActorVehicle;
    FVehicleNavigation: TNyaVehicleNavigation;
    procedure NavigationSetAnimation(
      const Sender: TNyaBaseNavigation;
      const AnimationName: String; AnimtionSpeed: Single);
  public
    procedure Start; override;
    function Press(const Event: TInputPressRelease): Boolean; override;
    procedure SitToVehicle(vehicle: TNyaActorVehicle);
    procedure GetOffFromVehicle;
  end;

implementation

uses
  CastleVectors, CastleScene, NyaThirdPersonCameraNavigation,
  NyaSwitch, NyaCastleUtils;

procedure TBaseViewRide.Start;
begin
  inherited;
  FTempCollider:=nil;
  FTempRBody:= nil;
  FTempActorParent:= nil;
  FVehicle:= nil;

  { set keys }
  FLightSwitch:= TKey.keyL;
  FGetOffSwitch:= TKey.keyR;
end;

function TBaseViewRide.Press(const Event: TInputPressRelease): Boolean;
begin
  Result:= inherited;
  if Result then Exit; // allow the ancestor to handle keys

  { switch light }
  if Event.IsKey(FLightSwitch) then
  begin
    if Assigned(FVehicle) then
      FVehicle.Headlight:= NOT FVehicle.Headlight;
    Exit(true);
  end;

  { get off from Vehicle }
  if Event.IsKey(FGetOffSwitch) then
  begin
    GetOffFromVehicle;
    Exit(true);
  end;
end;

procedure TBaseViewRide.SitToVehicle(vehicle: TNyaActorVehicle);
var
  camera: TNyaThirdPersonCameraNavigation;
  switch: TCastleBehavior;
begin
  Status.Caption:= '';
  FVehicle:= vehicle;

  FTempRBody:= MainActor.RigidBody;
  MainActor.RemoveBehavior(MainActor.RigidBody);

  FTempCollider:= MainActor.Collider;
  MainActor.RemoveBehavior(MainActor.Collider);

  FCharaNavigation.Exists:= False;
  if (FCameraNavigation is TNyaThirdPersonCameraNavigation) then
  begin
    camera:= (FCameraNavigation as TNyaThirdPersonCameraNavigation);
    camera.AvatarHierarchy:= FVehicle;
    camera.FollowRotation:= True;
  end;

  switch:= FVehicle[1].FindBehavior(TNyaSwitch);
    if Assigned(switch) then
      switch.Parent.Exists:= False;

  FVehicleNavigation.AvatarHierarchy:= FVehicle;
  FVehicleNavigation.Exists:= True;

  if Assigned(FVehicle.RigidBody) then
    FVehicle.RigidBody.Dynamic:= True;

  FVehicleNavigation.OnAnimation:= {$ifdef FPC}@{$endif}NavigationSetAnimation;

  MainActor.Translation:= TVector3.Zero;
  MainActor.Rotation:= TVector4.Zero;
  FTempActorParent:= MainActor.Parent;
  MainActor.Parent:= FVehicle;

  FVehicle.Headlight:= True;

  GroupSpeed.Exists:= True;
  SetUIColor(FVehicle.PersonalColor);

  Notifications.Show('Info: use WASD to move');
  Notifications.Show('Info: use Space to brake');
  Notifications.Show('Info: use L to switch headlight');
  Notifications.Show('Info: use R to get off from the Vehicle');
end;

procedure TBaseViewRide.GetOffFromVehicle;
var
  camera: TNyaThirdPersonCameraNavigation;
  switch: TCastleBehavior;
begin
  if NOT Assigned(FVehicle) then Exit;

  if Assigned(FVehicle.RigidBody) then
  begin
    if NOT (FVehicle.RigidBody.LinearVelocity.IsZero(0.1) AND
            FVehicle.RigidBody.AngularVelocity.IsZero(0.1)) then
      begin
          Notifications.Show('Stop movement before get off from the Vehicle');
          Exit;
      end;
  end;

  FVehicleNavigation.Exists:= False;
  FVehicleNavigation.Parking;
  FVehicleNavigation.OnAnimation:= nil;

  MainActor.Parent:= FTempActorParent;
  MainActor.Translation:= FVehicle.Translation;
  MainActor.Translate(Vector3(1.0, 0.0, 0.0));
  MainActor.Rotation:= FVehicle.Rotation;
  MainActor.Up:= Vector3(0.0, 1.0, 0.0);

  MainActor.AddBehavior(FTempCollider);
  MainActor.AddBehavior(FTempRBody);

  if (FCameraNavigation is TNyaThirdPersonCameraNavigation) then
  begin
    camera:= (FCameraNavigation as TNyaThirdPersonCameraNavigation);
    camera.AvatarHierarchy:= MainActor;
    camera.FollowRotation:= False;
  end;

  switch:= FVehicle[1].FindBehavior(TNyaSwitch);
    if Assigned(switch) then
      switch.Parent.Exists:= True;

  FCharaNavigation.Exists:= True;

  if NOT Assigned(FVehicle) then Exit;

  if Assigned(FVehicle.RigidBody) then
    FVehicle.RigidBody.Dynamic:= False;

  FVehicle.Up:= Vector3(0.0, 1.0, 0.0);
  FVehicle.Headlight:= False;

  FVehicle:= nil;

  GroupSpeed.Exists:= False;
  SetUIColor(MainActor.PersonalColor);
end;

procedure TBaseViewRide.NavigationSetAnimation(
                           const Sender: TNyaBaseNavigation;
                           const AnimationName: String; AnimtionSpeed: Single);
begin
  FVehicle.AutoAnimation:= AnimationName;
  FVehicle.AnimationSpeed:= AnimtionSpeed;
end;

end.

