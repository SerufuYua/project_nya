unit BaseViewRideNew;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleTransform, NyaVehicleNavigation,
  BaseViewTravel, NyaActor, NyaActorVehicle, NyaBaseNavigation;

type
  TBaseViewRideNew = class(TBaseViewTravel)
  protected
    FTempCollider: TCastleCollider;
    FTempRBody: TCastleRigidBody;
    FVehicle: TNyaActor;
    FVehicleNavigation: TNyaVehicleNavigation;
    procedure NavigationSetAnimation(
      const Sender: TNyaBaseNavigation;
      const AnimationName: String; AnimtionSpeed: Single);
  public
    procedure SitToVehicle(vehicle: TNyaActorVehicle);
  end;

implementation

uses
  CastleVectors, CastleScene, NyaThirdPersonCameraNavigation,
  NyaSwitch, NyaCastleUtils;

procedure TBaseViewRideNew.SitToVehicle(vehicle: TNyaActorVehicle);
var
  camera: TNyaThirdPersonCameraNavigation;
  switch: TCastleBehavior;
begin
  Status.Caption:= '';
  FVehicle:= vehicle;

{  if Assigned(MainActor.RigidBody) then
    MainActor.RigidBody.Exists:= False;   }

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
  MainActor.Parent:= FVehicle;
end;

procedure TBaseViewRideNew.NavigationSetAnimation(
                           const Sender: TNyaBaseNavigation;
                           const AnimationName: String; AnimtionSpeed: Single);
begin
  FVehicle.AutoAnimation:= AnimationName;
  FVehicle.AnimationSpeed:= AnimtionSpeed;
end;

end.

