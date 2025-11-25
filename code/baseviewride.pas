unit BaseViewRide;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleTransform, CastleKeysMouse, CastleUIControls,
  CastleSoundEngine, CastleControls, CastleVectors,
  BaseView, BaseViewTravel, NyaVehicleNavigation, NyaActor, NyaActorVehicle,
  NyaBaseNavigation;

type
  TBaseViewRide = class(TBaseViewTravel)
  published
    GroupSpeed: TCastleUserInterface;
    LabelSpeedValue: TCastleLabel;
  protected
    FWalkMusic, FRideMusic: TCastleSound;
    FGetOffSwitch, FLightSwitch: TKey;
    FTempCollider: TCastleCollider;
    FTempRBody: TCastleRigidBody;
    FTempActorParent: TCastleTransform;
    FVehicle: TNyaActorVehicle;
    FVehicleName: String;
    FVehicleNavigation: TNyaVehicleNavigation;
    FVehicleTrans: TVector3;
    FVehicleRot: TVector4;
    FVehicleNewTrans: Boolean;
    FVehicleNewRot: Boolean;
    procedure NavigationSetAnimation(
      const Sender: TNyaBaseNavigation;
      const AnimationName: String; AnimtionSpeed: Single);
    procedure SetVehicle(AVehicle: TNyaActorVehicle);
    procedure SetVehicleTrans(value: TVector3);
    procedure SetVehicleRot(value: TVector4);
  public
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
    procedure SitToVehicle(AVehicle: TNyaActorVehicle);
    procedure GetOffFromVehicle;
    procedure Pause; override;
    procedure Resume; override;
    procedure GetToRide(ALocation: TBaseViewRide;
                        AVehicle: String; { only vehicle name, not current object }
                        ATranslation: TVector3;
                        ARotation: TVector4);
    property Vehicle: TNyaActorVehicle read FVehicle write SetVehicle;
    property VehicleName: String read FVehicleName write FVehicleName;
    property VehicleTrans: TVector3 read FVehicleTrans write SetVehicleTrans;
    property VehicleRot: TVector4 read FVehicleRot write SetVehicleRot;
  end;

implementation

uses
  CastleScene, NyaThirdPersonCameraNavigation,
  NyaSwitch, NyaCastleUtils;

procedure TBaseViewRide.Start;
begin
  inherited;
  FTempCollider:=nil;
  FTempRBody:= nil;
  FTempActorParent:= nil;

  { set main actor with vechicle to preset position }
  Vehicle:= Map.DesignedComponent(VehicleName, False) as TNyaActorVehicle;

  if (Assigned(MainActor) AND Assigned(Vehicle))  then
  begin
    if FVehicleNewTrans then
      Vehicle.Translation:= VehicleTrans;
    if FVehicleNewRot then
      Vehicle.Rotation:= VehicleRot;

    SitToVehicle(Vehicle);
  end;

  FMainActorNewTrans:= False;
  FMainActorNewRot:= False;

  { set keys }
  FLightSwitch:= TKey.keyL;
  FGetOffSwitch:= TKey.keyR;
end;

procedure TBaseViewRide.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  { Executed every frame. }

  { Show Speed. Convert m/s to km/h }
  if Assigned(Vehicle) then
    LabelSpeedValue.Caption:= (Vehicle.ForwardVelocity * 60 * 60 / 1000).ToString(ffFixed, 3, 0);

  inherited;
end;

function TBaseViewRide.Press(const Event: TInputPressRelease): Boolean;
begin
  Result:= inherited;
  if Result then Exit; // allow the ancestor to handle keys

  { switch light }
  if Event.IsKey(FLightSwitch) then
  begin
    if Assigned(Vehicle) then
      Vehicle.Headlight:= NOT Vehicle.Headlight;
    Exit(true);
  end;

  { get off from Vehicle }
  if Event.IsKey(FGetOffSwitch) then
  begin
    GetOffFromVehicle;
    Exit(true);
  end;
end;

procedure TBaseViewRide.SitToVehicle(AVehicle: TNyaActorVehicle);
var
  camera: TNyaThirdPersonCameraNavigation;
  switch: TCastleBehavior;
begin
  Status.Caption:= '';
  Vehicle:= AVehicle;

  FTempRBody:= MainActor.RigidBody;
  MainActor.RemoveBehavior(MainActor.RigidBody);

  FTempCollider:= MainActor.Collider;
  MainActor.RemoveBehavior(MainActor.Collider);

  FCharaNavigation.Exists:= False;
  if (FCameraNavigation is TNyaThirdPersonCameraNavigation) then
  begin
    camera:= (FCameraNavigation as TNyaThirdPersonCameraNavigation);
    camera.AvatarHierarchy:= Vehicle;
    camera.FollowRotation:= True;
  end;

  switch:= Vehicle[1].FindBehavior(TNyaSwitch);
    if Assigned(switch) then
      switch.Parent.Exists:= False;

  FVehicleNavigation.AvatarHierarchy:= Vehicle;
  FVehicleNavigation.Exists:= True;

  if Assigned(Vehicle.RigidBody) then
    Vehicle.RigidBody.Dynamic:= True;

  FVehicleNavigation.OnAnimation:= {$ifdef FPC}@{$endif}NavigationSetAnimation;

  MainActor.Translation:= TVector3.Zero;
  MainActor.Rotation:= TVector4.Zero;
  FTempActorParent:= MainActor.Parent;
  MainActor.Parent:= Vehicle;

  Vehicle.Headlight:= True;

  GroupSpeed.Exists:= True;
  SetUIColor(Vehicle.PersonalColor);

  Notifications.Show('Info: use WASD to move');
  Notifications.Show('Info: use Space to brake');
  Notifications.Show('Info: use L to switch headlight');
  Notifications.Show('Info: use R to get off from the Vehicle');

  { Play Ride music }
  SoundEngine.LoopingChannel[0].Sound:= FRideMusic;
end;

procedure TBaseViewRide.GetOffFromVehicle;
var
  camera: TNyaThirdPersonCameraNavigation;
  switch: TCastleBehavior;
begin
  if NOT Assigned(Vehicle) then Exit;

  if Assigned(Vehicle.RigidBody) then
  begin
    if NOT (Vehicle.RigidBody.LinearVelocity.IsZero(0.1) AND
            Vehicle.RigidBody.AngularVelocity.IsZero(0.1)) then
      begin
          Notifications.Show('Stop movement before get off from the Vehicle');
          Exit;
      end;
  end;

  FVehicleNavigation.Exists:= False;
  FVehicleNavigation.Parking;
  FVehicleNavigation.OnAnimation:= nil;

  MainActor.Parent:= FTempActorParent;
  MainActor.Translation:= Vehicle.Translation;
  MainActor.Translate(Vector3(1.0, 0.0, 0.0));
  MainActor.Rotation:= Vehicle.Rotation;
  MainActor.Up:= Vector3(0.0, 1.0, 0.0);

  MainActor.AddBehavior(FTempCollider);
  MainActor.AddBehavior(FTempRBody);

  if (FCameraNavigation is TNyaThirdPersonCameraNavigation) then
  begin
    camera:= (FCameraNavigation as TNyaThirdPersonCameraNavigation);
    camera.AvatarHierarchy:= MainActor;
    camera.FollowRotation:= False;
  end;

  switch:= Vehicle[1].FindBehavior(TNyaSwitch);
    if Assigned(switch) then
      switch.Parent.Exists:= True;

  FCharaNavigation.Exists:= True;

  if NOT Assigned(Vehicle) then Exit;

  if Assigned(Vehicle.RigidBody) then
    Vehicle.RigidBody.Dynamic:= False;

  Vehicle.Up:= Vector3(0.0, 1.0, 0.0);
  Vehicle.Headlight:= False;

  Vehicle:= nil;

  GroupSpeed.Exists:= False;
  SetUIColor(MainActor.PersonalColor);

  { Play Walk music }
  SoundEngine.LoopingChannel[0].Sound:= FWalkMusic;
end;

procedure TBaseViewRide.Pause;
begin
  inherited;
  if Assigned(FVehicleNavigation) then
    FVehicleNavigation.Exists:= False;
end;

procedure TBaseViewRide.Resume;
begin
  inherited;
  if (Assigned(FVehicleNavigation) AND Assigned(Vehicle)) then
    FVehicleNavigation.Exists:= True;
end;

procedure TBaseViewRide.NavigationSetAnimation(
                           const Sender: TNyaBaseNavigation;
                           const AnimationName: String; AnimtionSpeed: Single);
begin
  Vehicle.AutoAnimation:= AnimationName;
  Vehicle.AnimationSpeed:= AnimtionSpeed;
end;

procedure TBaseViewRide.GetToRide(ALocation: TBaseViewRide;
                                  AVehicle: String;
                                  ATranslation: TVector3;
                                  ARotation: TVector4);
begin
  ALocation.VehicleName:= AVehicle;
  ALocation.VehicleTrans:= ATranslation;
  ALocation.VehicleRot:= ARotation;
  FGetToGo:= ALocation;
end;

procedure TBaseViewRide.SetVehicle(AVehicle: TNyaActorVehicle);
begin
  if (FVehicle = AVehicle) then Exit;

  FVehicle:= AVehicle;
end;

procedure TBaseViewRide.SetVehicleTrans(value: TVector3);
begin
  FVehicleTrans:= value;
  FVehicleNewTrans:= True;
end;

procedure TBaseViewRide.SetVehicleRot(value: TVector4);
begin
  FVehicleRot:= value;
  FVehicleNewRot:= True;
end;

end.

