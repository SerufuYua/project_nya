unit NyaBaseNavigation;

{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, SysUtils, CastleCameras, CastleTransform, CastleClassUtils,
  CastleInputs, CastleVectors, NyaMath;

type
  TNyaBaseNavigation = class;

  TNyaBaseNavigationAnimationEvent = procedure (
    const Sender: TNyaBaseNavigation;
    const AnimationName: String; AnimtionSpeed: Single) of object;

  TNyaBaseNavigation = class(TCastleNavigation)
  protected
    FGravityAlignSpeed: Single;
    FTurnSpeed: Single;
    FGravityForce: Single;
    FMoveInAirForce: Single;
    FInput_Forward: TInputShortcut;
    FInput_Backward: TInputShortcut;
    FInput_Leftward: TInputShortcut;
    FInput_Rightward: TInputShortcut;
    FOnAnimation: TNyaBaseNavigationAnimationEvent;
    FAvatarHierarchy: TCastleTransform;
    FAvatarHierarchyFreeObserver: TFreeNotificationObserver;
    procedure AvatarHierarchyFreeNotification(const Sender: TFreeNotificationObserver);
    procedure SetAvatarHierarchy(const Value: TCastleTransform);
  public
    const
      DefaultGravityAlignSpeed = 10.0;
      DefaultTurnSpeed = 20.0;
      DefaultJumpImpulse = 1.0;
      DefaultForceOfMoveInAir = 100.0;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsOnGround(RBody: TCastleRigidBody;
                        CBody: TCastleCollider): Boolean;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    property Input_Forward: TInputShortcut read FInput_Forward;
    property Input_Backward: TInputShortcut read FInput_Backward;
    property Input_Leftward: TInputShortcut read FInput_Leftward;
    property Input_Rightward: TInputShortcut read FInput_Rightward;
  published
    property AvatarHierarchy: TCastleTransform read FAvatarHierarchy write SetAvatarHierarchy;
    property SpeedOfGravityAlign: Single read FGravityAlignSpeed write FGravityAlignSpeed
             {$ifdef FPC}default DefaultGravityAlignSpeed{$endif};
    property SpeedOfTurn: Single read FTurnSpeed write FTurnSpeed
             {$ifdef FPC}default DefaultTurnSpeed{$endif};
    property ForceOfMoveInAir: Single read FMoveInAirForce write FMoveInAirForce
             {$ifdef FPC}default DefaultForceOfMoveInAir{$endif};

    property OnAnimation: TNyaBaseNavigationAnimationEvent
      read FOnAnimation write FOnAnimation;
  end;

implementation

uses
  CastleComponentSerialize, CastleUtils, CastleKeysMouse,
  CastleBoxes, NyaVectorMath, NyaActor;

constructor TNyaBaseNavigation.Create(AOwner: TComponent);
begin
  inherited;

  FInput_Forward               := TInputShortcut.Create(Self);
  FInput_Backward              := TInputShortcut.Create(Self);
  FInput_Leftward              := TInputShortcut.Create(Self);
  FInput_Rightward             := TInputShortcut.Create(Self);

  Input_Forward                .Assign(keyW, keyArrowUp);
  Input_Backward               .Assign(keyS, keyArrowDown);
  Input_Leftward               .Assign(keyA, keyArrowLeft);
  Input_Rightward              .Assign(keyD, keyArrowRight);

  Input_Forward                .SetSubComponent(true);
  Input_Backward               .SetSubComponent(true);
  Input_Leftward               .SetSubComponent(true);
  Input_Rightward              .SetSubComponent(true);

  Input_Forward                .Name:= 'Input_Forward';
  Input_Backward               .Name:= 'Input_Backward';
  Input_Leftward               .Name:= 'Input_Leftward';
  Input_Rightward              .Name:= 'Input_Rightward';

  FGravityAlignSpeed:= DefaultGravityAlignSpeed;
  FTurnSpeed:= DefaultTurnSpeed;
  FMoveInAirForce:= DefaultForceOfMoveInAir;

  FOnAnimation:= nil;

  FAvatarHierarchyFreeObserver:= TFreeNotificationObserver.Create(Self);
  FAvatarHierarchyFreeObserver.OnFreeNotification:= {$ifdef FPC}@{$endif}AvatarHierarchyFreeNotification;
end;

destructor TNyaBaseNavigation.Destroy;
begin
  AvatarHierarchy:= nil;
  FreeAndNil(FInput_Forward);
  FreeAndNil(FInput_Backward);
  FreeAndNil(FInput_Leftward);
  FreeAndNil(FInput_Rightward);
  inherited;
end;

function TNyaBaseNavigation.IsOnGround(RBody: TCastleRigidBody;
                                                  CBody: TCastleCollider): Boolean;
var
  GroundRayCast: TRayCastResult;
  AvatarBBox: TBox3D;
  ProbeLength: Single;
  RayDirection, RayOrigin: TVector3;
  ForwardDir, RightwardDir: TVector3;
begin
  AvatarBBox := AvatarHierarchy.BoundingBox;
  ProbeLength:=  (1.0 + 0.2) * AvatarBBox.SizeY / 2.0;
  RayOrigin:= AvatarBBox.Center;
  RayDirection:= -AvatarHierarchy.Up;

  ForwardDir:= AvatarHierarchy.Direction.Normalize;
  RightwardDir:= TVector3.CrossProduct(AvatarHierarchy.Up,
                                       AvatarHierarchy.Direction).Normalize;

  GroundRayCast:= RBody.PhysicsRayCast(RayOrigin,
                                       RayDirection,
                                       ProbeLength);

  if NOT GroundRayCast.Hit then
    GroundRayCast:= RBody.PhysicsRayCast(RayOrigin + ForwardDir * AvatarBBox.SizeZ / 2.0,
                                         RayDirection,
                                         ProbeLength);

  if NOT GroundRayCast.Hit then
    GroundRayCast:= RBody.PhysicsRayCast(RayOrigin - ForwardDir * AvatarBBox.SizeZ / 2.0,
                                         RayDirection,
                                         ProbeLength);

  if NOT GroundRayCast.Hit then
    GroundRayCast:= RBody.PhysicsRayCast(RayOrigin + RightwardDir * AvatarBBox.SizeX / 2.0,
                                         RayDirection,
                                         ProbeLength);

  if NOT GroundRayCast.Hit then
    GroundRayCast:= RBody.PhysicsRayCast(RayOrigin - RightwardDir * AvatarBBox.SizeX / 2.0,
                                         RayDirection,
                                         ProbeLength);

  if GroundRayCast.Hit then
    Exit((ProbeLength - GroundRayCast.Distance) > 0);

  Result:= False;
end;

function TNyaBaseNavigation.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'AvatarHierarchy', 'ForceOfMoveInAir',
       'SpeedOfTurn', 'SpeedOfGravityAlign'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

procedure TNyaBaseNavigation.AvatarHierarchyFreeNotification(
  const Sender: TFreeNotificationObserver);
begin
  AvatarHierarchy:= nil;
end;

procedure TNyaBaseNavigation.SetAvatarHierarchy(const Value: TCastleTransform);
begin
  if (FAvatarHierarchy <> Value) then
  begin
    FAvatarHierarchy:= Value;
    FAvatarHierarchyFreeObserver.Observed:= Value;
  end;
end;

end.

