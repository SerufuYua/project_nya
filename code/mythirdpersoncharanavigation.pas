unit MyThirdPersonCharaNavigation;

{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, SysUtils, CastleCameras, CastleTransform, CastleClassUtils,
  CastleInputs, CastleVectors;

type
  TMyThirdPersonCharaNavigation = class(TCastleNavigation)
  protected
    FLookTargetDir: TVector3;
    FGravityAlignSpeed: Single;
    FTurnSpeed: Single;
    FWalkSpeed: Single;
    FWalkForce: Single;
    FWalkForceStart: Single;
    FWalkInAirForce: Single;
    FJumpImpulse: Single;
    FInput_Forward: TInputShortcut;
    FInput_Backward: TInputShortcut;
    FInput_Leftward: TInputShortcut;
    FInput_Rightward: TInputShortcut;
    FInput_Jump: TInputShortcut;
    FAvatarHierarchy: TCastleTransform;
    FAvatarHierarchyFreeObserver: TFreeNotificationObserver;
    procedure AvatarHierarchyFreeNotification(const Sender: TFreeNotificationObserver);
    procedure SetAvatarHierarchy(const Value: TCastleTransform);
  protected
    procedure CalcLookTargetDir;
    procedure RotateChara(const SecondsPassed: Single);
    procedure MoveChara(const SecondsPassed: Single);
    function IsOnGround(RBody: TCastleRigidBody;
                        CBody: TCastleCollider): Boolean;
  public
  const
    DefaultGravityAlignSpeed = 20000;
    DefaultTurnSpeed = 20;
    DefaultWalkSpeed = 40;
    DefaultWalkForce = 400.0;
    DefaultWalkForceStart = 40000.0;
    DefaultWalkInAirForce = 400.0;
    DefaultJumpImpulse = 50.0;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single;
                     var HandleInput: Boolean); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  public
    property Input_Forward: TInputShortcut read FInput_Forward;
    property Input_Backward: TInputShortcut read FInput_Backward;
    property Input_Leftward: TInputShortcut read FInput_Leftward;
    property Input_Rightward: TInputShortcut read FInput_Rightward;
    property Input_Jump: TInputShortcut read FInput_Jump;
  published
    property AvatarHierarchy: TCastleTransform read FAvatarHierarchy write SetAvatarHierarchy;
    property SpeedOfGravityAlign: Single read FGravityAlignSpeed write FGravityAlignSpeed
      {$ifdef FPC}default DefaultGravityAlignSpeed{$endif};
    property SpeedOfTurn: Single read FTurnSpeed write FTurnSpeed
      {$ifdef FPC}default DefaultTurnSpeed{$endif};
    property SpeedOfWalk: Single read FWalkSpeed write FWalkSpeed
      {$ifdef FPC}default DefaultWalkSpeed{$endif};
    property ForceOfWalk: Single read FWalkForce write FWalkForce
      {$ifdef FPC}default DefaultWalkForce{$endif};
    property ForceOfWalkStart: Single read FWalkForceStart write FWalkForceStart
      {$ifdef FPC}default DefaultWalkForceStart{$endif};
    property ForceOfWalkInAir: Single read FWalkInAirForce write FWalkInAirForce
      {$ifdef FPC}default DefaultWalkInAirForce{$endif};
    property ImpulseOfJump: Single read FJumpImpulse write FJumpImpulse
      {$ifdef FPC}default DefaultJumpImpulse{$endif};
  end;

implementation

uses
  CastleComponentSerialize, CastleUtils, CastleKeysMouse,
  CastleBoxes, MyVectorMath;

constructor TMyThirdPersonCharaNavigation.Create(AOwner: TComponent);
begin
  inherited;

  FInput_Forward               := TInputShortcut.Create(Self);
  FInput_Backward              := TInputShortcut.Create(Self);
  FInput_Leftward              := TInputShortcut.Create(Self);
  FInput_Rightward             := TInputShortcut.Create(Self);
  FInput_Jump                  := TInputShortcut.Create(Self);

  Input_Forward                .Assign(keyW, keyArrowUp);
  Input_Backward               .Assign(keyS, keyArrowDown);
  Input_Leftward               .Assign(keyA, keyArrowLeft);
  Input_Rightward              .Assign(keyD, keyArrowRight);
  Input_Jump                   .Assign(keySpace);

  Input_Forward                .SetSubComponent(true);
  Input_Backward               .SetSubComponent(true);
  Input_Leftward               .SetSubComponent(true);
  Input_Rightward              .SetSubComponent(true);
  Input_Jump                   .SetSubComponent(true);

  Input_Forward                .Name:= 'Input_Forward';
  Input_Backward               .Name:= 'Input_Backward';
  Input_Leftward               .Name:= 'Input_LeftRotate';
  Input_Rightward              .Name:= 'Input_RightRotate';
  Input_Jump                   .Name:= 'Input_Jump';

  FLookTargetDir:= TVector3.Zero;
  FGravityAlignSpeed:= DefaultGravityAlignSpeed;
  FTurnSpeed:= DefaultTurnSpeed;
  FWalkSpeed:= DefaultWalkSpeed;
  FWalkForce:= DefaultWalkForce;
  FWalkForceStart:= DefaultWalkForceStart;
  FWalkInAirForce:= DefaultWalkInAirForce;
  FJumpImpulse:= DefaultJumpImpulse;

  FAvatarHierarchyFreeObserver:= TFreeNotificationObserver.Create(Self);
  FAvatarHierarchyFreeObserver.OnFreeNotification:= {$ifdef FPC}@{$endif}AvatarHierarchyFreeNotification;
end;

destructor TMyThirdPersonCharaNavigation.Destroy;
begin
  AvatarHierarchy:= nil;
  inherited;
end;

procedure TMyThirdPersonCharaNavigation.Update(const SecondsPassed: Single;
                 var HandleInput: Boolean);
begin
  inherited;
  if NOT Valid then Exit;
  if NOT Assigned(AvatarHierarchy) then Exit;

  CalcLookTargetDir;
  RotateChara(SecondsPassed);
  MoveChara(SecondsPassed);
end;

procedure TMyThirdPersonCharaNavigation.CalcLookTargetDir;
var
  AvaVerticalDir, ForwardDir, BackwardDir, RightwardDir, LeftwardDir: TVector3;
begin
  AvaVerticalDir:= AvatarHierarchy.Up;
  LeftwardDir:= TVector3.CrossProduct(AvaVerticalDir, Camera.Direction);
  RightwardDir:= -LeftwardDir;
  BackwardDir:= TVector3.CrossProduct(AvaVerticalDir, LeftwardDir);
  ForwardDir:= -BackwardDir;

  FLookTargetDir:= TVector3.Zero;

  if Input_Forward.IsPressed(Container) then
    FLookTargetDir:= FLookTargetDir + ForwardDir;

  if Input_Backward.IsPressed(Container) then
    FLookTargetDir:= FLookTargetDir + BackwardDir;

  if Input_Leftward.IsPressed(Container) then
    FLookTargetDir:= FLookTargetDir + LeftwardDir;

  if Input_Rightward.IsPressed(Container) then
    FLookTargetDir:= FLookTargetDir + RightwardDir;

  if NOT FLookTargetDir.IsZero then
    FLookTargetDir:= FLookTargetDir.Normalize;
end;

procedure TMyThirdPersonCharaNavigation.RotateChara(const SecondsPassed: Single);
var
  TurnVec, AngularVelocity: TVector3;
  RBody: TCastleRigidBody;
begin
  RBody:= AvatarHierarchy.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
  if NOT Assigned(RBody) then Exit;

  AngularVelocity:= TVector3.Zero;

  { turn avatar up aganist gravity }
  TurnVec:= TurnVectorToVector(AvatarHierarchy.Up, Camera.GravityUp);
  AngularVelocity:= AngularVelocity + TurnVec * SpeedOfGravityAlign;


  { turn avatar to target }
  if NOT FLookTargetDir.IsZero then
  begin
    TurnVec:= TurnVectorToVector(AvatarHierarchy.Direction, FLookTargetDir);
    AngularVelocity:= AngularVelocity + TurnVec * SpeedOfTurn;
  end;

  RBody.AngularVelocity:= AngularVelocity;
end;

procedure TMyThirdPersonCharaNavigation.MoveChara(const SecondsPassed: Single);
var
  RBody: TCastleRigidBody;
  CBody: TCastleCollider;
  OnGround: Boolean;
  AvaDir, MoveForce: TVector3;
  ForwardVelocity: Single;
begin
  RBody:= AvatarHierarchy.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
  CBody:= AvatarHierarchy.FindBehavior(TCastleCollider) as TCastleCollider;
  if NOT (Assigned(RBody) OR Assigned(CBody)) then Exit;

  OnGround:= IsOnGround(RBody, CBody);
  AvaDir:= AvatarHierarchy.Direction;

  { movement }
  if ((NOT FLookTargetDir.IsZero) AND OnGround) then
  begin
    { movement on ground }
    ForwardVelocity:= ProjectionVectorAtoBLength(RBody.LinearVelocity,
                                                 AvaDir);
    if (ForwardVelocity < SpeedOfWalk) then
      MoveForce:= AvaDir * ForceOfWalkStart
    else
      MoveForce:= AvaDir * ForceOfWalk;

    RBody.AddForce(MoveForce, False);
  end else if ((NOT FLookTargetDir.IsZero) AND (NOT OnGround)) then
  begin
    { movement in air }
    MoveForce:= AvaDir * ForceOfWalkInAir;
    RBody.AddForce(MoveForce, False);
  end;

  { jump }
  if (FInput_Jump.IsPressed(Container) AND OnGround) then
  begin
    RBody.ApplyImpulse(AvatarHierarchy.Up * ImpulseOfJump,
                       AvatarHierarchy.BoundingBox.Center);
  end;

end;

function TMyThirdPersonCharaNavigation.IsOnGround(RBody: TCastleRigidBody;
                                                  CBody: TCastleCollider): Boolean;
var
  GroundRayCast: TPhysicsRayCastResult;
  AvatarBBox: TBox3D;
  AvatarRadius, DistanceToGround: Single;
  RayDirection, RayOrigin: TVector3;

  ForwardDir, BackwardDir, RightwardDir, LeftwardDir: TVector3;
begin
  AvatarBBox := AvatarHierarchy.BoundingBox;
  AvatarRadius:= AvatarBBox.MinSize / 2.0;
  DistanceToGround:= AvatarBBox.MaxSize * 0.1;
  RayOrigin:= AvatarHierarchy.Translation;
  RayDirection:= -AvatarHierarchy.Up;

  ForwardDir:= AvatarHierarchy.Direction.Normalize;
  BackwardDir:= -ForwardDir;
  RightwardDir:= TVector3.CrossProduct(AvatarHierarchy.Up,
                                       AvatarHierarchy.Direction).Normalize;
  LeftwardDir:= -RightwardDir;

  GroundRayCast:= RBody.PhysicsRayCast(RayOrigin,
                                       RayDirection,
                                       DistanceToGround);

  if NOT GroundRayCast.Hit then
    GroundRayCast:= RBody.PhysicsRayCast(RayOrigin + ForwardDir * AvatarRadius,
                                         RayDirection,
                                         DistanceToGround);

  if NOT GroundRayCast.Hit then
    GroundRayCast:= RBody.PhysicsRayCast(RayOrigin + BackwardDir * AvatarRadius,
                                         RayDirection,
                                         DistanceToGround);

  if NOT GroundRayCast.Hit then
    GroundRayCast:= RBody.PhysicsRayCast(RayOrigin + RightwardDir * AvatarRadius,
                                         RayDirection,
                                         DistanceToGround);

  if NOT GroundRayCast.Hit then
    GroundRayCast:= RBody.PhysicsRayCast(RayOrigin + LeftwardDir * AvatarRadius,
                                         RayDirection,
                                         DistanceToGround);

  Result:= GroundRayCast.Hit;
end;

function TMyThirdPersonCharaNavigation.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'AvatarHierarchy', 'SpeedOfWalk', 'SpeedOfTurn', 'SpeedOfGravityAlign',
       'ForceOfWalk', 'ForceOfWalkStart', 'ForceOfWalkInAir', 'ImpulseOfJump'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TMyThirdPersonCharaNavigation.AvatarHierarchyFreeNotification(
  const Sender: TFreeNotificationObserver);
begin
  AvatarHierarchy:= nil;
end;

procedure TMyThirdPersonCharaNavigation.SetAvatarHierarchy(const Value: TCastleTransform);
begin
  if (FAvatarHierarchy <> Value) then
  begin
    FAvatarHierarchy:= Value;
    FAvatarHierarchyFreeObserver.Observed:= Value;
  end;
end;

initialization
  RegisterSerializableComponent(TMyThirdPersonCharaNavigation, ['Navigation', 'My-Third-Person-Chara']);
end.

