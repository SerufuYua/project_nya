unit MyThirdPersonCharaNavigation;

{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, SysUtils, CastleCameras, CastleTransform, CastleClassUtils,
  CastleInputs, CastleVectors, MyMath;

type
  TMyThirdPersonCharaNavigation = class;

  TMyThirdPersonCharaNavigationAnimationEvent = procedure (
    const Sender: TMyThirdPersonCharaNavigation;
    const AnimationName: String; AnimtionSpeed: Single) of object;

  TMyThirdPersonCharaNavigation = class(TCastleNavigation)
  protected
    FVelocityNoiseSuppressor: TNoiseSuppressor;
    FAnimationStand: String;
    FAnimationWalk: String;
    FAnimationRun: String;
    FLookTargetDir: TVector3;
    FAvatarPos: TVector3;
    FGravityAlignSpeed: Single;
    FTurnSpeed: Single;
    FWalkSpeed: Single;
    FRunSpeed: Single;
    FJumpSpeed: Single;
    FMoveInAirForce: Single;
    FGravityForce: Single;
    FJumpImpulse: Single;
    FInput_Forward: TInputShortcut;
    FInput_Backward: TInputShortcut;
    FInput_Leftward: TInputShortcut;
    FInput_Rightward: TInputShortcut;
    FInput_FastMove: TInputShortcut;
    FInput_Jump: TInputShortcut;
    FOnAnimation: TMyThirdPersonCharaNavigationAnimationEvent;
    FAvatarHierarchy: TCastleTransform;
    FAvatarHierarchyFreeObserver: TFreeNotificationObserver;
    procedure AvatarHierarchyFreeNotification(const Sender: TFreeNotificationObserver);
    procedure SetAvatarHierarchy(const Value: TCastleTransform);
  protected
    procedure CalcLookTargetDir;
    procedure RotateChara(const SecondsPassed: Single);
    procedure MoveChara(const SecondsPassed: Single; out OnGround: Boolean);
    function IsOnGround(RBody: TCastleRigidBody;
                        CBody: TCastleCollider): Boolean;
    procedure Animate(const SecondsPassed: Single; const OnGround: Boolean);

    function AnimationStandStored: Boolean;
    function AnimationWalkStored: Boolean;
    function AnimationRunStored: Boolean;
  public
  const
    DefaultGravityAlignSpeed = 20000;
    DefaultTurnSpeed = 20.0;
    DefaultWalkSpeed = 30.0;
    DefaultRunSpeed = 100.0;
    DefaultJumpSpeed = 10000.0;
    DefaultMoveInAirForce = 4.0;
    DefaultGravityForce = 500.0;
    DefaultJumpImpulse = 50.0;
    DefaultAnimationStand = 'stand';
    DefaultAnimationWalk = 'walk';
    DefaultAnimationRun = 'run';

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
    property Input_FastMove: TInputShortcut read FInput_FastMove;
    property Input_Jump: TInputShortcut read FInput_Jump;
  published
    property AvatarHierarchy: TCastleTransform read FAvatarHierarchy write SetAvatarHierarchy;
    property SpeedOfGravityAlign: Single read FGravityAlignSpeed write FGravityAlignSpeed
             {$ifdef FPC}default DefaultGravityAlignSpeed{$endif};
    property SpeedOfTurn: Single read FTurnSpeed write FTurnSpeed
             {$ifdef FPC}default DefaultTurnSpeed{$endif};
    property SpeedOfWalk: Single read FWalkSpeed write FWalkSpeed
             {$ifdef FPC}default DefaultWalkSpeed{$endif};
    property SpeedOfRun: Single read FRunSpeed write FRunSpeed
             {$ifdef FPC}default DefaultRunSpeed{$endif};
    property SpeedOfJump: Single read FJumpSpeed write FJumpSpeed
             {$ifdef FPC}default DefaultJumpSpeed{$endif};
    property ForceOfMoveInAir: Single read FMoveInAirForce write FMoveInAirForce
             {$ifdef FPC}default DefaultMoveInAirForce{$endif};
    property ForceOfGravity: Single read FGravityForce write FGravityForce
             {$ifdef FPC}default DefaultGravityForce{$endif};
    property ImpulseOfJump: Single read FJumpImpulse write FJumpImpulse
             {$ifdef FPC}default DefaultJumpImpulse{$endif};

    property AnimationStand: String read FAnimationStand write FAnimationStand
             stored AnimationStandStored nodefault;
    property AnimationWalk: String read FAnimationWalk write FAnimationWalk
             stored AnimationWalkStored nodefault;
    property AnimationRun: String read FAnimationRun write FAnimationRun
             stored AnimationRunStored nodefault;

    property OnAnimation: TMyThirdPersonCharaNavigationAnimationEvent
      read FOnAnimation write FOnAnimation;
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
  FInput_FastMove              := TInputShortcut.Create(Self);
  FInput_Jump                  := TInputShortcut.Create(Self);

  Input_Forward                .Assign(keyW, keyArrowUp);
  Input_Backward               .Assign(keyS, keyArrowDown);
  Input_Leftward               .Assign(keyA, keyArrowLeft);
  Input_Rightward              .Assign(keyD, keyArrowRight);
  Input_FastMove               .Assign(keyShift);
  Input_Jump                   .Assign(keySpace);

  Input_Forward                .SetSubComponent(true);
  Input_Backward               .SetSubComponent(true);
  Input_Leftward               .SetSubComponent(true);
  Input_Rightward              .SetSubComponent(true);
  Input_FastMove               .SetSubComponent(true);
  Input_Jump                   .SetSubComponent(true);

  Input_Forward                .Name:= 'Input_Forward';
  Input_Backward               .Name:= 'Input_Backward';
  Input_Leftward               .Name:= 'Input_Leftward';
  Input_Rightward              .Name:= 'Input_Rightward';
  Input_FastMove               .Name:= 'Input_FastMove';
  Input_Jump                   .Name:= 'Input_Jump';

  FVelocityNoiseSuppressor:= TNoiseSuppressor.Create;

  FLookTargetDir:= TVector3.Zero;
  FAvatarPos:= TVector3.Zero;
  FGravityAlignSpeed:= DefaultGravityAlignSpeed;
  FTurnSpeed:= DefaultTurnSpeed;
  FWalkSpeed:= DefaultWalkSpeed;
  FRunSpeed:= DefaultRunSpeed;
  FJumpSpeed:= DefaultJumpSpeed;
  FMoveInAirForce:= DefaultMoveInAirForce;
  FGravityForce:= DefaultGravityForce;
  FJumpImpulse:= DefaultJumpImpulse;

  FOnAnimation:= nil;
  FAnimationStand:= DefaultAnimationStand;
  FAnimationWalk:= DefaultAnimationWalk;
  FAnimationRun:= DefaultAnimationRun;

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
var
  OnGround: Boolean;
begin
  inherited;
  if NOT Valid then Exit;
  if NOT Assigned(AvatarHierarchy) then Exit;

  OnGround:= False;

  CalcLookTargetDir;
  RotateChara(SecondsPassed);
  MoveChara(SecondsPassed, OnGround);

  Animate(SecondsPassed, OnGround);
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

procedure TMyThirdPersonCharaNavigation.MoveChara(const SecondsPassed: Single;
                                                  out OnGround: Boolean);
var
  RBody: TCastleRigidBody;
  CBody: TCastleCollider;
  AvaDir, GravityVelocity: TVector3;
begin
  RBody:= AvatarHierarchy.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
  CBody:= AvatarHierarchy.FindBehavior(TCastleCollider) as TCastleCollider;
  if NOT (Assigned(RBody) OR Assigned(CBody)) then Exit;

  OnGround:= IsOnGround(RBody, CBody);
  AvaDir:= AvatarHierarchy.Direction;

  { save Velocity from gravity }
  GravityVelocity:= ProjectionVectorAtoB(RBody.LinearVelocity,
                                         -Camera.GravityUp);

  if NOT FLookTargetDir.IsZero then
  begin
    { movement }
    if OnGround then
    begin
      { movement on ground }
      if Input_FastMove.IsPressed(Container) then
        { walk }
        RBody.LinearVelocity:= AvaDir * SpeedOfRun + GravityVelocity
      else
        { run }
        RBody.LinearVelocity:= AvaDir * SpeedOfWalk + GravityVelocity;
    end else
      { movement in air }
      RBody.AddForce(AvaDir * ForceOfMoveInAir, False);
  end;

  { jump }
  if FInput_Jump.IsPressed(Container) AND OnGround then
    RBody.LinearVelocity:= RBody.LinearVelocity +
                           (AvatarHierarchy.Up + FLookTargetDir) * SpeedOfJump +
                           GravityVelocity;

  { gravity }
  RBody.AddForce(-Camera.GravityUp * ForceOfGravity, False);
end;

function TMyThirdPersonCharaNavigation.IsOnGround(RBody: TCastleRigidBody;
                                                  CBody: TCastleCollider): Boolean;
var
  GroundRayCast: TPhysicsRayCastResult;
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

procedure TMyThirdPersonCharaNavigation.Animate(const SecondsPassed: Single;
                                                const OnGround: Boolean);
var
  RBody: TCastleRigidBody;
  ForwardVelocity, RealForwardVelocity: Single;
begin
  if NOT Assigned(OnAnimation) then Exit;
  RBody:= AvatarHierarchy.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
  if NOT Assigned(RBody) then Exit;

  { calculate Velocity from Rigid Body }
  ForwardVelocity:= ProjectionVectorAtoBLength(RBody.LinearVelocity,
                                               AvatarHierarchy.Direction);

  { calculate real Velocity from Avatar Hierarchy }
  RealForwardVelocity:= ProjectionVectorAtoBLength((AvatarHierarchy.Translation - FAvatarPos) / SecondsPassed,
                                                   AvatarHierarchy.Direction);
  FAvatarPos:= AvatarHierarchy.Translation;

  FVelocityNoiseSuppressor.Update(RealForwardVelocity, 8);
  RealForwardVelocity:= FVelocityNoiseSuppressor.Value;

  { processing animations }
  if OnGround then
  begin
    if RealForwardVelocity < 0.2 * SpeedOfWalk then
      OnAnimation(self, AnimationStand, 1.0)
    else if (ForwardVelocity < (SpeedOfWalk + SpeedOfRun) / 2.0) then
      OnAnimation(self, AnimationWalk, ForwardVelocity / SpeedOfWalk)
    else
      OnAnimation(self, AnimationRun, ForwardVelocity / SpeedOfRun);
  end else
    OnAnimation(self, AnimationStand, 1.0);

end;

function TMyThirdPersonCharaNavigation.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'AvatarHierarchy', 'SpeedOfWalk', 'SpeedOfRun', 'SpeedOfJump',
       'SpeedOfTurn', 'SpeedOfGravityAlign', 'ForceOfGravity',
       'ForceOfMoveInAir', 'ImpulseOfJump', 'AnimationStand', 'AnimationWalk',
       'AnimationRun'
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

function TMyThirdPersonCharaNavigation.AnimationStandStored: Boolean;
begin
  Result := FAnimationStand <> DefaultAnimationStand;
end;

function TMyThirdPersonCharaNavigation.AnimationWalkStored: Boolean;
begin
  Result := FAnimationWalk <> DefaultAnimationWalk;
end;

function TMyThirdPersonCharaNavigation.AnimationRunStored: Boolean;
begin
  Result := FAnimationRun <> DefaultAnimationRun;
end;

initialization
  RegisterSerializableComponent(TMyThirdPersonCharaNavigation, ['Navigation', 'My-Third-Person-Chara']);
end.

