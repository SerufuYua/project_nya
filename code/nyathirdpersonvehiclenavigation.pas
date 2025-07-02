unit NyaThirdPersonVehicleNavigation;

{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, SysUtils, CastleCameras, CastleTransform, CastleClassUtils,
  CastleInputs, CastleVectors;

type
  TNyaThirdPersonVehicleNavigation = class;

  TNyaThirdPersonVehicleNavigationAnimationEvent = procedure (
    const Sender: TNyaThirdPersonVehicleNavigation;
    const AnimationName: String; AnimtionSpeed: Single) of object;

  TNyaThirdPersonVehicleNavigation = class(TCastleNavigation)
  protected
    FMoveVelocity: Single;
    FAnimationStand: String;
    FAnimationMoveFwd: String;
    FAnimationTurnRight: String;
    FAnimationTurnLeft: String;
    FGravityAlignSpeed: Single;
    FTurnSpeed: Single;
    FRollFactor: Single;
    FForceOfMove: Single;
    FMoveSpeedAnimation: Single;
    FJumpSpeed: Single;
    FMoveInAirForce: Single;
    FGravityForce: Single;
    FJumpImpulse: Single;
    FInput_Forward: TInputShortcut;
    FInput_Backward: TInputShortcut;
    FInput_Leftward: TInputShortcut;
    FInput_Rightward: TInputShortcut;
    FInput_Jump: TInputShortcut;
    FOnAnimation: TNyaThirdPersonVehicleNavigationAnimationEvent;
    FAvatarHierarchy: TCastleTransform;
    FAvatarHierarchyFreeObserver: TFreeNotificationObserver;
    procedure AvatarHierarchyFreeNotification(const Sender: TFreeNotificationObserver);
    procedure SetAvatarHierarchy(const Value: TCastleTransform);
  protected
    procedure RotateVehicle(RBody: TCastleRigidBody; const SecondsPassed: Single; const OnGround: Boolean; const FwdVelocity: Single);
    procedure MoveVehicle(RBody: TCastleRigidBody; CBody: TCastleCollider; const SecondsPassed: Single; const OnGround: Boolean);
    procedure Animate(const SecondsPassed: Single; const OnGround: Boolean; const FwdVelocity: Single);

    function WinFuncRotation(const value: Single): Single;

    function AnimationStandStored: Boolean;
    function AnimationMoveFwdStored: Boolean;
    function AnimationTurnRightStored: Boolean;
    function AnimationTurnLeftStored: Boolean;
  public
    const
      DefaultGravityAlignSpeed = 200.0;
      DefaultTurnSpeed = 2.0;
      DefaultRollFactor = 0.7;
      DefaultForceOfMove = 100.0;
      DefaultForceOfMoveInAir = 1.0;
      DefaultMoveSpeedAnimation = 25.0;
      DefaultJumpSpeed = 1.0;
      DefaultJumpImpulse = 1.0;
      DefaultAnimationStand = 'stand';
      DefaultAnimationMoveFwd = 'move';
      DefaultAnimationTurnRight = 'turn_right';
      DefaultAnimationTurnLeft = 'turn_left';

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single;
                     var HandleInput: Boolean); override;
    function IsOnGround(RBody: TCastleRigidBody;
                        CBody: TCastleCollider): Boolean;
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
    property RollFactor: Single read FRollFactor write FRollFactor
             {$ifdef FPC}default DefaultRollFactor{$endif};
    property ForceOfMove: Single read FForceOfMove write FForceOfMove
             {$ifdef FPC}default DefaultForceOfMove{$endif};
    property SpeedOfMoveAnimation: Single read FMoveSpeedAnimation write FMoveSpeedAnimation
             {$ifdef FPC}default DefaultMoveSpeedAnimation{$endif};
    property SpeedOfJump: Single read FJumpSpeed write FJumpSpeed
             {$ifdef FPC}default DefaultJumpSpeed{$endif};
    property ForceOfMoveInAir: Single read FMoveInAirForce write FMoveInAirForce
             {$ifdef FPC}default DefaultForceOfMoveInAir{$endif};
    property ImpulseOfJump: Single read FJumpImpulse write FJumpImpulse
             {$ifdef FPC}default DefaultJumpImpulse{$endif};

    property AnimationStand: String read FAnimationStand write FAnimationStand
             stored AnimationStandStored nodefault;
    property AnimationMoveFwd: String read FAnimationMoveFwd write FAnimationMoveFwd
             stored AnimationMoveFwdStored nodefault;
    property AnimationTurnRight: String read FAnimationTurnRight write FAnimationTurnRight
             stored AnimationTurnRightStored nodefault;
    property AnimationTurnLeft: String read FAnimationTurnLeft write FAnimationTurnLeft
             stored AnimationTurnLeftStored nodefault;

    property OnAnimation: TNyaThirdPersonVehicleNavigationAnimationEvent
      read FOnAnimation write FOnAnimation;
  end;

implementation

uses
  CastleComponentSerialize, CastleUtils, CastleKeysMouse,
  CastleBoxes, NyaVectorMath, NyaActor, NyaCastleUtils, Math
  {$ifdef CASTLE_DESIGN_MODE}
  , PropEdits, CastlePropEdits
  {$endif};

const
  MoveTreshold = 0.02;

constructor TNyaThirdPersonVehicleNavigation.Create(AOwner: TComponent);
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
  Input_Leftward               .Name:= 'Input_Leftward';
  Input_Rightward              .Name:= 'Input_Rightward';
  Input_Jump                   .Name:= 'Input_Jump';

  FMoveVelocity:= 0.0;
  FGravityAlignSpeed:= DefaultGravityAlignSpeed;
  FTurnSpeed:= DefaultTurnSpeed;
  FRollFactor:= DefaultRollFactor;
  FForceOfMove:= DefaultForceOfMove;
  FMoveSpeedAnimation:= DefaultMoveSpeedAnimation;
  FJumpSpeed:= DefaultJumpSpeed;
  FMoveInAirForce:= DefaultForceOfMoveInAir;
  FJumpImpulse:= DefaultJumpImpulse;

  FOnAnimation:= nil;
  FAnimationStand:= DefaultAnimationStand;
  FAnimationMoveFwd:= DefaultAnimationMoveFwd;
  FAnimationTurnRight:= DefaultAnimationTurnRight;

  FAvatarHierarchyFreeObserver:= TFreeNotificationObserver.Create(Self);
  FAvatarHierarchyFreeObserver.OnFreeNotification:= {$ifdef FPC}@{$endif}AvatarHierarchyFreeNotification;
end;

destructor TNyaThirdPersonVehicleNavigation.Destroy;
begin
  AvatarHierarchy:= nil;
  inherited;
end;

procedure TNyaThirdPersonVehicleNavigation.Update(const SecondsPassed: Single;
                                                  var HandleInput: Boolean);
var
  RBody: TCastleRigidBody;
  CBody: TCastleCollider;
  onGround: Boolean;
  moveVelocity: Single;
begin
  inherited;
  if NOT Valid then Exit;
  if NOT Assigned(AvatarHierarchy) then Exit;

  RBody:= AvatarHierarchy.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
  CBody:= AvatarHierarchy.FindBehavior(TCastleCollider) as TCastleCollider;
  if NOT (Assigned(RBody) OR Assigned(CBody)) then Exit;

  { calculate on ground condition }
  onGround:= IsOnGround(RBody, CBody);

  { calculate real Velocity from Avatar Hierarchy }
  if (AvatarHierarchy is TNyaActor) then
    moveVelocity:= (AvatarHierarchy as TNyaActor).ForwardVelocity
  else
    moveVelocity:= ProjectionVectorAtoBLength(RBody.LinearVelocity,
                                              AvatarHierarchy.Direction);


  RotateVehicle(RBody, SecondsPassed, onGround, moveVelocity);
  MoveVehicle(RBody, CBody, SecondsPassed, onGround);
  Animate(SecondsPassed, onGround, moveVelocity);
end;

procedure TNyaThirdPersonVehicleNavigation.RotateVehicle(RBody: TCastleRigidBody;
                                                         const SecondsPassed: Single;
                                                         const OnGround: Boolean;
                                                         const FwdVelocity: Single);
var
  gravAlign, turn, gravityUp, sideDir, gravSideDir: TVector3;
  speedFactor: Single;
begin
  gravAlign:= TVector3.Zero;
  turn:= TVector3.Zero;

  if (RBody.MaxLinearVelocity <> 0.0) then
    speedFactor:= FwdVelocity / RBody.MaxLinearVelocity
  else
    speedFactor:= FwdVelocity;

  { turn avatar up aganist gravity only around forward direction }
  gravityUp:= Camera.GravityUp;
  sideDir:= TVector3.CrossProduct(AvatarHierarchy.Up, AvatarHierarchy.Direction).Normalize;

  { add rolling factor from turns turns }
  if (Abs(FwdVelocity) > MoveTreshold * SpeedOfMoveAnimation) then
  begin
    if Input_Leftward.IsPressed(Container) then
      gravityUp:= gravityUp + ( sideDir * RollFactor - AvatarHierarchy.Direction) * speedFactor
    else if Input_Rightward.IsPressed(Container) then
      gravityUp:= gravityUp + (-sideDir * RollFactor - AvatarHierarchy.Direction) * speedFactor;
  end;

  gravSideDir:= TVector3.CrossProduct(gravityUp, AvatarHierarchy.Direction).Normalize;
  gravAlign:= TurnVectorToVector(sideDir, gravSideDir) * SpeedOfGravityAlign;

  { turn avatar }
  if OnGround then
  begin
    if Input_Leftward.IsPressed(Container) then
      turn:=  AvatarHierarchy.Up * SpeedOfTurn * WinFuncRotation(speedFactor)
    else if Input_Rightward.IsPressed(Container) then
      turn:= -AvatarHierarchy.Up * SpeedOfTurn * WinFuncRotation(speedFactor);
  end;

  RBody.AngularVelocity:= gravAlign + turn;
end;

procedure TNyaThirdPersonVehicleNavigation.MoveVehicle(RBody: TCastleRigidBody;
                                                       CBody: TCastleCollider;
                                                       const SecondsPassed: Single;
                                                       const OnGround: Boolean);
var
  avaDir, gravityVelocity, sideVelocity: TVector3;
begin
  avaDir:= AvatarHierarchy.Direction;

  { save Velocity from gravity }
  gravityVelocity:= ProjectionVectorAtoB(RBody.LinearVelocity,
                                         -Camera.GravityUp);

  { movement }
  if Input_Forward.IsPressed(Container) then
  begin
    if OnGround then
      { movement on ground }
      RBody.AddForce(avaDir * ForceOfMove, False)
    else
      { movement in air }
      RBody.AddForce(avaDir * ForceOfMoveInAir, False);
  end
  else if Input_Backward.IsPressed(Container) then
  begin
    if OnGround then
      { movement on ground }
      RBody.AddForce(avaDir * (-ForceOfMove) * 0.5, False)
    else
      { movement in air }
      RBody.AddForce(avaDir * ForceOfMoveInAir, False);
  end;


  { jump }
  if FInput_Jump.IsPressed(Container) AND OnGround then
    RBody.LinearVelocity:= RBody.LinearVelocity +
                           AvatarHierarchy.Up * SpeedOfJump +
                           gravityVelocity;

  { zero side velocity }
  if OnGround then
  begin
      sideVelocity:= ProjectionVectorAtoB(RBody.LinearVelocity,
        TVector3.CrossProduct(AvatarHierarchy.Up, AvatarHierarchy.Direction).Normalize);
      RBody.LinearVelocity:= RBody.LinearVelocity - sideVelocity * 0.5;
  end;
end;

procedure TNyaThirdPersonVehicleNavigation.Animate(const SecondsPassed: Single;
                                                   const OnGround: Boolean;
                                                   const FwdVelocity: Single);
begin
  if NOT Assigned(OnAnimation) then Exit;

  { processing animations }
  if (Input_Forward.IsPressed(Container) OR
      (Abs(FwdVelocity) > MoveTreshold * SpeedOfMoveAnimation)) then
  begin
    { move animation }
    if Input_Rightward.IsPressed(Container) then
      OnAnimation(self, AnimationTurnRight, FwdVelocity / SpeedOfMoveAnimation)
    else if Input_Leftward.IsPressed(Container) then
      OnAnimation(self, AnimationTurnLeft, FwdVelocity / SpeedOfMoveAnimation)
    else
      OnAnimation(self, AnimationMoveFwd, FwdVelocity / SpeedOfMoveAnimation);
  end else
    { stand }
    if OnGround then
      OnAnimation(self, AnimationStand, 1.0)
    else
      OnAnimation(self, AnimationMoveFwd, 1.0);


end;

function TNyaThirdPersonVehicleNavigation.WinFuncRotation(const value: Single): Single;
var
  x, dir: Single;
begin
  x:= Abs(value);
  dir:= Sign(value);

  if (x < 0.25) then
    Result:= 4.0 * x
  else
    Result:= -0.5333 * x + 1.1333;

  Result:= Result * dir;
end;

function TNyaThirdPersonVehicleNavigation.IsOnGround(RBody: TCastleRigidBody;
                                                     CBody: TCastleCollider): Boolean;
var
  groundRayCast: TRayCastResult;
  avatarBBox: TBox3D;
  probeLength: Single;
  rayDirection, rayOrigin: TVector3;
  forwardDir, rightwardDir: TVector3;
begin
  avatarBBox := AvatarHierarchy.BoundingBox;
  probeLength:=  (1.0 + 0.2) * avatarBBox.SizeY / 2.0;
  rayOrigin:= avatarBBox.Center;
  rayDirection:= -AvatarHierarchy.Up;

  forwardDir:= AvatarHierarchy.Direction.Normalize;
  rightwardDir:= TVector3.CrossProduct(AvatarHierarchy.Up,
                                       AvatarHierarchy.Direction).Normalize;

  groundRayCast:= RBody.PhysicsRayCast(rayOrigin,
                                       rayDirection,
                                       probeLength);

  if NOT groundRayCast.Hit then
    groundRayCast:= RBody.PhysicsRayCast(rayOrigin + forwardDir * avatarBBox.SizeZ / 2.0,
                                         rayDirection,
                                         probeLength);

  if NOT groundRayCast.Hit then
    groundRayCast:= RBody.PhysicsRayCast(rayOrigin - forwardDir * avatarBBox.SizeZ / 2.0,
                                         rayDirection,
                                         probeLength);

  if NOT groundRayCast.Hit then
    groundRayCast:= RBody.PhysicsRayCast(rayOrigin + rightwardDir * avatarBBox.SizeX / 2.0,
                                         rayDirection,
                                         probeLength);

  if NOT groundRayCast.Hit then
    groundRayCast:= RBody.PhysicsRayCast(rayOrigin - rightwardDir * avatarBBox.SizeX / 2.0,
                                         rayDirection,
                                         probeLength);

  if groundRayCast.Hit then
    Exit((probeLength - groundRayCast.Distance) > 0);

  Result:= False;
end;

function TNyaThirdPersonVehicleNavigation.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'AvatarHierarchy', 'ForceOfMove', 'SpeedOfMoveAnimation',
       'SpeedOfRun', 'SpeedOfRunAnimation', 'SpeedOfJump',
       'SpeedOfTurn', 'RollFactor', 'SpeedOfGravityAlign', 'ForceOfGravity',
       'ForceOfMoveInAir', 'ImpulseOfJump', 'AnimationStand', 'AnimationMoveFwd',
       'AnimationTurnRight', 'AnimationTurnLeft'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

procedure TNyaThirdPersonVehicleNavigation.AvatarHierarchyFreeNotification(
  const Sender: TFreeNotificationObserver);
begin
  AvatarHierarchy:= nil;
end;

procedure TNyaThirdPersonVehicleNavigation.SetAvatarHierarchy(const Value: TCastleTransform);
begin
  if (FAvatarHierarchy <> Value) then
  begin
    FAvatarHierarchy:= Value;
    FAvatarHierarchyFreeObserver.Observed:= Value;
  end;
end;

function TNyaThirdPersonVehicleNavigation.AnimationStandStored: Boolean;
begin
  Result:= FAnimationStand <> DefaultAnimationStand;
end;

function TNyaThirdPersonVehicleNavigation.AnimationMoveFwdStored: Boolean;
begin
  Result:= FAnimationMoveFwd <> DefaultAnimationMoveFwd;
end;

function TNyaThirdPersonVehicleNavigation.AnimationTurnRightStored: Boolean;
begin
  Result:= FAnimationTurnRight <> DefaultAnimationTurnRight;
end;

function TNyaThirdPersonVehicleNavigation.AnimationTurnLeftStored: Boolean;
begin
  Result:= FAnimationTurnLeft <> DefaultAnimationTurnLeft;
end;

{$ifdef CASTLE_DESIGN_MODE}
type
  { Property editor to select an animation on TNyaThirdPersonVehicleNavigation }
  TNyaThirdPersonVehicleNavigationPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

function TNyaThirdPersonVehicleNavigationPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result:= [paMultiSelect, paValueList, paSortList, paRevertable];
end;

procedure TNyaThirdPersonVehicleNavigationPropertyEditor.GetValues(Proc: TGetStrProc);
var
  Nav: TNyaThirdPersonVehicleNavigation;
  S: String;
begin
  Proc('');
  Nav:= GetComponent(0) as TNyaThirdPersonVehicleNavigation;
  if (Nav.AvatarHierarchy is TNyaActor) then
    for S in (Nav.AvatarHierarchy as TNyaActor).AnimationsList do
      Proc(S);
end;
{$endif}

initialization
  RegisterSerializableComponent(TNyaThirdPersonVehicleNavigation, ['Navigation', 'Nya Third-Person Vehicle']);

  {$ifdef CASTLE_DESIGN_MODE}
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaThirdPersonVehicleNavigation, 'AnimationStand',
                         TNyaThirdPersonVehicleNavigationPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaThirdPersonVehicleNavigation, 'AnimationMoveFwd',
                         TNyaThirdPersonVehicleNavigationPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaThirdPersonVehicleNavigation, 'AnimationTurnRight',
                         TNyaThirdPersonVehicleNavigationPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaThirdPersonVehicleNavigation, 'AnimationTurnLeft',
                         TNyaThirdPersonVehicleNavigationPropertyEditor);
  {$endif}
end.

