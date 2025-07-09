unit NyaVehicleNavigation;

{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, SysUtils, CastleCameras, CastleTransform, CastleClassUtils,
  CastleInputs, CastleVectors, CastleKeysMouse, NyaBaseNavigation;

type
  TNyaVehicleNavigation = class(TNyaBaseNavigation)
  protected
    FMoveVelocity: Single;
    FAnimationStand: String;
    FAnimationMoveFwd: String;
    FAnimationTurnRight: String;
    FAnimationTurnLeft: String;
    FRollFactor: Single;
    FForceOfMove: Single;
    FMoveSpeedAnimation: Single;
    FJumpSpeed: Single;
    FJumpImpulse: Single;
    FBrakeFactor: Single;
    FInput_Brake: TInputShortcut;
  protected
    procedure RotateVehicle(const SecondsPassed: Single; RBody: TCastleRigidBody; const OnGround: Boolean; const FwdVelocityFactor, FwdShift: Single);
    procedure MoveVehicle(const SecondsPassed: Single; RBody: TCastleRigidBody; CBody: TCastleCollider; const OnGround: Boolean; const FwdVelocityFactor: Single);
    procedure Animate(const SecondsPassed: Single; const OnGround: Boolean; const FwdVelocity: Single);

    function WinFuncRotation(const value: Single): Single;
    function WinFuncRolling(const value: Single): Single;

    function AnimationStandStored: Boolean;
    function AnimationMoveFwdStored: Boolean;
    function AnimationTurnRightStored: Boolean;
    function AnimationTurnLeftStored: Boolean;
  public
    const
      DefaultRollFactor = 40.0;
      DefaultForceOfMove = 8000.0;
      DefaultMoveSpeedAnimation = 25.0;
      DefaultJumpSpeed = 1.0;
      DefaultAnimationStand = 'stand';
      DefaultAnimationMoveFwd = 'move';
      DefaultAnimationTurnRight = 'turn_right';
      DefaultAnimationTurnLeft = 'turn_left';
      DefaultBrakeFactor = 4.0;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single;
                     var HandleInput: Boolean); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    property Input_Brake: TInputShortcut read FInput_Brake;
  published
    property RollFactor: Single read FRollFactor write FRollFactor
             {$ifdef FPC}default DefaultRollFactor{$endif};
    property ForceOfMove: Single read FForceOfMove write FForceOfMove
             {$ifdef FPC}default DefaultForceOfMove{$endif};
    property SpeedOfMoveAnimation: Single read FMoveSpeedAnimation write FMoveSpeedAnimation
             {$ifdef FPC}default DefaultMoveSpeedAnimation{$endif};
    property SpeedOfJump: Single read FJumpSpeed write FJumpSpeed
             {$ifdef FPC}default DefaultJumpSpeed{$endif};
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
    property BrakeFactor: Single read FBrakeFactor write FBrakeFactor
             {$ifdef FPC}default DefaultBrakeFactor{$endif};
  end;

implementation

uses
  CastleComponentSerialize, CastleUtils,
  CastleBoxes, NyaVectorMath, NyaActor, NyaCastleUtils, Math
  {$ifdef CASTLE_DESIGN_MODE}
  , PropEdits, CastlePropEdits
  {$endif};

const
  MoveTreshold = 0.02;

constructor TNyaVehicleNavigation.Create(AOwner: TComponent);
begin
  inherited;

  FInput_Brake                 := TInputShortcut.Create(Self);
  Input_Brake                  .Assign(keySpace);
  Input_Brake                  .SetSubComponent(true);
  Input_Brake                  .Name:= 'Input_Brake';

  FMoveVelocity:= 0.0;
  FRollFactor:= DefaultRollFactor;
  FForceOfMove:= DefaultForceOfMove;
  FMoveSpeedAnimation:= DefaultMoveSpeedAnimation;
  FJumpSpeed:= DefaultJumpSpeed;
  FJumpImpulse:= DefaultJumpImpulse;
  FBrakeFactor:= DefaultBrakeFactor;

  FAnimationStand:= DefaultAnimationStand;
  FAnimationMoveFwd:= DefaultAnimationMoveFwd;
  FAnimationTurnRight:= DefaultAnimationTurnRight;
end;

destructor TNyaVehicleNavigation.Destroy;
begin
  FreeAndNil(FInput_Brake);
  inherited;
end;

procedure TNyaVehicleNavigation.Update(const SecondsPassed: Single;
                                                  var HandleInput: Boolean);
var
  RBody: TCastleRigidBody;
  CBody: TCastleCollider;
  onGround: Boolean;
  fwdVelocity, fwdVelocityFactor, fwdShift: Single;
  actor: TNyaActor;
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
  begin
    actor:= AvatarHierarchy as TNyaActor;
    fwdVelocity:= actor.ForwardVelocity;
    fwdShift:= actor.ForwardShift;

  end else
  begin
    fwdVelocity:= ProjectionVectorAtoBLength(RBody.LinearVelocity,
                                             AvatarHierarchy.Direction);
    fwdShift:= 0.0;
  end;

  { calculate real Velocity Factor }
  if (RBody.MaxLinearVelocity > 0.0) then
    fwdVelocityFactor:= fwdVelocity / RBody.MaxLinearVelocity
  else if (SpeedOfMoveAnimation > 0.0) then
    fwdVelocityFactor:= fwdVelocity / SpeedOfMoveAnimation
  else
    fwdVelocityFactor:= 0.0;

  RotateVehicle(SecondsPassed, RBody, onGround, fwdVelocityFactor, fwdShift);
  MoveVehicle(SecondsPassed, RBody, CBody, onGround, fwdVelocityFactor);
  Animate(SecondsPassed, onGround, fwdVelocity);
end;

procedure TNyaVehicleNavigation.RotateVehicle(const SecondsPassed: Single;
                                              RBody: TCastleRigidBody;
                                              const OnGround: Boolean;
                                              const FwdVelocityFactor,
                                              FwdShift: Single);
var
  gravAlign, turn, gravityUp, tangage, sideDir, gravSideDir: TVector3;
  roll: Single;
begin
  gravAlign:= TVector3.Zero;
  turn:= TVector3.Zero;
  tangage:= TVector3.Zero;

  { turn avatar up aganist gravity only around forward direction }
  gravityUp:= Camera.GravityUp;
  sideDir:= TVector3.CrossProduct(AvatarHierarchy.Up, AvatarHierarchy.Direction).Normalize;

  { add rolling factor from turns turns }
  if (OnGround AND (Abs(FwdVelocityFactor) > MoveTreshold)) then
  begin
    roll:= WinFuncRolling(FwdVelocityFactor) * RollFactor * SecondsPassed;

    if Input_Leftward.IsPressed(Container) then
      gravityUp:= gravityUp + sideDir * roll
    else if Input_Rightward.IsPressed(Container) then
      gravityUp:= gravityUp - sideDir * roll;

    { compensate retrogradation }
    if (Input_Leftward.IsPressed(Container) OR Input_Rightward.IsPressed(Container)) then
      tangage:= -sideDir * roll;
  end;

  gravSideDir:= TVector3.CrossProduct(gravityUp, AvatarHierarchy.Direction).Normalize;
  gravAlign:= TurnVectorToVector(sideDir, gravSideDir) * SpeedOfGravityAlign + tangage;

  { turn avatar }
  if OnGround then
  begin
    if Input_Leftward.IsPressed(Container) then
      turn:=  AvatarHierarchy.Up * SpeedOfTurn * WinFuncRotation(FwdVelocityFactor)
    else if Input_Rightward.IsPressed(Container) then
      turn:= -AvatarHierarchy.Up * SpeedOfTurn * WinFuncRotation(FwdVelocityFactor);
  end;

  RBody.AngularVelocity:= gravAlign + turn;
end;

procedure TNyaVehicleNavigation.MoveVehicle(const SecondsPassed: Single;
                                            RBody: TCastleRigidBody;
                                            CBody: TCastleCollider;
                                            const OnGround: Boolean;
                                            const FwdVelocityFactor: Single);
var
  avaDir, sideVelocity: TVector3;
begin
  avaDir:= AvatarHierarchy.Direction;

  { movement }
  if Input_Forward.IsPressed(Container) then
  begin
    if OnGround then
      { movement forward on ground }
      RBody.AddForce(avaDir * ForceOfMove * SecondsPassed, False)
    else
      { movement forward in air }
      RBody.AddForce(avaDir * ForceOfMoveInAir * SecondsPassed, False);
  end
  else if Input_Backward.IsPressed(Container) then
  begin
    if OnGround then
    begin
      { movement backward on ground }
      if (FwdVelocityFactor > -0.2) then
        RBody.AddForce(avaDir * (-ForceOfMove) * SecondsPassed, False);
    end else
      { movement backward in air }
      RBody.AddForce(avaDir * (-ForceOfMoveInAir) * SecondsPassed, False);
  end;


  if OnGround then
  begin
    { break }
    if Input_Brake.IsPressed(Container) then
      RBody.LinearVelocity:= RBody.LinearVelocity *
                             (1.0 - (1.0 - FwdVelocityFactor) * BrakeFactor * SecondsPassed);

    { zero side velocity }
    sideVelocity:= ProjectionVectorAtoB(RBody.LinearVelocity,
      TVector3.CrossProduct(AvatarHierarchy.Up, AvatarHierarchy.Direction).Normalize);
    RBody.LinearVelocity:= RBody.LinearVelocity - sideVelocity * 0.5;
  end;
end;

procedure TNyaVehicleNavigation.Animate(const SecondsPassed: Single;
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

function TNyaVehicleNavigation.WinFuncRotation(const value: Single): Single;
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

function TNyaVehicleNavigation.WinFuncRolling(const value: Single): Single;
var
  x, dir: Single;
begin
  x:= Abs(value);
  dir:= Sign(value);

  Result:= Power(x, 0.3) * dir;
end;

function TNyaVehicleNavigation.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'ForceOfMove', 'SpeedOfMoveAnimation',
       'SpeedOfRun', 'SpeedOfRunAnimation', 'SpeedOfJump',
       'RollFactor', 'BrakeFactor',
       'ImpulseOfJump', 'AnimationStand', 'AnimationMoveFwd',
       'AnimationTurnRight', 'AnimationTurnLeft'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

function TNyaVehicleNavigation.AnimationStandStored: Boolean;
begin
  Result:= FAnimationStand <> DefaultAnimationStand;
end;

function TNyaVehicleNavigation.AnimationMoveFwdStored: Boolean;
begin
  Result:= FAnimationMoveFwd <> DefaultAnimationMoveFwd;
end;

function TNyaVehicleNavigation.AnimationTurnRightStored: Boolean;
begin
  Result:= FAnimationTurnRight <> DefaultAnimationTurnRight;
end;

function TNyaVehicleNavigation.AnimationTurnLeftStored: Boolean;
begin
  Result:= FAnimationTurnLeft <> DefaultAnimationTurnLeft;
end;

{$ifdef CASTLE_DESIGN_MODE}
type
  { Property editor to select an animation on TNyaVehicleNavigation }
  TNyaVehicleNavigationPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

function TNyaVehicleNavigationPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result:= [paMultiSelect, paValueList, paSortList, paRevertable];
end;

procedure TNyaVehicleNavigationPropertyEditor.GetValues(Proc: TGetStrProc);
var
  Nav: TNyaVehicleNavigation;
  S: String;
begin
  Proc('');
  Nav:= GetComponent(0) as TNyaVehicleNavigation;
  if (Nav.AvatarHierarchy is TNyaActor) then
    for S in (Nav.AvatarHierarchy as TNyaActor).AnimationsList do
      Proc(S);
end;
{$endif}

initialization
  RegisterSerializableComponent(TNyaVehicleNavigation, ['Navigation', 'Nya Vehicle']);

  {$ifdef CASTLE_DESIGN_MODE}
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaVehicleNavigation, 'AnimationStand',
                         TNyaVehicleNavigationPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaVehicleNavigation, 'AnimationMoveFwd',
                         TNyaVehicleNavigationPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaVehicleNavigation, 'AnimationTurnRight',
                         TNyaVehicleNavigationPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaVehicleNavigation, 'AnimationTurnLeft',
                         TNyaVehicleNavigationPropertyEditor);
  {$endif}
end.

