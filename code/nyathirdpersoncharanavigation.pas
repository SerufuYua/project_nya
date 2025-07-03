unit NyaThirdPersonCharaNavigation;

{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, SysUtils, CastleCameras, CastleTransform, CastleClassUtils,
  CastleInputs, CastleVectors, NyaMath, NyaBaseNavigation;

type
  TNyaThirdPersonCharaNavigation = class(TNyaBaseNavigation)
  protected
    FRunFlag: Boolean; { True - Run; False - Walk }
    FAnimationStand: String;
    FAnimationWalk: String;
    FAnimationRun: String;
    FLookTargetDir: TVector3;
    FWalkSpeed: Single;
    FWalkSpeedAnimation: Single;
    FRunSpeed: Single;
    FRunSpeedAnimation: Single;
    FJumpSpeed: Single;
    FJumpImpulse: Single;
    FInput_FastMove: TInputShortcut;
    FInput_Jump: TInputShortcut;
  protected
    procedure CalcLookTargetDir;
    procedure RotateChara(const SecondsPassed: Single; RBody: TCastleRigidBody; const OnGround: Boolean);
    procedure MoveChara(const SecondsPassed: Single; RBody: TCastleRigidBody; CBody: TCastleCollider; const OnGround: Boolean; const FwdVelocity: Single);
    procedure Animate(const SecondsPassed: Single; const OnGround: Boolean; const FwdVelocity: Single);

    function AnimationStandStored: Boolean;
    function AnimationWalkStored: Boolean;
    function AnimationRunStored: Boolean;
  public
    const
      DefaultWalkSpeed = 1.0;
      DefaultWalkSpeedAnimation = 1.0;
      DefaultRunSpeed = 3.0;
      DefaultRunSpeedAnimation = 3.0;
      DefaultJumpSpeed = 1.0;
      DefaultAnimationStand = 'stand';
      DefaultAnimationWalk = 'walk';
      DefaultAnimationRun = 'run';

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single;
                     var HandleInput: Boolean); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    property Input_FastMove: TInputShortcut read FInput_FastMove;
    property Input_Jump: TInputShortcut read FInput_Jump;
  published
    property SpeedOfWalk: Single read FWalkSpeed write FWalkSpeed
             {$ifdef FPC}default DefaultWalkSpeed{$endif};
    property SpeedOfWalkAnimation: Single read FWalkSpeedAnimation write FWalkSpeedAnimation
             {$ifdef FPC}default DefaultWalkSpeedAnimation{$endif};
    property SpeedOfRun: Single read FRunSpeed write FRunSpeed
             {$ifdef FPC}default DefaultRunSpeed{$endif};
    property SpeedOfRunAnimation: Single read FRunSpeedAnimation write FRunSpeedAnimation
             {$ifdef FPC}default DefaultRunSpeedAnimation{$endif};
    property SpeedOfJump: Single read FJumpSpeed write FJumpSpeed
             {$ifdef FPC}default DefaultJumpSpeed{$endif};
    property ImpulseOfJump: Single read FJumpImpulse write FJumpImpulse
             {$ifdef FPC}default DefaultJumpImpulse{$endif};

    property AnimationStand: String read FAnimationStand write FAnimationStand
             stored AnimationStandStored nodefault;
    property AnimationWalk: String read FAnimationWalk write FAnimationWalk
             stored AnimationWalkStored nodefault;
    property AnimationRun: String read FAnimationRun write FAnimationRun
             stored AnimationRunStored nodefault;
  end;

implementation

uses
  CastleComponentSerialize, CastleUtils, CastleKeysMouse,
  CastleBoxes, NyaVectorMath, NyaActor
  {$ifdef CASTLE_DESIGN_MODE}
  , PropEdits, CastlePropEdits
  {$endif};

constructor TNyaThirdPersonCharaNavigation.Create(AOwner: TComponent);
begin
  inherited;

  FInput_FastMove              := TInputShortcut.Create(Self);
  FInput_Jump                  := TInputShortcut.Create(Self);

  Input_FastMove               .Assign(keyShift);
  Input_Jump                   .Assign(keySpace);

  Input_FastMove               .SetSubComponent(true);
  Input_Jump                   .SetSubComponent(true);

  Input_FastMove               .Name:= 'Input_FastMove';
  Input_Jump                   .Name:= 'Input_Jump';

  FLookTargetDir:= TVector3.Zero;
  FWalkSpeed:= DefaultWalkSpeed;
  FWalkSpeedAnimation:= DefaultWalkSpeedAnimation;
  FRunSpeed:= DefaultRunSpeed;
  FRunSpeedAnimation:= DefaultRunSpeedAnimation;
  FJumpSpeed:= DefaultJumpSpeed;
  FJumpImpulse:= DefaultJumpImpulse;

  FRunFlag:= False;
  FAnimationStand:= DefaultAnimationStand;
  FAnimationWalk:= DefaultAnimationWalk;
  FAnimationRun:= DefaultAnimationRun;
end;

destructor TNyaThirdPersonCharaNavigation.Destroy;
begin
  FreeAndNil(FInput_FastMove);
  FreeAndNil(FInput_Jump);
  inherited;
end;

procedure TNyaThirdPersonCharaNavigation.Update(const SecondsPassed: Single;
                                               var HandleInput: Boolean);
var
  RBody: TCastleRigidBody;
  CBody: TCastleCollider;
  onGround: Boolean;
  fwdVelocity: Single;
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
    fwdVelocity:= (AvatarHierarchy as TNyaActor).ForwardVelocity
  else
    fwdVelocity:= ProjectionVectorAtoBLength(RBody.LinearVelocity,
                                              AvatarHierarchy.Direction);

  CalcLookTargetDir;
  RotateChara(SecondsPassed, RBody, onGround);
  MoveChara(SecondsPassed, RBody, CBody, onGround, fwdVelocity);
  Animate(SecondsPassed, OnGround, fwdVelocity);
end;

procedure TNyaThirdPersonCharaNavigation.CalcLookTargetDir;
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

procedure TNyaThirdPersonCharaNavigation.RotateChara(const SecondsPassed: Single;
                                                     RBody: TCastleRigidBody;
                                                     const OnGround: Boolean);
var
  gravAlign, turn: TVector3;
begin
  gravAlign:= TVector3.Zero;
  turn:= TVector3.Zero;

  { turn avatar up aganist gravity }
  gravAlign:= TurnVectorToVector(AvatarHierarchy.Up, Camera.GravityUp) * SpeedOfGravityAlign;


  { turn avatar to target }
  if NOT FLookTargetDir.IsZero then
    turn:= TurnVectorToVector(AvatarHierarchy.Direction, FLookTargetDir) * SpeedOfTurn;

  RBody.AngularVelocity:= gravAlign + turn;
end;

procedure TNyaThirdPersonCharaNavigation.MoveChara(const SecondsPassed: Single;
                                                   RBody: TCastleRigidBody;
                                                   CBody: TCastleCollider;
                                                   const OnGround: Boolean;
                                                   const FwdVelocity: Single);
var
  AvaDir, GravityVelocity: TVector3;
begin
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
end;

procedure TNyaThirdPersonCharaNavigation.Animate(const SecondsPassed: Single;
                                                 const OnGround: Boolean;
                                                 const FwdVelocity: Single);
begin
  if NOT Assigned(OnAnimation) then Exit;

  { processing animations }
  if OnGround then
  begin
    { switch walk/run state }
    { W + (R - W) * 0.6 }
    { W * 0.4 + R * 0.6 }
    if (NOT FRunFlag) AND (FwdVelocity > (SpeedOfWalkAnimation* 0.4 + SpeedOfRunAnimation * 0.6)) then
      FRunFlag:= True;
    if FRunFlag AND (FwdVelocity < (SpeedOfWalkAnimation* 0.6 + SpeedOfRunAnimation * 0.4)) then
      FRunFlag:= False;

    if (FwdVelocity < 0.2 * SpeedOfWalkAnimation) then
      { stand }
      OnAnimation(self, AnimationStand, 1.0)
    else begin
      { enable move animation }
      if FRunFlag then
        { run }
        OnAnimation(self, AnimationRun, FwdVelocity / SpeedOfRunAnimation)
      else
        { walk }
        OnAnimation(self, AnimationWalk, FwdVelocity / SpeedOfWalkAnimation);
    end
  end else
    OnAnimation(self, AnimationStand, 1.0);

end;

function TNyaThirdPersonCharaNavigation.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'SpeedOfWalk', 'SpeedOfWalkAnimation', 'SpeedOfRun',
       'SpeedOfRunAnimation', 'SpeedOfJump',
       'ImpulseOfJump', 'AnimationStand', 'AnimationWalk',
       'AnimationRun'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

function TNyaThirdPersonCharaNavigation.AnimationStandStored: Boolean;
begin
  Result:= FAnimationStand <> DefaultAnimationStand;
end;

function TNyaThirdPersonCharaNavigation.AnimationWalkStored: Boolean;
begin
  Result:= FAnimationWalk <> DefaultAnimationWalk;
end;

function TNyaThirdPersonCharaNavigation.AnimationRunStored: Boolean;
begin
  Result:= FAnimationRun <> DefaultAnimationRun;
end;

{$ifdef CASTLE_DESIGN_MODE}
type
  { Property editor to select an animation on TNyaThirdPersonCharaNavigation }
  TNyaThirdPersonCharaNavigationPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

function TNyaThirdPersonCharaNavigationPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result:= [paMultiSelect, paValueList, paSortList, paRevertable];
end;

procedure TNyaThirdPersonCharaNavigationPropertyEditor.GetValues(Proc: TGetStrProc);
var
  Nav: TNyaThirdPersonCharaNavigation;
  S: String;
begin
  Proc('');
  Nav:= GetComponent(0) as TNyaThirdPersonCharaNavigation;
  if (Nav.AvatarHierarchy is TNyaActor) then
    for S in (Nav.AvatarHierarchy as TNyaActor).AnimationsList do
      Proc(S);
end;
{$endif}

initialization
  RegisterSerializableComponent(TNyaThirdPersonCharaNavigation, ['Navigation', 'Nya Third-Person Chara']);

  {$ifdef CASTLE_DESIGN_MODE}
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaThirdPersonCharaNavigation, 'AnimationStand',
                         TNyaThirdPersonCharaNavigationPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaThirdPersonCharaNavigation, 'AnimationWalk',
                         TNyaThirdPersonCharaNavigationPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaThirdPersonCharaNavigation, 'AnimationRun',
                         TNyaThirdPersonCharaNavigationPropertyEditor);
  {$endif}
end.

