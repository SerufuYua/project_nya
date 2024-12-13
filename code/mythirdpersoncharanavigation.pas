unit MyThirdPersonCharaNavigation;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleCameras, CastleTransform, CastleClassUtils,
  CastleInputs, CastleVectors;

type
  TMyThirdPersonCharaNavigation = class(TCastleNavigation)
  protected
    FTurnSpeed: Single;
    FWalkSpeed: Single;
    FInput_Forward: TInputShortcut;
    FInput_Backward: TInputShortcut;
    FInput_Leftward: TInputShortcut;
    FInput_Rightward: TInputShortcut;
    FInput_Jump: TInputShortcut;
    FWasMoveInput: Boolean;
    FOldMoveVelocity: TVector3;
    FAvatarHierarchy: TCastleTransform;
    FAvatarHierarchyFreeObserver: TFreeNotificationObserver;
    procedure AvatarHierarchyFreeNotification(const Sender: TFreeNotificationObserver);
    procedure SetAvatarHierarchy(const Value: TCastleTransform);
  protected
    procedure RotateChara(const SecondsPassed: Single);
    procedure MoveChara(const SecondsPassed: Single);
  public
  const
    DefaultTurnSpeed = 20;
    DefaultWalkSpeed = 40;

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
    property SpeedOfTurn: Single read FTurnSpeed write FTurnSpeed
      {$ifdef FPC}default DefaultTurnSpeed{$endif};
    property SpeedOfWalk: Single read FWalkSpeed write FWalkSpeed
      {$ifdef FPC}default DefaultWalkSpeed{$endif};
  end;

implementation

uses
  CastleComponentSerialize, CastleUtils, CastleKeysMouse;

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
  Input_Jump                   .Name := 'Input_Jump';

  FWasMoveInput:= False;
  FOldMoveVelocity:= TVector3.Zero;
  FTurnSpeed:= DefaultTurnSpeed;
  FWalkSpeed:= DefaultWalkSpeed;

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

  RotateChara(SecondsPassed);
  MoveChara(SecondsPassed);
end;

procedure TMyThirdPersonCharaNavigation.RotateChara(const SecondsPassed: Single);
var
  AvaVerticalDir, ForwardDir, BackwardDir, RightwardDir, LeftwardDir: TVector3;
  LookToDir: TVector3;
begin
  AvaVerticalDir:= AvatarHierarchy.Up;
  LeftwardDir:= TVector3.CrossProduct(AvaVerticalDir, Camera.Direction);
  RightwardDir:= -LeftwardDir;
  BackwardDir:= TVector3.CrossProduct(AvaVerticalDir, LeftwardDir);
  ForwardDir:= -BackwardDir;

  LookToDir:= TVector3.Zero;

  if Input_Forward.IsPressed(Container) then
    LookToDir:= LookToDir + ForwardDir;

  if Input_Backward.IsPressed(Container) then
    LookToDir:= LookToDir + BackwardDir;

  if Input_Leftward.IsPressed(Container) then
    LookToDir:= LookToDir + LeftwardDir;

  if Input_Rightward.IsPressed(Container) then
    LookToDir:= LookToDir + RightwardDir;


  AvatarHierarchy.Up:= SmoothTowards(AvatarHierarchy.Up.Normalize,
                                     Camera.GravityUp,
                                     SecondsPassed, SpeedOfTurn);

  AvatarHierarchy.Direction:= SmoothTowards(AvatarHierarchy.Direction.Normalize,
                                            LookToDir.Normalize,
                                            SecondsPassed, SpeedOfTurn);
end;

procedure TMyThirdPersonCharaNavigation.MoveChara(const SecondsPassed: Single);
var
  RBody: TCastleRigidBody;
  CBody: TCastleCollider;
  MoveVelocity, GravityVelocity: TVector3;
begin
  RBody:= AvatarHierarchy.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
  if NOT Assigned(RBody) then Exit;


  MoveVelocity:= Tvector3.Zero;

  CBody:= AvatarHierarchy.FindBehavior(TCastleCollider) as TCastleCollider;
  if Assigned(CBody) then
    GravityVelocity:= -Camera.GravityUp * CBody.Mass
  else
    GravityVelocity:= -Camera.GravityUp;

  if (Input_Forward.IsPressed(Container) OR
      Input_Backward.IsPressed(Container) OR
      Input_Leftward.IsPressed(Container) OR
      Input_Rightward.IsPressed(Container)) then
  begin
    FWasMoveInput:= True;
    MoveVelocity:= AvatarHierarchy.Direction.Normalize;
    MoveVelocity:= MoveVelocity * SpeedOfWalk;
  end else if FWasMoveInput then
  begin
    FWasMoveInput:= False;
    RBody.LinearVelocity:= Tvector3.Zero + GravityVelocity;
  end;

  if NOT MoveVelocity.IsZero then
    RBody.LinearVelocity:= MoveVelocity + GravityVelocity;

end;

function TMyThirdPersonCharaNavigation.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'AvatarHierarchy', 'SpeedOfWalk', 'SpeedOfTurn'
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

