unit NyaRandomWalk;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleBehaviors, CastleTransform, CastleClassUtils;

type
  TNyaRandomWalk = class(TCastleBehavior)
  protected
    type
      TState = (Idle, WalkStright, WalkLeft, WalkRight, Confuse);
  protected
    FVelocity, FActionTimeout, FConfuseTimeout, FTimer: Single;
    FState: TState;
    FAnimationIdle, FAnimationMove, FAnimationConfuse: String;
    function AnimationIdleStored: Boolean;
    function AnimationMoveStored: Boolean;
    function AnimationConfuseStored: Boolean;
  public
    const
      DefaultVelocity = 1.0;
      DefaultActionTimeout = 3.0;
      DefaultConfuseTimeout = 5.0;
      DefaultAnimationIdle = 'idle';
      DefaultAnimationMove = 'move';
      DefaultAnimationConfuse = 'confuse';

    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    property Velocity: Single read FVelocity write FVelocity
             {$ifdef FPC}default DefaultVelocity{$endif};
    property ActionTimeout: Single read FActionTimeout write FActionTimeout
             {$ifdef FPC}default DefaultActionTimeout{$endif};
    property ConfuseTimeout: Single read FConfuseTimeout write FConfuseTimeout
             {$ifdef FPC}default DefaultConfuseTimeout{$endif};
    property AnimationIdle: String read FAnimationIdle write FAnimationIdle
             stored AnimationIdleStored nodefault;
    property AnimationMove: String read FAnimationMove write FAnimationMove
             stored AnimationMoveStored nodefault;
    property AnimationConfuse: String read FAnimationConfuse write FAnimationConfuse
             stored AnimationConfuseStored nodefault;
  end;

implementation

uses
  CastleComponentSerialize, CastleUtils, CastleScene, CastleVectors, Math,
  NyaActor
  {$ifdef CASTLE_DESIGN_MODE}
  , PropEdits, CastlePropEdits
  {$endif};

constructor TNyaRandomWalk.Create(AOwner: TComponent);
begin
  inherited;

  FVelocity:= DefaultVelocity;
  FActionTimeout:= DefaultActionTimeout;
  FConfuseTimeout:= DefaultConfuseTimeout;
  FAnimationIdle:= DefaultAnimationIdle;
  FAnimationMove:= DefaultAnimationMove;
  FAnimationConfuse:= DefaultAnimationConfuse;
  FTimer:= 0.0;
  FState:= TState.Idle;
end;

procedure TNyaRandomWalk.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
var
  nHigh, nLow: Integer;
  currentAnimation: String;
  RBody: TCastleRigidBody;
  colliding: TCastleTransform;
begin
  inherited;

  currentAnimation:= FAnimationIdle;

  RBody:= Parent.RigidBody;
  if (Assigned(RBody) and RBody.ExistsInRoot) then
  begin
    FTimer:= FTimer + SecondsPassed;

    { count Confuse time }
    if (FState = TState.Confuse) then
    begin
      if (FTimer > FConfuseTimeout) then
      begin
        FTimer:= 0.0;
        RBody.Dynamic:= True;
        FState:= TState.Idle;
      end;
      Exit;
    end;

    { detect Confuse action }
    for colliding in RBody.GetCollidingTransforms do
      if (colliding is TNyaActor) then
      begin
        FState:= TState.Confuse;
        FTimer:= 0.0;
        RBody.LinearVelocity:= TVector3.Zero;
        RBody.AngularVelocity:= TVector3.Zero;
        RBody.Dynamic:= False;
        if (Parent is TCastleScene) then
          (Parent as TCastleScene).PlayAnimation(FAnimationConfuse, False);
        Exit;
      end;

    { count normal time }
    if (FTimer > FActionTimeout) then
    begin
      FTimer:= 0.0;
      nLow:= Ord(Low(TState));
      nHigh:= Ord(High(TState)); { without Confuse state }
      FState:= TState(RandomRange(nLow, nHigh));
    end;

    { normal action }
    Case FState of
      TState.Idle:
        begin
          currentAnimation:= FAnimationIdle;
        end;
      TState.WalkStright:
        begin
          currentAnimation:= FAnimationMove;
          RBody.LinearVelocity:= Parent.Direction * FVelocity;
        end;
      TState.WalkLeft:
        begin
          currentAnimation:= FAnimationMove;
          RBody.LinearVelocity:= Parent.Direction * FVelocity;
          RBody.AngularVelocity:= Parent.Up * FVelocity;
        end;
      TState.WalkRight:
        begin
          currentAnimation:= FAnimationMove;
          RBody.LinearVelocity:= Parent.Direction * FVelocity;
          RBody.AngularVelocity:= Parent.Up * (-FVelocity);
        end;
    end;

    if (Parent is TCastleScene) then
      (Parent as TCastleScene).AutoAnimation:= currentAnimation;
  end;

end;

function TNyaRandomWalk.AnimationIdleStored: Boolean;
begin
  Result:= FAnimationIdle <> DefaultAnimationIdle;
end;

function TNyaRandomWalk.AnimationMoveStored: Boolean;
begin
  Result:= FAnimationMove <> DefaultAnimationMove;
end;

function TNyaRandomWalk.AnimationConfuseStored: Boolean;
begin
  Result:= FAnimationConfuse <> DefaultAnimationConfuse;
end;

function TNyaRandomWalk.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'Velocity', 'ActionTimeout', 'ConfuseTimeout', 'Beater',
       'AnimationIdle', 'AnimationMove', 'AnimationConfuse'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

{$ifdef CASTLE_DESIGN_MODE}
type
  { Property editor to select an animation }
  TNyaRandomWalkPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

function TNyaRandomWalkPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result:= [paMultiSelect, paValueList, paSortList, paRevertable];
end;

procedure TNyaRandomWalkPropertyEditor.GetValues(Proc: TGetStrProc);
var
  Nav: TNyaRandomWalk;
  S: String;
begin
  Proc('');
  Nav:= GetComponent(0) as TNyaRandomWalk;
  if (Nav.Parent is TCastleScene) then
    for S in (Nav.Parent as TCastleScene).AnimationsList do
      Proc(S);
end;
{$endif}

initialization
  RegisterSerializableComponent(TNyaRandomWalk, 'Nya Random Walk');

  {$ifdef CASTLE_DESIGN_MODE}
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaRandomWalk, 'AnimationIdle',
                         TNyaRandomWalkPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaRandomWalk, 'AnimationMove',
                         TNyaRandomWalkPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TNyaRandomWalk, 'AnimationConfuse',
                         TNyaRandomWalkPropertyEditor);
  {$endif}
end.

