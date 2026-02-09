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
    FVelocityWalk, FVelocityRotate, FTimeoutWalk, FTimeoutRotate, FTimeoutIdle,
      FTimeoutConfuse, FTimer: Single;
    FState: TState;
    FAnimationIdle, FAnimationMove, FAnimationConfuse: String;
    function AnimationIdleStored: Boolean;
    function AnimationMoveStored: Boolean;
    function AnimationConfuseStored: Boolean;
  public
    const
      DefaultVelocityWalk = 1.0;
      DefaultVelocityRotate = 1.0;
      DefaultTimeoutWalk = 3.0;
      DefaultTimeoutRotate = 1.0;
      DefaultTimeoutIdle = 8.0;
      DefaultTimeoutConfuse = 15.0;
      DefaultAnimationIdle = 'idle';
      DefaultAnimationMove = 'move';
      DefaultAnimationConfuse = 'confuse';

    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    property VelocityWalk: Single read FVelocityWalk write FVelocityWalk
             {$ifdef FPC}default DefaultVelocityWalk{$endif};
    property VelocityRotate: Single read FVelocityRotate write FVelocityRotate
             {$ifdef FPC}default DefaultVelocityRotate{$endif};
    property TimeoutWalk: Single read FTimeoutWalk write FTimeoutWalk
             {$ifdef FPC}default DefaultTimeoutWalk{$endif};
    property TimeoutRotate: Single read FTimeoutRotate write FTimeoutRotate
             {$ifdef FPC}default DefaultTimeoutRotate{$endif};
    property TimeoutIdle: Single read FTimeoutIdle write FTimeoutIdle
             {$ifdef FPC}default DefaultTimeoutIdle{$endif};
    property TimeoutConfuse: Single read FTimeoutConfuse write FTimeoutConfuse
             {$ifdef FPC}default DefaultTimeoutConfuse{$endif};
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

  FVelocityWalk:= DefaultVelocityWalk;
  FVelocityRotate:= DefaultVelocityRotate;
  FTimeoutWalk:= DefaultTimeoutWalk;
  FTimeoutRotate:= DefaultTimeoutRotate;
  FTimeoutIdle:= DefaultTimeoutIdle;
  FTimeoutConfuse:= DefaultTimeoutConfuse;
  FAnimationIdle:= DefaultAnimationIdle;
  FAnimationMove:= DefaultAnimationMove;
  FAnimationConfuse:= DefaultAnimationConfuse;
  FTimer:= DefaultTimeoutIdle;
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

  RBody:= Parent.RigidBody;
  if (NOT (Assigned(RBody) and RBody.ExistsInRoot)) then Exit;

  currentAnimation:= FAnimationIdle;
  FTimer:= FTimer - SecondsPassed;

  { detect Confuse action }
  if (FState <> TState.Confuse) then
  begin
    for colliding in RBody.GetCollidingTransforms do
    begin
      if (colliding is TNyaActor) then
      begin
        FTimer:= FTimeoutConfuse;
        FState:= TState.Confuse;
        RBody.LinearVelocity:= TVector3.Zero;
        RBody.AngularVelocity:= TVector3.Zero;
        RBody.Dynamic:= False;
        if (Parent is TCastleScene) then
          (Parent as TCastleScene).PlayAnimation(FAnimationConfuse, False);
        Exit;
      end;
    end;
  end;

  { processing timer elapse }
  if (FTimer <= 0.0) then
  begin
    Case FState of
      { end Confuse action }
      TState.Confuse:
        begin
          FTimer:= FTimeoutIdle;
          RBody.Dynamic:= True;
          FState:= TState.Idle;
        end;
      { end Normal action }
      TState.Idle, TState.WalkStright, TState.WalkLeft, TState.WalkRight:
        begin
          nLow:= Ord(Low(TState));
          nHigh:= Ord(High(TState)); { without Confuse state }
          FState:= TState(RandomRange(nLow, nHigh));
          Case FState of
            TState.Idle: FTimer:= FTimeoutIdle;
            TState.WalkStright: FTimer:= FTimeoutWalk;
            TState.WalkLeft, TState.WalkRight: FTimer:= FTimeoutRotate;
          end;
        end;
    end;
  end;

  { normal action }
  Case FState of
    TState.Idle:
      begin
        RBody.LinearVelocity:= TVector3.Zero;
        RBody.AngularVelocity:= TVector3.Zero;
        currentAnimation:= FAnimationIdle;
      end;
    TState.WalkStright:
      begin
        currentAnimation:= FAnimationMove;
        RBody.LinearVelocity:= Parent.Direction * FVelocityWalk;
      end;
    TState.WalkLeft:
      begin
        currentAnimation:= FAnimationMove;
        RBody.LinearVelocity:= Parent.Direction * FVelocityWalk;
        RBody.AngularVelocity:= Parent.Up * FVelocityRotate;
      end;
    TState.WalkRight:
      begin
        currentAnimation:= FAnimationMove;
        RBody.LinearVelocity:= Parent.Direction * FVelocityWalk;
        RBody.AngularVelocity:= Parent.Up * (-FVelocityRotate);
      end;
  end;

  if (Parent is TCastleScene) then
    (Parent as TCastleScene).AutoAnimation:= currentAnimation;
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
       'VelocityWalk', 'TimeoutRotate', 'VelocityRotate', 'TimeoutWalk',
       'TimeoutIdle', 'TimeoutConfuse', 'Beater', 'AnimationIdle',
       'AnimationMove', 'AnimationConfuse'
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

