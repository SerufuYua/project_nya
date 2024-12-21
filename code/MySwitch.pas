unit MySwitch;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleTransform, CastleClassUtils, CastleScene,
  CastleSceneCore, X3DNodes;

type
  TTouchEvent = procedure (const Sender: TObject; Touch: Boolean) of object;

  TSwitchStatus = (inactive, touched, activated, unknown);

  TMySwitch = class(TCastleBehavior)
  protected
    FDistance: Single;
    FOnTouch: TTouchEvent;
    FOnActivate: TNotifyEvent;
    FStatus: TSwitchStatus;
    FActionString: String;
    FAnimationInactive: String;
    FAnimationTouched: String;
    FAnimationActivated: String;
    FIndicator: TCastleScene;
    FIndicatorObserver: TFreeNotificationObserver;
    FActivator: TCastleTransform;
    FActivatorObserver: TFreeNotificationObserver;
    procedure SetIndicator(const Value: TCastleScene);
    procedure IndicatorFreeNotification(const Sender: TFreeNotificationObserver);
    procedure SetActivator(const Value: TCastleTransform);
    procedure ActivatorFreeNotification(const Sender: TFreeNotificationObserver);
    procedure SetStatus(const Value: TSwitchStatus);
    function AnimationInactiveStored: Boolean;
    function AnimationTouchedStored: Boolean;
    function AnimationActivatedStored: Boolean;
  protected
    function CanAttachToParent(const NewParent: TCastleTransform;
      out ReasonWhyCannot: String): Boolean; override;
    function LookForActivator: TSwitchStatus; virtual;
    procedure ActavateAfterAnimation(const Scene: TCastleSceneCore;
                                     const Animation: TTimeSensorNode);
  public
    const
      DefaultActionString = 'use';
      DefaultAnimationInactive  = 'inactive';
      DefaultAnimationTouched   = 'touched';
      DefaultAnimationActivated = 'activated';
      DefaultDistance = 20;

    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    procedure Activate;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    property Status: TSwitchStatus read FStatus write SetStatus;
  published
    property Indicator: TCastleScene read FIndicator write SetIndicator;
    property Activator: TCastleTransform read FActivator write SetActivator;
    property ActionString: String read FActionString write FActionString;
    property Distance: Single read FDistance write FDistance
      {$ifdef FPC}default DefaultDistance{$endif};

    property OnTouch: TTouchEvent read FOnTouch write FOnTouch;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;

    { Indicator Animations }
    property IndicatorAnimationInactive: String read FAnimationInactive write FAnimationInactive stored AnimationInactiveStored nodefault;
    property IndicatorAnimationTouched: String read FAnimationTouched write FAnimationTouched stored AnimationTouchedStored nodefault;
    property IndicatorAnimationActivated: String read FAnimationActivated write FAnimationActivated stored AnimationActivatedStored nodefault;
  end;

  TMyFrontSwitch = class(TMySwitch)
  protected
    FAngleCOS: Single;
    function GetAngle: Single;
    procedure SetAngle(value: Single);
  protected
    function LookForActivator: TSwitchStatus; override;
  public
    const
      DefaultAngleCOS = 0.707106769; { 45 grad }

    constructor Create(AOwner: TComponent); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    property VisibleAngle: Single read GetAngle write SetAngle
      {$ifdef FPC}default DefaultAngleCOS{$endif};
  end;

implementation

uses
  CastleComponentSerialize, CastleUtils, CastleBoxes, CastleVectors, Math,
  MyVectorMath;

{ ========= ------------------------------------------------------------------ }
{ TMySwitch ------------------------------------------------------------------ }
{ ========= ------------------------------------------------------------------ }

constructor TMySwitch.Create(AOwner: TComponent);
begin
  inherited;

  FStatus:= TSwitchStatus.unknown;
  FDistance:= DefaultDistance;

  FActionString:= DefaultActionString;
  FAnimationInactive:= DefaultAnimationInactive;
  FAnimationTouched:= DefaultAnimationTouched;
  FAnimationActivated:= DefaultAnimationActivated;

  FIndicatorObserver:= TFreeNotificationObserver.Create(Self);
  FIndicatorObserver.OnFreeNotification:= {$ifdef FPC}@{$endif}IndicatorFreeNotification;
  FActivatorObserver:= TFreeNotificationObserver.Create(Self);
  FActivatorObserver.OnFreeNotification:= {$ifdef FPC}@{$endif}ActivatorFreeNotification;
end;

procedure TMySwitch.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  inherited;

  if Status = TSwitchStatus.activated then Exit;

  Status:= LookForActivator;
end;

function TMySwitch.LookForActivator: TSwitchStatus;
var
  SwitchBox: TBox3D;
begin
  if NOT (Assigned(Parent) AND Assigned(Activator)) then
    Exit(TSwitchStatus.inactive);

  SwitchBox:= Parent.BoundingBox;

  if SwitchBox.Grow(20.0).Contains(Activator.Translation) then
    Result:= TSwitchStatus.touched
  else
    Result:= TSwitchStatus.inactive;
end;

function TMySwitch.CanAttachToParent(const NewParent: TCastleTransform;
  out ReasonWhyCannot: String): Boolean;
begin
  Result:= inherited;
  if NOT Result then Exit;

  if NewParent.FindBehavior(TMySwitch) <> nil then
  begin
    ReasonWhyCannot:= 'Only one TMySwitch can be added to the "' +
                      NewParent.Name + '"';
    Result:= false;
  end;
end;

procedure TMySwitch.Activate;
begin
  Status:= TSwitchStatus.activated;
end;

procedure TMySwitch.ActavateAfterAnimation(const Scene: TCastleSceneCore;
                                           const Animation: TTimeSensorNode);
begin
  if Assigned(OnActivate) then
    OnActivate(Self);
  Status:= TSwitchStatus.unknown;
end;

function TMySwitch.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'Activator', 'ActionString', 'Indicator',
       'IndicatorAnimationInactive', 'IndicatorAnimationTouched',
       'IndicatorAnimationActivated', 'Distance'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

procedure TMySwitch.SetStatus(const Value: TSwitchStatus);
var
  animation: TPlayAnimationParameters;
begin
  if FStatus = Value then Exit;
  FStatus:= Value;
  animation:= TPlayAnimationParameters.Create;

  Case FStatus of
  TSwitchStatus.inactive:
    begin
      if Assigned(Indicator) then
      begin
        Indicator.StopAnimation;
        animation.Name:= IndicatorAnimationInactive;
        animation.Loop:= True;
        Indicator.PlayAnimation(animation);
      end;

      if Assigned(OnTouch) then
        OnTouch(Self, False);
    end;
  TSwitchStatus.touched:
    begin
      if Assigned(Indicator) then
      begin
        animation.Name:= IndicatorAnimationTouched;
        animation.Loop:= True;
        Indicator.PlayAnimation(animation);
      end;

      if Assigned(OnTouch) then
        OnTouch(Self, True);
    end;
  TSwitchStatus.activated:
    begin
      if Assigned(Indicator) then
      begin
        animation.Name:= IndicatorAnimationActivated;
        animation.Loop:= False;
        animation.StopNotification:= {$ifdef FPC}@{$endif}ActavateAfterAnimation;
        Indicator.PlayAnimation(animation);
      end else
      begin
        if Assigned(OnActivate) then
          OnActivate(Self);
        Status:= TSwitchStatus.unknown;
      end;
    end;
  end;

  FreeAndNil(animation);
end;

procedure TMySwitch.SetIndicator(const Value: TCastleScene);
begin
  if (FIndicator <> Value) then
  begin
    FIndicatorObserver.Observed:= Value;
    FIndicator:= Value;
  end;
end;

procedure TMySwitch.IndicatorFreeNotification(
  const Sender: TFreeNotificationObserver);
begin
  Indicator:= nil;
end;

procedure TMySwitch.SetActivator(const Value: TCastleTransform);
begin
  if (FActivator <> Value) then
  begin
    FActivatorObserver.Observed:= Value;
    FActivator:= Value;
  end;
end;

procedure TMySwitch.ActivatorFreeNotification(
  const Sender: TFreeNotificationObserver);
begin
  Activator:= nil;
end;

function TMySwitch.AnimationInactiveStored: Boolean;
begin
  Result:= FAnimationInactive <> DefaultAnimationInactive;
end;

function TMySwitch.AnimationTouchedStored: Boolean;
begin
  Result:= FAnimationTouched <> DefaultAnimationTouched;
end;

function TMySwitch.AnimationActivatedStored: Boolean;
begin
  Result:= FAnimationActivated <> DefaultAnimationActivated;
end;

{ ========= ------------------------------------------------------------------ }
{ TMyFrontSwitch ------------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

constructor TMyFrontSwitch.Create(AOwner: TComponent);
begin
  inherited;

  FAngleCOS:= DefaultAngleCOS;
end;

function TMyFrontSwitch.LookForActivator: TSwitchStatus;
var
  ActivatorBox, SwitchBox: TBox3D;
  FromRootActivator, FromActivatorDir, ProjPoint, SwitchCenter: TVector3;
begin
  Result:= inherited;
  if (Result = TSwitchStatus.inactive) then Exit;

  ActivatorBox:= Activator.BoundingBox;
  SwitchBox:= Parent.BoundingBox;

  SwitchCenter:= SwitchBox.Center;
  FromRootActivator:= (SwitchCenter - Activator.Translation);
  ProjPoint:= Activator.Translation + ProjectionVectorAtoB(FromRootActivator, Activator.Up);

  if ActivatorBox.Contains(ProjPoint) then
  begin
    FromActivatorDir:= (SwitchCenter - ProjPoint).Normalize;
    if (TVector3.DotProduct(FromActivatorDir, Activator.Direction) > FAngleCOS) then
      Exit(TSwitchStatus.touched);
  end;

  Result:= TSwitchStatus.inactive;
end;

function TMyFrontSwitch.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'VisibleAngle'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

function TMyFrontSwitch.GetAngle: Single;
begin
  Result:= 180.0 * ArcCos(FAngleCOS) / Pi;
end;

procedure TMyFrontSwitch.SetAngle(value: Single);
begin
  FAngleCOS:= Cos(value * Pi / 180.0);
end;

initialization
  RegisterSerializableComponent(TMySwitch, ['Switches', 'Switch']);
  RegisterSerializableComponent(TMyFrontSwitch, ['Switches', 'Front Switch']);
end.

