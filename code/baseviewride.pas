unit BaseViewRide;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CastleUIControls, CastleControls, CastleNotifications, CastleClassUtils,
  CastleColors, CastleKeysMouse, CastleTransform,
  CastleViewport, CastleVectors,
  BaseView, NyaActorChara,
  NyaSpectatorCameraNavigation, NyaVehicleNavigation, NyaSwitch,
  NyaBaseNavigation, NyaWorldCondition, NyaFollow;

type
  TBaseViewRide = class(TBaseView)
  published
    LabelFps: TCastleLabel;
    BtnSettings: TCastleButton;
    BtnBack: TCastleButton;
    Notifications: TCastleNotifications;
    Status: TCastleLabel;
    LabelSpeedValue, LabelTimeValue: TCastleLabel;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  protected
    FCamera: TCastleCamera;
    FLightSwitch: TKey;
    FTouchedSwitch: TNyaSwitch;
    FCheckPointNum: Integer;
    FEnableTimer: Boolean;
    FTrackTimer: Single;
    procedure FocusButton(const Sender: TCastleUserInterface);
    procedure ClickControl(Sender: TObject);
    procedure DoTouchSwitch(const Sender: TObject; Touch: Boolean); virtual;
    procedure SetSwitches;
    procedure DoStart(Sender: TObject);
    procedure NavigationSetAnimation(
      const Sender: TNyaBaseNavigation;
      const AnimationName: String; AnimtionSpeed: Single);
    function PointVisible(const value: TVector3): boolean;
  end;

implementation

uses
  GameViewMain, GameViewTravelRoadAsteroid, GameViewDressingMenu,
  GameViewLoading, GameViewSettingsMenu, NyaActorVehicle,
  CastleComponentSerialize,
  CastleSoundEngine, GameSound,
  CastleScene, CastleFonts, CastleCameras, CastleUtils,
  StrUtils, NyaCastleUtils;

constructor TBaseViewRide.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewride.castle-user-interface';
end;

procedure TBaseViewRide.Start;
var
  charaNavigation: TNyaVehicleNavigation;
begin
  inherited;
  FTouchedSwitch:= nil;
  FCheckPointNum:= 0;
  FEnableTimer:= False;
  FTrackTimer:= 0.0;

  { set Camera }
  FCamera:= (Map.DesignedComponent('ViewportMain') as TCastleViewport).Camera;

  { set navigation }
  charaNavigation:= Map.DesignedComponent('VehicleNavigation') as TNyaVehicleNavigation;
  charaNavigation.OnAnimation:= {$ifdef FPC}@{$endif}NavigationSetAnimation;
  FCameraNavigation:= Map.DesignedComponent('CameraNavigationFollow') as TCastleMouseLookNavigation;

  { set Buttons }
  BtnSettings.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  BtnBack.OnClick:= {$ifdef FPC}@{$endif}ClickControl;

  BtnSettings.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusButton;
  BtnBack.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusButton;

  { set keys }
  FLightSwitch:= TKey.keyF;

  { set color }
  SetUIColor(MainActor.PersonalColor);

  { set Switches }
  SetSwitches;

  { clear status info }
  Status.Caption:= '';

  { set initial action }
  WaitForRenderAndCall({$ifdef FPC}@{$endif}DoStart);
end;

procedure TBaseViewRide.Stop;
begin
  inherited;
end;

procedure TBaseViewRide.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  { Executed every frame. }

  { update FPS }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption:= 'FPS: ' + Container.Fps.ToString;

  { update Track Timer }
  if FEnableTimer then
    FTrackTimer:= FTrackTimer + SecondsPassed;

  { Show Time }
  LabelTimeValue.Caption:= FTrackTimer.ToString(ffFixed, 3, 2);

  { Show Speed. Convert m/s to km/h }
  LabelSpeedValue.Caption:= (MainActor.ForwardVelocity * 60 * 60 / 1000).ToString(ffFixed, 3, 0);

  inherited;
end;

function TBaseViewRide.Press(const Event: TInputPressRelease): Boolean;
var
  motorbike: TNyaActorVehicle;
begin
  Result:= inherited;
  if Result then Exit; // allow the ancestor to handle keys

  { switch light }
  if Event.IsKey(FLightSwitch) then
  begin
    if (MainActor is TNyaActorVehicle) then
    begin
      motorbike:= (MainActor as TNyaActorVehicle);
      motorbike.Headlight:= NOT motorbike.Headlight;
    end;
    Exit(true);
  end;
end;

procedure TBaseViewRide.FocusButton(const Sender: TCastleUserInterface);
begin
  SoundEngine.Play(NamedSound('SfxButtonFocus'));
end;

procedure TBaseViewRide.ClickControl(Sender: TObject);
var
  button: TCastleButton;
begin
  button:= Sender as TCastleButton;
  if NOT Assigned(button) then exit;

  SoundEngine.Play(NamedSound('SfxButtonPress'));

  Case button.Name of
    'BtnSettings':
        if NOT (Container.FrontView is TViewSettingsMenu) then
          Container.PushView(TViewSettingsMenu.CreateUntilStopped);
    'BtnBack':
        GetToGo(ViewTravelRoadAsteroid, Vector3(-3.4, 0.2, -16.5),
                                        Vector4(0.0, -1.0, 0.0, Deg(-205.0)));
  end;
end;

procedure TBaseViewRide.SetSwitches;
var
  behaviors: TCastleBehaviors;
  behavior: TCastleBehavior;
  switch: TNyaSwitch;
begin
  behaviors:= GetAllBehavior(Map, TNyaSwitch);

  for behavior in behaviors do
  begin
    switch:= behavior as TNyaSwitch;
    if Assigned(switch) then
    begin
      switch.OnTouch:= {$ifdef FPC}@{$endif}DoTouchSwitch;
    end;
  end;
end;

procedure TBaseViewRide.NavigationSetAnimation(
                        const Sender: TNyaBaseNavigation;
                        const AnimationName: String; AnimtionSpeed: Single);
begin
  MainActor.AutoAnimation:= AnimationName;
  MainActor.AnimationSpeed:= AnimtionSpeed;
end;

function TBaseViewRide.PointVisible(const value: TVector3): boolean;
var
  dot: Single;
begin
  dot:= TVector3.DotProduct(FCamera.Direction,
                            (value - FCamera.Translation).Normalize);
  Result:= (dot >= 0.0);
end;

procedure TBaseViewRide.DoTouchSwitch(const Sender: TObject; Touch: Boolean);
var
  switch: TNyaSwitch;
begin
  switch:= Sender as TNyaSwitch;
  if NOT Assigned(switch) then Exit;

  if Touch then
  begin
    if(FTouchedSwitch <> switch) then
    begin
      Status.Caption:= switch.ActionString;
      FTouchedSwitch:= switch;
      SoundEngine.Play(NamedSound('SfxInspect'));
    end;
  end else
  begin
    if(FTouchedSwitch = switch) then
    begin
      FTouchedSwitch:= nil;
      Status.Caption:= '';
    end;

    { process Check Points }
    if ((switch.Name = 'CheckSwitchStart') AND (NOT FEnableTimer)) then
    begin
      FTrackTimer:= 0.0;
      FEnableTimer:= True;
    end;

    if (FCheckPointNum = (switch.Tag - 1)) then
    begin
      FCheckPointNum:= switch.Tag;

      if switch.Name = 'CheckSwitchFinish' then
      begin
        FEnableTimer:= False;
        FCheckPointNum:= 0;
      end;
    end else
      Notifications.Show('Warning: wrong way');
  end;
end;

procedure TBaseViewRide.DoStart(Sender: TObject);
begin
  Notifications.Show('Info: use WASD to move');
  Notifications.Show('Info: use Space to brake');
  Notifications.Show('Info: use F to switch headlight');
end;

end.

