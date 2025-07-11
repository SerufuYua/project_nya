unit BaseViewRide;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CastleUIControls, CastleControls, CastleNotifications, CastleClassUtils,
  CastleColors, CastleKeysMouse, CastleTransform, CastleDebugTransform,
  CastleViewport, CastleVectors,
  ViewWarper, NyaActorChara, NyaThirdPersonCameraNavigation,
  NyaSpectatorCameraNavigation, NyaVehicleNavigation, NyaSwitch,
  NyaBaseNavigation, NyaWorldCondition, NyaFollow;

type
  TBaseViewRide = class(TViewWarper)
  published
    Map: TCastleDesign;
    MainViewport: TCastleViewport;
    LabelFps: TCastleLabel;
    BtnSettings: TCastleButton;
    BtnBack: TCastleButton;
    Notifications: TCastleNotifications;
    Status: TCastleLabel;
    LabelSpeedValue: TCastleLabel;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;
    procedure Pause; override;
    procedure Resume; override;
  protected
    FDebugAvatar: TDebugTransform;
    FCameraNavigationFollow: TNyaThirdPersonCameraNavigation;
    FCamera: TCastleCamera;
    FLightSwitch: TKey;
    FKeyDebug: TKey;
    FTouchedSwitch: TNyaSwitch;
    procedure FocusButton(const Sender: TCastleUserInterface);
    procedure ClickControl(Sender: TObject);
    procedure DoTouchSwitch(const Sender: TObject; Touch: Boolean); virtual;
    procedure DoActivateSwitch(Sender: TObject); virtual;
    procedure SetUIColor;
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
  CastleScene, CastleFonts, CastleUtils,
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

  { set Main Viewport }
  MainViewport:= Map.DesignedComponent('ViewportMain') as TCastleViewport;

  { reset go to map }
  FGetToGo:= nil;

  { set Camera }
  FCamera:= (Map.DesignedComponent('ViewportMain') as TCastleViewport).Camera;

  { Visualize SceneAvatar bounding box, sphere, middle point, direction etc. }
  FDebugAvatar:= TDebugTransform.Create(FreeAtStop);
  FDebugAvatar.Parent:= MainActor;
  FDebugAvatar.Exists:= False;

  { set navigation }
  charaNavigation:= Map.DesignedComponent('VehicleNavigation') as TNyaVehicleNavigation;
  charaNavigation.OnAnimation:= {$ifdef FPC}@{$endif}NavigationSetAnimation;
  FCameraNavigationFollow:= Map.DesignedComponent('CameraNavigationFollow') as TNyaThirdPersonCameraNavigation;

  { set Buttons }
  BtnSettings.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  BtnBack.OnClick:= {$ifdef FPC}@{$endif}ClickControl;

  BtnSettings.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusButton;
  BtnBack.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusButton;

  { set keys }
  FLightSwitch:= TKey.keyF;
  FKeyDebug:= TKey.keyF4;

  { set color }
  SetUIColor;

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

  { enable camera control }
  if Event.IsMouseButton(buttonRight) OR Event.IsMouseButton(buttonMiddle) then
  begin
    FCameraNavigationFollow.MouseLook:= True;
    Exit(true);
  end;

  { show debug }
  if Event.IsKey(FKeyDebug) then
  begin
    FDebugAvatar.Exists:= NOT FDebugAvatar.Exists;
    Exit(true);
  end;

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

function TBaseViewRide.Release(const Event: TInputPressRelease): boolean;
begin
  { disable camera control }
  if Event.IsMouseButton(buttonRight) OR Event.IsMouseButton(buttonMiddle) then
    FCameraNavigationFollow.MouseLook:= False;

  Result := inherited;
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

procedure TBaseViewRide.SetUIColor;
var
  rootItem: TCastleUserInterface;
  item: TCastleImageControl;
  alpha: single;
begin
  rootItem:= DesignedComponent('SceneMain') as TCastleUserInterface;

  for item in GetAllUIImages(rootItem) do
  begin
    alpha:= item.Color.W;
    if (item.Tag = 1) then
      item.Color:= Vector4(MainActor.PersonalColor, alpha);
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
      switch.OnActivate:= {$ifdef FPC}@{$endif}DoActivateSwitch;
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
      Status.Caption:= 'Press "' + GetKeyName(FLightSwitch) + '" to ' +
                       switch.ActionString;
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
  end;
end;

procedure TBaseViewRide.DoActivateSwitch(Sender: TObject);
begin
  SoundEngine.Play(NamedSound('SfcActivate'));
end;

procedure TBaseViewRide.Pause;
begin
  inherited;
  FCameraNavigationFollow.MouseLook:= False;
  MainViewport.Items.TimeScale:= 0;
end;

procedure TBaseViewRide.Resume;
begin
  inherited;
  MainViewport.Items.TimeScale:= 1;
end;

procedure TBaseViewRide.DoStart(Sender: TObject);
begin
  Notifications.Show('Info: use WASD to move');
  Notifications.Show('Info: use Space to brake');
  Notifications.Show('Info: use F to switch headlight');
end;

end.

