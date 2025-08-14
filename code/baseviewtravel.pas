unit BaseViewTravel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CastleUIControls, CastleControls, CastleNotifications, CastleClassUtils,
  CastleColors, CastleKeysMouse, CastleTransform,
  CastleViewport, CastleVectors,
  BaseView, NyaActorChara,
  NyaThirdPersonCameraNavigation, NyaThirdPersonCharaNavigation, NyaSwitch,
  NyaBaseNavigation, NyaWorldCondition;

type
  TBaseViewTravel = class(TBaseView)
  published
    LabelFps: TCastleLabel;
    BtnSettings: TCastleButton;
    BtnBack: TCastleButton;
    GroupDressingButtons: TCastlePackedGroup;
    Notifications: TCastleNotifications;
    Status: TCastleLabel;
    WorldCondition: TNyaWorldCondition;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  protected
    FCamera: TCastleCamera;
    FKeyUse: TKey;
    FTouchedSwitch: TNyaSwitch;
    procedure FocusButton(const Sender: TCastleUserInterface);
    procedure ClickControl(Sender: TObject);
    procedure ClickDress(Sender: TObject);
    procedure DoTouchSwitch(const Sender: TObject; Touch: Boolean); virtual;
    procedure DoActivateSwitch(Sender: TObject); virtual;
    procedure SetDressButtons;
    procedure SetSwitches;
    procedure SaveCharasCondition; virtual;
    procedure DoStart(Sender: TObject);
    procedure NavigationSetAnimation(
      const Sender: TNyaBaseNavigation;
      const AnimationName: String; AnimtionSpeed: Single);
    function PointVisible(const value: TVector3): boolean;
  end;

implementation

uses
  GameViewMain, GameViewDressing, GameViewLoading, GameViewSettings,
  CastleComponentSerialize,
  CastleSoundEngine, GameSound,
  CastleScene, CastleFonts, CastleCameras,
  StrUtils, NyaCastleUtils;

constructor TBaseViewTravel.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewtravel.castle-user-interface';
end;

procedure TBaseViewTravel.Start;
begin
  inherited;
  FTouchedSwitch:= nil;

  { set Camera }
  FCamera:= (Map.DesignedComponent('ViewportMain') as TCastleViewport).Camera;

  { set navigation }
  FCharaNavigation:= Map.DesignedComponent('CharaNavigation') as TNyaThirdPersonCharaNavigation;
  FCharaNavigation.OnAnimation:= {$ifdef FPC}@{$endif}NavigationSetAnimation;
  FCameraNavigation:= Map.DesignedComponent('CameraNavigationFollow') as TCastleMouseLookNavigation;

  { set Buttons }
  BtnSettings.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  BtnBack.OnClick:= {$ifdef FPC}@{$endif}ClickControl;

  BtnSettings.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusButton;
  BtnBack.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusButton;

  { set keys }
  FKeyUse:= TKey.keyF;

  { set UI color }
  SetUIColor(MainActor.PersonalColor);

  { set Switches }
  SetSwitches;

  { set dress buttons }
  SetDressButtons;

  { clear status info }
  Status.Caption:= '';

  { set initial action }
  WaitForRenderAndCall({$ifdef FPC}@{$endif}DoStart);
end;

procedure TBaseViewTravel.Stop;
begin
  SaveCharasCondition;
  WorldCondition.Boy.Dresser:= nil;
  inherited;
end;

procedure TBaseViewTravel.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  { Executed every frame. }

  { update FPS }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption:= 'FPS: ' + Container.Fps.ToString;

  inherited;
end;

function TBaseViewTravel.Press(const Event: TInputPressRelease): Boolean;
begin
  Result:= inherited;
  if Result then Exit; // allow the ancestor to handle keys

  { activate switch }
  if Event.IsKey(FKeyUse) then
  begin
    { activate switch }
    if Assigned(FTouchedSwitch) then
      FTouchedSwitch.Activate;
    Exit(true);
  end;
end;

procedure TBaseViewTravel.FocusButton(const Sender: TCastleUserInterface);
begin
  SoundEngine.Play(NamedSound('SfxButtonFocus'));
end;

procedure TBaseViewTravel.ClickControl(Sender: TObject);
var
  button: TCastleButton;
begin
  button:= Sender as TCastleButton;
  if NOT Assigned(button) then exit;

  SoundEngine.Play(NamedSound('SfxButtonPress'));

  Case button.Name of
    'BtnSettings':
        if NOT (Container.FrontView is TViewSettings) then
          Container.PushView(TViewSettings.CreateUntilStopped);
    'BtnBack':
        GetToGo(ViewMain);
  end;
end;

procedure TBaseViewTravel.SetSwitches;
var
  behaviors: TCastleBehaviors;
  behavior: TCastleBehavior;
  switch: TNyaSwitch;
begin
  behaviors:= GetAllBehaviors(Map, TNyaSwitch);

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

procedure TBaseViewTravel.SaveCharasCondition;
begin
  if (MainActor is TNyaActorChara) then
    (MainActor as TNyaActorChara).SaveCondition;
end;

procedure TBaseViewTravel.ClickDress(Sender: TObject);
var
  btnDress: TCastleButton;
  i: integer;
  dressFound: Boolean;
  actorChara: TNyaActorChara;
begin
  btnDress:= Sender as TCastleButton;
  if NOT Assigned(btnDress) then Exit;

  SoundEngine.Play(NamedSound('SfxButtonPress'));

  { Check if Dreesing menu already opened }
  dressFound:= False;
  for i:= 0 to (Container.ViewStackCount - 1) do
  begin
    if (Container.ViewStack[i] is TViewDressing) then
    begin
      dressFound:= False;
      break;
    end;
  end;

  { Show Dressing Menu }
  if NOT dressFound then
  begin
    { check selected chara }
    if (MainActor is TNyaActorChara) then
    begin
      actorChara:= MainActor as TNyaActorChara;
      if (actorChara.ActorName = btnDress.Caption) then
        Container.PushView(TViewDressing.CreateUntilStopped(actorChara));
    end;
  end;
end;

procedure TBaseViewTravel.SetDressButtons;
var
  newBtn, sampleBtn: TCastleButton;
  myBtnFactory: TCastleComponentFactory;
  myFont: TCastleAbstractFont;
begin
  if ((GroupDressingButtons.ControlsCount > 0) AND
      (GroupDressingButtons.Controls[0] is TCastleButton)) then
  begin
    sampleBtn:= GroupDressingButtons.Controls[0] as TCastleButton;
    myFont:= sampleBtn.CustomFont;
    myBtnFactory:= TCastleComponentFactory.Create(self);
    myBtnFactory.LoadFromComponent(sampleBtn);
  end else
  begin
    sampleBtn:= nil;
    myBtnFactory:= nil;
  end;

  GroupDressingButtons.ClearControls;

  if Assigned(myBtnFactory) then
  begin
    newBtn:= myBtnFactory.ComponentLoad(GroupDressingButtons) as TCastleButton;
    newBtn.CustomFont:= myFont;
  end else
    newBtn:= TCastleButton.Create(GroupDressingButtons);

  newBtn.Caption:= MainActor.ActorName;
  newBtn.OnClick:= {$ifdef FPC}@{$endif}ClickDress;
  newBtn.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusButton;
  GroupDressingButtons.InsertFront(newBtn);

  if Assigned(myBtnFactory) then
    FreeAndNil(myBtnFactory);
end;

procedure TBaseViewTravel.NavigationSetAnimation(
                        const Sender: TNyaBaseNavigation;
                        const AnimationName: String; AnimtionSpeed: Single);
begin
  MainActor.AutoAnimation:= AnimationName;
  MainActor.AnimationSpeed:= AnimtionSpeed;
end;

function TBaseViewTravel.PointVisible(const value: TVector3): boolean;
var
  dot: Single;
begin
  dot:= TVector3.DotProduct(FCamera.Direction,
                            (value - FCamera.Translation).Normalize);
  Result:= (dot >= 0.0);
end;

procedure TBaseViewTravel.DoTouchSwitch(const Sender: TObject; Touch: Boolean);
var
  switch: TNyaSwitch;
begin
  switch:= Sender as TNyaSwitch;
  if NOT Assigned(switch) then Exit;

  if Touch then
  begin
    if(FTouchedSwitch <> switch) then
    begin
      Status.Caption:= 'Press "' + GetKeyName(FKeyUse) + '" to ' +
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

procedure TBaseViewTravel.DoActivateSwitch(Sender: TObject);
begin
  SoundEngine.Play(NamedSound('SfcActivate'));
end;

procedure TBaseViewTravel.DoStart(Sender: TObject);
begin
  Notifications.Show('Info: use WASD to move');
  Notifications.Show('Info: hold Left Mouse Button to rotate');
  Notifications.Show('Info: use SHIFT to run');
end;

end.

