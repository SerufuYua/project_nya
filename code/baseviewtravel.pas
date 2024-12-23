unit BaseViewTravel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CastleUIControls, CastleControls, CastleNotifications, CastleClassUtils,
  CastleColors, CastleKeysMouse, CastleTransform, CastleDebugTransform,
  FadeInOut, ActorChara,
  MyThirdPersonCameraNavigation, MySpectatorCameraNavigation,
  MyThirdPersonCharaNavigation, MySwitch;

type
  TBaseViewPlay = class(TCastleView)
  published
    Map: TCastleTransformDesign;
    LabelFps: TCastleLabel;
    BtnBack: TCastleButton;
    CameraNavigationSpectator: TMySpectatorCameraNavigation;
    CameraNavigationFollow: TMyThirdPersonCameraNavigation;
    CharaNavigation: TMyThirdPersonCharaNavigation;
    GroupDressingButtons: TCastlePackedGroup;
    ImageControlDressing: TCastleImageControl;
    MySwitchTest: TMySwitch;
    Notifications: TCastleNotifications;
    Status: TCastleLabel;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;
  protected
    FActorMain: TActorChara;
    FDebugAvatar: TDebugTransform;
    FKeyUse: TKey;
    FKeyDebug: TKey;
    FTouchedSwitch: TMySwitch;
    FGetToGo: TCastleView;
    procedure ClickControl(Sender: TObject);
    procedure ClickDress(Sender: TObject);
    procedure TouchSwitch(const Sender: TObject; Touch: Boolean); virtual;
    procedure ActivateSwitch(Sender: TObject); virtual; abstract;
    procedure SetDressButtons;
    procedure SetUIColor;
    procedure SetSwitches;
    procedure SaveCharasCondition;
    procedure NavigationSetAnimation(
      const Sender: TMyThirdPersonCharaNavigation;
      const AnimationName: String; AnimtionSpeed: Single);
  end;

implementation

uses
  GameViewMain, GameViewDressingMenu, GameViewLoading, CastleComponentSerialize,
  CastleScene, CastleFonts, CastleViewport, CastleVectors,
  StrUtils, MyCastleUtils;

constructor TBaseViewPlay.Create(AOwner: TComponent);
begin
  inherited;
  FGetToGo:= nil;
  DesignUrl := 'castle-data:/gameviewtravel.castle-user-interface';
end;

procedure TBaseViewPlay.Start;
var
  viewportMain: TCastleViewport;
  cameraMain: TCastleCamera;
  skyMain: TCastleBackground;
  fogMain: TCastleFog;
begin
  inherited;
  FTouchedSwitch:= nil;

  { set Navigation }
  CharaNavigation.AvatarHierarchy:= FActorMain.ActorRoot;
  CameraNavigationSpectator.AvatarHierarchy:= FActorMain.ActorRoot;
  CameraNavigationFollow.AvatarHierarchy:= FActorMain.ActorRoot;

  { Visualize SceneAvatar bounding box, sphere, middle point, direction etc. }
  FDebugAvatar:= TDebugTransform.Create(FreeAtStop);
  FDebugAvatar.Parent:= FActorMain.ActorRoot;
  FDebugAvatar.Exists:= False;

  { set cahara animation event }
  CharaNavigation.OnAnimation:= {$ifdef FPC}@{$endif}NavigationSetAnimation;

  { set characters self emission }
  FActorMain.SelfEmission:= 0.15;

  { set Buttons }
  BtnBack.OnClick:= {$ifdef FPC}@{$endif}ClickControl;

  { set Camera }
  viewportMain:= DesignedComponent('ViewportMain') as TCastleViewport;
  cameraMain:= Map.DesignedComponent('CameraMain') as TCastleCamera;
  viewportMain.Camera:= cameraMain;

  { set Sky }
  skyMain:= Map.DesignedComponent('Sky', False) as TCastleBackground;
  if Assigned(skyMain) then
    viewportMain.Background:= skyMain;

  { set Fog }
  fogMain:= Map.DesignedComponent('Fog', False) as TCastleFog;
  if Assigned(fogMain) then
    viewportMain.Fog:= fogMain;

  { set keys }
  FKeyUse:= TKey.keyF;
  FKeyDebug:= TKey.keyF4;

  { set color }
  SetUIColor;

  { set Switches }
  SetSwitches;

  { set dress buttons }
  SetDressButtons;
end;

procedure TBaseViewPlay.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  { Executed every frame. }

  { update FPS }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption:= 'FPS: ' + Container.Fps.ToString;

  { Release Dressing Menu Buttons }
  if NOT (Container.FrontView = ViewDressingMenu) then
    ImageControlDressing.Exists:= True;

  { change map }
  if Assigned(FGetToGo) then
  begin
    ViewLoading.SetToLoad(FGetToGo);
    Container.View:= ViewLoading;
  end;
end;

function TBaseViewPlay.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  Notifications.Show(Event.ToString);

  { enable camera control }
  if Event.IsMouseButton(buttonRight) OR Event.IsMouseButton(buttonMiddle) then
  begin
    CameraNavigationFollow.MouseLook:= True;
    Exit(true);
  end;

  { show debug }
  if Event.IsKey(FKeyDebug) then
    FDebugAvatar.Exists:= NOT FDebugAvatar.Exists;

  { activate switch }
  if Event.IsKey(FKeyUse) then
  begin
    { activate switch }
    if Assigned(FTouchedSwitch) then
      FTouchedSwitch.Activate;
    Exit(true);
  end;
end;

function TBaseViewPlay.Release(const Event: TInputPressRelease): boolean;
begin
  { disable camera control }
  if Event.IsMouseButton(buttonRight) OR Event.IsMouseButton(buttonMiddle) then
    CameraNavigationFollow.MouseLook:= False;

  Result := inherited;
end;

procedure TBaseViewPlay.ClickControl(Sender: TObject);
var
  button: TCastleButton;
begin
  button:= Sender as TCastleButton;
  if NOT Assigned(button) then exit;

  Case button.Name of
  'BtnBack':
    begin
      Notifications.Show('saving characters condition...');
      SaveCharasCondition();
      ViewLoading.SetToLoad(ViewMain);
      Container.View:= ViewLoading;
    end;
  end;
end;

procedure TBaseViewPlay.SetUIColor;
var
  rootItem: TCastleUserInterface;
  item: TCastleImageControl;
  alpha: single;
begin
  rootItem:= DesignedComponent('SceneMain') as TCastleUserInterface;

  for item in GetAllUIImages(rootItem) do
  begin
      alpha:= item.Color.W;
      item.Color:= Vector4(FActorMain.PersonalColor, alpha);
  end;
end;

procedure TBaseViewPlay.SetSwitches;
var
  behaviors: TCastleBehaviors;
  behavior: TCastleBehavior;
  switch: TMySwitch;
begin
  behaviors:= GetAllBehavior(Map, TMySwitch);

  for behavior in behaviors do
  begin
    switch:= behavior as TMySwitch;
    if Assigned(switch) then
    begin
      switch.OnTouch:= {$ifdef FPC}@{$endif}TouchSwitch;
      switch.OnActivate:= {$ifdef FPC}@{$endif}ActivateSwitch;
    end;
  end;
end;

procedure TBaseViewPlay.SaveCharasCondition;
begin
  FActorMain.SaveCondition;
end;

procedure TBaseViewPlay.ClickDress(Sender: TObject);
var
  btnDress: TCastleButton;
begin
  btnDress:= Sender as TCastleButton;
  if NOT Assigned(btnDress) then Exit;

  { Show Dressing Menu }
  if NOT (Container.FrontView = ViewDressingMenu) then
  begin
    Container.PushView(ViewDressingMenu);

    { check selected chara }
    if (FActorMain.ActorName = btnDress.Caption) then
      ViewDressingMenu.SetChara(FActorMain);

    ImageControlDressing.Exists:= False;
  end;
end;

procedure TBaseViewPlay.SetDressButtons;
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

  newBtn.Caption:= FActorMain.ActorName;
  newBtn.OnClick:= {$ifdef FPC}@{$endif}ClickDress;
  GroupDressingButtons.InsertFront(newBtn);

  if Assigned(myBtnFactory) then
    FreeAndNil(myBtnFactory);
end;

procedure TBaseViewPlay.NavigationSetAnimation(
                        const Sender: TMyThirdPersonCharaNavigation;
                        const AnimationName: String; AnimtionSpeed: Single);
begin
  FActorMain.AutoAnimation:= AnimationName;
  FActorMain.SetSpeed(AnimtionSpeed);
end;

procedure TBaseViewPlay.TouchSwitch(const Sender: TObject; Touch: Boolean);
var
  switch: TMySwitch;
begin
  switch:= Sender as TMySwitch;
  if NOT Assigned(switch) then Exit;

  if Touch then
    Status.Caption:= 'Press "' + GetKeyName(FKeyUse) + '" to ' +
                     switch.ActionString
  else
    Status.Caption:= '';

  if Touch then
  begin
    if(FTouchedSwitch <> switch) then
      FTouchedSwitch:= switch;
  end else
  begin
    if(FTouchedSwitch = switch) then
      FTouchedSwitch:= nil;
  end;
end;

end.

