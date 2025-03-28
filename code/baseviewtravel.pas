unit BaseViewTravel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CastleUIControls, CastleControls, CastleNotifications, CastleClassUtils,
  CastleColors, CastleKeysMouse, CastleTransform, CastleDebugTransform,
  CastleViewport,
  ViewWarper, NyaActorChara, NyaThirdPersonCameraNavigation,
  NyaSpectatorCameraNavigation, NyaThirdPersonCharaNavigation, NyaSwitch;

type
  TBaseViewPlay = class(TViewWarper)
  published
    Map: TCastleDesign;
    MainViewport: TCastleViewport;
    LabelFps: TCastleLabel;
    BtnBack: TCastleButton;
    GroupDressingButtons: TCastlePackedGroup;
    ImageControlDressing: TCastleImageControl;
    Notifications: TCastleNotifications;
    Status: TCastleLabel;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;
    procedure Pause; override;
    procedure Resume; override;
  protected
    FDebugAvatar: TDebugTransform;
    FCameraNavigationFollow: TNyaThirdPersonCameraNavigation;
    FKeyUse: TKey;
    FKeyDebug: TKey;
    FTouchedSwitch: TNyaSwitch;
    FGetToGo: TCastleView;
    procedure ClickControl(Sender: TObject);
    procedure ClickDress(Sender: TObject);
    procedure DoTouchSwitch(const Sender: TObject; Touch: Boolean); virtual;
    procedure DoActivateSwitch(Sender: TObject); virtual; abstract;
    procedure SetDressButtons;
    procedure SetUIColor;
    procedure SetSwitches;
    procedure SaveCharasCondition;
    procedure NavigationSetAnimation(
      const Sender: TNyaThirdPersonCharaNavigation;
      const AnimationName: String; AnimtionSpeed: Single);
  end;

implementation

uses
  GameViewMain, GameViewDressingMenu, GameViewLoading, CastleComponentSerialize,
  CastleScene, CastleFonts, CastleVectors,
  StrUtils, NyaCastleUtils;

constructor TBaseViewPlay.Create(AOwner: TComponent);
begin
  inherited;
  FGetToGo:= nil;
  DesignUrl := 'castle-data:/gameviewtravel.castle-user-interface';
end;

procedure TBaseViewPlay.Start;
var
  charaNavigation: TNyaThirdPersonCharaNavigation;
begin
  inherited;
  FTouchedSwitch:= nil;

  { set Main Viewport }
  MainViewport:= Map.DesignedComponent('ViewportMain') as TCastleViewport;

  { reset go to map }
  FGetToGo:= nil;

  { Visualize SceneAvatar bounding box, sphere, middle point, direction etc. }
  FDebugAvatar:= TDebugTransform.Create(FreeAtStop);
  FDebugAvatar.Parent:= MainActor;
  FDebugAvatar.Exists:= False;

  { set navigation }
  charaNavigation:= Map.DesignedComponent('CharaNavigation') as TNyaThirdPersonCharaNavigation;
  charaNavigation.OnAnimation:= {$ifdef FPC}@{$endif}NavigationSetAnimation;
  FCameraNavigationFollow:= Map.DesignedComponent('CameraNavigationFollow') as TNyaThirdPersonCameraNavigation;

  { set Buttons }
  BtnBack.OnClick:= {$ifdef FPC}@{$endif}ClickControl;

  { set keys }
  FKeyUse:= TKey.keyF;
  FKeyDebug:= TKey.keyF4;

  { set color }
  SetUIColor;

  { set Switches }
  SetSwitches;

  { set dress buttons }
  SetDressButtons;

  { show info }
  Notifications.Show('Info: use WASD for move');
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
    Notifications.Show('saving characters condition...');
    SaveCharasCondition();
    ViewLoading.SetToLoad(FGetToGo);
    Container.View:= ViewLoading;
  end;
end;

function TBaseViewPlay.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  { enable camera control }
  if Event.IsMouseButton(buttonRight) OR Event.IsMouseButton(buttonMiddle) then
  begin
    FCameraNavigationFollow.MouseLook:= True;
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
    FCameraNavigationFollow.MouseLook:= False;

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
    if (item.Tag = 1) then
      item.Color:= Vector4(MainActor.PersonalColor, alpha);
  end;
end;

procedure TBaseViewPlay.SetSwitches;
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

procedure TBaseViewPlay.SaveCharasCondition;
begin
  MainActor.SaveCondition;
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
    if (MainActor.ActorName = btnDress.Caption) then
      ViewDressingMenu.SetChara(MainActor);

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

  newBtn.Caption:= MainActor.ActorName;
  newBtn.OnClick:= {$ifdef FPC}@{$endif}ClickDress;
  GroupDressingButtons.InsertFront(newBtn);

  if Assigned(myBtnFactory) then
    FreeAndNil(myBtnFactory);
end;

procedure TBaseViewPlay.NavigationSetAnimation(
                        const Sender: TNyaThirdPersonCharaNavigation;
                        const AnimationName: String; AnimtionSpeed: Single);
begin
  MainActor.AutoAnimation:= AnimationName;
  MainActor.AnimationSpeed:= AnimtionSpeed;
end;

procedure TBaseViewPlay.DoTouchSwitch(const Sender: TObject; Touch: Boolean);
var
  switch: TNyaSwitch;
begin
  switch:= Sender as TNyaSwitch;
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

procedure TBaseViewPlay.Pause;
begin
  inherited;
  MainViewport.Items.TimeScale:= 0;
end;

procedure TBaseViewPlay.Resume;
begin
  inherited;
  MainViewport.Items.TimeScale:= 1;
end;

end.

