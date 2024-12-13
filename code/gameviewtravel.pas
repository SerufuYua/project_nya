unit GameViewTravel;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  CastleThirdPersonNavigation, CastleTransform, CastleNotifications,
  ActorChara, MyThirdPersonCameraNavigation, MyThirdPersonCharaNavigation,
  CastleDebugTransform;

type
  TViewTravel = class(TCastleView)
  published
    Chara: TCastleTransformDesign;
    Map: TCastleTransformDesign;
    LabelFps: TCastleLabel;
    BtnBack: TCastleButton;
    CameraNavigation: TMyThirdPersonCameraNavigation;
    CameraMain: TCastleCamera;
    GroupDressingButtons: TCastlePackedGroup;
    ImageControlDressing: TCastleImageControl;
    Notifications: TCastleNotifications;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  protected
    FActorMain: TActorChara;
    DebugAvatar: TDebugTransform;
    procedure ClickControl(Sender: TObject);
    procedure ClickDress(Sender: TObject);
    procedure ChangedEmission(value: Single);
    procedure SaveCharasCondition();
    procedure SetDressButtons();
    procedure NavigationSetAnimation(
              const Sender: TCastleThirdPersonNavigation;
              const AnimationNames: array of String);
  end;

var
  ViewTravel: TViewTravel;

implementation

uses
  GameViewLoading, GameViewMain, CastleViewport, CastleScene,
  CastleComponentSerialize, CastleFonts, SysUtils, GameViewDressingMenu;

constructor TViewTravel.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewtravel.castle-user-interface';
end;

procedure TViewTravel.Start;
var
  viewportMain: TCastleViewport;
  skyMain: TCastleBackground;
  fogMain: TCastleFog;
begin
  inherited;

  { Create Girl Character instance }
  FActorMain:= TActorChara.Create(Chara, 'Girl');

  { Visualize SceneAvatar bounding box, sphere, middle point, direction etc. }
  DebugAvatar:= TDebugTransform.Create(FreeAtStop);
  DebugAvatar.Parent:= Chara;
  DebugAvatar.Exists:= True;

  { set cahara animation event }
//  CameraNavigation.OnAnimation:= {$ifdef FPC}@{$endif}NavigationSetAnimation;

  { set characters self emission }
  ChangedEmission(0.3);

  { set Buttons }
  BtnBack.OnClick:= {$ifdef FPC}@{$endif}ClickControl;

  viewportMain:= DesignedComponent('ViewportMain') as TCastleViewport;

  { set Sky }
  skyMain:= Map.DesignedComponent('Sky', False) as TCastleBackground;
  if Assigned(skyMain) then
    viewportMain.Background:= skyMain;

  { set Fog }
  fogMain:= Map.DesignedComponent('Fog', False) as TCastleFog;
  if Assigned(fogMain) then
    viewportMain.Fog:= fogMain;

  { set dress buttons }
  SetDressButtons();
end;

procedure TViewTravel.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  { Executed every frame. }

  { update FPS }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption:= 'FPS: ' + Container.Fps.ToString;

  { Release Dressing Menu Buttons }
  if NOT (Container.FrontView = ViewDressingMenu) then
    ImageControlDressing.Exists:= True;
end;

function TViewTravel.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  Notifications.Show(Event.ToString);

  if Event.IsMouseButton(buttonRight) then
  begin
    CameraNavigation.MouseLook := not CameraNavigation.MouseLook;
    Exit(true);
  end;

end;

procedure TViewTravel.ChangedEmission(value: Single);
begin
  FActorMain.SelfEmission:= value;
end;

procedure TViewTravel.SaveCharasCondition();
begin
  FActorMain.SaveCondition;
end;

procedure TViewTravel.ClickDress(Sender: TObject);
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

procedure TViewTravel.ClickControl(Sender: TObject);
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

procedure TViewTravel.SetDressButtons();
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

procedure TViewTravel.NavigationSetAnimation(
                      const Sender: TCastleThirdPersonNavigation;
                      const AnimationNames: array of String);
begin
  if (Length(AnimationNames) > 0) then
    FActorMain.AutoAnimation:= AnimationNames[0];
end;

end.
