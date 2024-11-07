unit GameViewPlayGirl;

interface

uses Classes,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleColors,
  CastleTransform, CastleNotifications, CastleClassUtils,
  ActorChara, ActorToyA, ActorsLogic, FadeInOut,
  CastleParticleEmitter;

type
  TViewPlayGirl = class(TCastleView)
  published
    LabelFps: TCastleLabel;
    BtnBack: TCastleButton;
    BtnStop: TCastleButton;
    BtnNext: TCastleButton;
    FloatSliderEmission: TCastleFloatSlider;
    BtnEmission: TCastleButton;
    FloatSliderSpeed: TCastleFloatSlider;
    FloatSliderPleasure: TCastleFloatSlider;
    FloatSliderTension: TCastleFloatSlider;
    RectangleControlDressing: TCastleRectangleControl;
    GroupDressingButtons: TCastlePackedGroup;
    GroupActionSelect: TCastlePackedGroup;
    ImageScreen: TCastleImageControl;
    Notifications: TCastleNotifications;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single;
                     var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  private
    FActorGirl: TActorChara;
    FActorToyA: TActorToyA;
    FActorsLogic: TActorsLogic;
    FScreenFader: TImageFader;
    procedure ClickAction(Sender: TObject);
    procedure ClickDress(Sender: TObject);
    procedure ClickControl(Sender: TObject);
    procedure ChangedEmission(Sender: TObject);
    procedure ChangedSpeed(Sender: TObject);
    procedure SetDressButtons();
    procedure SetActionsList(actList: TCastleComponent);
    procedure SetUIColor(newColor: TCastleColorRGB);
    procedure ScreenShot;
    procedure DoStart(Sender: TObject);
  end;

var
  ViewPlayGirl: TViewPlayGirl;

implementation

uses
  GameViewMain, GameViewDressingMenu, CastleScene, CastleViewport,
  GameViewLoading, SysUtils, CastleVectors;

constructor TViewPlayGirl.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl:= 'castle-data:/gameviewplay.castle-user-interface';
end;

procedure TViewPlayGirl.Start;
var
  girlScene, toysScene, mapScene: TCastleTransformDesign;
  cameraMain: TCastleCamera;
  viewportMain: TCastleViewport;
  skyMain: TCastleBackground;
  fogMain: TCastleFog;
begin
  inherited;

  { set Buttons }
  BtnBack.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  BtnStop.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  BtnNext.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  BtnEmission.OnClick:= {$ifdef FPC}@{$endif}ChangedEmission;
  FloatSliderSpeed.OnChange:= {$ifdef FPC}@{$endif}ChangedSpeed;

  { set fade animator }
  FScreenFader:= TImageFader.Create(ImageScreen, Container);

  { set map }
  mapScene:= DesignedComponent('Map') as TCastleTransformDesign;
  mapScene.Url:= 'castle-data:/MapPlayGirlToyA.castle-transform';

  { Create Girl Character instance }
  girlScene:= mapScene.DesignedComponent('CharaGirl') as TCastleTransformDesign;
  FActorGirl:= TActorChara.Create(girlScene, 'Girl');

  { Create Toys instance }
  toysScene:= mapScene.DesignedComponent('ToyA') as TCastleTransformDesign;
  FActorToyA:= TActorToyA.Create(toysScene, 'ToyA');

  { Create Actors Logic }
  FActorsLogic:= TActorsLogic.Create(FActorGirl, FActorToyA,
                                     'GAME.GIRL_TOYA.PLAY',
                                     FScreenFader);
  { set Camera }
  viewportMain:= DesignedComponent('ViewportMain') as TCastleViewport;
  cameraMain:= mapScene.DesignedComponent('CameraMain') as TCastleCamera;
  viewportMain.Camera:= cameraMain;

  { set Sky }
  skyMain:= mapScene.DesignedComponent('Sky', False) as TCastleBackground;
  if Assigned(skyMain) then
    viewportMain.Background:= skyMain;

  { set Fog }
  fogMain:= mapScene.DesignedComponent('Fog', False) as TCastleFog;
  if Assigned(fogMain) then
    viewportMain.Fog:= fogMain;

  { set characters self emission }
  ChangedEmission(FloatSliderEmission);

  { set dress buttons }
  SetDressButtons();

  { set actions list }
  SetActionsList(mapScene.DesignedComponent('ActionsList') as TCastleComponent);

  { set color }
  SetUIColor(FActorsLogic.CharasColor);

  { set initial action }
  WaitForRenderAndCall({$ifdef FPC}@{$endif}DoStart);
end;

procedure TViewPlayGirl.Update(const SecondsPassed: Single;
                               var HandleInput: boolean);
begin
  inherited;
  { Executed every frame. }

  { update FPS }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption:= 'FPS: ' + Container.Fps.ToString;

  { update fader }
  FScreenFader.AnimateQuadFade(SecondsPassed);

  { upade gauges }
  FActorsLogic.Update(SecondsPassed);
  FloatSliderPleasure.Value:= FActorsLogic.Pleasure;
  FloatSliderTension.Value:= FActorsLogic.Tension;

  { Release Dressing Menu Buttons }
  if NOT (Container.FrontView = ViewDressingMenu) then
    RectangleControlDressing.Exists:= True;
end;

procedure TViewPlayGirl.ClickAction(Sender: TObject);
var
  btnDress: TCastleButton;
begin
  btnDress:= Sender as TCastleButton;
  if NOT Assigned(btnDress) then Exit;

  FActorsLogic.Stop;
  FActorsLogic.SetAction(btnDress.Tag);
end;

procedure TViewPlayGirl.ClickDress(Sender: TObject);
var
  chara: TActorChara;
  btnDress: TCastleButton;
begin
  btnDress:= Sender as TCastleButton;
  if NOT Assigned(btnDress) then Exit;

  { Show Dressing Menu }
  if NOT (Container.FrontView = ViewDressingMenu) then
  begin
    Container.PushView(ViewDressingMenu);

    { find selected chara }
    for chara in FActorsLogic.Charas do
    begin
      if (chara.ActorName = btnDress.Caption) then
      begin
        ViewDressingMenu.SetChara(chara);
        Break;
      end;
    end;

    RectangleControlDressing.Exists:= False;
  end;
end;

procedure TViewPlayGirl.ClickControl(Sender: TObject);
var
  button: TCastleButton;
begin
  button:= Sender as TCastleButton;
  if NOT Assigned(button) then exit;

  Case button.Name of
  'BtnBack':
    begin
      FActorGirl.SaveCondition;
      ViewLoading.SetToLoad(ViewMain);
      Container.View:= ViewLoading;
    end;
  'BtnStop':
    begin
      FActorsLogic.Stop;
    end;
  'BtnNext':
    begin
      FActorsLogic.NextPart;
    end;
  end;
end;

procedure TViewPlayGirl.SetDressButtons();
var
  chara: TActorChara;
  newBtn: TCastleButton;
begin
  GroupDressingButtons.ClearControls;

  for chara in FActorsLogic.Charas do
  begin
    newBtn:= TCastleButton.Create(GroupDressingButtons);
    newBtn.Caption:= chara.ActorName;
    newBtn.OnClick:= {$ifdef FPC}@{$endif}ClickDress;
    GroupDressingButtons.InsertFront(newBtn);
  end;
end;

procedure TViewPlayGirl.SetActionsList(actList: TCastleComponent);
var
  num, i: Integer;
  actionDescr: TCastleComponent;
  newBtn: TCastleButton;
begin
  num:= actList.NonVisualComponentsCount;
  if (num < 1) then Exit;

  GroupActionSelect.ClearControls;

  for i:= 0 to (num - 1) do
  begin
    actionDescr:= actList.NonVisualComponents[i] as TCastleComponent;
    newBtn:= TCastleButton.Create(GroupActionSelect);
    newBtn.Caption:= actionDescr.Name;
    newBtn.Tag:= actionDescr.Tag;
    newBtn.OnClick:= {$ifdef FPC}@{$endif}ClickAction;
    GroupActionSelect.InsertFront(newBtn);
  end;
end;

procedure TViewPlayGirl.ChangedEmission(Sender: TObject);
var
  chara: TActorChara;
begin
  for chara in FActorsLogic.Charas do
    chara.SelfEmission:= FloatSliderEmission.Value;
end;

procedure TViewPlayGirl.ChangedSpeed(Sender: TObject);
var
  slider: TCastleFloatSlider;
begin
  slider:= Sender as TCastleFloatSlider;
  if NOT Assigned(slider) then Exit;

  FActorsLogic.SetSpeed(slider.Value);
end;

procedure TViewPlayGirl.SetUIColor(newColor: TCastleColorRGB);
var
  rectanpleUI: TCastleRectangleControl;
  alpha: Single;
begin
  rectanpleUI:= DesignedComponent('RectangleControlActions') as TCastleRectangleControl;
  alpha:= rectanpleUI.Color.W;
  rectanpleUI.Color:= Vector4(FActorsLogic.CharasColor, alpha);

  rectanpleUI:= DesignedComponent('RectangleControlNavigation') as TCastleRectangleControl;
  alpha:= rectanpleUI.Color.W;
  rectanpleUI.Color:= Vector4(FActorsLogic.CharasColor, alpha);

  rectanpleUI:= DesignedComponent('RectangleControlDressing') as TCastleRectangleControl;
  alpha:= rectanpleUI.Color.W;
  rectanpleUI.Color:= Vector4(FActorsLogic.CharasColor, alpha);
end;

procedure TViewPlayGirl.ScreenShot;
begin
  FScreenFader.Fade(1.5);
end;

function TViewPlayGirl.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(keyP) then
  begin
    ScreenShot;
    Exit(true);
  end;
end;

procedure TViewPlayGirl.DoStart(Sender: TObject);
begin
  FActorsLogic.Stop;
end;

end.
