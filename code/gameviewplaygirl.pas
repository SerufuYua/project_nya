unit GameViewPlayGirl;

interface

uses Classes,
  CastleUIControls, CastleControls, CastleKeysMouse,
  CastleTransform, CastleNotifications,
  ActorChara, ActorToyA, ActorsLogic, FadeInOut,
  CastleParticleEmitter;

type
  TViewPlayGirl = class(TCastleView)
  published
    LabelFps: TCastleLabel;
    //BtnDress: TCastleButton;
    BtnBack: TCastleButton;
    BtnStop: TCastleButton;
    BtnNext: TCastleButton;
    BtnPlayA1: TCastleButton;
    BtnPlayA2: TCastleButton;
    FloatSliderSpeed: TCastleFloatSlider;
    FloatSliderPleasure: TCastleFloatSlider;
    FloatSliderTension: TCastleFloatSlider;
    DressingControl: TCastleRectangleControl;
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
    procedure ClickDress(Sender: TObject);
    procedure ClickControl(Sender: TObject);
    procedure ChangedSpeed(Sender: TObject);
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
  controlActions: TCastleRectangleControl;
begin
  inherited;
  {
  //BtnDress.OnClick:= {$ifdef FPC}@{$endif}ClickDress;
  BtnBack.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  BtnStop.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  BtnNext.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  BtnPlayA1.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  BtnPlayA2.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  FloatSliderSpeed.OnChange:=  {$ifdef FPC}@{$endif}ChangedSpeed;     }

  { set fade animator }
  FScreenFader:= TImageFader.Create(ImageScreen, Container);

  { set map }
  mapScene:= DesignedComponent('Map') as TCastleTransformDesign;
  mapScene.Url:= 'castle-data:/MapPlayGirlToyA.castle-transform';

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


  { Create Girl Character instance }
  girlScene:= mapScene.DesignedComponent('CharaGirl') as TCastleTransformDesign;
  FActorGirl:= TActorChara.Create(girlScene, 'Girl');

  { Create Toys instance }
  toysScene:= mapScene.DesignedComponent('ToyA') as TCastleTransformDesign;
  FActorToyA:= TActorToyA.Create(toysScene, 'ToyA');

  { set character self emission }
  FActorGirl.SelfEmission:= 0.15;

  { Create Actors Logic }
  FActorsLogic:= TActorsLogic.Create(FActorGirl, FActorToyA,
                                     'GAME.GIRL_TOYA.PLAY',
                                     FScreenFader);

  { set initial action }
  WaitForRenderAndCall({$ifdef FPC}@{$endif}DoStart);

  { set color }
  controlActions:= DesignedComponent('RectangleControlActions') as TCastleRectangleControl;
  controlActions.Color:= Vector4(FActorsLogic.CharasColor, 0.5);
end;

procedure TViewPlayGirl.Update(const SecondsPassed: Single; var HandleInput: boolean);
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

  {
  { Release Dressing Menu Button }
  if NOT (Container.FrontView = ViewDressingMenu) then
    DressingControl.Exists:= True;
  }
end;

procedure TViewPlayGirl.ClickDress(Sender: TObject);
begin
  { Show Dressing Menu }
  if NOT (Container.FrontView = ViewDressingMenu) then
  begin
    Container.PushView(ViewDressingMenu);
    ViewDressingMenu.SetChara(FActorGirl);
    DressingControl.Exists:= False;
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
  'BtnPlayA1':
    begin
      FActorsLogic.Stop;
      FActorsLogic.SetAction(1);
    end;
  'BtnPlayA2':
    begin
      FActorsLogic.Stop;
      FActorsLogic.SetAction(2);
    end;
  end;

end;

procedure TViewPlayGirl.ChangedSpeed(Sender: TObject);
var
  slider: TCastleFloatSlider;
begin
  slider:= Sender as TCastleFloatSlider;
  if NOT Assigned(slider) then Exit;

  FActorsLogic.SetSpeed(slider.Value);
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
