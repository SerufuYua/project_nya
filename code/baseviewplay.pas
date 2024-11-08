unit BaseViewPlay;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CastleUIControls, CastleControls, CastleNotifications, CastleClassUtils,
  CastleColors, CastleKeysMouse, CastleTransform,
  FadeInOut, ActorsLogic, BaseActor;

type
  TBaseViewPlay = class(TCastleView)
  published
    Map: TCastleTransformDesign;
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
  protected
    FActorA: TBaseActor;
    FActorB: TBaseActor;
    FAnimationPrefix: String;
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
    procedure SaveCharasCondition();
    procedure DoStart(Sender: TObject);
  end;

implementation

uses
  GameViewMain, GameViewDressingMenu, GameViewLoading,
  ActorChara,
  CastleScene, CastleViewport, CastleVectors,
  StrUtils, MyCastleUtils;

constructor TBaseViewPlay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DesignUrl:= 'castle-data:/gameviewplay.castle-user-interface';
end;

procedure TBaseViewPlay.Start;
var
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

  { Create Actors Logic }
  FActorsLogic:= TActorsLogic.Create(FActorA, FActorB,
                                     FAnimationPrefix,
                                     FScreenFader);
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

  { set characters self emission }
  ChangedEmission(FloatSliderEmission);

  { set dress buttons }
  SetDressButtons();

  { set actions list }
  SetActionsList(Map.DesignedComponent('ActionsList') as TCastleComponent);

  { set color }
  SetUIColor(FActorsLogic.CharasColor);

  { set initial action }
  WaitForRenderAndCall({$ifdef FPC}@{$endif}DoStart);
end;

procedure TBaseViewPlay.Update(const SecondsPassed: Single;
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

function TBaseViewPlay.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

{  if Event.IsKey(keyP) then
  begin
    ScreenShot;
    Exit(true);
  end; }
end;

procedure TBaseViewPlay.ClickAction(Sender: TObject);
var
  btnDress: TCastleButton;
begin
  btnDress:= Sender as TCastleButton;
  if NOT Assigned(btnDress) then Exit;

  FActorsLogic.Stop;
  FActorsLogic.SetAction(btnDress.Tag);
end;

procedure TBaseViewPlay.ClickDress(Sender: TObject);
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

procedure TBaseViewPlay.ClickControl(Sender: TObject);
var
  button: TCastleButton;
begin
  button:= Sender as TCastleButton;
  if NOT Assigned(button) then exit;

  Case button.Name of
  'BtnBack':
    begin
      SaveCharasCondition();
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

procedure TBaseViewPlay.SetDressButtons();
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

procedure TBaseViewPlay.SetActionsList(actList: TCastleComponent);
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
    newBtn.Caption:= ReplaceStr(actionDescr.Name, '_', ' ');
    newBtn.Tag:= actionDescr.Tag;
    newBtn.OnClick:= {$ifdef FPC}@{$endif}ClickAction;
    GroupActionSelect.InsertFront(newBtn);
  end;
end;

procedure TBaseViewPlay.ChangedEmission(Sender: TObject);
var
  chara: TActorChara;
begin
  for chara in FActorsLogic.Charas do
    chara.SelfEmission:= FloatSliderEmission.Value;
end;

procedure TBaseViewPlay.ChangedSpeed(Sender: TObject);
var
  slider: TCastleFloatSlider;
begin
  slider:= Sender as TCastleFloatSlider;
  if NOT Assigned(slider) then Exit;

  FActorsLogic.SetSpeed(slider.Value);
end;

procedure TBaseViewPlay.SetUIColor(newColor: TCastleColorRGB);
var
  rootItem: TCastleUserInterface;
  item: TCastleImageControl;
  alpha: single;
begin
  rootItem:= DesignedComponent('SceneMain') as TCastleUserInterface;

  for item in GetAllUIImages(rootItem) do
  begin
      alpha:= item.Color.W;
      item.Color:= Vector4(FActorsLogic.CharasColor, alpha);
  end;
end;

procedure TBaseViewPlay.SaveCharasCondition();
var
  chara: TActorChara;
begin
  for chara in FActorsLogic.Charas do
    chara.SaveCondition;
end;

procedure TBaseViewPlay.DoStart(Sender: TObject);
begin
  FActorsLogic.Stop;
end;

end.

