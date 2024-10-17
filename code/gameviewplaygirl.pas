unit GameViewPlayGirl;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  CastleTransform,
  ActorChara, ActorToyA, ActorsLogic, FadeInOut;

type
  TViewPlayGirl = class(TCastleView)
  published
    LabelFps: TCastleLabel;
    BtnDress: TCastleButton;
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
  end;

var
  ViewPlayGirl: TViewPlayGirl;

implementation

uses
  GameViewMain, CastleScene, GameViewDressingMenu,
  GameViewLoading;

constructor TViewPlayGirl.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl:= 'castle-data:/gameviewplaygirl.castle-user-interface';
end;

procedure TViewPlayGirl.Start;
var
  girlScene, toysScene: TCastleTransformDesign;
begin
  inherited;

  BtnDress.OnClick:= {$ifdef FPC}@{$endif}ClickDress;
  BtnBack.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  BtnStop.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  BtnNext.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  BtnPlayA1.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  BtnPlayA2.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  FloatSliderSpeed.OnChange:=  {$ifdef FPC}@{$endif}ChangedSpeed;

  { set fade animator }
  FScreenFader:= TImageFader.Create(ImageScreen, Container);

  { Create Girl Character instance }
  girlScene:= DesignedComponent('CharaGirl') as TCastleTransformDesign;
  FActorGirl:= TActorChara.Create(girlScene, 'Girl');

  { Create Toys instance }
  toysScene:= DesignedComponent('Toys') as TCastleTransformDesign;
  FActorToyA:= TActorToyA.Create(toysScene);

  { Create Actors Logic }
  FActorsLogic:= TActorsLogic.Create(FActorGirl, FActorToyA,
                                     'GAME.GIRL_TOYA.PLAY',
                                     FScreenFader);

  { set character self emission }
  FActorGirl.SelfEmission:= 0.15;
end;

procedure TViewPlayGirl.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  { Executed every frame. }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption:= 'FPS: ' + Container.Fps.ToString;

  { Release Dressing Menu Button }
  if NOT (Container.FrontView = ViewDressingMenu) then
    DressingControl.Exists:= True;

  FScreenFader.AnimateQuadFade(SecondsPassed);

  { upade gauges }
  FActorsLogic.Update(SecondsPassed);
  FloatSliderPleasure.Value:= FActorsLogic.Pleasure;
  FloatSliderTension.Value:= FActorsLogic.Tension;

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
      FActorsLogic.SetAction(1);
    end;
  'BtnPlayA2':
    begin
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

end.
