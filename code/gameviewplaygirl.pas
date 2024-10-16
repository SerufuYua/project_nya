unit GameViewPlayGirl;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  CastleTransform,
  ActorGirl, ActorToyA, FadeInOut;

type
  TViewPlayGirl = class(TCastleView)
  published
    LabelFps: TCastleLabel;
    BtnDress: TCastleButton;
    BtnBack: TCastleButton;
    BtnStop: TCastleButton;
    BtnPause: TCastleButton;
    BtnPlayA1: TCastleButton;
    BtnPlayA2: TCastleButton;
    FloatSliderSpeed: TCastleFloatSlider;
    DressingControl: TCastleRectangleControl;
    ImageScreen: TCastleImageControl;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single;
                     var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  private
    FGirlBehavior: TActorGirl;
    FToysBehavior: TActorToyA;
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
  GameViewMain, CastleScene, ActorChara, GameViewDressingMenu,
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
  BtnPause.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  BtnPlayA1.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  BtnPlayA2.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  FloatSliderSpeed.OnChange:=  {$ifdef FPC}@{$endif}ChangedSpeed;

  { set fade animator }
  FScreenFader:= TImageFader.Create(ImageScreen);

  { Create Girl Character instance }
  girlScene:= DesignedComponent('CharaGirl') as TCastleTransformDesign;
  FGirlBehavior:= TActorGirl.Create(girlScene);

  { Create Toys instance }
  toysScene:= DesignedComponent('Toys') as TCastleTransformDesign;
  FToysBehavior:= TActorToyA.Create(toysScene);

  { set character self emission }
  FGirlBehavior.SelfEmission:= 0.15;

  { default chara action }
  FGirlBehavior.ActionPlayToyA_Idle;
  FToysBehavior.ActionPlayToyA_Idle;
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
end;

procedure TViewPlayGirl.ClickDress(Sender: TObject);
begin
  { Show Dressing Menu }
  if NOT (Container.FrontView = ViewDressingMenu) then
  begin
    Container.PushView(ViewDressingMenu);
    ViewDressingMenu.SetChara(FGirlBehavior);
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
      FGirlBehavior.SaveCondition;
      ViewLoading.SetToLoad(ViewMain);
      Container.View:= ViewLoading;
    end;
  'BtnStop':
    begin
      FScreenFader.Fade(Container.SaveScreen, 0.25);
      FGirlBehavior.ActionPlayToyA_Idle;
      FToysBehavior.ActionPlayToyA_Idle;
    end;
  'BtnPause':
    begin
      FGirlBehavior.PauseAnimation;
      FToysBehavior.PauseAnimation;
    end;
  'BtnPlayA1':
    begin
      FScreenFader.Fade(Container.SaveScreen, 0.25);
      FGirlBehavior.ActionPlayToyA_A1P1;
      FToysBehavior.ActionPlayToyA_A1P1;
    end;
  'BtnPlayA2':
    begin
      FScreenFader.Fade(Container.SaveScreen, 0.25);
      FGirlBehavior.ActionPlayToyA_A2P1;
      FToysBehavior.ActionPlayToyA_A2P1;
    end;
  end;

end;

procedure TViewPlayGirl.ChangedSpeed(Sender: TObject);
var
  slider: TCastleFloatSlider;
begin
  slider:= Sender as TCastleFloatSlider;
  if NOT Assigned(slider) then Exit;

  FGirlBehavior.Speed:= slider.Value;
  FToysBehavior.Speed:= slider.Value;
end;

procedure TViewPlayGirl.ScreenShot;
begin
  FScreenFader.Fade(Container.SaveScreen, 0.5);
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
