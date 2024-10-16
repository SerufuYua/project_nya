unit GameViewPlayTogether;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  CastleTransform,
  ActorGirl, ActorBoy;

type
  TViewPlayTogether = class(TCastleView)
  published
    LabelFps: TCastleLabel;
    BtnBack: TCastleButton;
    BtnDressGirl: TCastleButton;
    BtnDressBoy: TCastleButton;
    BtnPause: TCastleButton;
    BtnStop: TCastleButton;
    BtnPlayA1P1: TCastleButton;
    BtnPlayA1P2: TCastleButton;
    BtnCharaLight: TCastleButton;
    FloatSliderEmission: TCastleFloatSlider;
    FloatSliderSpeed: TCastleFloatSlider;
    SceneActors: TCastleTransform; { Charas Root }
    DressingControl: TCastleRectangleControl;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
  private
    FActorGirl: TActorGirl;
    FActorBoy: TActorBoy;
    procedure ClickControl(Sender: TObject);
    procedure ClicCharaLight(Sender: TObject);
    procedure ChangedEmission(Sender: TObject);
    procedure ChangedSpeed(Sender: TObject);
    procedure ClickDress(Sender: TObject);
  end;

var
  ViewPlayTogether: TViewPlayTogether;

implementation

uses
  GameViewMain, ActorChara, GameViewDressingMenu, GameViewLoading;

constructor TViewPlayTogether.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewplaytogether.castle-user-interface';
end;

procedure TViewPlayTogether.Start;
var
  GirlScene: TCastleTransformDesign;
  BoyScene: TCastleTransformDesign;
begin
  inherited;
  { Executed once when view starts. }

  BtnDressGirl.OnClick:= {$ifdef FPC}@{$endif}ClickDress;
  BtnDressBoy.OnClick:= {$ifdef FPC}@{$endif}ClickDress;
  BtnBack.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  BtnPause.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  BtnStop.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  BtnPlayA1P1.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  BtnPlayA1P2.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  BtnCharaLight.OnClick:= {$ifdef FPC}@{$endif}ClicCharaLight;
  FloatSliderEmission.OnChange:=  {$ifdef FPC}@{$endif}ChangedEmission;
  FloatSliderSpeed.OnChange:=  {$ifdef FPC}@{$endif}ChangedSpeed;

  { Create Girl Character instance }
  GirlScene:= DesignedComponent('CharaGirl') as TCastleTransformDesign;
  FActorGirl:= TActorGirl.Create(GirlScene);

  { Create Boy Character instance }
  BoyScene:= DesignedComponent('CharaBoy') as TCastleTransformDesign;
  FActorBoy:= TActorBoy.Create(BoyScene);

  { set character self emission }
  FActorGirl.SelfEmission:= 0.15;
  FActorBoy.SelfEmission:= 0.15;
end;

procedure TViewPlayTogether.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  { Executed every frame. }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption:= 'FPS: ' + Container.Fps.ToString;

  { Release Dressing Menu Button }
  if NOT (Container.FrontView = ViewDressingMenu) then
    DressingControl.Exists:= True;
end;

procedure TViewPlayTogether.ClickControl(Sender: TObject);
var
  button: TCastleButton;
begin
  button:= Sender as TCastleButton;
  if NOT Assigned(button) then exit;

  Case button.Name of
  'BtnBack':
    begin
      FActorGirl.SaveCondition;
      FActorBoy.SaveCondition;
      ViewLoading.SetToLoad(ViewMain);
      Container.View:= ViewLoading;
    end;
  'BtnStop':
    begin
      SceneActors.Translation:= Vector3(45, 0, 0);
      FActorGirl.ActionPlayTogether_Idle;
      FActorBoy.ActionPlayTogether_Idle;
    end;
  'BtnPause':
    begin
      FActorGirl.PauseAnimation;
      FActorBoy.PauseAnimation;
    end;
  'BtnPlayA1P1':
    begin
      SceneActors.Translation:= Vector3(45, 12, -57);
      FActorGirl.ActionPlayTogether_A1P1;
      FActorBoy.ActionPlayTogether_A1P1;
    end;
  'BtnPlayA1P2':
    begin
      SceneActors.Translation:= Vector3(45, 12, -57);
      FActorGirl.ActionPlayTogether_A1P2;
      FActorBoy.ActionPlayTogether_A1P2;
    end;
  end;
end;

procedure TViewPlayTogether.ClickDress(Sender: TObject);
var
  button: TCastleButton;
begin
  button:= Sender as TCastleButton;
  if NOT Assigned(button) then exit;

  { Show Dressing Menu }
  if NOT (Container.FrontView = ViewDressingMenu) then
  begin
    Container.PushView(ViewDressingMenu);

    Case button.Name of
    'BtnDressGirl': ViewDressingMenu.SetChara(FActorGirl);
    'BtnDressBoy': ViewDressingMenu.SetChara(FActorBoy);
    end;

    DressingControl.Exists:= False;
  end;
end;

procedure TViewPlayTogether.ClicCharaLight(Sender: TObject);
begin
  FActorGirl.Lightning:= NOT FActorGirl.Lightning;
  FActorBoy.Lightning:= NOT FActorBoy.Lightning;
end;

procedure TViewPlayTogether.ChangedEmission(Sender: TObject);
var
  slider: TCastleFloatSlider;
begin
  slider:= Sender as TCastleFloatSlider;
  if NOT Assigned(slider) then Exit;

  FActorGirl.SelfEmission:= slider.Value;
  FActorBoy.SelfEmission:= slider.Value;
end;

procedure TViewPlayTogether.ChangedSpeed(Sender: TObject);
var
  slider: TCastleFloatSlider;
begin
  slider:= Sender as TCastleFloatSlider;
  if NOT Assigned(slider) then Exit;

  FActorGirl.Speed:= slider.Value;
  FActorBoy.Speed:= slider.Value;
end;

end.
