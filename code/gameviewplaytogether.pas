unit GameViewPlayTogether;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  CastleTransform,
  CharaGirlBehavior, CharaBoyBehavior;

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
    FGirlBehavior: TCharaGirlBehavior;
    FBoyBehavior: TCharaBoyBehavior;
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
  GameViewMain, CharaBehavior, GameViewDressingMenu, GameViewLoading;

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
  FGirlBehavior:= TCharaGirlBehavior.Create(FreeAtStop);
  GirlScene.AddBehavior(FGirlBehavior);

  { Create Boy Character instance }
  BoyScene:= DesignedComponent('CharaBoy') as TCastleTransformDesign;
  FBoyBehavior:= TCharaBoyBehavior.Create(FreeAtStop);
  BoyScene.AddBehavior(FBoyBehavior);

  { set character self emission }
  FGirlBehavior.SelfEmission:= 0.15;
  FBoyBehavior.SelfEmission:= 0.15;
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
      FGirlBehavior.SaveCondition;
      FBoyBehavior.SaveCondition;
      ViewLoading.SetToLoad(ViewMain);
      Container.View:= ViewLoading;
    end;
  'BtnStop':
    begin
      SceneActors.Translation:= Vector3(45, 0, 0);
      FGirlBehavior.ActionPlayTogether_Idle;
      FBoyBehavior.ActionPlayTogether_Idle;
    end;
  'BtnPause':
    begin
      FGirlBehavior.ActionPause;
      FBoyBehavior.ActionPause;
    end;
  'BtnPlayA1P1':
    begin
      SceneActors.Translation:= Vector3(45, 12, -57);
      FGirlBehavior.ActionPlayTogether_A1P1;
      FBoyBehavior.ActionPlayTogether_A1P1;
    end;
  'BtnPlayA1P2':
    begin
      SceneActors.Translation:= Vector3(45, 12, -57);
      FGirlBehavior.ActionPlayTogether_A1P2;
      FBoyBehavior.ActionPlayTogether_A1P2;
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
    'BtnDressGirl': ViewDressingMenu.SetChara(FGirlBehavior);
    'BtnDressBoy': ViewDressingMenu.SetChara(FBoyBehavior);
    end;

    DressingControl.Exists:= False;
  end;
end;

procedure TViewPlayTogether.ClicCharaLight(Sender: TObject);
begin
  FGirlBehavior.Lightning:= NOT FGirlBehavior.Lightning;
  FBoyBehavior.Lightning:= NOT FBoyBehavior.Lightning;
end;

procedure TViewPlayTogether.ChangedEmission(Sender: TObject);
var
  slider: TCastleFloatSlider;
begin
  slider:= Sender as TCastleFloatSlider;
  if NOT Assigned(slider) then Exit;

  FGirlBehavior.SelfEmission:= slider.Value;
  FBoyBehavior.SelfEmission:= slider.Value;
end;

procedure TViewPlayTogether.ChangedSpeed(Sender: TObject);
var
  slider: TCastleFloatSlider;
begin
  slider:= Sender as TCastleFloatSlider;
  if NOT Assigned(slider) then Exit;

  FGirlBehavior.Speed:= slider.Value;
  FBoyBehavior.Speed:= slider.Value;
end;

end.
