unit GameViewPlayTogether;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  CastleTransform,
  CharaGirlBehavior, CharaBoyBehavior;

type
  TViewPlayTogether = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    // ButtonXxx: TCastleButton;
    LabelFps: TCastleLabel;
    BtnBack: TCastleButton;
    BtnDressGirl: TCastleButton;
    BtnDressBoy: TCastleButton;
    BtnDressNone: TCastleButton;
    BtnPause: TCastleButton;
    BtnStop: TCastleButton;
    BtnPlayA1P1: TCastleButton;
    BtnPlayA1P2: TCastleButton;
    BtnCharaLight: TCastleButton;
    FloatSliderEmission: TCastleFloatSlider;
    FloatSliderSpeed: TCastleFloatSlider;
    Charas: TCastleTransform; { Charas Root }
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
  private
    GirlBehavior: TCharaGirlBehavior;
    BoyBehavior: TCharaBoyBehavior;
    DressMenu: boolean;
    procedure ClickBack(Sender: TObject);
    procedure ClickPlay(Sender: TObject);
    procedure ClicCharaLight(Sender: TObject);
    procedure ChangedEmission(Sender: TObject);
    procedure ChangedSpeed(Sender: TObject);
    procedure ClickDress(Sender: TObject);
  end;

var
  ViewPlayTogether: TViewPlayTogether;

implementation

uses GameViewMain, CharaBehavior, GameViewDressingMenu;

constructor TViewPlayTogether.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewplaytogether.castle-user-interface';
  DressMenu:= False;
end;

procedure TViewPlayTogether.Start;
var
  GirlScene: TCastleTransformDesign;
  BoyScene: TCastleTransformDesign;
begin
  inherited;
  { Executed once when view starts. }

  BtnBack.OnClick:= {$ifdef FPC}@{$endif}ClickBack;
  BtnDressGirl.OnClick:= {$ifdef FPC}@{$endif}ClickDress;
  BtnDressBoy.OnClick:= {$ifdef FPC}@{$endif}ClickDress;
  BtnDressNone.OnClick:= {$ifdef FPC}@{$endif}ClickDress;
  BtnPause.OnClick:= {$ifdef FPC}@{$endif}ClickPlay;
  BtnStop.OnClick:= {$ifdef FPC}@{$endif}ClickPlay;
  BtnPlayA1P1.OnClick:= {$ifdef FPC}@{$endif}ClickPlay;
  BtnPlayA1P2.OnClick:= {$ifdef FPC}@{$endif}ClickPlay;
  BtnCharaLight.OnClick:= {$ifdef FPC}@{$endif}ClicCharaLight;
  FloatSliderEmission.OnChange:=  {$ifdef FPC}@{$endif}ChangedEmission;
  FloatSliderSpeed.OnChange:=  {$ifdef FPC}@{$endif}ChangedSpeed;

  { Create Girl Character instance }
  GirlScene:= DesignedComponent('CharaGirl') as TCastleTransformDesign;
  GirlBehavior:= TCharaGirlBehavior.Create(FreeAtStop);
  GirlScene.AddBehavior(GirlBehavior);

  { Create Boy Character instance }
  BoyScene:= DesignedComponent('CharaBoy') as TCastleTransformDesign;
  BoyBehavior:= TCharaBoyBehavior.Create(FreeAtStop);
  BoyScene.AddBehavior(BoyBehavior);
end;

procedure TViewPlayTogether.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  { Executed every frame. }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption:= 'FPS: ' + Container.Fps.ToString;
end;

procedure TViewPlayTogether.ClickBack(Sender: TObject);
begin
  Container.View:= ViewMain;
end;

procedure TViewPlayTogether.ClickPlay(Sender: TObject);
var
  button: TCastleButton;
begin
  button:= Sender as TCastleButton;
  if NOT Assigned(button) then exit;

  Case button.Name of
  'BtnStop':
    begin
      Charas.Translation:= Vector3(45, 0, 0);
      GirlBehavior.ActionIdleTogether;
      BoyBehavior.ActionIdleTogether;
    end;
  'BtnPause':
    begin
      GirlBehavior.ActionPause;
      BoyBehavior.ActionPause;
    end;
  'BtnPlayA1P1':
    begin
      Charas.Translation:= Vector3(45, 12, -57);
      GirlBehavior.ActionPlayTogetherA1P1;
      BoyBehavior.ActionPlayTogetherA1P1;
    end;
  'BtnPlayA1P2':
    begin
      Charas.Translation:= Vector3(45, 12, -57);
      GirlBehavior.ActionPlayTogetherA1P2;
      BoyBehavior.ActionPlayTogetherA1P2;
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
  if (DressMenu AND (button.Name = 'BtnDressNone')) then
  begin
    Container.PopView;
    DressMenu:= False;
  end else if (NOT DressMenu AND (button.Name <> 'BtnDressNone')) then
  begin
    Container.PushView(ViewDressingMenu);
    DressMenu:= True;
  end;

  { Updte Dressing Menu if it showed}
  if DressMenu then
  begin
    Case button.Name of
    'BtnDressGirl': ViewDressingMenu.SetChara(GirlBehavior);
    'BtnDressBoy': ViewDressingMenu.SetChara(BoyBehavior);
    end;
  end;
end;

procedure TViewPlayTogether.ClicCharaLight(Sender: TObject);
begin
  GirlBehavior.Lightning:= NOT GirlBehavior.Lightning;
  BoyBehavior.Lightning:= NOT BoyBehavior.Lightning;
end;

procedure TViewPlayTogether.ChangedEmission(Sender: TObject);
var
  slider: TCastleFloatSlider;
begin
  slider:= Sender as TCastleFloatSlider;
  if NOT Assigned(slider) then Exit;

  GirlBehavior.SelfEmission:= slider.Value;
  BoyBehavior.SelfEmission:= slider.Value;
end;

procedure TViewPlayTogether.ChangedSpeed(Sender: TObject);
var
  slider: TCastleFloatSlider;
begin
  slider:= Sender as TCastleFloatSlider;
  if NOT Assigned(slider) then Exit;

  GirlBehavior.Speed:= slider.Value;
  BoyBehavior.Speed:= slider.Value;
end;

end.
