unit GameViewPlayGirl;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  CastleTransform,
  CharaGirlBehavior, ToysForGirlBehavior;

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
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single;
                     var HandleInput: boolean); override;
  private
    GirlBehavior: TCharaGirlBehavior;
    ToysBehavior: TToysForGirlBehavior;
    DressMenu: boolean;
    procedure ClickBack(Sender: TObject);
    procedure ClickDress(Sender: TObject);
    procedure ClickControl(Sender: TObject);
    procedure ChangedSpeed(Sender: TObject);
  end;

var
  ViewPlayGirl: TViewPlayGirl;

implementation

uses GameViewMain, CastleScene, CharaBehavior, GameViewDressingMenu;

constructor TViewPlayGirl.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl:= 'castle-data:/gameviewplaygirl.castle-user-interface';
  DressMenu:= False;
end;

procedure TViewPlayGirl.Start;
var
  girlScene, toysScene: TCastleTransformDesign;
begin
  inherited;
  { Executed once when view starts }

  BtnBack.OnClick:= {$ifdef FPC}@{$endif}ClickBack;
  BtnDress.OnClick:= {$ifdef FPC}@{$endif}ClickDress;
  BtnStop.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  BtnPause.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  BtnPlayA1.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  BtnPlayA2.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  FloatSliderSpeed.OnChange:=  {$ifdef FPC}@{$endif}ChangedSpeed;

  { Create Girl Character instance }
  girlScene:= DesignedComponent('CharaGirl') as TCastleTransformDesign;
  GirlBehavior:= TCharaGirlBehavior.Create(FreeAtStop);
  girlScene.AddBehavior(GirlBehavior);

  { Create Toys instance }
  toysScene:= DesignedComponent('Toys') as TCastleTransformDesign;
  ToysBehavior:= TToysForGirlBehavior.Create(FreeAtStop);
  toysScene.AddBehavior(ToysBehavior);

  { set character self emission }
  GirlBehavior.SelfEmission:= 0.15;

  { default chara action }
  GirlBehavior.ActionPlayToyA_Idle;
  ToysBehavior.ActionPlayToyA_Idle;
end;

procedure TViewPlayGirl.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  { Executed every frame. }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption:= 'FPS: ' + Container.Fps.ToString;
end;

procedure TViewPlayGirl.ClickBack(Sender: TObject);
begin
  Container.View:= ViewMain;
end;

procedure TViewPlayGirl.ClickDress(Sender: TObject);
begin
  { Show Dressing Menu }
  if DressMenu then
  begin
    Container.PopView;
    DressMenu:= False;
  end else
  begin
    Container.PushView(ViewDressingMenu);
    ViewDressingMenu.SetChara(GirlBehavior);
    DressMenu:= True;
  end;
end;

procedure TViewPlayGirl.ClickControl(Sender: TObject);
var
  button: TCastleButton;
begin
  button:= Sender as TCastleButton;
  if NOT Assigned(button) then exit;

  Case button.Name of
  'BtnStop':
    begin
      GirlBehavior.ActionPlayToyA_Idle;
      ToysBehavior.ActionPlayToyA_Idle;
    end;
  'BtnPause':
    begin
      GirlBehavior.ActionPause;
      ToysBehavior.ActionPause;
    end;
  'BtnPlayA1':
    begin
      GirlBehavior.ActionPlayToyA_A1P1;
      ToysBehavior.ActionPlayToyA_A1P1;
    end;
  'BtnPlayA2':
    begin
      GirlBehavior.ActionPlayToyA_A2P1;
      ToysBehavior.ActionPlayToyA_A2P1;
    end;
  end;

end;

procedure TViewPlayGirl.ChangedSpeed(Sender: TObject);
var
  slider: TCastleFloatSlider;
begin
  slider:= Sender as TCastleFloatSlider;
  if NOT Assigned(slider) then Exit;

  GirlBehavior.Speed:= slider.Value;
  ToysBehavior.Speed:= slider.Value;
end;

end.
