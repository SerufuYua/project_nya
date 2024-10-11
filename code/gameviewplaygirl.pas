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
    FGirlBehavior: TCharaGirlBehavior;
    FToysBehavior: TToysForGirlBehavior;
    FDressMenu: boolean;
    procedure ClickDress(Sender: TObject);
    procedure ClickControl(Sender: TObject);
    procedure ChangedSpeed(Sender: TObject);
  end;

var
  ViewPlayGirl: TViewPlayGirl;

implementation

uses
  GameViewMain, CastleScene, CharaBehavior, GameViewDressingMenu;

constructor TViewPlayGirl.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl:= 'castle-data:/gameviewplaygirl.castle-user-interface';
  FDressMenu:= False;
end;

procedure TViewPlayGirl.Start;
var
  girlScene, toysScene: TCastleTransformDesign;
begin
  inherited;
  { Executed once when view starts }

  BtnDress.OnClick:= {$ifdef FPC}@{$endif}ClickDress;
  BtnBack.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  BtnStop.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  BtnPause.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  BtnPlayA1.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  BtnPlayA2.OnClick:= {$ifdef FPC}@{$endif}ClickControl;
  FloatSliderSpeed.OnChange:=  {$ifdef FPC}@{$endif}ChangedSpeed;

  { Create Girl Character instance }
  girlScene:= DesignedComponent('CharaGirl') as TCastleTransformDesign;
  FGirlBehavior:= TCharaGirlBehavior.Create(FreeAtStop);
  girlScene.AddBehavior(FGirlBehavior);

  { Create Toys instance }
  toysScene:= DesignedComponent('Toys') as TCastleTransformDesign;
  FToysBehavior:= TToysForGirlBehavior.Create(FreeAtStop);
  toysScene.AddBehavior(FToysBehavior);

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
end;

procedure TViewPlayGirl.ClickDress(Sender: TObject);
begin
  { Show Dressing Menu }
  if FDressMenu then
  begin
    Container.PopView;
    FDressMenu:= False;
  end else
  begin
    Container.PushView(ViewDressingMenu);
    ViewDressingMenu.SetChara(FGirlBehavior);
    FDressMenu:= True;
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
      Container.View:= ViewMain;
    end;
  'BtnStop':
    begin
      FGirlBehavior.ActionPlayToyA_Idle;
      FToysBehavior.ActionPlayToyA_Idle;
    end;
  'BtnPause':
    begin
      FGirlBehavior.ActionPause;
      FToysBehavior.ActionPause;
    end;
  'BtnPlayA1':
    begin
      FGirlBehavior.ActionPlayToyA_A1P1;
      FToysBehavior.ActionPlayToyA_A1P1;
    end;
  'BtnPlayA2':
    begin
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

end.
