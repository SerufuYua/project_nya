unit GameViewSettingsMenu;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse;

type
  TViewSettingsMenu = class(TCastleView)
  published
    BtnClose: TCastleButton;
    FullScreenCheck: TCastleCheckbox;
    SfxSlider, MusicSlider: TCastleFloatSlider;
    BtnSfxMinus, BtnSfxPlus, BtnMusicMinus, BtnMusicPlus: TCastleButton;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
  private
    procedure ClickClose(Sender: TObject);
    procedure ClickScreen(Sender: TObject);
    procedure ClickPlusMinus(Sender: TObject);
    procedure ChangeSoundSlider(Sender: TObject);
  private
    procedure StepChangeSlider(slider: TCastleFloatSlider; step: Single);
  end;

var
  ViewSettingsMenu: TViewSettingsMenu;

implementation

uses
  CastleWindow, CastleSoundEngine;

var
  Window: TCastleWindow;

constructor TViewSettingsMenu.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewsettingsmenu.castle-user-interface';
end;

procedure TViewSettingsMenu.Start;
begin
  inherited;
  InterceptInput:= True;
  Window:= Application.MainWindow;

  BtnClose.OnClick:= {$ifdef FPC}@{$endif}ClickClose;
  FullScreenCheck.OnChange:= {$ifdef FPC}@{$endif}ClickScreen;
  SfxSlider.OnChange:= {$ifdef FPC}@{$endif}ChangeSoundSlider;
  MusicSlider.OnChange:= {$ifdef FPC}@{$endif}ChangeSoundSlider;
  BtnSfxMinus.OnClick:= {$ifdef FPC}@{$endif}ClickPlusMinus;
  BtnSfxPlus.OnClick:= {$ifdef FPC}@{$endif}ClickPlusMinus;
  BtnMusicMinus.OnClick:= {$ifdef FPC}@{$endif}ClickPlusMinus;
  BtnMusicPlus.OnClick:= {$ifdef FPC}@{$endif}ClickPlusMinus;
end;

procedure TViewSettingsMenu.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  { Executed every frame. }
end;

procedure TViewSettingsMenu.ClickClose(Sender: TObject);
begin
  Container.PopView(self);
end;

procedure TViewSettingsMenu.ClickScreen(Sender: TObject);
var
  check: TCastleCheckbox;
begin
  check:= Sender as TCastleCheckbox;
  if NOT Assigned(check) then exit;

  Case check.Name of
    'FullScreenCheck': Window.FullScreen:= check.Checked;
  end;
end;

procedure TViewSettingsMenu.ChangeSoundSlider(Sender: TObject);
var
  slider: TCastleFloatSlider;
begin
  slider:= Sender as TCastleFloatSlider;
  if NOT Assigned(slider) then exit;

  Case slider.Name of
    'SfxSlider':   SoundEngine.Volume:= SfxSlider.Value;
    'MusicSlider': SoundEngine.LoopingChannel[0].Volume:= MusicSlider.Value;
  end;
end;

procedure TViewSettingsMenu.ClickPlusMinus(Sender: TObject);
const
  step = 0.1;
var
  button: TCastleButton;
begin
  button:= Sender as TCastleButton;
  if NOT Assigned(button) then exit;

  Case button.Name of
    'BtnSfxMinus':   StepChangeSlider(SfxSlider, -step);
    'BtnSfxPlus':    StepChangeSlider(SfxSlider, step);
    'BtnMusicMinus': StepChangeSlider(MusicSlider, -step);
    'BtnMusicPlus':  StepChangeSlider(MusicSlider, step);
  end;
end;

procedure TViewSettingsMenu.StepChangeSlider(slider: TCastleFloatSlider; step: Single);
var
  value: Single;
begin
  value:= slider.Value;
  value:= value + step;

  if (value < slider.Min) then value:= slider.Min
  else if (value > slider.Max) then value:= slider.Max;

  slider.Value:= value;
end;

end.
