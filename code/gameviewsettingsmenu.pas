unit GameViewSettingsMenu;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse;

const
  FullScreenPath = 'settings/screen/FullScreen';
  SfxPath = 'settings/sound/SFX';
  MusicPath = 'settings/sound/Music';
  DefaultFullScreen = False;
  DefaultSfxValue = 1.0;
  DefaultMusicvalue = 0.8;

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
    procedure FocusButton(const Sender: TCastleUserInterface);
    procedure ClickClose(Sender: TObject);
    procedure ClickScreen(Sender: TObject);
    procedure ClickPlusMinus(Sender: TObject);
    procedure ChangeSoundSlider(Sender: TObject);
  private
    procedure SetSfx(value: Single);
    procedure SetMusic(value: Single);
  end;

var
  ViewSettingsMenu: TViewSettingsMenu;

implementation

uses
  CastleWindow, CastleSoundEngine, GameSound, CastleConfig, NyaCastleUiUtils,
  CastleMessages;

constructor TViewSettingsMenu.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewsettingsmenu.castle-user-interface';
end;

procedure TViewSettingsMenu.Start;
begin
  inherited;
  InterceptInput:= True;

  FullScreenCheck.Checked:= UserConfig.GetValue(FullScreenPath,
                                                DefaultFullScreen);
  SfxSlider.Value:= UserConfig.GetFloat(SfxPath, DefaultSfxValue);
  MusicSlider.Value:= UserConfig.GetFloat(MusicPath, DefaultMusicvalue);

  BtnClose.OnClick:= {$ifdef FPC}@{$endif}ClickClose;
  FullScreenCheck.OnChange:= {$ifdef FPC}@{$endif}ClickScreen;
  SfxSlider.OnChange:= {$ifdef FPC}@{$endif}ChangeSoundSlider;
  MusicSlider.OnChange:= {$ifdef FPC}@{$endif}ChangeSoundSlider;
  BtnSfxMinus.OnClick:= {$ifdef FPC}@{$endif}ClickPlusMinus;
  BtnSfxPlus.OnClick:= {$ifdef FPC}@{$endif}ClickPlusMinus;
  BtnMusicMinus.OnClick:= {$ifdef FPC}@{$endif}ClickPlusMinus;
  BtnMusicPlus.OnClick:= {$ifdef FPC}@{$endif}ClickPlusMinus;

  BtnClose.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusButton;
  FullScreenCheck.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusButton;
  SfxSlider.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusButton;
  MusicSlider.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusButton;
  BtnSfxMinus.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusButton;
  BtnSfxPlus.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusButton;
  BtnMusicMinus.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusButton;
  BtnMusicPlus.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusButton;
end;

procedure TViewSettingsMenu.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  { Executed every frame. }
end;

procedure TViewSettingsMenu.FocusButton(const Sender: TCastleUserInterface);
begin
  SoundEngine.Play(NamedSound('SfxButtonFocus'));
end;

procedure TViewSettingsMenu.ClickClose(Sender: TObject);
begin
  SoundEngine.Play(NamedSound('SfxButtonPress'));
  Container.PopView(self);
end;

procedure TViewSettingsMenu.ClickScreen(Sender: TObject);
var
  check: TCastleCheckbox;
begin
  check:= Sender as TCastleCheckbox;
  if NOT Assigned(check) then exit;

  SoundEngine.Play(NamedSound('SfxButtonPress'));

  Case check.Name of
    'FullScreenCheck':
      begin
        UserConfig.SetValue(FullScreenPath, check.Checked);
        UserConfig.Save;
        MessageOK(Application.MainWindow,
                  'To switch the screen mode, you need to restart the game');
      end;
  end;
end;

procedure TViewSettingsMenu.ChangeSoundSlider(Sender: TObject);
var
  slider: TCastleFloatSlider;
begin
  slider:= Sender as TCastleFloatSlider;
  if NOT Assigned(slider) then exit;

  SoundEngine.Play(NamedSound('SfxButtonPress'));

  Case slider.Name of
    'SfxSlider':   SetSfx(SfxSlider.Value);
    'MusicSlider': SetMusic(MusicSlider.Value);
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

  SoundEngine.Play(NamedSound('SfxButtonPress'));

  Case button.Name of
    'BtnSfxMinus':
      begin
        StepChangeSlider(SfxSlider, -step);
        SetSfx(SfxSlider.Value);
      end;
    'BtnSfxPlus':
      begin
        StepChangeSlider(SfxSlider, step);
        SetSfx(SfxSlider.Value);
      end;
    'BtnMusicMinus':
      begin
        StepChangeSlider(MusicSlider, -step);
        SetMusic(MusicSlider.Value);
      end;
    'BtnMusicPlus':
      begin
        StepChangeSlider(MusicSlider, step);
        SetMusic(MusicSlider.Value);
      end;
  end;
end;

procedure TViewSettingsMenu.SetSfx(value: Single);
begin
  SoundEngine.Volume:= value;
  UserConfig.SetFloat(SfxPath, value);
  UserConfig.Save;
end;

procedure TViewSettingsMenu.SetMusic(value: Single);
begin
  SoundEngine.LoopingChannel[0].Volume:= value;
  UserConfig.SetFloat(MusicPath, value);
  UserConfig.Save;
end;

end.
