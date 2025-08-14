unit GameViewSettings;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse;

const
  FullScreenPath = 'settings/screen/FullScreen';
  SfxPath = 'settings/sound/SFX';
  MusicPath = 'settings/sound/Music';
  DefaultFullScreen = False;
  DefaultSfxValue = 1.0;
  DefaultMusicValue = 0.8;

type
  TViewSettings = class(TCastleView)
  strict private
    type
      TViewSettingsDialog = class(TCastleUserInterface)
      private
        ButtonClose: TCastleButton;
        CheckFullScreen: TCastleCheckbox;
        SliderSfx, SliderMusic: TCastleFloatSlider;
        ButtonSfxMinus, ButtonSfxPlus, ButtonMusicMinus,
          ButtonMusicPlus: TCastleButton;
      private
        procedure FocusButton(const Sender: TCastleUserInterface);
        procedure ClickClose(Sender: TObject);
        procedure ClickScreen(Sender: TObject);
        procedure ClickPlusMinus(Sender: TObject);
        procedure ChangeSliderSound(Sender: TObject);
        procedure SetSfx(value: Single);
        procedure SetMusic(value: Single);
      public
        Closed: Boolean;
        constructor Create(AOwner: TComponent); override;
      end;
    var
      FDialog: TViewSettingsDialog;
  public
    constructor CreateUntilStopped;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
  end;

implementation

uses
  CastleComponentSerialize, CastleWindow, CastleSoundEngine, GameSound,
  CastleConfig, NyaCastleUiUtils, CastleMessages;

{ ========= ------------------------------------------------------------------ }
{ TViewSettingsDialog -------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

constructor TViewSettings.TViewSettingsDialog.Create(AOwner: TComponent);
var
  UiOwner: TComponent;
  Ui: TCastleUserInterface;
begin
  inherited Create(AOwner);
  Closed:= False;

  // UiOwner is useful to keep reference to all components loaded from the design
  UiOwner := TComponent.Create(Self);

  { Load designed user interface }
  Ui := UserInterfaceLoad('castle-data:/gameviewsettings.castle-user-interface', UiOwner);
  InsertFront(Ui);

  { Find components, by name, that we need to access from code }
  ButtonClose:= UiOwner.FindRequiredComponent('ButtonClose') as TCastleButton;
  CheckFullScreen:= UiOwner.FindRequiredComponent('CheckFullScreen') as TCastleCheckbox;
  SliderSfx:= UiOwner.FindRequiredComponent('SliderSfx') as TCastleFloatSlider;
  SliderMusic:= UiOwner.FindRequiredComponent('SliderMusic') as TCastleFloatSlider;
  ButtonSfxMinus:= UiOwner.FindRequiredComponent('ButtonSfxMinus') as TCastleButton;
  ButtonSfxPlus:= UiOwner.FindRequiredComponent('ButtonSfxPlus') as TCastleButton;
  ButtonMusicMinus:= UiOwner.FindRequiredComponent('ButtonMusicMinus') as TCastleButton;
  ButtonMusicPlus:= UiOwner.FindRequiredComponent('ButtonMusicPlus') as TCastleButton;

  CheckFullScreen.Checked:= UserConfig.GetValue(FullScreenPath,
                                                DefaultFullScreen);
  SliderSfx.Value:= UserConfig.GetFloat(SfxPath, DefaultSfxValue);
  SliderMusic.Value:= UserConfig.GetFloat(MusicPath, DefaultMusicValue);

  ButtonClose.OnClick:= {$ifdef FPC}@{$endif}ClickClose;
  CheckFullScreen.OnChange:= {$ifdef FPC}@{$endif}ClickScreen;
  SliderSfx.OnChange:= {$ifdef FPC}@{$endif}ChangeSliderSound;
  SliderMusic.OnChange:= {$ifdef FPC}@{$endif}ChangeSliderSound;
  ButtonSfxMinus.OnClick:= {$ifdef FPC}@{$endif}ClickPlusMinus;
  ButtonSfxPlus.OnClick:= {$ifdef FPC}@{$endif}ClickPlusMinus;
  ButtonMusicMinus.OnClick:= {$ifdef FPC}@{$endif}ClickPlusMinus;
  ButtonMusicPlus.OnClick:= {$ifdef FPC}@{$endif}ClickPlusMinus;

  ButtonClose.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusButton;
  CheckFullScreen.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusButton;
  SliderSfx.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusButton;
  SliderMusic.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusButton;
  ButtonSfxMinus.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusButton;
  ButtonSfxPlus.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusButton;
  ButtonMusicMinus.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusButton;
  ButtonMusicPlus.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusButton;
end;

procedure TViewSettings.TViewSettingsDialog.FocusButton(const Sender: TCastleUserInterface);
begin
  SoundEngine.Play(NamedSound('SfxButtonFocus'));
end;

procedure TViewSettings.TViewSettingsDialog.ClickClose(Sender: TObject);
begin
  SoundEngine.Play(NamedSound('SfxButtonPress'));
  Closed:= True;
end;

procedure TViewSettings.TViewSettingsDialog.ClickScreen(Sender: TObject);
var
  check: TCastleCheckbox;
begin
  check:= Sender as TCastleCheckbox;
  if NOT Assigned(check) then exit;

  SoundEngine.Play(NamedSound('SfxButtonPress'));

  UserConfig.SetValue(FullScreenPath, check.Checked);
  UserConfig.Save;
  MessageOK(Application.MainWindow,
            'To switch the screen mode, you need to restart the game');
end;

procedure TViewSettings.TViewSettingsDialog.ChangeSliderSound(Sender: TObject);
var
  slider: TCastleFloatSlider;
begin
  slider:= Sender as TCastleFloatSlider;
  if NOT Assigned(slider) then exit;

  SoundEngine.Play(NamedSound('SfxButtonPress'));

  Case slider.Name of
    'SliderSfx':   SetSfx(SliderSfx.Value);
    'SliderMusic': SetMusic(SliderMusic.Value);
  end;
end;

procedure TViewSettings.TViewSettingsDialog.ClickPlusMinus(Sender: TObject);
const
  step = 0.1;
var
  button: TCastleButton;
begin
  button:= Sender as TCastleButton;
  if NOT Assigned(button) then exit;

  SoundEngine.Play(NamedSound('SfxButtonPress'));

  Case button.Name of
    'ButtonSfxMinus':
      begin
        StepChangeSlider(SliderSfx, -step);
        SetSfx(SliderSfx.Value);
      end;
    'ButtonSfxPlus':
      begin
        StepChangeSlider(SliderSfx, step);
        SetSfx(SliderSfx.Value);
      end;
    'ButtonMusicMinus':
      begin
        StepChangeSlider(SliderMusic, -step);
        SetMusic(SliderMusic.Value);
      end;
    'ButtonMusicPlus':
      begin
        StepChangeSlider(SliderMusic, step);
        SetMusic(SliderMusic.Value);
      end;
  end;
end;

procedure TViewSettings.TViewSettingsDialog.SetSfx(value: Single);
begin
  SoundEngine.Volume:= value;
  UserConfig.SetFloat(SfxPath, value);
  UserConfig.Save;
end;

procedure TViewSettings.TViewSettingsDialog.SetMusic(value: Single);
begin
  SoundEngine.LoopingChannel[0].Volume:= value;
  UserConfig.SetFloat(MusicPath, value);
  UserConfig.Save;
end;

{ ========= ------------------------------------------------------------------ }
{ TViewSettings -------------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

constructor TViewSettings.CreateUntilStopped;
begin
  inherited CreateUntilStopped;
  DesignUrl:= 'castle-data:/bgsettings.castle-user-interface';
end;

procedure TViewSettings.Start;
begin
  inherited;
  InterceptInput:= True;

  FDialog:= TViewSettingsDialog.Create(FreeAtStop);
  FDialog.Anchor(hpMiddle);
  FDialog.Anchor(vpMiddle);
  FDialog.FullSize:= True;
  InsertFront(FDialog);
end;

procedure TViewSettings.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;

  if FDialog.Closed then
    Container.PopView(Self);
end;

end.
