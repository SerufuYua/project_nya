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
  strict private
    type
      TViewSettingsDialog = class(TCastleUserInterface)
      private
        BtnClose: TCastleButton;
        FullScreenCheck: TCastleCheckbox;
        SfxSlider, MusicSlider: TCastleFloatSlider;
        BtnSfxMinus, BtnSfxPlus, BtnMusicMinus, BtnMusicPlus: TCastleButton;
      private
        procedure FocusButton(const Sender: TCastleUserInterface);
        procedure ClickClose(Sender: TObject);
        procedure ClickScreen(Sender: TObject);
        procedure ClickPlusMinus(Sender: TObject);
        procedure ChangeSoundSlider(Sender: TObject);
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
{ TViewSettingsMenu ---------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

constructor TViewSettingsMenu.TViewSettingsDialog.Create(AOwner: TComponent);
var
  UiOwner: TComponent;
  Ui: TCastleUserInterface;
begin
  inherited Create(AOwner);
  Closed:= False;

  // UiOwner is useful to keep reference to all components loaded from the design
  UiOwner := TComponent.Create(Self);

  { Load designed user interface }
  Ui := UserInterfaceLoad('castle-data:/gameviewsettingsmenu.castle-user-interface', UiOwner);
  InsertFront(Ui);

  { Find components, by name, that we need to access from code }
  BtnClose:= UiOwner.FindRequiredComponent('BtnClose') as TCastleButton;
  FullScreenCheck:= UiOwner.FindRequiredComponent('FullScreenCheck') as TCastleCheckbox;
  SfxSlider:= UiOwner.FindRequiredComponent('SfxSlider') as TCastleFloatSlider;
  MusicSlider:= UiOwner.FindRequiredComponent('MusicSlider') as TCastleFloatSlider;
  BtnSfxMinus:= UiOwner.FindRequiredComponent('BtnSfxMinus') as TCastleButton;
  BtnSfxPlus:= UiOwner.FindRequiredComponent('BtnSfxPlus') as TCastleButton;
  BtnMusicMinus:= UiOwner.FindRequiredComponent('BtnMusicMinus') as TCastleButton;
  BtnMusicPlus:= UiOwner.FindRequiredComponent('BtnMusicPlus') as TCastleButton;

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

procedure TViewSettingsMenu.TViewSettingsDialog.FocusButton(const Sender: TCastleUserInterface);
begin
  SoundEngine.Play(NamedSound('SfxButtonFocus'));
end;

procedure TViewSettingsMenu.TViewSettingsDialog.ClickClose(Sender: TObject);
begin
  SoundEngine.Play(NamedSound('SfxButtonPress'));
  Closed:= True;
end;

procedure TViewSettingsMenu.TViewSettingsDialog.ClickScreen(Sender: TObject);
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

procedure TViewSettingsMenu.TViewSettingsDialog.ChangeSoundSlider(Sender: TObject);
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

procedure TViewSettingsMenu.TViewSettingsDialog.ClickPlusMinus(Sender: TObject);
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

procedure TViewSettingsMenu.TViewSettingsDialog.SetSfx(value: Single);
begin
  SoundEngine.Volume:= value;
  UserConfig.SetFloat(SfxPath, value);
  UserConfig.Save;
end;

procedure TViewSettingsMenu.TViewSettingsDialog.SetMusic(value: Single);
begin
  SoundEngine.LoopingChannel[0].Volume:= value;
  UserConfig.SetFloat(MusicPath, value);
  UserConfig.Save;
end;

{ ========= ------------------------------------------------------------------ }
{ TViewSettingsMenu ---------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

constructor TViewSettingsMenu.CreateUntilStopped;
begin
  inherited CreateUntilStopped;
  DesignUrl:= 'castle-data:/bgsettings.castle-user-interface';
end;

procedure TViewSettingsMenu.Start;
begin
  inherited;
  InterceptInput:= True;

  FDialog:= TViewSettingsDialog.Create(FreeAtStop);
  FDialog.Anchor(hpMiddle);
  FDialog.Anchor(vpMiddle);
  FDialog.FullSize:= True;
  InsertFront(FDialog);
end;

procedure TViewSettingsMenu.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;

  if FDialog.Closed then
    Container.PopView(Self);
end;

end.
