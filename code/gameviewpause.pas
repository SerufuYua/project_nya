unit GameViewPause;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse;

type
  TViewPause = class(TCastleView)
  strict private
    type
      TViewPauseDialog = class(TCastleUserInterface)
      private
        BtnResume: TCastleButton;
      private
        procedure FocusButton(const Sender: TCastleUserInterface);
        procedure ClickResume(Sender: TObject);
      public
        Closed: Boolean;
        constructor Create(AOwner: TComponent); override;
      end;
    var
      FDialog: TViewPauseDialog;
  public
    constructor CreateUntilStopped;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
  end;

implementation

uses
  CastleComponentSerialize, CastleWindow, CastleSoundEngine, GameSound;

{ ========= ------------------------------------------------------------------ }
{ TViewPauseDialog ----------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

constructor TViewPause.TViewPauseDialog.Create(AOwner: TComponent);
var
  UiOwner: TComponent;
  Ui: TCastleUserInterface;
begin
  inherited Create(AOwner);
  Closed:= False;

  // UiOwner is useful to keep reference to all components loaded from the design
  UiOwner := TComponent.Create(Self);

  { Load designed user interface }
  Ui := UserInterfaceLoad('castle-data:/gameviewpause.castle-user-interface', UiOwner);
  InsertFront(Ui);

  { Find components, by name, that we need to access from code }
  BtnResume:= UiOwner.FindRequiredComponent('BtnResume') as TCastleButton;

  BtnResume.OnClick:= {$ifdef FPC}@{$endif}ClickResume;

  BtnResume.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusButton;
end;

procedure TViewPause.TViewPauseDialog.FocusButton(const Sender: TCastleUserInterface);
begin
  SoundEngine.Play(NamedSound('SfxButtonFocus'));
end;

procedure TViewPause.TViewPauseDialog.ClickResume(Sender: TObject);
begin
  SoundEngine.Play(NamedSound('SfxButtonPress'));
  Closed:= True;
end;

{ ========= ------------------------------------------------------------------ }
{ TViewPause ----------------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

constructor TViewPause.CreateUntilStopped;
begin
  inherited CreateUntilStopped;
  DesignUrl:= 'castle-data:/bgsettings.castle-user-interface';
end;

procedure TViewPause.Start;
begin
  inherited;
  InterceptInput:= True;

  FDialog:= TViewPauseDialog.Create(FreeAtStop);
  FDialog.Anchor(hpMiddle);
  FDialog.Anchor(vpMiddle);
  FDialog.FullSize:= True;
  InsertFront(FDialog);
end;

procedure TViewPause.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;

  if FDialog.Closed then
    Container.PopView(Self);
end;

end.
