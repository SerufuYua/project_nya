unit GameViewCredits;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  NyaWebButton;

type
  TViewCredits = class(TCastleView)
  strict private
    type
      TViewCreditsDialog = class(TCastleUserInterface)
      private
        BtnClose: TCastleButton;
      private
        procedure FocusButton(const Sender: TCastleUserInterface);
        procedure ClickClose(Sender: TObject);
      public
        Closed: Boolean;
        constructor Create(AOwner: TComponent); override;
      end;
    var
      FDialog: TViewCreditsDialog;
  public
    constructor CreateUntilStopped;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
  end;

implementation

uses
  CastleComponentSerialize, CastleSoundEngine, GameSound;

{ ========= ------------------------------------------------------------------ }
{ TViewCreditsDialog --------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

constructor TViewCredits.TViewCreditsDialog.Create(AOwner: TComponent);
var
  UiOwner: TComponent;
  Ui: TCastleUserInterface;
begin
  inherited Create(AOwner);
  Closed:= False;

  // UiOwner is useful to keep reference to all components loaded from the design
  UiOwner := TComponent.Create(Self);

  { Load designed user interface }
  Ui := UserInterfaceLoad('castle-data:/gameviewcredits.castle-user-interface', UiOwner);
  InsertFront(Ui);

  { Find components, by name, that we need to access from code }
  BtnClose:= UiOwner.FindRequiredComponent('BtnClose') as TCastleButton;

  BtnClose.OnClick:= {$ifdef FPC}@{$endif}ClickClose;

  BtnClose.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusButton;
end;

procedure TViewCredits.TViewCreditsDialog.FocusButton(const Sender: TCastleUserInterface);
begin
  SoundEngine.Play(NamedSound('SfxButtonFocus'));
end;

procedure TViewCredits.TViewCreditsDialog.ClickClose(Sender: TObject);
begin
  SoundEngine.Play(NamedSound('SfxButtonPress'));
  Closed:= True;
end;

{ ========= ------------------------------------------------------------------ }
{ TViewCredits --------------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

constructor TViewCredits.CreateUntilStopped;
begin
  inherited CreateUntilStopped;
  DesignUrl:= 'castle-data:/bgsettings.castle-user-interface';
end;

procedure TViewCredits.Start;
begin
  inherited;
  InterceptInput:= True;

  FDialog:= TViewCreditsDialog.Create(FreeAtStop);
  FDialog.Anchor(hpMiddle);
  FDialog.Anchor(vpMiddle);
  FDialog.FullSize:= True;
  InsertFront(FDialog);
end;

procedure TViewCredits.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;

  if FDialog.Closed then
    Container.PopView(Self);
end;

end.
