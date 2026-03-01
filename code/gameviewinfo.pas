unit GameViewInfo;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  NyaWebButton;

type
  TViewInfo = class(TCastleView)
  strict private
    type
      TViewCreditsDialog = class(TCastleUserInterface)
      private
        ButtonClose: TCastleButton;
      private
        procedure FocusButton(const Sender: TCastleUserInterface);
        procedure ClickClose(Sender: TObject);
      public
        Closed: Boolean;
        constructor CreateWin(AOwner: TComponent; const AUrl: String);
      end;
    var
      FDialog: TViewCreditsDialog;
      FWinUrl: String;
  public
    constructor CreateUntilStopped(const AUrl: String);
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
  end;

implementation

uses
  CastleComponentSerialize, CastleSoundEngine, GameSound;

{ ========= ------------------------------------------------------------------ }
{ TViewCreditsDialog --------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

constructor TViewInfo.TViewCreditsDialog.CreateWin(AOwner: TComponent; const AUrl: String);
var
  UiOwner: TComponent;
  Ui: TCastleUserInterface;
begin
  inherited Create(AOwner);
  Closed:= False;

  // UiOwner is useful to keep reference to all components loaded from the design
  UiOwner := TComponent.Create(Self);

  { Load designed user interface }
  Ui := UserInterfaceLoad(AUrl, UiOwner);
  InsertFront(Ui);

  { Find components, by name, that we need to access from code }
  ButtonClose:= UiOwner.FindRequiredComponent('ButtonClose') as TCastleButton;

  ButtonClose.OnClick:= {$ifdef FPC}@{$endif}ClickClose;

  ButtonClose.OnInternalMouseEnter:= {$ifdef FPC}@{$endif}FocusButton;
end;

procedure TViewInfo.TViewCreditsDialog.FocusButton(const Sender: TCastleUserInterface);
begin
  SoundEngine.Play(NamedSound('SfxButtonFocus'));
end;

procedure TViewInfo.TViewCreditsDialog.ClickClose(Sender: TObject);
begin
  SoundEngine.Play(NamedSound('SfxButtonPress'));
  Closed:= True;
end;

{ ========= ------------------------------------------------------------------ }
{ TViewInfo --------------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

constructor TViewInfo.CreateUntilStopped(const AUrl: String);
begin
  inherited CreateUntilStopped;
  FWinUrl:= AUrl;
  DesignUrl:= 'castle-data:/bgwin.castle-user-interface';
end;

procedure TViewInfo.Start;
begin
  inherited;
  InterceptInput:= True;

  FDialog:= TViewCreditsDialog.CreateWin(FreeAtStop, FWinUrl);
  FDialog.Anchor(hpMiddle);
  FDialog.Anchor(vpMiddle);
  FDialog.FullSize:= True;
  InsertFront(FDialog);
end;

procedure TViewInfo.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;

  if FDialog.Closed then
    Container.PopView(Self);
end;

end.
