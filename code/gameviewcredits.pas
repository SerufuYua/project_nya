unit GameViewCredits;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse;

type
  TViewCredits = class(TCastleView)
  strict private
    type
      TViewCreditsDialog = class(TCastleUserInterface)
      private
        BtnClose, CGEButton, MusicAuthorButton1, MusicAuthorButton2,
          AuthorButton, SourceButton: TCastleButton;
      private
        procedure FocusButton(const Sender: TCastleUserInterface);
        procedure ClickClose(Sender: TObject);
        procedure ClickUrl(Sender: TObject);
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
  CastleComponentSerialize, CastleSoundEngine, GameSound, CastleOpenDocument;

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
  CGEButton:= UiOwner.FindRequiredComponent('CGEButton') as TCastleButton;
  MusicAuthorButton1:= UiOwner.FindRequiredComponent('MusicAuthorButton1') as TCastleButton;
  MusicAuthorButton2:= UiOwner.FindRequiredComponent('MusicAuthorButton2') as TCastleButton;
  AuthorButton:= UiOwner.FindRequiredComponent('AuthorButton') as TCastleButton;
  SourceButton:= UiOwner.FindRequiredComponent('SourceButton') as TCastleButton;

  BtnClose.OnClick:= {$ifdef FPC}@{$endif}ClickClose;
  CGEButton.OnClick:= {$ifdef FPC}@{$endif}ClickUrl;
  MusicAuthorButton1.OnClick:= {$ifdef FPC}@{$endif}ClickUrl;
  MusicAuthorButton2.OnClick:= {$ifdef FPC}@{$endif}ClickUrl;
  AuthorButton.OnClick:= {$ifdef FPC}@{$endif}ClickUrl;
  SourceButton.OnClick:= {$ifdef FPC}@{$endif}ClickUrl;

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

procedure TViewCredits.TViewCreditsDialog.ClickUrl(Sender: TObject);
var
  button: TCastleButton;
begin
  button:= Sender as TCastleButton;
  if NOT Assigned(button) then exit;

  Case button.Name of
    'CGEButton':
      OpenURL('https://castle-engine.io');
    'MusicAuthorButton1':
      OpenURL('https://www.youtube.com/@LunarMoth');
    'MusicAuthorButton2':
      OpenURL('https://www.bensound.com/royalty-free-music/track/moonlight-drive-lo-fi-relaxing');
    'AuthorButton':
      OpenURL('https://x.com/serufu_yua');
    'SourceButton':
      OpenURL('https://github.com/SerufuYua/project_nya');
  end;
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
