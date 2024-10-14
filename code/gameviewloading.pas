unit GameViewLoading;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse;

type
  TViewLoading = class(TCastleView)
  published
    { Components designed using CGE editor }
    LabelMessage: TCastleLabel;
    LabelMessageShadow: TCastleLabel;
  private
    FWhatToLoad: TCastleView;
    procedure DoLoading(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    procedure SetMessage(msg: String);
    procedure SetLoading(whatToLoad: TCastleView);
  end;

var
  ViewLoading: TViewLoading;

implementation

uses
  GameViewMain;

constructor TViewLoading.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewloading.castle-user-interface';
end;

procedure TViewLoading.Start;
begin
  inherited;
  { Executed once when view starts. }

  WaitForRenderAndCall({$ifdef FPC}@{$endif}DoLoading);
end;

procedure TViewLoading.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  { Executed every frame. }
end;

procedure TViewLoading.SetMessage(msg: String);
begin
  LabelMessage.Caption:= msg;
  LabelMessageShadow.Caption:= msg;
end;

procedure TViewLoading.SetLoading(whatToLoad: TCastleView);
begin
  FWhatToLoad:= whatToLoad;
end;

procedure TViewLoading.DoLoading(Sender: TObject);
begin
  if Assigned(FWhatToLoad) then
    Container.View:= FWhatToLoad
  else
  begin
    SetMessage('ERROR: Nothing to load');
    Container.View:= ViewMain;
  end;
end;

end.
