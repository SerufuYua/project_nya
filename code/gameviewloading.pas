unit GameViewLoading;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse;

type
  TViewLoading = class(TCastleView)
  published
    { Components designed using CGE editor }
    LabelMessage: TCastleLabel;
  private
    FWhatToLoad: TCastleView;
    procedure DoLoading(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
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
    LabelMessage.Caption:= 'ERROR: Nothing to load';
    Container.View:= ViewMain;
  end;
end;

end.
