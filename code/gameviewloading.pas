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
    FWview: TCastleView;
    procedure DoLoading(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    procedure SetMessage(msg: String);
    procedure SetToLoad(view: TCastleView);
  end;

var
  ViewLoading: TViewLoading;

implementation

uses
  GameViewMain;

constructor TViewLoading.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl:= 'castle-data:/gameviewloading.castle-user-interface';
  FWview:= nil;
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

procedure TViewLoading.SetToLoad(view: TCastleView);
begin
  FWview:= view;
end;

procedure TViewLoading.DoLoading(Sender: TObject);
begin
  if Assigned(FWview) then
    Container.View:= FWview
  else
  begin
    SetMessage('ERROR: Nothing to load');
    Container.View:= ViewMain;
  end;
end;

end.
