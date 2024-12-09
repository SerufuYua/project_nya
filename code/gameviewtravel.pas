unit GameViewTravel;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  CastleThirdPersonNavigation, CastleTransform, CastleNotifications;

type
  TViewTravel = class(TCastleView)
  published
    LabelFps: TCastleLabel;
    ThirdPersonNavigation: TCastleThirdPersonNavigation;
    CameraMain: TCastleCamera;
    Notifications: TCastleNotifications;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewTravel: TViewTravel;

implementation

constructor TViewTravel.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewtravel.castle-user-interface';
end;

procedure TViewTravel.Start;
begin
  inherited;
  { Executed once when view starts. }
end;

procedure TViewTravel.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  { Executed every frame. }

  { update FPS }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption:= 'FPS: ' + Container.Fps.ToString;
end;

function TViewTravel.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  Notifications.Show(Event.Description);

  if Event.IsMouseButton(buttonRight) then
  begin
    ThirdPersonNavigation.MouseLook := not ThirdPersonNavigation.MouseLook;
    Exit(true);
  end;

end;

end.
