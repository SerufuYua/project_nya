unit GameViewTravelRoadAsteroid;

interface

uses Classes, BaseViewTravel,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  CastleTransform, NyaActorChara, NyaSwitch;

type
  TViewTravelRoadAsteroid = class(TBaseViewPlay)
  public
    procedure Start; override;
  protected
    procedure DoTouchSwitch(const Sender: TObject; Touch: Boolean); override;
    procedure DoActivateSwitch(Sender: TObject); override;
  end;

var
  ViewTravelRoadAsteroid: TViewTravelRoadAsteroid;

implementation

uses
  GameViewTravelContainerRoom, GameViewMain,
  CastleViewport, CastleScene, NyaCastleUtils, CastleComponentSerialize,
  CastleFonts, SysUtils, GameViewDressingMenu;

procedure TViewTravelRoadAsteroid.Start;
begin
  { set map }
  Map.Url:= 'castle-data:/MapTravel_RoadAsteroid.castle-user-interface';

  { set Girl Character }
  FActorMain:= Map.DesignedComponent('CharaGirl') as TNyaActorChara;

  inherited;
end;

procedure TViewTravelRoadAsteroid.DoTouchSwitch(const Sender: TObject; Touch: Boolean);
var
  switch: TNyaSwitch;
begin
  inherited;

  switch:= Sender as TNyaSwitch;
  if NOT Assigned(switch) then Exit;
end;

procedure TViewTravelRoadAsteroid.DoActivateSwitch(Sender: TObject);
var
  switch: TNyaSwitch;
begin
  switch:= Sender as TNyaSwitch;
  if NOT Assigned(switch) then Exit;

  Case switch.Name of
  'GoRoomSwitch':
    begin
      Notifications.Show('Im home!');
      FGetToGo:= ViewTravelContainerRoom;
    end;
  else
    Notifications.Show('There is nothing to do');
  end;
end;

end.
