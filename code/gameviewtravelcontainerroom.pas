unit GameViewTravelContainerRoom;

interface

uses Classes, BaseViewTravel,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  CastleTransform, NyaActorChara, NyaSwitch;

type
  TViewTravelContainerRoom = class(TBaseViewPlay)
  public
    procedure Start; override;
  protected
    FActorBoy: TNyaActorChara;
    procedure DoTouchSwitch(const Sender: TObject; Touch: Boolean); override;
    procedure DoActivateSwitch(Sender: TObject); override;
  end;

var
  ViewTravelContainerRoom: TViewTravelContainerRoom;

implementation

uses
  GameViewPlayGirl, GameViewPlayTogether, GameViewTravelRoadAsteroid,
  GameViewMain, CastleViewport, CastleScene, NyaCastleUtils,
  CastleComponentSerialize, CastleFonts, SysUtils, GameViewDressingMenu;

procedure TViewTravelContainerRoom.Start;
begin
  { set map }
  Map.Url:= 'castle-data:/MapTravel_ContainerRoom.castle-user-interface';

  { set Girl Character }
  FActorMain:= Map.DesignedComponent('CharaGirl') as TNyaActorChara;

  { set Boy Character }
  FActorBoy:= Map.DesignedComponent('CharaBoy') as TNyaActorChara;

  inherited;
end;

procedure TViewTravelContainerRoom.DoTouchSwitch(const Sender: TObject; Touch: Boolean);
var
  switch: TNyaSwitch;
begin
  inherited;

  switch:= Sender as TNyaSwitch;
  if NOT Assigned(switch) then Exit;

  Case switch.Name of
  'BoySwitch':
    begin
      if Touch then
        FActorBoy.AutoAnimation:= 'GAME.INROOM.SEAT_WITH_PDA.LOOKING'
      else
        FActorBoy.AutoAnimation:= 'GAME.INROOM.SEAT_WITH_PDA.WORKING';
    end;
  end;
end;

procedure TViewTravelContainerRoom.DoActivateSwitch(Sender: TObject);
var
  switch: TNyaSwitch;
begin
  switch:= Sender as TNyaSwitch;
  if NOT Assigned(switch) then Exit;

  Case switch.Name of
  'ToyASwitch':
    begin
      Notifications.Show('Lets play with my Toy!');
      FGetToGo:= ViewPlayGirl;
    end;
  'BoySwitch':
    begin
      Notifications.Show('Lets play Together!');
      FGetToGo:= ViewPlayTogether;
    end;
  'GoOutSwitch':
    begin
      Notifications.Show('What is outside?');
      FGetToGo:= ViewTravelRoadAsteroid;
    end;
  { #todo : Solo Play Scene }
{  'BedSwitch':
    begin
      Notifications.Show('Lets play in my Bed!');
      FGetToGo:= ViewPlaySolo;
    end;}
  else
    Notifications.Show('There is nothing to do');
  end;
end;

end.
