unit GameViewTravelSpaceJunk;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, CastleUIControls, CastleControls, CastleKeysMouse, CastleTransform,
  BaseViewRide, NyaActor, NyaActorChara, NyaActorVehicle, NyaVehicleNavigation;

type
  TViewTravelSpaceJunk = class(TBaseViewRide)
  public
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
  protected
    FActorSpacePlane: TNyaActor;
    procedure DoTouchSwitch(const Sender: TObject; Touch: Boolean); override;
    procedure DoActivateSwitch(Sender: TObject); override;
  protected
    procedure ConversationSpacePlane;
  protected
    procedure TalkToPlaneOk;
  protected
    procedure GetToGoShip;
    procedure GetToGoRoadAsteroid;
  end;

var
  ViewTravelSpaceJunk: TViewTravelSpaceJunk;

implementation

uses
  SysUtils, CastleViewport, CastleScene, CastleUtils, CastleVectors,
  CastleComponentSerialize,
  CastleSoundEngine, GameSound,
  NyaSwitch, NyaCastleUtils, NyaWorldCondition,
  GameViewDressing,
  GameViewTravelRoadAsteroid, GameViewMain, GameViewTravelSpaceshipIndoors,
  GameViewConversation;

procedure TViewTravelSpaceJunk.Start;
begin
  { set map }
  Map.Url:= 'castle-data:/MapTravel_SpaceJunk.castle-user-interface';

  { set Girl Character }
  MainActor:= Map.DesignedComponent('CharaGirl') as TNyaActorChara;

  { set Space Plane Character }
  FActorSpacePlane:= Map.DesignedComponent('SpacePlane') as TNyaActor;
  FActorSpacePlane.Exists:= WorldCondition.Boy.Location in
                            [TBoyLocation.HomeSleep, TBoyLocation.HomeWorking];

  { set vehicle Navigation }
  FVehicleNavigation:= Map.DesignedComponent('VehicleNavigation') as TNyaVehicleNavigation;

  { Play music }
  FWalkMusic:= NamedSound('MusicOutdoors');
  FRideMusic:= NamedSound('MusicRide');
  SoundEngine.LoopingChannel[0].Sound:= FWalkMusic;

  inherited;
end;

procedure TViewTravelSpaceJunk.Update(const SecondsPassed: Single;
                                      var HandleInput: boolean);
begin
  { update Plane visibility }
  WorldCondition.Boy.Visible:= PointVisible(FActorSpacePlane.Translation);

  { update Space Plane Exists }
  FActorSpacePlane.Exists:= WorldCondition.Boy.Location in
                            [TBoyLocation.HomeSleep, TBoyLocation.HomeWorking];
  inherited;
end;

procedure TViewTravelSpaceJunk.DoTouchSwitch(const Sender: TObject; Touch: Boolean);
var
  switch: TNyaSwitch;
begin
  inherited;

  switch:= Sender as TNyaSwitch;
  if NOT Assigned(switch) then Exit;

  Case switch.Name of
  'SwitchRoadAsteroud': if Touch then GetToGoRoadAsteroid;
  end;
end;

procedure TViewTravelSpaceJunk.DoActivateSwitch(Sender: TObject);
var
  switch: TNyaSwitch;
begin
  switch:= Sender as TNyaSwitch;
  if NOT Assigned(switch) then Exit;

  inherited;

  Case switch.Name of
  'GoShipSwitch':
    GetToGoShip;
  'SpacePlaneSwitch':
    ConversationSpacePlane;
  'SwitchMoto':
    SitToVehicle(Map.DesignedComponent('VehicleMoto') as TNyaActorVehicle);
  else
    Notifications.Show('There is nothing to do');
  end;
end;

{ ========= ------------------------------------------------------------------ }
{ Conversations -------------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

procedure TViewTravelSpaceJunk.ConversationSpacePlane;
var
  messages: TMessages;
begin
  if (TBoyStatus.FirstTalkDone in WorldCondition.Boy.Status) then
  begin
    SetLength(messages, 3);
    messages[0].FActor:= MainActor;
    messages[0].FMessage:= '<p>Hi! How are you!</p>';
    messages[1].FActor:= FActorSpacePlane;
    messages[1].FMessage:= '<p>Fine! How are you?</p>';
    messages[2].FActor:= MainActor;
    messages[2].FMessage:= '<p>Nya!</p>';
    Container.PushView(TViewConversation.CreateUntilStopped(
                       messages,
                       nil,
                       nil));
  end else
  begin
    SetLength(messages, 5);
    messages[0].FActor:= MainActor;
    messages[0].FMessage:= '<p>Hi!</p><p>How are you?!</p>';
    messages[1].FActor:= FActorSpacePlane;
    messages[1].FMessage:= '<p>Yo! I&apos;m fine.</p>';
    messages[2].FActor:= MainActor;
    messages[2].FMessage:= '<p>Have you seen my friend?</p>';
    messages[3].FActor:= FActorSpacePlane;
    messages[3].FMessage:= '<p>Why are you asking me? I&apos;m not a spy for him. ' +
                           'I&apos;m just his vehicle. Look for him yourself...</p>' +
                           '<p>Maybe he&apos;s somewhere on board a ship.<br>' +
                           'Not on me ha-ha... I don&apos;t know.</p>';
    messages[4].FActor:= MainActor;
    messages[4].FMessage:= '<p>Thanks! See ya!</p>';
    Container.PushView(TViewConversation.CreateUntilStopped(
                       messages,
                       {$ifdef FPC}@{$endif}TalkToPlaneOk,
                       nil));
  end;
end;

{ ========= ------------------------------------------------------------------ }
{ TalkTo Result -------------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

procedure TViewTravelSpaceJunk.TalkToPlaneOk;
begin
  WorldCondition.Boy.Status:= WorldCondition.Boy.Status + [TBoyStatus.InSearch];
end;

{ ========= ------------------------------------------------------------------ }
{ Get To Go ------------------------------------------------------------------ }
{ ========= ------------------------------------------------------------------ }

procedure TViewTravelSpaceJunk.GetToGoShip;
begin
  GetToGo(ViewTravelSpaceshipIndoors);
end;

procedure TViewTravelSpaceJunk.GetToGoRoadAsteroid;
begin
  if Assigned(Vehicle) then
    GetToRide(ViewTravelRoadAsteroid, Vehicle.Name,
                                      Vector3(8.4, 13.3, -257.0),
                                      Vector4(0.0, 1.0, 0.0, Deg(-25.0)))
  else
    GetToGo(ViewTravelRoadAsteroid, Vector3(-0.4, 0.1, -11.0),
                                    Vector4(0.0, 1.0, 0.0, Deg(180.0)));
end;

end.

