unit GameViewTravelContainerRoom;

interface

uses
  Classes, BaseViewTravel, CastleVectors, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleTransform, NyaActorChara, NyaSwitch;

type
  TViewTravelContainerRoom = class(TBaseViewPlay)
  public
    procedure Start; override;
  protected
    FActorBoy: TNyaActorChara;
    procedure DoTouchSwitch(const Sender: TObject; Touch: Boolean); override;
    procedure DoActivateSwitch(Sender: TObject); override;
  protected
    procedure ConversationToyA;
    procedure ConversationBed;
    procedure ConversationBoy;
  protected
    procedure GetToGoToyA;
    procedure GetToGoBed;
    procedure GetToGoBoy;
    procedure GetToGoOut;
  end;

var
  ViewTravelContainerRoom: TViewTravelContainerRoom;

implementation

uses
  GameViewPlayGirl, GameViewPlaySolo, GameViewPlayTogether,
  GameViewTravelRoadAsteroid, GameViewConversationMenu, GameViewMain,
  CastleViewport, CastleScene, NyaCastleUtils, CastleComponentSerialize,
  CastleFonts, SysUtils;

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
    ConversationToyA;
  'BoySwitch':
    ConversationBoy;
  'GoOutSwitch':
    GetToGoOut;
  'BedSwitch':
    ConversationBed;
  else
    Notifications.Show('There is nothing to do');
  end;
end;

{ ========= ------------------------------------------------------------------ }
{ Conversations -------------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

procedure TViewTravelContainerRoom.ConversationToyA;
var
  messages: TMessages;
begin
  SetLength(messages, 1);
  messages[0].FActor:= FActorMain;
  messages[0].FMessage:= 'Lets play with my Toy!';
  Container.PushView(TViewConversationMenu.CreateUntilStopped(
                     messages,
                     {$ifdef FPC}@{$endif}GetToGoToyA,
                     nil));
end;

procedure TViewTravelContainerRoom.ConversationBed;
var
  messages: TMessages;
begin
  SetLength(messages, 1);
  messages[0].FActor:= FActorMain;
  messages[0].FMessage:= 'Maybe little relax in bed...';
  Container.PushView(TViewConversationMenu.CreateUntilStopped(
                     messages,
                     {$ifdef FPC}@{$endif}GetToGoBed,
                     nil));
end;

procedure TViewTravelContainerRoom.ConversationBoy;
var
  messages: TMessages;
begin
  SetLength(messages, 2);
  messages[0].FActor:= FActorMain;
  messages[0].FMessage:= 'Hey! Lets play together! Nya!';
  messages[1].FActor:= FActorBoy;
  messages[1].FMessage:= 'Ah... what... shure. Lets play! Nya!';
  Container.PushView(TViewConversationMenu.CreateUntilStopped(
                     messages,
                     {$ifdef FPC}@{$endif}GetToGoBoy,
                     nil));
end;

{ ========= ------------------------------------------------------------------ }
{ Get To Go ------------------------------------------------------------------ }
{ ========= ------------------------------------------------------------------ }

procedure TViewTravelContainerRoom.GetToGoToyA;
begin
  FGetToGo:= ViewPlayGirl;
end;

procedure TViewTravelContainerRoom.GetToGoBed;
begin
  FGetToGo:= ViewPlaySolo;
end;

procedure TViewTravelContainerRoom.GetToGoBoy;
begin
  FGetToGo:= ViewPlayTogether;
end;

procedure TViewTravelContainerRoom.GetToGoOut;
begin
  FGetToGo:= ViewTravelRoadAsteroid;
end;

end.
