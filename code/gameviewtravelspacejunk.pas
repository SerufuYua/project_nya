unit GameViewTravelSpaceJunk;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, CastleUIControls, CastleControls, CastleKeysMouse, CastleTransform,
  BaseViewTravel, NyaActor, NyaActorChara;

type
  TViewTravelSpaceJunk = class(TBaseViewTravel)
  public
    procedure Start; override;
  end;

var
  ViewTravelSpaceJunk: TViewTravelSpaceJunk;

implementation

uses
  SysUtils, CastleViewport, CastleScene, CastleUtils, CastleVectors,
  CastleComponentSerialize,
  CastleSoundEngine, GameSound,
  NyaSwitch, NyaCastleUtils, NyaWorldCondition,
  GameViewDressing, GameViewPlayGirl, GameViewPlaySolo,
  GameViewPlayTogether, GameViewTravelRoadAsteroid, GameViewMain,
  GameViewConversation;

procedure TViewTravelSpaceJunk.Start;
begin
  { set map }
  Map.Url:= 'castle-data:/MapTravel_SpaceJunk.castle-user-interface';

  { set Girl Character }
  MainActor:= Map.DesignedComponent('CharaGirl') as TNyaActorChara;

  { Play music }
  SoundEngine.LoopingChannel[0].Sound:= NamedSound('MusicOutdoors');

  inherited;
end;

end.

