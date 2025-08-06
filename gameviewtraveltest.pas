unit GameViewTravelTest;

interface

uses
  Classes, BaseViewTravel, NyaActorChara;

type
  TViewTravelTest = class(TBaseViewTravel)
  public
    procedure Start; override;
  end;

var
  ViewTravelTest: TViewTravelTest;

implementation

uses
  CastleSoundEngine, GameSound,
  GameViewDressingMenu;

procedure TViewTravelTest.Start;
begin
  { set map }
  Map.Url:= 'castle-data:/MapTest.castle-user-interface';

  { set Girl Character }
  MainActor:= Map.DesignedComponent('CharaGirl') as TNyaActorChara;

  { Play music }
//  SoundEngine.LoopingChannel[0].Sound:= NamedSound('MusicOutdoors');

  inherited;
end;

end.
