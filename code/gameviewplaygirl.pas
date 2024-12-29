unit GameViewPlayGirl;

interface

uses
  Classes, BaseViewPlay, NyaActorChara, NyaActorToyA;

type
  TViewPlayGirl = class(TBaseViewPlay)
  public
    procedure Start; override;
  end;

var
  ViewPlayGirl: TViewPlayGirl;

implementation

uses
  SysUtils, CastleTransform, NyaActor, ActorsLogic;

procedure TViewPlayGirl.Start;
var
  actorA, actorB: TNyaActor;
begin
  { set map }
  Map.Url:= 'castle-data:/MapPlayGirlToyA.castle-transform';

  { get Girl Character instance }
  actorA:= Map.DesignedComponent('CharaGirl') as TNyaActor;

  { get Toys instance }
  actorB:= Map.DesignedComponent('ToyA') as TNyaActor;

  { Create Actors Logic }
    FActorsLogic:= TActorsLogic.Create([ actorA, actorB ],
                                       'GAME.GIRL_TOYA.PLAY',
                                       FadeEffect);
  inherited;
end;

end.
