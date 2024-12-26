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
  SysUtils, CastleTransform, NyaBaseActor, ActorsLogic;

procedure TViewPlayGirl.Start;
var
  actorA, actorB: TNyaBaseActor;
begin
  { set map }
  Map.Url:= 'castle-data:/MapPlayGirlToyA.castle-transform';

  { get Girl Character instance }
  actorA:= Map.DesignedComponent('CharaGirl') as TNyaBaseActor;

  { get Toys instance }
  actorB:= Map.DesignedComponent('ToyA') as TNyaBaseActor;

{ Create Actors Logic }
  FActorsLogic:= TActorsLogic.Create([ actorA, actorB ],
                                     'GAME.GIRL_TOYA.PLAY',
                                     FadeEffect);
  inherited;
end;

end.
