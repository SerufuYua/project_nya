unit GameViewPlayTogether;

interface

uses
  Classes, BaseViewPlay, NyaActorChara;

type
  TViewPlayTogether = class(TBaseViewPlay)
  public
    procedure Start; override;
  end;

var
  ViewPlayTogether: TViewPlayTogether;

implementation

uses
  SysUtils, CastleTransform, NyaActor, ActorsLogic;

procedure TViewPlayTogether.Start;
var
  actorA, actorB: TNyaActor;
begin
  { set map }
  Map.Url:= 'castle-data:/MapPlayTogether.castle-transform';

  { get Girl Character instance }
  actorA:= Map.DesignedComponent('CharaGirl') as TNyaActor;

  { get Boy Character instance }
  actorB:= Map.DesignedComponent('CharaBoy') as TNyaActor;

  { Create Actors Logic }
    FActorsLogic:= TActorsLogic.Create([ actorA, actorB ],
                                       'GAME.TOGETHER.PLAY',
                                       FadeEffect);
  inherited;
end;

end.
