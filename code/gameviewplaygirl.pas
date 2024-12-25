unit GameViewPlayGirl;

interface

uses
  Classes, BaseViewPlay;

type
  TViewPlayGirl = class(TBaseViewPlay)
  public
    procedure Start; override;
  end;

var
  ViewPlayGirl: TViewPlayGirl;

implementation

uses
  SysUtils, CastleTransform, ActorChara, ActorToyA;

procedure TViewPlayGirl.Start;
var
  actorScene: TCastleTransformDesign;
begin
  { set map }
  Map.Url:= 'castle-data:/MapPlayGirlToyA.castle-transform';

  { Create Girl Character instance }
{  actorScene:= Map.DesignedComponent('CharaGirl') as TCastleTransformDesign;
  FActorA:= TActorChara.Create(actorScene, 'Girl');}

  { Create Toys instance }
{  actorScene:= Map.DesignedComponent('ToyA') as TCastleTransformDesign;
  FActorB:= TActorToyA.Create(actorScene, 'ToyA');}

  { set animation name prefix }
  FAnimationPrefix:= 'GAME.GIRL_TOYA.PLAY';

  inherited;
end;

end.
