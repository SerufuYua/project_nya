unit GameViewPlayTogether;

interface

uses
  Classes, BaseViewPlay;

type
  TViewPlayTogether = class(TBaseViewPlay)
  public
    procedure Start; override;
  end;

var
  ViewPlayTogether: TViewPlayTogether;

implementation

uses
  SysUtils, CastleTransform, ActorChara;

procedure TViewPlayTogether.Start;
var
  actorScene: TCastleTransformDesign;
begin
  { set map }
  Map.Url:= 'castle-data:/MapPlayTogether.castle-transform';

  { Create Girl Character instance }
{  actorScene:= Map.DesignedComponent('CharaGirl') as TCastleTransformDesign;
  FActorA:= TActorChara.Create(actorScene, 'Girl');}

  { Create Toys instance }
{  actorScene:= Map.DesignedComponent('CharaBoy') as TCastleTransformDesign;
  FActorB:= TActorChara.Create(actorScene, 'Boy');}

  { set animation name prefix }
  FAnimationPrefix:= 'GAME.TOGETHER.PLAY';

  inherited;
end;

end.
