unit ToysForGirlBehavior;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Generics.Collections,
  CastleVectors, CastleTransform, CastleScene;

type
  TToysForGirlBehavior = class(TCastleBehavior)
  public
    constructor Create(AOwner: TComponent); override;
    procedure ParentAfterAttach(); override;
    procedure Update(const SecondsPassed: Single;
                     var RemoveMe: TRemoveType); override;
    procedure ActionPause;
    procedure ActionIdle;
    procedure ActionPlayA1;
    procedure ActionPlayA2;
    procedure ActionPlayA3;
    procedure ActionPlayA4;
    procedure ActionPlayA5;
  protected
    Scene: TCastleTransformDesign;
    function GetCurrentTool(): TCastleScene;
    procedure RailingUsed(enable: Boolean);
  end;

implementation

uses
  CastleComponentSerialize;

constructor TToysForGirlBehavior.Create(AOwner: TComponent);
begin
  inherited;
  // any other initialization
end;

procedure TToysForGirlBehavior.ParentAfterAttach;
begin
  inherited;
  Scene:= Parent as TCastleTransformDesign;
  ActionIdle;
end;

procedure TToysForGirlBehavior.Update(const SecondsPassed: Single;
                                      var RemoveMe: TRemoveType);
begin
  inherited;
  // any other updates
end;

procedure TToysForGirlBehavior.ActionPause;
begin
  GetCurrentTool().StopAnimation();
end;

procedure TToysForGirlBehavior.ActionIdle;
begin
  GetCurrentTool().PlayAnimation('GAME.TOYA.IDLE', true);
end;

procedure TToysForGirlBehavior.ActionPlayA1;
begin
  RailingUsed(True);
  GetCurrentTool().PlayAnimation('GAME.RU.PLAY.A1', true);
end;

procedure TToysForGirlBehavior.ActionPlayA2;
begin
  RailingUsed(True);
  GetCurrentTool().PlayAnimation('GAME.RU.PLAY.A2', true);
end;

procedure TToysForGirlBehavior.ActionPlayA3;
begin
  RailingUsed(False);
  GetCurrentTool().PlayAnimation('GAME.RU.PLAY.A3', true);
end;

procedure TToysForGirlBehavior.ActionPlayA4;
begin
  RailingUsed(False);
  GetCurrentTool().PlayAnimation('GAME.RU.PLAY.A4', true);
end;

procedure TToysForGirlBehavior.ActionPlayA5;
begin
  RailingUsed(False);
  GetCurrentTool().PlayAnimation('GAME.RU.PLAY.A5', true);
end;

function TToysForGirlBehavior.GetCurrentTool(): TCastleScene;
begin
  Result:= Scene.DesignedComponent('ToyA') as TCastleScene;
end;

procedure TToysForGirlBehavior.RailingUsed(enable: Boolean);
var
  railing: TCastleScene;
begin
  railing:= Scene.DesignedComponent('Railing') as TCastleScene;

  if enable then
  begin
    railing.Translation:= Vector3(0, 0, 0);
    railing.Rotation:= Vector4(1, 0, 0, 0);
  end else begin
    railing.Translation:= Vector3(-30, 0, 0);
    railing.Rotation:= Vector4(0, 1, 0, 30);
  end;
end;

end.

