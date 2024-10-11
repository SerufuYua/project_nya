unit ToysForGirlBehavior;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Generics.Collections,
  CastleVectors, CastleTransform, CastleScene;

type
  TToysForGirlBehavior = class(TCastleBehavior)
  private
    function GetSpeed: Single;
    procedure SetSpeed(value: Single);
  public
    constructor Create(AOwner: TComponent); override;
    procedure ParentAfterAttach(); override;
    procedure Update(const SecondsPassed: Single;
                     var RemoveMe: TRemoveType); override;
    procedure ActionPause;
    procedure ActionPlayToyA_Idle;
    procedure ActionPlayToyA_A1P1;
    procedure ActionPlayToyA_A2P1;
    property Speed: Single read GetSpeed write SetSpeed;
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
  ActionPlayToyA_Idle;
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

procedure TToysForGirlBehavior.ActionPlayToyA_Idle;
begin
  RailingUsed(True);
  GetCurrentTool().PlayAnimation('GAME.GIRL_TOYA.PLAY.IDLE', true);
end;

procedure TToysForGirlBehavior.ActionPlayToyA_A1P1;
begin
  RailingUsed(True);
  GetCurrentTool().PlayAnimation('GAME.GIRL_TOYA.PLAY.A1.P1', true);
end;

procedure TToysForGirlBehavior.ActionPlayToyA_A2P1;
begin
  RailingUsed(False);
  GetCurrentTool().PlayAnimation('GAME.GIRL_TOYA.PLAY.A2.P1', true);
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
    railing.Translation:= Vector3(-28, 0, -10);
    railing.Rotation:= Vector4(0, 1, 0, -30);
  end;
end;

function TToysForGirlBehavior.GetSpeed: Single;
begin
  Result:= GetCurrentTool().TimePlayingSpeed;
end;

procedure TToysForGirlBehavior.SetSpeed(value: Single);
begin
  GetCurrentTool().TimePlayingSpeed:= value;
end;

end.

