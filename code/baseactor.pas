unit BaseActor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleSceneCore, CastleTransform, CastleVectors,
  CastleColors, CharaDress;

type
  TBaseActor = class
  protected
    FActorRoot: TCastleTransformDesign;
    FActorName: String;
    function GetTrans(): TVector3;
    procedure SetTrans(coord: TVector3);
    function GetRot(): TVector4;
    procedure SetRot(coord: TVector4);
  public
    constructor Create(actorRoot: TCastleTransformDesign; actorName: String); virtual;
    procedure Update(const SecondsPassed: Single); virtual; abstract;
    procedure PauseAnimation; virtual; abstract;
    procedure PlayAnimation(const animationName: String; loop: boolean = true); virtual; abstract;
    procedure PlayAnimation(const Parameters: TPlayAnimationParameters); virtual; abstract;
    procedure StopAnimation(const DisableStopNotification: Boolean = false); virtual; abstract;
    procedure SetSpeed(value: Single); virtual; abstract;
    property Translation: TVector3 read GetTrans write SetTrans;
    property Rotation: TVector4 read GetRot write SetRot;
  end;

implementation

constructor TBaseActor.Create(actorRoot: TCastleTransformDesign; actorName: String);
begin
  FActorRoot:= actorRoot as TCastleTransformDesign;
  FActorName:= actorName;
end;

function TBaseActor.GetTrans(): TVector3;
begin
  Result:= FActorRoot.Translation;
end;

procedure TBaseActor.SetTrans(coord: TVector3);
begin
  FActorRoot.Translation:= coord;
end;

function TBaseActor.GetRot(): TVector4;
begin
  Result:= FActorRoot.Rotation;
end;

procedure TBaseActor.SetRot(coord: TVector4);
begin
  FActorRoot.Rotation:= coord;
end;

end.

