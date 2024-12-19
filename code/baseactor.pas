unit BaseActor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleSceneCore, CastleTransform, CastleVectors,
  CastleColors, CharaDress;

type
  TBaseActor = class abstract
  protected
    FActorRoot: TCastleTransformDesign;
    FActorName: String;
    FPleasure: Single;
    FTension: Single;
    function GetTrans(): TVector3;
    procedure SetTrans(coord: TVector3);
    function GetRot(): TVector4;
    procedure SetRot(coord: TVector4);
  public
    constructor Create(AActorRoot: TCastleTransformDesign; AActorName: String); virtual;
    procedure Update(const SecondsPassed: Single); virtual; abstract;
    procedure PauseAnimation; virtual; abstract;
    procedure PlayAnimation(const animationName: String; loop: boolean = true); virtual; abstract;
    procedure PlayAnimation(const Parameters: TPlayAnimationParameters); virtual; abstract;
    procedure StopAnimation(const DisableStopNotification: Boolean = false); virtual; abstract;
    procedure SetSpeed(value: Single); virtual; abstract;
    property Translation: TVector3 read GetTrans write SetTrans;
    property Rotation: TVector4 read GetRot write SetRot;
    property ActorName: String read FActorName;
    property Pleasure: Single read FPleasure write FPleasure;
    property Tension: Single read FTension write FTension;
    property ActorRoot: TCastleTransformDesign read FActorRoot;
  end;

implementation

constructor TBaseActor.Create(AActorRoot: TCastleTransformDesign; AActorName: String);
begin
  FActorRoot:= AActorRoot as TCastleTransformDesign;
  FActorName:= AActorName;
end;

function TBaseActor.GetTrans(): TVector3;
begin
  Result:= ActorRoot.Translation;
end;

procedure TBaseActor.SetTrans(coord: TVector3);
begin
  ActorRoot.Translation:= coord;
end;

function TBaseActor.GetRot(): TVector4;
begin
  Result:= ActorRoot.Rotation;
end;

procedure TBaseActor.SetRot(coord: TVector4);
begin
  ActorRoot.Rotation:= coord;
end;

end.

