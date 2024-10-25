unit ActorInterfaces;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleSceneCore, CastleVectors, CastleColors, CharaDress;

type
  IActor = interface
    procedure PauseAnimation;
    procedure Update(const SecondsPassed: Single);
    procedure PlayAnimation(const animationName: String; loop: boolean = true);
    procedure PlayAnimation(const Parameters: TPlayAnimationParameters);
    procedure StopAnimation(const DisableStopNotification: Boolean = false);
    procedure SetSpeed(value: Single);
    function GetDresser(): TCharaDresser;
    property Pos: TVector3;
    property Rot: TVector4;
    property Lightning: Boolean;
    property SelfEmission: Single;
  end;

implementation

end.

