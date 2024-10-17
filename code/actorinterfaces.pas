unit ActorInterfaces;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleSceneCore, CastleVectors, CastleColors, CharaDress;

type
  IActor = interface
    procedure PauseAnimation;
    procedure PlayAnimation(const animationName: String; loop: boolean = true);
    procedure PlayAnimation(const Parameters: TPlayAnimationParameters);
    function GetDresser(): TCharaDresser;
    property Pos: TVector3;
    property Rot: TVector4;
    property Lightning: Boolean;
    property SelfEmission: Single;
    property Speed: Single;
  end;

implementation

end.

