unit ActorInterfaces;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleVectors, CastleColors, CharaDress;

type
  IActor = interface
    procedure PauseAnimation;
    procedure PlayAnimation(const animationName: String; loop: boolean = true);
    property Pos: TVector3;
    property Rot: TVector4;
    property Lightning: Boolean;
    property SelfEmission: Single;
    property Speed: Single;
  end;

  IActorChara = interface
    procedure SaveCondition;
    function GetDresser(): TCharaDresser;
    property PersonalColor: TCastleColorRGB;
  end;

implementation

end.

