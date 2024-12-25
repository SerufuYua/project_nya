unit NyaVectorMath;

{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, SysUtils, CastleVectors;

function TurnVectorAroundVector(const turnVector, axis: TVector3;
                                const angle: Single): TVector3;

{ calculate angular velocity for turn VectorA to VectorB }
function TurnVectorToVector(const VectorA, VectorB: TVector3): TVector3;

{ projection of vector VectorA to VectorB }
function ProjectionVectorAtoB(const VectorA, VectorB: TVector3): TVector3;

{ length of vector VectorA to VectorB projection }
function ProjectionVectorAtoBLength(const VectorA, VectorB: TVector3): Single;

implementation

function TurnVectorAroundVector(const turnVector, axis: TVector3;
                                const angle: Single): TVector3;
var
  CrossAxisVec: TVector3;
begin
  { using formula rotate vector v around vector k:
  v_rot = v + (1-cos(angle))(k x (k x v)) + sin(angle)(k x v) }

  CrossAxisVec:= TVector3.CrossProduct(axis, turnVector);
  Result:= turnVector +
           (1 - cos(angle)) * TVector3.CrossProduct(axis, CrossAxisVec) +
           sin(angle) * CrossAxisVec;
end;

function TurnVectorToVector(const VectorA, VectorB: TVector3): TVector3;
var
  TurnVec: TVector3;
  AngleCoeff: Single;
begin
  AngleCoeff:= 1 - TVector3.DotProduct(VectorA, VectorB);
  TurnVec:= TVector3.CrossProduct(VectorA, VectorB);
  Result:= AngleCoeff * TurnVec;
end;

function ProjectionVectorAtoB(const VectorA, VectorB: TVector3): TVector3;
begin
  Result:= VectorB * (TVector3.DotProduct(VectorA, VectorB) /
                      TVector3.DotProduct(VectorB, VectorB));
end;

function ProjectionVectorAtoBLength(const VectorA, VectorB: TVector3): Single;
begin
  Result:= TVector3.DotProduct(VectorA, VectorB) /
           TVector3.DotProduct(VectorB, VectorB);
end;

end.

