unit MyVectorMath;

{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, SysUtils, CastleVectors;

  function TurnVectorAroundVector(const turnVector, axis: TVector3;
                                  const angle: Single): TVector3;

  function TwoVectorAngleCoeff(const VectorA, VectorB,
                               PlaneNormal: TVector3): Single;

  function Sign(const num: Single): Single;

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

function TwoVectorAngleCoeff(const VectorA, VectorB,
                             PlaneNormal: TVector3): Single;
var
  AngleSign, AngleCoeff: Single;
begin
  AngleCoeff:= TVector3.DotProduct(VectorA, VectorB);
  AngleSign:= Sign(TVector3.DotProduct(PlaneNormal, TVector3.CrossProduct(VectorA, VectorB)));
  Result:= AngleSign * (1 - AngleCoeff);
end;

function Sign(const num: Single): Single;
begin
  if (num < 0) then
    Result:= -1.0
  else
    Result:= 1.0;
end;

end.

