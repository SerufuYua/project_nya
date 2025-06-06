unit NyaRoundRectangle;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleUIControls, CastleClassUtils, CastleVectors,
  CastleColors;

type
  TNyaRoundRectangle = class(TCastleUserInterface)
  protected
    FPoints: array of TVector2;
    FRound: Single;
    FColor: TCastleColor;
    FColorPersistent: TCastleColorPersistent;
    procedure SetRound(const value: Single);
    function GetColorForPersistent: TCastleColor;
    procedure SetColorForPersistent(const AValue: TCastleColor);
    procedure CalcPoints;
  public
    const
      DefaultColor: TCastleColor = (X: 1.0; Y: 1.0; Z: 1.0; W: 1.0);
      DefaultRound = 20;

    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    procedure Render; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    property Color: TCastleColor read FColor write FColor;
  published
    property Round: Single read FRound write SetRound
             {$ifdef FPC}default DefaultRound{$endif};
    property ColorPersistent: TCastleColorPersistent read FColorPersistent;
  end;

implementation

uses
  CastleComponentSerialize, CastleUtils, CastleGLUtils, Math;

constructor TNyaRoundRectangle.Create(AOwner: TComponent);
begin
  inherited;
  FColor:= DefaultColor;
  FRound:= DefaultRound;

  { Persistent for Color }
  FColorPersistent:= TCastleColorPersistent.Create(nil);
  FColorPersistent.SetSubComponent(true);
  FColorPersistent.InternalGetValue:= {$ifdef FPC}@{$endif}GetColorForPersistent;
  FColorPersistent.InternalSetValue:= {$ifdef FPC}@{$endif}SetColorForPersistent;
  FColorPersistent.InternalDefaultValue:= Color;

  { create points }
  SetLength(FPoints, 8);
end;

procedure TNyaRoundRectangle.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  CalcPoints;
end;

procedure TNyaRoundRectangle.Render;
begin
  inherited;
  DrawPrimitive2D(TPrimitiveMode.pmTriangleFan, FPoints, FColor);
end;

procedure TNyaRoundRectangle.CalcPoints;
begin
  FPoints[0].X:= RenderRect.Left + FRound;
  FPoints[0].Y:= RenderRect.Top;

  if (FPoints[0].X > (RenderRect.Right - FRound)) then
    FPoints[0].X:= (RenderRect.Right - RenderRect.Width / 2.0);

  FPoints[1].X:= RenderRect.Left;
  FPoints[1].Y:= RenderRect.Top - FRound;

  if (FPoints[1].Y < (RenderRect.Bottom + FRound)) then
    FPoints[1].Y:= (RenderRect.Bottom + RenderRect.Height / 2.0);

  FPoints[2].X:= RenderRect.Left;
  FPoints[2].Y:= RenderRect.Bottom + FRound;

  if (FPoints[2].Y > (RenderRect.Top - FRound)) then
    FPoints[2].Y:= (RenderRect.Top - RenderRect.Height / 2.0);

  FPoints[3].X:= RenderRect.Left + FRound;
  FPoints[3].Y:= RenderRect.Bottom;

  if (FPoints[3].X > (RenderRect.Right - FRound)) then
    FPoints[3].X:= (RenderRect.Right - RenderRect.Width / 2.0);

  FPoints[4].X:= RenderRect.Right - FRound;
  FPoints[4].Y:= RenderRect.Bottom;

  if (FPoints[4].X < (RenderRect.Left + FRound)) then
    FPoints[4].X:= (RenderRect.Left + RenderRect.Width / 2.0);

  FPoints[5].X:= RenderRect.Right;
  FPoints[5].Y:= RenderRect.Bottom + FRound;

  if (FPoints[5].Y > (RenderRect.Top - FRound)) then
    FPoints[5].Y:= (RenderRect.Top - RenderRect.Height / 2.0);

  FPoints[6].X:= RenderRect.Right;
  FPoints[6].Y:= RenderRect.Top - FRound;

  if (FPoints[6].Y < (RenderRect.Bottom + FRound)) then
    FPoints[6].Y:= (RenderRect.Bottom + RenderRect.Height / 2.0);

  FPoints[7].X:= RenderRect.Right - FRound;
  FPoints[7].Y:= RenderRect.Top;

  if (FPoints[7].X < (RenderRect.Left + FRound)) then
    FPoints[7].X:= (RenderRect.Left + RenderRect.Width / 2.0);
end;

procedure TNyaRoundRectangle.SetRound(const value: Single);
var
  cRound: Single;
begin
  cRound:= value;

  if (cRound < 0.0) then
    cRound:= -cRound;

  if (FRound <> cRound) then
    FRound:= cRound;
end;

function TNyaRoundRectangle.GetColorForPersistent: TCastleColor;
begin
  Result:= Color;
end;

procedure TNyaRoundRectangle.SetColorForPersistent(const AValue: TCastleColor);
begin
  Color:= AValue;
end;

function TNyaRoundRectangle.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'ColorPersistent', 'Round'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TNyaRoundRectangle, 'Nya Round Rectangle');
end.

