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
    FPointsBGp1: array of TVector2;
    FPointsBGp2: array of TVector2;
    FPointsSpeckle: array of TVector2;
    FPointsOutline: array of TVector2;
    FRound: Single;
    FOutlineWidth: Single;
    FColor, FColorSpeckle: TCastleColor;
    FColorPersistent, FColorSpecklePersistent: TCastleColorPersistent;
    procedure SetRound(const value: Single);
    function GetColorForPersistent: TCastleColor;
    procedure SetColorForPersistent(const AValue: TCastleColor);
    function GetColorSpeckleForPersistent: TCastleColor;
    procedure SetColorSpeckleForPersistent(const AValue: TCastleColor);
    procedure CalcPoints;
  public
    const
      DefaultColor: TCastleColor = (X: 1.0; Y: 0.25; Z: 0.75; W: 0.5);
      DefaultColorSpeckle: TCastleColor = (X: 1.0; Y: 1.0; Z: 1.0; W: 0.5);
      DefaultRound = 20;
      DefaultOutlineWidth = 2.0;

    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    procedure Render; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    property Color: TCastleColor read FColor write FColor;
    property ColorSpeckle: TCastleColor read FColorSpeckle write FColorSpeckle;
  published
    property Round: Single read FRound write SetRound
             {$ifdef FPC}default DefaultRound{$endif};
    property OutlineWidth: Single read FOutlineWidth write FOutlineWidth
             {$ifdef FPC}default DefaultOutlineWidth{$endif};
    property ColorPersistent: TCastleColorPersistent read FColorPersistent;
    property ColorSpecklePersistent: TCastleColorPersistent read FColorSpecklePersistent;
  end;

implementation

uses
  CastleComponentSerialize, CastleUtils, CastleGLUtils, CastleRenderOptions,
  Math;

constructor TNyaRoundRectangle.Create(AOwner: TComponent);
begin
  inherited;
  FColor:= DefaultColor;
  FColorSpeckle:= DefaultColorSpeckle;
  FRound:= DefaultRound;
  FOutlineWidth:= DefaultOutlineWidth;

  { Persistent for Color }
  FColorPersistent:= TCastleColorPersistent.Create(nil);
  FColorPersistent.SetSubComponent(true);
  FColorPersistent.InternalGetValue:= {$ifdef FPC}@{$endif}GetColorForPersistent;
  FColorPersistent.InternalSetValue:= {$ifdef FPC}@{$endif}SetColorForPersistent;
  FColorPersistent.InternalDefaultValue:= Color;

  { Persistent for Color Speckle }
  FColorSpecklePersistent:= TCastleColorPersistent.Create(nil);
  FColorSpecklePersistent.SetSubComponent(true);
  FColorSpecklePersistent.InternalGetValue:= {$ifdef FPC}@{$endif}GetColorSpeckleForPersistent;
  FColorSpecklePersistent.InternalSetValue:= {$ifdef FPC}@{$endif}SetColorSpeckleForPersistent;
  FColorSpecklePersistent.InternalDefaultValue:= ColorSpeckle;

  { create points }
  SetLength(FPoints, 8);
  SetLength(FPointsSpeckle, 4);
  SetLength(FPointsBGp1, 4);
  SetLength(FPointsBGp2, 4);
  SetLength(FPointsOutline, 8);
end;

procedure TNyaRoundRectangle.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  CalcPoints;
end;

procedure TNyaRoundRectangle.Render;
begin
  inherited;
  DrawPrimitive2D(TPrimitiveMode.pmTriangleFan, FPointsBGp1, FColor);
  DrawPrimitive2D(TPrimitiveMode.pmTriangleFan, FPointsBGp2, FColor);
  DrawPrimitive2D(TPrimitiveMode.pmTriangleFan, FPointsSpeckle, FColorSpeckle);

  if (FOutlineWidth > 0.0) then
    DrawPrimitive2D(TPrimitiveMode.pmLineLoop, FPointsOutline, FColorSpeckle,
      bsSrcAlpha, bdOneMinusSrcAlpha, False, FOutlineWidth);
end;

procedure TNyaRoundRectangle.CalcPoints;
var
  i: Integer;
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

  FPointsBGp1[0]:= FPoints[0];
  FPointsBGp1[1]:= FPoints[1];
  FPointsBGp1[2]:= FPoints[2];
  FPointsBGp1[3]:= FPoints[7];

  FPointsBGp2[0]:= FPoints[3];
  FPointsBGp2[1]:= FPoints[4];
  FPointsBGp2[2]:= FPoints[5];
  FPointsBGp2[3]:= FPoints[6];

  FPointsSpeckle[0]:= FPoints[2];
  FPointsSpeckle[1]:= FPoints[3];
  FPointsSpeckle[2]:= FPoints[6];
  FPointsSpeckle[3]:= FPoints[7];

  for i:= Low(FPointsOutline) to High(FPointsOutline) do
    FPointsOutline[i]:= FPoints[i];
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

function TNyaRoundRectangle.GetColorSpeckleForPersistent: TCastleColor;
begin
  Result:= ColorSpeckle;
end;

procedure TNyaRoundRectangle.SetColorSpeckleForPersistent(const AValue: TCastleColor);
begin
  ColorSpeckle:= AValue;
end;

function TNyaRoundRectangle.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'ColorPersistent', 'ColorSpecklePersistent', 'Round', 'OutlineWidth'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TNyaRoundRectangle, 'Nya Round Rectangle');
end.

