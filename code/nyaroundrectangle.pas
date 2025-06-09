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
    FPointsBGp1, FPointsBGp2: array of TVector2;
    FPointsSpeckle, FPointsSpeckleLow1, FPointsSpeckleLow2: array of TVector2;
    FPointsOutline: array of TVector2;
    FRound1, FRound2: Single;
    FOutlineWidth, FOutlineWidthScaled: Single;
    FColor, FColorSpeckle, FColorSpeckleLow: TCastleColor;
    FColorPersistent, FColorSpecklePersistent: TCastleColorPersistent;
    procedure SetRound1(const value: Single);
    procedure SetRound2(const value: Single);
    function GetColorForPersistent: TCastleColor;
    procedure SetColorForPersistent(const AValue: TCastleColor);
    function GetColorSpeckleForPersistent: TCastleColor;
    procedure SetColorSpeckleForPersistent(const AValue: TCastleColor);
    procedure SetColor(value: TCastleColor);
    procedure SetColorSpeckle(value: TCastleColor);
    procedure CalcColorSpeckleLow;
    procedure CalcPoints;
  public
    const
      DefaultColor: TCastleColor = (X: 1.0; Y: 0.25; Z: 0.75; W: 0.5);
      DefaultColorSpeckle: TCastleColor = (X: 1.0; Y: 1.0; Z: 1.0; W: 0.5);
      DefaultRound1 = 20;
      DefaultRound2 = 15;
      DefaultOutlineWidth = 2.0;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    procedure Render; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    property Color: TCastleColor read FColor write SetColor;
    property ColorSpeckle: TCastleColor read FColorSpeckle write SetColorSpeckle;
  published
    property Round1: Single read FRound1 write SetRound1
             {$ifdef FPC}default DefaultRound1{$endif};
    property Round2: Single read FRound2 write SetRound2
             {$ifdef FPC}default DefaultRound2{$endif};
    property OutlineWidth: Single read FOutlineWidth write FOutlineWidth
             {$ifdef FPC}default DefaultOutlineWidth{$endif};
    property ColorPersistent: TCastleColorPersistent read FColorPersistent;
    property ColorSpecklePersistent: TCastleColorPersistent read FColorSpecklePersistent;
  end;

implementation

uses
  CastleComponentSerialize, CastleUtils, CastleGLUtils, CastleRenderOptions,
  CastleRectangles, Math;

constructor TNyaRoundRectangle.Create(AOwner: TComponent);
begin
  inherited;
  FColor:= DefaultColor;
  FColorSpeckle:= DefaultColorSpeckle;
  FRound1:= DefaultRound1;
  FRound2:= DefaultRound2;
  FOutlineWidth:= DefaultOutlineWidth;
  FOutlineWidthScaled:= FOutlineWidth * UIScale;

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
  SetLength(FPointsSpeckleLow1, 4);
  SetLength(FPointsSpeckleLow2, 4);
  SetLength(FPointsBGp1, 4);
  SetLength(FPointsBGp2, 4);
  SetLength(FPointsOutline, 8);
end;

destructor TNyaRoundRectangle.Destroy;
begin
  FreeAndNil(FColorPersistent);
  FreeAndNil(FColorSpecklePersistent);
  inherited;
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
  DrawPrimitive2D(TPrimitiveMode.pmTriangleFan, FPointsSpeckleLow1, FColorSpeckleLow);
  DrawPrimitive2D(TPrimitiveMode.pmTriangleFan, FPointsSpeckleLow2, FColorSpeckleLow);

  if (FOutlineWidth > 0.0) then
    DrawPrimitive2D(TPrimitiveMode.pmLineLoop, FPointsOutline, FColorSpeckle,
                    bsSrcAlpha, bdOneMinusSrcAlpha, False,
                    FOutlineWidthScaled);
end;

procedure TNyaRoundRectangle.CalcPoints;
const
  low1 = 0.2;
  low2 = 0.6;
var
  i: Integer;
  innerRect: TFloatRectangle;
  ident, lRound1, lRound2: single;
begin
  FOutlineWidthScaled:= FOutlineWidth * UIScale;
  ident:= FOutlineWidthScaled / 2.0;
  innerRect.Left:= RenderRect.Left + ident;
  innerRect.Bottom:= RenderRect.Bottom + ident;
  innerRect.Width:= RenderRect.Width - FOutlineWidth;
  innerRect.Height:= RenderRect.Height - FOutlineWidth;

  lRound1:= FRound1 * UIScale;
  lRound2:= FRound2 * UIScale;

  FPoints[0].X:= innerRect.Left + lRound2;
  FPoints[0].Y:= innerRect.Top;

  if (FPoints[0].X > (innerRect.Right - lRound2)) then
    FPoints[0].X:= (innerRect.Right - innerRect.Width / 2.0);

  FPoints[1].X:= innerRect.Left;
  FPoints[1].Y:= innerRect.Top - lRound2;

  if (FPoints[1].Y < (innerRect.Bottom + lRound2)) then
    FPoints[1].Y:= (innerRect.Bottom + innerRect.Height / 2.0);

  FPoints[2].X:= innerRect.Left;
  FPoints[2].Y:= innerRect.Bottom + lRound1;

  if (FPoints[2].Y > (innerRect.Top - lRound1)) then
    FPoints[2].Y:= (innerRect.Top - innerRect.Height / 2.0);

  FPoints[3].X:= innerRect.Left + lRound1;
  FPoints[3].Y:= innerRect.Bottom;

  if (FPoints[3].X > (innerRect.Right - lRound1)) then
    FPoints[3].X:= (innerRect.Right - innerRect.Width / 2.0);

  FPoints[4].X:= innerRect.Right - lRound2;
  FPoints[4].Y:= innerRect.Bottom;

  if (FPoints[4].X < (innerRect.Left + lRound2)) then
    FPoints[4].X:= (innerRect.Left + innerRect.Width / 2.0);

  FPoints[5].X:= innerRect.Right;
  FPoints[5].Y:= innerRect.Bottom + lRound2;

  if (FPoints[5].Y > (innerRect.Top - lRound2)) then
    FPoints[5].Y:= (innerRect.Top - innerRect.Height / 2.0);

  FPoints[6].X:= innerRect.Right;
  FPoints[6].Y:= innerRect.Top - lRound1;

  if (FPoints[6].Y < (innerRect.Bottom + lRound1)) then
    FPoints[6].Y:= (innerRect.Bottom + innerRect.Height / 2.0);

  FPoints[7].X:= innerRect.Right - lRound1;
  FPoints[7].Y:= innerRect.Top;

  if (FPoints[7].X < (innerRect.Left + lRound1)) then
    FPoints[7].X:= (innerRect.Left + innerRect.Width / 2.0);

  FPointsBGp1[0]:= FPoints[0];
  FPointsBGp1[1]:= FPoints[1];
  FPointsBGp1[2]:= FPoints[2];
  FPointsBGp1[3]:= FPoints[7];

  FPointsBGp2[0]:= FPoints[3];
  FPointsBGp2[1]:= FPoints[4];
  FPointsBGp2[2]:= FPoints[5];
  FPointsBGp2[3]:= FPoints[6];

  FPointsSpeckle[0]:= Lerp(low1, FPoints[2], FPoints[3]);
  FPointsSpeckle[1]:= Lerp(low2, FPoints[2], FPoints[3]);
  FPointsSpeckle[2]:= Lerp(low2, FPoints[7], FPoints[6]);
  FPointsSpeckle[3]:= Lerp(low1, FPoints[7], FPoints[6]);

  FPointsSpeckleLow1[0]:= FPoints[2];
  FPointsSpeckleLow1[1]:= FPointsSpeckle[0];
  FPointsSpeckleLow1[2]:= FPointsSpeckle[3];
  FPointsSpeckleLow1[3]:= FPoints[7];

  FPointsSpeckleLow2[0]:= FPointsSpeckle[1];
  FPointsSpeckleLow2[1]:= FPoints[3];
  FPointsSpeckleLow2[2]:= FPoints[6];
  FPointsSpeckleLow2[3]:= FPointsSpeckle[2];

  for i:= Low(FPointsOutline) to High(FPointsOutline) do
    FPointsOutline[i]:= FPoints[i];
end;

procedure TNyaRoundRectangle.SetRound1(const value: Single);
var
  cRound: Single;
begin
  cRound:= value;

  if (cRound < 0.0) then
    cRound:= -cRound;

  if (FRound1 <> cRound) then
    FRound1:= cRound;
end;

procedure TNyaRoundRectangle.SetRound2(const value: Single);
var
  cRound: Single;
begin
  cRound:= value;

  if (cRound < 0.0) then
    cRound:= -cRound;

  if (FRound2 <> cRound) then
    FRound2:= cRound;
end;

procedure TNyaRoundRectangle.SetColor(value: TCastleColor);
begin
  if TVector4.Equals(FColor,  value) then Exit;
  FColor:= value;

  CalcColorSpeckleLow;
end;

procedure TNyaRoundRectangle.SetColorSpeckle(value: TCastleColor);
begin
  if TVector4.Equals(FColorSpeckle, value) then Exit;
  FColorSpeckle:= value;

  CalcColorSpeckleLow;
end;

procedure TNyaRoundRectangle.CalcColorSpeckleLow;
begin
  FColorSpeckleLow:= Lerp(0.5, FColor, FColorSpeckle);
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
       'ColorPersistent', 'ColorSpecklePersistent', 'Round1', 'Round2',
       'OutlineWidth'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TNyaRoundRectangle, 'Nya Round Rectangle');
end.

