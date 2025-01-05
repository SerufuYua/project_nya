unit NyaLoadingBar;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleUIControls, CastleControls, CastleRectangles,
  CastleColors, CastleClassUtils, CastleVectors;

type
  TBarDir = (LeftToRight, RightToLeft, BottomToTop, TopToBottom);

  TNyaLoadingBar = class(TCastleUserInterface)
  protected
    FRectBGLow: TFloatRectangle;
    FRectBGMid: TFloatRectangle;
    FRectBGHi: TFloatRectangle;
    FRectGauge: TFloatRectangle;
    FPointsBGLow: array of TVector2;
    FPointsBGHi: array of TVector2;
    FPointsGauge: array of TVector2;
    procedure CalcPoints;
  protected
    FDirection: TBarDir;
    FGaugeValue: Single;
    FThresholdMid: Single;
    FThresholdHi: Single;
    FCornerCut: Single;
    FColorBGLow: TCastleColor;
    FColorBGMid: TCastleColor;
    FColorBGHi: TCastleColor;
    FColorGauge: TCastleColor;
    FColorBGLowPersistent: TCastleColorPersistent;
    FColorBGMidPersistent: TCastleColorPersistent;
    FColorBGHiPersistent: TCastleColorPersistent;
    FColorGaugePersistent: TCastleColorPersistent;
    function GetColorBGLowForPersistent: TCastleColor;
    procedure SetColorBGLowForPersistent(const AValue: TCastleColor);
    function GetColorBGMidForPersistent: TCastleColor;
    procedure SetColorBGMidForPersistent(const AValue: TCastleColor);
    function GetColorBGHiForPersistent: TCastleColor;
    procedure SetColorBGHiForPersistent(const AValue: TCastleColor);
    function GetColorGaugeForPersistent: TCastleColor;
    procedure SetColorGaugeForPersistent(const AValue: TCastleColor);
    procedure SetGaugeValue(const value: Single);
    procedure SetThresholdMid(const value: Single);
    procedure SetThresholdHid(const value: Single);
  public
    const
      DefaultDirection = TBarDir.LeftToRight;
      DefaultGaugeValue = 0.2;
      DefaultThresholdMid = 0.6;
      DefaultThresholdHi = 0.9;
      DefaultCornerCut = 0.2;
      DefaultColorBGLow: TCastleColor = (X: 0.0; Y: 1.0; Z: 0.0; W: 1.0);
      DefaultColorBGMid: TCastleColor = (X: 1.0; Y: 1.0; Z: 0.0; W: 1.0);
      DefaultColorBGHi: TCastleColor =  (X: 1.0; Y: 0.0; Z: 0.0; W: 1.0);
      DefaultColorGauge: TCastleColor = (X: 0.0; Y: 0.0; Z: 0.5; W: 0.75);

    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single;
                     var HandleInput: boolean); override;
    procedure Render; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    property ColorBGLow: TCastleColor read FColorBGLow write FColorBGLow;
    property ColorBGMid: TCastleColor read FColorBGMid write FColorBGMid;
    property ColorBGHi: TCastleColor read FColorBGHi write FColorBGHi;
    property ColorGauge: TCastleColor read FColorGauge write FColorGauge;
  published
    property Direction: TBarDir read FDirection write FDirection
           {$ifdef FPC}default DefaultDirection{$endif};
    property GaugeValue: Single read FGaugeValue write SetGaugeValue
           {$ifdef FPC}default DefaultGaugeValue{$endif};
    property ThresholdMid: Single read FThresholdMid write SetThresholdMid
           {$ifdef FPC}default DefaultThresholdMid{$endif};
    property ThresholdHi: Single read FThresholdHi write SetThresholdHid
           {$ifdef FPC}default DefaultThresholdHi{$endif};
    property CornerCut: Single read FCornerCut write FCornerCut
           {$ifdef FPC}default DefaultCornerCut{$endif};
    property ColorBGLowPersistent: TCastleColorPersistent read FColorBGLowPersistent;
    property ColorBGMidPersistent: TCastleColorPersistent read FColorBGMidPersistent;
    property ColorBGHiPersistent: TCastleColorPersistent read FColorBGHiPersistent;
    property ColorGaugePersistent: TCastleColorPersistent read FColorGaugePersistent;
  end;

implementation

uses
  CastleComponentSerialize, CastleGLUtils, CastleUtils, Math;

constructor TNyaLoadingBar.Create(AOwner: TComponent);
begin
  inherited;

  FDirection:= DefaultDirection;
  FGaugeValue:= DefaultGaugeValue;
  FThresholdMid:= DefaultThresholdMid;
  FThresholdHi:= DefaultThresholdHi;
  FCornerCut:= DefaultCornerCut;

  FColorBGLow:= DefaultColorBGLow;
  FColorBGMid:= DefaultColorBGMid;
  FColorBGHi:= DefaultColorBGHi;
  FColorGauge:= DefaultColorGauge;

  { Persistent for ColorBGLow }
  FColorBGLowPersistent:= TCastleColorPersistent.Create(nil);
  FColorBGLowPersistent.SetSubComponent(true);
  FColorBGLowPersistent.InternalGetValue:= {$ifdef FPC}@{$endif}GetColorBGLowForPersistent;
  FColorBGLowPersistent.InternalSetValue:= {$ifdef FPC}@{$endif}SetColorBGLowForPersistent;
  FColorBGLowPersistent.InternalDefaultValue:= ColorBGLow; // current value is default

  { Persistent for ColorBGMid }
  FColorBGMidPersistent:= TCastleColorPersistent.Create(nil);
  FColorBGMidPersistent.SetSubComponent(true);
  FColorBGMidPersistent.InternalGetValue:= {$ifdef FPC}@{$endif}GetColorBGMidForPersistent;
  FColorBGMidPersistent.InternalSetValue:= {$ifdef FPC}@{$endif}SetColorBGMidForPersistent;
  FColorBGMidPersistent.InternalDefaultValue:= ColorBGMid; // current value is default

  { Persistent for ColorBGHi }
  FColorBGHiPersistent:= TCastleColorPersistent.Create(nil);
  FColorBGHiPersistent.SetSubComponent(true);
  FColorBGHiPersistent.InternalGetValue:= {$ifdef FPC}@{$endif}GetColorBGHiForPersistent;
  FColorBGHiPersistent.InternalSetValue:= {$ifdef FPC}@{$endif}SetColorBGHiForPersistent;
  FColorBGHiPersistent.InternalDefaultValue:= ColorBGHi; // current value is default

  { Persistent for ColorGaguge }
  FColorGaugePersistent:= TCastleColorPersistent.Create(nil);
  FColorGaugePersistent.SetSubComponent(true);
  FColorGaugePersistent.InternalGetValue:= {$ifdef FPC}@{$endif}GetColorGaugeForPersistent;
  FColorGaugePersistent.InternalSetValue:= {$ifdef FPC}@{$endif}SetColorGaugeForPersistent;
  FColorGaugePersistent.InternalDefaultValue:= ColorGauge; // current value is default

  { create points }
  SetLength(FPointsGauge, 8);
  SetLength(FPointsBGLow, 6);
  SetLength(FPointsBGHi, 6);
end;

procedure TNyaLoadingBar.Update(const SecondsPassed: Single;
                                var HandleInput: boolean);
var
  Indent: Single;
begin
  inherited;
  Indent:= FCornerCut * Min(RenderRect.Height, RenderRect.Width);

  if (FDirection = TBarDir.LeftToRight) then
  begin
    FRectBGLow.Left:= RenderRect.Left;
    FRectBGLow.Bottom:= RenderRect.Bottom;
    FRectBGLow.Width:= RenderRect.Width * FThresholdMid;
    FRectBGLow.Height:= RenderRect.Height;

    FRectBGMid.Left:= RenderRect.Left + RenderRect.Width * FThresholdMid;
    FRectBGMid.Bottom:= RenderRect.Bottom;
    FRectBGMid.Width:= RenderRect.Width * (FThresholdHi - FThresholdMid);
    FRectBGMid.Height:= RenderRect.Height;

    FRectBGHi.Left:= RenderRect.Left + RenderRect.Width * FThresholdHi;
    FRectBGHi.Bottom:= RenderRect.Bottom;
    FRectBGHi.Width:= RenderRect.Width * (1.0 - FThresholdHi);
    FRectBGHi.Height:= RenderRect.Height;

    FRectGauge.Left:= RenderRect.Left;
    FRectGauge.Bottom:= RenderRect.Bottom;
    FRectGauge.Width:= Max(RenderRect.Width * FGaugeValue, Indent * 2);
    FRectGauge.Height:= RenderRect.Height;
  end
  else if (FDirection = TBarDir.RightToLeft) then
  begin
    FRectBGLow.Left:= RenderRect.Left + RenderRect.Width * (1.0 - FThresholdMid);
    FRectBGLow.Bottom:= RenderRect.Bottom;
    FRectBGLow.Width:= RenderRect.Width * FThresholdMid;
    FRectBGLow.Height:= RenderRect.Height;

    FRectBGMid.Left:= RenderRect.Left + RenderRect.Width * (1.0 - FThresholdHi);
    FRectBGMid.Bottom:= RenderRect.Bottom;
    FRectBGMid.Width:= RenderRect.Width * (FThresholdHi - FThresholdMid);
    FRectBGMid.Height:= RenderRect.Height;

    FRectBGHi.Left:= RenderRect.Left;
    FRectBGHi.Bottom:= RenderRect.Bottom;
    FRectBGHi.Width:= RenderRect.Width * (1.0 - FThresholdHi);
    FRectBGHi.Height:= RenderRect.Height;

    FRectGauge.Bottom:= RenderRect.Bottom;
    FRectGauge.Width:= Max(RenderRect.Width * FGaugeValue, Indent * 2);;
    FRectGauge.Left:= RenderRect.Left + RenderRect.Width - FRectGauge.Width;
    FRectGauge.Height:= RenderRect.Height;
  end
  else if (FDirection = TBarDir.BottomToTop) then
  begin
    FRectBGLow.Left:= RenderRect.Left;
    FRectBGLow.Bottom:= RenderRect.Bottom;
    FRectBGLow.Width:= RenderRect.Width;
    FRectBGLow.Height:= RenderRect.Height * FThresholdMid;

    FRectBGMid.Left:= RenderRect.Left;
    FRectBGMid.Bottom:= RenderRect.Bottom + RenderRect.Height * FThresholdMid;
    FRectBGMid.Width:= RenderRect.Width;
    FRectBGMid.Height:= RenderRect.Height * (FThresholdHi - FThresholdMid);

    FRectBGHi.Left:= RenderRect.Left;
    FRectBGHi.Bottom:= RenderRect.Bottom + RenderRect.Height * FThresholdHi;
    FRectBGHi.Width:= RenderRect.Width;
    FRectBGHi.Height:= RenderRect.Height * (1.0 - FThresholdHi);

    FRectGauge.Left:= RenderRect.Left;
    FRectGauge.Bottom:= RenderRect.Bottom;
    FRectGauge.Width:= RenderRect.Width;
    FRectGauge.Height:= Max(RenderRect.Height * FGaugeValue, Indent * 2);
  end
  else if (FDirection = TBarDir.TopToBottom) then
  begin
    FRectBGLow.Left:= RenderRect.Left;
    FRectBGLow.Bottom:= RenderRect.Bottom + RenderRect.Height * (1.0 - FThresholdMid);
    FRectBGLow.Width:= RenderRect.Width;
    FRectBGLow.Height:= RenderRect.Height * FThresholdMid;

    FRectBGMid.Left:= RenderRect.Left;
    FRectBGMid.Bottom:= RenderRect.Bottom + RenderRect.Height * (1.0 - FThresholdHi);
    FRectBGMid.Width:= RenderRect.Width;
    FRectBGMid.Height:= RenderRect.Height * (FThresholdHi - FThresholdMid);

    FRectBGHi.Left:= RenderRect.Left;
    FRectBGHi.Bottom:= RenderRect.Bottom;
    FRectBGHi.Width:= RenderRect.Width;
    FRectBGHi.Height:= RenderRect.Height * (1.0 - FThresholdHi);

    FRectGauge.Left:= RenderRect.Left;
    FRectGauge.Width:= RenderRect.Width;
    FRectGauge.Height:= Max(RenderRect.Height * FGaugeValue, Indent * 2);
    FRectGauge.Bottom:= RenderRect.Bottom + RenderRect.Height - FRectGauge.Height;
  end;

  CalcPoints;
end;

procedure TNyaLoadingBar.Render;
begin
  inherited;
  DrawPrimitive2D(TPrimitiveMode.pmTriangleFan, FPointsBGLow, FColorBGLow);
  DrawRectangle(FRectBGMid, FColorBGMid);
  DrawPrimitive2D(TPrimitiveMode.pmTriangleFan, FPointsBGHi, FColorBGHi);
  DrawPrimitive2D(TPrimitiveMode.pmTriangleFan, FPointsGauge, FColorGauge);
end;

procedure TNyaLoadingBar.CalcPoints;
var
  Indent: Single;
begin
  Indent:= FCornerCut * Min(RenderRect.Height, RenderRect.Width);

  { BGLow }
  if (FDirection = TBarDir.LeftToRight) then
  begin
    FPointsBGLow[0].X:= FRectBGLow.Left;
    FPointsBGLow[0].Y:= FRectBGLow.Bottom + Indent;
    FPointsBGLow[1].X:= FRectBGLow.Left + Indent;
    FPointsBGLow[1].Y:= FRectBGLow.Bottom;
    FPointsBGLow[2].X:= FRectBGLow.Right;
    FPointsBGLow[2].Y:= FRectBGLow.Bottom;
    FPointsBGLow[3].X:= FRectBGLow.Right;
    FPointsBGLow[3].Y:= FRectBGLow.Top;
    FPointsBGLow[4].X:= FRectBGLow.Left + Indent;
    FPointsBGLow[4].Y:= FRectBGLow.Top;
    FPointsBGLow[5].X:= FRectBGLow.Left;
    FPointsBGLow[5].Y:= FRectBGLow.Top - Indent;

    FPointsBGHi[0].X:= FRectBGHi.Right;
    FPointsBGHi[0].Y:= FRectBGHi.Top - Indent;
    FPointsBGHi[1].X:= FRectBGHi.Right - Indent;
    FPointsBGHi[1].Y:= FRectBGHi.Top;
    FPointsBGHi[2].X:= FRectBGHi.Left;
    FPointsBGHi[2].Y:= FRectBGHi.Top;
    FPointsBGHi[3].X:= FRectBGHi.Left;
    FPointsBGHi[3].Y:= FRectBGHi.Bottom;
    FPointsBGHi[4].X:= FRectBGHi.Right - Indent;
    FPointsBGHi[4].Y:= FRectBGHi.Bottom;
    FPointsBGHi[5].X:= FRectBGHi.Right;
    FPointsBGHi[5].Y:= FRectBGHi.Bottom + Indent;
end
  else if (FDirection = TBarDir.RightToLeft) then
  begin
    FPointsBGLow[0].X:= FRectBGLow.Right;
    FPointsBGLow[0].Y:= FRectBGLow.Top - Indent;
    FPointsBGLow[1].X:= FRectBGLow.Right - Indent;
    FPointsBGLow[1].Y:= FRectBGLow.Top;
    FPointsBGLow[2].X:= FRectBGLow.Left;
    FPointsBGLow[2].Y:= FRectBGLow.Top;
    FPointsBGLow[3].X:= FRectBGLow.Left;
    FPointsBGLow[3].Y:= FRectBGLow.Bottom;
    FPointsBGLow[4].X:= FRectBGLow.Right - Indent;
    FPointsBGLow[4].Y:= FRectBGLow.Bottom;
    FPointsBGLow[5].X:= FRectBGLow.Right;
    FPointsBGLow[5].Y:= FRectBGLow.Bottom + Indent;

    FPointsBGHi[0].X:= FRectBGHi.Left;
    FPointsBGHi[0].Y:= FRectBGHi.Bottom + Indent;
    FPointsBGHi[1].X:= FRectBGHi.Left + Indent;
    FPointsBGHi[1].Y:= FRectBGHi.Bottom;
    FPointsBGHi[2].X:= FRectBGHi.Right;
    FPointsBGHi[2].Y:= FRectBGHi.Bottom;
    FPointsBGHi[3].X:= FRectBGHi.Right;
    FPointsBGHi[3].Y:= FRectBGHi.Top;
    FPointsBGHi[4].X:= FRectBGHi.Left + Indent;
    FPointsBGHi[4].Y:= FRectBGHi.Top;
    FPointsBGHi[5].X:= FRectBGHi.Left;
    FPointsBGHi[5].Y:= FRectBGHi.Top - Indent;
end
  else if (FDirection = TBarDir.BottomToTop) then
  begin
    FPointsBGLow[0].X:= FRectBGLow.Right - Indent;
    FPointsBGLow[0].Y:= FRectBGLow.Bottom;
    FPointsBGLow[1].X:= FRectBGLow.Right;
    FPointsBGLow[1].Y:= FRectBGLow.Bottom + Indent;
    FPointsBGLow[2].X:= FRectBGLow.Right;
    FPointsBGLow[2].Y:= FRectBGLow.Top;
    FPointsBGLow[3].X:= FRectBGLow.Left;
    FPointsBGLow[3].Y:= FRectBGLow.Top;
    FPointsBGLow[4].X:= FRectBGLow.Left;
    FPointsBGLow[4].Y:= FRectBGLow.Bottom + Indent;
    FPointsBGLow[5].X:= FRectBGLow.Left + Indent;
    FPointsBGLow[5].Y:= FRectBGLow.Bottom;

    FPointsBGHi[0].X:= FRectBGHi.Left + Indent;
    FPointsBGHi[0].Y:= FRectBGHi.Top;
    FPointsBGHi[1].X:= FRectBGHi.Left;
    FPointsBGHi[1].Y:= FRectBGHi.Top - Indent;
    FPointsBGHi[2].X:= FRectBGHi.Left;
    FPointsBGHi[2].Y:= FRectBGHi.Bottom;
    FPointsBGHi[3].X:= FRectBGHi.Right;
    FPointsBGHi[3].Y:= FRectBGHi.Bottom;
    FPointsBGHi[4].X:= FRectBGHi.Right;
    FPointsBGHi[4].Y:= FRectBGHi.Top - Indent;
    FPointsBGHi[5].X:= FRectBGHi.Right - Indent;
    FPointsBGHi[5].Y:= FRectBGHi.Top;
  end
  else if (FDirection = TBarDir.TopToBottom) then
  begin
    FPointsBGLow[0].X:= FRectBGLow.Left + Indent;
    FPointsBGLow[0].Y:= FRectBGLow.Top;
    FPointsBGLow[1].X:= FRectBGLow.Left;
    FPointsBGLow[1].Y:= FRectBGLow.Top - Indent;
    FPointsBGLow[2].X:= FRectBGLow.Left;
    FPointsBGLow[2].Y:= FRectBGLow.Bottom;
    FPointsBGLow[3].X:= FRectBGLow.Right;
    FPointsBGLow[3].Y:= FRectBGLow.Bottom;
    FPointsBGLow[4].X:= FRectBGLow.Right;
    FPointsBGLow[4].Y:= FRectBGLow.Top - Indent;
    FPointsBGLow[5].X:= FRectBGLow.Right - Indent;
    FPointsBGLow[5].Y:= FRectBGLow.Top;

    FPointsBGHi[0].X:= FRectBGHi.Right - Indent;
    FPointsBGHi[0].Y:= FRectBGHi.Bottom;
    FPointsBGHi[1].X:= FRectBGHi.Right;
    FPointsBGHi[1].Y:= FRectBGHi.Bottom + Indent;
    FPointsBGHi[2].X:= FRectBGHi.Right;
    FPointsBGHi[2].Y:= FRectBGHi.Top;
    FPointsBGHi[3].X:= FRectBGHi.Left;
    FPointsBGHi[3].Y:= FRectBGHi.Top;
    FPointsBGHi[4].X:= FRectBGHi.Left;
    FPointsBGHi[4].Y:= FRectBGHi.Bottom + Indent;
    FPointsBGHi[5].X:= FRectBGHi.Left + Indent;
    FPointsBGHi[5].Y:= FRectBGHi.Bottom;
  end;

  { gauge }
  FPointsGauge[0].X:= FRectGauge.Left;
  FPointsGauge[0].Y:= FRectGauge.Bottom + Indent;
  FPointsGauge[1].X:= FRectGauge.Left + Indent;
  FPointsGauge[1].Y:= FRectGauge.Bottom;
  FPointsGauge[2].X:= FRectGauge.Right - Indent;
  FPointsGauge[2].Y:= FRectGauge.Bottom;
  FPointsGauge[3].X:= FRectGauge.Right;
  FPointsGauge[3].Y:= FRectGauge.Bottom + Indent;
  FPointsGauge[4].X:= FRectGauge.Right;
  FPointsGauge[4].Y:= FRectGauge.Top - Indent;
  FPointsGauge[5].X:= FRectGauge.Right - Indent;
  FPointsGauge[5].Y:= FRectGauge.Top;
  FPointsGauge[6].X:= FRectGauge.Left + Indent;
  FPointsGauge[6].Y:= FRectGauge.Top;
  FPointsGauge[7].X:= FRectGauge.Left;
  FPointsGauge[7].Y:= FRectGauge.Top - Indent;
end;

procedure TNyaLoadingBar.SetGaugeValue(const value: Single);
begin
  if (FGaugeValue = value) then exit;
  FGaugeValue:= Clamped(value, 0.0, 1.0);
end;

procedure TNyaLoadingBar.SetThresholdMid(const value: Single);
begin
  if (FThresholdMid = value) then exit;
  FThresholdMid:= Clamped(value, 0.0, ThresholdHi);
end;

procedure TNyaLoadingBar.SetThresholdHid(const value: Single);
begin
  if (FThresholdHi = value) then exit;
  FThresholdHi:= Clamped(value, ThresholdMid, 1.0);
end;

function TNyaLoadingBar.GetColorBGLowForPersistent: TCastleColor;
begin
  Result:= ColorBGLow;
end;

procedure TNyaLoadingBar.SetColorBGLowForPersistent(const AValue: TCastleColor);
begin
  ColorBGLow:= AValue;
end;

function TNyaLoadingBar.GetColorBGMidForPersistent: TCastleColor;
begin
  Result:= ColorBGMid;
end;

procedure TNyaLoadingBar.SetColorBGMidForPersistent(const AValue: TCastleColor);
begin
  ColorBGMid:= AValue;
end;

function TNyaLoadingBar.GetColorBGHiForPersistent: TCastleColor;
begin
  Result:= ColorBGHi;
end;

procedure TNyaLoadingBar.SetColorBGHiForPersistent(const AValue: TCastleColor);
begin
  ColorBGHi:= AValue;
end;

function TNyaLoadingBar.GetColorGaugeForPersistent: TCastleColor;
begin
  Result:= ColorGauge;
end;

procedure TNyaLoadingBar.SetColorGaugeForPersistent(const AValue: TCastleColor);
begin
  ColorGauge:= AValue;
end;

function TNyaLoadingBar.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'GaugeValue', 'ThresholdMid', 'ThresholdHi', 'ColorBGLowPersistent',
       'ColorBGMidPersistent', 'ColorBGHiPersistent', 'ColorGaugePersistent',
       'Direction', 'CornerCut'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TNyaLoadingBar, 'Nya Loading Bar');
end.


