unit NyaLoadingBar;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleUIControls, CastleControls, CastleRectangles,
  CastleColors, CastleClassUtils;

type
  TBarDir = (LeftToRight, RightToLeft, BottomToTop, TopToBottom);

  TNyaLoadingBar = class(TCastleUserInterface)
  protected
    FRectBGLow: TFloatRectangle;
    FRectBGMid: TFloatRectangle;
    FRectBGHi: TFloatRectangle;
    FRectGauge: TFloatRectangle;
  protected
    FDirection: TBarDir;
    FThresholdMid: Single;
    FThresholdHi: Single;
    FGaugeValue: Single;
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
    property ColorBGLowPersistent: TCastleColorPersistent read FColorBGLowPersistent;
    property ColorBGMidPersistent: TCastleColorPersistent read FColorBGMidPersistent;
    property ColorBGHiPersistent: TCastleColorPersistent read FColorBGHiPersistent;
    property ColorGaugePersistent: TCastleColorPersistent read FColorGaugePersistent;
  end;

implementation

uses
  CastleComponentSerialize, CastleGLUtils, CastleVectors,
  CastleUtils;

constructor TNyaLoadingBar.Create(AOwner: TComponent);
begin
  inherited;

  FDirection:= DefaultDirection;
  FGaugeValue:= DefaultGaugeValue;
  FThresholdMid:= DefaultThresholdMid;
  FThresholdHi:= DefaultThresholdHi;

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
end;

procedure TNyaLoadingBar.Update(const SecondsPassed: Single;
                                var HandleInput: boolean);
begin
  inherited;

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
    FRectGauge.Width:= RenderRect.Width * FGaugeValue;
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

    FRectGauge.Left:= RenderRect.Left + RenderRect.Width * (1.0 - FGaugeValue);
    FRectGauge.Bottom:= RenderRect.Bottom;
    FRectGauge.Width:= RenderRect.Width * FGaugeValue;
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
    FRectGauge.Height:= RenderRect.Height * FGaugeValue;
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
    FRectGauge.Bottom:= RenderRect.Bottom + RenderRect.Height * (1.0 - FGaugeValue);
    FRectGauge.Width:= RenderRect.Width;
    FRectGauge.Height:= RenderRect.Height * FGaugeValue;
  end;
end;

procedure TNyaLoadingBar.Render;
begin
  inherited;
  DrawRectangle(FRectBGLow, FColorBGLow);
  DrawRectangle(FRectBGMid, FColorBGMid);
  DrawRectangle(FRectBGHi, FColorBGHi);
  DrawRectangle(FRectGauge, FColorGauge);
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
       'Direction'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TNyaLoadingBar, 'Nya Loading Bar');
end.

