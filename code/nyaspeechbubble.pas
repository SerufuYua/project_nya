unit NyaSpeechBubble;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleUIControls, CastleClassUtils, CastleControls,
  CastleColors, CastleFonts, NyaRoundRectangle;

type
  TNyaSpeechBubble = class(TCastleUserInterface)
  protected
    FTime, FTimeAppear, FTimePerSymbol, FTimeLive, FTimeVanish: Single;
    RectangleBG: TNyaRoundRectangle;
    FGroup: TCastlePackedGroup;
    ActorName, TextMessage: TCastleLabel;
    FTransparency: Single;
    FColor: TCastleColorRGB;
    FColorPersistent: TCastleColorRGBPersistent;
    procedure SetColor(const value: TCastleColorRGB);
    function GetColorForPersistent: TCastleColorRGB;
    procedure SetColorForPersistent(const AValue: TCastleColorRGB);
    procedure SetTimePerSymbol(const value: Single);
    procedure SetRound(const value: Single);
    function GetRound: Single;
    procedure SetCustomFont(const value: TCastleAbstractFont);
    function GetCustomFont: TCastleAbstractFont;
    procedure SetTransparency(const value: Single);
    procedure ApplyTransparency;
  public
    const
      DefaultColor: TCastleColorRGB = (X: 0.6; Y: 0.0; Z: 0.5);
      DefaultRound = 15;
      DefaultTimeAppear = 0.125;
      DefaultTimePerSymbol = 0.25;
      DefaultTimeVanish = 0.25;
      DefaultPadding = 16;
      DefaultSpacing = 14;
      {$ifdef CASTLE_DESIGN_MODE}
      DefaultTransparency = 1.0;
      {$else}
      DefaultTransparency = 0.0;
      {$endif}

    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    property Color: TCastleColorRGB read FColor write SetColor;
  published
    property Transparency: Single read FTransparency write SetTransparency
             {$ifdef FPC}default DefaultTransparency{$endif};
    property Round: Single read GetRound write SetRound
             {$ifdef FPC}default DefaultRound{$endif};
    property TimeAppear: Single read FTimeAppear write FTimeAppear
             {$ifdef FPC}default DefaultTimeAppear{$endif};
    property TimePerSymbol: Single read FTimePerSymbol write SetTimePerSymbol
             {$ifdef FPC}default DefaultTimePerSymbol{$endif};
    property TimeVanish: Single read FTimeVanish write FTimeVanish
             {$ifdef FPC}default DefaultTimeVanish{$endif};
    property ColorPersistent: TCastleColorRGBPersistent read FColorPersistent;
    property CustomFont: TCastleAbstractFont read GetCustomFont write SetCustomFont;
  end;

implementation

uses
  CastleComponentSerialize, CastleUtils, CastleRectangles, CastleVectors;

constructor TNyaSpeechBubble.Create(AOwner: TComponent);
begin
  inherited;
  FTime:= 0.0;
  FTimeAppear:= DefaultTimeAppear;
  FTimePerSymbol:= DefaultTimePerSymbol;
  FTimeVanish:= DefaultTimeVanish;
  FTransparency:= DefaultTransparency;

  AutoSizeToChildren:= True;
  HorizontalAnchorParent:= THorizontalPosition.hpMiddle;
  HorizontalAnchorSelf:= THorizontalPosition.hpMiddle;
  VerticalAnchorParent:= TVerticalPosition.vpMiddle;
  VerticalAnchorSelf:= TVerticalPosition.vpMiddle;
  Translation:= Vector2(0.0, 0.0);

  RectangleBG:= TNyaRoundRectangle.Create(self);
  RectangleBG.SetTransient;
  RectangleBG.AutoSizeToChildren:= True;
  RectangleBG.Round:= DefaultRound;
  InsertFront(RectangleBG);

  FGroup:= TCastleVerticalGroup.Create(RectangleBG);
  FGroup.SetTransient;
  FGroup.Padding:= DefaultPadding + RectangleBG.Round / 2.0;
  FGroup.Spacing:= DefaultSpacing;
  RectangleBG.InsertFront(FGroup);

  ActorName:= TCastleLabel.Create(FGroup);
  ActorName.SetTransient;
  ActorName.Caption:= 'Actor:';
  FGroup.InsertFront(ActorName);

  TextMessage:= TCastleLabel.Create(FGroup);
  TextMessage.SetTransient;
  TextMessage.Caption:= 'Hello World!';
  FGroup.InsertFront(TextMessage);

  { Persistent for Color }
  FColorPersistent:= TCastleColorRGBPersistent.Create(nil);
  FColorPersistent.SetSubComponent(true);
  FColorPersistent.InternalGetValue:= {$ifdef FPC}@{$endif}GetColorForPersistent;
  FColorPersistent.InternalSetValue:= {$ifdef FPC}@{$endif}SetColorForPersistent;
  FColorPersistent.InternalDefaultValue:= Color;
  Color:= DefaultColor;

  ApplyTransparency;
end;

procedure TNyaSpeechBubble.Update(const SecondsPassed: Single; var HandleInput: boolean);
var
  ready, old, fin: Single;
begin
  inherited;

  {$ifndef CASTLE_DESIGN_MODE}
  FTime:= FTime + SecondsPassed;

  ready:= FTimeAppear;
  old:= FTimeAppear + FTimeLive;
  fin:= FTimeAppear + FTimeLive + FTimeVanish;


  if (FTime <= ready) then
  begin
    Transparency:= (FTime / ready);
  end
  else if ((FTime > ready) AND (FTime < old)) then
  begin
    Transparency:= 1.0;
  end
  else if ((FTime >= old) AND (FTime <= fin)) then
  begin
    Transparency:= ((FTimeVanish - (FTime - (old))) / FTimeVanish);
  end
  else if (FTime > fin) then
    Parent.RemoveControl(self);
  {$endif};
end;

procedure TNyaSpeechBubble.SetTimePerSymbol(const value: Single);
begin
  FTimeLive:= TextMessage.Caption.Length * FTimePerSymbol;
end;

procedure TNyaSpeechBubble.SetRound(const value: Single);
begin
  RectangleBG.Round:= value;
  FGroup.Padding:= DefaultPadding + RectangleBG.Round / 2.0;
end;

function TNyaSpeechBubble.GetRound: Single;
begin
  Result:= RectangleBG.Round;
end;

procedure TNyaSpeechBubble.SetCustomFont(const value: TCastleAbstractFont);
begin
  ActorName.CustomFont:= value;
  TextMessage.CustomFont:= value;
end;

function TNyaSpeechBubble.GetCustomFont: TCastleAbstractFont;
begin
  Result:= TextMessage.CustomFont;
end;

procedure TNyaSpeechBubble.SetColor(const value: TCastleColorRGB);
const
  base = 0.9;
  fill = 1.0 - base;
var
  alpha: Single;
begin
  FColor:= value;

  alpha:= RectangleBG.Color.W;
  RectangleBG.Color:= Vector4(FColor, alpha);

  alpha:= ActorName.Color.W;
  ActorName.Color:= Vector4(base + FColor.X * fill,
                            base + FColor.Y * fill,
                            base + FColor.Z * fill,
                            alpha);

  alpha:= TextMessage.Color.W;
  TextMessage.Color:= Vector4(base + FColor.X * fill,
                              base + FColor.Y * fill,
                              base + FColor.Z * fill,
                              alpha);
end;

procedure TNyaSpeechBubble.SetTransparency(const value: Single);
begin
  if (FTransparency = value) then Exit;
  FTransparency:= value;

  ApplyTransparency;
end;

procedure TNyaSpeechBubble.ApplyTransparency;
var
  c: TCastleColor;
begin
  c:= RectangleBG.Color;
  c.W:= FTransparency * 0.5;
  RectangleBG.Color:= c;

  c:= ActorName.Color;
  c.W:= FTransparency;
  ActorName.Color:= c;

  c:= TextMessage.Color;
  c.W:= FTransparency;
  TextMessage.Color:= c;
end;

function TNyaSpeechBubble.GetColorForPersistent: TCastleColorRGB;
begin
  Result:= Color;
end;

procedure TNyaSpeechBubble.SetColorForPersistent(const AValue: TCastleColorRGB);
begin
  Color:= AValue;
end;

function TNyaSpeechBubble.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'ColorPersistent', 'CustomFont', 'Transparency', 'TimeAppear',
       'TimePerSymbol', 'TimeVanish', 'Round'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TNyaSpeechBubble, 'Nya Speech Bubble');
end.

