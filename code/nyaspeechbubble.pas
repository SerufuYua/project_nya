unit NyaSpeechBubble;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleUIControls, CastleClassUtils, CastleControls,
  CastleColors, CastleFonts, NyaRoundRectangle, CastleRectangles,
  NyaActor;

type
  TNyaSpeechBubble = class(TCastleUserInterface)
  protected
    FTime, FTimeAppear, FTimePerSymbol, FTimeLive, FTimeVanish: Single;
    RectangleBG: TNyaRoundRectangle;
    FGroup: TCastlePackedGroup;
    TextActorName, TextMessage: TCastleLabel;
    FActorName, FMessage: String;
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
    procedure SetActorName(const value: String);
    procedure SetMessage(const value: String);
    function GetOutlineWidth: Single;
    procedure SetOutlineWidth(const value: Single);
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
      DefaultActorName = 'Actor';
      DefaultMessage = 'Hello World!';
      DefaultOutlineWidth = 2.0;
      {$ifdef CASTLE_DESIGN_MODE}
      DefaultTransparency = 1.0;
      {$else}
      DefaultTransparency = 0.0;
      {$endif}

    constructor Create(AOwner: TComponent; AActror: TNyaActor; AMsg: String;
                       AFont: TCastleAbstractFont;
                       ATimePerSymbol: Single = DefaultTimePerSymbol);
    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    property Color: TCastleColorRGB read FColor write SetColor;
  published
    property ActorName: String read FActorName write SetActorName;
    property Message: String read FMessage write SetMessage;
    property Transparency: Single read FTransparency write SetTransparency
             {$ifdef FPC}default DefaultTransparency{$endif};
    property Round: Single read GetRound write SetRound
             {$ifdef FPC}default DefaultRound{$endif};
    property OutlineWidth: Single read GetOutlineWidth write SetOutlineWidth
             {$ifdef FPC}default DefaultOutlineWidth{$endif};
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
  CastleComponentSerialize, CastleUtils, CastleVectors;

constructor TNyaSpeechBubble.Create(AOwner: TComponent;
                                    AActror: TNyaActor; AMsg: String;
                                    AFont: TCastleAbstractFont;
                                    ATimePerSymbol: Single);
var
  areaRect: TFloatRectangle;
begin
  Create(AOwner);

  if Assigned(AFont) then
    CustomFont:= AFont;

  Color:= AActror.PersonalColor;

  TextActorName.Caption:= AActror.ActorName + ':';
  TextMessage.Caption:= AMsg;

  if (AOwner is TCastleUserInterface) then
  begin
    areaRect:= (AOwner as TCastleUserInterface).RenderRect;
    Translation:= Vector2(RandomFloatRange(areaRect.Left,   areaRect.Right),
                          RandomFloatRange(areaRect.Bottom, areaRect.Top));
  end;

  TimePerSymbol:= ATimePerSymbol;
end;

constructor TNyaSpeechBubble.Create(AOwner: TComponent);
begin
  inherited;
  FTime:= 0.0;
  FTimeAppear:= DefaultTimeAppear;
  FTimePerSymbol:= DefaultTimePerSymbol;
  FTimeVanish:= DefaultTimeVanish;
  FTransparency:= DefaultTransparency;
  FActorName:= DefaultActorName;
  FMessage:= DefaultMessage;
  AutoSizeToChildren:= True;

  RectangleBG:= TNyaRoundRectangle.Create(self);
  RectangleBG.SetTransient;
  RectangleBG.AutoSizeToChildren:= True;
  RectangleBG.Round:= DefaultRound;
  RectangleBG.OutlineWidth:= DefaultOutlineWidth;
  InsertFront(RectangleBG);

  FGroup:= TCastleVerticalGroup.Create(RectangleBG);
  FGroup.SetTransient;
  FGroup.Padding:= DefaultPadding + RectangleBG.Round / 2.0;
  FGroup.Spacing:= DefaultSpacing;
  RectangleBG.InsertFront(FGroup);

  TextActorName:= TCastleLabel.Create(FGroup);
  TextActorName.SetTransient;
  TextActorName.Html:= True;
  TextActorName.Caption:= FActorName + ':';
  FGroup.InsertFront(TextActorName);

  TextMessage:= TCastleLabel.Create(FGroup);
  TextMessage.SetTransient;
  TextMessage.Html:= True;
  TextMessage.Caption:= FMessage;
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
  TextActorName.CustomFont:= value;
  TextMessage.CustomFont:= value;
end;

function TNyaSpeechBubble.GetCustomFont: TCastleAbstractFont;
begin
  Result:= TextMessage.CustomFont;
end;

procedure TNyaSpeechBubble.SetColor(const value: TCastleColorRGB);
const
  tbase = 0.9;
  tfill = 1.0 - tbase;
  sbase = 0.3;
  sfill = 1.0 - sbase;
var
  alpha: Single;
begin
  FColor:= value;

  alpha:= RectangleBG.Color.W;
  RectangleBG.Color:= Vector4(FColor, alpha);

  alpha:= RectangleBG.ColorSpeckle.W;
  RectangleBG.ColorSpeckle:= Vector4(sbase + FColor.X * sfill,
                                     sbase + FColor.Y * sfill,
                                     sbase + FColor.Z * sfill,
                                     alpha);

  alpha:= TextActorName.Color.W;
  TextActorName.Color:= Vector4(tbase + FColor.X * tfill,
                                tbase + FColor.Y * tfill,
                                tbase + FColor.Z * tfill,
                                alpha);

  alpha:= TextMessage.Color.W;
  TextMessage.Color:= Vector4(tbase + FColor.X * tfill,
                              tbase + FColor.Y * tfill,
                              tbase + FColor.Z * tfill,
                              alpha);
end;

procedure TNyaSpeechBubble.SetTransparency(const value: Single);
begin
  if (FTransparency = value) then Exit;
  FTransparency:= value;

  ApplyTransparency;
end;

procedure TNyaSpeechBubble.SetActorName(const value: String);
begin
  if (FActorName = value) then Exit;

  FActorName:= value;
  TextActorName.Caption:= value + ':';
end;

procedure TNyaSpeechBubble.SetMessage(const value: String);
begin
  if (FMessage = value) then Exit;

  FMessage:= value;
  TextMessage.Caption:= value;
end;

procedure TNyaSpeechBubble.SetOutlineWidth(const value: Single);
begin
  RectangleBG.OutlineWidth:= value;
end;

function TNyaSpeechBubble.GetOutlineWidth: Single;
begin
  Result:= RectangleBG.OutlineWidth;
end;

procedure TNyaSpeechBubble.ApplyTransparency;
var
  c: TCastleColor;
begin
  c:= RectangleBG.Color;
  c.W:= FTransparency * 0.5;
  RectangleBG.Color:= c;

  c:= RectangleBG.ColorSpeckle;
  c.W:= FTransparency * 0.65;
  RectangleBG.ColorSpeckle:= c;

  c:= TextActorName.Color;
  c.W:= FTransparency;
  TextActorName.Color:= c;

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
       'TimePerSymbol', 'TimeVanish', 'Round', 'ActorName', 'Message',
       'OutlineWidth'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TNyaSpeechBubble, 'Nya Speech Bubble');
end.

