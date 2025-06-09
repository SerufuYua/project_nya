unit NyaSpeechBubble;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleUIControls, CastleClassUtils, CastleControls,
  CastleColors, CastleFonts, NyaRoundRectangle, CastleRectangles, CastleVectors,
  CastleTransform, CastleViewport, CastleBoxes,
  NyaActor;

type
  TNyaSpeechBubble = class(TCastleUserInterface)
  protected
    FTime, FTimeAppear, FTimePerSymbol, FTimeLive, FTimeVanish: Single;
    FAllowedArea: Single;
    RectangleBG: TNyaRoundRectangle;
    FGroup: TCastlePackedGroup;
    TextActorName, TextMessage: TCastleLabel;
    FActorName, FMessage: String;
    FTransparency: Single;
    FViewport: TCastleViewport;
    FColor: TCastleColorRGB;
    FColorPersistent: TCastleColorRGBPersistent;
    FPointInWorld: TVector3;
    FPointInWorldPersistent: TCastleVector3Persistent;
    function GetPointInWorldForPersistent: TVector3;
    procedure SetPointInWorldForPersistent(const AValue: TVector3);
    procedure SetColor(const value: TCastleColorRGB);
    function GetColorForPersistent: TCastleColorRGB;
    procedure SetColorForPersistent(const AValue: TCastleColorRGB);
    procedure SetTimePerSymbol(const value: Single);
    procedure SetRound1(const value: Single);
    function GetRound1: Single;
    procedure SetRound2(const value: Single);
    function GetRound2: Single;
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
      DefaultPointInWorld: TVector3 = (X: 0.0; Y: 0.0; Z: 0.0);
      DefaultRound1 = 15;
      DefaultRound2 = 8;
      DefaultTimeAppear = 0.125;
      DefaultTimePerSymbol = 0.25;
      DefaultTimeVanish = 0.25;
      DefaultAllowedArea = 0.975;
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

    constructor Create(AOwner: TComponent; AArea: TBox3D;
                       AActror: TNyaActor; const AMsg: String;
                       AFont: TCastleAbstractFont;
                       ATimePerSymbol: Single = DefaultTimePerSymbol);
    constructor Create(AOwner: TComponent; AActror: TNyaActor;
                       const AMsg: String; AFont: TCastleAbstractFont;
                       ATimePerSymbol: Single = DefaultTimePerSymbol);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    property Color: TCastleColorRGB read FColor write SetColor;
    property PointInWorld: TVector3 read FPointInWorld write FPointInWorld;
  published
    property ActorName: String read FActorName write SetActorName;
    property Message: String read FMessage write SetMessage;
    property Transparency: Single read FTransparency write SetTransparency
             {$ifdef FPC}default DefaultTransparency{$endif};
    property Round1: Single read GetRound1 write SetRound1
             {$ifdef FPC}default DefaultRound1{$endif};
    property Round2: Single read GetRound2 write SetRound2
             {$ifdef FPC}default DefaultRound2{$endif};
    property OutlineWidth: Single read GetOutlineWidth write SetOutlineWidth
             {$ifdef FPC}default DefaultOutlineWidth{$endif};
    property TimeAppear: Single read FTimeAppear write FTimeAppear
             {$ifdef FPC}default DefaultTimeAppear{$endif};
    property TimePerSymbol: Single read FTimePerSymbol write SetTimePerSymbol
             {$ifdef FPC}default DefaultTimePerSymbol{$endif};
    property TimeVanish: Single read FTimeVanish write FTimeVanish
             {$ifdef FPC}default DefaultTimeVanish{$endif};
    property AllowedArea: Single read FAllowedArea write FAllowedArea
             {$ifdef FPC}default DefaultAllowedArea{$endif};
    property ColorPersistent: TCastleColorRGBPersistent read FColorPersistent;
    property CustomFont: TCastleAbstractFont read GetCustomFont write SetCustomFont;
    property PointInWorldPersistent: TCastleVector3Persistent read FPointInWorldPersistent stored True;
    property Viewport: TCastleViewport read FViewport write FViewport;
  end;

implementation

uses
  CastleComponentSerialize, CastleUtils, Math;

constructor TNyaSpeechBubble.Create(AOwner: TComponent; AArea: TBox3D;
                                    AActror: TNyaActor; const AMsg: String;
                                    AFont: TCastleAbstractFont;
                                    ATimePerSymbol: Single = DefaultTimePerSymbol);
var
  vport: TCastleViewport;
begin
  Create(AOwner);

  if Assigned(AFont) then
    CustomFont:= AFont;

  Color:= AActror.PersonalColor;

  TextActorName.Caption:= AActror.ActorName + ':';
  TextMessage.Caption:= AMsg;
  TimePerSymbol:= ATimePerSymbol;

  FPointInWorld:= Vector3(RandomFloatRange(AArea.Min.X, AArea.Max.X),
                          RandomFloatRange(AArea.Min.Y, AArea.Max.Y),
                          RandomFloatRange(AArea.Min.Z, AArea.Max.Z));

  if (Assigned(AOwner) AND (AOwner is TCastleViewport)) then
  begin
    vport:= (AOwner as TCastleViewport);
    vport.InsertFront(self);
    FViewport:= vport;
  end;
end;

constructor TNyaSpeechBubble.Create(AOwner: TComponent;
                                    AActror: TNyaActor; const AMsg: String;
                                    AFont: TCastleAbstractFont;
                                    ATimePerSymbol: Single);
var
  areaRect: TFloatRectangle;
  ownerUI: TCastleUserInterface;
begin
  Create(AOwner);

  if Assigned(AFont) then
    CustomFont:= AFont;

  Color:= AActror.PersonalColor;

  TextActorName.Caption:= AActror.ActorName + ':';
  TextMessage.Caption:= AMsg;
  TimePerSymbol:= ATimePerSymbol;

  if (Assigned(AOwner) AND (AOwner is TCastleUserInterface)) then
  begin
    ownerUI:= AOwner as TCastleUserInterface;
    areaRect:= ownerUI.RenderRect;
    Translation:= Vector2(RandomFloatRange(areaRect.Left,   areaRect.Right),
                          RandomFloatRange(areaRect.Bottom, areaRect.Top));
    ownerUI.InsertFront(self);
  end;
end;

constructor TNyaSpeechBubble.Create(AOwner: TComponent);
begin
  inherited;
  FTime:= 0.0;
  FTimeAppear:= DefaultTimeAppear;
  FTimePerSymbol:= DefaultTimePerSymbol;
  FTimeVanish:= DefaultTimeVanish;
  FAllowedArea:= DefaultAllowedArea;
  FTransparency:= DefaultTransparency;
  FActorName:= DefaultActorName;
  FMessage:= DefaultMessage;
  FPointInWorld:= DefaultPointInWorld;
  AutoSizeToChildren:= True;

  RectangleBG:= TNyaRoundRectangle.Create(self);
  RectangleBG.SetTransient;
  RectangleBG.AutoSizeToChildren:= True;
  RectangleBG.Round1:= DefaultRound1;
  RectangleBG.Round2:= DefaultRound2;
  RectangleBG.OutlineWidth:= DefaultOutlineWidth;
  InsertFront(RectangleBG);

  FGroup:= TCastleVerticalGroup.Create(RectangleBG);
  FGroup.SetTransient;
  FGroup.Padding:= DefaultPadding + RectangleBG.Round1 / 2.0;
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

  { Persistent for PointInWorld }
  FPointInWorldPersistent:= TCastleVector3Persistent.Create(nil);
  FPointInWorldPersistent.SetSubComponent(true);
  FPointInWorldPersistent.InternalGetValue:= {$ifdef FPC}@{$endif}GetPointInWorldForPersistent;
  FPointInWorldPersistent.InternalSetValue:= {$ifdef FPC}@{$endif}SetPointInWorldForPersistent;
  FPointInWorldPersistent.InternalDefaultValue:= PointInWorld;

  ApplyTransparency;
end;

destructor TNyaSpeechBubble.Destroy;
begin
  FreeAndNil(FColorPersistent);
  FreeAndNil(FPointInWorldPersistent);
  inherited;
end;

procedure TNyaSpeechBubble.Update(const SecondsPassed: Single; var HandleInput: boolean);
var
  ready, old, fin: Single;
  edgeLeft, edgeRight, edgeTop, edgeBottom: Single;
  pos: TVector2;
begin
  inherited;

  { assign position to 3D-point }
  if Assigned(FViewport) then
    try
      Translation:= FViewport.PositionFromWorld(FPointInWorld);
    except
      Translation:= Vector2((FViewport.EffectiveRect.Width - EffectiveRect.Width) / 2.0,
                            (FViewport.EffectiveRect.Height - EffectiveRect.Height) / 2.0);
    end;

  { limit area of Bubble appearing on screen }
  if (FAllowedArea > 0.0) then
  begin
    edgeLeft:= (FViewport.EffectiveRect.Width) * (1.0 - FAllowedArea);
    edgeRight:= (FViewport.EffectiveRect.Width - EffectiveRect.Width) * FAllowedArea;
    edgeTop:= (FViewport.EffectiveRect.Height - EffectiveRect.Height) * FAllowedArea;
    edgeBottom:= (FViewport.EffectiveRect.Height) * (1.0 - FAllowedArea);

    pos:= Translation;

    if (pos.X < edgeLeft) then
      pos.X:= edgeLeft
    else if (pos.X > edgeRight) then
      pos.X:= edgeRight;

    if (pos.Y > edgeTop) then
      pos.Y:= edgeTop
    else if (pos.Y < edgeBottom) then
      pos.Y:= edgeBottom;

    Translation:= pos;
  end;

  {$ifndef CASTLE_DESIGN_MODE}
  { Appear-Vanish animation }
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

procedure TNyaSpeechBubble.SetRound1(const value: Single);
begin
  RectangleBG.Round1:= value;
  FGroup.Padding:= DefaultPadding +
                   Max(RectangleBG.Round1, RectangleBG.Round2) / 2.0;
end;

function TNyaSpeechBubble.GetRound1: Single;
begin
  Result:= RectangleBG.Round1;
end;

procedure TNyaSpeechBubble.SetRound2(const value: Single);
begin
  RectangleBG.Round2:= value;
  FGroup.Padding:= DefaultPadding +
                   Max(RectangleBG.Round1, RectangleBG.Round2) / 2.0;
end;

function TNyaSpeechBubble.GetRound2: Single;
begin
  Result:= RectangleBG.Round2;
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

function TNyaSpeechBubble.GetPointInWorldForPersistent: TVector3;
begin
  Result:= PointInWorld;
end;

procedure TNyaSpeechBubble.SetPointInWorldForPersistent(const AValue: TVector3);
begin
  PointInWorld:= AValue;
end;

function TNyaSpeechBubble.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'ColorPersistent', 'CustomFont', 'Transparency', 'TimeAppear',
       'TimePerSymbol', 'TimeVanish', 'Round1', 'Round2', 'ActorName',
       'Message', 'OutlineWidth', 'PointInWorldPersistent', 'Viewport',
       'AllowedArea'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TNyaSpeechBubble, 'Nya Speech Bubble');
end.

