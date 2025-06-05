unit NyaSpeechBubble;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleUIControls, CastleClassUtils, CastleControls,
  CastleColors, CastleFonts;

type
  TNyaSpeechBubble = class(TCastleUserInterface)
  protected
    RectangleBG: TCastleRectangleControl;
    ActorName, TextMessage: TCastleLabel;
    FTransparency: Single;
    FColor: TCastleColorRGB;
    FColorPersistent: TCastleColorRGBPersistent;
    procedure SetColor(const value: TCastleColorRGB);
    function GetColorForPersistent: TCastleColorRGB;
    procedure SetColorForPersistent(const AValue: TCastleColorRGB);
    procedure SetCustomFont(const value: TCastleAbstractFont);
    function GetCustomFont: TCastleAbstractFont;
    procedure SetTransparency(const value: Single);
    procedure ApplyTransparency;
  public
    const
      DefaultColor: TCastleColorRGB = (X: 0.6; Y: 0.0; Z: 0.5);
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
    property ColorPersistent: TCastleColorRGBPersistent read FColorPersistent;
    property CustomFont: TCastleAbstractFont read GetCustomFont write SetCustomFont;
  end;

implementation

uses
  CastleComponentSerialize, CastleUtils, CastleRectangles, CastleVectors;

constructor TNyaSpeechBubble.Create(AOwner: TComponent);
var
  group: TCastlePackedGroup;
begin
  inherited;
  FTransparency:= DefaultTransparency;

  AutoSizeToChildren:= True;
  HorizontalAnchorParent:= THorizontalPosition.hpMiddle;
  HorizontalAnchorSelf:= THorizontalPosition.hpMiddle;
  VerticalAnchorParent:= TVerticalPosition.vpMiddle;
  VerticalAnchorSelf:= TVerticalPosition.vpMiddle;
  Translation:= Vector2(0.0, 0.0);

  RectangleBG:= TCastleRectangleControl.Create(self);
  RectangleBG.SetTransient;
  RectangleBG.AutoSizeToChildren:= True;
  InsertFront(RectangleBG);

  group:= TCastleVerticalGroup.Create(RectangleBG);
  group.SetTransient;
  group.Padding:= 16;
  group.Spacing:= 14;
  RectangleBG.InsertFront(group);

  ActorName:= TCastleLabel.Create(group);
  ActorName.SetTransient;
  ActorName.Caption:= 'Actor:';
  group.InsertFront(ActorName);

  TextMessage:= TCastleLabel.Create(group);
  TextMessage.SetTransient;
  TextMessage.Caption:= 'Hello World!';
  group.InsertFront(TextMessage);

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
begin
  inherited;
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
       'ColorPersistent', 'CustomFont', 'Transparency'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TNyaSpeechBubble, 'Nya Speech Bubble');
end.

