unit NyaSpeechBubble;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleUIControls, CastleClassUtils, CastleControls,
  CastleColors;

type
  TNyaSpeechBubble = class(TCastleUserInterface)
  protected
    RectangleBG: TCastleRectangleControl;
    ActorName, TextMessage: TCastleLabel;
    FColor: TCastleColorRGB;
    FColorPersistent: TCastleColorRGBPersistent;
    procedure SetColor(const value: TCastleColorRGB);
    function GetColorForPersistent: TCastleColorRGB;
    procedure SetColorForPersistent(const AValue: TCastleColorRGB);
  public
    const
      DefaultColor: TCastleColorRGB = (X: 0.6; Y: 0.0; Z: 0.5);

    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    property Color: TCastleColorRGB read FColor write SetColor;
  published
    property ColorPersistent: TCastleColorRGBPersistent read FColorPersistent;
  end;

implementation

uses
  CastleComponentSerialize, CastleUtils, CastleRectangles, CastleVectors;

constructor TNyaSpeechBubble.Create(AOwner: TComponent);
var
  group: TCastlePackedGroup;
begin
  inherited;

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
end;

procedure TNyaSpeechBubble.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
end;

procedure TNyaSpeechBubble.SetColor(const value: TCastleColorRGB);
var
  alpha: Single;
begin
  FColor:= value;

  alpha:= RectangleBG.Color.W;
  RectangleBG.Color:= Vector4(FColor, alpha);

  alpha:= ActorName.Color.W;
  ActorName.Color:= Vector4(0.75 + FColor.X * 0.25,
                            0.75 + FColor.Y * 0.25,
                            0.75 + FColor.Z * 0.25,
                            alpha);

  alpha:= TextMessage.Color.W;
  TextMessage.Color:= Vector4(0.75 + FColor.X * 0.25,
                              0.75 + FColor.Y * 0.25,
                              0.75 + FColor.Z * 0.25,
                              alpha);
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
       'nya'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TNyaSpeechBubble, 'Nya Speech Bubble');
end.

