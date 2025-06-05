unit NyaSpeechBubble;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleUIControls, CastleClassUtils, CastleControls;

type
  TNyaSpeechBubble = class(TCastleUserInterface)
  protected
    RectangleBG: TCastleRectangleControl;
    ActorName, TextMessage: TCastleLabel;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
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
end;

procedure TNyaSpeechBubble.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
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

