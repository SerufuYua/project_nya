unit NyaSpeechBubble;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleUIControls, CastleClassUtils;

type
  TNyaSpeechBubble = class(TCastleUserInterface)
  public
    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  end;

implementation

uses
  CastleComponentSerialize, CastleUtils;

constructor TNyaSpeechBubble.Create(AOwner: TComponent);
begin
  inherited;
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

