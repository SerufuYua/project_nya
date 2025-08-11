unit NyaWebButton;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleControls, CastleClassUtils;

type
  TNyaWebButton = class(TCastleButton)
  protected
    FWebUrl: String;
  public
    procedure DoClick; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    property WebUrl: String read FWebUrl write FWebUrl;
  end;

implementation

uses
  CastleComponentSerialize, CastleUtils, CastleOpenDocument;

procedure TNyaWebButton.DoClick;
begin
  OpenURL(FWebUrl);
  inherited;
end;

function TNyaWebButton.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'WebUrl'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TNyaWebButton, 'Nya Web Button');
end.

