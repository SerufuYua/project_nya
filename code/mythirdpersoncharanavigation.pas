unit MyThirdPersonCharaNavigation;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleCameras, CastleTransform, CastleClassUtils;

type
  TMyThirdPersonCharaNavigation = class(TCastleNavigation)
  protected
    FAvatarHierarchy: TCastleTransform;
    FAvatarHierarchyFreeObserver: TFreeNotificationObserver;
    procedure AvatarHierarchyFreeNotification(const Sender: TFreeNotificationObserver);
    procedure SetAvatarHierarchy(const Value: TCastleTransform);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single;
                     var HandleInput: Boolean); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    property AvatarHierarchy: TCastleTransform read FAvatarHierarchy write SetAvatarHierarchy;
  end;

implementation

uses
  CastleComponentSerialize, CastleUtils;

constructor TMyThirdPersonCharaNavigation.Create(AOwner: TComponent);
begin
  inherited;

  FAvatarHierarchyFreeObserver:= TFreeNotificationObserver.Create(Self);
  FAvatarHierarchyFreeObserver.OnFreeNotification:= {$ifdef FPC}@{$endif}AvatarHierarchyFreeNotification;
end;

destructor TMyThirdPersonCharaNavigation.Destroy;
begin
  AvatarHierarchy:= nil;
  inherited;
end;

procedure TMyThirdPersonCharaNavigation.Update(const SecondsPassed: Single;
                 var HandleInput: Boolean);
begin
  inherited;
end;

function TMyThirdPersonCharaNavigation.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'AvatarHierarchy'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TMyThirdPersonCharaNavigation.AvatarHierarchyFreeNotification(
  const Sender: TFreeNotificationObserver);
begin
  AvatarHierarchy:= nil;
end;

procedure TMyThirdPersonCharaNavigation.SetAvatarHierarchy(const Value: TCastleTransform);
begin
  if (FAvatarHierarchy <> Value) then
  begin
    FAvatarHierarchy:= Value;
    FAvatarHierarchyFreeObserver.Observed:= Value;
  end;
end;

initialization
  RegisterSerializableComponent(TMyThirdPersonCharaNavigation, ['Navigation', 'My-Third-Person-Chara']);
end.

