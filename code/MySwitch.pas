unit MySwitch;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleTransform, CastleClassUtils;

type
  TTouchEvent = procedure (const Sender: TObject; Touch: Boolean) of object;

  TMySwitch = class(TCastleBehavior)
  protected
    FOnTouch: TTouchEvent;
    FOnActivate: TNotifyEvent;
    FIsTouch: Boolean;
    FAngleCOS: Single;
    FActionString: String;
    FActivator: TCastleTransform;
    FActivatorObserver: TFreeNotificationObserver;
    procedure SetActivator(const Value: TCastleTransform);
    procedure ActivatorFreeNotification(const Sender: TFreeNotificationObserver);
    procedure SetIsTouch(const Value: Boolean);
    function GetAngle: Single;
    procedure SetAngle(value: Single);
  protected
    function CanAttachToParent(const NewParent: TCastleTransform;
      out ReasonWhyCannot: String): Boolean; override;
    procedure LookForActivator; virtual;
  public
    const
      DefaultAngleCOS = 0.707106769; { 45 grad }
      DefaultActionString = 'use';

    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    procedure Activate;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    property IsTouch: Boolean read FIsTouch write SetIsTouch;
  published
    property Activator: TCastleTransform read FActivator write SetActivator;
    property ActionString: String read FActionString write FActionString;
    property VisibleAngle: Single read GetAngle write SetAngle
      {$ifdef FPC}default DefaultAngleCOS{$endif};
    property OnTouch: TTouchEvent read FOnTouch write FOnTouch;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
  end;

implementation

uses
  CastleComponentSerialize, CastleUtils, CastleBoxes, CastleVectors,
  Math;

{ ========= ------------------------------------------------------------------ }
{ TMySwitch ------------------------------------------------------------------ }
{ ========= ------------------------------------------------------------------ }

constructor TMySwitch.Create(AOwner: TComponent);
begin
  inherited;

  FAngleCOS:= DefaultAngleCOS;
  FActionString:= DefaultActionString;

  FActivatorObserver:= TFreeNotificationObserver.Create(Self);
  FActivatorObserver.OnFreeNotification:= {$ifdef FPC}@{$endif}ActivatorFreeNotification;
end;

procedure TMySwitch.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  inherited;

  LookForActivator;
end;

procedure TMySwitch.LookForActivator;
var
  ActivatorBox, ParentBox: TBox3D;
  FromActivatorDir, ActivatorDir: TVector3;
begin
  if NOT (Assigned(Parent) AND Assigned(Activator)) then Exit;

  ActivatorBox:= Activator.BoundingBox;
  ParentBox:= Parent.BoundingBox;

  if ActivatorBox.Collides(ParentBox) then
  begin
    ActivatorDir:= Activator.Direction;
    FromActivatorDir:= (ParentBox.Center - ActivatorBox.Center).Normalize;
    IsTouch:= (TVector3.DotProduct(FromActivatorDir, ActivatorDir) > FAngleCOS);
  end;
end;

function TMySwitch.CanAttachToParent(const NewParent: TCastleTransform;
  out ReasonWhyCannot: String): Boolean;
begin
  Result:= inherited;
  if NOT Result then Exit;

  if NewParent.FindBehavior(TMySwitch) <> nil then
  begin
    ReasonWhyCannot:= 'Only one TMySwitch can be added to the "' +
                      NewParent.Name + '"';
    Result:= false;
  end;
end;

procedure TMySwitch.Activate;
begin
  if Assigned(OnActivate) then
    OnActivate(Self);
end;

function TMySwitch.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'Activator', 'VisibleAngle', 'ActionString'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

procedure TMySwitch.SetIsTouch(const Value: Boolean);
begin
  if FIsTouch <> Value then
  begin
    FIsTouch:= Value;

    if Assigned(OnTouch) then
      OnTouch(Self, Value);
  end;
end;

function TMySwitch.GetAngle: Single;
begin
  Result:= 180.0 * ArcCos(FAngleCOS) / Pi;
end;

procedure TMySwitch.SetAngle(value: Single);
begin
  FAngleCOS:= Cos(value * Pi / 180.0);
end;

procedure TMySwitch.SetActivator(const Value: TCastleTransform);
begin
  if (FActivator <> Value) then
  begin
    FActivatorObserver.Observed:= Value;
    FActivator:= Value;
  end;
end;

procedure TMySwitch.ActivatorFreeNotification(
  const Sender: TFreeNotificationObserver);
begin
  Activator:= nil;
end;

initialization
  RegisterSerializableComponent(TMySwitch, ['Switches', 'Switch']);
end.

