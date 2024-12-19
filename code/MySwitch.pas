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
    FActionString: String;
    FActivator: TCastleTransform;
    FActivatorObserver: TFreeNotificationObserver;
    procedure SetActivator(const Value: TCastleTransform);
    procedure ActivatorFreeNotification(const Sender: TFreeNotificationObserver);
    procedure SetIsTouch(const Value: Boolean);
  protected
    function CanAttachToParent(const NewParent: TCastleTransform;
      out ReasonWhyCannot: String): Boolean; override;
    procedure LookForActivator; virtual;
  public
    const
      DefaultActionString = 'use';

    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    procedure Activate;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    property IsTouch: Boolean read FIsTouch write SetIsTouch;
  published
    property Activator: TCastleTransform read FActivator write SetActivator;
    property ActionString: String read FActionString write FActionString;
    property OnTouch: TTouchEvent read FOnTouch write FOnTouch;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
  end;

  TMyFrontSwitch = class(TMySwitch)
  protected
    FAngleCOS: Single;
    function GetAngle: Single;
    procedure SetAngle(value: Single);
  protected
    procedure LookForActivator; override;
  public
    const
      DefaultAngleCOS = 0.707106769; { 45 grad }

    constructor Create(AOwner: TComponent); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    property VisibleAngle: Single read GetAngle write SetAngle
      {$ifdef FPC}default DefaultAngleCOS{$endif};
  end;

implementation

uses
  CastleComponentSerialize, CastleUtils, CastleBoxes, CastleVectors, Math,
  MyVectorMath;

{ ========= ------------------------------------------------------------------ }
{ TMySwitch ------------------------------------------------------------------ }
{ ========= ------------------------------------------------------------------ }

constructor TMySwitch.Create(AOwner: TComponent);
begin
  inherited;

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
  ActivatorBox, SwitchBox: TBox3D;
begin
  if NOT (Assigned(Parent) AND Assigned(Activator)) then Exit;

  ActivatorBox:= Activator.BoundingBox;
  SwitchBox:= Parent.BoundingBox;

  IsTouch:= ActivatorBox.Collides(SwitchBox);
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
       'Activator', 'ActionString'
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

{ ========= ------------------------------------------------------------------ }
{ TMyFrontSwitch ------------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

constructor TMyFrontSwitch.Create(AOwner: TComponent);
begin
  inherited;

  FAngleCOS:= DefaultAngleCOS;
end;

procedure TMyFrontSwitch.LookForActivator;
var
  ActivatorBox, SwitchBox: TBox3D;
  FromRootActivator, FromActivatorDir, ProjPoint, SwitchCenter: TVector3;
begin
  if NOT (Assigned(Parent) AND Assigned(Activator)) then Exit;

  ActivatorBox:= Activator.BoundingBox;
  SwitchBox:= Parent.BoundingBox;

  if ActivatorBox.Collides(SwitchBox) then
  begin
    SwitchCenter:= SwitchBox.Center;
    FromRootActivator:= (SwitchCenter - Activator.Translation);
    ProjPoint:= Activator.Translation + ProjectionVectorAtoB(FromRootActivator, Activator.Up);

    if ActivatorBox.Contains(ProjPoint) then
    begin
      FromActivatorDir:= (SwitchCenter - ProjPoint).Normalize;
      if (TVector3.DotProduct(FromActivatorDir, Activator.Direction) > FAngleCOS) then
      begin
        IsTouch:= True;
        Exit;
      end;
    end;
  end;
  IsTouch:= False;
end;

function TMyFrontSwitch.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'VisibleAngle'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

function TMyFrontSwitch.GetAngle: Single;
begin
  Result:= 180.0 * ArcCos(FAngleCOS) / Pi;
end;

procedure TMyFrontSwitch.SetAngle(value: Single);
begin
  FAngleCOS:= Cos(value * Pi / 180.0);
end;

initialization
  RegisterSerializableComponent(TMySwitch, ['Switches', 'Switch']);
  RegisterSerializableComponent(TMyFrontSwitch, ['Switches', 'Front Switch']);
end.

