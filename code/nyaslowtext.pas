unit NyaSlowText;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleControls, CastleClassUtils, CastleTimeUtils,
  CastleKeysMouse;

type
  TNyaSlowText = class(TCastleLabel)
  protected
    FDelay, FTimer: Single;
    FMessage: String;
    FCount: Integer;
    procedure SetSlowCaption(value: String);
    function GetSlowCaption: String;
  public
    const
      DefaultDelay = 0.25;

    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    procedure ShowAll;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    property SlowCaption: String read GetSlowCaption write SetSlowCaption stored False;
    property SecondsDelay: Single read FDelay write FDelay
             {$ifdef FPC}default DefaultDelay{$endif};
  end;

implementation

uses
  CastleComponentSerialize, CastleUtils, StrUtils;

constructor TNyaSlowText.Create(AOwner: TComponent);
begin
  inherited;

  FDelay:= DefaultDelay;
  FMessage:= '';
  FCount:= 0;
  FTimer:= 0.0;
end;

procedure TNyaSlowText.Update(const SecondsPassed: Single; var HandleInput: Boolean);
var
  mText: String;
begin
  inherited;

  if (FCount > 0) then
  begin
    if (FTimer < 0) then
    begin
      mText:= Copy(FMessage, 1, (Length(FMessage) - FCount + 1));

      { get full HTML tag }
      if EndsText('<', mText) then
      begin
        while NOT EndsText('>', mText) do
        begin
          mText:= Copy(FMessage, 1, (Length(FMessage) - FCount + 1));
          FCount:= FCount - 1;
        end;
      end;

      { get full HTML symbol }
      if EndsText('&', mText) then
      begin
        while NOT EndsText(';', mText) do
        begin
          mText:= Copy(FMessage, 1, (Length(FMessage) - FCount + 1));
          FCount:= FCount - 1;
        end;
      end;

      Caption:= mText;
      FCount:= FCount - 1;
      FTimer:= FDelay;
    end;
    FTimer:= FTimer - SecondsPassed;
  end;
end;

procedure TNyaSlowText.ShowAll;
begin
  if (FCount > 0) then
  begin
    FCount:= 0;
    Caption:= FMessage;
  end;
end;

procedure TNyaSlowText.SetSlowCaption(value: String);
begin
  Caption:= '';
  FMessage:= value;
  FCount:= Length(value);
  FTimer:= FDelay;
end;

function TNyaSlowText.GetSlowCaption: String;
begin
  Result:= Caption;
end;

function TNyaSlowText.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'SlowCaption', 'SecondsDelay'
     ]) then
    Result:= [psBasic]
  else
    Result:= inherited PropertySections(PropertyName);
end;

initialization
  RegisterSerializableComponent(TNyaSlowText, 'Nya Slow Text');
end.

