unit CharaDress;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleTransform, CastleScene, MyCastleUtils;

type
  TSuits = (All, Top, Bottom, Foots, Arms);

  TCharaDresser = class
  public
    constructor Create(newScene: TCastleTransformDesign);
    function GetSuitsList(suitType: TSuits): TShapeNames;
    procedure WearSuit(suitType: TSuits; const suitName: String);
  protected
    Scene: TCastleTransformDesign;
    function GetMainBody(): TCastleScene; { main chara Body }
  end;

implementation

uses
  X3DNodes, StrUtils;

constructor TCharaDresser.Create(newScene: TCastleTransformDesign);
begin
  Scene:= newScene;
end;

function TCharaDresser.GetSuitsList(suitType: TSuits): TShapeNames;
var
  i: Integer;
  found: Boolean;
  shortName, newName: String;
  fullNames: TShapeNames;
begin
  { get list of full suit shape names }
  Case suitType of
  Top: fullNames:= GetShapeNamesByNameStart(GetMainBody(), 'top.');
  Bottom: fullNames:= GetShapeNamesByNameStart(GetMainBody(), 'bottom.');
  Foots: fullNames:= GetShapeNamesByNameStart(GetMainBody(), 'foots.');
  Arms: fullNames:= GetShapeNamesByNameStart(GetMainBody(), 'arms.');
  All:
    begin
      fullNames:= [];
      Insert(GetShapeNamesByNameStart(GetMainBody(), 'top.'),
             fullNames, Length(fullNames));
      Insert(GetShapeNamesByNameStart(GetMainBody(), 'bottom.'),
             fullNames, Length(fullNames));
      Insert(GetShapeNamesByNameStart(GetMainBody(), 'foots.'),
             fullNames, Length(fullNames));
      Insert(GetShapeNamesByNameStart(GetMainBody(), 'arms.'),
             fullNames, Length(fullNames));
    end
  else
    fullNames:= [];
  end;

  { extract suit names from full names and remove dupe}
  Result:=[];

  for i:= 0 to (Length(fullNames) - 1) do
  begin
    found:= False;
    shortName:= ExtractDelimited(2, fullNames[i], ['.']);
    for newName in Result do
    begin
      if (String.Compare(shortName, newName) = 0) then
      begin
        found:= True;
        Break;
      end;
    end;
    if NOT found then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)]:= shortName;
    end;
  end;
end;

procedure TCharaDresser.WearSuit(suitType: TSuits; const suitName: String);
var
  shapes: TShapeNodes;
  shape: TShapeNode;
  shapeName: String;
begin
  Case suitType of
  Top: shapes:= GetShapesByNameStart(GetMainBody(), 'top.');
  Bottom: shapes:= GetShapesByNameStart(GetMainBody(), 'bottom.');
  Foots: shapes:= GetShapesByNameStart(GetMainBody(), 'foots.');
  Arms: shapes:= GetShapesByNameStart(GetMainBody(), 'arms.');
  else
    shapes:= [];
  end;

  for shape in shapes do
  begin
    shapeName:= shape.X3DName;
    if shapeName.Contains(suitName) then
      shape.Visible:= True
    else
      shape.Visible:= False;
  end;
end;

function TCharaDresser.GetMainBody(): TCastleScene;
begin
  Result:= Scene.DesignedComponent('Body') as TCastleScene;
end;

end.

