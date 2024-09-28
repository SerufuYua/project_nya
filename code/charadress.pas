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
    function GetAcessoriesList(): TSceneNames;
    procedure WearAcessory(const accessoryName: String; visible: boolean);
  protected
    Scene: TCastleTransformDesign;
    function GetMainBody(): TCastleScene; { main chara Body }
  end;

implementation

uses
  X3DNodes, StrUtils;

const
  PrefixTop = 'top.';
  PrefixBottom = 'bottom.';
  PrefixFoots = 'foots.';
  PrefixArms = 'arms.';
  PrefixAccesory = 'accessory_';

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
  Top: fullNames:= GetShapeNamesByNameStart(GetMainBody(), PrefixTop);
  Bottom: fullNames:= GetShapeNamesByNameStart(GetMainBody(), PrefixBottom);
  Foots: fullNames:= GetShapeNamesByNameStart(GetMainBody(), PrefixFoots);
  Arms: fullNames:= GetShapeNamesByNameStart(GetMainBody(), PrefixArms);
  All:
    begin
      fullNames:= [];
      Insert(GetShapeNamesByNameStart(GetMainBody(), PrefixTop),
             fullNames, Length(fullNames));
      Insert(GetShapeNamesByNameStart(GetMainBody(), PrefixBottom),
             fullNames, Length(fullNames));
      Insert(GetShapeNamesByNameStart(GetMainBody(), PrefixFoots),
             fullNames, Length(fullNames));
      Insert(GetShapeNamesByNameStart(GetMainBody(), PrefixArms),
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
  Top: shapes:= GetShapesByNameStart(GetMainBody(), PrefixTop);
  Bottom: shapes:= GetShapesByNameStart(GetMainBody(), PrefixBottom);
  Foots: shapes:= GetShapesByNameStart(GetMainBody(), PrefixFoots);
  Arms: shapes:= GetShapesByNameStart(GetMainBody(), PrefixArms);
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

function TCharaDresser.GetAcessoriesList(): TSceneNames;
var
  i: Integer;
  acessoryNames: TSceneNames;
begin
  acessoryNames:= GetSceneNamesByNameStart(Scene, PrefixAccesory);

  for i:= 0 to (Length(acessoryNames) - 1) do
  begin
    delete(acessoryNames[i], 1, Length(PrefixAccesory));
  end;

  Result:= acessoryNames;
end;

procedure TCharaDresser.WearAcessory(const accessoryName: String; visible: boolean);
var
  items: TCastleScenes;
  item: TCastleScene;
begin
  items:= GetAllScenes(scene);

  for item in items do
  begin
    if item.Name.Contains(accessoryName) then
      item.Visible:= visible;
  end;
end;

function TCharaDresser.GetMainBody(): TCastleScene;
begin
  Result:= Scene.DesignedComponent('Body') as TCastleScene;
end;

end.

