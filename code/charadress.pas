unit CharaDress;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleTransform, CastleScene, MyCastleUtils;

type
  TSuits = (All, Top, Bottom, Foots, Arms);

  TCharaDresser = class
  public
    constructor Create(scene: TCastleTransformDesign);
    function GetSuitsList(suitType: TSuits): TItemConditions;
    function GetDressedSuit(suitType: TSuits): String;
    procedure WearSuit(suitType: TSuits; const suitName: String);
    function GetAcessoriesList(): TItemConditions;
    procedure WearAcessory(const accessoryName: String; visible: boolean);
  protected
    FScene: TCastleTransformDesign;
    function GetMainBody(): TCastleScene; { main chara Body }
  end;

  TDressSaver = class
  public
    constructor Create(dresser: TCharaDresser; charaName: String);
    procedure RestoreProperties;
    procedure SaveProperties;
  protected
    FIniName: String;
    FDresser: TCharaDresser;
    FCharaName: String;
  end;

implementation

uses
  X3DNodes, StrUtils, IniFiles;

const
  PrefixTop = 'top.';
  PrefixBottom = 'bottom.';
  PrefixFoots = 'foots.';
  PrefixArms = 'arms.';
  PrefixAccesory = 'accessory_';

{ TCharaDresser}

constructor TCharaDresser.Create(scene: TCastleTransformDesign);
begin
  FScene:= scene;
end;

function TCharaDresser.GetSuitsList(suitType: TSuits): TItemConditions;
var
  i: Integer;
  found: Boolean;
  fullNames: TItemConditions;
  shortName, newName: TItemCondition;
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
    shortName.Name:= ExtractDelimited(2, fullNames[i].Name, ['.']);
    shortName.Visible:= fullNames[i].Visible;
    for newName in Result do
    begin
      if (String.Compare(shortName.Name, newName.Name) = 0) then
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

function TCharaDresser.GetDressedSuit(suitType: TSuits): String;
var
  suit: TItemCondition;
begin
  for suit in GetSuitsList(suitType) do
  begin
    if suit.Visible then
    begin
      Result:= suit.Name;
      Exit
    end;
  end;
  Result:= 'none';
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

function TCharaDresser.GetAcessoriesList(): TItemConditions;
var
  i: Integer;
  acessoryNames: TItemConditions;
begin
  acessoryNames:= GetSceneNamesByNameStart(FScene, PrefixAccesory);

  for i:= 0 to (Length(acessoryNames) - 1) do
  begin
    delete(acessoryNames[i].Name, 1, Length(PrefixAccesory));
  end;

  Result:= acessoryNames;
end;

procedure TCharaDresser.WearAcessory(const accessoryName: String; visible: boolean);
var
  items: TCastleScenes;
  item: TCastleScene;
begin
  items:= GetAllScenes(FScene);

  for item in items do
  begin
    if item.Name.Contains(accessoryName) then
      item.Visible:= visible;
  end;
end;

function TCharaDresser.GetMainBody(): TCastleScene;
begin
  Result:= FScene.DesignedComponent('Body') as TCastleScene;
end;

{ TDressSaver }

constructor TDressSaver.Create(dresser: TCharaDresser; charaName: String);
begin
  FIniName:= 'condition.ini';
  FDresser:= dresser;
  FCharaName:= charaName;

  RestoreProperties;
end;

procedure TDressSaver.RestoreProperties;
var
  ini: TCustomIniFile;
  accessory: TItemCondition;
  visible: boolean;
begin
  ini:= TMemIniFile.Create(FIniName);
  ini.FormatSettings.DecimalSeparator := '|';
  ini.Options:= [ifoFormatSettingsActive];

  { suits }
  FDresser.WearSuit(Top, ini.ReadString(FCharaName, PrefixTop, 'summer_shirt'));
  FDresser.WearSuit(Bottom, ini.ReadString(FCharaName, PrefixBottom, 'briefs'));
  FDresser.WearSuit(Foots, ini.ReadString(FCharaName, PrefixFoots, 'sneakers'));
  FDresser.WearSuit(Arms, ini.ReadString(FCharaName, PrefixArms, 'none'));

  { accessories }
  for accessory in FDresser.GetAcessoriesList() do
  begin
    visible:= ini.ReadBool(FCharaName, accessory.Name, False);
    FDresser.WearAcessory(accessory.Name, visible);
  end;

  ini.Free;
end;

procedure TDressSaver.SaveProperties;
var
  ini: TCustomIniFile;
  accessory: TItemCondition;
begin
  ini:= TMemIniFile.Create(FIniName);
  ini.FormatSettings.DecimalSeparator := '|';
  ini.Options:= [ifoFormatSettingsActive];

  { suits }
  ini.WriteString(FCharaName, PrefixTop, FDresser.GetDressedSuit(Top));
  ini.WriteString(FCharaName, PrefixBottom, FDresser.GetDressedSuit(Bottom));
  ini.WriteString(FCharaName, PrefixFoots, FDresser.GetDressedSuit(Foots));
  ini.WriteString(FCharaName, PrefixArms, FDresser.GetDressedSuit(Arms));

  { accessories }
  for accessory in FDresser.GetAcessoriesList() do
  begin
    ini.WriteBool(FCharaName, accessory.Name, accessory.Visible);
  end;

  ini.Free;
end;

end.

