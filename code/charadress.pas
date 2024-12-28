unit CharaDress;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleTransform, CastleScene, NyaCastleUtils;

type
  TSuits = (All, Top, Bottom, Foots, Arms);
  TDressSaver = class;

  TCharaDresser = class
  protected
    FScene: TCastleTransformDesign;
    function MainBody: TCastleScene; { main chara Body }
  public
    constructor Create(scene: TCastleTransformDesign);
    function SuitsList(suitType: TSuits): TItemConditions;
    function DressedSuit(suitType: TSuits): String;
    procedure WearSuit(suitType: TSuits; const suitName: String);
    function AcessoriesList: TItemConditions;
    procedure WearAcessory(const accessoryName: String; visible: boolean);
    procedure SaveCondition(const name:string);
    procedure RestoreCondition(const name:string);
  end;

  TDressSaver = class
  protected
    FDresser: TCharaDresser;
    FCharaName: String;
  public
    constructor Create(dresser: TCharaDresser; const charaName: String);
    procedure RestoreProperties;
    procedure SaveProperties;
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
  IniFileName = 'condition.ini';

{ ---------------------------------------------------------------------------- }
{ TCharaDresser -------------------------------------------------------------- }
{ ---------------------------------------------------------------------------- }

constructor TCharaDresser.Create(scene: TCastleTransformDesign);
begin
  FScene:= scene;
end;

function TCharaDresser.SuitsList(suitType: TSuits): TItemConditions;
var
  i: Integer;
  found: Boolean;
  fullNames: TItemConditions;
  shortName, newName: TItemCondition;
begin
  { get list of full suit shape names }
  Case suitType of
  Top: fullNames:= GetShapeNamesByNameStart(MainBody, PrefixTop);
  Bottom: fullNames:= GetShapeNamesByNameStart(MainBody, PrefixBottom);
  Foots: fullNames:= GetShapeNamesByNameStart(MainBody, PrefixFoots);
  Arms: fullNames:= GetShapeNamesByNameStart(MainBody, PrefixArms);
  All:
    begin
      fullNames:= [];
      Insert(GetShapeNamesByNameStart(MainBody, PrefixTop),
             fullNames, Length(fullNames));
      Insert(GetShapeNamesByNameStart(MainBody, PrefixBottom),
             fullNames, Length(fullNames));
      Insert(GetShapeNamesByNameStart(MainBody, PrefixFoots),
             fullNames, Length(fullNames));
      Insert(GetShapeNamesByNameStart(MainBody, PrefixArms),
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
      Insert(shortName, Result, 0);
  end;
end;

function TCharaDresser.DressedSuit(suitType: TSuits): String;
var
  suit: TItemCondition;
begin
  for suit in SuitsList(suitType) do
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
  Top: shapes:= GetShapesByNameStart(MainBody, PrefixTop);
  Bottom: shapes:= GetShapesByNameStart(MainBody, PrefixBottom);
  Foots: shapes:= GetShapesByNameStart(MainBody, PrefixFoots);
  Arms: shapes:= GetShapesByNameStart(MainBody, PrefixArms);
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

function TCharaDresser.AcessoriesList: TItemConditions;
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
  item: TCastleScene;
begin
  for item in GetAllScenes(FScene) do
  begin
    if item.Name.Contains(accessoryName) then
      item.Visible:= visible;
  end;
end;

procedure TCharaDresser.SaveCondition(const name:string);
var
  DressSaver: TDressSaver;
begin
  DressSaver:= TDressSaver.Create(self, name);
  DressSaver.SaveProperties;
  FreeAndNil(DressSaver);
end;

procedure TCharaDresser.RestoreCondition(const name:string);
var
  DressSaver: TDressSaver;
begin
  DressSaver:= TDressSaver.Create(self, name);
  DressSaver.RestoreProperties;
  FreeAndNil(DressSaver);
end;

function TCharaDresser.MainBody: TCastleScene;
begin
  Result:= FScene.DesignedComponent('Body', False) as TCastleScene;
end;

{ ---------------------------------------------------------------------------- }
{ TDressSaver ---------------------------------------------------------------- }
{ ---------------------------------------------------------------------------- }

constructor TDressSaver.Create(dresser: TCharaDresser; const charaName: String);
begin
  FDresser:= dresser;
  FCharaName:= charaName;
end;

procedure TDressSaver.RestoreProperties;
var
  ini: TCustomIniFile;
  accessory: TItemCondition;
  visible: boolean;
  suitName: String;
begin
  ini:= TMemIniFile.Create(IniFileName);
  ini.FormatSettings.DecimalSeparator := '|';
  ini.Options:= [ifoFormatSettingsActive];

  { suits }
  suitName:= ini.ReadString(FCharaName, PrefixTop, 'summer_shirt');
  FDresser.WearSuit(Top, suitName);

  suitName:= ini.ReadString(FCharaName, PrefixBottom, 'jorts');
  FDresser.WearSuit(Bottom, suitName);

  suitName:= ini.ReadString(FCharaName, PrefixFoots, 'sneakers');
  FDresser.WearSuit(Foots, suitName);

  suitName:= ini.ReadString(FCharaName, PrefixArms, 'none');
  FDresser.WearSuit(Arms, suitName);

  { accessories }
  for accessory in FDresser.AcessoriesList do
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
  suitName: String;
begin
  ini:= TMemIniFile.Create(IniFileName);
  ini.FormatSettings.DecimalSeparator := '|';
  ini.Options:= [ifoFormatSettingsActive];

  { suits }
  suitName:= FDresser.DressedSuit(Top);
  ini.WriteString(FCharaName, PrefixTop, suitName);

  suitName:= FDresser.DressedSuit(Bottom);
  ini.WriteString(FCharaName, PrefixBottom, suitName);

  suitName:= FDresser.DressedSuit(Foots);
  ini.WriteString(FCharaName, PrefixFoots, suitName);

  suitName:= FDresser.DressedSuit(Arms);
  ini.WriteString(FCharaName, PrefixArms, suitName);

  { accessories }
  for accessory in FDresser.AcessoriesList do
  begin
    ini.WriteBool(FCharaName, accessory.Name, accessory.Visible);
  end;

  ini.Free;
end;

end.

