unit CharaDress;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleScene, MyCastleUtils;

type
  TSuits = (All, Top, Bottom, Foots, Arms);

  TCharaDresser = class
  public
    constructor Create(newBody: TCastleScene);
    function GetSuitsList(suitType: TSuits): TShapeNames;
    procedure WearSuit(suitType: TSuits; const suitName: String);
  protected
    CharaBody: TCastleScene;
  end;

implementation

uses
  X3DNodes, StrUtils;

constructor TCharaDresser.Create(newBody: TCastleScene);
begin
  CharaBody:= newBody;
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
  Top: fullNames:= GetShapeNamesByNameStart(CharaBody, 'top.');
  Bottom: fullNames:= GetShapeNamesByNameStart(CharaBody, 'bottom.');
  Foots: fullNames:= GetShapeNamesByNameStart(CharaBody, 'foots.');
  Arms: fullNames:= GetShapeNamesByNameStart(CharaBody, 'arms.');
  All:
    begin
      fullNames:= [];
      Insert(GetShapeNamesByNameStart(CharaBody, 'top.'), fullNames, Length(fullNames));
      Insert(GetShapeNamesByNameStart(CharaBody, 'bottom.'), fullNames, Length(fullNames));
      Insert(GetShapeNamesByNameStart(CharaBody, 'foots.'), fullNames, Length(fullNames));
      Insert(GetShapeNamesByNameStart(CharaBody, 'arms.'), fullNames, Length(fullNames));
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
  Top: shapes:= GetShapesByNameStart(CharaBody, 'top.');
  Bottom: shapes:= GetShapesByNameStart(CharaBody, 'bottom.');
  Foots: shapes:= GetShapesByNameStart(CharaBody, 'foots.');
  Arms: shapes:= GetShapesByNameStart(CharaBody, 'arms.');
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

end.

