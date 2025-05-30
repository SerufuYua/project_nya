unit GameViewSpeakWindow;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  NyaActorChara;

type
  TViewSpeakMenu = class(TCastleView)
  strict private
    type
      TViewSpeakWindow = class(TCastleUserInterface)
      private
        ImageBG: TCastleImageControl;
        ActorName, TextMessage: TCastleLabel;
        FTime, FAppearTime, FLiveTime, FVanishTime: Single;
        procedure SetTransparency(value: Single);
      public
        ParentView: TViewSpeakMenu;
        constructor Create(AOwner: TComponent;
                           chara: TNyaActorChara;
                           message: String;
                           timePerSymbol: Single); reintroduce;
        //procedure Start; override;
        procedure Update(const SecondsPassed: Single;
                         var HandleInput: boolean); override;
      end;
    var
      FWin: TViewSpeakWindow;
      FChara: TNyaActorChara;
      FTimePerSymbol: Single;
      FMessage: String;
  public
    const
      DefaultAppearTime = 0.25;
      DefaultLiveTime = 0.5;
      DefaultFVanishTime = 4.0;

    constructor CreateUntilStopped(chara: TNyaActorChara; message: String;
                                   timePerSymbol: Single = DefaultLiveTime);
    procedure Start; override;
  end;

var
  ViewSpeakWindow: TViewSpeakMenu;

implementation

uses
  CastleComponentSerialize, CastleColors;

{ ========= ------------------------------------------------------------------ }
{ TViewSpeakWindow ----------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

constructor TViewSpeakMenu.TViewSpeakWindow.Create(AOwner: TComponent;
                                                   chara: TNyaActorChara;
                                                   message: String;
                                                   timePerSymbol: Single);
var
  UiOwner: TComponent;
  Ui: TCastleUserInterface;
  alpha: single;
begin
  inherited Create(AOwner);
  FTime:= 0.0;
  FAppearTime:= DefaultAppearTime;
  FLiveTime:= timePerSymbol * Length(message);
  FVanishTime:= DefaultFVanishTime;

  // UiOwner is useful to keep reference to all components loaded from the design
  UiOwner := TComponent.Create(Self);

  { Load designed user interface }
  Ui := UserInterfaceLoad('castle-data:/gameviewspeakwindow.castle-user-interface', UiOwner);
  InsertFront(Ui);

  { Find components, by name, that we need to access from code }
  ImageBG := UiOwner.FindRequiredComponent('ImageBG') as TCastleImageControl;
  ActorName := UiOwner.FindRequiredComponent('ActorName') as TCastleLabel;
  TextMessage := UiOwner.FindRequiredComponent('TextMessage') as TCastleLabel;

  { set speaker Name }
  ActorName.Caption:= chara.ActorName;

  { set message }
  TextMessage.Caption:= message;

  { set Color }
  alpha:= ImageBG.Color.W;
  ImageBG.Color:= Vector4(chara.PersonalColor, alpha);
end;

procedure TViewSpeakMenu.TViewSpeakWindow.Update(const SecondsPassed: Single;
                                                 var HandleInput: boolean);
begin
  inherited;
  { Executed every frame. }

  FTime:= FTime + SecondsPassed;

  if (FTime <= FAppearTime) then
    SetTransparency(FTime / FAppearTime)
  else if ((FTime >= (FAppearTime + FLiveTime)) AND (FTime <= (FAppearTime + FLiveTime + FVanishTime))) then
    SetTransparency((FVanishTime - (FTime - (FAppearTime + FLiveTime))) / FVanishTime);
end;

procedure TViewSpeakMenu.TViewSpeakWindow.SetTransparency(value: Single);
var
  tmpColor: TCastleColor;
begin
  tmpColor:= ImageBG.Color;
  tmpColor.W:= value;
  ImageBG.Color:= tmpColor;

  tmpColor:= ActorName.Color;
  tmpColor.W:= value;
  ActorName.Color:= tmpColor;

  tmpColor:= TextMessage.Color;
  tmpColor.W:= value;
  TextMessage.Color:= tmpColor;
end;

{ ========= ------------------------------------------------------------------ }
{ TViewSpeakMenu ------------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

constructor TViewSpeakMenu.CreateUntilStopped(chara: TNyaActorChara;
                                              message: String;
                                              timePerSymbol: Single);
begin
  inherited CreateUntilStopped;
  FChara:= chara;
  FMessage:= message;
  FTimePerSymbol:= timePerSymbol;
end;

procedure TViewSpeakMenu.Start;
begin
  inherited;

  FWin:= TViewSpeakWindow.Create(FreeAtStop, FChara, FMessage, FTimePerSymbol);
  FWin.ParentView := Self;
  FWin.Anchor(hpMiddle);
  FWin.Anchor(vpMiddle);
  InsertFront(FWin);
end;

end.
