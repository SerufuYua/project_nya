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
      public
        ParentView: TViewSpeakMenu;
        constructor Create(AOwner: TComponent;
                           chara: TNyaActorChara;
                           message: String); reintroduce;
        //procedure Start; override;
        procedure Update(const SecondsPassed: Single;
                         var HandleInput: boolean); override;
      end;
    var
      FWin: TViewSpeakWindow;
      FChara: TNyaActorChara;
      FMessage: String;
  public
    constructor CreateUntilStopped(chara: TNyaActorChara; message: String);
    procedure Start; override;
  end;

var
  ViewSpeakWindow: TViewSpeakMenu;

implementation

uses
  CastleComponentSerialize;

{ ========= ------------------------------------------------------------------ }
{ TViewSpeakWindow ----------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

constructor TViewSpeakMenu.TViewSpeakWindow.Create(AOwner: TComponent;
                                                   chara: TNyaActorChara;
                                                   message: String);
var
  UiOwner: TComponent;
  Ui: TCastleUserInterface;
  alpha: single;
begin
  inherited Create(AOwner);

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
end;

{ ========= ------------------------------------------------------------------ }
{ TViewSpeakMenu ------------------------------------------------------------- }
{ ========= ------------------------------------------------------------------ }

constructor TViewSpeakMenu.CreateUntilStopped(chara: TNyaActorChara;
                                              message: String);
begin
  inherited CreateUntilStopped;
  FChara:= chara;
  FMessage:= message;
end;

procedure TViewSpeakMenu.Start;
begin
  inherited;

  FWin:= TViewSpeakWindow.Create(FreeAtStop, FChara, FMessage);
  FWin.ParentView := Self;
  FWin.Anchor(hpMiddle);
  FWin.Anchor(vpMiddle);
  InsertFront(FWin);
end;

end.
