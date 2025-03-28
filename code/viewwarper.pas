unit ViewWarper;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleUIControls, NyaActorChara;

type
  TViewWarper = class(TCastleView)
  protected
    FActorMain: TNyaActorChara;
    procedure SetMainActor(AActor: TNyaActorChara);
  public
    property MainActor: TNyaActorChara read FActorMain write SetMainActor;
  end;

implementation

procedure TViewWarper.SetMainActor(AActor: TNyaActorChara);
begin
  if (FActorMain = AActor) then Exit;

  FActorMain:= AActor;
end;

end.

