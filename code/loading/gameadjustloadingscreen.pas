unit GameAdjustLoadingScreen;

interface

uses CastleUIControls, CastleColors, CastleControls, GameEmbeddedImages;

implementation

initialization
  Theme.LoadingBackgroundColor:= Black; // adjust as needed
  Theme.LoadingColor:= White; // adjust as needed
  Theme.ImagesPersistent[tiLoading].Image:= Loading;
  Theme.ImagesPersistent[tiLoading].OwnsImage:= false;
end.

