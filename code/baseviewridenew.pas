unit BaseViewRideNew;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, BaseViewTravel, NyaActorVehicle;

type
  TBaseViewRide = class(TBaseViewTravel)
  protected
    FVehicle: TNyaActorVehicle;
  public
    procedure SitToVehicle;

    property Vehicle: TNyaActorVehicle read FVehicle write FVehicle;
  end;

implementation

procedure TBaseViewRide.SitToVehicle;
begin
  MainActor.Parent:= Vehicle;
end;

end.

