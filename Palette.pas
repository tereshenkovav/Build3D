unit Palette;

interface
uses Classes ;

type
  TPalette = class
  private
    pal:TStringList ;
  public
    constructor Create(src:TStrings) ;
    destructor Destroy ; override ;
    function getList():TStrings ;
  end;

implementation
uses SysUtils, Constants, IOUtils ;

{ TPalette }

constructor TPalette.Create(src: TStrings);
var el:string ;
begin
  pal:=TStringList.Create() ;
  for el in src do
    if Trim(el)<>'' then
      if (TFile.Exists(TEXDIR+'\'+Trim(el))) then
        pal.Add(Trim(el)) ;

end;

destructor TPalette.Destroy;
begin
  pal.Free ;
  inherited Destroy ;
end;

function TPalette.getList: TStrings;
begin
  Result:=pal ;
end;

end.
