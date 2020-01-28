unit Monitor;

interface
uses Generics.Collections, Classes, syncobjs ;

type
  TStamp = record
    dt:TDateTime ;
    value:Integer ;
  end;

  TMonitor = class
  private
     stamps:TList<TStamp> ;
     CS:TCriticalSection ;
     v:Integer ;
  public
     constructor Create ;
     procedure WriteDelay(value:Integer) ;
     function GetInfo():string ;
     procedure SetVar(varname:string; varvalue:Integer) ;
     procedure Reset() ;
  end;

function getMonitor():TMonitor ;

implementation
uses SysUtils ;

const
  SECS_PERIOD = 10 ;

var monitor_:TMonitor ;

function getMonitor():TMonitor ;
begin
  if monitor_=nil then monitor_:=TMonitor.Create() ;
  Result:=monitor_ ;
end;

{ TMonitor }

constructor TMonitor.Create;
begin
  CS:=TCriticalSection.Create() ;
  stamps:=TList<TStamp>.Create() ;
//  AssignFile(f,'perf.log') ;
//  ReWrite(f) ;
end;

function TMonitor.GetInfo: string;
begin
  CS.Enter() ;
  try

  if stamps.Count<2 then Exit('нет данных') ;

  Result:='FPS: '+IntToStr(Round(stamps.Count/
    (3600*24*(stamps.Last.dt-stamps.First.dt))))+
    ' Frames: '+stamps.Count.ToString()+#13#10+
    ' Val '+IntToStr(v) ;

  finally
    CS.Leave() ;
  end;
end;

procedure TMonitor.Reset;
begin
  CS.Enter() ;
  try

    stamps.Clear() ;

  finally
    CS.Leave() ;
  end;
end;

procedure TMonitor.SetVar(varname: string; varvalue: Integer);
begin
  CS.Enter() ;
  try
    v:=varvalue ;
  finally
    CS.Leave() ;
  end;
end;

procedure TMonitor.WriteDelay(value:Integer);
var r:TStamp ;
begin
  CS.Enter() ;
  try

  r.dt:=Now() ;

  if (stamps.Count>0) then
    while (3600*24*(r.dt-stamps[0].dt)>SECS_PERIOD) do begin
      stamps.Delete(0);
      if (stamps.Count=0) then break ;
    end;

  r.value:=value ;
  stamps.Add(r) ;

  finally
    CS.Leave() ;
  end;

  //  Writeln(f,IntToStr(value)) ;
//  Flush(f) ;
end;

end.
