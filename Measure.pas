unit Measure;

interface

procedure StartMeasure() ;
function FinishMeasure():Integer ;

implementation
uses Windows ;

var freq,start,finish:Int64 ;

procedure StartMeasure() ;
begin
  QueryPerformanceFrequency(freq) ;
  QueryPerformanceCounter(start);
end;

function FinishMeasure():Integer ;
begin
  QueryPerformanceCounter(finish);
  Result:=Round((finish - start) * 1000 / freq) ;
end;


end.
