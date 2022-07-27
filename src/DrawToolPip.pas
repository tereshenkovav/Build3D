unit DrawToolPip;

interface
uses DrawTools, Model, CommonClasses ;

type
  TDrawToolPip = class(TDrawTool)
  public
     function Caption():string; override ;
     function Order():Integer ; override ;
     function Apply(model:TModel; block:TBlock; dir:TBlockDir):Boolean ; override ;
  end;

implementation

{ TDrawToolPip }

function TDrawToolPip.Apply(model: TModel; block:TBlock;
  dir: TBlockDir): Boolean;
begin
  // �� ����� ������ �������
  // inherited Apply(model,block,dir) ;
  // ������ ����������
end;

function TDrawToolPip.Caption: string;
begin
  Result:='�������' ;
end;

function TDrawToolPip.Order: Integer;
begin
  Result:=888 ;
end;

initialization
  ForceRef(TDrawToolPip) ;

end.
