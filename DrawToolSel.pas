unit DrawToolSel;

interface
uses DrawTools, Model, CommonClasses ;

type
  TDrawToolSel = class(TDrawTool)
  public
     function Caption():string; override ;
     function Order():Integer ; override ;
     function Apply(model:TModel; block:TBlock; dir:TBlockDir):Boolean ; override ;
  end;

implementation

{ TDrawToolSel }

function TDrawToolSel.Apply(model: TModel; block:TBlock;
  dir: TBlockDir): Boolean;
begin
  // �� ����� ������ �������
  // inherited Apply(model,block,dir) ;
  // ������ ����������
end;

function TDrawToolSel.Caption: string;
begin
  Result:='��������' ;
end;

function TDrawToolSel.Order: Integer;
begin
  Result:=999 ;
end;

initialization
  ForceRef(TDrawToolSel) ;

end.
