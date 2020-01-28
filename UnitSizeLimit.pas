unit UnitSizeLimit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  CommonClasses, Vcl.Buttons ;

type
  TFormSizeLimit = class(TForm)
    Label1: TLabel;
    EdX: TEdit;
    Label2: TLabel;
    EdY: TEdit;
    Label3: TLabel;
    EdZ: TEdit;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    procedure BitBtn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    procedure setLimits(v:TPoint3I) ;
    function getLimits():TPoint3I ;
  end;

implementation

{$R *.dfm}

{ TFormSizeLimit }

procedure TFormSizeLimit.BitBtn1Click(Sender: TObject);
var v:TPoint3I ;
begin
  try
    v:=getLimits() ;
    if (v.x<=0)or(v.y<=0)or(v.z<=0) then
      raise Exception.Create('Размер должен быть больше нуля!');
    ModalResult:=mrOK ;
    Hide() ;
  except
    on e:Exception do begin
      ShowMessage('Неверные данные: '+e.Message) ;
    end;
  end;
end;

function TFormSizeLimit.getLimits: TPoint3I;
begin
  Result.x:=StrToInt(Trim(EdX.Text)) ;
  Result.y:=StrToInt(Trim(EdY.Text)) ;
  Result.z:=StrToInt(Trim(EdZ.Text)) ;
end;

procedure TFormSizeLimit.setLimits(v: TPoint3I);
begin
  EdX.Text:=IntToStr(v.x) ;
  EdY.Text:=IntToStr(v.y) ;
  EdZ.Text:=IntToStr(v.z) ;
end;

end.
