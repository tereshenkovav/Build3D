unit UnitSliceOpt;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  CommonClasses;

type
  TFormSliceOpt = class(TForm)
    Label1: TLabel;
    EdDir: TEdit;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    ButSelDir: TButton;
    Label2: TLabel;
    ComboFormat: TComboBox;
    rbX: TRadioButton;
    rbY: TRadioButton;
    rbZ: TRadioButton;
    Label3: TLabel;
    EdTpl: TEdit;
    cbShowGrid: TCheckBox;
    EdGridWidth: TEdit;
    cbPriorLayer: TCheckBox;
    EdPriorLayerBr: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    procedure BitBtn2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    function Dir():string ;
    function FileTpl():string ;
    function FormatExt():string ;
    function Axis():TAxis ;
    function ShowGrid():Boolean ;
    function ShowPriorLayer():Boolean ;
    function GridWidth():Integer ;
    function LayerBr():Integer ;
  end;

implementation
uses IOUtils, IniFiles, CommonProc ;

{$R *.dfm}

function TFormSliceOpt.Axis: TAxis;
begin
  Result:=axisX ;
  if rbX.Checked then Result:=axisX ;
  if rbY.Checked then Result:=axisY ;
  if rbZ.Checked then Result:=axisZ ;
end;

procedure TFormSliceOpt.BitBtn1Click(Sender: TObject);
var axisstr:string ;
begin
  with TIniFile.Create(AppDataPath+'\config.ini') do begin
    WriteString('FormSliceOpt','EdDir',EdDir.Text) ;
    WriteInteger('FormSliceOpt','ComboFormat',ComboFormat.ItemIndex) ;
    WriteString('FormSliceOpt','EdTpl',EdTpl.Text) ;
    if rbX.Checked then axisstr:='X' else
    if rbY.Checked then axisstr:='Y' else axisstr:='Z' ;
    WriteString('FormSliceOpt','Axis',axisstr) ;
    WriteBool('FormSliceOpt','cbShowGrid',cbShowGrid.Checked) ;
    WriteBool('FormSliceOpt','cbPriorLayer',cbPriorLayer.Checked) ;
    WriteString('FormSliceOpt','EdGridWidth',EdGridWidth.Text) ;
    WriteString('FormSliceOpt','EdPriorLayerBr',EdPriorLayerBr.Text) ;
    Free ;
  end;

  ModalResult:=mrOk ;
  Hide() ;
end;

procedure TFormSliceOpt.BitBtn2Click(Sender: TObject);
begin
  Close() ;
end;

function TFormSliceOpt.Dir: string;
begin
  Result:=edDir.Text ;
end;

function TFormSliceOpt.FileTpl: string;
begin
  Result:=edTpl.Text ;
  Result:=Result.Trim() ;
end;

function TFormSliceOpt.FormatExt: string;
begin
  Result:=LowerCase(ComboFormat.Items[ComboFormat.ItemIndex]) ;
end;

procedure TFormSliceOpt.FormCreate(Sender: TObject);
begin
  with TIniFile.Create(AppDataPath+'\config.ini') do begin
    EdDir.Text:=ReadString('FormSliceOpt','EdDir',TDirectory.GetCurrentDirectory) ;
    ComboFormat.ItemIndex:=ReadInteger('FormSliceOpt','ComboFormat',0) ;
    EdTpl.Text:=ReadString('FormSliceOpt','EdTpl','layer_%.3d') ;
    rbX.Checked:=ReadString('FormSliceOpt','Axis','X')='X' ;
    rbY.Checked:=ReadString('FormSliceOpt','Axis','X')='Y' ;
    rbZ.Checked:=ReadString('FormSliceOpt','Axis','X')='Z' ;
    cbShowGrid.Checked:=ReadBool('FormSliceOpt','cbShowGrid',False) ;
    cbPriorLayer.Checked:=ReadBool('FormSliceOpt','cbPriorLayer',False) ;
    EdGridWidth.Text:=ReadString('FormSliceOpt','EdGridWidth','2') ;
    EdPriorLayerBr.Text:=ReadString('FormSliceOpt','EdPriorLayerBr','50') ;
    Free ;
  end;
end;

function TFormSliceOpt.GridWidth: Integer;
begin
  Result:=StrToInt(Trim(EdGridWidth.Text)) ;
end;

function TFormSliceOpt.LayerBr: Integer;
begin
  Result:=StrToInt(Trim(EdPriorLayerBr.Text)) ;
end;

function TFormSliceOpt.ShowGrid: Boolean;
begin
  Result:=cbShowGrid.Checked ;
end;

function TFormSliceOpt.ShowPriorLayer: Boolean;
begin
  Result:=cbPriorLayer.Checked ;
end;

end.
