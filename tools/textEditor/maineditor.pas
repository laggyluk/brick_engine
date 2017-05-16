unit mainEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  Buttons, ExtCtrls, StdCtrls, core_text_Reader,inifiles;

type

  { TForm1 }

  TForm1 = class(TForm)
    keyEdit: TLabeledEdit;
    Label1: TLabel;
    openBtn: TSpeedButton;
    openDialog: TOpenDialog;
    saveBtn: TSpeedButton;
    saveAsBtn: TSpeedButton;
    SaveDialog: TSaveDialog;
    SynEdit1: TSynEdit;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure openBtnClick(Sender: TObject);
    procedure saveAsBtnClick(Sender: TObject);
    procedure saveBtnClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure save(const filename:string);
  end;

var
  Form1: TForm1;
  lastFile:string;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.openBtnClick(Sender: TObject);
begin
  if openDialog.Execute then begin
     loadTextFile(synEdit1.Lines,opendialog.filename,strtoint(keyEdit.text));
     lastfile:=opendialog.FileName;
  end;
end;

procedure TForm1.saveAsBtnClick(Sender: TObject);
begin
  if savedialog.Execute then begin
     save(savedialog.filename);
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var f:tinifile;
begin
  f:=tinifile.create(extractfilepath(application.ExeName)+'config.ini');
  if lastFile<>'' then begin
    f.writeString('options','lastFile',lastFile);
    f.writeBool('options','loadLastFile',true)
  end;
  f.free;
  if synedit1.Modified then if MessageDlg('Quit qithout saving?',mtWarning, mbOKCancel, 0)=mrCancel then
     CloseAction:=TCloseAction.caNone;
end;

procedure TForm1.FormCreate(Sender: TObject);
var f:tinifile;
    i:integer;
begin
  f:=tinifile.create(extractfilepath(application.ExeName)+'config.ini');
  if ParamCount>0 THEN begin
       lastFile:=ParamStr(1);
       loadTextFile(synedit1.Lines,lastFile,strtoint(keyEdit.text));
  end else
  if f.ReadBool('options','loadLastFile',false) then if
     fileexists(f.ReadString('options','lastFile','')) then begin
        lastFile:=f.ReadString('options','lastFile','');
        loadTextFile(synedit1.Lines,lastFile,strtoint(keyEdit.text));
     end;
  f.free;
end;

procedure TForm1.saveBtnClick(Sender: TObject);
var  f:tinifile;
begin
  if lastFile<>'' then save(lastFile)
  else
  if savedialog.Execute then begin
     save(savedialog.filename);
  end;
end;

procedure TForm1.save(const filename: string);
begin
   saveTextFile(synEdit1.Lines,filename,strtoint(keyEdit.text));
   synedit1.Modified:=false;
   lastFile:=filename;
end;

end.

