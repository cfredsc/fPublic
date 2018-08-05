unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DBGrids, DB, BufDataset, mysql56conn;

type
  { TForm1 }

  TForm1 = class(TForm)
    Button2: TButton;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    MySQL56Connection1: TMySQL56Connection;
    OpenDialog1: TOpenDialog;
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  ofx;

{ TForm1 }

var
  OFXObj: TOFXParser;
  MemDS_STMTTRN: TBufDataSet;

procedure TForm1.FormCreate(Sender: TObject);
begin
  OFXObj := TOFXParser.Create;

  MemDS_STMTTRN := OFXObj.CreateMemDS_STMTTRN;
  DataSource1.DataSet := MemDS_STMTTRN;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  MemDS_STMTTRN.Free;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    OFXObj.LoadFromFile(OpenDialog1.FileName);
    OFXObj.GetList_STMTTRN;

    MemDS_STMTTRN.First;
  end;
end;

end.

