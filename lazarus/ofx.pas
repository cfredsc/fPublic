unit ofx;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, BufDataset;

type
  TSTMTTRN = record
    TRNTYPE: String;
    DTPOSTED: TDateTime;
    TRNAMT: Double;
    FITID: String;
    CHECKNUM: String;
    PAYEEID: String;
    MEMO: String;
  end;

  TOFXParser = class(TObject)
  private
    FOFX: TStringList;
    FOFXPos: Array[0..11] of Integer;

    FFmtSettings: TFormatSettings;

    // ponteiro -> instanciar chamando método CreateMemDS_STMTTRN
    FMemDS_STMTTRN: TBufDataSet;
  public
    constructor Create;
    destructor Destroy;

    procedure Clear;
    procedure Init;

    function LoadFromFile(FN: String): Boolean;

    procedure GetList_STMTTRN;
    function CreateMemDS_STMTTRN: TBufDataSet;

    property OFX: TStringList read FOFX;
    property FmtSettings: TFormatSettings read FFmtSettings write FFmtSettings;
  end;

implementation

const
  OFXTags: Array[0..11] of String =
    ('<OFX>','</OFX>',
     '<SIGNONMSGSRSV1>','</SIGNONMSGSRSV1>',
     '<SONRS>','</SONRS>',
     '<BANKMSGSRSV1>','</BANKMSGSRSV1>',
     '<STMTTRNRS>','</STMTTRNRS>',
     '<STMTRS>','</STMTRS>'
    );

constructor TOFXParser.Create;
begin
  FOFX := TStringList.Create;

  FFmtSettings.ThousandSeparator := ',';
  FFmtSettings.DecimalSeparator := '.';

  FMemDS_STMTTRN := nil;
end;

destructor TOFXParser.Destroy;
begin
  FOFX.Free;
end;

procedure TOFXParser.Clear;
var
  IdentTags: Word;
  I: Integer;
begin
  IdentTags := High(OFXTags);

  for I := 0 to IdentTags do
    FOFXPos[I] := 0;

  FOFX.Clear;
end;

procedure TOFXParser.Init;
var
  IdentTags: Word;
  N: Integer;
  I: Integer;
  S: String;
  E: Integer;
  P: Integer;
begin
  IdentTags := High(OFXTags);

  N := FOFX.Count;
  for I := 0 to N - 1 do
  begin
    S := FOFX[I];

    for E := 0 to IdentTags do
    begin
      P := Pos(OFXTags[E], S);
      if P > 0 then
        FOFXPos[E] := I;
    end;
  end;
end;

function TOFXParser.LoadFromFile(FN: String): Boolean;
begin
  try
    Self.Clear;
    FOFX.LoadFromFile(FN);
    Self.Init;

    Result := True;
  except
    Result := False;
    Raise;
  end;
end;

procedure TOFXParser.GetList_STMTTRN;
var
  SL: TStringList;

  N: Integer;
  I: Integer;
  S: String;
  P: Integer;

  V: TSTMTTRN;

  procedure SL_AddStrings(source, target: TStringList; Idx1, Idx2: Integer);
  var
    I: Integer;
  begin
    for I := Idx1 to Idx2 do
      target.Add(source[I]);
  end;
begin
  if FMemDS_STMTTRN = nil then
    Exit;

  SL := nil;
  try
    SL := TStringList.Create;
    SL_AddStrings(FOFX, SL, FOFXPos[10], FOFXPos[11]);

    I := 0;
    N := SL.Count;

    while I < N do
    begin
      S := SL[I];

      P := Pos('<STMTTRN>', S);
      if P > 0 then
      begin
        I := I + 1;

        while Pos('</STMTTRN>', SL[I]) <= 0 do
        begin
          S := Trim(StringReplace(SL[I], #9, '', [rfReplaceAll]));

          if Pos('<TRNTYPE>', S) > 0 then
          begin
            S := Trim(StringReplace(S, '<TRNTYPE>', '', [rfReplaceAll, rfIgnoreCase]));
            V.TRNTYPE := S;
          end else if Pos('<DTPOSTED>', S) > 0 then
          begin
            S := Trim(StringReplace(S, '<DTPOSTED>', '', [rfReplaceAll, rfIgnoreCase]));
            V.DTPOSTED := EncodeDate(StrToInt(Copy(S, 1, 4)),
                                     StrToInt(Copy(S, 5, 2)),
                                     StrToInt(Copy(S, 7, 2)));
          end else if Pos('<TRNAMT>', S) > 0 then
          begin
            S := Trim(StringReplace(S, '<TRNAMT>', '', [rfReplaceAll, rfIgnoreCase]));
            V.TRNAMT := StrToFloat(S, FFmtSettings);
          end else if Pos('<FITID>', S) > 0 then
          begin
            S := Trim(StringReplace(S, '<FITID>', '', [rfReplaceAll, rfIgnoreCase]));
            V.FITID := S;
          end else if Pos('<CHECKNUM>', S) > 0 then
          begin
            S := Trim(StringReplace(S, '<CHECKNUM>', '', [rfReplaceAll, rfIgnoreCase]));
            V.CHECKNUM := S;
          end else if Pos('<PAYEEID>', S) > 0 then
          begin
            S := Trim(StringReplace(S, '<PAYEEID>', '', [rfReplaceAll, rfIgnoreCase]));
            V.PAYEEID := S;
          end else if Pos('<MEMO>', S) > 0 then
          begin
            S := Trim(StringReplace(S, '<MEMO>', '', [rfReplaceAll, rfIgnoreCase]));
            V.MEMO := String(S);
          end;

          I := I + 1;
        end;

        with FMemDS_STMTTRN do
        begin
          Append;
          FieldByName('TRNTYPE').AsString := V.TRNTYPE;
          FieldByName('DTPOSTED').Value := V.DTPOSTED;
          FieldByName('TRNAMT').Value := V.TRNAMT;
          FieldByName('FITID').Value := V.FITID;
          FieldByName('CHECKNUM').Value := V.CHECKNUM;
          FieldByName('PAYEEID').Value := V.PAYEEID;
          FieldByName('MEMO').Value := V.MEMO;
          Post;
        end;
      end else
        I := I + 1;
    end;
  finally
    SL.Free;
  end;
end;

function TOFXParser.CreateMemDS_STMTTRN: TBufDataSet;
var
  Flds: Array[0..6] of Array[0..2] of Variant;
  I: Integer;
begin
  Result := TBufDataSet.Create(nil);
  FMemDS_STMTTRN := Result;

  Flds[0,0] := 'TRNTYPE';
  Flds[0,1] := ftString;
  Flds[0,2] := 100;

  Flds[1,0] := 'DTPOSTED';
  Flds[1,1] := ftDateTime;
  Flds[1,2] := 0;

  Flds[2,0] := 'TRNAMT';
  Flds[2,1] := ftFloat;
  Flds[2,2] := 0;

  Flds[3,0] := 'FITID';
  Flds[3,1] := ftString;
  Flds[3,2] := 50;

  Flds[4,0] := 'CHECKNUM';
  Flds[4,1] := ftString;
  Flds[4,2] := 50;

  Flds[5,0] := 'PAYEEID';
  Flds[5,1] := ftString;
  Flds[5,2] := 50;

  Flds[6,0] := 'MEMO';
  Flds[6,1] := ftString;
  Flds[6,2] := 100;

  for I := 0 to 6 do
  begin
    with Result.FieldDefs.AddFieldDef do
    begin
      Name := Flds[I][0];
      DataType := Flds[I][1];

      if Flds[I][1] = ftString then
        Size := Flds[I][2];
    end;
  end;

  Result.CreateDataSet;
end;

end.

