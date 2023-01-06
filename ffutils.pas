unit ffutils;
{ $ MODE Delphi}

interface
uses libavformat,libavutil,fftypes;



function PPtrIdx(P: PPAnsiChar; I: Integer): PAnsiChar; overload;
function PPtrIdx(P: PPByte; I: Integer): PByte; overload;
function PPtrIdx(P: PInteger; I: Integer): Integer; overload;
function PPtrIdx(P: PInt64; I: Integer): Int64; overload;

function PtrIdx(P: PSmallInt; I: Integer): PSmallInt; overload;
function PtrIdx(P: PSingle; I: Integer): PSingle; overload;
function PtrIdx(P: PDouble; I: Integer): PDouble; overload;

function PPtrIdx(P: PPAVStream; I: Integer): PAVStream; overload;
//function PPtrIdx(P: PPAVDictionary; I: Integer): PAVDictionary; overload;

type
  av_intfloat32 = record
    case Integer of
      0: (i: Cardinal);
      1: (f: Single);
  end;

(**
 * Reinterpret a 32-bit integer as a float.
 *)
function av_int2float(i: Cardinal): Single;

implementation

function PPtrIdx(P: PPAnsiChar; I: Integer): PAnsiChar;
begin
  Inc(P, I);
  Result := P^;
end;

function PPtrIdx(P: PPByte; I: Integer): PByte;
begin
  Inc(P, I);
  Result := P^;
end;

function PPtrIdx(P: PInteger; I: Integer): Integer;
begin
  Inc(P, I);
  Result := P^;
end;

function PPtrIdx(P: PInt64; I: Integer): Int64;
begin
  Inc(P, I);
  Result := P^;
end;

function PtrIdx(P: PSmallInt; I: Integer): PSmallInt;
begin
  Result := P;
  Inc(Result, I);
end;

function PtrIdx(P: PSingle; I: Integer): PSingle;
begin
  Inc(P, I);
  Result := P;
end;

function PtrIdx(P: PDouble; I: Integer): PDouble;
begin
  Inc(P, I);
  Result := P;
end;

function PPtrIdx(P: PPAVStream; I: Integer): PAVStream;
begin
  Inc(P, I);
  Result := P^;
end;


//function PPtrIdx(P: PPAVDictionary; I: Integer): PAVDictionary;
//begin
//  Inc(P, I);
//  Result := P^;
//end;

function av_int2float(i: Cardinal): Single;
var
  v: av_intfloat32;
begin
  v.i := i;
  Result := v.f;
end;

end.
