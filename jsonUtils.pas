(*******************************************************************************
    The MIT License (MIT)

Copyright (c) 2011 by Sivv LLC,
Copyright (c) 2007 by Arcana Technologies Incorporated

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

Donations are appreciated if you find this code useful.
Please visit http://arcana.sivv.com to donate

*******************************************************************************)

unit jsonUtils;

interface

uses SysUtils, Classes, Types, DB, json;

function DatasetToJSON(Dataset : TDataset; RowStart : integer = 0; RowCount : integer = High(integer); PropertyName : string = 'data'; Compact : boolean = false; DateFormat : PJSONDateFormatter = nil; DataOnly : boolean = false) : String; overload;
function DatasetToJSON(Dataset : TDataset; FieldList : array of String; RowStart : integer = 0; RowCount : integer = High(integer); PropertyName : string = 'data'; Compact : boolean = false; DateFormat : PJSONDateFormatter = nil; DataOnly : boolean = false) : String; overload;

function DataRowToJSON(Dataset : TDataset; PropertyName : string = 'data'; Compact : boolean = false; DateFormat : PJSONDateFormatter = nil; DataOnly : boolean = false) : String; overload;
function DataRowToJSON(Dataset : TDataset; FieldList : array of String; PropertyName : string = 'data'; Compact : boolean = false; DateFormat : PJSONDateFormatter = nil; DataOnly : boolean = false) : String; overload;

const
  JSONContentType : string = 'applicaton/json';

implementation

function DatasetToJSON(Dataset : TDataset; RowStart : integer = 0; RowCount : integer = High(integer); PropertyName : string = 'data'; Compact : boolean = false; DateFormat : PJSONDateFormatter = nil; DataOnly : boolean = false) : String; overload;
begin
  Result := DatasetToJSON(Dataset, [], RowStart, RowCount, PropertyName, Compact, DateFormat, DataOnly);
end;

function DatasetToJSON(Dataset : TDataset; FieldList : array of String; RowStart : integer = 0; RowCount : integer = High(integer); PropertyName : string = 'data'; Compact : boolean = false; DateFormat : PJSONDateFormatter = nil; DataOnly : boolean = false) : String; overload;
var
  x: Integer;
  i, iCnt : integer;
  s: string;
  sl : TStringList;
  sField: String;
begin
  if DateFormat = nil then
    DateFormat := @JSONDates;
  Result := '';
  sl := TStringList.Create;
  try
    if RowStart >= 0 then
    begin
      Dataset.First;
      if RowStart > 0 then
        Dataset.MoveBy(RowStart);
    end;
    iCnt := 0;
    sField := '';
    while (not Dataset.EOF) and (iCnt < RowCount) do
    begin
      s := '';
      for x := 0 to Dataset.FieldCount - 1 do
      begin
        if InArray(Dataset.Fields[x].FieldName,FieldList) then
        begin
          if x <> 0 then
            s := s+',';
          if not Compact then
            sField := '"'+EncodeJSONString(Dataset.Fields[x].FieldName)+'" : ';

          case Dataset.Fields[x].DataType of
            ftString, ftFmtMemo, ftFixedChar, ftWideString, ftFixedWideChar, ftWideMemo, ftGuid, ftMemo:
              s := s + sField +'"'+EncodeJSONString(Dataset.Fields[x].AsString)+'"';

            ftSmallint, ftInteger, ftWord, ftFloat, ftCurrency, ftBCD, ftAutoInc, ftLargeint:
              s := s + sField +EncodeJSONString(Dataset.Fields[x].AsString);

            ftBoolean:
              s := s + sField +lowercase(BoolToStr(Dataset.Fields[x].AsBoolean,True));

            ftDate:
              s := s + sField +DateFormat.DateToString(Dataset.Fields[x].AsDateTime,ddDate);
            ftTime:
              s := s + sField +DateFormat.DateToString(Dataset.Fields[x].AsDateTime,ddTime);
            ftTimeStamp, ftOraTimeStamp, ftDateTime:
              s := s + sField +DateFormat.DateToString(Dataset.Fields[x].AsDateTime,ddDateTime);

            {ftUnknown,
            ftBytes,
            ftVarBytes,
            ftBlob,
            ftGraphic,
            ftParadoxOle,
            ftDBaseOle,
            ftTypedBinary,
            ftCursor,
            ftADT,
            ftArray,
            ftReference,
            ftDataSet,
            ftOraBlob,
            ftOraClob,
            ftVariant,
            ftInterface,
            ftIDispatch,
            ftFMTBcd,
            ftOraInterval}
          end;
        end;
      end;
      if s = '' then
        break;
      sl.Add(s);
      Dataset.Next;
      inc(iCnt);
    end;
    if sl.Count = 0 then
      exit;

    if not DataOnly then
      Result := '{ "'+EncodeJSONString(PropertyName)+'" : [';

    for i := 0 to sl.Count - 1 do
    begin
      if i <> 0 then
        Result := Result+',';
      if not Compact then
        Result := Result+'{'+sl[i]+'}'
      else
        Result := Result+'['+sl[i]+']';
    end;
    if not DataOnly then
      Result := Result+']}';
  finally
    sl.Free;
  end;
end;


function DataRowToJSON(Dataset : TDataset; PropertyName : string = 'data'; Compact : boolean = false; DateFormat : PJSONDateFormatter = nil; DataOnly : boolean = false) : String; overload;
begin
  Result := DataRowToJSON(Dataset, [], PropertyName, Compact, DateFormat, DataOnly);
end;

function DataRowToJSON(Dataset : TDataset; FieldList : array of String; PropertyName : string = 'data'; Compact : boolean = false; DateFormat : PJSONDateFormatter = nil; DataOnly : boolean = false) : String; overload;
begin
  Result := DatasetToJSON(Dataset,FieldList,-1,1, PropertyName, Compact, DateFormat, DataOnly);
end;


end.
