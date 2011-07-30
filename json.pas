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

unit json;

interface

uses SysUtils, Classes, Variants, TypInfo, StrUtils;

type
  TJSONObject = Variant;
  PJSONObject = PVariant;
  TJSONValue = class;

  TDateDetail = (ddDateTime, ddDate, ddTime);
  TJSONDateToString = function(dt : TDateTime; Scope : TDateDetail = ddDateTime) : String;
  TJSONStringToDate = function(dt : String) : TDateTime;
  TJSONStringIsDate = function(dt : String) : Boolean;

  PJSONDateFormatter = ^TJSONDateFormatter;
  TJSONDateFormatter = record
    DateToString : TJSONDateToString;
    StringToDate : TJSONStringToDate;
    StringIsDate : TJSONStringIsDate;
  end;

  TJSONValueObject = class;

  TJSONValueType = (jvtNull, jvtString, jvtNumber, jvtObject, jvtArray, jvtBoolean);

  TJSONNotificationObject = class(TObject)
  private
    FRefCount : integer;
  protected
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddRef;
    procedure DecRef;
    property RefCount : integer read FRefCount;
  end;

  TJSONArrayList = class(TList)
  private
    function getVariantValue(idx: integer): Variant;
    procedure setVariantValue(idx: integer; const Value: Variant);
  public
    property Values[idx : integer] : Variant read getVariantValue write setVariantValue; default;
  end;

  TJSONValue = class(TObject)
  private
    FDateFormat : PJSONDateFormatter;
    FBooleanValue: Boolean;
    FNumberValue: Extended;
    FArrayValue: TJSONArrayList;
    FValueType: TJSONValueType;
    FObjectValue: TJSONValueObject;
    FStringValue: String;
    function GetStringValue: String;
    procedure SetStringValue(const Value: String);
    function getAsJSONValue: String;
    function getArrayJSON: String;
    procedure setArrayJSON(const Value: String);
    function getAsVariant: Variant;
    procedure setAsVariant(const Value: Variant);
    procedure SetBooleanValue(const Value: Boolean);
    procedure SetNumberValue(const Value: Extended);
    procedure SetObjectValue(const Value: TJSONValueObject);
  protected
    procedure AssignObject(value : Variant);
    procedure AssignArray(Value : Variant);
    procedure ClearArrayValue;
    procedure EnsureArrayValue;
    procedure ClearObjectValue;
  public
    property ValueType : TJSONValueType read FValueType;
    property StringValue : String read GetStringValue write SetStringValue;
    property NumberValue : Extended read FNumberValue write SetNumberValue;
    property ObjectValue : TJSONValueObject read FObjectValue write SetObjectValue;
    property ArrayValue : TJSONArrayList read FArrayValue;
    property ArrayJSON : String read getArrayJSON write setArrayJSON;
    property BooleanValue : Boolean read FBooleanValue write SetBooleanValue;

    property AsJSONValue : String read getAsJSONValue;
    property AsVariant : Variant read getAsVariant write setAsVariant;
    destructor Destroy; override;
    constructor Create(const Value : String; DateFormat : PJSONDateFormatter = nil); virtual;
  end;

  TJSONValueObject = class(TJSONNotificationObject)
  private
    FProps : TStringList;
    FDateFormat: PJSONDateFormatter;
    FLastPropertyName: string;
    function getAsJSON: String;
    procedure setAsJSON(const Value: String);
    function GetValues(idx: integer): TJSONValue;
    function GetProps(Name: String): TJSONValue;
    function getAsObject: Variant;
  protected
    procedure ClearProps; virtual;
  public
    constructor Create(JSON : String; DateFormat : PJSONDateFormatter = nil); virtual;
    destructor Destroy; override;

    property AsJSON : String read getAsJSON write setAsJSON;
    property Values[idx : integer] : TJSONValue read GetValues;
    property Props[Name : String] : TJSONValue read GetProps;
    property AsObject : Variant read getAsObject;
    property DateFormat : PJSONDateFormatter read FDateFormat;
    property LastPropertyName : string read FLastPropertyName write FLastPropertyName;

    procedure UpdateLastProperty(Name : string; Value : TJSONValueObject);
  end;

const
  Whitespace = [' ',#13,#10,#9];
  //ValueTerminators = ['}',',',#13,#10,#9];
  ValueTerminators = ['}',',',']'];//#13,#10,#9];

procedure DebugWriteLn(s : string);
function DecodeJSONString(Value : String) : String;
function EncodeJSONString(Value : String) : String;

function jvtTypeOfVariant(Value : Variant) : TJSONValueType;

function ObjectToJSON(Obj : TPersistent; WriteClass : boolean = false; DateFormat : PJSONDateFormatter = nil) : String; overload;
function ObjectToJSON(Obj : TPersistent; PropList : array of String; WriteClass : boolean = false; DateFormat : PJSONDateFormatter = nil) : String; overload;
function InArray(Str : string; ary : array of String) : boolean;

var
  JSONDates : TJSONDateFormatter;

implementation

uses DateUtils, jsonVariants;

function jvtTypeOfVariant(Value : Variant) : TJSONValueType;
begin
  if not (VarIsArray(Value)) then
  begin
    Result := jvtNull;
    case VarType(Value) and varTypeMask of
      varArray:
        Result := jvtArray;

      varSmallint,
      varInteger,
      varSingle,
      varDouble,
      varCurrency,
      varShortInt,
      varByte,
      varWord,
      varLongWord,
      varInt64,
      varUInt64:
        Result := jvtNumber;

      varDate,
      varOleStr,
      varStrArg,
      varUStrArg,
      varString,
      varAny,
      varUString:
        Result := jvtString;

      varBoolean:
        Result := jvtBoolean;

      varVariant,
      varUnknown,
      varObject:
        Result := jvtObject;
    end;
    if (Result = jvtNull) and (VarIsJSON(Value)) then
      Result := jvtObject
    else if (Result = jvtNull) and (VarIsJSONArray(Value)) then
      Result := jvtArray;
  end else
    Result := jvtArray;
end;

function ObjectToJSON(Obj : TPersistent; WriteClass : boolean = false; DateFormat : PJSONDateFormatter = nil) : String; overload;
begin
  Result := ObjectToJSON(Obj, [], WriteClass, DateFormat);
end;

function InArray(Str : string; ary : array of String) : boolean;
var
  i: Integer;
begin
  Result := Length(ary)=0;
  for i := 0 to Length(ary) - 1 do
  begin
    if CompareText(ary[i],Str) = 0 then
    begin
      Result := True;
      break;
    end;
  end;
end;


function ObjectToJSON(Obj : TPersistent; PropList : array of String; WriteClass : boolean = false; DateFormat : PJSONDateFormatter = nil) : String; overload;
var
  pl : PPropList;
  iCnt : integer;
  i: Integer;
  sVal : string;
  o : TObject;
begin
  if not Assigned(obj) then
    raise Exception.Create('Object is nil');
  if DateFormat = nil then
    DateFormat := @JSONDates;

  iCnt := GetPropList(Obj, pl);
  try
    Result := '{';
    if WriteClass then
    begin
      Result := Result+'"class" : "'+EncodeJSONString(obj.ClassName)+'"';
    end;
    for i := 0 to iCnt-1 do
    begin
      if not InArray(pl[i]^.Name, PropList) then
        Continue;
      sVal := '';
      case pl[i]^.PropType^^.Kind of
        tkInteger: sVal := IntToStr(GetOrdProp(obj,pl[i]));
        tkFloat  :
          begin
            if (pl[i].PropType^ = TypeInfo(TDateTime)) then
              sVal := DateFormat^.DateToString(GetFloatProp(obj,pl[i]),ddDateTime)
            else
              sVal := FloatToStr(GetFloatProp(obj,pl[i]));
          end;
        tkInt64  : sVal := IntToStr(GetInt64Prop(obj,pl[i]));

        tkChar   : sVal := '"'+EncodeJSONString(Char(GetOrdProp(obj,pl[i])))+'"';
        tkString : sVal := '"'+EncodeJSONString(GetStrProp(obj,pl[i]))+'"';
        tkUString: sVal := '"'+EncodeJSONString(GetStrProp(obj,pl[i]))+'"';
        tkWChar  : sVal := '"'+EncodeJSONString(WideChar(GetOrdProp(obj,pl[i])))+'"';
        tkLString: sVal := '"'+EncodeJSONString(GetStrProp(obj,pl[i]))+'"';
        tkWString: sVal := '"'+EncodeJSONString(GetWideStrProp(obj,pl[i]))+'"';

        tkEnumeration:
                   sVal := '"'+EncodeJSONString(GetEnumProp(obj,pl[i]))+'"';
        tkClass:
          begin
            o := GetObjectProp(Obj,pl[i]);
            if o is TPersistent then
              sVal := ObjectToJSON(TPersistent(o),PropList)
            else
              Continue;
          end;
        tkUnknown,
        tkSet,
        tkMethod,
        tkVariant,
        tkArray,
        tkRecord,
        tkInterface,
        tkDynArray: continue;
      end;
      if Result <> '{' then
        Result := Result+' , ';
      Result := Result + '"' + EncodeJSONString(pl[i]^.Name)+'" : '+sVal;
    end;
  finally
    FreeMem(pl);
  end;
  Result := Result+'}';
end;


// Detault date formatting functions that match the reference implmentation from http://json.org/json.js
// Made to be replaceable as everyone seems to do JSON dates differently.
// TODO: resolve to and from UTC automatically
function JSONDateToString(dt : TDateTime; Scope : TDateDetail = ddDateTime) : String;
  function PadIt(Number, Size : integer) : String;
  begin
    Result := IntToStr(Number);
    while length(Result) < Size do
      Result := '0'+Result;
  end;
begin
  Result := '"'+PadIt(YearOf(dt),4)+'-'+
            PadIt(MonthOf(dt),2)+'-'+
            PadIt(DayOf(dt),2)+'T'+
            PadIt(HourOf(dt),2)+':'+
            PadIt(MinuteOf(dt),2)+':'+
            PadIt(SecondOf(dt),2)+'Z"';
end;

function JSONStringToDate(dt : String) : TDateTime;
begin
  Result :=
    EncodeDateTime(
      StrToInt(Copy(dt,1,4)),
      StrToInt(Copy(dt,6,2)),
      StrToInt(Copy(dt,9,2)),
      StrToInt(Copy(dt,12,2)),
      StrToInt(Copy(dt,15,2)),
      StrToInt(Copy(dt,18,2)),0);
end;

function JSONStringIsDate(dt : String) : boolean;
var
  iTmp: integer;
begin
  Result := (Length(dt)=20) and
            (dt[5] = '-') and
            (dt[8] = '-') and
            (dt[11] = 'T') and
            (dt[14] = ':') and
            (dt[17] = ':') and
            (dt[20] = 'Z') and
            TryStrToInt(Copy(dt,1,4),iTmp) and
            TryStrToInt(Copy(dt,6,2),iTmp) and
            TryStrToInt(Copy(dt,9,2),iTmp) and
            TryStrToInt(Copy(dt,12,2),iTmp) and
            TryStrToInt(Copy(dt,15,2),iTmp) and
            TryStrToInt(Copy(dt,18,2),iTmp);
end;

procedure DebugWriteLn(s : string);
begin
  //Writeln(s);
end;

function DecodeJSONString(Value : String) : String;
begin
  Result := StringReplace(Value,'\"','"',[rfReplaceAll]);
  Result := StringReplace(Result,'\/','/',[rfReplaceAll]);
  Result := StringReplace(Result,'\b',#8,[rfReplaceAll]);
  Result := StringReplace(Result,'\t',#9,[rfReplaceAll]);
  Result := StringReplace(Result,'\r',#10,[rfReplaceAll]);
  Result := StringReplace(Result,'\f',#12,[rfReplaceAll]);
  Result := StringReplace(Result,'\n',#13,[rfReplaceAll]);
  Result := StringReplace(Result,'\\','\',[rfReplaceAll]);
end;

function EncodeJSONString(Value : String) : String;
var
  i: integer;
begin
  Result := Value;
  i := 1;
  while i < length(Result) do
  begin
    if Result[i] in ['"','\','/',#8,#9,#10,#12,#13] then
    begin
      case Result[i] of
        #8:  Result[i] := 'b';
        #9:  Result[i] := 't';
        #10: Result[i] := 'r';
        #12: Result[i] := 'f';
        #13: Result[i] := 'n';
      end;
      insert('\',Result,i);
      inc(i);
    end;
    inc(i);
  end;
end;

{ TJSONValueObject }

procedure TJSONValueObject.ClearProps;
var
  i: Integer;
begin
  for i := 0 to FProps.Count-1 do
  begin
    TJSONValue(FProps.Objects[i]).Free;
  end;
  FProps.Clear;
end;

constructor TJSONValueObject.Create(JSON : String; DateFormat : PJSONDateFormatter = nil);
begin
  inherited Create;
  FProps := TStringList.Create;
  FDateFormat := DateFormat;
  AsJSON := JSON;
end;

destructor TJSONValueObject.Destroy;
begin
  ClearProps;
  FProps.Free;
  inherited;
end;

function TJSONValueObject.getAsJSON: String;
var
  i: Integer;
begin
  Result := '{';
  for i := 0 to FProps.Count - 1 do
  begin
    if i > 0 then
      Result := Result+',';
    Result := Result+'"'+FProps[i]+'" : '+TJSONValue(FProps.Objects[i]).AsJSONValue;
  end;
  Result := Result+'}';
end;

function TJSONValueObject.getAsObject: Variant;
begin
  Result := JSONObjectFromJSON(Self);
end;

function TJSONValueObject.GetProps(Name: String): TJSONValue;
var
  idx: Integer;
begin
  idx := FProps.IndexOf(Name);
  if idx < 0 then
    idx := FProps.AddObject(Name, TJSONValue.Create('', FDateFormat));
  Result := TJSONValue(FProps.Objects[idx]);
end;

function TJSONValueObject.GetValues(idx: integer): TJSONValue;
begin
  Result := TJSONValue(FProps.Objects[idx]);
end;

procedure TJSONValueObject.setAsJSON(const Value: String);
var
  iPos, iLen: Integer;
  cThis : WideChar;
  function ReadNextChar : boolean;
  begin
    Result := False;
    inc(iPos);
    if iPos > iLen then
      exit;
    cThis := Value[iPos];
    Result := True;
  end;
  function ReadPropName : string;
  var
    iEnd : integer;
    iStart: Integer;
  begin
    while (cThis in Whitespace) do
      ReadNextChar;
    if cThis <> '"' then
      raise Exception.Create('Invalid value name.');
    iStart := iPos;
    ReadNextChar;
    while (cThis <>'"') do
      ReadNextChar;
    iEnd := iPos;
    while (cThis <>':') do
      ReadNextChar;
    Result := DecodeJSONString(Copy(Value,iStart+1,iEnd-iStart-1));
  end;
  function ReadValue(name : string) : TJSONValue;
  var
    iDepth : integer;
    iStart: Integer;
    bInString: Boolean;
    iCnt: Integer;
  begin
    while cThis in [':',#13,#10,#9, ' '] do
      ReadNextChar;
    //while (not (cThis in Whitespace)) do
      //ReadNextChar;
    iDepth := 0;
    iStart := iPos;
    bInString := False;
    while not ((cThis in ValueTerminators) and (iDepth = 0) and (not bInString)) do
    begin
      if Value[iPos-1] <> '\' then
        case cThis of
          '{': if not bInString then inc(iDepth);
          '[': if not bInString then inc(iDepth);
          '}': if not bInString then dec(iDepth);
          ']': if not bInString then dec(iDepth);
          '"': bInString := not bInString;
        end;
      if not ReadNextChar then
        break;
    end;
    iCnt := iPos-iStart;
    Result := TJSONValue.Create(TrimLeft(TrimRight(Copy(Value,iStart,iCnt))), FDateFormat);
  end;

  procedure ReadPropAndValue;
  var
    s : String;
    v: TJSONValue;
  begin
    ReadNextChar;

    s := ReadPropName;
    v := ReadValue(s);
    FProps.AddObject(s, v);

    if cThis = ',' then
      ReadPropAndValue;
  end;

  procedure ReadObject;
  begin
    while (cThis <> '{') do
      ReadNextChar;

    ReadPropAndValue;

    while (cThis <> '}') do
    begin
      if cThis = ',' then
        ReadPropAndValue;
      ReadNextChar;
    end;
  end;
begin
  FProps.Clear;
  if Value <> '' then
  begin
    iPos := 0;
    iLen := Length(Value);
    ReadNextChar;
    ReadObject;
  end;
end;

procedure TJSONValueObject.UpdateLastProperty(Name : string; Value: TJSONValueObject);
begin
  if Name <> '' then
  begin
    if Props[Name].ValueType in [jvtNull, jvtObject] then
    begin
      Props[Name].ClearObjectValue;
      Props[Name].FObjectValue := Value;
      Props[Name].FValueType := jvtObject;
      Value.AddRef;
    end;
  end;
end;

{ TJSONValue }

procedure TJSONValue.AssignArray(Value: Variant);
var
  i : integer;
begin
  if VarIsArray(Value) then
  begin
    ClearArrayValue;
    EnsureArrayValue;
    for i := VarArrayLowBound(Value,1) to VarArrayHighBound(Value,1) do
    begin
      case jvtTypeOfVariant(Value[i]) of
        jvtString:
          FArrayValue.Add(TJSONValue.Create('"'+EncodeJSONString(Value[i])+'"',FDateFormat));
        else
          FArrayValue.Add(TJSONValue.Create(Value[i],FDateFormat));
      end;
    end;
  end else
    raise Exception.Create('Value is not an Array');
end;

procedure TJSONValue.AssignObject(value: Variant);
begin
  if VarIsJSON(value) then
  begin
    if (FObjectValue <> TJSONVarData(Value).VObject) then
    begin
      if Assigned(FObjectValue) then
        ClearObjectValue;
      FObjectValue := TJSONVarData(Value).VObject;
      FObjectValue.AddRef;
    end;
  end else
  begin
    raise Exception.Create('Unknown object type for JSON.');
  end;
end;

procedure TJSONValue.ClearArrayValue;
var
  i: Integer;
begin
  if FArrayValue <> nil then
  begin
    for i := 0 to FArrayValue.Count-1 do
      TJSONValue(FArrayValue.Items[i]).Free;
    FArrayValue.Clear;
  end;
end;

procedure TJSONValue.ClearObjectValue;
begin
  if Assigned(FObjectValue) then
  begin
    FObjectValue.DecRef;
    if FObjectValue.RefCount = 0 then
      FreeAndNil(FObjectValue)
    else
      FObjectValue := nil;
  end;
end;

constructor TJSONValue.Create(const Value : String; DateFormat : PJSONDateFormatter = nil);
var
  fv: extended;
begin
  inherited Create;
  FDateFormat := DateFormat;

  if FDateFormat = nil then
    FDateFormat := @JSONDates;

  if Value <> '' then
  begin
    case lowercase(Value)[1] of
      '"':
        begin
          if FDateFormat^.StringIsDate(Copy(Value,2,length(Value)-2)) then
          begin
            FValueType := jvtNumber;
            fv := FDateFormat^.StringToDate(Copy(Value,2,length(Value)-2))
          end else
            FValueType := jvtString;
        end;
      '[': FValueType := jvtArray;
      '{': FValueType := jvtObject;
      't',
      'f': FValueType := jvtBoolean;
      'n': FValueType := jvtNull;
      else
        begin
          if not TryStrToFloat(TrimLeft(TrimRight(Value)),fv) then
            raise Exception.Create('Cannot create JSON value object for: '+Value);
          FValueType := jvtNumber;
        end;

    end;

  end else
    FValueType := jvtNull;

  case FValueType of
    jvtString: FStringValue := Copy(Value,2,length(Value)-2);
    jvtNumber: FNumberValue := fv;
    jvtBoolean: FBooleanValue := StrToBool(Value);
    jvtObject: FObjectValue := TJSONValueObject.Create(Value, FDateFormat);
    jvtArray:
      begin
        ArrayJSON := Copy(Value,2,length(Value)-2);
      end;
  end;
end;

destructor TJSONValue.Destroy;
begin
  ClearArrayValue;
  if Assigned(FArrayValue) then
    FArrayValue.Free;
  ClearObjectValue;
  FStringValue := '';
  inherited;
end;

procedure TJSONValue.EnsureArrayValue;
begin
  if not Assigned(FArrayValue) then
    FArrayValue := TJSONArrayList.Create;
end;

function TJSONValue.getArrayJSON: String;
var
  i: Integer;
begin
  EnsureArrayValue;
  Result := '';
  for i := 0 to FArrayValue.Count - 1 do
  begin
    if i <> 0 then
      Result := Result+',';
    Result := Result+TJSONValue(FArrayValue.Items[i]).AsJSONValue;
  end;
end;

function TJSONValue.getAsJSONValue: String;
begin
  Result := 'null';
  case FValueType of
    jvtString: Result := '"'+StringValue+'"';
    jvtNumber: Result := FloatToStr(NumberValue);
    jvtObject: Result := FObjectValue.AsJSON;
    jvtArray:  Result := '['+ArrayJSON+']';
    jvtBoolean: Result := Lowercase(BoolToStr(FBooleanValue,True)) ;
  end;
end;

function TJSONValue.getAsVariant: Variant;
begin
  Result := Null;
  case FValueType of
    jvtNull    : DebugWriteLn('returning variant.nullvalue');
    jvtString  : DebugWriteLn('returning variant.StringValue');
    jvtNumber  : DebugWriteLn('returning variant.NumberValue');
    jvtBoolean : DebugWriteLn('returning variant.BooleanValue');
    jvtObject  : DebugWriteLn('returning variant.ObjectFromJSON(ObjectValue.AsJSON)');
    jvtArray   : DebugWriteLn('returning variant.ObjectFromJSONArray(FArrayValue)');
  end;
  case FValueType of
    jvtNull    : Result := NewJSONObject;
    jvtString  : Result := StringValue;
    jvtNumber  : Result := NumberValue;
    jvtBoolean : Result := BooleanValue;
    jvtObject  : Result := JSONObjectFromJSON(ObjectValue.AsJSON);
    jvtArray   : Result := JSONObjectFromJSONArray(FArrayValue);
  end;
  DebugWriteLn('getAsVariant is back');
end;

function TJSONValue.GetStringValue: String;
begin
  Result := EncodeJSONString(FStringValue);
end;

procedure TJSONValue.setArrayJSON(const Value: String);
var
  iPos, iLen : integer;
  cThis : WideChar;
  procedure ReadNextChar(RaiseException : boolean = true);
  begin
    inc(iPos);
    if iPos > iLen then
      if RaiseException then
        raise Exception.Create('Read past JSON End of Line.')
      else
        exit;
    cThis := Value[iPos];
  end;
  function ReadPropName : String;
  var
    iEnd : integer;
    iStart: Integer;
  begin
    while (cThis in Whitespace) do
      ReadNextChar;
    if cThis <> '"' then
      raise Exception.Create('Invlalid value name.');
    iStart := iPos;
    ReadNextChar;
    while (cThis <>'"') do
      ReadNextChar;
    iEnd := iPos;
    while (cThis <>':') do
      ReadNextChar;
    Result := DecodeJSONString(Copy(Value,iStart+1,iEnd-iStart-1));
  end;
  function ReadValue : String;
  var
    iDepth : integer;
    iStart: Integer;
    bInString: Boolean;
    iCnt: Integer;
  begin
    while cThis in [':',#13,#10,#9,' '] do
      ReadNextChar;
    {while (not (cThis in Whitespace)) do
      ReadNextChar;}
    iDepth := 0;
    iStart := iPos;
    bInString := False;
    while not ((cThis in ValueTerminators) and (iDepth = 0) and (not bInString)) do
    begin
      if Value[iPos-1] <> '\' then
        case cThis of
          '{': if not bInString then inc(iDepth);
          '[': if not bInString then inc(iDepth);
          '}': if not bInString then dec(iDepth);
          ']': if not bInString then dec(iDepth);
          '"': bInString := not bInString;
        end;
      ReadNextChar(false);
      if (iPos > iLen) then
        break;
    end;
    iCnt := iPos-iStart;
    Result := TrimLeft(TrimRight(Copy(Value,iStart,iCnt)));
  end;

  function ReadPropAndValue : String;
  var
    s, v : String;
  begin
    ReadNextChar;

    s := ReadPropName;
    v := ReadValue;
    Result := '"'+s+'":'+v;

    if cThis = ',' then
      Result := Result+','+ReadPropAndValue;
  end;

  function ReadObject : String;
  begin
    while (cThis <> '{') do
      ReadNextChar;

    Result := '{'+ReadPropAndValue;

    while (cThis <> '}') do
    begin
      if cThis = ',' then
        Result := Result+','+ReadPropAndValue;
      ReadNextChar;
    end;
    Result := Result+'}';
  end;
begin
  if FValueType <> jvtArray then
    raise Exception.Create('Cannot set array values on a non array value type.');
  EnsureArrayValue;
  if Value = '' then
    exit;

  iPos := 0;
  iLen := Length(Value);
  repeat
    ReadNextChar;
  until not (cThis in Whitespace);

  repeat
    if cThis = ',' then
      ReadNextChar;
    case cThis of
      '{': FArrayValue.Add(TJSONValue.Create(ReadObject,FDateFormat));
      else FArrayValue.Add(TJSONValue.Create(ReadValue,FDateFormat));
    end;
    ReadNextChar(false);
  until iPos > iLen;
end;

procedure TJSONValue.setAsVariant(const Value: Variant);
var
  i : integer;
begin
  if FValueType <> jvtTypeOfVariant(Value) then
    FValueType := jvtTypeOfVariant(Value);
  case FValueType of
    jvtString  : StringValue := Value;
    jvtNumber  : NumberValue := Value;
    jvtBoolean : BooleanValue := Value;
    jvtObject  : AssignObject(Value);
    jvtArray   : AssignArray(Value);
  end;
end;

procedure TJSONValue.SetBooleanValue(const Value: Boolean);
begin
  FBooleanValue := Value;
end;

procedure TJSONValue.SetNumberValue(const Value: Extended);
begin
  FNumberValue := Value;
end;

procedure TJSONValue.SetObjectValue(const Value: TJSONValueObject);
begin
  FObjectValue := Value;
end;

procedure TJSONValue.SetStringValue(const Value: String);
begin
  FStringValue := DecodeJSONString(Value);
end;


{ TJSONArrayList }

function TJSONArrayList.getVariantValue(idx: integer): Variant;
var
  v: TJSONValue;
begin
  DebugWriteLn('getVariantValue('+IntToStr(idx)+')');
  Result := NULL;
  v := TJSONValue(Items[idx]);
  case v.ValueType of
    jvtString:  Result := v.StringValue;
    jvtNumber:  Result := v.NumberValue;
    jvtObject:  Result := v.ObjectValue.AsObject;
    jvtArray:   Result := v.StringValue;
    jvtBoolean: Result := v.BooleanValue;
  end;
end;

procedure TJSONArrayList.setVariantValue(idx: integer; const Value: Variant);
var
  v: TJSONValue;
begin
  DebugWriteLn('setVariantValue('+IntToStr(idx)+')');
  v := TJSONValue(Items[idx]);
  case v.ValueType of
    jvtString:  v.StringValue := Value;
    jvtNumber:  v.NumberValue := Value;
    jvtObject:;  //v.ObjectValue.AsObject := Value;
    jvtArray: ;  //v.StringValue := Value;
    jvtBoolean: v.BooleanValue := Value;
  end;
end;

{ TJSONNotificationObject }

procedure TJSONNotificationObject.AddRef;
begin
  Inc(FRefCount);
end;

constructor TJSONNotificationObject.Create;
begin
  inherited Create;
  FRefCount := 0;
  AddRef;
end;

procedure TJSONNotificationObject.DecRef;
begin
  Dec(FRefCount);
  assert(fRefCount >= 0);
end;

destructor TJSONNotificationObject.Destroy;
begin
  inherited;
end;

initialization
  JSONDates.DateToString := JSONDateToString;
  JSONDates.StringToDate := JSONStringToDate;
  JSONDates.StringIsDate := JSONStringIsDate;

end.
