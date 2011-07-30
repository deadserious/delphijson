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

unit jsonVariants;

interface

uses SysUtils, Classes, Variants, json;

type
  TJSONvarData = packed record
    VType : TVarType;
    VObject : TJSONValueObject;
    VSourceObject : TJSONValueObject;
    Reserved1: LongInt;
    Reserved2 : WordBool;
  end;

  TJSONVariantType = class(TInvokeableVariantType)
  protected
    procedure DispInvoke(Dest: PVarData; const Source: TVarData;
      CallDesc: PCallDesc; Params: Pointer); override;
  public
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: Boolean); override;
    function GetProperty(var Dest: TVarData; const V: TVarData;
      const Name: string): Boolean; override;
    function SetProperty(const V: TVarData; const Name: string;
      const Value: TVarData): Boolean; override;
  end;

type
  TJSONArrayvarData = packed record
    VType : TVarType;
    VList : TJSONArrayList;
    Reserved1, Reserved2: LongInt;
    Reserved3 : Word;
  end;

  TJSONArrayVariantType = class(TInvokeableVariantType)
  protected
    procedure DispInvoke(Dest: PVarData; const Source: TVarData;
      CallDesc: PCallDesc; Params: Pointer); override;
  public
    function IsClear(const V: TVarData): Boolean; override;
    function DoFunction(var Dest: TVarData; const V: TVarData;
      const Name: string; const Arguments: TVarDataArray): Boolean; override;
    function DoProcedure(const V: TVarData; const Name: string;
      const Arguments: TVarDataArray): Boolean; override;
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: Boolean); override;
    function GetProperty(var Dest: TVarData; const V: TVarData;
      const Name: string): Boolean; override;
    function SetProperty(const V: TVarData; const Name: string;
      const Value: TVarData): Boolean; override;
    function GetPropertyByIndex(var Dest: TVarData; const V: TVarData;
      const Name: string; const Index : integer): Boolean;
    function SetPropertyByIndex(const V: TVarData; const Name: string; const Index : integer;
      const Value: TVarData): Boolean;
  end;

function JSONVarType : TVarType;
function VarIsJSON(const AValue : Variant) : Boolean;
function VarAsJSON(const AValue : Variant) : TJSONObject;
function JSONObjectFromJSON(JSON : string; DateFormat : PJSONDateFormatter = nil) : TJSONObject; overload;
function JSONObjectFromJSON(JSON : TJSONValueObject) : TJSONObject; overload;
function JSONObjectToJSON(JSON : TJSONObject) : string;
function NewJSONObject(DateFormat : PJSONDateFormatter = nil) : TJSONObject;

function ObjectToJSONObject(Obj : TPersistent; WriteClass : boolean = false; DateFormat : PJSONDateFormatter = nil) : TJSONObject; overload;
function ObjectToJSONObject(Obj : TPersistent; PropList : array of String; WriteClass : boolean = false; DateFormat : PJSONDateFormatter = nil) : TJSONObject; overload;

function JSONArrayVarType : TVarType;
function VarIsJSONArray(const AValue : Variant) : Boolean;
function VarAsJSONArray(const AValue : Variant) : Variant;
function JSONObjectFromJSONArray(JSONArray : TJSONArrayList) : TJSONObject; overload;

implementation

var
  JSONVariant : TJSONVariantType;
  JSONArrayVariant : TJSONArrayVariantType;

function NewJSONObject(DateFormat : PJSONDateFormatter = nil) : TJSONObject;
var
  p : pointer;
begin
  DebugWriteLn('NewJSONObject');
  System.VarClear(Result);
  p := @Result;
  TJSONVarData(p^).VType := JSONVariant.VarType;
  TJSONVarData(p^).VObject := TJSONValueObject.Create('', DateFormat);
  TJSONVarData(p^).VSourceObject := nil;
end;

function JSONObjectFromJSON(JSON : string; DateFormat : PJSONDateFormatter = nil) : TJSONObject;
begin
  DebugWriteLn('ObjectFromJSON(JSON : string)');
  System.VarClear(Result);
  TJSONVarData(Result).VType := JSONVariant.VarType;
  TJSONVarData(Result).VObject := TJSONValueObject.Create(JSON, DateFormat);
  TJSONVarData(Result).VSourceObject := nil;
end;

function JSONObjectFromJSON(JSON : TJSONValueObject) : TJSONObject;
begin
  DebugWriteLn('ObjectFromJSON(JSON : TJSONObject)');
  System.VarClear(Result);
  TJSONVarData(Result).VType := JSONVariant.VarType;
  TJSONVarData(Result).VObject := JSON;
  TJSONVarData(Result).VSourceObject := nil;
  JSON.AddRef;
end;

function ObjectToJSONObject(Obj : TPersistent; WriteClass : boolean = false; DateFormat : PJSONDateFormatter = nil) : TJSONObject; overload;
begin
  Result := JSONObjectFromJSON(ObjectToJSON(Obj,WriteClass,DateFormat));
end;

function ObjectToJSONObject(Obj : TPersistent; PropList : array of String; WriteClass : boolean = false; DateFormat : PJSONDateFormatter = nil) : TJSONObject; overload;
begin
  Result := JSONObjectFromJSON(ObjectToJSON(Obj,PropList, WriteClass,DateFormat));
end;

function JSONObjectToJSON(JSON : Variant) : string;
begin
  Result := TJSONvarData(JSON).VObject.AsJSON;
end;

function JSONVarType : TVarType;
begin
  Result := JSONVariant.VarType;
end;

function VarIsJSON(const AValue : Variant) : Boolean;
begin
  Result := (TVarData(AValue).VType and varTypeMask) = JSONVarType;
end;

function VarAsJSON(const AValue : Variant) : Variant;
begin
  if not VarIsJSON(AValue) then
    VarCast(Result,AValue,JSONVarType)
  else
    Result := AValue;
end;

function JSONObjectFromJSONArray(JSONArray : TJSONArrayList) : TJSONObject;
begin
  DebugWriteLn('ObjectFromJSONArray(JSONArray : TJSONArrayList)');
  System.VarClear(Result);
  TJSONArrayvarData(Result).VType := JSONArrayVariant.VarType;
  TJSONArrayvarData(Result).VList := JSONArray;
end;

function JSONArrayVarType : TVarType;
begin
  Result := JSONArrayVariant.VarType;
end;

function VarIsJSONArray(const AValue : Variant) : Boolean;
begin
  Result := (TVarData(AValue).VType and varTypeMask) = JSONArrayVarType;
end;

function VarAsJSONArray(const AValue : Variant) : Variant;
begin
  if not VarIsJSONArray(AValue) then
    VarCast(Result,AValue,JSONArrayVarType)
  else
    Result := AValue;
end;


{ TJSONVariantType }

procedure TJSONVariantType.Clear(var V: TVarData);
begin
  DebugWriteLn('clearing json');
  TJSONVarData(V).VObject.DecRef;
  if TJSONVarData(V).VObject.RefCount = 0 then
    FreeAndNil(TJSONVarData(V).VObject)
  else
    TJSONVarData(V).VObject := nil;
  if Assigned(TJSONVarData(V).VSourceObject) then
  begin
    TJSONVarData(V).VSourceObject.DecRef;
    if TJSONVarData(V).VSourceObject.RefCount = 0 then
      FreeAndNil(TJSONVarData(V).VSourceObject)
    else
      TJSONVarData(V).VSourceObject := nil;
  end;
end;

procedure TJSONVariantType.Copy(var Dest: TVarData; const Source: TVarData;
  const Indirect: Boolean);
var
  s: String;
  o: TJSONValueObject;
begin
  DebugWriteLn('copying json');
  if not (Indirect and VarDataIsByRef(Source)) then
  begin
    TJSONVarData(Dest).VType := VarType;
    TJSONVarData(Dest).VObject := TJSONVarData(Source).VObject;
    TJSONVarData(Source).VObject.AddRef;
    TJSONVarData(Dest).VSourceObject := TJSONVarData(Source).VSourceObject;
    if Assigned(TJSONVarData(Dest).VSourceObject) then
      TJSONVarData(Dest).VSourceObject.AddRef;
  end else
    VarDataCopyNoInd(Dest,Source);
end;

procedure TJSONVariantType.DispInvoke(Dest: PVarData; const Source: TVarData;
  CallDesc: PCallDesc; Params: Pointer);
type
  PParamRec = ^TParamRec;
  TParamRec = array[0..3] of LongInt;
  TStringDesc = record
    BStr: WideString;
    PStr: PAnsiString;
  end;
const
  CDoMethod    = $01;
  CPropertyGet = $02;
  CPropertySet = $04;
var
  I, LArgCount: Integer;
  LIdent: string;
  LCasedIdent : string;
  LTemp: TVarData;
  VarParams : TVarDataArray;
  Strings: TStringRefList;
  LDest: PVarData;
  v : variant;
begin
  // Grab the identifier
  LArgCount := CallDesc^.ArgCount;
  LCasedIdent := AnsiString(PAnsiChar(@CallDesc^.ArgTypes[LArgCount]));
  LIdent := FixupIdent(LCasedIdent);

  FillChar(Strings, SizeOf(Strings), 0);
  VarParams := GetDispatchInvokeArgs(CallDesc, Params, Strings, true);

  // What type of invoke is this?
  case CallDesc^.CallType of
    CPropertyGet: begin
      if ((Dest <> nil) and                         // there must be a dest
              (LArgCount = 0)) then                       // only no args
      begin
        if TJSONVarData(Source).VObject.Props[LCasedIdent].ValueType = jvtArray then
        begin
          v := JSONObjectFromJSONArray(TJSONVarData(Source).VObject.Props[LCasedIdent].ArrayValue);
          if VarIsNull(v) then
            RaiseDispError;
          Variant(Dest^) := v;
        end else
        begin
          if not GetProperty(Dest^, Source, LIdent) then
            RaiseDispError;
        end;
      end else
      begin
        if not ((Dest <> nil) and                         // there must be a dest
              (LArgCount = 1)) then
          RaiseDispError;
        v := TJSONVarData(Source).VObject.Props[LCasedIdent].AsVariant.item(VarParams[0].VInteger);
        if VarIsNull(v) then
          RaiseDispError;
        Variant(Dest^) := v;
      end;
    end;
    CPropertySet:
      if ((Dest = nil) and                         // there must be a dest
          (LArgCount = 1)) then
      begin
        TJSONVarData(Source).VObject.Props[LCasedIdent].AsVariant := Variant(VarParams[0]);
        if Assigned(TJSONVarData(Source).VSourceObject) then
          TJSONVarData(Source).VSourceObject.UpdateLastProperty(TJSONVarData(source).vobject.lastpropertyname, TJSONVarData(Source).VObject);
      end else
      begin
        if not ((Dest = nil) and                         // there must be a dest
              (LArgCount = 2)) then
          RaiseDispError;
        TJSONVarData(Source).VObject.Props[LCasedIdent].AsVariant.item(VarParams[0].VInteger, Variant(VarParams[1]));
        if Assigned(TJSONVarData(Source).VSourceObject) then
          TJSONVarData(Source).VSourceObject.UpdateLastProperty(TJSONVarData(source).vobject.lastpropertyname, TJSONVarData(Source).VObject);
      end;
  else
    if ((Dest <> nil) and                         // there must be a dest
        (LArgCount = 0) and
        (LIdent <> 'COUNT') and
        (LIdent <> 'ASJSON')) then                       // only no args
    begin
      if TJSONVarData(Source).VObject.Props[LCasedIdent].ValueType = jvtArray then
      begin
        v := JSONObjectFromJSONArray(TJSONVarData(Source).VObject.Props[LCasedIdent].ArrayValue);
        if VarIsNull(v) then
          RaiseDispError;
        Variant(Dest^) := v;
        if Assigned(TJSONVarData(Source).VSourceObject) then
          TJSONVarData(Source).VSourceObject.UpdateLastProperty(TJSONVarData(source).vobject.lastpropertyname, TJSONVarData(Source).VObject);
      end else
      begin
        if not GetProperty(Dest^, Source, LIdent) then
          RaiseDispError;
        if Assigned(TJSONVarData(Source).VSourceObject) then
          TJSONVarData(Source).VSourceObject.UpdateLastProperty(TJSONVarData(source).vobject.lastpropertyname, TJSONVarData(Source).VObject);
      end;
    end else
    begin
      inherited;
      exit;
    end;
  end;

  if Dest <> nil then
  begin
    if VarIsJSON(Variant(Source)) and VarIsJSON(Variant(Dest^)) then
    begin
      TJSONvarData(Dest^).VSourceObject := TJSONVarData(Source).VObject;
      TJSONvarData(Dest^).VSourceObject.AddRef;
      TJSONvarData(Dest^).VObject.LastPropertyName := LCasedIdent;
    end;
  end;
  for I := 0 to Length(Strings) - 1 do
  begin
    if Pointer(Strings[I].Wide) = nil then
      Break;
    if Strings[I].Ansi <> nil then
      Strings[I].Ansi^ := AnsiString(Strings[I].Wide)
    else if Strings[I].Unicode <> nil then
      Strings[I].Unicode^ := UnicodeString(Strings[I].Wide)
  end;
end;

function TJSONVariantType.GetProperty(var Dest: TVarData; const V: TVarData;
  const Name: string): Boolean;
begin
  DebugWriteLn('getting JSON.'+Name);
  Result := False;
  if Name = 'ASJSON' then
  begin
    Variant(Dest) := TJSONvarData(V).VObject.AsJSON;
    Result := True;
  end else
  begin
    Variant(Dest) := TJSONvarData(V).VObject.Props[Name].AsVariant;
    Result := True;
  end;
  DebugWriteLn('got JSON.'+Name);
end;

function TJSONVariantType.SetProperty(const V: TVarData; const Name: string;
  const Value: TVarData): Boolean;
begin
  DebugWriteLn('setting JSON.'+Name);
  Result := False;
  if Name = 'ASJSON' then
  begin
    TJSONvarData(V).VObject.AsJSON := Variant(Value);
    Result := True;
  end else
  begin
    TJSONvarData(V).VObject.Props[Name].AsVariant  := Variant(Value);
    Result := True;
  end;
  DebugWriteLn('set JSON.'+Name);
end;


{ TJSONArrayVariantType }

procedure TJSONArrayVariantType.Clear(var V: TVarData);
begin
  DebugWriteLn('clearing array');
  TjsonArrayVarData(v).VList := nil;
end;

procedure TJSONArrayVariantType.Copy(var Dest: TVarData; const Source: TVarData;
  const Indirect: Boolean);
var
  s: String;
  o: TJSONArrayList;
begin
  DebugWriteLn('copying array');
  if not (Indirect and VarDataIsByRef(Source)) then
  begin
    TJSONArrayVarData(Dest).VType := VarType;
    o := TJSONArrayVarData(Source).VList;
    TJSONArrayVarData(Dest).VList := o;
  end else
    VarDataCopyNoInd(Dest,Source);
end;

procedure TJSONArrayVariantType.DispInvoke(Dest: PVarData;
  const Source: TVarData; CallDesc: PCallDesc; Params: Pointer);
type
  PParamRec = ^TParamRec;
  TParamRec = array[0..3] of LongInt;
  TStringDesc = record
    BStr: WideString;
    PStr: PAnsiString;
  end;
const
  CDoMethod    = $01;
  CPropertyGet = $02;
  CPropertySet = $04;
var
  I, LArgCount: Integer;
  LIdent: string;
  LCasedIdent : string;
  LTemp: TVarData;
  VarParams : TVarDataArray;
  Strings: TStringRefList;
begin
  // Grab the identifier
  LArgCount := CallDesc^.ArgCount;
  LCasedIdent := AnsiString(PAnsiChar(@CallDesc^.ArgTypes[LArgCount]));
  LIdent := FixupIdent(LCasedIdent);

  FillChar(Strings, SizeOf(Strings), 0);
  VarParams := GetDispatchInvokeArgs(CallDesc, Params, Strings, true);

  // What type of invoke is this?
  case CallDesc^.CallType of
    CPropertyGet:
      if not ((Dest <> nil) and                         // there must be a dest
              (LArgCount = 0) and                       // only no args
              GetProperty(Dest^, Source, LIdent)) then  // get op be valid
      else if not ((Dest <> nil) and                         // there must be a dest
              (LArgCount = 1) and                       // only no args
              GetPropertyByIndex(Dest^, Source, LIdent, VarParams[0].VInteger)) then  // get op be valid
        RaiseDispError;

    CPropertySet:
      if not ((Dest = nil) and                          // there can't be a dest
              (LArgCount = 1) and                       // can only be one arg
              SetProperty(Source, LIdent, VarParams[0])) then // set op be valid
      else if not ((Dest = nil) and                          // there can't be a dest
              (LArgCount = 2) and                       // can only be one arg
              SetPropertyByIndex(Source, LIdent, VarParams[0].VInteger, VarParams[1])) then // set op be valid
        RaiseDispError;
  else
    inherited;
    exit;
  end;

  for I := 0 to Length(Strings) - 1 do
  begin
    if Pointer(Strings[I].Wide) = nil then
      Break;
    if Strings[I].Ansi <> nil then
      Strings[I].Ansi^ := AnsiString(Strings[I].Wide)
    else if Strings[I].Unicode <> nil then
      Strings[I].Unicode^ := UnicodeString(Strings[I].Wide)
  end;
end;

function TJSONArrayVariantType.DoFunction(var Dest: TVarData; const V: TVarData;
  const Name: string; const Arguments: TVarDataArray): Boolean;
begin
  DebugWriteLn('returning Array.'+Name);
  Result := False;
  if (Name = 'INDEX') or (Name = 'ITEM')  then
  begin
    Variant(Dest) := TJSONArrayvarData(V).VList.Values[Variant(Arguments[0])];
    Result := True;
  end else if Name = 'COUNT'  then
  begin
    Variant(Dest) := TJSONArrayvarData(V).VList.Count;
    Result := True;
  end;
end;

function TJSONArrayVariantType.DoProcedure(const V: TVarData;
  const Name: string; const Arguments: TVarDataArray): Boolean;
begin
  DebugWriteLn('calling Array.'+Name);
  Result := False;
  if (Name = 'INDEX') or (Name = 'ITEM') then
  begin
    TJSONArrayvarData(V).VList.Values[Variant(Arguments[0])] := Variant(Arguments[1]);
    Result := True;
  end;
end;

function TJSONArrayVariantType.GetProperty(var Dest: TVarData; const V: TVarData;
  const Name: string): Boolean;
begin
  if Name = 'COUNT'  then
  begin
    Variant(Dest) := TJSONArrayvarData(V).VList.Count;
    Result := True;
  end;
end;

function TJSONArrayVariantType.GetPropertyByIndex(var Dest: TVarData;
  const V: TVarData; const Name: string; const Index: integer): Boolean;
begin
  Variant(Dest) := TJSONArrayvarData(V).VList.Values[Index];
  Result := True;
end;

function TJSONArrayVariantType.IsClear(const V: TVarData): Boolean;
begin
  Result := TjsonArrayVarData(v).VList = nil;
end;

function TJSONArrayVariantType.SetProperty(const V: TVarData; const Name: string;
  const Value: TVarData): Boolean;
begin
  Result := False;
end;

function TJSONArrayVariantType.SetPropertyByIndex(const V: TVarData;
  const Name: string; const Index: integer; const Value: TVarData): Boolean;
begin
  Result := False;
end;

initialization
  JSONVariant := TJSONVariantType.Create;
  JSONArrayVariant := TJSONArrayVariantType.Create;

finalization
  FreeAndNil(JSONVariant);
  FreeAndNil(JSONArrayVariant);


end.
