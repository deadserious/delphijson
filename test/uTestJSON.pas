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

unit uTestJSON;

interface

uses SysUtils, Types, Classes, json, Variants;

type
  TTestComponent = class(TComponent)
  private
    FDateCreated: TDateTime;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property DateCreated : TDateTime read FDateCreated;
  end;

procedure DoFromScratch;
procedure DoSimple;
procedure DoObject;
procedure DoDataset;

procedure RunTest(proc : TProcedure; Caption : string);

implementation

uses jsonVariants, jsonUtils;

const TESTSEP = '--------------------------------------------------------------------------------';

procedure RunTest(proc : TProcedure; Caption : string);
begin
  WriteLn('** '+Caption+' **');
  WriteLn;
  proc();
  WriteLn;
  WriteLn(TESTSEP);
end;

procedure DoFromScratch;
var
  j, a : TJSONObject;
begin
  // Create a new empty JSON object.
  j := NewJSONObject;

  // Start assigning properties.  Properties that don't already exist are
  // automatically added when assigned.
  j.x := 50;
  j.y := 45;
  j.text := 'This is my message';
  j.backgroundColor := 'white';
  j.color := '#123301';

  // Arrays must be assigned using Variant Arrays
  j.buttons := VarArrayOf(['Ok','Cancel']);

  // Object properties can be automatically created by simply accessing it
  // and assigning required properties
  j.User.FirstName := 'Fred';
  j.User.LastName := 'Willard';

  // The JSON object can be converted to a JSON string with the AsJSON method.
  WriteLn(j.AsJSON);
end;

procedure DoSimple;
var
  j : TJSONObject;
begin
  // Create a new JSON object from a JSON string
  j := JSONObjectFromJSON(
    '{' +#13#10+
    '  "firstName": "John",' +#13#10+
    '  "lastName": "Smith",' +#13#10+
    '  "address": {' +#13#10+
    '    "streetAddress": "21 2nd Street",' +#13#10+
    '    "city": "New York",' +#13#10+
    '    "state": "NY",' +#13#10+
    '    "postalCode": 10021' +#13#10+
    '  },' +#13#10+
    '  "phoneNumbers": [' +#13#10+
    '    "212 732-1234",' +#13#10+
    '    "646 123-4567"' +#13#10+
    '  ]' +
    '}');

  // Access JSON properties as actual object properties
  WriteLn('First Name is '+j.FirstName);
  WriteLn('Zip is '+IntToStr(j.address.postalCode));

  // Array type properties get a fake count property to indicate array size.
  WriteLn('Has a total of '+IntToStr(j.phoneNumbers.Count)+' Phone Numbers');

  // Array items can be accessed by standard delphi indexed property notation.
  WriteLn('Second Phone Number is '+j.phoneNumbers[1]);

  WriteLn;

  // Overwrite a few properties
  j.FirstName := 'Ted';
  j.PhoneNumbers[1] := '555-555-5555';

  // Generate JSON string for altered JSON object;
  WriteLn(j.AsJSON);
end;

procedure DoObject;
var
  cmp : TComponent;
  v : TJSONObject;
begin
  // Any TPersistent descendant can be streamed out to a JSON string.
  cmp := TTestComponent.Create(nil);
  try
    cmp.Name := 'test';

    // Write Component directly to a JSON string
    WriteLn(ObjectToJSON(cmp,True));

    // Write Component to JSON object for manipulation
    v := ObjectToJSONObject(cmp,True);
    v.Name := v.Name+'Altered';
    WriteLn(v.Class+'.'+v.Name+' was created at '+DateTimeToStr(v.DateCreated));
  finally
    cmp.Free;
  end;
end;

procedure DoDataset;
var
  v : TJSONObject;
  sl: TStringList;
  i: Integer;
  iCnt: Integer;
begin
  // This method shows how you can manipulate a sample JSON dataset stored in a
  // text file.  Implementation of JSON datasets differ based on the framework
  // supplying the data and you may have to alter your approach accordingly.

  sl := TStringList.Create;
  try
    // Load a JSON dataset from file.
    sl.LoadFromFile('dataset.json');

    // Create a JSON Object from the dataset.
    v := JSONObjectFromJSON(sl.Text);

    // Check the count of records in the JSON dataset
    iCnt := v.Data.Count;
    WriteLn('Record Count: '+IntToStr(iCnt));
    WriteLn;

    // Write out rows and fields in the dataset.
    for i := 0 to iCnt - 1 do
    begin
      WriteLn('Orig: Task: '+IntToStr(v.Data[i].TaskID)+#9+'Assigned: '+v.Data[i].AssignedTo);

      // Alter one field and write out altered record
      v.Data[i].AssignedTo := 'The Dudemeister';
      WriteLn('New:  Task: '+IntToStr(v.Data[i].TaskID)+#9+'Assigned: '+v.Data[i].AssignedTo);
    end;
    WriteLn;
    sl.Clear;

    // Write out newly altered dataset to new json file for inspection.
    sl.Text := JSONObjectToJSON(v);
    sl.SaveToFile('dataset_new.json');
  finally
    sl.Free;
  end;
end;

{ TTestComponent }

constructor TTestComponent.Create(AOwner: TComponent);
begin
  inherited;
  FDateCreated := Now;
end;

end.
