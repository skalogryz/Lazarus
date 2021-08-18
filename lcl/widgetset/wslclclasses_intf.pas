{ $Id$}
{
 *****************************************************************************
 *                              wslclclasses.pp                              *
 *                              ---------------                              *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit WSLCLClasses_Intf;

{$mode objfpc}{$H+}
{$I lcl_defines.inc}

{$ifndef wsintf}

interface

implementation

{$else}

{off$DEFINE VerboseWSRegistration}
{off$DEFINE VerboseWSRegistration_methods}
{off$DEFINE VerboseWSRegistration_treedump}
{.$DEFINE VerboseWSBrunoK }

interface
////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// 1) Only class methods allowed
// 2) Class methods have to be published and virtual
// 3) To get as little as possible circles, the uses
//    clause should contain only those LCL units
//    needed for registration. WSxxx units are OK
// 4) To improve speed, register only classes in the
//    initialization section which actually
//    implement something
// 5) To enable your XXX widgetset units, look at
//    the uses clause of the XXXintf.pp
////////////////////////////////////////////////////
uses
  Classes, SysUtils, LCLProc;

type
  { TWSPrivate }

  { Internal WidgetSet specific object tree }
  TWSPrivate = class(TObject)
  end;
  TWSPrivateClass = class of TWSPrivate;

  { For non-TComponent WS objects }
  TWSObject = class(TObject)
  public
  end;
  TWSObjectClass = class of TWSObject;

  { TWSLCLComponent }
  TWSLCLComponentClass = interface
    ['{3B826F70-BEA2-4F4D-B6EE-A10335918977}']
    function DebugName: string;
  end;
  TWSLCLComponent = class(TInterfacedObject, TWSLCLComponentClass)
    function DebugName: string;
  end;

  { TWSLCLHandleComponent }

  TWSLCLReferenceComponentClass = interface(TWSLCLComponentClass)
    ['{789375CC-3B5B-43AB-AC4B-CAF21BAA9322}']
    procedure DestroyReference(AComponent: TComponent);
  end;

  { TWSLCLReferenceComponent }

  TWSLCLReferenceComponent = class(TWSLCLComponent, TWSLCLReferenceComponentClass)
  impsection
    imptype procedure DestroyReference(AComponent: TComponent); virtual;
  end;


function FindWSComponentClass(const AComponent: TComponentClass): TWSLCLComponentClass;
{$ifndef WSINTF}
function IsWSComponentInheritsFrom(const AComponent: TComponentClass;
  InheritFromClass: TWSLCLComponentClass): Boolean;
procedure RegisterWSComponent(AComponent: TComponentClass;
  AWSComponent: TWSLCLComponentClass; AWSPrivate: TWSPrivateClass = nil);
{$endif}
function RegisterNewWSComp(AComponent: TComponentClass): TWSLCLComponentClass; //inline;

// Only for non-TComponent based objects
function GetWSLazAccessibleObject: TWSObjectClass;
procedure RegisterWSLazAccessibleObject(const AWSObject: TWSObjectClass);

function GetWSLazDeviceAPIs: TWSObjectClass;
procedure RegisterWSLazDeviceAPIs(const AWSObject: TWSObjectClass);
// ~bk Search for already registered classes
function FindWSRegistered(const AComponent: TComponentClass): TWSLCLComponentClass; //inline;
procedure RegisterWSComponent(AComponent: TComponentClass;
    AWSComponent: TWSLCLComponentClass);


{ Debug : Dump the WSClassesList nodes }
{$IFDEF VerboseWSBrunoK}
const
  cWSLCLDirectHit : integer = 0;
  cWSLCLParentHit : integer = 0;
  cWSLCLRegister : integer = 0;

procedure DumpWSClassesList;
{$ENDIF}

implementation

uses
  LCLClasses;

procedure DoInitialization; forward;

////////////////////////////////////////////////////
// Registration code
////////////////////////////////////////////////////
type
  TClassNode = class(TObject)
    LCLClass: TComponentClass;     { Class of the created instances }
    WSClass: TWSLCLComponentClass; { WidgetSet specific implementation class }
    //VClass: Pointer;               { Adjusted vmt table to handle WS virtual methods }
    //VClassName: ShortString;       { Class name attibuted when node was create }
    //VClassNew: Boolean;            { True Indicates that VClass=Parent.VClass.
    //                                 When True VClass is not runtime created }
    //Parent: PClassNode;
    //Child: PClassNode;
    //Sibling: PClassNode;
  end;

const
  // vmtAutoTable is something Delphi 2 and not used, we 'borrow' the vmt entry
  vmtWSPrivate = vmtAutoTable;

//type

  { TWSClassesList }

  // Holds list of already registered TWidgetSetClass'es so TLCLComponent.NewInstance
  // can find faster the WidgetSetClass of the newinstance.

  (*TWSClassesList = class(TFPList)
  private
    FLastFoundIdx: integer;
    FLastFoundClass: TClass;
    function FindWSClass(const AComponent: TComponentClass): TWSLCLComponentClass;
    function Get(Index: integer): PClassNode; inline;
    procedure Insert(aIndex: Integer; aItem: Pointer);
    function Search(const aItem: TClass; Out Index: integer): boolean;
    procedure UpdatLastFound(aClass: TClass; aIndex: integer);
    property Items[Index: integer]: PClassNode read Get; { write Put; default; }
    {$IFDEF VerboseWSBrunoK} {$ENDIF}
    {$IFDEF VerboseWSBrunoK}
    procedure DumpNode(aN : integer; aPClassNode : PClassNode);
    procedure DumpNodes;
    {$ENDIF}
  public
    constructor Create;
  end;*)

var
  WSClassesList: TFPList = nil;
  WSLazAccessibleObjectClass: TWSObjectClass;
  WSLazDeviceAPIsClass: TWSObjectClass;

{function FindNodeParent(AComponent: TClass): TClassNode;
var
  idx: integer;
begin
  while AComponent <> nil do begin
    if WSClassesList.Search(AComponent, idx) then
      Exit(PClassNode(WSClassesList[idx]));
    AComponent := AComponent.ClassParent;
  end;
  Result := nil;
end;}

function FindClassIndex(const AComponent: TComponentClass): Integer;
var
  n : TClassNode;
  i : integer;
begin
  if not Assigned(WSClassesList) then
    DoInitialization;
  Result := -1;
  for i:=0 to WSClassesList.Count-1 do begin
    n := TClassNode(WSClassesList[i]);
    if n.LCLClass = AComponent then begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;


function FindClassNode(const AComponent: TComponentClass): TClassNode;
var
  idx: integer;
begin
  Result := nil;
  idx := FindClassIndex(AComponent);
  if (idx>=0) then
    Exit(TClassNode(WSClassesList[idx]));
  //Result := FindNodeParent(AComponent.ClassParent);
end;

function FindWSComponentClass(const AComponent: TComponentClass): TWSLCLComponentClass;
begin
  //Result := FindWSRegistered(AComponent);
  Result := RegisterNewWSComp(AComponent);
end;

{$ifndef WSINTF}
function IsWSComponentInheritsFrom(const AComponent: TComponentClass;
  InheritFromClass: TWSLCLComponentClass): Boolean;
var
  Node: PClassNode;
begin
  Node := FindClassNode(AComponent);
  if Assigned(Node) then
    Result := TWSLCLComponentClass(Node^.WSClass).InheritsFrom(InheritFromClass)
  else
    Result := false;
end;
{$endif}


{function FindParentWSClassNode(const ANode: PClassNode): PClassNode;
begin
  Result := ANode^.Parent;
  while Result <> nil do begin
    if Result^.WSClass <> nil then Exit;
    Result := Result^.Parent;
  end;
  Result := nil;
end;}

function FindCommonAncestor(const AClass1, AClass2: TClass): TClass;
begin
  Result := AClass1;
  if AClass2.InheritsFrom(Result) then Exit;
  Result := AClass2;
  while Result <> nil do begin
    if AClass1.InheritsFrom(Result) then Exit;
    Result := Result.ClassParent;
  end;
  Result := nil;
end;

{$ifndef WSINTF}
procedure CreateVClass(const ANode: PClassNode;
  const AWSPrivate: TWSPrivateClass = nil; AOldPrivate: TClass = nil);
var
  ParentWSNode: PClassNode;
  CommonClass: TClass;
  Vvmt, Cvmt, Pvmt: PPointerArray;
  Cmnt: PMethodNameTable;
  SearchAddr: Pointer;
  n, idx: integer;
  WSPrivate, OrgPrivate: TClass;
  Processed: array of boolean;
  VvmtCount, VvmtSize: integer;
  {$IFDEF VerboseWSRegistration}
  Indent: string;
  {$ENDIF}
begin
  if AWSPrivate = nil then WSPrivate := TWSPrivate
  else WSPrivate := AWSPrivate;

  // Determine VMT count and size => http://wiki.freepascal.org/Compiler-generated_data_and_data_structures
  VvmtCount := 0;
  Vvmt := Pointer(ANode^.WSClass) + vmtMethodStart;
  // AWSComponent is equal to ANode^.WSClass;
  while (Vvmt^[VvmtCount] <> nil) do
    Inc(VvmtCount);                                { ~bk 1 more for nil at end }
  VvmtSize := vmtMethodStart + VvmtCount * SizeOf(Pointer) + SizeOf(Pointer);
  if ANode^.VClass = nil then begin
    ANode^.VClass := GetMem(VvmtSize);
  end
  else begin
    // keep original WSPrivate (only when different than default class)
    OrgPrivate := PClass(ANode^.VClass + vmtWSPrivate)^;

    if (OrgPrivate <> nil) and (OrgPrivate <> AOldPrivate) and
      OrgPrivate.InheritsFrom(WSPrivate) then begin
      {$IFDEF VerboseWSRegistration}
      DebugLn('Keep org private: ', WSPrivate.ClassName, ' -> ', OrgPrivate.ClassName);
      {$ENDIF}
      WSPrivate := OrgPrivate;
    end;
  end;

  // Initially copy the WSClass
  Move(Pointer(ANode^.WSClass)^, ANode^.VClass^, VvmtSize);

  // Set WSPrivate class
  ParentWSNode := FindParentWSClassNode(ANode);
  if ParentWSNode = nil then begin
    // nothing to do
    PClass(ANode^.VClass + vmtWSPrivate)^ := WSPrivate;
    {$IFDEF VerboseWSRegistration}
    DebugLn('Virtual parent: nil, WSPrivate: ', PClass(ANode^.VClass +
      vmtWSPrivate)^.ClassName);
    {$ENDIF}
    Exit;
  end;

  if WSPrivate = TWSPrivate then begin
    if ParentWSNode^.VClass = nil then begin
      DebugLN('[WARNING] Missing VClass for: ', ParentWSNode^.WSClass.ClassName);
      PClass(ANode^.VClass + vmtWSPrivate)^ := TWSPrivate;
    end
    else PClass(ANode^.VClass + vmtWSPrivate)^ :=
        PClass(ParentWSNode^.VClass + vmtWSPrivate)^;
  end
  else PClass(ANode^.VClass + vmtWSPrivate)^ := WSPrivate;

  {$IFDEF VerboseWSRegistration}
  DebugLn('Virtual parent: ', ParentWSNode^.WSClass.ClassName,
    ', WSPrivate: ', PClass(ANode^.VClass + vmtWSPrivate)^.ClassName);
  {$ENDIF}

  // Try to find the common ancestor
  CommonClass := FindCommonAncestor(ANode^.WSClass, ParentWSNode^.WSClass);
  {$IFDEF VerboseWSRegistration}
  DebugLn('Common: ', CommonClass.ClassName);
  Indent := '';
  {$ENDIF}

  Vvmt := ANode^.VClass + vmtMethodStart;
  Pvmt := ParentWSNode^.VClass + vmtMethodStart;
  SetLength(Processed, VvmtCount);

  while CommonClass <> nil do begin
    Cmnt := PPointer(Pointer(CommonClass) + vmtMethodTable)^;
    if Cmnt <> nil then begin
      {$IFDEF VerboseWSRegistration_methods}
      DebugLn(Indent, '*', CommonClass.ClassName, ' method count: ',
        IntToStr(Cmnt^.Count));
      Indent := Indent + ' ';
      {$ENDIF}

      Cvmt := Pointer(CommonClass) + vmtMethodStart;
      Assert(Cmnt^.Count < VvmtCount,
        'MethodTable count is larger than determined VvmtCount');

      // Loop through the VMT to see what is overridden
      for n := 0 to Cmnt^.Count - 1 do begin
        SearchAddr := Cmnt^.Entries[n].Addr;
        {$IFDEF VerboseWSRegistration_methods}
        DebugLn('%sSearch: %s (%p)', [Indent, Cmnt^.Entries[n].Name^, SearchAddr]);
        {$ENDIF}

        for idx := 0 to VvmtCount - 1 do begin
          if Cvmt^[idx] = SearchAddr then begin
            {$IFDEF VerboseWSRegistration_methods}
            DebugLn('%sFound at index: %d (v=%p p=%p)',
              [Indent, idx, Vvmt^[idx], Pvmt^[idx]]);
            {$ENDIF}

            if Processed[idx] then begin
              {$IFDEF VerboseWSRegistration_methods}
              DebugLn(Indent, 'Processed -> skipping');
              {$ENDIF}
              Break;
            end;
            Processed[idx] := True;

            if (Vvmt^[idx] = SearchAddr)  //original
              and (Pvmt^[idx] <> SearchAddr) //overridden by parent
            then begin
              {$IFDEF VerboseWSRegistration_methods}
              DebugLn('%sUpdating %p -> %p', [Indent, Vvmt^[idx], Pvmt^[idx]]);
              {$ENDIF}
              Vvmt^[idx] := Pvmt^[idx];
            end;

            Break;
          end;
          if idx = VvmtCount - 1 then begin
            DebugLn('[WARNING] VMT entry "', Cmnt^.Entries[n].Name^,
              '" not found in "', CommonClass.ClassName, '"');
            Break;
          end;
        end;
      end;
    end;
    CommonClass := Commonclass.ClassParent;
  end;

  // Adjust classname
  ANode^.VClassName := '(V)' + ANode^.WSClass.ClassName;
  PPointer(ANode^.VClass + vmtClassName)^ := @ANode^.VClassName;
  // Adjust classparent
  {$IF (FPC_FULLVERSION >= 30101)}
  PPointer(ANode^.VClass + vmtParent)^ := @ParentWSNode^.WSClass;
  {$ELSE}
  PPointer(ANode^.VClass + vmtParent)^ := ParentWSNode^.WSClass;
  {$ENDIF}
  // Delete methodtable entry
  PPointer(ANode^.VClass + vmtMethodTable)^ := nil;
end;
{$endif}

{ Get PClass node is recursive, we want to detect if a new node may be an
  unregistered intermediate in the ancestor class tree }
{$ifndef WSINTF}
function GetPClassNode(AClass: TClass; AWSComponent: TWSLCLComponentClass;
                       aParentGet: boolean; aLeaf: boolean): PClassNode;
var
  idx: Integer;
  lParentNode : PClassNode;
  lClassNode : TClassNode; { A temp local node to fake normal processing
                             of a node that won't be stored aParentGet = 0
                             and TWSLCLComponentClass = nil }
  lInsertNode : boolean;   { Indicator that New(Result) has been requested }
begin
  if (AClass = nil) or not (AClass.InheritsFrom(TLCLComponent)) then
    Exit(nil);

  if not WSClassesList.Search(AClass, idx) then
  begin
    lInsertNode := aParentGet or Assigned(AWSComponent);
    if lInsertNode then
      New(Result)
    else
      Result := @lClassNode;
    Result^.LCLClass := TComponentClass(AClass);
    Result^.WSClass := nil;
    Result^.VClass := nil;
    Result^.VClassName := '';
    Result^.VClassNew := aParentGet;
    Result^.Child := nil;
    lParentNode := GetPClassNode(AClass.ClassParent, AWSComponent, True, False);
    Result^.Parent := lParentNode;
    { Unregistered Intermediate nodes are patched with the parent information }
    if aParentGet then
    begin
      Result^.WSClass := lParentNode^.WSClass;
      Result^.VClass := lParentNode^.VClass;
      PPointer(Result^.VClass + vmtWSPrivate)^ := PPointer(lParentNode^.VClass + vmtWSPrivate)^;
      // Build a VClassName
      if aLeaf then        { Node that has an empty WSRegisterClass procedure }
        Result^.VClassName := '(L)' + Result^.WSClass.ClassName
      else                 { Internal node needed for tree consistency }
        Result^.VClassName := '(I)' + Result^.WSClass.ClassName
    end;
    if lParentNode = nil then
    begin
      Result^.Sibling := nil;
      if aLeaf then
        Result^.VClassName := '(ROOT)' + AClass.ClassName;
    end
    else if lInsertNode then
    begin
      Result^.Sibling := lParentNode^.Child;
      lParentNode^.Child := Result;
    end
    else
      Result^.Sibling := nil;
    if lInsertNode then
    begin
      WSClassesList.Search(aClass, idx);
      WSClassesList.Insert(idx, Result);
    end
    else
      Result := nil;
  end
  else
    Result := WSClassesList[idx];
end;
{$endif}

// Create VClass at runtime
{$ifndef WSINTF}
procedure RegisterWSComponent(AComponent: TComponentClass;
  AWSComponent: TWSLCLComponentClass; AWSPrivate: TWSPrivateClass = nil);

  procedure UpdateChildren(const ANode: PClassNode; AOldPrivate: TClass);
  var
    Node: PClassNode;
  begin
    Node := ANode^.Child;
    while Node <> nil do
    begin
      if (Node^.WSClass <> nil) and (not Node^.VClassNew) then
      begin
        {$IFDEF VerboseWSRegistration}
        DebugLn('Update VClass for: ', Node^.WSClass.ClassName);
        {$ENDIF}
        CreateVClass(Node, AWSPrivate, AOldPrivate);
      end;
      UpdateChildren(Node, AOldPrivate);
      Node := Node^.Sibling;
    end;
  end;

var
  Node: PClassNode;
  OldPrivate: TClass;
begin
  if not Assigned(WSClassesList) then
    DoInitialization;
  Node := GetPClassNode(AComponent, AWSComponent, False, True);
  if Node = nil then // No node created
    Exit;
  { If AWSComponent specified but node already exists, nothing more to do. }
  if Assigned(AWSComponent) and (Node^.WSClass = AWSComponent) then
    Exit;
  Node^.WSClass := AWSComponent;

  // childclasses "inherit" the private from their parent
  // the child privates should only be updated when their private is still
  // the same as their parents
  if Node^.VClass = nil then
    OldPrivate := nil
  else
    OldPrivate := PClass(Node^.VClass + vmtWSPrivate)^;

  {$IFDEF VerboseWSRegistration}
  DebugLn('Create VClass for: ', AComponent.ClassName, ' -> ', Node^.WSClass.ClassName);
  {$ENDIF}
  CreateVClass(Node, AWSPrivate);

  // Since child classes may depend on us, recreate them
  UpdateChildren(Node, OldPrivate);
end;
{$endif}

// Do not create VClass at runtime but use normal Object Pascal class creation.
function RegisterNewWSComp(AComponent: TComponentClass): TWSLCLComponentClass;
var
  p : TComponentClass;
  reg : TList;
  i   : integer;
  c   : TClass;
begin
  (* RegisterNewWSComp should only be called, if a previous FindWSRegistered failed
     => WSClassesList should be created already *)
  Assert(Assigned(WSClassesList), 'RegisterNewWSComp: WSClassesList=Nil');
  writeln('trying to find: ', AComponent.ClassName);
  Result := FindWSRegistered(AComponent);
  if (Result = nil) then begin
    reg := TList.Create;
    try
      while not Assigned(Result) do begin
        p := AComponent;
        reg.Add(p);
        c := p.ClassParent;
        writeln('class = ',c.className,' ',c.InheritsFrom(TComponent));
        if c.InheritsFrom(TComponent) then
          AComponent := TComponentClass(c)
        else
          Break;
        Result := FindWSComponentClass(AComponent);
      end;
      if Assigned(Result) then
        for i:=0 to reg.Count-1 do
          RegisterWSComponent(tComponentClass(reg[i]), Result);
    finally
      reg.Free;
    end;
  end;
end;


function GetWSLazAccessibleObject: TWSObjectClass;
begin
  Result := WSLazAccessibleObjectClass;
end;

procedure RegisterWSLazAccessibleObject(const AWSObject: TWSObjectClass);
begin
  WSLazAccessibleObjectClass := AWSObject;
end;

function GetWSLazDeviceAPIs: TWSObjectClass;
begin
  Result := WSLazDeviceAPIsClass;
end;

procedure RegisterWSLazDeviceAPIs(const AWSObject: TWSObjectClass);
begin
  WSLazDeviceAPIsClass := AWSObject;
end;

 { TWSLCLComponent }

  function TWSLCLComponent.DebugName: string;
 begin
   Result := Self.ClassName;
 end;

{$ifndef WSINTF}
function FindWSRegistered(const AComponent: TComponentClass): TWSLCLComponentClass;
begin
  if not Assigned(WSClassesList) then
    DoInitialization;
  Result := WSClassesList.FindWSClass(AComponent);
end;
{$endif}

{$IFDEF VerboseWSBrunoK}
procedure DumpWSClassesList;
begin
  WSClassesList.DumpNodes;
end;
{$ENDIF}

{ TWSClassesList }

{$IFDEF VerboseWSBrunoK}
procedure TWSClassesList.DumpNode(aN: integer; aPClassNode: PClassNode);
var
  LCLClassClassName, lWSClassClassName, lVClassName, ParentVClassName: string;
  lClassNode : PClassNode;
begin
  with aPClassNode^ do begin
    if Assigned(LCLClass) then
      LCLClassClassName := LCLClass.ClassName
    else
      LCLClassClassName := '???';
    if Assigned(WSClass) then
      lWSClassClassName := WSClass.ClassName
    else
      lWSClassClassName := '???';
    if Assigned(VClass) then
      lVClassName := TClass(VClass).ClassName
    else
      lVClassName := '???';
    if Assigned(Parent) and  Assigned(PClassNode(Parent)^.WSClass) then
      ParentVClassName := PClassNode(Parent)^.WSClass.ClassName
    else
      ParentVClassName := '???';
    writeln(
      aN, ';',
      { DbgCreateSeq, ';', }
      HexStr(aPClassNode), ';',
      HexStr(LCLClass), ';',  // : TComponentClass;
      LCLClassClassName, ';',
      HexStr(WSClass), ';', // : TWSLCLComponentClass;
      lWSClassClassName, ';',
      HexStr(VClass), ';', // : Pointer;
      VClassName, ';',
      // VVmtCount, ';',
      lVClassName, ';',
      VClassNew, ';',    // : Boolean;
      HexStr(Parent), ';', // Parent: PClassNode;
      ParentVClassName, ';', // ShortString;
      HexStr(Child), ';',   // Child: PClassNode;
      HexStr(Sibling)  // Sibling: PClassNode;
      );
  end;
end;

procedure TWSClassesList.DumpNodes;
var
  i: integer;
begin
  WriteLn('n;',          // aN, ';',
    { 'CreateSeq;',        // DbgCreateSeq, ';', }
    'PClassNode;',        // Node
    'LCLClass;',         // HexStr(LCLClass), ';',  // : TComponentClass;
    'LCLClassName;',     // LCLClassClassName, ';',
    'WSClass;',          // HexStr(WSClass), ';', // : TWSLCLComponentClass
    'WSClassName;',      // lWSClassClassName, ';',
    'VClass;',           // HexStr(VClass), ';', // : Pointer;
    'VClassName;',       // VClassName
  {  'VVmtCount', }      // VVmtCount, ';',
    'VClassName;',       // lVClassName, ';',
    'VClassNew;',        // VClassNew,           ';',  // : Boolean;
    'Parent;',           // HexStr(Parent), ';', // Parent: PClassNode;
    'Parent.Name;',      // ParentClassName, ';', // ShortString;
    'Child;',            // HexStr(Child), ';',   // Child: PClassNode;
    'Sibling'            // HexStr(Sibling)  // Sibling: PClassNode;
    );
  for i := 0 to Count - 1 do
    DumpNode(i, PClassNode(Items[i]));
end;
{$ENDIF}

{ TWSLCLComponent }

{$ifndef WSINTF}
class function TWSLCLComponent.WSPrivate: TWSPrivateClass;
begin
  Result := TWSPrivateClass(PClass(Pointer(Self) + vmtWSPrivate)^);
end;
{$endif}

{ TWSLCLHandleComponent }

imptype procedure TWSLCLReferenceComponent.DestroyReference(AComponent: TComponent);
begin
end;

procedure DoInitialization;
begin
  WSClassesList := TFPList.Create;
end;

procedure DoFinalization;
var
  n: Integer;
  Node: TClassNode;
begin
  {$IFDEF VerboseWSBrunoK}
  WSClassesList.DumpNodes;
  WriteLn;
  WriteLn('cWSLCLDirectHit=', cWSLCLDirectHit,
          ' cWSLCLParentHit=', cWSLCLParentHit,
          ' cWSLCLRegister=', cWSLCLRegister);
  {$ENDIF}
  for n := 0 to WSClassesList.Count - 1 do
  begin
    Node := TClassNode(WSClassesList[n]);
    Node.Free;
  end;
  FreeAndNil(WSClassesList);
  {$IFDEF VerboseWSBrunoK}
  Write('Press enter to quit > '); ReadLn;
  {$ENDIF}
end;

{$ifdef WSINTF}

function FindWSRegistered(const AComponent: TComponentClass): TWSLCLComponentClass; //inline;
var
  idx : integer;
begin
  if not Assigned(WSClassesList) then
    DoInitialization;
  idx := FindClassIndex(AComponent);
  if (idx < 0) then Result:=nil
  else Result := TClassNode(WSClassesList[idx]).WSClass;
end;

procedure RegisterWSComponent(AComponent: TComponentClass;
  AWSComponent: TWSLCLComponentClass);
var
  idx : integer;
  p : TClassNode;
begin
  writeln('register: ', AComponent.ClassName);
  idx := FindClassIndex(AComponent);
  if (idx < 0) then begin
    p := TClassNode.Create;
    p.WSClass := AWSComponent;
    p.LCLClass := AComponent;
    WSClassesList.Add(p);
  end else
    TClassNode(WSClassesList[idx]).WSClass := AWSComponent;
end;
{$endif}

finalization
  DoFinalization;
{$endif}

end.

