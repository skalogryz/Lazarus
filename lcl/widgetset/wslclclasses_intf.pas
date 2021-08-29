{ $Id$}
{
 *****************************************************************************
 *                          wslclclasses_intf.pas                            *
 *                          ---------------------                            *
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
  Classes, SysUtils, LCLProc, AVL_Tree;

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
  IWSLCLComponent = interface
    ['{3B826F70-BEA2-4F4D-B6EE-A10335918977}']
    function DebugName: shortstring;
    // this is a legacy implementation support. WSPrivate should be removed
    procedure SetPrivateClass(AWSPrivate: TWSPrivateClass);
    function WSPrivate: TWSPrivateClass;
  end;
  TWSLCLComponent = class(TInterfacedObject, IWSLCLComponent)
  private
    fWSPrivate: TWSPrivateClass;
  protected
    function DebugName: shortstring;
    procedure SetPrivateClass(AWSPrivate: TWSPrivateClass); deprecated;
    function WSPrivate: TWSPrivateClass;
  end;
  TWSLCLComponentClass = IWSLCLComponent; // for LCL compatibility

  { TWSLCLHandleComponent }

  IWSLCLReferenceComponent = interface(IWSLCLComponent)
    ['{789375CC-3B5B-43AB-AC4B-CAF21BAA9322}']
    procedure DestroyReference(AComponent: TComponent);
  end;

  { TWSLCLReferenceComponent }

  TWSLCLReferenceComponent = class(TWSLCLComponent, IWSLCLReferenceComponent)
  impsection
    imptype procedure DestroyReference(AComponent: TComponent); virtual;
  end;


function FindWSComponentClass(const AComponent: TComponentClass): IWSLCLComponent;
{$ifndef WSINTF}
function IsWSComponentInheritsFrom(const AComponent: TComponentClass;
  InheritFromClass: TWSLCLComponentClass): Boolean;
{$endif}
procedure RegisterWSComponent(AComponent: TComponentClass;
  AWSComponent: TWSLCLComponentClass; AWSPrivate: TWSPrivateClass); deprecated; // don't use PrivateClass anymore
procedure RegisterWSComponent(AComponent: TComponentClass;
  AWSComponent: TWSLCLComponentClass);
function RegisterNewWSComp(AComponent: TComponentClass): IWSLCLComponent; //inline;

// Only for non-TComponent based objects
function GetWSLazAccessibleObject: TWSObjectClass;
procedure RegisterWSLazAccessibleObject(const AWSObject: TWSObjectClass);

function GetWSLazDeviceAPIs: TWSObjectClass;
procedure RegisterWSLazDeviceAPIs(const AWSObject: TWSObjectClass);
// ~bk Search for already registered classes
function FindWSRegistered(const AComponent: TComponentClass): IWSLCLComponent; //inline;

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
    WSClass: IWSLCLComponent; { WidgetSet specific implementation class }
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

  { TWSClassesList }

  // Holds list of already registered TWidgetSetClass'es so TLCLComponent.NewInstance
  // can find faster the WidgetSetClass of the newinstance.

type

  { TClassNodeAVLManager }

  TClassNodeAVLManager = class(TBaseAVLTreeNodeManager)
  public
    procedure DisposeNode(ANode: TAVLTreeNode); override;
    function NewNode: TAVLTreeNode; override;
  end;

  { TClassNodeAVLTree }

  TClassNodeAVLTree = class(TAVLTree)
  public
    function FindByLCLComponent(AComponent: TClass): TClassNode;
    function FindAVLByLCLComponent(AComponent: TClass): TAVLTreeNode;
  end;

function ComparePtrUInt(d1, d2: Pointer): integer;
begin
  if PtrUInt(d1) = PtrUInt(d2) then Result := 0
  else if PtrUInt(d1) < PtrUInt(d2) then Result := -1
  else Result := 1;
end;

function CompareByClass(Data1, Data2:Pointer): Integer;
var
  lcl : TClass;
  c2  : TClassNode;
begin
  lcl := TClass(Data1);
  c2 := TClassNode(data2);
  Result := ComparePtrUInt(lcl, TClassNode(data2).LCLClass);
end;

function CompareClassNodes(Data1, Data2: Pointer): integer;
var
  c1, c2: TClassNode;
begin
  c1 := TClassNode(Data1);
  c2 := TClassNode(Data2);
  Result := ComparePtrUInt(c1.LCLCLass, c2.LCLClass);
end;

{ TClassNodeAVLManager }

procedure TClassNodeAVLManager.DisposeNode(ANode: TAVLTreeNode);
begin
  if Assigned(ANode) then begin
    if Assigned(ANode.Data) then TClassNode(ANode.Data).Free;
    ANode.Free;
  end;
end;

function TClassNodeAVLManager.NewNode: TAVLTreeNode;
begin
  Result := TAVLTreeNode.Create;
end;

function TClassNodeAVLTree.FindByLCLComponent(AComponent: TClass): TClassNode;
var
  avl : TAVLTreeNode;
begin
  avl := FindAVLByLCLComponent(AComponent);
  if not Assigned(avl) then Result := nil
  else Result := TClassNode(avl.Data);
end;

function TClassNodeAVLTree.FindAVLByLCLComponent(AComponent: TClass): TAVLTreeNode;
begin
  Result := FindKey(AComponent, @CompareByClass);
end;

var
  WSClassesList: TClassNodeAVLTree = nil;
  WSAnyPrivate: Boolean = false;
  WSLazAccessibleObjectClass: TWSObjectClass;
  WSLazDeviceAPIsClass: TWSObjectClass;

function FindClassNode(const AComponent: TComponentClass): TClassNode;
begin
  if not Assigned(WSClassesList) then
    DoInitialization;
  Result := WSClassesList.FindByLCLComponent(AComponent);
end;

function FindWSComponentClass(const AComponent: TComponentClass): IWSLCLComponent;
begin
  //Result := FindWSRegistered(AComponent);
  Result := RegisterNewWSComp(AComponent);
end;

// doesn't check for duplicated values (existing registrations)
procedure RegisterWSComponentInt(AComponent: TComponentClass;
  AWSComponent: IWSLCLComponent);
var
  p  : TClassNode;
  c  : TClass;
  nd : TClassNode;
begin
  p := TClassNode.Create;
  p.WSClass := AWSComponent;
  p.LCLClass := AComponent;
  WSClassesList.Add(Pointer(p));
  if Assigned(p.WSClass.WSPrivate) then
    WSAnyPrivate:=true;

  if not (WSAnyPrivate) then Exit;

  // WSPrivate are used by Gtk2.
  if not Assigned(p.WSClass.WSPrivate) then begin
    // searching for all the prior classes, IF they have any privates to be used
    c := Acomponent.ClassParent;
    while (c.InheritsFrom(TComponent)) do begin
      nd := FindClassNode(TComponentClass(c));
      if Assigned(nd) and Assigned(nd.WSClass.WSPrivate) then begin
        p.WSClass.SetPrivateClass(nd.WSClass.WSPrivate);
        Exit;
      end;
      c := c.ClassParent;
    end;
  end;
end;

// Do not create VClass at runtime but use normal Object Pascal class creation.
function RegisterNewWSComp(AComponent: TComponentClass): IWSLCLComponent;
var
  p : TComponentClass;
  reg : TList;
  i   : integer;
  c   : TClass;
begin
  (* RegisterNewWSComp should only be called, if a previous FindWSRegistered failed
     => WSClassesList should be created already *)
  Assert(Assigned(WSClassesList), 'RegisterNewWSComp: WSClassesList=Nil');
  Result := FindWSRegistered(AComponent);
  if (Result = nil) and (AComponent.InheritsFrom(TComponent)) then begin
    Result := FindWSComponentClass(TComponentClass(AComponent.ClassParent));
    if Assigned(Result) then
      RegisterWSComponentInt(AComponent, Result);
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

function TWSLCLComponent.DebugName: shortstring;
begin
  Result := Self.ClassName;
end;

procedure TWSLCLComponent.SetPrivateClass(AWSPrivate: TWSPrivateClass);
begin
  fWSPrivate:=AWSPrivate;
end;

function TWSLCLComponent.WSPrivate: TWSPrivateClass;
begin
  Result := fWSPrivate;
end;


{ TWSLCLHandleComponent }

imptype procedure TWSLCLReferenceComponent.DestroyReference(AComponent: TComponent);
begin
end;

procedure DoInitialization;
begin
  //WSClassesList := TFPList.Create;
  WSClassesList := TClassNodeAVLTree.Create(@CompareClassNodes);
  WSClassesList.SetNodeManager(TClassNodeAVLManager.Create, true);
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
  if Assigned(WSClassesList) then
    WSClassesList.FreeAndClear;
  FreeAndNil(WSClassesList);
  {$IFDEF VerboseWSBrunoK}
  Write('Press enter to quit > '); ReadLn;
  {$ENDIF}
end;

function FindWSRegistered(const AComponent: TComponentClass): IWSLCLComponent; //inline;
var
  nd : TClassNode;
begin
  if not Assigned(WSClassesList) then
    DoInitialization;
  nd := FindClassNode(AComponent);
  if Assigned(nd) then
    Result := nd.WSClass
  else
    Result:=nil;
end;

procedure RegisterWSComponent(AComponent: TComponentClass;
  AWSComponent: TWSLCLComponentClass);
var
  idx : integer;
  p   : TClassNode;
  avl : TAVLTreeNode;
begin
  if not Assigned(WSClassesList) then
    DoInitialization;
  avl := WSClassesList.FindAVLByLCLComponent(AComponent);
  if Assigned(avl) then
  begin
    if (TClassNode(avl.Data).WSClass = AWSComponent) then
      // it's already registered
      Exit;
    // previously was something different
    WSClassesList.Delete(avl);
  end;
  RegisterWSComponentInt(AComponent, AWSComponent);
end;

procedure RegisterWSComponent(AComponent: TComponentClass;
  AWSComponent: TWSLCLComponentClass; AWSPrivate: TWSPrivateClass);
begin
  if Assigned(AWSComponent) then
    AWSComponent.SetPrivateClass(AWSPrivate);
  RegisterWSComponentInt(AComponent, AWSComponent);
end;

finalization
  DoFinalization;
{$endif}

end.

