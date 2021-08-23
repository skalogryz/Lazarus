{ $Id$}
{
 *****************************************************************************
 *                                WSMenus.pp                                 * 
 *                                ----------                                 * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit WSMenus;

{$mode objfpc}{$H+}
{$I lcl_defines.inc}

interface
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// 1) Only class methods allowed
// 2) Class methods have to be published and virtual
// 3) To get as little as posible circles, the uses
//    clause should contain only those LCL units 
//    needed for registration. WSxxx units are OK
// 4) To improve speed, register only classes in the 
//    initialization section which actually 
//    implement something
// 5) To enable your XXX widgetset units, look at
//    the uses clause of the XXXintf.pp
////////////////////////////////////////////////////
uses
  Classes,
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Menus, Graphics,
////////////////////////////////////////////////////
  {$ifdef wsintf}WSLCLClasses_Intf{$else}WSLCLClasses{$endif}, LCLType, WSFactory,
  LazUtilities, LazLogger;

type
  { TWSMenuItem }
  {$ifdef wsintf}
  TWSMenuItemClass = interface(TWSLCLComponentClass)
    ['{030AC85F-1A83-419D-A9E0-C6FEF0F0779C}']
    function  OpenCommand: LongInt;
    procedure CloseCommand(ACommand: LongInt);
    procedure AttachMenu(const AMenuItem: TMenuItem);
    function  CreateHandle(const AMenuItem: TMenuItem): HMENU;
    procedure DestroyHandle(const AMenuItem: TMenuItem);
    procedure SetCaption(const AMenuItem: TMenuItem; const ACaption: string);
    procedure SetShortCut(const AMenuItem: TMenuItem; const ShortCutK1, ShortCutK2: TShortCut);
    procedure SetVisible(const AMenuItem: TMenuItem; const Visible: boolean);
    function SetCheck(const AMenuItem: TMenuItem; const Checked: boolean): boolean;
    function SetEnable(const AMenuItem: TMenuItem; const Enabled: boolean): boolean;
    function SetRadioItem(const AMenuItem: TMenuItem; const RadioItem: boolean): boolean;
    function SetRightJustify(const AMenuItem: TMenuItem; const Justified: boolean): boolean;
    procedure UpdateMenuIcon(const AMenuItem: TMenuItem; const HasIcon: Boolean; const AIcon: TBitmap);
  end;
  {$endif}
  TWSMenuItem = class(TWSLCLComponent{$ifdef wsintf},TWSMenuItemClass{$endif})
  impsection
    imptype function  OpenCommand: LongInt; virtual;
    imptype procedure CloseCommand(ACommand: LongInt); virtual;
    imptype procedure AttachMenu(const AMenuItem: TMenuItem); virtual;
    imptype function  CreateHandle(const AMenuItem: TMenuItem): HMENU; virtual;
    imptype procedure DestroyHandle(const AMenuItem: TMenuItem); virtual;
    imptype procedure SetCaption(const AMenuItem: TMenuItem; const ACaption: string); virtual;
    imptype procedure SetShortCut(const AMenuItem: TMenuItem; const ShortCutK1, ShortCutK2: TShortCut); virtual;
    imptype procedure SetVisible(const AMenuItem: TMenuItem; const Visible: boolean); virtual;
    imptype function SetCheck(const AMenuItem: TMenuItem; const Checked: boolean): boolean; virtual;
    imptype function SetEnable(const AMenuItem: TMenuItem; const Enabled: boolean): boolean; virtual;
    imptype function SetRadioItem(const AMenuItem: TMenuItem; const RadioItem: boolean): boolean; virtual;
    imptype function SetRightJustify(const AMenuItem: TMenuItem; const Justified: boolean): boolean; virtual;
    imptype procedure UpdateMenuIcon(const AMenuItem: TMenuItem; const HasIcon: Boolean; const AIcon: TBitmap); virtual;
  end;
  {$ifndef wsintf}TWSMenuItemClass = class of TWSMenuItem;{$endif}

  { TWSMenu }

  {$ifndef wsintf}TWSMenuClass = class of TWSMenu;{$endif}
  {$ifdef wsintf}
  TWSMenuClass = interface(TWSLCLComponentClass)
    ['{167D872B-A510-4782-B18E-06CD4A775F68}']
    function CreateHandle(const AMenu: TMenu): HMENU;
    procedure SetBiDiMode(const AMenu: TMenu; UseRightToLeftAlign, UseRightToLeftReading : Boolean);
  end;
  {$endif}
  TWSMenu = class(TWSLCLComponent{$ifdef wsintf},TWSMenuClass{$endif})
  impsection
    imptype function CreateHandle(const AMenu: TMenu): HMENU; virtual;
    
    imptype procedure SetBiDiMode(const AMenu: TMenu; UseRightToLeftAlign, UseRightToLeftReading : Boolean); virtual;
  end;

  { TWSMainMenu }

  TWSMainMenu = class(TWSMenu)
  published
  end;

  { TWSPopupMenu }
  {$ifdef wsintf}
  TWSPopupMenuClass = interface(TWSMenuClass)
    ['{899F0651-E52E-4F97-8A2E-49CEFF04D101}']
    procedure Popup(const APopupMenu: TPopupMenu; const X, Y: integer);
  end;
  {$endif}
  TWSPopupMenu = class(TWSMenu{$ifdef wsintf}, TWSPopupMenuClass{$endif})
  impsection
    imptype procedure Popup(const APopupMenu: TPopupMenu; const X, Y: integer); virtual;
  end;
  {$ifndef wsintf}TWSPopupMenuClass = class of TWSPopupMenu;{$endif}

function WSCheckMenuItem(const AMenuItem: TMenuItem;
  const AProcName: String): Boolean;

  { WidgetSetRegistration }

  procedure RegisterMenuItem;
  procedure RegisterMenu;
  procedure RegisterMainMenu;
  procedure RegisterPopupMenu;

function WSMenuItemClass(AWidgetSetClass: {$ifdef wsintf}TWSLCLComponentClass{$else}TClass{$endif}): TWSMenuItemClass; inline;
function WSMenuClass(AWidgetSetClass: {$ifdef wsintf}TWSLCLComponentClass{$else}TClass{$endif}): TWSMenuClass; inline;
function WSPopupMenuClass(AWidgetSetClass: {$ifdef wsintf}TWSLCLComponentClass{$else}TClass{$endif}): TWSPopupMenuClass; inline;
{$ifdef wsintf}
function CreateMenuCommand: LongInt;
procedure ReleaseMenuCommand(ACommand: LongInt);
{$endif}

implementation

{ Menu command management }

var
  CommandPool: TBits = nil;

function UniqueCommand: LongInt;
begin
  if CommandPool = nil then
    CommandPool := TBits.Create(16);
  Result := CommandPool.OpenBit;
  CommandPool[Result] := True;
end;

{$ifdef wsintf}
function CreateMenuCommand: LongInt;
begin
  Result := UniqueCommand;
end;

procedure ReleaseMenuCommand(ACommand: LongInt);
begin
  if CommandPool = nil then Exit;
  CommandPool[ACommand] := False;
end;
{$endif}

{ TWSMenuItem }

imptype function TWSMenuItem.OpenCommand: LongInt;
begin
  Result := UniqueCommand;
end;

imptype procedure TWSMenuItem.CloseCommand(ACommand: LongInt);
begin
  CommandPool[ACommand] := False;
end;

imptype procedure TWSMenuItem.AttachMenu(const AMenuItem: TMenuItem);
begin
end;

imptype function  TWSMenuItem.CreateHandle(const AMenuItem: TMenuItem): HMENU;
begin
  Result := 0;
end;

imptype procedure TWSMenuItem.DestroyHandle(const AMenuItem: TMenuItem);
begin
end;

imptype procedure TWSMenuItem.SetCaption(const AMenuItem: TMenuItem; const ACaption: string);
begin
end;

imptype procedure TWSMenuItem.SetShortCut(const AMenuItem: TMenuItem; const ShortCutK1, ShortCutK2: TShortCut);
begin
end;

imptype procedure TWSMenuItem.SetVisible(const AMenuItem: TMenuItem; const Visible: boolean);
begin
end;

imptype function TWSMenuItem.SetCheck(const AMenuItem: TMenuItem; const Checked: boolean): boolean;
begin
  Result := false;
end;

imptype function TWSMenuItem.SetEnable(const AMenuItem: TMenuItem; const Enabled: boolean): boolean;
begin
  Result := false;
end;

imptype function TWSMenuItem.SetRadioItem(const AMenuItem: TMenuItem; const RadioItem: boolean): boolean;
begin
  Result := false;
end;

imptype function TWSMenuItem.SetRightJustify(const AMenuItem: TMenuItem; const Justified: boolean): boolean;
begin
  Result := false;
end;

imptype procedure TWSMenuItem.UpdateMenuIcon(const AMenuItem: TMenuItem; const HasIcon: Boolean; const AIcon: TBitmap);
begin
  // emulate old behaviour
  AMenuItem.RecreateHandle;
end;


          
{ TWSMenu }

imptype function  TWSMenu.CreateHandle(const AMenu: TMenu): HMENU;
begin
  Result := 0;
end;

imptype procedure TWSMenu.SetBiDiMode(const AMenu : TMenu; UseRightToLeftAlign,
  UseRightToLeftReading : Boolean);
begin
end;


{ TWSPopupMenu }

imptype procedure TWSPopupMenu.Popup(const APopupMenu: TPopupMenu; const X, Y: integer);
begin
end;

function WSCheckMenuItem(const AMenuItem: TMenuItem;
  const AProcName: String): Boolean;

  procedure Warn;
  begin
    DebugLn('[WARNING] %s called without handle for %s(%s)', [AProcName, AMenuItem.Name, AMenuItem.ClassName]);
  end;
begin
  Result := AMenuItem.HandleAllocated;
  if Result then Exit;
  Warn;
end;

{ WidgetSetRegistration }

procedure RegisterMenuItem;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterMenuItem;
//  if not WSRegisterMenuItem then
//    RegisterWSComponent(TMenuItem, TWSMenuItem);
  Done := True;
end;

procedure RegisterMenu;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterMenu;
//  if not WSRegisterMenu then
//    RegisterWSComponent(TMenu, TWSMenu);
  Done := True;
end;

procedure RegisterMainMenu;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterMainMenu;
//  if not WSRegisterMainMenu then
//    RegisterWSComponent(TMainMenu, TWSMainMenu);
  Done := True;
end;

procedure RegisterPopupMenu;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterPopupMenu;
//  if not WSRegisterPopupMenu then
//    RegisterWSComponent(TPopupMenu, TWSPopupMenu);
  Done := True;
end;

function WSMenuItemClass(AWidgetSetClass: {$ifdef wsintf}TWSLCLComponentClass{$else}TClass{$endif}): TWSMenuItemClass; inline;
begin
  {$ifdef wsintf}
  Result := (AWidgetSetClass as TWSMenuItemClass);
  {$else}
  Result := TWSMenuItemClass(AWidgetSetClass);
  {$endif}
end;

function WSMenuClass(AWidgetSetClass: {$ifdef wsintf}TWSLCLComponentClass{$else}TClass{$endif}): TWSMenuClass; inline;
begin
  {$ifdef wsintf}
  Result := (AWidgetSetClass as TWSMenuClass);
  {$else}
  Result := TWSMenuClass(AWidgetSetClass);
  {$endif}
end;

function WSPopupMenuClass(AWidgetSetClass: {$ifdef wsintf}TWSLCLComponentClass{$else}TClass{$endif}): TWSPopupMenuClass; inline;
begin
  {$ifdef wsintf}
  Result := (AWidgetSetClass as TWSPopupMenuClass);
  {$else}
  Result := TWSPopupMenuClass(AWidgetSetClass);
  {$endif}
end;

finalization
  FreeThenNil(CommandPool);
end.
