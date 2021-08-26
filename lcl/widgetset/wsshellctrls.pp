{
 *****************************************************************************
 *                              WSShellCtrls.pp                              * 
 *                              -------------                                * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit WSShellCtrls;

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
  Classes, Types,
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  ShellCtrls, ComCtrls,
////////////////////////////////////////////////////
  WSControls, WSFactory, {$ifdef wsintf}WSLCLClasses_Intf{$else}WSLCLClasses{$endif}, WSComCtrls;

type

  { TWSCustomShellTreeView }
  {$ifdef wsintf}
  IWSCustomShellTreeView = interface(IWSWinControl)
    ['{C5451984-00E2-41AD-B25E-25DAF38E0DA5}']
    function DrawBuiltInIcon(ATreeView: TCustomShellTreeView;
      ANode: TTreeNode; ARect: TRect): TSize;
    function GetBuiltinIconSize: TSize;
  end;
  TWSCustomShellTreeViewClass = IWSCustomShellTreeView;
  {$endif}

  TWSCustomShellTreeView = class(TWSCustomTreeView{$ifdef wsintf},IWSCustomShellTreeView{$endif})
  impsection
    imptype function DrawBuiltInIcon(ATreeView: TCustomShellTreeView;
      ANode: TTreeNode; ARect: TRect): TSize; virtual;
    imptype function GetBuiltinIconSize: TSize; virtual;
  end;
  {$ifndef wsintf}TWSCustomShellTreeViewClass = class of TWSCustomShellTreeView;{$endif}

  { TWSCustomShellListView }
  {$ifdef wsintf}
  IWSCustomShellListView = interface(IWSCustomListView)
    ['{BEFE27CD-DAC3-410E-84C0-F55D33FE31F5}']
    function GetBuiltInImageIndex(AListView: TCustomShellListView;
      const AFileName: String; ALargeImage: Boolean): Integer;
  end;
  TWSCustomShellListViewClass = IWSCustomShellListView;
  {$endif}
  TWSCustomShellListView = class(TWSCustomListView{$ifdef wsintf},IWSCustomShellListView{$endif})
  impsection
    imptype function GetBuiltInImageIndex(AListView: TCustomShellListView;
      const AFileName: String; ALargeImage: Boolean): Integer; virtual;
  end;
  {$ifndef wsintf}TWSCustomShellListViewClass = class of TWSCustomShellListView;{$endif}

procedure RegisterCustomShellTreeView;
procedure RegisterCustomShellListView;

function WSCustomShellTreeViewClass(AWidgetSetClass: TWSLCLComponentClass): TWSCustomShellTreeViewClass; inline;
function WSCustomShellListViewClass(AWidgetSetClass: TWSLCLComponentClass): TWSCustomShellListViewClass; inline;

implementation

uses
  LResources;

{ TWSCustomShellTreeView }

imptype function TWSCustomShellTreeView.DrawBuiltInIcon(ATreeView: TCustomShellTreeView;
  ANode: TTreeNode; ARect: TRect): TSize;
begin
  Result.CX := 0;
  Result.CY := 0;
end;

imptype function TWSCustomShellTreeView.GetBuiltinIconSize: TSize;
begin
  Result.CX := 0;
  Result.CY := 0;
end;


{ TWSCustomShellListView }

imptype function TWSCustomShellListView.GetBuiltInImageIndex(
  AListView: TCustomShellListView; const AFileName: String;
  ALargeImage: Boolean): Integer;
begin
  Result := -1;
end;


{ Registration }

procedure RegisterCustomShellTreeView;
const
  Done: Boolean = False;
begin
  if Done then exit;
  //WSRegisterCustomShellTreeView;
  if not WSRegisterCustomShellTreeView then
    RegisterWSComponent(TCustomShellTreeView, TWSCustomShellTreeView{$ifdef wsintf}.Create{$endif});
  Done := True;
end;

procedure RegisterCustomShellListView;
const
  Done: Boolean = False;
begin
  if Done then exit;
//  WSRegisterCustomShellListView;
  if not WSRegisterCustomShellListView then
    RegisterWSComponent(TCustomShellListView, TWSCustomShellListView{$ifdef wsintf}.Create{$endif});
  Done := True;
end;

function WSCustomShellTreeViewClass(AWidgetSetClass: TWSLCLComponentClass): TWSCustomShellTreeViewClass; inline;
begin
  {$ifdef wsintf}
  Result := (AWidgetSetClass as IWSCustomShellTreeView);
  {$else}
  Result := TWSCustomShellTreeViewClass(AWidgetSetClass);
  {$endif}
end;

function WSCustomShellListViewClass(AWidgetSetClass: TWSLCLComponentClass): TWSCustomShellListViewClass; inline;
begin
  {$ifdef wsintf}
  Result := (AWidgetSetClass as IWSCustomShellListView);
  {$else}
  Result := TWSCustomShellListViewClass(AWidgetSetClass);
  {$endif}
end;

end.
