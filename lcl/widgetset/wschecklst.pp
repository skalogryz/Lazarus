{
 *****************************************************************************
 *                               WSCheckLst.pp                               * 
 *                               -------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit WSCheckLst;

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
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  StdCtrls, CheckLst,
////////////////////////////////////////////////////
  {$ifdef wsintf}WSLCLClasses_Intf{$else}WSLCLClasses{$endif}, WSStdCtrls, Classes, WSFactory;

type
  { TWSCustomCheckListBox }
  {$ifdef wsintf}
  IWSCustomCheckListBox = interface(IWSCustomListBox)
    ['{064C5BE7-FAD5-488F-8FEE-558B01B158CB}']
    function GetCheckWidth(const ACheckListBox: TCustomCheckListBox): integer;
    function GetItemEnabled(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer): Boolean;
    function GetHeader(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer): Boolean;
    function GetState(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer): TCheckBoxState;
    procedure SetItemEnabled(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer; const AEnabled: Boolean);
    procedure SetHeader(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer; const AHeader: Boolean);
    procedure SetState(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer; const AState: TCheckBoxState);
  end;
  TWSCustomCheckListBoxClass = IWSCustomCheckListBox;
  {$endif}

  TWSCustomCheckListBox = class(TWSCustomListBox{$ifdef wsintf},IWSCustomCheckListBox{$endif})
  impsection
    imptype function GetCheckWidth(const ACheckListBox: TCustomCheckListBox):
      integer; virtual;
    imptype function GetItemEnabled(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer): Boolean; virtual;
    imptype function GetHeader(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer): Boolean; virtual;
    imptype function GetState(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer): TCheckBoxState; virtual;
    imptype procedure SetItemEnabled(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer; const AEnabled: Boolean); virtual;
    imptype procedure SetHeader(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer; const AHeader: Boolean); virtual;
    imptype procedure SetState(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer; const AState: TCheckBoxState); virtual;
  end;
  {$ifndef wsintf}
  TWSCustomCheckListBoxClass = class of TWSCustomCheckListBox;
  {$endif}

  { WidgetSetRegistration }

  procedure RegisterCustomCheckListBox;

function GetWSCustomCheckListBox(AWidgetSetClass: TWSLCLComponentClass): TWSCustomCheckListBoxClass; inline;

implementation

imptype function TWSCustomCheckListBox.GetCheckWidth(
  const ACheckListBox: TCustomCheckListBox): Integer;
begin
  Result := 0;
end;

imptype function TWSCustomCheckListBox.GetHeader(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer): Boolean;
begin
  Result := False;
end;

imptype function TWSCustomCheckListBox.GetItemEnabled(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer): Boolean;
begin
  Result := True;
end;

imptype function TWSCustomCheckListBox.GetState(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer
  ): TCheckBoxState;
begin
  Result := cbUnchecked;
end;

imptype procedure TWSCustomCheckListBox.SetHeader(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer;
  const AHeader: Boolean);
begin
end;

imptype procedure TWSCustomCheckListBox.SetItemEnabled(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer;
  const AEnabled: Boolean);
begin
end;

imptype procedure TWSCustomCheckListBox.SetState(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer;
  const AState: TCheckBoxState);
begin
end;

{ WidgetSetRegistration }

procedure RegisterCustomCheckListBox;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomCheckListBox;
//  if not WSRegisterCustomCheckListBox then
//    RegisterWSComponent(TCustomCheckListBox, TWSCustomCheckListBox);
  Done := True;
end;

function GetWSCustomCheckListBox(AWidgetSetClass: TWSLCLComponentClass): TWSCustomCheckListBoxClass;
begin
  {$ifdef wsintf}
  AWidgetSetClass.QueryInterface(IWSCustomCheckListBox, Result);
  {$else}
  Result := TWSCustomCheckListBoxClass(AWidgetSetClass);
  {$endif}
end;

end.
