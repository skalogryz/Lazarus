{ $Id$}
{
 *****************************************************************************
 *                                 WSSpin.pp                                 * 
 *                                 ---------                                 * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit WSSpin;

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
  Spin, LResources,
////////////////////////////////////////////////////
  {$ifdef wsintf}WSLCLClasses_Intf{$else}WSLCLClasses{$endif}, WSControls, WSStdCtrls, WSFactory;

type
  { TWSCustomFloatSpinEdit }
  {$ifdef wsintf}
  IWSCustomFloatSpinEdit = interface(IWSCustomEdit)
    ['{E879A434-9543-46B1-B880-004E0C72A1EF}']
    function  GetValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit): double;
    procedure UpdateControl(const ACustomFloatSpinEdit: TCustomFloatSpinEdit);
    procedure SetEditorEnabled(const ACustomFloatSpinEdit: TCustomFloatSpinEdit; AValue: Boolean);
  end;
  TWSCustomFloatSpinEditClass = IWSCustomFloatSpinEdit;
  {$endif}

  TWSCustomFloatSpinEdit = class(TWSCustomEdit{$ifdef wsintf},IWSCustomFloatSpinEdit{$endif})
  impsection
    imptype function  GetValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit): double; virtual;

(*  TODO: seperation into properties instead of bulk update
    imptype procedure SetIncrement(const ACustomFloatSpinEdit: TCustomFloatSpinEdit; NewIncrement: Double); virtual;
    imptype procedure SetMinValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit; NewValue: Double); virtual;
    imptype procedure SetMaxValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit; NewValue: Double); virtual;
    imptype procedure SetValueEmpty(const ACustomFloatSpinEdit: TCustomFloatSpinEdit; NewEmpty: boolean); virtual;
*)

    imptype procedure UpdateControl(const ACustomFloatSpinEdit: TCustomFloatSpinEdit); virtual;
    imptype procedure SetEditorEnabled(const ACustomFloatSpinEdit: TCustomFloatSpinEdit; AValue: Boolean); virtual;
  end;
  {$ifndef wsintf}TWSCustomFloatSpinEditClass = class of TWSCustomFloatSpinEdit;{$endif}

  { WidgetSetRegistration }

  procedure RegisterCustomFloatSpinEdit;

function GetWSCustomFloatSpinEdit(AWidgetSetClass: TWSLCLComponentClass): TWSCustomFloatSpinEditClass; inline;

implementation

{ TWSCustomFloatSpinEdit }

imptype function TWSCustomFloatSpinEdit.GetValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit): double;
begin
  Result := 0.0;
end;

imptype procedure TWSCustomFloatSpinEdit.UpdateControl(const ACustomFloatSpinEdit: TCustomFloatSpinEdit);
begin
end;

imptype procedure TWSCustomFloatSpinEdit.SetEditorEnabled(
  const ACustomFloatSpinEdit: TCustomFloatSpinEdit; AValue: Boolean);
begin
end;

{ WidgetSetRegistration }

procedure RegisterCustomFloatSpinEdit;
const
  Done: Boolean = False;
begin
  if Done then exit;
  RegisterPropertyToSkip(TCustomFloatSpinEdit, 'MaxLength', 'VCL compatibility property', '');
  if not WSRegisterCustomFloatSpinEdit then
    RegisterWSComponent(TCustomFloatSpinEdit, TWSCustomFloatSpinEdit{$ifdef wsintf}.Create{$endif});
  Done := True;
end;

function GetWSCustomFloatSpinEdit(AWidgetSetClass: TWSLCLComponentClass): TWSCustomFloatSpinEditClass;
begin
  {$ifdef wsintf}
  Result := (AWidgetSetClass as IWSCustomFloatSpinEdit);
  {$else}
  Result := TWSCustomFloatSpinEditClass(AWidgetSetClass);
  {$endif}
end;

end.
