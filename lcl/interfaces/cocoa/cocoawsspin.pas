{
 *****************************************************************************
 *                                CocoaWSSpin.pas                            *
 *                                -----------                                * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit CocoaWSSpin;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}
{$include cocoadefines.inc}

interface

uses
  // rtl+lcl
  Controls, Spin, LCLType,
  // widgetset
  WSSpin, {$ifndef wsintf}WSLCLClasses{$else}WSLCLClasses_Intf{$endif},
  // cocoa ws
  CocoaPrivate, CocoaWSCommon, CocoaTextEdits, CocoaWSStdCtrls;

type

  { TCocoaWSCustomFloatSpinEdit }

  TCocoaWSCustomFloatSpinEdit = class({$ifndef wsintf}TWSCustomFloatSpinEdit{$else}TCocoaWSCustomEdit, IWSCustomFloatSpinEdit{$endif})
  impsection
    imptype function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    imptype procedure DestroyHandle(const AWinControl: TWinControl); override;
    imptype function  GetValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit): Double; rootoverride;
    imptype procedure UpdateControl(const ACustomFloatSpinEdit: TCustomFloatSpinEdit); rootoverride;
    imptype procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); override;
    {$ifdef wsintf}
    imptype procedure SetEditorEnabled(const ACustomFloatSpinEdit: TCustomFloatSpinEdit; AValue: Boolean); rootoverride;
    {$endif}
  end;

implementation

procedure UpdateControlLCLToCocoa(src: TCustomFloatSpinEdit; dst: TCocoaSpinEdit);
begin
  dst.UpdateControl(src.MinValue, src.MaxValue, src.Increment, src.Value, src.DecimalPlaces);
end;

{ TCocoaWSCustomFloatSpinEdit }

{------------------------------------------------------------------------------
  Method:  TCocoaWSCustomFloatSpinEdit.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new spin edit in Carbon interface with the specified parameters
 ------------------------------------------------------------------------------}
imptype function TCocoaWSCustomFloatSpinEdit.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  lSpin: TCocoaSpinEdit;
begin
  lSpin := TCocoaSpinEdit.alloc.lclInitWithCreateParams(AParams);
  Result := TLCLIntfHandle(lSpin);
  if Result = 0 then Exit;
  lSpin.decimalPlaces := -1;
  lSpin.lclCreateSubcontrols(AParams);
  lSpin.callback := TLCLCommonCallback.Create(lSpin, AWinControl);
  if (lSpin.Stepper.isKindOfClass(TCocoaSpinEditStepper)) then
    TCocoaSpinEditStepper(lSpin.Stepper).callback:=lSpin.callback;
end;

imptype procedure TCocoaWSCustomFloatSpinEdit.DestroyHandle(const AWinControl: TWinControl);
begin
  if not AWinControl.HandleAllocated then Exit;
  TCocoaSpinEdit(AWinControl.Handle).lclReleaseSubcontrols;
  {$ifndef wsintf}
  TCocoaWSWinControl.DestroyHandle(AWinControl);
  {$else}
  inherited DestroyHandle(AWinControl);
  {$endif}
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSCustomFloatSpinEdit.GetValue
  Params:  ACustomFloatSpinEdit - LCL custom float spin edit
  Returns: The float spin edit value
 ------------------------------------------------------------------------------}
imptype function TCocoaWSCustomFloatSpinEdit.GetValue(const ACustomFloatSpinEdit: TCustomFloatSpinEdit): Double;
var
  lSpin: TCocoaSpinEdit;
begin
  Result := 0;
  if ACustomFloatSpinEdit = nil then Exit;
  if not ACustomFloatSpinEdit.HandleAllocated then Exit;
  lSpin := TCocoaSpinEdit(ACustomFloatSpinEdit.Handle);

  Result := lSpin.Stepper.doubleValue();
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSCustomFloatSpinEdit.UpdateControl
  Params:  ACustomFloatSpinEdit - LCL custom float spin edit
  
  Update the value, min, max and increment of custom float spin edit in Cocoa
  interface
 ------------------------------------------------------------------------------}
imptype procedure TCocoaWSCustomFloatSpinEdit.UpdateControl(const ACustomFloatSpinEdit: TCustomFloatSpinEdit);
var
  lSpin: TCocoaSpinEdit;
begin
  if ACustomFloatSpinEdit = nil then Exit;
  if not ACustomFloatSpinEdit.HandleAllocated then Exit;
  lSpin := TCocoaSpinEdit(ACustomFloatSpinEdit.Handle);
  UpdateControlLCLToCocoa(ACustomFloatSpinEdit, lSpin);
end;

imptype procedure TCocoaWSCustomFloatSpinEdit.SetBounds(
  const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer);
var
  lSpin: TCocoaSpinEdit;
  ACustomFloatSpinEdit: TCustomFloatSpinEdit absolute AWinControl;
begin
  if ACustomFloatSpinEdit = nil then Exit;
  if not ACustomFloatSpinEdit.HandleAllocated then Exit;
  lSpin := TCocoaSpinEdit(ACustomFloatSpinEdit.Handle);
  {$ifndef wsintf}
  TCocoaWSWinControl.SetBounds(AWinControl, ALeft, ATop, AWidth, AHeight);
  {$else}
  inherited SetBounds(AWinControl, ALeft, ATop, AWidth, AHeight);
  {$endif}
  lSpin.PositionSubcontrols(ALeft, ATop, AWidth, AHeight);
end;
{$ifdef wsintf}
imptype procedure TCocoaWSCustomFloatSpinEdit.SetEditorEnabled(const ACustomFloatSpinEdit: TCustomFloatSpinEdit; AValue: Boolean);
begin
end;
{$endif}
end.
