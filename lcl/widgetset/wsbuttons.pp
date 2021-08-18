{
 *****************************************************************************
 *                               WSButtons.pp                                * 
 *                               ------------                                * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit WSButtons;

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
  Classes, Controls, Buttons, Graphics,
////////////////////////////////////////////////////
  {$ifdef wsintf}WSLCLClasses_Intf{$else}WSLCLClasses{$endif}, WSStdCtrls, WSControls, LCLType, LCLIntf, WSFactory;

type

  { TWSBitBtn }
  {$ifndef wsintf}
  TWSBitBtnClass = class of TWSBitBtn;
  {$else}
  TWSBitBtnClass = interface(TWSButtonClass)
    ['{EF40D0DA-42AA-49BF-B433-D3F88B1AB218}']
    procedure SetGlyph(const ABitBtn: TCustomBitBtn; const AValue: TButtonGlyph);
    procedure SetLayout(const ABitBtn: TCustomBitBtn; const AValue: TButtonLayout);
    procedure SetMargin(const ABitBtn: TCustomBitBtn; const AValue: Integer);
    procedure SetSpacing(const ABitBtn: TCustomBitBtn; const AValue: Integer);
  end;
  {$endif}
  TWSBitBtn = class(TWSButton, TWSBitBtnClass)
  impsection
    imptype procedure SetGlyph(const ABitBtn: TCustomBitBtn; const AValue: TButtonGlyph); virtual;
    imptype procedure SetLayout(const ABitBtn: TCustomBitBtn; const AValue: TButtonLayout); virtual;
    imptype procedure SetMargin(const ABitBtn: TCustomBitBtn; const AValue: Integer); virtual;
    imptype procedure SetSpacing(const ABitBtn: TCustomBitBtn; const AValue: Integer); virtual;
  end;

  { TWSSpeedButton }

  TWSSpeedButtonClass = class of TWSSpeedButton;
  TWSSpeedButton = class(TWSGraphicControl)
  published
  end;

  { WidgetSetRegistration }

  procedure RegisterCustomBitBtn;
  procedure RegisterCustomSpeedButton;

function WSBitBtnClass(AWidgetSetClass: {$ifdef wsintf}TWSLCLComponentClass{$else}TClass{$endif}): TWSBitBtnClass; inline;

implementation

uses
  LResources;


// TODO: Can't be virtual abstract ?

{ TWSCustomBitBtn }

imptype procedure TWSBitBtn.SetGlyph(const ABitBtn: TCustomBitBtn;
  const AValue: TButtonGlyph);
begin
end;

imptype procedure TWSBitBtn.SetLayout(const ABitBtn: TCustomBitBtn;
  const AValue: TButtonLayout);
begin
end;

imptype procedure TWSBitBtn.SetMargin(const ABitBtn: TCustomBitBtn;
  const AValue: Integer);
begin
end;

imptype procedure TWSBitBtn.SetSpacing(const ABitBtn: TCustomBitBtn;
  const AValue: Integer);
begin
end;

{ WidgetSetRegistration }

procedure RegisterCustomBitBtn;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomBitBtn;
  RegisterPropertyToSkip(TBitBtn, 'Style', 'VCL compatibility property', '');
//  if not WSRegisterCustomBitBtn then
//    RegisterWSComponent(TCustomBitBtn, TWSBitBtn);
  Done := True;
end;

procedure RegisterCustomSpeedButton;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomSpeedButton;
//  if not WSRegisterCustomSpeedButton then
//    RegisterWSComponent(TCustomSpeedButton, TWSSpeedButton);
  Done := True;
end;

function WSBitBtnClass(AWidgetSetClass: {$ifdef wsintf}TWSLCLComponentClass{$else}TClass{$endif}): TWSBitBtnClass; inline;
begin
  {$ifdef wsintf}
  Result := (AWidgetSetClass as TWSBitBtnClass);
  {$else}
  Result := TWSBitBtnClass(AWidgetSetClass);
  {$endif}
end;


end.
