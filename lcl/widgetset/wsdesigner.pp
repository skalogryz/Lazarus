{
 *****************************************************************************
 *                               WSDesigner.pp                                * 
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
unit WSDesigner;

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
  Classes, RubberBand,
  WsControls, WSFactory;

type
  { TWsCustomRubberBand }
  {$ifdef wsintf}
  TWsCustomRubberBandClass = interface(TWsWinControlclass)
    procedure SetShapeRubberBand(ARubberBand: TCustomRubberBand; AShape: TRubberBandShape);
  end;
  {$endif}
  TWsCustomRubberBand = class(TWsWinControl)
  impsection
    {$ifndef wsintf}
    imptype procedure SetShape(ARubberBand: TCustomRubberBand; AShape: TRubberBandShape); virtual; overload;
    {$else}
    imptype procedure SetShapeRubberBand(ARubberBand: TCustomRubberBand; AShape: TRubberBandShape); virtual;
    {$endif}
  end;
  {$ifndef wsintf}TWsCustomRubberBandClass = class of TWsCustomRubberBand;{$endif}

  { WidgetSetRegistration }

  procedure RegisterCustomRubberBand;

implementation

{ TWsCustomRubberBand }
{$ifdef wsintf}
imptype procedure TWsCustomRubberBand.SetShapeRubberBand(ARubberBand: TCustomRubberBand; AShape: TRubberBandShape);
{$else}
imptype procedure TWsCustomRubberBand.SetShape(ARubberBand: TCustomRubberBand;
  AShape: TRubberBandShape);
{$endif}
begin
end;

  { WidgetSetRegistration }

procedure RegisterCustomRubberBand;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomRubberBand;
//  if not WSRegisterCustomRubberBand then
//    RegisterWSComponent(TCustomRubberBand, TWSCustomRubberBand);
  Done := True;
end;

end.
