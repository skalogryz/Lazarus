{
 *****************************************************************************
 *                            QtWSPairSplitter.pp                            * 
 *                            -------------------                            * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit QtWSPairSplitter;

{$mode objfpc}{$H+}

interface

{$I qtdefines.inc}

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
//  PairSplitter,
////////////////////////////////////////////////////
  qt5, qtwidgets,
  Controls, LCLType, LCLProc,
  WSPairSplitter, {$ifndef wsintf}WSLCLClasses{$else}QtWSControls, WSLCLClasses_Intf{$endif};

type

  { TQtWSPairSplitterSide }

  TQtWSPairSplitterSide = class({$ifndef wsintf}TWSPairSplitterSide{$else}TQtWSWinControl{$endif})
  impsection
    imptype function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TQtWSCustomPairSplitter }

  TQtWSCustomPairSplitter = class({$ifndef wsintf}TWSCustomPairSplitter{$else}TQtWSWinControl{$endif})
  published
  end;

implementation

{ TQtWSPairSplitterSide }

imptype function TQtWSPairSplitterSide.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams
  ): TLCLIntfHandle;
var
  QtWidget: TQtWidget;
begin
  {$ifdef VerboseQt}
    WriteLn('> TQtWSPairSplitterSide.CreateHandle for ',dbgsname(AWinControl));
  {$endif}
  QtWidget := TQtWidget.Create(AWinControl, AParams);
  QtWidget.setAttribute(QtWA_NoMousePropagation, True);

  QtWidget.AttachEvents;

  Result := TLCLIntfHandle(QtWidget);

  {$ifdef VerboseQt}
    WriteLn('< TQtWSPairSplitterSide.CreateHandle for ',dbgsname(AWinControl),' Result: ', dbgHex(Result));
  {$endif}
end;

end.
