{
 *****************************************************************************
 *                              QtWSExtDlgs.pp                               * 
 *                              --------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit QtWSExtDlgs;

{$mode objfpc}{$H+}

interface

{$i qtdefines.inc}

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
//  ExtDlgs,
////////////////////////////////////////////////////
  WSExtDlgs, {$ifndef wsintf}WSLCLClasses{$else}WSLCLClasses_Intf{$endif};

type

  { TQtWSPreviewFileControl }

  TQtWSPreviewFileControl = class(TWSPreviewFileControl)
  published
  end;

  { TQtWSPreviewFileDialog }

  TQtWSPreviewFileDialog = class(TWSPreviewFileDialog)
  published
  end;

  { TQtWSOpenPictureDialog }

  TQtWSOpenPictureDialog = class(TWSOpenPictureDialog)
  published
  end;

  { TQtWSSavePictureDialog }

  TQtWSSavePictureDialog = class(TWSSavePictureDialog)
  published
  end;

  { TQtWSCalculatorDialog }

  TQtWSCalculatorDialog = class(TWSCalculatorDialog)
  published
  end;

  { TQtWSCalculatorForm }

  TQtWSCalculatorForm = class(TWSCalculatorForm)
  published
  end;

  { TQtWSCalendarDialogForm }

  TQtWSCalendarDialogForm = class(TWSCalendarDialogForm)
  published
  end;

  { TQtWSCalendarDialog }

  TQtWSCalendarDialog = class(TWSCalendarDialog)
  published
  end;


implementation

end.
