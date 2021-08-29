{ $Id$}
{
 *****************************************************************************
 *                             Gtk2WSExtDlgs.pp                              * 
 *                             ----------------                              * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit Gtk2WSExtDlgs;

{$mode objfpc}{$H+}

interface

{$I gtk2defines.inc}

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
//  ExtDlgs,
////////////////////////////////////////////////////
  WSExtDlgs, {$ifndef wsintf}WSLCLClasses{$else}Gtk2WSControls, WSLCLClasses_Intf{$endif};

type

  { TGtk2WSPreviewFileControl }

  TGtk2WSPreviewFileControl = class({$ifndef wsintf}TWSPreviewFileControl{$else}TGtk2WSWinControl{$endif})
  private
  protected
  public
  end;

  { TGtk2WSPreviewFileDialog }

  TGtk2WSPreviewFileDialog = class(TWSPreviewFileDialog)
  private
  protected
  public
  end;

  { TGtk2WSOpenPictureDialog }

  TGtk2WSOpenPictureDialog = class(TWSOpenPictureDialog)
  private
  protected
  public
  end;

  { TGtk2WSSavePictureDialog }

  TGtk2WSSavePictureDialog = class(TWSSavePictureDialog)
  private
  protected
  public
  end;

  { TGtk2WSCalculatorDialog }

  TGtk2WSCalculatorDialog = class(TWSCalculatorDialog)
  private
  protected
  public
  end;

  { TGtk2WSCalculatorForm }

  TGtk2WSCalculatorForm = class(TWSCalculatorForm)
  private
  protected
  public
  end;

  { TGtk2WSCalendarDialogForm }

  TGtk2WSCalendarDialogForm = class(TWSCalendarDialogForm)
  private
  protected
  public
  end;

  { TGtk2WSCalendarDialog }

  TGtk2WSCalendarDialog = class(TWSCalendarDialog)
  private
  protected
  public
  end;


implementation

end.
