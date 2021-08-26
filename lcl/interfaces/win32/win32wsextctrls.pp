{
 *****************************************************************************
 *                            Win32WSExtCtrls.pp                             *
 *                            ------------------                             *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit Win32WSExtCtrls;

{$mode objfpc}{$H+}
{$I win32defines.inc}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
// rtl
  Windows, CommCtrl, SysUtils, Classes,
// lcl
  ExtCtrls, Controls, ImgList, LCLType, LCLIntf, LazUTF8, Themes, LCLMessageGlue, ComCtrls, WSComCtrls,
// ws
  WSControls, WSExtCtrls, {$ifdef wsintf}Graphics, WSLCLClasses_Intf{$else}WSLCLClasses{$endif}, WSProc, Win32Extra, Win32Int, Win32Proc,
  InterfaceBase, Win32WSControls{$ifdef wsintf}, Win32WSStdCtrls{$endif};

type
  { TWin32WSPage }

  TWin32WSPage = class({$ifndef wsintf}TWSPage{$else}TWin32WSCustomControl{$endif})
  published
  end;

  { TWin32WSNotebook }

  TWin32WSNotebook = class({$ifndef wsintf}TWSNotebook{$else}TWin32WSCustomControl{$endif})
  published
  end;

  { TWin32WSShape }

  TWin32WSShape = class({$ifndef wsintf}TWSShape{$else}TWin32WSControl{$endif})
  published
  end;

  { TWin32WSCustomSplitter }

  TWin32WSCustomSplitter = class({$ifndef wsintf}TWSCustomSplitter{$else}TWin32WSCustomControl{$endif})
  published
  end;

  { TWin32WSSplitter }

  TWin32WSSplitter = class({$ifndef wsintf}TWSSplitter{$else}TWin32WSCustomControl{$endif})
  published
  end;

  { TWin32WSPaintBox }

  TWin32WSPaintBox = class({$ifndef wsintf}TWSPaintBox{$else}TWin32WSControl{$endif})
  published
  end;

  { TWin32WSCustomImage }

  TWin32WSCustomImage = class({$ifndef wsintf}TWSCustomImage{$else}TWin32WSControl{$endif})
  published
  end;

  { TWin32WSImage }

  TWin32WSImage = class({$ifndef wsintf}TWSImage{$else}TWin32WSCustomImage{$endif})
  published
  end;

  { TWin32WSBevel }

  TWin32WSBevel = class({$ifndef wsintf}TWSBevel{$else}TWin32WSControl{$endif})
  published
  end;

  { TWin32WSCustomRadioGroup }

  TWin32WSCustomRadioGroup = class({$ifndef wsintf}TWSCustomRadioGroup{$else}TWin32WSCustomGroupBox{$endif})
  published
  end;

  { TWin32WSRadioGroup }

  TWin32WSRadioGroup = class({$ifndef wsintf}TWSRadioGroup{$else}TWin32WSCustomRadioGroup{$endif})
  published
  end;

  { TWin32WSCustomCheckGroup }

  TWin32WSCustomCheckGroup = class({$ifndef wsintf}TWSCustomCheckGroup{$else}TWin32WSCustomGroupBox{$endif})
  published
  end;

  { TWin32WSCheckGroup }

  TWin32WSCheckGroup = class({$ifndef wsintf}TWSCheckGroup{$else}TWin32WSCustomCheckGroup{$endif})
  published
  end;

  { TWin32WSCustomLabeledEdit }

  TWin32WSCustomLabeledEdit = class({$ifndef wsintf}TWSCustomLabeledEdit{$else}TWin32WSCustomEdit{$endif})
  published
  end;

  { TWin32WSLabeledEdit }

  TWin32WSLabeledEdit = class({$ifndef wsintf}TWSLabeledEdit{$else}TWin32WSCustomLabeledEdit{$endif})
  published
  end;

  { TWin32WSCustomPanel }

  TWin32WSCustomPanel = class({$ifndef wsintf}TWSCustomPanel{$else}TWin32WSCustomControl{$endif})
  published
  end;

  { TWin32WSPanel }

  TWin32WSPanel = class({$ifndef wsintf}TWSPanel{$else}TWin32WSCustomPanel{$endif})
  published
  end;

  { TWin32WSCustomTrayIcon }

  TWin32WSCustomTrayIcon = class({$ifndef wsintf}TWSCustomTrayIcon{$else}TWSLCLComponent, IWSCustomTrayIcon, IWSLCLComponent{$endif})
  protected
    class function AddIcon(ATrayIcon: TCustomTrayIcon): Boolean;
  impsection
    imptype function Hide(const ATrayIcon: TCustomTrayIcon): Boolean; rootoverride;
    imptype function Show(const ATrayIcon: TCustomTrayIcon): Boolean; rootoverride;
    imptype procedure InternalUpdate(const ATrayIcon: TCustomTrayIcon); rootoverride;
    imptype function ShowBalloonHint(const ATrayIcon: TCustomTrayIcon): Boolean; rootoverride;
    imptype function GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint; rootoverride;
    {$ifdef wsintf}
    imptype function GetCanvas(const ATrayIcon: TCustomTrayIcon): TCanvas; rootoverride;
    {$endif}
  end;

implementation

uses
  Forms, LMessages, ShellAPI;

{$include win32trayicon.inc}

end.
