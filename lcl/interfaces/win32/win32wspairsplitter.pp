{ $Id$}
{
 *****************************************************************************
 *                          Win32WSPairSplitter.pp                           * 
 *                          ----------------------                           * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit Win32WSPairSplitter;

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
  WSPairSplitter, {$ifdef wsintf}WSLCLClasses_Intf, Win32WSControls{$else}WSLCLClasses{$endif};

type

  { TWin32WSPairSplitterSide }

  TWin32WSPairSplitterSide = class({$ifndef wsintf}TWSPairSplitterSide{$else}TWin32WSWinControl{$endif})
  published
  end;

  { TWin32WSCustomPairSplitter }

  TWin32WSCustomPairSplitter = class({$ifndef wsintf}TWSCustomPairSplitter{$else}TWin32WSWinControl{$endif})
  published
  end;

implementation

end.
