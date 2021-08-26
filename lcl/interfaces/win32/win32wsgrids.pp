{ $Id$}
{
 *****************************************************************************
 *                              Win32WSGrids.pp                              * 
 *                              ---------------                              * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit Win32WSGrids;

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
  Windows, LCLType, LazUTF8, Types, Controls, Grids, Win32Proc, Graphics,
////////////////////////////////////////////////////
  WSGrids{$ifdef wsintf},Win32WSControls{$endif};

type
  { TWin32WSCustomGrid }

  TWin32WSCustomGrid = class({$ifndef wsintf}TWSCustomGrid{$else}TWin32WSWinControl, IWSCustomGrid{$endif})
  impsection
    imptype procedure SendCharToEditor(AEditor:TWinControl; Ch: TUTF8Char); rootoverride;
    imptype function GetEditorBoundsFromCellRect(ACanvas: TCanvas;
      const ACellRect: TRect; const AColumnLayout: TTextLayout): TRect; rootoverride;
    {$ifdef wsintf}
    imptype function InvalidateStartY(const FixedHeight, RowOffset: Integer): integer; rootoverride;
    {$endif}
  end;

implementation

{ TWin32WSCustomGrid }

imptype function TWin32WSCustomGrid.GetEditorBoundsFromCellRect(ACanvas: TCanvas;
  const ACellRect: TRect; const AColumnLayout: TTextLayout): TRect;
var
  EditorTop: LongInt;
  TextHeight: Integer;
begin
  Result:=ACellRect;
  Dec(Result.Right);
  Dec(Result.Bottom);
  Inc(Result.Left, constCellPadding);
  Dec(Result.Right, constCellPadding);
  TextHeight := ACanvas.TextHeight(' ');
  case AColumnLayout of
    tlTop: EditorTop:=Result.Top+constCellPadding;
    tlCenter: EditorTop:=Result.Top+Round((Result.Bottom-Result.Top-TextHeight) / 2);
    tlBottom: EditorTop:=Result.Bottom-constCellPadding-TextHeight+1;
  end;
  if EditorTop>Result.Top then Result.Top:=EditorTop;
  Result.Bottom:=Result.Top+TextHeight;
end;

imptype procedure TWin32WSCustomGrid.SendCharToEditor(AEditor: TWinControl;
  Ch: TUTF8Char);
var
  S: widestring;
  WChar: WPARAM;
begin
  WChar:=WPARAM(Ord(Ch[1]));
  if Length(Ch)>1 then begin
    S := UTF8ToUTF16(Ch);
    if S='' then WChar := WPARAM(Ord('?'))
    else         WChar := WPARAM(S[1]);
  end;
  PostMessageW(AEditor.Handle, WM_CHAR, WChar, 0);
end;
{$ifdef wsintf}
imptype function TWin32WSCustomGrid.InvalidateStartY(const FixedHeight, RowOffset: Integer): integer;
begin
  Result := FixedHeight;
end;
{$endif}

end.
