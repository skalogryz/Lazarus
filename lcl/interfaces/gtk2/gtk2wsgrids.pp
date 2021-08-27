{ $Id$}
{
 *****************************************************************************
 *                              Gtk2WSGrids.pp                               * 
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
unit Gtk2WSGrids;

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
  Types, Grids, Graphics, LCLType,
////////////////////////////////////////////////////
  WSGrids,
  Controls,
  {$ifdef wsintf}
  StdCtrls, Gtk2WSControls, LazUTF8, WSLCLClasses_Intf
  {$else}
  WSLCLClasses
  {$endif}
  ;

type

  { TGtk2WSCustomGrid }

  TGtk2WSCustomGrid = class({$ifndef wsintf}TWSCustomGrid{$else}TGtk2WSWinControl, IWSCustomGrid{$endif})
  impsection
    imptype function GetEditorBoundsFromCellRect(ACanvas: TCanvas;
      const ACellRect: TRect; const AColumnLayout: TTextLayout): TRect; rootoverride;
    {$ifndef wsintf}
    imptype procedure Invalidate(sender: TCustomGrid); override;
    {$endif}
    {$ifdef wsintf}
    imptype procedure SendCharToEditor(AEditor:TWinControl; Ch: TUTF8Char); rootoverride;
    imptype function InvalidateStartY(const FixedHeight, RowOffset: Integer): integer; rootoverride;
    {$endif}
  end;

implementation

{ TGtk2WSCustomGrid }

imptype function TGtk2WSCustomGrid.GetEditorBoundsFromCellRect(ACanvas: TCanvas;
  const ACellRect: TRect; const AColumnLayout: TTextLayout): TRect;
var
  EditorTop: LongInt;
  TextHeight: Integer;
begin
  Result:=ACellRect;
  Inc(Result.Left);
  Dec(Result.Right, 2);
  Dec(Result.Bottom);
  TextHeight := ACanvas.TextHeight(' ');
  case AColumnLayout of
    tlTop: EditorTop:=Result.Top+constCellPadding;
    tlCenter: EditorTop:=Result.Top+(Result.Bottom-Result.Top-TextHeight+1) div 2;
    tlBottom: EditorTop:=Result.Bottom-constCellPadding-TextHeight+1;
  end;
  if EditorTop>Result.Top then Result.Top:=EditorTop;
  Result.Bottom:=Result.Top+TextHeight;
end;
{$ifndef wsintf}
imptype procedure TGtk2WSCustomGrid.Invalidate(sender: TCustomGrid);
begin
  Sender.Invalidate;
end;
{$endif}
{$ifdef wsintf}
type
  TCustomGridAccess=class(TCustomGrid)
  end;

imptype procedure TGtk2WSCustomGrid.SendCharToEditor(AEditor:TWinControl; Ch: TUTF8Char);
var
  GMsg: TGridMessage;
  GridEditor: boolean;
begin
  GMsg.Grid := nil;
  GMsg.Options:= 0;
  GMsg.LclMsg.Msg:=GM_GETGRID;
  AEditor.Dispatch(GMsg);
  GridEditor := (GMsg.Options and EO_IMPLEMENTED<>0) and (GMsg.Grid<>nil);

  GMsg.LclMsg.Msg:=GM_SETVALUE;
  if Ch=#8 then // backspace
    GMsg.Value:=''
  else
    GMsg.Value:=Ch;

  if GridEditor then
    AEditor.Dispatch(GMsg)
  else begin
    // TODO: Find a generic way ...
    if AEditor is TCustomEdit then begin
      TCustomEdit(AEditor).Text:=GMsg.Value;
      TCustomEdit(AEditor).SelStart:=UTF8Length(GMsg.Value);
    end else
    if AEditor is TCustomCombobox then begin
      TCustomCombobox(AEditor).Text:=GMsg.Value;
      TCustomCombobox(AEditor).SelStart:=UTF8Length(GMsg.Value);
    end;
  end;

  // make sure the grid is notified that some text is changed, some
  // widgets do not notify when they are modified programmatically.
  if GMsg.Grid<>nil then
    with TCustomGridAccess(GMsg.Grid) do
      EditorTextChanged(Col, Row, GMsg.Value);
end;

imptype function TGtk2WSCustomGrid.InvalidateStartY(const FixedHeight, RowOffset: Integer): integer;
begin
  result := FixedHeight;
end;

{$endif}
end.
