{
 *****************************************************************************
 *                               QtWSGrids.pp                                * 
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
unit QtWSGrids;

{$mode objfpc}{$H+}

interface

{$I qtdefines.inc}

uses
  Controls, Types, Graphics, Grids, LCLType,
  // Widgetset
  WSGrids,
  {$ifndef wsintf}WSLCLClasses
  {$else}
  LazUTF8, StdCtrls, QtWSControls, WSLCLClasses_Intf{$endif};

type

  { TQtWSCustomGrid }

  TQtWSCustomGrid = class({$ifndef wsintf}TWSCustomGrid{$else}TQtWSWinControl, IWSCustomGrid{$endif})
  impsection
    imptype function GetEditorBoundsFromCellRect(ACanvas: TCanvas;
      const ACellRect: TRect; const AColumnLayout: TTextLayout): TRect; rootoverride;
    {$ifdef wsintf}
    imptype procedure SendCharToEditor(AEditor:TWinControl; Ch: TUTF8Char); rootoverride;
    imptype function InvalidateStartY(const FixedHeight, RowOffset: Integer): integer; rootoverride;
    {$endif}
  end;


implementation

{ TQtWSCustomGrid }

imptype function TQtWSCustomGrid.GetEditorBoundsFromCellRect(ACanvas: TCanvas;
  const ACellRect: TRect; const AColumnLayout: TTextLayout): TRect;
var
  EditorTop: LongInt;
  TextHeight: Integer;
begin
  Result:=ACellRect;
  Inc(Result.Right);
  Dec(Result.Bottom);
  TextHeight := ACanvas.TextHeight(' ');
  case AColumnLayout of
    tlTop: EditorTop:=Result.Top+constCellPadding;
    tlCenter: EditorTop:=Result.Top+Round((Result.Bottom-Result.Top-TextHeight+1) div 2);
    tlBottom: EditorTop:=Result.Bottom-constCellPadding-TextHeight+1;
  end;
  if EditorTop>Result.Top then Result.Top:=EditorTop;
  Result.Bottom:=Result.Top+TextHeight;
end;

{$ifdef wsintf}
type
  TCustomGridAccess=class(TCustomGrid)
  end;

imptype procedure TQtWSCustomGrid.SendCharToEditor(AEditor:TWinControl; Ch: TUTF8Char);
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

imptype function TQtWSCustomGrid.InvalidateStartY(const FixedHeight, RowOffset: Integer): integer;
begin
  Result := FixedHeight;
end;
{$endif}

end.
