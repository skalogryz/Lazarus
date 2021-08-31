{
 *****************************************************************************
 *                              QtWSButtons.pp                               * 
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
unit QtWSButtons;

{$mode objfpc}{$H+}

interface

{$I qtdefines.inc}

uses
  // Libs
  qt5,
  qtwidgets, qtobjects,
  // RTL
  SysUtils, Types,
  // LCL
  Controls, LCLType, Forms, InterfaceBase, Buttons, Graphics, ImgList,
  // LazUtils
  GraphType,
  // Widgetset
  WSProc, WSButtons, {$ifndef wsintf}WSLCLClasses{$else}WSLCLClasses_Intf{$endif};

type

  { TQtWSBitBtn }

  TQtWSBitBtn = class(TWSBitBtn)
  impsection
    imptype function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    imptype procedure SetGlyph(const ABitBtn: TCustomBitBtn; const AValue: TButtonGlyph); rootoverride;
    imptype procedure SetLayout(const ABitBtn: TCustomBitBtn; const AValue: TButtonLayout); rootoverride;
    imptype procedure SetMargin(const ABitBtn: TCustomBitBtn; const AValue: Integer); rootoverride;
    imptype procedure SetSpacing(const ABitBtn: TCustomBitBtn; const AValue: Integer); rootoverride;
  end;

  { TQtWSSpeedButton }

  TQtWSSpeedButton = class(TWSSpeedButton)
  published
  end;


implementation


{ TQtWSBitBtn }

imptype function TQtWSBitBtn.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  QtBitBtn: TQtBitBtn;
begin
  QtBitBtn := TQtBitBtn.Create(AWinControl, AParams);
  QtBitBtn.AttachEvents;
  Result := TLCLIntfHandle(QtBitBtn);
end;

{------------------------------------------------------------------------------
  Function: TQtWSBitBtn.SetGlyph
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
imptype procedure TQtWSBitBtn.SetGlyph(const ABitBtn: TCustomBitBtn; const AValue: TButtonGlyph);
const
  IconModeToButtonState: array[QIconMode] of TButtonState =
  (
{ QIconNormal   } bsUp,
{ QIconDisabled } bsDisabled,
{ QIconActive   } bsHot,
{ QIconSelected } bsDown
  );

var
  AIcon: QIconH;
  APixmap: QPixmapH;
  AGlyph: TBitmap;
  AIndex: Integer;
  AEffect: TGraphicsDrawEffect;
  Mode: QIconMode;
  ASize: TSize;
  AImageRes: TScaledImageListResolution;
begin
  if not WSCheckHandleAllocated(ABitBtn, 'SetGlyph') then
    Exit;

  TQtBitBtn(ABitBtn.Handle).GlyphLayout := Ord(ABitBtn.Layout);
  AIcon := QIcon_create();
  if ABitBtn.CanShowGlyph(True) then
  begin
    AGlyph := TBitmap.Create;
    APixmap := QPixmap_create();

    for Mode := QIconNormal to QIconSelected do
    begin
      AValue.GetImageIndexAndEffect(IconModeToButtonState[Mode],
        ABitBtn.Font.PixelsPerInch, ABitBtn.GetCanvasScaleFactor,
        AImageRes, AIndex, AEffect);
      AImageRes.GetBitmap(AIndex, AGlyph, AEffect);
      QPixmap_fromImage(APixmap, TQtImage(AGlyph.Handle).Handle);
      QIcon_addPixmap(AIcon, APixmap, Mode, QIconOn);
    end;
    QPixmap_destroy(APixmap);
    AGlyph.Free;

    ASize.cx := AImageRes.Width;
    ASize.cy := AImageRes.Height;
    TQtBitBtn(ABitBtn.Handle).setIconSize(@ASize);
  end;

  TQtBitBtn(ABitBtn.Handle).setIcon(AIcon);
  QIcon_destroy(AIcon);
end;

imptype procedure TQtWSBitBtn.SetLayout(const ABitBtn: TCustomBitBtn;
  const AValue: TButtonLayout);
begin
  if not WSCheckHandleAllocated(ABitBtn, 'SetLayout') then
    Exit;
  TQtBitBtn(ABitBtn.Handle).GlyphLayout := Ord(ABitBtn.Layout);
  if TQtBitBtn(ABitBtn.Handle).getVisible then
    TQtBitBtn(ABitBtn.Handle).Update(nil);
end;

imptype procedure TQtWSBitBtn.SetMargin(const ABitBtn: TCustomBitBtn;
  const AValue: Integer);
begin
  if not WSCheckHandleAllocated(ABitBtn, 'SetMargin') then
    Exit;
  if TQtBitBtn(ABitBtn.Handle).getVisible then
    TQtBitBtn(ABitBtn.Handle).Update(nil);
end;

imptype procedure TQtWSBitBtn.SetSpacing(const ABitBtn: TCustomBitBtn;
  const AValue: Integer);
begin
  if not WSCheckHandleAllocated(ABitBtn, 'SetSpacing') then
    Exit;
  if TQtBitBtn(ABitBtn.Handle).getVisible then
    TQtBitBtn(ABitBtn.Handle).Update(nil);
end;

end.
