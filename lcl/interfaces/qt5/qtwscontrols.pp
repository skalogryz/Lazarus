{
 *****************************************************************************
 *                              QtWSControls.pp                              * 
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
unit QtWSControls;

{$mode objfpc}{$H+}

interface

{$I qtdefines.inc}

uses
  // Bindings
  qt5,
  qtwidgets, qtobjects, qtproc, qtint,
  // LCL
  SysUtils, Classes, Types, Controls, LCLType, LazUTF8, Forms, Graphics,
  // Widgetset
  InterfaceBase, WSProc, WSControls, {$ifndef wsintf}WSLCLClasses{$else}WSLCLClasses_Intf{$endif};

type

  { TQtWSDragImageListResolution }

  TQtWSDragImageListResolution = class(TWSDragImageListResolution)
  impsection
    imptype function BeginDrag(const ADragImageList: TDragImageListResolution; Window: HWND; AIndex, X, Y: Integer): Boolean; override;
    imptype function DragMove(const ADragImageList: TDragImageListResolution; X, Y: Integer): Boolean; override;
    imptype procedure EndDrag(const ADragImageList: TDragImageListResolution); override;
    imptype function HideDragImage(const ADragImageList: TDragImageListResolution;
      ALockedWindow: HWND; DoUnLock: Boolean): Boolean; override;
    imptype function ShowDragImage(const ADragImageList: TDragImageListResolution;
      ALockedWindow: HWND; X, Y: Integer; DoLock: Boolean): Boolean; override;
  end;

  { TQtWSLazAccessibleObject }
  {$IFDEF QTACCESSIBILITY}
  TQtWSLazAccessibleObject = class(TWSLazAccessibleObject)
  public
    class function CreateHandle(const AObject: TLazAccessibleObject): HWND; override;
    class procedure DestroyHandle(const AObject: TLazAccessibleObject); override;
    class procedure SetAccessibleRole(const AObject: TLazAccessibleObject; const ARole: TLazAccessibilityRole); override;
  end;
  {$ENDIF}

  { TQtWSControl }

  TQtWSControl = class(TWSControl)
  impsection
  end;

  { TQtWSWinControl }

  TQtWSWinControl = class({$ifndef wsintf}TWSWinControl{$else}TQtWSControl, IWSWinControl{$endif})
  impsection
    imptype function  CanFocus(const AWinControl: TWinControl): Boolean; rootoverride;
    imptype function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): TLCLIntfHandle; rootoverride;
    imptype procedure DestroyHandle(const AWinControl: TWinControl); rootoverride;
    imptype procedure Invalidate(const AWinControl: TWinControl); rootoverride;
    imptype procedure AddControl(const AControl: TControl); override;
    imptype function  GetClientBounds(const AWincontrol: TWinControl; var ARect: TRect): Boolean; rootoverride;
    imptype function  GetClientRect(const AWincontrol: TWinControl; var ARect: TRect): Boolean; rootoverride;
    imptype function GetDesignInteractive(const AWinControl: TWinControl; AClientPos: TPoint): Boolean; rootoverride;

    imptype procedure SetBiDiMode(const AWinControl: TWinControl; UseRightToLeftAlign, UseRightToLeftReading, UseRightToLeftScrollBar : Boolean); rootoverride;
    imptype procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); rootoverride;
    imptype procedure SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle); rootoverride;
    imptype procedure SetPos(const AWinControl: TWinControl; const ALeft, ATop: Integer); rootoverride;
    imptype procedure SetSize(const AWinControl: TWinControl; const AWidth, AHeight: Integer); rootoverride;
    imptype procedure ShowHide(const AWinControl: TWinControl); rootoverride; //TODO: rename to SetVisible(control, visible)
    imptype procedure SetColor(const AWinControl: TWinControl); rootoverride;
    imptype procedure SetCursor(const AWinControl: TWinControl; const ACursor: HCURSOR); rootoverride;
    imptype procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); rootoverride;
    imptype procedure SetShape(const AWinControl: TWinControl; const AShape: HBITMAP); rootoverride;

    imptype procedure GetPreferredSize(const AWinControl: TWinControl;
      var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); rootoverride;

    imptype function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; rootoverride;
    imptype procedure SetText(const AWinControl: TWinControl; const AText: string); rootoverride;

    imptype procedure SetChildZPosition(const AWinControl, AChild: TWinControl;
                                      const AOldPos, ANewPos: Integer;
                                      const AChildren: TFPList); rootoverride;

    imptype procedure ConstraintsChange(const AWinControl: TWinControl); rootoverride;
    imptype procedure PaintTo(const AWinControl: TWinControl; ADC: HDC; X, Y: Integer); rootoverride;
    imptype procedure Repaint(const AWinControl: TWinControl); rootoverride;
    imptype procedure ScrollBy(const AWinControl: TWinControl; DeltaX, DeltaY: integer); rootoverride;
    {$ifdef wsintf}
    imptype function GetDefaultClientRect(const AWinControl: TWinControl; const aLeft, aTop, aWidth, aHeight: integer; var aClientRect: TRect): boolean; rootoverride;
    imptype function GetDoubleBuffered(const AWinControl: TWinControl): Boolean; rootoverride;
    imptype function GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean; rootoverride;
    imptype procedure AdaptBounds(const AWinControl: TWinControl;
        var Left, Top, Width, Height: integer; var SuppressMove: boolean); rootoverride;
    imptype procedure DefaultWndHandler(const AWinControl: TWinControl; var AMessage); rootoverride;
    {$endif}
  end;

  { TQtWSGraphicControl }

  TQtWSGraphicControl = class({$ifndef wsintf}TWSWinControl{$else}TQtWSControl{$endif})
  impsection
  end;

  { TQtWSCustomControl }

  TQtWSCustomControl = class({$ifndef wsintf}TWSCustomControl{$else}TQtWSWinControl{$endif})
  impsection
    imptype function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TQtWSImageList }

  TQtWSImageList = class({$ifndef wsintf}TWSImageList{$else}TQtWSDragImageListResolution{$endif})
  published
  end;

const
  TBorderStyleToQtFrameShapeMap: array[TBorderStyle] of QFrameShape =
  (
 { bsNone   } QFrameNoFrame,
 { bsSingle } QFrameStyledPanel
  );
  TLayoutDirectionMap: array[Boolean] of QtLayoutDirection =
  (
 { False } QtLeftToRight,
 { True  } QtRightToLeft
  );
implementation

uses LCLProc;

{------------------------------------------------------------------------------
  Method: TQtWSCustomControl.CreateHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
imptype function TQtWSCustomControl.CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): TLCLIntfHandle;
var
  QtCustomControl: TQtCustomControl;
begin
  {$ifdef VerboseQt}
    WriteLn('> TQtWSCustomControl.CreateHandle for ',dbgsname(AWinControl));
  {$endif}

  QtCustomControl := TQtCustomControl.Create(AWinControl, AParams);
  QtCustomControl.setFrameShape(TBorderStyleToQtFrameShapeMap[TCustomControl(AWinControl).BorderStyle]);
  QtCustomControl.viewportNeeded;
  QtCustomControl.verticalScrollBar;
  QtCustomControl.horizontalScrollBar;
  QtCustomControl.AttachEvents;
  Result := TLCLIntfHandle(QtCustomControl);

  {$ifdef VerboseQt}
    WriteLn('< TQtWSCustomControl.CreateHandle for ',dbgsname(AWinControl),' Result: ', dbgHex(Result));
  {$endif}
end;

{$IFDEF QTACCESSIBILITY}
class function TQtWSLazAccessibleObject.CreateHandle(const AObject: TLazAccessibleObject): HWND;
var
  widget: QWidgetH;
  WinControl: TWinControl;
  H: TQtWidget;
begin
  QAccessible_installFactory(@QtAxFactory);
  Result := 0;
  if (AObject.OwnerControl <> nil) and (AObject.OwnerControl is TWinControl) and
     (AObject.OwnerControl.GetAccessibleObject() = AObject) then begin
    { Need to improve handling here.  Problem is that we hit here before TWinControl
      has handle allocated but nothing will send us back here once handle is allocated
      thus code in TQtCustomControl.initializeAccessibility that does
      TLazAccessibleObject.handle assignment when TWinControl.Handle created}
    //if TQtWidget(TWinControl(AObject.OwnerControl).HandleAllocated then begin
    //  widget := QWidgetH(TQtWidget(TWinControl(AObject.OwnerControl).Handle).Widget);
    //  Result := HWND(TQtAccessibleObject.Create(AObject, widget));
    // end;
  end
  else begin
    if AObject.AccessibleRole = larTreeItem then
      Result := HWND(TQtAccessibleTreeRow.Create(AObject, QWidgetH(0)))
    else
      Result := HWND(TQtAccessibleObject.Create(AObject, QWidgetH(0)));
  end;
end;

class procedure TQtWSLazAccessibleObject.DestroyHandle(const AObject: TLazAccessibleObject);
begin
  TQtAccessibleObject(AObject.Handle).Free;
end;

class procedure TQtWSLazAccessibleObject.SetAccessibleRole(const AObject: TLazAccessibleObject; const ARole: TLazAccessibilityRole);
begin
  {Need to improve this to do something similar to Cocoa where handle is recreated
   if accessibleRole has changed}
  CreateHandle(AObject);
end;
{$ENDIF}

{------------------------------------------------------------------------------
  Function: TQtWSWinControl.CanFocus
  Params:  TWinControl
  Returns: Boolean
 ------------------------------------------------------------------------------}
imptype function TQtWSWinControl.CanFocus(const AWinControl: TWinControl): Boolean;
var
  Widget: TQtWidget;
begin
  if AWinControl.HandleAllocated then
  begin
    Widget := TQtWidget(AWinControl.Handle);
    Result := (Widget.getFocusPolicy <> QtNoFocus);
  end else
    Result := False;
end;

{------------------------------------------------------------------------------
  Method: TQtWSWinControl.CreateHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
imptype function TQtWSWinControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  QtWidget: TQtWidget;
begin

  {$ifdef VerboseQt}
    WriteLn('> TQtWSWinControl.CreateHandle for ',dbgsname(AWinControl));
  {$endif}
  QtWidget := TQtWidget.Create(AWinControl, AParams);

  QtWidget.AttachEvents;

  // Finalization

  Result := TLCLIntfHandle(QtWidget);

  {$ifdef VerboseQt}
    WriteLn('< TQtWSWinControl.CreateHandle for ',dbgsname(AWinControl),' Result: ', dbgHex(Result));
  {$endif}
end;

{------------------------------------------------------------------------------
  Method: TQtWSWinControl.DestroyHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
imptype procedure TQtWSWinControl.DestroyHandle(const AWinControl: TWinControl);
begin
  TQtWidget(AWinControl.Handle).Release;
end;

{------------------------------------------------------------------------------
  Method: TQtWSWinControl.Invalidate
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
imptype procedure TQtWSWinControl.Invalidate(const AWinControl: TWinControl);
begin
  if not WSCheckHandleAllocated(AWinControl, 'Invalidate') then
    Exit;

  TQtWidget(AWinControl.Handle).Update;
end;

imptype procedure TQtWSWinControl.AddControl(const AControl: TControl);
var
  Child: TQtWidget;
  Parent: TQtWidget;
begin
  if (AControl is TWinControl) and (TWinControl(AControl).HandleAllocated) then
  begin
    Child := TQtWidget(TWinControl(AControl).Handle);
    Parent := TQtWidget(AControl.Parent.Handle);
    if Child.getParent <> Parent.GetContainerWidget then
    begin
      Child.BeginUpdate;
      Child.setParent(Parent.GetContainerWidget);
      Child.EndUpdate;
    end;
  end;
end;

imptype function TQtWSWinControl.GetClientBounds(const AWincontrol: TWinControl;
  var ARect: TRect): Boolean;
begin
  Result := False;
  if not WSCheckHandleAllocated(AWinControl, 'GetClientBounds') then
    Exit;

  ARect := TQtWidget(AWinControl.Handle).getClientBounds;
  Result := True;
end;

imptype function TQtWSWinControl.GetClientRect(const AWincontrol: TWinControl;
  var ARect: TRect): Boolean;
begin
  Result := False;
  if not WSCheckHandleAllocated(AWinControl, 'GetClientRect') then
    Exit;
    
  ARect := TQtWidget(AWinControl.Handle).getClientBounds;
  OffsetRect(ARect, -ARect.Left, -ARect.Top);
  Result := True;
end;

imptype function TQtWSWinControl.GetDesignInteractive(
  const AWinControl: TWinControl; AClientPos: TPoint): Boolean;
begin
  Result := False;
  if not WSCheckHandleAllocated(AWinControl, 'GetDesignInteractive') then
    Exit;
end;

imptype procedure TQtWSWinControl.SetBiDiMode(const AWinControl : TWinControl;
  UseRightToLeftAlign, UseRightToLeftReading, UseRightToLeftScrollBar : Boolean);
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetBiDiMode') then
    Exit;

  TQtWidget(AWinControl.Handle).setLayoutDirection(TLayoutDirectionMap[UseRightToLeftAlign]);
end;

imptype procedure TQtWSWinControl.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  if not WSCheckHandleAllocated(AWinControl, 'GetPreferredSize') then
    Exit;
  TQtWidget(AWinControl.Handle).PreferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);
end;

imptype function TQtWSWinControl.GetText(const AWinControl: TWinControl;
  var AText: String): Boolean;
begin
  Result := False;
  if not WSCheckHandleAllocated(AWincontrol, 'GetText') then
    Exit;
  if not QtWidgetSet.IsValidHandle(AWinControl.Handle) then
    exit;
  Result := not TQtWidget(AWinControl.Handle).getTextStatic;
  if Result then
    AText := UTF16ToUTF8(TQtWidget(AWinControl.Handle).getText);
end;

imptype procedure TQtWSWinControl.SetText(const AWinControl: TWinControl;
  const AText: string);
var
  Wdgt: TQtWidget;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetText') then
    Exit;
  Wdgt := TQtWidget(AWinControl.Handle);
  Wdgt.BeginUpdate;
  Wdgt.setText(GetUtf8String(AText));
  Wdgt.EndUpdate;
end;

imptype procedure TQtWSWinControl.SetChildZPosition(const AWinControl,
                AChild: TWinControl; const AOldPos, ANewPos: Integer; const AChildren: TFPList);
var
  n: Integer;
  Child: TWinControl;
  Reorder: TFPList;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetChildZPosition') then
    Exit;
  if not WSCheckHandleAllocated(AChild, 'SetChildZPosition (child)') then
    Exit;

  if (ANewPos <= 0) or (ANewPos >= AChildren.Count - 1) then
  begin
    // simple
    if ANewPos <= 0 then // bottom
      TQtWidget(AChild.Handle).lowerWidget
    else
      TQtWidget(AChild.Handle).raiseWidget;
  end else
  begin
    if (ANewPos >= 0) and (ANewPos < AChildren.Count -1) then
    begin
      Reorder := TFPList.Create;
      for n := AChildren.Count - 1 downto 0 do
        Reorder.Add(AChildren[n]);
      Child := TWinControl(Reorder[ANewPos + 1]);
      if Child.HandleAllocated then
        TQtWidget(AChild.Handle).stackUnder(TQtWidget(Child.Handle).Widget)
      else
        TQtWidget(AChild.Handle).lowerWidget;
      Reorder.Free;
    end;
  end;
end;

imptype procedure TQtWSWinControl.ConstraintsChange(const AWinControl: TWinControl);
const
  QtMaxContraint = $FFFFFF;
var
  Widget: TQtWidget;
  MW, MH: Integer;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'ConstraintsChange') then
    Exit;
    
  Widget := TQtWidget(AWinControl.Handle);
  with AWinControl do
  begin
    MW := Constraints.MinWidth;
    MH := Constraints.MinHeight;

    if MW < QtMinimumWidgetSize then
      MW := 0;
    if MH < QtMinimumWidgetSize then
      MH := 0;

    Widget.setMinimumSize(MW, MH);

    if Constraints.MaxWidth = 0 then
      MW := QtMaxContraint
    else
      MW := Constraints.MaxWidth;
    if Constraints.MaxHeight = 0 then
      MH := QtMaxContraint
    else
      MH := Constraints.MaxHeight;
    Widget.setMaximumSize(MW, MH);
  end;
end;

imptype procedure TQtWSWinControl.PaintTo(const AWinControl: TWinControl;
  ADC: HDC; X, Y: Integer);
var
  Context: TQtDeviceContext absolute ADC;
  Widget: TQtWidget;
  DCSize: TSize;
  APoint: TQtPoint;
  ARect: TRect;
  Pixmap: QPixmapH;
  ASourceRegion: QRegionH;
  AFlags: Integer;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'PaintTo') or (ADC = 0) then
    Exit;

  Widget := TQtWidget(AWinControl.Handle);
  ARect := Widget.getFrameGeometry;

  with DCSize, ARect do
  begin
    cx := Right - Left;
    cy := Bottom - Top;
  end;
  Pixmap := QPixmap_create(PSize(@DCSize));
  try
    APoint := QtPoint(0, 0);
    ASourceRegion := QRegion_Create(0, 0, DCSize.cx, DCSize.cy);
    AFlags := QWidgetDrawChildren;
    if (Widget is TQtMainWindow) then
      AFlags := AFlags or QWidgetDrawWindowBackground;
    QWidget_render(Widget.Widget, QPaintDeviceH(Pixmap), @APoint, ASourceRegion, AFlags);
    QRegion_destroy(ASourceRegion);

    APoint := QtPoint(X, Y);
    ARect := Rect(0, 0, QPixmap_width(Pixmap), QPixmap_height(Pixmap));
    Context.drawPixmap(@APoint, Pixmap, @ARect);
  finally
    QPixmap_destroy(Pixmap);
  end;
end;

imptype procedure TQtWSWinControl.Repaint(const AWinControl: TWinControl);
begin
  if not WSCheckHandleAllocated(AWinControl, 'Repaint') then
    Exit;
  TQtWidget(AWinControl.Handle).Repaint;
end;

imptype procedure TQtWSWinControl.ScrollBy(const AWinControl: TWinControl;
  DeltaX, DeltaY: integer);
var
  Widget: TQtCustomControl;
  ABar: TQtScrollBar;
  APosition: Integer;
begin
  if not WSCheckHandleAllocated(AWinControl, 'ScrollBy') then
    Exit;
  if TQtWidget(AWinControl.Handle) is TQtCustomControl then
  begin
    Widget := TQtCustomControl(AWinControl.Handle);
    Widget.viewport.scroll(DeltaX, DeltaY);
  end else
  if TQtWidget(AWinControl.Handle) is TQtAbstractScrollArea then
  begin
    ABar := TQtAbstractScrollArea(AWinControl.Handle).horizontalScrollBar;
    if ABar = nil then
      exit;
    if ABar.getTracking then
      APosition := ABar.getSliderPosition
    else
      APosition := ABar.getValue;
    if DeltaX <> 0 then
    begin
      APosition += -DeltaX;
      if ABar.getTracking then
        ABar.setSliderPosition(APosition)
      else
        ABar.setValue(APosition);
    end;
    ABar := TQtAbstractScrollArea(AWinControl.Handle).verticalScrollBar;
    if ABar = nil then
      exit;
    if ABar.getTracking then
      APosition := ABar.getSliderPosition
    else
      APosition := ABar.getValue;
    if DeltaY <> 0 then
    begin
      APosition += -DeltaY;
      if ABar.getTracking then
        ABar.setSliderPosition(APosition)
      else
        ABar.setValue(APosition);
    end;
  end
  {$IFDEF VerboseQt}
  else
    DebugLn(Format('WARNING: TQtWSWinControl.ScrollBy(): Qt widget handle %s is not TQtCustomControl',[DbgSName(TQtWidget(AWinControl.Handle))]));
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Method: TQtWSWinControl.SetBounds
  Params:  AWinControl - the calling object
           ALeft, ATop - Position
           AWidth, AHeight - Size
  Returns: Nothing

  Sets the position and size of a widget
 ------------------------------------------------------------------------------}
imptype procedure TQtWSWinControl.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: Integer);
var
  R: TRect;
  Box: TQtWidget;
  AForm: TCustomForm;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetBounds') then
    Exit;
  R := Rect(ALeft, ATop, AWidth, AHeight);

  Box := nil;
  if Assigned(AWinControl.Parent) and
    AWinControl.Parent.HandleAllocated then
      Box := TQtWidget(AWinControl.Parent.Handle);

  if Assigned(Box) and
    (Box.ChildOfComplexWidget = ccwScrollingWinControl) then
  begin
    R := Rect(ALeft - TQtCustomControl(Box).horizontalScrollBar.getValue,
      ATop - TQtCustomControl(Box).verticalScrollBar.getValue, AWidth, AHeight);
  end;

  {$IFDEF QTSCROLLABLEFORMS}
  if Assigned(AWinControl.Parent) and
    (AWinControl.Parent.FCompStyle = csForm) then
  begin
    AForm := TCustomForm(AWinControl.Parent);
    if Assigned(TQtMainWindow(AForm.Handle).ScrollArea) then
    begin
      Box := TQtMainWindow(AForm.Handle).ScrollArea;
      R := Rect(ALeft - TQtWindowArea(Box).horizontalScrollBar.getValue,
        ATop - TQtWindowArea(Box).verticalScrollBar.getValue, AWidth, AHeight);
    end;
  end;
  {$ENDIF}

  {$IFDEF VerboseQtResize}
  DebugLn('>TQtWSWinControl.SetBounds(',dbgsName(AWinControl),') NewBounds=',dbgs(R));
  {$ENDIF}
  TQtWidget(AWinControl.Handle).BeginUpdate;
  with R do
  begin
    TQtWidget(AWinControl.Handle).move(Left, Top);
    TQtWidget(AWinControl.Handle).resize(Right, Bottom);
  end;
  TQtWidget(AWinControl.Handle).EndUpdate;
  {$IFDEF VerboseQtResize}
  DebugLn('<TQtWSWinControl.SetBounds(',dbgsName(AWinControl),') NewBounds=',dbgs(R));
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Method: TQtWSWinControl.SetPos
  Params:  AWinControl - the calling object
           ALeft, ATop - Position
  Returns: Nothing

  Sets the position of a widget
 ------------------------------------------------------------------------------}
imptype procedure TQtWSWinControl.SetPos(const AWinControl: TWinControl;
  const ALeft, ATop: Integer);
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetPos') then
    Exit;

  TQtWidget(AWinControl.Handle).BeginUpdate;
  TQtWidget(AWinControl.Handle).move(ALeft, ATop);
  TQtWidget(AWinControl.Handle).EndUpdate;
end;

{------------------------------------------------------------------------------
  Method: TQtWSWinControl.SetSize
  Params:  AWinControl     - the calling object
           AWidth, AHeight - Size
  Returns: Nothing

  Sets the size of a widget
 ------------------------------------------------------------------------------}
imptype procedure TQtWSWinControl.SetSize(const AWinControl: TWinControl;
  const AWidth, AHeight: Integer);
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetSize') then
    Exit;
  TQtWidget(AWinControl.Handle).BeginUpdate;
  TQtWidget(AWinControl.Handle).resize(AWidth, AHeight);
  TQtWidget(AWinControl.Handle).EndUpdate;
end;

{------------------------------------------------------------------------------
  Method: TQtWSWinControl.ShowHide
  Params:  AWinControl     - the calling object

  Returns: Nothing

  Shows or hides a widget.
 ------------------------------------------------------------------------------}
imptype procedure TQtWSWinControl.ShowHide(const AWinControl: TWinControl);
var
  Widget: TQtWidget;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'ShowHide') then
    Exit;

  Widget := TQtWidget(AWinControl.Handle);
  Widget.BeginUpdate;
  // issue #28437, #30966 - regression from r53365: when FontChanged() is called
  // here handle is recreated inside LCL, so we are dead - SEGFAULT.
  if AWinControl.HandleObjectShouldBeVisible and
    IsFontNameDefault(AWinControl.Font.Name) then
  begin
    if AWinControl.IsParentFont and Assigned(AWinControl.Parent) then
      SetFont(AWinControl, AWinControl.Parent.Font) {DO NOT TOUCH THIS PLEASE !}
    else
      SetFont(AWinControl, AWinControl.Font); {DO NOT TOUCH THIS PLEASE !}
  end;

  Widget.setVisible(AWinControl.HandleObjectShouldBeVisible);
  Widget.EndUpdate;
end;

{------------------------------------------------------------------------------
  Method: TQtWSWinControl.SetColor
  Params:  AWinControl     - the calling object
  Returns: Nothing

  Sets the color of the widget.
 ------------------------------------------------------------------------------}
imptype procedure TQtWSWinControl.SetColor(const AWinControl: TWinControl);
var
  QColor: TQColor;
  ColorRef: TColorRef;
  QtWidget: TQtWidget;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetColor') then
    Exit;

  QtWidget := TQtWidget(AWinControl.Handle);
  QtWidget.BeginUpdate;
  QtWidget.WidgetState := QtWidget.WidgetState + [qtwsColorUpdating];
  try
    // Get the color numeric value (system colors are mapped to numeric colors depending on the widget style)
    if AWinControl.Color = clDefault then
      QtWidget.SetDefaultColor(dctBrush)
    else
    begin
      ColorRef := ColorToRGB(AWinControl.Color);

      // Fill QColor
      QColor_fromRgb(@QColor,Red(ColorRef),Green(ColorRef),Blue(ColorRef));

      // Set color of the widget to QColor
      QtWidget.SetColor(@QColor);
    end;
  finally
    QtWidget.WidgetState := QtWidget.WidgetState - [qtwsColorUpdating];
    QtWidget.EndUpdate;
  end;
end;

{------------------------------------------------------------------------------
  Method: TQtWSWinControl.SetCursor
  Params:  AWinControl     - the calling object
  Returns: Nothing

  Sets the cursor of the widget.
 ------------------------------------------------------------------------------}
imptype procedure TQtWSWinControl.SetCursor(const AWinControl: TWinControl; const ACursor: HCURSOR);
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetCursor') then
    Exit;
  if ACursor <> 0 then
    TQtWidget(AWinControl.Handle).SetCursor(TQtCursor(ACursor).Handle)
  else
    TQtWidget(AWinControl.Handle).SetCursor(nil);
end;

{------------------------------------------------------------------------------
  Method: TQtWSWinControl.SetFont
  Params:  AWinControl - the calling object, AFont - object font
  Returns: Nothing

  Sets the font of the widget.
 ------------------------------------------------------------------------------}
imptype procedure TQtWSWinControl.SetFont(const AWinControl: TWinControl; const AFont: TFont);
var
  QtWidget: TQtWidget;
  QColor: TQColor;
  ColorRef: TColorRef;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetFont') then
    Exit;

  QtWidget := TQtWidget(AWinControl.Handle);
  QtWidget.BeginUpdate;
  QtWidget.WidgetState := QtWidget.WidgetState + [qtwsFontUpdating];
  try
    QtWidget.SetLCLFont(TQtFont(AFont.Reference.Handle));
    QtWidget.setFont(TQtFont(AFont.Reference.Handle).FHandle);

    // tscrollbar, ttrackbar etc.
    if not QtWidget.CanChangeFontColor then
    begin
      with QtWidget do
      begin
        Palette.ForceColor := True;
        setDefaultColor(dctFont);
        Palette.ForceColor := False;
      end;
      exit;
    end;

    if AFont.Color = clDefault then
      QtWidget.SetDefaultColor(dctFont)
    else
    begin
      ColorRef := ColorToRGB(AFont.Color);
      QColor_fromRgb(@QColor,Red(ColorRef),Green(ColorRef),Blue(ColorRef));
      QtWidget.SetTextColor(@QColor);
    end;
  finally
    QtWidget.WidgetState := QtWidget.WidgetState - [qtwsFontUpdating];
    QtWidget.EndUpdate;
  end;
end;

imptype procedure TQtWSWinControl.SetShape(const AWinControl: TWinControl;
  const AShape: HBITMAP);
var
  Widget: TQtWidget;
  Shape: TQtImage;
  AMask: QBitmapH;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetShape') then
    Exit;
  Widget := TQtWidget(AWinControl.Handle);

  if AShape <> 0 then
  begin
    Shape := TQtImage(AShape);
    // invert white/black
    Shape.invertPixels;
    AMask := Shape.AsBitmap;
    Widget.setMask(AMask);
    QBitmap_destroy(AMask);
    // invert back
    Shape.invertPixels;
  end
  else
    Widget.clearMask;
end;

imptype procedure TQtWSWinControl.SetBorderStyle(const AWinControl: TWinControl;
  const ABorderStyle: TBorderStyle);
var
  Widget: TQtWidget;
  QtEdit: IQtEdit;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetBorderStyle') then
    Exit;
    
  Widget := TQtWidget(AWinControl.Handle);
  QtEdit := nil;
  if Widget is TQtFrame then
    TQtFrame(Widget).setFrameShape(TBorderStyleToQtFrameShapeMap[ABorderStyle])
  else
  if Supports(Widget, IQtEdit, QtEdit) then
    QtEdit.setBorder(ABorderStyle = bsSingle);
end;

{$ifdef wsintf}
imptype function TQtWSWinControl.GetDefaultClientRect(const AWinControl: TWinControl; const aLeft, aTop, aWidth, aHeight: integer; var aClientRect: TRect): boolean;
begin
  Result:=false;
end;

imptype function TQtWSWinControl.GetDoubleBuffered(const AWinControl: TWinControl): Boolean;
begin
  Result := AWinControl.DoubleBuffered;
end;

imptype function TQtWSWinControl.GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean;
var
  S: String;
begin
  Result := GetText(AWinControl, S);
  if Result
  then ALength := Length(S);
end;

imptype procedure TQtWSWinControl.AdaptBounds(const AWinControl: TWinControl;
    var Left, Top, Width, Height: integer; var SuppressMove: boolean);
begin
end;

imptype procedure TQtWSWinControl.DefaultWndHandler(const AWinControl: TWinControl; var AMessage);
begin
  WidgetSet.CallDefaultWndHandler(AWinControl, AMessage);
end;
{$endif}

{ TQtWSDragImageListResolution }

imptype function TQtWSDragImageListResolution.BeginDrag(
  const ADragImageList: TDragImageListResolution; Window: HWND; AIndex, X, Y: Integer): Boolean;
var
  ABitmap: TBitmap;
begin
  ABitmap := TBitmap.Create;
  try
    ADragImageList.GetBitmap(AIndex, ABitmap);

    if (ABitmap.Handle = 0) or (ABitmap.Width = 0) or (ABitmap.Height = 0) then
    begin
      Result := False;
      Exit;
    end;

    Result := TQtWidgetset(Widgetset).DragImageList_BeginDrag(
      TQtImage(ABitmap.Handle).Handle, ADragImageList.DragHotSpot);
    if Result then
      TQtWidgetset(Widgetset).DragImageList_DragMove(X, Y);
  finally
    ABitmap.Free;
  end;
end;

imptype function TQtWSDragImageListResolution.DragMove(
  const ADragImageList: TDragImageListResolution; X, Y: Integer): Boolean;
begin
  Result := TQtWidgetset(Widgetset).DragImageList_DragMove(X, Y);
end;

imptype procedure TQtWSDragImageListResolution.EndDrag(const ADragImageList: TDragImageListResolution);
begin
  TQtWidgetset(Widgetset).DragImageList_EndDrag;
end;

imptype function TQtWSDragImageListResolution.HideDragImage(
  const ADragImageList: TDragImageListResolution; ALockedWindow: HWND; DoUnLock: Boolean
  ): Boolean;
begin
  Result := True;
  if DoUnlock then
  begin
    TQtWidgetset(Widgetset).DragImageLock := False;
    Result := TQtWidgetset(Widgetset).DragImageList_SetVisible(False);
  end;
end;

imptype function TQtWSDragImageListResolution.ShowDragImage(
  const ADragImageList: TDragImageListResolution; ALockedWindow: HWND; X, Y: Integer;
  DoLock: Boolean): Boolean;
begin
  Result := TQtWidgetset(Widgetset).DragImageLock;
  if not DoLock then
  begin
    if not Result then
      Result := TQtWidgetset(Widgetset).DragImageList_SetVisible(True);
  end else
  begin
    TQtWidgetset(Widgetset).DragImageLock := True;
    Result := TQtWidgetset(Widgetset).DragImageList_DragMove(X, Y) and
      TQtWidgetset(Widgetset).DragImageList_SetVisible(True);
  end;
end;

end.
