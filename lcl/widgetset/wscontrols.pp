{
 *****************************************************************************
 *                               WSControls.pp                               * 
 *                               -------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit WSControls;

{$mode objfpc}{$H+}
{$I lcl_defines.inc}

interface
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// 1) Only class methods allowed
// 2) Class methods have to be published and virtual
// 3) To get as little as posible circles, the uses
//    clause should contain only those LCL units 
//    needed for registration. WSxxx units are OK
// 4) To improve speed, register only classes in the 
//    initialization section which actually 
//    implement something
// 5) To enable your XXX widgetset units, look at
//    the uses clause of the XXXintf.pp
////////////////////////////////////////////////////
uses
  Classes, Types,
////////////////////////////////////////////////////
// To get as little as possible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Controls, Graphics, LCLType,
////////////////////////////////////////////////////
  {$ifdef WSINTF}WSLCLClasses_Intf{$else}WSLCLClasses{$endif}, WSImgList,
  { TODO: remove when CreateHandle/Component code moved }
  InterfaceBase, WSFactory;

const
  DefBtnColors: array[TDefaultColorType] of TColor = (
 { dctBrush } clBtnFace,
 { dctFont  } clBtnText
  );
type
  {$ifdef WSINTF}TWSDragImageListResolutionClass = interface (TWSCustomImageListResolutionClass)
    function BeginDrag(const ADragImageList: TDragImageListResolution; Window: HWND; AIndex, X, Y: Integer): Boolean; virtual;
    function DragMove(const ADragImageList: TDragImageListResolution; X, Y: Integer): Boolean; virtual;
    procedure EndDrag(const ADragImageList: TDragImageListResolution); virtual;
    function HideDragImage(const ADragImageList: TDragImageListResolution;
    ALockedWindow: HWND; DoUnLock: Boolean): Boolean; virtual;
    function ShowDragImage(const ADragImageList: TDragImageListResolution;
      ALockedWindow: HWND; X, Y: Integer; DoLock: Boolean): Boolean; virtual;
  end;{$endif}

  { TWSDragImageListResolution }

  TWSDragImageListResolution = class(TWSCustomImageListResolution {$ifdef WSINTF},TWSDragImageListResolutionClass{$endif})
  protected
    function _BeginDrag(const ADragImageList: TDragImageListResolution; Window: HWND; AIndex, X, Y: Integer): Boolean; virtual;
    function _DragMove(const ADragImageList: TDragImageListResolution; X, Y: Integer): Boolean; virtual;
    procedure _EndDrag(const ADragImageList: TDragImageListResolution); virtual;
    function _HideDragImage(const ADragImageList: TDragImageListResolution;
    ALockedWindow: HWND; DoUnLock: Boolean): Boolean; virtual;
    function _ShowDragImage(const ADragImageList: TDragImageListResolution;
      ALockedWindow: HWND; X, Y: Integer; DoLock: Boolean): Boolean; virtual;
    function TWSDragImageListResolutionClass.BeginDrag     = _BeginDrag;
    function TWSDragImageListResolutionClass.DragMove      = _DragMove;
    procedure TWSDragImageListResolutionClass.EndDrag      = _EndDrag;
    function TWSDragImageListResolutionClass.HideDragImage = _HideDragImage;
    function TWSDragImageListResolutionClass.ShowDragImage = _ShowDragImage;
  {$ifndef WSINTF}published{$else}public{$endif}
    class function BeginDrag(const ADragImageList: TDragImageListResolution; Window: HWND; AIndex, X, Y: Integer): Boolean; virtual;
    class function DragMove(const ADragImageList: TDragImageListResolution; X, Y: Integer): Boolean; virtual;
    class procedure EndDrag(const ADragImageList: TDragImageListResolution); virtual;
    class function HideDragImage(const ADragImageList: TDragImageListResolution;
      ALockedWindow: HWND; DoUnLock: Boolean): Boolean; virtual;
    class function ShowDragImage(const ADragImageList: TDragImageListResolution;
      ALockedWindow: HWND; X, Y: Integer; DoLock: Boolean): Boolean; virtual;
  end;

  {$ifndef WSINTF}TWSDragImageListResolutionClass = class ofTWSDragImageListResolution;{$endif}

  { TWSLazAccessibleObject }

  TWSLazAccessibleObject = class(TWSObject)
  public
    class function CreateHandle(const AObject: TLazAccessibleObject): HWND; virtual;
    class procedure DestroyHandle(const AObject: TLazAccessibleObject); virtual;
    class procedure SetAccessibleName(const AObject: TLazAccessibleObject; const AName: string); virtual;
    class procedure SetAccessibleDescription(const AObject: TLazAccessibleObject; const ADescription: string); virtual;
    class procedure SetAccessibleValue(const AObject: TLazAccessibleObject; const AValue: string); virtual;
    class procedure SetAccessibleRole(const AObject: TLazAccessibleObject; const ARole: TLazAccessibilityRole); virtual;
    class procedure SetPosition(const AObject: TLazAccessibleObject; const AValue: TPoint); virtual;
    class procedure SetSize(const AObject: TLazAccessibleObject; const AValue: TSize); virtual;
  end;
  TWSLazAccessibleObjectClass = class of TWSLazAccessibleObject;

  { TWSControl }

  TWSControl = class(TWSLCLComponent)
  {$ifndwf WSINTF}published{$else}public{$endif}
    class procedure AddControl(const AControl: TControl); virtual;
    class function GetConstraints(const AControl: TControl; const AConstraints: TObject): Boolean; virtual;
    class function GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor; virtual;
    class procedure ConstraintWidth(const AControl: TControl; const AConstraints: TObject; var aWidth: integer); virtual;
    class procedure ConstraintHeight(const AControl: TControl; const AConstraints: TObject; var aHeight: integer); virtual;
    class function GetCanvasScaleFactor(const AControl: TControl): Double; virtual;
  end;

  {$ifndef WSINTF}TWSControlClass = class of TWSControl;{$endif}

  { TWSWinControl }

  TWSZPosition = (wszpBack, wszpFront);
  
  { TWSWinControl }

  TWSWinControl = {$ifndef WSINTF}class(TWSControl)
  published{$else}interface(TWSControl){$endif}
    {$ifndef WSINTF}class{$endif} function  CanFocus(const AWincontrol: TWinControl): Boolean; virtual;
    
    {$ifndef WSINTF}class{$endif} function  GetClientBounds(const AWincontrol: TWinControl; var ARect: TRect): Boolean; virtual;
    {$ifndef WSINTF}class{$endif} function  GetClientRect(const AWincontrol: TWinControl; var ARect: TRect): Boolean; virtual;
    {$ifndef WSINTF}class{$endif} procedure GetPreferredSize(const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); virtual;
    {$ifndef WSINTF}class{$endif} function  GetDefaultClientRect(const AWinControl: TWinControl; const aLeft, aTop, aWidth, aHeight: integer; var aClientRect: TRect): boolean; virtual;
    {$ifndef WSINTF}class{$endif} function GetDesignInteractive(const AWinControl: TWinControl; AClientPos: TPoint): Boolean; virtual;
    {$ifndef WSINTF}class{$endif} function GetDoubleBuffered(const AWinControl: TWinControl): Boolean; virtual;
    {$ifndef WSINTF}class{$endif} function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; virtual;
    {$ifndef WSINTF}class{$endif} function  GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean; virtual;

    {$ifndef WSINTF}class{$endif} procedure SetBiDiMode(const AWinControl: TWinControl; UseRightToLeftAlign, UseRightToLeftReading, UseRightToLeftScrollBar : Boolean); virtual;
    {$ifndef WSINTF}class{$endif} procedure SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle); virtual;
    {$ifndef WSINTF}class{$endif} procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); virtual;
    {$ifndef WSINTF}class{$endif} procedure SetColor(const AWinControl: TWinControl); virtual;
    {$ifndef WSINTF}class{$endif} procedure SetChildZPosition(const AWinControl, AChild: TWinControl; const AOldPos, ANewPos: Integer; const AChildren: TFPList); virtual;
    {$ifndef WSINTF}class{$endif} procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); virtual;
    {$ifndef WSINTF}class{$endif} procedure SetPos(const AWinControl: TWinControl; const ALeft, ATop: Integer); virtual;
    {$ifndef WSINTF}class{$endif} procedure SetSize(const AWinControl: TWinControl; const AWidth, AHeight: Integer); virtual;
    {$ifndef WSINTF}class{$endif} procedure SetText(const AWinControl: TWinControl; const AText: String); virtual;
    {$ifndef WSINTF}class{$endif} procedure SetCursor(const AWinControl: TWinControl; const ACursor: HCursor); virtual;
    {$ifndef WSINTF}class{$endif} procedure SetShape(const AWinControl: TWinControl; const AShape: HBITMAP); virtual;

    { TODO: move AdaptBounds: it is only used in winapi interfaces }
    {$ifndef WSINTF}class{$endif} procedure AdaptBounds(const AWinControl: TWinControl;
          var Left, Top, Width, Height: integer; var SuppressMove: boolean); virtual;
          
    {$ifndef WSINTF}class{$endif} procedure ConstraintsChange(const AWinControl: TWinControl); virtual;
    {$ifndef WSINTF}class{$endif} function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; virtual;
    {$ifndef WSINTF}class{$endif} procedure DestroyHandle(const AWinControl: TWinControl); virtual;
    {$ifndef WSINTF}class{$endif} procedure DefaultWndHandler(const AWinControl: TWinControl; var AMessage); virtual;
    {$ifndef WSINTF}class{$endif} procedure Invalidate(const AWinControl: TWinControl); virtual;
    {$ifndef WSINTF}class{$endif} procedure PaintTo(const AWinControl: TWinControl; ADC: HDC; X, Y: Integer); virtual;
    {$ifndef WSINTF}class{$endif} procedure Repaint(const AWinControl: TWinControl); virtual;
    {$ifndef WSINTF}class{$endif} procedure ShowHide(const AWinControl: TWinControl); virtual; //TODO: rename to SetVisible(control, visible)
    {$ifndef WSINTF}class{$endif} procedure ScrollBy(const AWinControl: TWinControl; DeltaX, DeltaY: integer); virtual;
  end;
  TWSWinControlClass = {$ifndef WSINTF}class of{$endif} TWSWinControl;

  { TWSGraphicControl }

  TWSGraphicControl = {$ifndef WSINTF}class (TWSControl)
  published{$else}interface(TWSControl)
  end;{$endif}

  { TWSCustomControl }

  TWSCustomControl = {$ifndef WSINTF}class(TWSWinControl)
  published{$else}interface(TWSWinControl){$endif}
  end;

  { TWSImageList }

  TWSImageList = {$ifndef WSINTF}class(TWSDragImageListResolution)
  published{$else}interface(TWSDragImageListResolution){$endif}
  end;

procedure RegisterDragImageListResolution;
procedure RegisterLazAccessibleObject;
procedure RegisterControl;
procedure RegisterWinControl;
procedure RegisterGraphicControl;
procedure RegisterCustomControl;

implementation

{ TWSLazAccessibleObject }

class function TWSLazAccessibleObject.CreateHandle(
  const AObject: TLazAccessibleObject): HWND;
begin
  Result := 0;
end;

class procedure TWSLazAccessibleObject.DestroyHandle(
  const AObject: TLazAccessibleObject);
begin

end;

class procedure TWSLazAccessibleObject.SetAccessibleName(const AObject: TLazAccessibleObject; const AName: string);
begin

end;

class procedure TWSLazAccessibleObject.SetAccessibleDescription(const AObject: TLazAccessibleObject; const ADescription: string);
begin

end;

class procedure TWSLazAccessibleObject.SetAccessibleValue(const AObject: TLazAccessibleObject; const AValue: string);
begin

end;

class procedure TWSLazAccessibleObject.SetAccessibleRole(const AObject: TLazAccessibleObject; const ARole: TLazAccessibilityRole);
begin

end;

class procedure TWSLazAccessibleObject.SetPosition(
  const AObject: TLazAccessibleObject; const AValue: TPoint);
begin

end;

class procedure TWSLazAccessibleObject.SetSize(
  const AObject: TLazAccessibleObject; const AValue: TSize);
begin

end;

{ TWSControl }

class procedure TWSControl.AddControl(const AControl: TControl);
begin
end;

class function TWSControl.GetConstraints(const AControl: TControl; const AConstraints: TObject): Boolean;
begin
  Result := WidgetSet.GetControlConstraints(AConstraints);
end;

class function TWSControl.GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor;
begin
  Result := clDefault;
end;

class procedure TWSControl.ConstraintWidth(const AControl: TControl;
  const AConstraints: TObject; var aWidth: integer);
begin

end;

class procedure TWSControl.ConstraintHeight(const AControl: TControl;
  const AConstraints: TObject; var aHeight: integer);
begin

end;

class function TWSControl.GetCanvasScaleFactor(const AControl: TControl): Double;
begin
  Result := 1;
end;

{ TWSWinControl }

class procedure TWSWinControl.AdaptBounds(const AWinControl: TWinControl;
  var Left, Top, Width, Height: integer; var SuppressMove: boolean);
begin
end;

class procedure TWSWinControl.ConstraintsChange(const AWinControl: TWinControl);
begin
end;

class function TWSWinControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  // For now default to the old creation routines
  Result := 0;
end;

class procedure TWSWinControl.DestroyHandle(const AWinControl: TWinControl);
begin
end;

class procedure TWSWinControl.DefaultWndHandler(const AWinControl: TWinControl; var AMessage);
begin
  WidgetSet.CallDefaultWndHandler(AWinControl, AMessage);
end;

class function TWSWinControl.CanFocus(const AWincontrol: TWinControl): Boolean;
begin
  // lets consider that by deafult all WinControls can be focused
  Result := True;
end;

class function TWSWinControl.GetClientBounds(const AWincontrol: TWinControl; var ARect: TRect): Boolean;
begin
  // for now default to the WinAPI version
  Result := WidgetSet.GetClientBounds(AWincontrol.Handle, ARect);
end;

class function TWSWinControl.GetClientRect(const AWincontrol: TWinControl; var ARect: TRect): Boolean;
begin
  // for now default to the WinAPI version
  Result := WidgetSet.GetClientRect(AWincontrol.Handle, ARect);
end;

{------------------------------------------------------------------------------
  Function: TWSWinControl.GetText
  Params:  Sender: The control to retrieve the text from
  Returns: the requested text

  Retrieves the text from a control. 
 ------------------------------------------------------------------------------}
class function TWSWinControl.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
begin
  Result := false;
end;
  
class function TWSWinControl.GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean;
var
  S: String;
begin
  Result := GetText(AWinControl, S);
  if Result
  then ALength := Length(S);
end;

class procedure TWSWinControl.SetBiDiMode(const AWinControl: TWinControl; UseRightToLeftAlign, UseRightToLeftReading, UseRightToLeftScrollBar : Boolean);
begin
end;

class procedure TWSWinControl.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  PreferredWidth := 0;
  PreferredHeight := 0;
end;

class function TWSWinControl.GetDefaultClientRect(
  const AWinControl: TWinControl; const aLeft, aTop, aWidth, aHeight: integer;
  var aClientRect: TRect): boolean;
begin
  Result:=false;
end;

class function TWSWinControl.GetDesignInteractive(
  const AWinControl: TWinControl; AClientPos: TPoint): Boolean;
begin
  Result := False;
end;

class function TWSWinControl.GetDoubleBuffered(
  const AWinControl: TWinControl): Boolean;
begin
  Result := AWinControl.DoubleBuffered;
end;

class procedure TWSWinControl.Invalidate(const AWinControl: TWinControl);
begin
end;

class procedure TWSWinControl.PaintTo(const AWinControl: TWinControl; ADC: HDC;
  X, Y: Integer);
begin

end;

class procedure TWSWinControl.Repaint(const AWinControl: TWinControl);
begin
  AWinControl.Invalidate;
  AWinControl.Update;
end;

class procedure TWSWinControl.SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer);
begin
end;
    
class procedure TWSWinControl.SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle);
begin
end;

class procedure TWSWinControl.SetChildZPosition(
  const AWinControl, AChild: TWinControl; const AOldPos, ANewPos: Integer;
  const AChildren: TFPList);
begin
end;

class procedure TWSWinControl.SetColor(const AWinControl: TWinControl);
begin
end;

class procedure TWSWinControl.SetCursor(const AWinControl: TWinControl; const ACursor: HCursor);
begin
end;

class procedure TWSWinControl.SetShape(const AWinControl: TWinControl;
  const AShape: HBITMAP);
begin
end;

class procedure TWSWinControl.SetFont(const AWinControl: TWinControl; const AFont: TFont);
begin
end;

class procedure TWSWinControl.SetPos(const AWinControl: TWinControl; const ALeft, ATop: Integer);
begin
end;

class procedure TWSWinControl.SetSize(const AWinControl: TWinControl; const AWidth, AHeight: Integer);
begin
end;

{------------------------------------------------------------------------------
  Method: TWSWinControl.SetLabel
  Params:  AWinControl - the calling object
           AText       - String to be set as label/text for a control
  Returns: Nothing

  Sets the label text on a widget
 ------------------------------------------------------------------------------}
class procedure TWSWinControl.SetText(const AWinControl: TWinControl; const AText: String);
begin
end;

class procedure TWSWinControl.ShowHide(const AWinControl: TWinControl);
begin
end;

class procedure TWSWinControl.ScrollBy(const AWinControl: TWinControl; DeltaX, DeltaY: integer);
begin
  AWinControl.Invalidate;
end;

{ TWSDragImageListResolution }

class function TWSDragImageListResolution.BeginDrag(
  const ADragImageList: TDragImageListResolution; Window: HWND; AIndex, X,
  Y: Integer): Boolean;
begin
  Result := False;
end;

class function TWSDragImageListResolution.DragMove(const ADragImageList: TDragImageListResolution;
  X, Y: Integer): Boolean;
begin
  Result := False;
end;

class procedure TWSDragImageListResolution.EndDrag(const ADragImageList: TDragImageListResolution);
begin
end;

class function TWSDragImageListResolution.HideDragImage(const ADragImageList: TDragImageListResolution;
  ALockedWindow: HWND; DoUnLock: Boolean): Boolean;
begin
  Result := False;
end;

class function TWSDragImageListResolution.ShowDragImage(const ADragImageList: TDragImageListResolution;
  ALockedWindow: HWND; X, Y: Integer; DoLock: Boolean): Boolean;
begin
  Result := False;
end;
{$ENDIF}

{ WidgetSetRegistration }

procedure RegisterDragImageListResolution;
const
  Done: Boolean = False;
begin
  if Done then exit;
  if not WSRegisterDragImageListResolution then
    RegisterWSComponent(TDragImageListResolution, TWSDragImageListResolution);
  Done := True;
end;

procedure RegisterLazAccessibleObject;
const
  Done: Boolean = False;
begin
  if Done then exit;
  if not WSRegisterLazAccessibleObject then
    RegisterWSLazAccessibleObject(TWSLazAccessibleObject);
  Done := True;
end;

procedure RegisterControl;
const
  Done: Boolean = False;
begin
  if Done then exit;
  if not WSRegisterControl then
    RegisterWSComponent(TControl, TWSControl);
  Done := True;
end;

procedure RegisterWinControl;
const
  Done: Boolean = False;
begin
  if Done then exit;
  if not WSRegisterWinControl then
    RegisterWSComponent(TWinControl, TWSWinControl);
  Done := True;
end;

procedure RegisterGraphicControl;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterGraphicControl;
//  if not WSRegisterGraphicControl then
//    RegisterWSComponent(TGraphicControl, TWSGraphicControl);
  Done := True;
end;

procedure RegisterCustomControl;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomControl;
//  if not WSRegisterCustomControl then
//    RegisterWSComponent(TCustomControl, TWSCustomControl);
  Done := True;
end;

end.
