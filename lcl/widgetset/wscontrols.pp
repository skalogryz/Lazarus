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
    function BeginDrag(const ADragImageList: TDragImageListResolution; Window: HWND; AIndex, X, Y: Integer): Boolean;
    function DragMove(const ADragImageList: TDragImageListResolution; X, Y: Integer): Boolean;
    procedure EndDrag(const ADragImageList: TDragImageListResolution);
    function HideDragImage(const ADragImageList: TDragImageListResolution;
    ALockedWindow: HWND; DoUnLock: Boolean): Boolean;
    function ShowDragImage(const ADragImageList: TDragImageListResolution;
      ALockedWindow: HWND; X, Y: Integer; DoLock: Boolean): Boolean;
  end;{$endif}

  { TWSDragImageListResolution }

  TWSDragImageListResolution = class(TWSCustomImageListResolution {$ifdef WSINTF},TWSDragImageListResolutionClass{$endif})
  impsection
    imptype function BeginDrag(const ADragImageList: TDragImageListResolution; Window: HWND; AIndex, X, Y: Integer): Boolean; virtual;
    imptype function DragMove(const ADragImageList: TDragImageListResolution; X, Y: Integer): Boolean; virtual;
    imptype procedure EndDrag(const ADragImageList: TDragImageListResolution); virtual;
    imptype function HideDragImage(const ADragImageList: TDragImageListResolution;
      ALockedWindow: HWND; DoUnLock: Boolean): Boolean; virtual;
    imptype function ShowDragImage(const ADragImageList: TDragImageListResolution;
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
  {$ifdef WSINTF}
  TWSControlClass = interface(TWSLCLComponentClass)
    procedure AddControl(const AControl: TControl);
    function GetConstraints(const AControl: TControl; const AConstraints: TObject): Boolean;
    function GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor;
    procedure ConstraintWidth(const AControl: TControl; const AConstraints: TObject; var aWidth: integer);
    procedure ConstraintHeight(const AControl: TControl; const AConstraints: TObject; var aHeight: integer);
    function GetCanvasScaleFactor(const AControl: TControl): Double;
  end;
  {$endif}

  TWSControl = class(TWSLCLComponent{$ifdef WSINTF}, TWSControlClass{$endif})
  impsection
    imptype procedure AddControl(const AControl: TControl); virtual;
    imptype function GetConstraints(const AControl: TControl; const AConstraints: TObject): Boolean; virtual;
    imptype function GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor; virtual;
    imptype procedure ConstraintWidth(const AControl: TControl; const AConstraints: TObject; var aWidth: integer); virtual;
    imptype procedure ConstraintHeight(const AControl: TControl; const AConstraints: TObject; var aHeight: integer); virtual;
    imptype function GetCanvasScaleFactor(const AControl: TControl): Double; virtual;
  end;

  {$ifndef WSINTF}TWSControlClass = class of TWSControl;{$endif}

  { TWSWinControl }

  TWSZPosition = (wszpBack, wszpFront);
  
  { TWSWinControl }
  {$ifdef WSINTF}
  TWSWinControlClass = interface(TWSControlClass)
    function  CanFocus(const AWincontrol: TWinControl): Boolean;
    function  GetClientBounds(const AWincontrol: TWinControl; var ARect: TRect): Boolean;
    function  GetClientRect(const AWincontrol: TWinControl; var ARect: TRect): Boolean;
    procedure GetPreferredSize(const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
    function  GetDefaultClientRect(const AWinControl: TWinControl; const aLeft, aTop, aWidth, aHeight: integer; var aClientRect: TRect): boolean;
    function GetDesignInteractive(const AWinControl: TWinControl; AClientPos: TPoint): Boolean;
    function GetDoubleBuffered(const AWinControl: TWinControl): Boolean;
    function  GetText(const AWinControl: TWinControl; var AText: String): Boolean;
    function  GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean;

    procedure SetBiDiMode(const AWinControl: TWinControl; UseRightToLeftAlign, UseRightToLeftReading, UseRightToLeftScrollBar : Boolean);
    procedure SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle);
    procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer);
    procedure SetColor(const AWinControl: TWinControl);
    procedure SetChildZPosition(const AWinControl, AChild: TWinControl; const AOldPos, ANewPos: Integer; const AChildren: TFPList);
    procedure SetFont(const AWinControl: TWinControl; const AFont: TFont);
    procedure SetPos(const AWinControl: TWinControl; const ALeft, ATop: Integer);
    procedure SetSize(const AWinControl: TWinControl; const AWidth, AHeight: Integer);
    procedure SetText(const AWinControl: TWinControl; const AText: String);
    procedure SetCursor(const AWinControl: TWinControl; const ACursor: HCursor);
    procedure SetShape(const AWinControl: TWinControl; const AShape: HBITMAP);

    { TODO: move AdaptBounds: it is only used in winapi interfaces }
    procedure AdaptBounds(const AWinControl: TWinControl;
          var Left, Top, Width, Height: integer; var SuppressMove: boolean);

    procedure ConstraintsChange(const AWinControl: TWinControl);
    function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle;
    procedure DestroyHandle(const AWinControl: TWinControl);
    procedure DefaultWndHandler(const AWinControl: TWinControl; var AMessage);
    procedure Invalidate(const AWinControl: TWinControl);
    procedure PaintTo(const AWinControl: TWinControl; ADC: HDC; X, Y: Integer);
    procedure Repaint(const AWinControl: TWinControl);
    procedure ShowHide(const AWinControl: TWinControl);
    procedure ScrollBy(const AWinControl: TWinControl; DeltaX, DeltaY: integer);
  end;
  {$endif}

  TWSWinControl = class(TWSControl{$ifdef WSINTF},TWSWinControlClass{$endif})
  impsection
    imptype function  CanFocus(const AWincontrol: TWinControl): Boolean; virtual;
    imptype function  GetClientBounds(const AWincontrol: TWinControl; var ARect: TRect): Boolean; virtual;
    imptype function  GetClientRect(const AWincontrol: TWinControl; var ARect: TRect): Boolean; virtual;
    imptype procedure GetPreferredSize(const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); virtual;
    imptype function  GetDefaultClientRect(const AWinControl: TWinControl; const aLeft, aTop, aWidth, aHeight: integer; var aClientRect: TRect): boolean; virtual;
    imptype function GetDesignInteractive(const AWinControl: TWinControl; AClientPos: TPoint): Boolean; virtual;
    imptype function GetDoubleBuffered(const AWinControl: TWinControl): Boolean; virtual;
    imptype function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; virtual;
    imptype function  GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean; virtual;

    imptype procedure SetBiDiMode(const AWinControl: TWinControl; UseRightToLeftAlign, UseRightToLeftReading, UseRightToLeftScrollBar : Boolean); virtual;
    imptype procedure SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle); virtual;
    imptype procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); virtual;
    imptype procedure SetColor(const AWinControl: TWinControl); virtual;
    imptype procedure SetChildZPosition(const AWinControl, AChild: TWinControl; const AOldPos, ANewPos: Integer; const AChildren: TFPList); virtual;
    imptype procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); virtual;
    imptype procedure SetPos(const AWinControl: TWinControl; const ALeft, ATop: Integer); virtual;
    imptype procedure SetSize(const AWinControl: TWinControl; const AWidth, AHeight: Integer); virtual;
    imptype procedure SetText(const AWinControl: TWinControl; const AText: String); virtual;
    imptype procedure SetCursor(const AWinControl: TWinControl; const ACursor: HCursor); virtual;
    imptype procedure SetShape(const AWinControl: TWinControl; const AShape: HBITMAP); virtual;

    { TODO: move AdaptBounds: it is only used in winapi interfaces }
    imptype procedure AdaptBounds(const AWinControl: TWinControl;
          var Left, Top, Width, Height: integer; var SuppressMove: boolean); virtual;
          
    imptype procedure ConstraintsChange(const AWinControl: TWinControl); virtual;
    imptype function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; virtual;
    imptype procedure DestroyHandle(const AWinControl: TWinControl); virtual;
    imptype procedure DefaultWndHandler(const AWinControl: TWinControl; var AMessage); virtual;
    imptype procedure Invalidate(const AWinControl: TWinControl); virtual;
    imptype procedure PaintTo(const AWinControl: TWinControl; ADC: HDC; X, Y: Integer); virtual;
    imptype procedure Repaint(const AWinControl: TWinControl); virtual;
    imptype procedure ShowHide(const AWinControl: TWinControl); virtual; //TODO: rename to SetVisible(control, visible)
    imptype procedure ScrollBy(const AWinControl: TWinControl; DeltaX, DeltaY: integer); virtual;
  end;
  {$ifndef WSINTF}TWSWinControlClass = class of TWSWinControl;{$endif}

  { TWSGraphicControl }

  TWSGraphicControl = class (TWSControl)
  published
  end;

  { TWSCustomControl }

  TWSCustomControl = class(TWSWinControl)
  published
  end;

  { TWSImageList }

  TWSImageList = class(TWSDragImageListResolution)
  published
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

imptype procedure TWSControl.AddControl(const AControl: TControl);
begin
end;

imptype function TWSControl.GetConstraints(const AControl: TControl; const AConstraints: TObject): Boolean;
begin
  Result := WidgetSet.GetControlConstraints(AConstraints);
end;

imptype function TWSControl.GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor;
begin
  Result := clDefault;
end;

imptype procedure TWSControl.ConstraintWidth(const AControl: TControl;
  const AConstraints: TObject; var aWidth: integer);
begin

end;

imptype procedure TWSControl.ConstraintHeight(const AControl: TControl;
  const AConstraints: TObject; var aHeight: integer);
begin

end;

imptype function TWSControl.GetCanvasScaleFactor(const AControl: TControl): Double;
begin
  Result := 1;
end;

{ TWSWinControl }

imptype procedure TWSWinControl.AdaptBounds(const AWinControl: TWinControl;
  var Left, Top, Width, Height: integer; var SuppressMove: boolean);
begin
end;

imptype procedure TWSWinControl.ConstraintsChange(const AWinControl: TWinControl);
begin
end;

imptype function TWSWinControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  // For now default to the old creation routines
  Result := 0;
end;

imptype procedure TWSWinControl.DestroyHandle(const AWinControl: TWinControl);
begin
end;

imptype procedure TWSWinControl.DefaultWndHandler(const AWinControl: TWinControl; var AMessage);
begin
  WidgetSet.CallDefaultWndHandler(AWinControl, AMessage);
end;

imptype function TWSWinControl.CanFocus(const AWincontrol: TWinControl): Boolean;
begin
  // lets consider that by deafult all WinControls can be focused
  Result := True;
end;

imptype function TWSWinControl.GetClientBounds(const AWincontrol: TWinControl; var ARect: TRect): Boolean;
begin
  // for now default to the WinAPI version
  Result := WidgetSet.GetClientBounds(AWincontrol.Handle, ARect);
end;

imptype function TWSWinControl.GetClientRect(const AWincontrol: TWinControl; var ARect: TRect): Boolean;
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
imptype function TWSWinControl.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
begin
  Result := false;
end;
  
imptype function TWSWinControl.GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean;
var
  S: String;
begin
  Result := GetText(AWinControl, S);
  if Result
  then ALength := Length(S);
end;

imptype procedure TWSWinControl.SetBiDiMode(const AWinControl: TWinControl; UseRightToLeftAlign, UseRightToLeftReading, UseRightToLeftScrollBar : Boolean);
begin
end;

imptype procedure TWSWinControl.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  PreferredWidth := 0;
  PreferredHeight := 0;
end;

imptype function TWSWinControl.GetDefaultClientRect(
  const AWinControl: TWinControl; const aLeft, aTop, aWidth, aHeight: integer;
  var aClientRect: TRect): boolean;
begin
  Result:=false;
end;

imptype function TWSWinControl.GetDesignInteractive(
  const AWinControl: TWinControl; AClientPos: TPoint): Boolean;
begin
  Result := False;
end;

imptype function TWSWinControl.GetDoubleBuffered(
  const AWinControl: TWinControl): Boolean;
begin
  Result := AWinControl.DoubleBuffered;
end;

imptype procedure TWSWinControl.Invalidate(const AWinControl: TWinControl);
begin
end;

imptype procedure TWSWinControl.PaintTo(const AWinControl: TWinControl; ADC: HDC;
  X, Y: Integer);
begin

end;

imptype procedure TWSWinControl.Repaint(const AWinControl: TWinControl);
begin
  AWinControl.Invalidate;
  AWinControl.Update;
end;

imptype procedure TWSWinControl.SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer);
begin
end;
    
imptype procedure TWSWinControl.SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle);
begin
end;

imptype procedure TWSWinControl.SetChildZPosition(
  const AWinControl, AChild: TWinControl; const AOldPos, ANewPos: Integer;
  const AChildren: TFPList);
begin
end;

imptype procedure TWSWinControl.SetColor(const AWinControl: TWinControl);
begin
end;

imptype procedure TWSWinControl.SetCursor(const AWinControl: TWinControl; const ACursor: HCursor);
begin
end;

imptype procedure TWSWinControl.SetShape(const AWinControl: TWinControl;
  const AShape: HBITMAP);
begin
end;

imptype procedure TWSWinControl.SetFont(const AWinControl: TWinControl; const AFont: TFont);
begin
end;

imptype procedure TWSWinControl.SetPos(const AWinControl: TWinControl; const ALeft, ATop: Integer);
begin
end;

imptype procedure TWSWinControl.SetSize(const AWinControl: TWinControl; const AWidth, AHeight: Integer);
begin
end;

{------------------------------------------------------------------------------
  Method: TWSWinControl.SetLabel
  Params:  AWinControl - the calling object
           AText       - String to be set as label/text for a control
  Returns: Nothing

  Sets the label text on a widget
 ------------------------------------------------------------------------------}
imptype procedure TWSWinControl.SetText(const AWinControl: TWinControl; const AText: String);
begin
end;

imptype procedure TWSWinControl.ShowHide(const AWinControl: TWinControl);
begin
end;

imptype procedure TWSWinControl.ScrollBy(const AWinControl: TWinControl; DeltaX, DeltaY: integer);
begin
  AWinControl.Invalidate;
end;

{ TWSDragImageListResolution }

imptype function TWSDragImageListResolution.BeginDrag(
  const ADragImageList: TDragImageListResolution; Window: HWND; AIndex, X,
  Y: Integer): Boolean;
begin
  Result := False;
end;

imptype function TWSDragImageListResolution.DragMove(const ADragImageList: TDragImageListResolution;
  X, Y: Integer): Boolean;
begin
  Result := False;
end;

imptype procedure TWSDragImageListResolution.EndDrag(const ADragImageList: TDragImageListResolution);
begin
end;

imptype function TWSDragImageListResolution.HideDragImage(const ADragImageList: TDragImageListResolution;
  ALockedWindow: HWND; DoUnLock: Boolean): Boolean;
begin
  Result := False;
end;

imptype function TWSDragImageListResolution.ShowDragImage(const ADragImageList: TDragImageListResolution;
  ALockedWindow: HWND; X, Y: Integer; DoLock: Boolean): Boolean;
begin
  Result := False;
end;

{ WidgetSetRegistration }

procedure RegisterDragImageListResolution;
const
  Done: Boolean = False;
begin
  if Done then exit;
  if not WSRegisterDragImageListResolution then
    RegisterWSComponent(TDragImageListResolution, TWSDragImageListResolution.Create);
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
    RegisterWSComponent(TControl, TWSControl.Create);
  Done := True;
end;

procedure RegisterWinControl;
const
  Done: Boolean = False;
begin
  if Done then exit;
  if not WSRegisterWinControl then
    RegisterWSComponent(TWinControl, TWSWinControl.Create);
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
