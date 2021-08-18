{ $Id$}
{
 *****************************************************************************
 *                                WSForms.pp                                 * 
 *                                ----------                                 * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit WSForms;

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
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Graphics, Controls, Forms, LCLType,
////////////////////////////////////////////////////
  {$ifdef WSINTF}WSLCLClasses_Intf{$else}WSLCLClasses{$endif}, WSControls, WSFactory;

type
  { TWSScrollingWinControl }

  TWSScrollingWinControlClass = {$ifdef wsintf}
  interface(TWSWinControlClass)
  end;
  {$else}
  class of TWSScrollingWinControl;
  {$endif}
  TWSScrollingWinControl = class(TWSWinControl{$ifdef wnintf},TWSScrollingWinControlClass{$endif})
  impsection
    // procedure ScrollBy is moved to TWSWinControl.
    imptype function GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor; override;
  end;

  { TWSScrollBox }

  TWSScrollBox = class(TWSScrollingWinControl)
  published
  end;

  { TWSCustomFrame }

  TWSCustomFrame = class(TWSScrollingWinControl)
  published
  end;

  { TWSFrame }

  TWSFrame = class(TWSCustomFrame)
  published
  end;

  { TWSCustomForm }

  {$ifdef wsintf}
  TWSCustomFormClass = interface(TWSScrollingWinControlClass)
    procedure CloseModal(const ACustomForm: TCustomForm);
    procedure SetAllowDropFiles(const AForm: TCustomForm; AValue: Boolean);
    procedure SetAlphaBlend(const ACustomForm: TCustomForm; const AlphaBlend: Boolean;
      const Alpha: Byte);
    procedure SetBorderIcons(const AForm: TCustomForm;
        const ABorderIcons: TBorderIcons);
    procedure SetFormBorderStyle(const AForm: TCustomForm;
                             const AFormBorderStyle: TFormBorderStyle);
    procedure SetFormStyle(const AForm: TCustomform; const AFormStyle, AOldFormStyle: TFormStyle);
    procedure SetIcon(const AForm: TCustomForm; const Small, Big: HICON);
    procedure ShowModal(const ACustomForm: TCustomForm);
    procedure SetModalResult(const ACustomForm: TCustomForm; ANewValue: TModalResult);
    procedure SetRealPopupParent(const ACustomForm: TCustomForm;
      const APopupParent: TCustomForm);
    procedure SetShowInTaskbar(const AForm: TCustomForm; const AValue: TShowInTaskbar);
    procedure SetZPosition(const AWinControl: TWinControl; const APosition: TWSZPosition);
    function GetDefaultDoubleBuffered: Boolean;

    {mdi support}
    function ActiveMDIChild(const AForm: TCustomForm): TCustomForm;
    function Cascade(const AForm: TCustomForm): Boolean;
    function GetClientHandle(const AForm: TCustomForm): HWND;
    function GetMDIChildren(const AForm: TCustomForm; AIndex: Integer): TCustomForm;
    function Next(const AForm: TCustomForm): Boolean;
    function Previous(const AForm: TCustomForm): Boolean;
    function Tile(const AForm: TCustomForm): Boolean;
    function ArrangeIcons(const AForm: TCustomForm): Boolean;
    function MDIChildCount(const AForm: TCustomForm): Integer;
  end;
  {$endif}

  TWSCustomForm = class(TWSScrollingWinControl{$ifdef wsintf},TWSCustomFormClass{$endif})
  impsection
    imptype procedure CloseModal(const ACustomForm: TCustomForm); virtual;
    imptype procedure SetAllowDropFiles(const AForm: TCustomForm; AValue: Boolean); virtual;
    imptype procedure SetAlphaBlend(const ACustomForm: TCustomForm; const AlphaBlend: Boolean;
      const Alpha: Byte); virtual;
    imptype procedure SetBorderIcons(const AForm: TCustomForm;
        const ABorderIcons: TBorderIcons); virtual;
    imptype procedure SetFormBorderStyle(const AForm: TCustomForm;
                             const AFormBorderStyle: TFormBorderStyle); virtual;
    imptype procedure SetFormStyle(const AForm: TCustomform; const AFormStyle, AOldFormStyle: TFormStyle); virtual;
    imptype procedure SetIcon(const AForm: TCustomForm; const Small, Big: HICON); virtual;
    imptype procedure ShowModal(const ACustomForm: TCustomForm); virtual;
    imptype procedure SetModalResult(const ACustomForm: TCustomForm; ANewValue: TModalResult); virtual;
    imptype procedure SetRealPopupParent(const ACustomForm: TCustomForm;
      const APopupParent: TCustomForm); virtual;
    imptype procedure SetShowInTaskbar(const AForm: TCustomForm; const AValue: TShowInTaskbar); virtual;
    imptype procedure SetZPosition(const AWinControl: TWinControl; const APosition: TWSZPosition); virtual;
    imptype function GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor; override;
    imptype function GetDefaultDoubleBuffered: Boolean; virtual;

    {mdi support}
    imptype function ActiveMDIChild(const AForm: TCustomForm): TCustomForm; virtual;
    imptype function Cascade(const AForm: TCustomForm): Boolean; virtual;
    imptype function GetClientHandle(const AForm: TCustomForm): HWND; virtual;
    imptype function GetMDIChildren(const AForm: TCustomForm; AIndex: Integer): TCustomForm; virtual;
    imptype function Next(const AForm: TCustomForm): Boolean; virtual;
    imptype function Previous(const AForm: TCustomForm): Boolean; virtual;
    imptype function Tile(const AForm: TCustomForm): Boolean; virtual;
    imptype function ArrangeIcons(const AForm: TCustomForm): Boolean; virtual;
    imptype function MDIChildCount(const AForm: TCustomForm): Integer; virtual;
  end;
  {$ifndef wsintf}
  TWSCustomFormClass = class of TWSCustomForm;
  {$endif}

  { TWSForm }

  TWSForm = class(TWSCustomForm)
  published
  end;

  { TWSHintWindow }

  TWSHintWindow = class(TWSCustomForm)
  published
  end;

  { TWSScreen }

  TWSScreen = class(TWSLCLComponent)
  published
  end;

  { TWSApplicationProperties }

  TWSApplicationProperties = class(TWSLCLComponent)
  published
  end;

  { WidgetSetRegistration }

  procedure RegisterScrollingWinControl;
  procedure RegisterScrollBox;
  procedure RegisterCustomFrame;
  procedure RegisterCustomForm;
  procedure RegisterHintWindow;

implementation

{ TWSScrollingWinControl }

imptype function TWSScrollingWinControl.GetDefaultColor(const AControl: TControl;
  const ADefaultColorType: TDefaultColorType): TColor;
const
  DefColors: array[TDefaultColorType] of TColor = (
 { dctBrush } clForm,
 { dctFont  } clBtnText
  );
begin
  Result := DefColors[ADefaultColorType];
end;

{ TWSCustomForm }

imptype procedure TWSCustomForm.CloseModal(const ACustomForm: TCustomForm);
begin
end;

imptype procedure TWSCustomForm.SetAllowDropFiles(const AForm: TCustomForm;
  AValue: Boolean);
begin
end;

imptype procedure TWSCustomForm.SetBorderIcons(const AForm: TCustomForm;
        const ABorderIcons: TBorderIcons);
begin
end;

imptype procedure TWSCustomForm.SetFormBorderStyle(const AForm: TCustomForm;
  const AFormBorderStyle: TFormBorderStyle);
begin
  // will be done in interface override
end;

imptype procedure TWSCustomForm.SetFormStyle(const AForm: TCustomform;
  const AFormStyle, AOldFormStyle: TFormStyle);
begin
end;
    
imptype procedure TWSCustomForm.SetIcon(const AForm: TCustomForm; const Small, Big: HICON);
begin
end;

imptype procedure TWSCustomForm.SetShowInTaskbar(const AForm: TCustomForm;
  const AValue: TShowInTaskbar);
begin
end;

imptype procedure TWSCustomForm.SetZPosition(const AWinControl: TWinControl; const APosition: TWSZPosition);
begin
end;

imptype function TWSCustomForm.GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor;
const
  DefColors: array[TDefaultColorType] of TColor = (
 { dctBrush } clForm,
 { dctFont  } clBtnText
  );
begin
  Result := DefColors[ADefaultColorType];
end;

imptype function TWSCustomForm.GetDefaultDoubleBuffered: Boolean;
begin
  Result := False;
end;

imptype procedure TWSCustomForm.ShowModal(const ACustomForm: TCustomForm);
begin
end;

// This needs implementing only if the TWSCustomForm.ShowModal implementation
// is fully blocking (which it shouldn't be ideally)
imptype procedure TWSCustomForm.SetModalResult(const ACustomForm: TCustomForm;
  ANewValue: TModalResult);
begin
end;

imptype procedure TWSCustomForm.SetRealPopupParent(
  const ACustomForm: TCustomForm; const APopupParent: TCustomForm);
begin
end;

imptype procedure TWSCustomForm.SetAlphaBlend(const ACustomForm: TCustomForm;
  const AlphaBlend: Boolean; const Alpha: Byte);
begin
end;

{ mdi support }

imptype function TWSCustomForm.ActiveMDIChild(const AForm: TCustomForm
  ): TCustomForm;
begin
  Result := nil;
end;

imptype function TWSCustomForm.Cascade(const AForm: TCustomForm): Boolean;
begin
  Result := False;
end;

imptype function TWSCustomForm.GetClientHandle(const AForm: TCustomForm): HWND;
begin
  Result := 0;
end;

imptype function TWSCustomForm.GetMDIChildren(const AForm: TCustomForm;
  AIndex: Integer): TCustomForm;
begin
  Result := nil;
end;

imptype function TWSCustomForm.MDIChildCount(const AForm: TCustomForm): Integer;
begin
  Result := 0;
end;

imptype function TWSCustomForm.Next(const AForm: TCustomForm): Boolean;
begin
  Result := False;
end;

imptype function TWSCustomForm.Previous(const AForm: TCustomForm): Boolean;
begin
  Result := False;
end;

imptype function TWSCustomForm.ArrangeIcons(const AForm: TCustomForm): Boolean;
begin
  Result := False;
end;

imptype function TWSCustomForm.Tile(const AForm: TCustomForm): Boolean;
begin
  Result := False;
end;


{ WidgetSetRegistration }

procedure RegisterScrollingWinControl;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterScrollingWinControl;
//  if not WSRegisterScrollingWinControl then
//    RegisterWSComponent(TScrollingWinControl, TWSScrollingWinControl);
  Done := True;
end;

procedure RegisterScrollBox;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterScrollBox;
//  if not WSRegisterScrollBox then
//    RegisterWSComponent(TScrollBox, TWSScrollBox);
  Done := True;
end;

procedure RegisterCustomFrame;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomFrame;
//  if not WSRegisterCustomFrame then
//    RegisterWSComponent(TCustomFrame, TWSCustomFrame);
  Done := True;
end;

procedure RegisterCustomForm;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomForm;
//  if not WSRegisterCustomForm then
//    RegisterWSComponent(TCustomForm, TWSCustomForm);
  Done := True;
end;

procedure RegisterHintWindow;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterHintWindow;
//  if not WSRegisterHintWindow then
//    RegisterWSComponent(THintWindow, TWSHintWindow);
  Done := True;
end;

end.
