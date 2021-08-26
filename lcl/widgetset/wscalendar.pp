{
 *****************************************************************************
 *                               WSCalendar.pp                               * 
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
unit WSCalendar;

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
  Types, Calendar,
////////////////////////////////////////////////////
  {$ifdef wsintf}WSLCLClasses_Intf{$else}WSLCLClasses{$endif}, WSControls, WSFactory;

type
  { TWSCustomCalendar }
  {$ifdef wsintf}
  IWSCustomCalendar = interface(IWSWinControl)
    ['{3EF2EEA5-E865-4C85-B569-C7A63BE9BACF}']
    function GetDateTime(const ACalendar: TCustomCalendar): TDateTime;
    function HitTest(const ACalendar: TCustomCalendar; const APoint: TPoint): TCalendarPart;
    function GetCurrentView(const ACalendar: TCustomCalendar): TCalendarView;
    procedure SetDateTime(const ACalendar: TCustomCalendar; const ADateTime: TDateTime);
    procedure SetDisplaySettings(const ACalendar: TCustomCalendar;
      const ADisplaySettings: TDisplaySettings);
    procedure SetFirstDayOfWeek(const ACalendar: TCustomCalendar;
      const ADayOfWeek: TCalDayOfWeek);
  end;
  TWSCustomCalendarClass = IWSCustomCalendar;
  {$endif}
  TWSCustomCalendar = class(TWSWinControl{$ifdef wsintf},IWSCustomCalendar{$endif})
  impsection
    imptype function GetDateTime(const ACalendar: TCustomCalendar): TDateTime; virtual;
    imptype function HitTest(const ACalendar: TCustomCalendar; const APoint: TPoint): TCalendarPart; virtual;
    imptype function GetCurrentView(const ACalendar: TCustomCalendar): TCalendarView; virtual;
    imptype procedure SetDateTime(const ACalendar: TCustomCalendar; const ADateTime: TDateTime); virtual;
    imptype procedure SetDisplaySettings(const ACalendar: TCustomCalendar;
      const ADisplaySettings: TDisplaySettings); virtual;
    imptype procedure SetFirstDayOfWeek(const ACalendar: TCustomCalendar;
      const ADayOfWeek: TCalDayOfWeek); virtual;
  end;
  {$ifndef wsintf}TWSCustomCalendarClass = class of TWSCustomCalendar;{$endif}

  { WidgetSetRegistration }

  procedure RegisterCustomCalendar;

function GetWSCustomCalendar(AWidgetSetClass: TWSLCLComponentClass): TWSCustomCalendarClass; inline;

implementation

uses
  LResources;

imptype function  TWSCustomCalendar.GetDateTime(const ACalendar: TCustomCalendar): TDateTime;
begin
  Result := 0.0;
end;

imptype function TWSCustomCalendar.HitTest(const ACalendar: TCustomCalendar; const APoint: TPoint): TCalendarPart;
begin
  Result := cpNoWhere;
end;

imptype function TWSCustomCalendar.GetCurrentView(const ACalendar: TCustomCalendar
  ): TCalendarView;
begin
  Result := cvMonth;
end;

imptype procedure TWSCustomCalendar.SetDateTime(const ACalendar: TCustomCalendar; const ADateTime: TDateTime);
begin
end;

imptype procedure TWSCustomCalendar.SetDisplaySettings(const ACalendar: TCustomCalendar;
  const ADisplaySettings: TDisplaySettings);
begin
end;

imptype procedure TWSCustomCalendar.SetFirstDayOfWeek(const ACalendar: TCustomCalendar;
  const ADayOfWeek: TCalDayOfWeek);
begin
end;

{ WidgetSetRegistration }

procedure RegisterCustomCalendar;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomCalendar;
  RegisterPropertyToSkip(TCalendar, 'ReadOnly', 'Obsoleted property', '');
//  if not WSRegisterCustomCalendar then
//    RegisterWSComponent(TCustomCalendar, TWSCustomCalendar);
  Done := True;
end;

function GetWSCustomCalendar(AWidgetSetClass: TWSLCLComponentClass): TWSCustomCalendarClass; inline;
begin
  {$ifdef wsintf}
  Result := (AWidgetSetClass as IWSCustomCalendar);
  {$else}
  Result := TWSCustomCalendarClass(AWidgetSetClass);
  {$endif}
end;

end.
