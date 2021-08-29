unit CocoaWSDatePicker;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}
{$include cocoadefines.inc}

interface

uses
  CocoaAll,
  Classes, SysUtils, Controls, Calendar,
  WSCalendar, CocoaWSCommon, CocoaDatePicker,
  LCLtype, LclProc, LMessages, LCLMessageGlue,
  CocoaUtils, CocoaPrivate;

const
  singleDateMode                          = 0;
  rangeDateMode                           = 1;
  NSDatePickerStyle_Stepper               = 0;
  NSDatePickerStyle_ClockCal              = 1;
  NSDatePickerStyle_Edit                  = 2;
  NSHourMinuteDatePickerElementFlag       = $000c;
  NSHourMinuteSecondDatePickerElementFlag = $000e;
  NSTimeZoneDatePickerElementFlag         = $0010;
  NSYearMonthDatePickerElementFlag        = $00c0;
  NSYearMonthDayDatePickerElementFlag     = $00e0;
  NSEraDatePickerElementFlag              = $0100;

type
  TCocoaWSCustomCalendar = class({$ifndef wsintf}TWSCustomCalendar{$else}TCocoaWSWinControl, IWSCustomCalendar{$endif})
  impsection
    imptype function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    imptype function GetDateTime(const ACalendar: TCustomCalendar): TDateTime; rootoverride;
    imptype procedure SetDateTime(const ACalendar: TCustomCalendar; const ADateTime: TDateTime); rootoverride;
    imptype function HitTest(const ACalendar: TCustomCalendar; const APoint: TPoint): TCalendarPart; rootoverride;
    {$ifdef wsintf}
    imptype function GetCurrentView(const ACalendar: TCustomCalendar): TCalendarView; rootoverride;
    imptype procedure SetDisplaySettings(const ACalendar: TCustomCalendar;
      const ADisplaySettings: TDisplaySettings); rootoverride;
    imptype procedure SetFirstDayOfWeek(const ACalendar: TCustomCalendar;
      const ADayOfWeek: TCalDayOfWeek); rootoverride;
    {$endif}
  end;

implementation

function AnsiStrToNSStr(value : AnsiString): NSString;
begin
  Result:= NSStringUtf8(String(value));
end;

function NSStrToAnsiStr(value: NSString): AnsiString;
begin
  Result:= AnsiString(NSStringToString(value));
end;

function AllocDatePicker(const ATarget: TWinControl; const AParams: TCreateParams): TCocoaDatePicker;
var
  ns : NSString;
  nsc: NSString;
  c  : NSCalendar;
  flags : NSDatePickerElementFlags;
  mode  : NSDatePickerMode;
begin
  Result:= TCocoaDatePicker.alloc.lclInitWithCreateParams(AParams);

  if Assigned(Result) then
  begin
    flags:= NSYearMonthDayDatePickerElementFlag;
    Result.setDatePickerElements(flags);

    Result.setTimeZone(NSTimeZone.localTimeZone);

    Result.setDateValue(DateTimeToNSDate(Now));

    Result.setDatePickerStyle(NSDatePickerStyle_Stepper);

    mode:= singleDateMode;
    Result.setDatePickerMode(mode);

    c := NSCalendar.alloc.initWithCalendarIdentifier(NSString.string_);
    Result.setCalendar(c);

    TCocoaDatePicker(Result).callback:= TLCLCommonCallback.Create(Result, ATarget);

    Result.setBezeled(True);

    //Result.setBordered(True);
  end;
end;

imptype function TCocoaWSCustomCalendar.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  dp: TCocoaDatePicker;
  Params: TCreateParams;
begin
  dp:= AllocDatePicker(AWinControl, AParams);
  dp.autoResize := true;
  dp.retainAspectRatio := true;

  if Assigned(dp) then
  begin
    NSDatePickerCell(TLCLIntfHandle(dp)).setDatePickerStyle(NSDatePickerStyle_ClockCal);
  end;

  Result:= TLCLIntfHandle(dp);
end;

imptype function  TCocoaWSCustomCalendar.GetDateTime(const ACalendar: TCustomCalendar): TDateTime;
begin
  Result:= NSDateToDateTime(NSDatePickerCell(ACalendar.Handle).dateValue);
end;

imptype procedure TCocoaWSCustomCalendar.SetDateTime(const ACalendar: TCustomCalendar; const ADateTime: TDateTime);
begin
  NSDatePickerCell(ACalendar.Handle).setDateValue(DateTimeToNSDate(ADateTime));
end;

imptype function TCocoaWSCustomCalendar.HitTest(const ACalendar: TCustomCalendar; const APoint: TPoint): TCalendarPart;
begin
  // need to validate this decision...
  //Debugln('TCocoaWSCustomCalendar.HitTest Mouse Y : ' + IntToStr(APoint.y));
  if APoint.y >= 40 then
    Result:= cpDate
  else
    Result:= cpTitle;
end;
{$ifdef wsintf}
imptype function TCocoaWSCustomCalendar.GetCurrentView(const ACalendar: TCustomCalendar): TCalendarView;
begin
  Result := cvMonth;
end;

imptype procedure TCocoaWSCustomCalendar.SetDisplaySettings(const ACalendar: TCustomCalendar;
  const ADisplaySettings: TDisplaySettings);
begin
end;

imptype procedure TCocoaWSCustomCalendar.SetFirstDayOfWeek(const ACalendar: TCustomCalendar;
  const ADayOfWeek: TCalDayOfWeek);
begin
end;
{$endif}
end.

