{
 *****************************************************************************
 *                              QtWSCalendar.pp                              * 
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
unit QtWSCalendar;

{$mode objfpc}{$H+}

interface

{$I qtdefines.inc}

uses
  // Bindings
  qt5,
  qtwidgets,
  // LCL
  SysUtils, Types, DateUtils, Controls, Calendar, LCLType, LCLIntf, LCLProc,
  // Widgetset
  WSProc, WSCalendar, {$ifndef wsintf}WSLCLClasses{$else}WSLCLClasses_Intf{$endif};

type

  { TQtWSCustomCalendar }

  TQtWSCustomCalendar = class(TWSCustomCalendar)
  impsection
    imptype function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    imptype function GetDateTime(const ACalendar: TCustomCalendar): TDateTime; rootoverride;
    imptype function HitTest(const ACalendar: TCustomCalendar; const APoint: TPoint): TCalendarPart; rootoverride;
    imptype procedure SetDateTime(const ACalendar: TCustomCalendar; const ADateTime: TDateTime); rootoverride;
    imptype procedure SetDisplaySettings(const ACalendar: TCustomCalendar; const ADisplaySettings: TDisplaySettings); rootoverride;
    imptype procedure SetFirstDayOfWeek(const ACalendar: TCustomCalendar; const ADayOfWeek: TCalDayOfWeek); rootoverride;
  end;


implementation

{ TQtWSCustomCalendar }

imptype function TQtWSCustomCalendar.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  QtCalendar: TQtCalendar;
begin
  QtCalendar := TQtCalendar.Create(AWinControl, AParams);

  QtCalendar.AttachEvents;

  Result := TLCLIntfHandle(QtCalendar);
end;

imptype function TQtWSCustomCalendar.GetDateTime(const ACalendar: TCustomCalendar): TDateTime;
var
  QtCalendar: TQtCalendar;
begin
  QtCalendar := TQtCalendar(ACalendar.Handle);
  Result := QtCalendar.DateTime;
end;

imptype function TQtWSCustomCalendar.HitTest(const ACalendar: TCustomCalendar;
  const APoint: TPoint): TCalendarPart;
var
  QtCalendar: TQtCalendar;
begin
  Result := cpNoWhere;
  if not WSCheckHandleAllocated(ACalendar, 'HitTest') then
    Exit;
  QtCalendar := TQtCalendar(ACalendar.Handle);
  Result := TCalendarPart(QtCalendar.HitTest(APoint))
end;

imptype procedure TQtWSCustomCalendar.SetDateTime(const ACalendar: TCustomCalendar;
  const ADateTime: TDateTime);
var
  QtCalendar: TQtCalendar;
begin
  QtCalendar := TQtCalendar(ACalendar.Handle);
  QtCalendar.BeginUpdate;
  QtCalendar.DateTime := ADateTime;
  QtCalendar.EndUpdate;
end;

imptype procedure TQtWSCustomCalendar.SetDisplaySettings(const ACalendar: TCustomCalendar;
 const ADisplaySettings: TDisplaySettings);
var
  QtCalendar: TQtCalendar;
  HHdrFmt: QCalendarWidgetHorizontalHeaderFormat;
  VHdrFmt: QCalendarWidgetVerticalHeaderFormat;
  SelMode: QCalendarWidgetSelectionMode;
begin
  QtCalendar := TQtCalendar(ACalendar.Handle);

  SelMode := QCalendarWidgetSingleSelection;

  if dsShowDayNames in ADisplaySettings then
    HHdrFmt := QCalendarWidgetShortDayNames
  else
    HHdrFmt := QCalendarWidgetNoHorizontalHeader;

  if dsShowWeekNumbers in ADisplaySettings then
    VHdrFmt := QCalendarWidgetISOWeekNumbers
  else
    VHdrFmt := QCalendarWidgetNoVerticalHeader;

  QtCalendar.BeginUpdate;
  QtCalendar.SetDisplaySettings(HHdrFmt, VHdrFmt, SelMode,
   dsShowHeadings in ADisplaySettings, dsShowWeekNumbers in ADisplaySettings);
  QtCalendar.EndUpdate;
end;

imptype procedure TQtWSCustomCalendar.SetFirstDayOfWeek(const ACalendar: TCustomCalendar;
  const ADayOfWeek: TCalDayOfWeek);
var
  QtCalendar: TQtCalendar;
  H: QLocaleH;
  dow: QtDayOfWeek;
begin
  QtCalendar := TQtCalendar(ACalendar.Handle);

  if ADayOfWeek = dowDefault then begin
    H := QLocale_Create();
    try
      dow := QLocale_firstDayOfWeek(H);
    finally
      QLocale_Destroy(H);
    end;
  end else
    dow := QtDayOfWeek(ord(ADayOfWeek) + 1);

  QtCalendar.BeginUpdate;
  QtCalendar.SetFirstDayOfWeek(dow);
  QtCalendar.EndUpdate;
end;

end.
