{
 *****************************************************************************
 *                              QtWSExtCtrls.pp                              * 
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
unit QtWSExtCtrls;

{$mode objfpc}{$H+}

interface

{$I qtdefines.inc}

uses
  // Bindings
  qt5,
  qtwidgets, qtobjects, qtproc, QtWSControls,
  // LCL
  SysUtils, Classes, Controls, Graphics, Forms, ExtCtrls, LCLType, LazUTF8,
  // Widgetset
  WSExtCtrls, {$ifndef wsintf}WSLCLClasses{$else}QtWSStdCtrls, WSLCLClasses_Intf{$endif};

type
  { TQtWSPage }

  TQtWSPage = class({$ifndef wsintf}TWSPage{$else}TQtWSCustomControl{$endif})
  published
  end;

  { TQtWSNotebook }

  TQtWSNotebook = class({$ifndef wsintf}TWSNotebook{$else}TQtWSCustomControl{$endif})
  published
  end;

  { TQtWSShape }

  TQtWSShape = class({$ifndef wsintf}TWSShape{$else}TQtWSGraphicControl{$endif})
  published
  end;

  { TQtWSCustomSplitter }

  TQtWSCustomSplitter = class({$ifndef wsintf}TWSCustomSplitter{$else}TQtWSCustomControl{$endif})
  published
  end;

  { TQtWSSplitter }

  TQtWSSplitter = class({$ifndef wsintf}TWSSplitter{$else}TQtWSCustomSplitter{$endif})
  published
  end;

  { TQtWSPaintBox }

  TQtWSPaintBox = class({$ifndef wsintf}TWSPaintBox{$else}TQtWSGraphicControl{$endif})
  published
  end;

  { TQtWSCustomImage }

  TQtWSCustomImage = class({$ifndef wsintf}TWSCustomImage{$else}TQtWSGraphicControl{$endif})
  published
  end;

  { TQtWSImage }

  TQtWSImage = class({$ifndef wsintf}TWSImage{$else}TQtWSCustomImage{$endif})
  published
  end;

  { TQtWSBevel }

  TQtWSBevel = class({$ifndef wsintf}TWSBevel{$else}TQtWSGraphicControl{$endif})
  published
  end;

  { TQtWSCustomRadioGroup }

  TQtWSCustomRadioGroup = class({$ifndef wsintf}TWSCustomRadioGroup{$else}TQtWSCustomGroupbox{$endif})
  impsection
    imptype function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TQtWSRadioGroup }

  TQtWSRadioGroup = class({$ifndef wsintf}TWSRadioGroup{$else}TQtWSCustomRadioGroup{$endif})
  published
  end;

  { TQtWSCustomCheckGroup }

  TQtWSCustomCheckGroup = class({$ifndef wsintf}TWSCustomCheckGroup{$else}TQtWSCustomGroupbox{$endif})
  impsection
    imptype function  CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TQtWSCheckGroup }

  TQtWSCheckGroup = class({$ifndef wsintf}TTWSCheckGroup{$else}TQtWSCustomCheckGroup{$endif})
  published
  end;

  { TQtWSCustomLabeledEdit }

  TQtWSCustomLabeledEdit = class({$ifndef wsintf}TWSCustomLabeledEdit{$else}TQtWSCustomEdit{$endif})
  published
  end;

  { TQtWSLabeledEdit }

  TQtWSLabeledEdit = class({$ifndef wsintf}TWSLabeledEdit{$else}TQtWSCustomLabeledEdit{$endif})
  published
  end;

  { TQtWSCustomPanel }

  TQtWSCustomPanel = class({$ifndef wsintf}TWSCustomPanel{$else}TQtWSWinControl{$endif})
  impsection
    imptype function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): TLCLIntfHandle; override;
    imptype function GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor; override;
  end;

  { TQtWSPanel }

  TQtWSPanel = class(TWSPanel)
  published
  end;

  { TQtWSCustomTrayIcon }

  TQtWSCustomTrayIcon = class(TWSCustomTrayIcon)
  impsection
    imptype function Hide(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    imptype function Show(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    imptype procedure InternalUpdate(const ATrayIcon: TCustomTrayIcon); override;
    imptype function ShowBalloonHint(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    imptype function GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint; override;
    imptype function GetCanvas(const ATrayIcon: TCustomTrayIcon): TCanvas; override;
  end;

implementation
uses qtsystemtrayicon;

{ TQtWSCustomPanel }

{------------------------------------------------------------------------------
  Method: TQtWSCustomPanel.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}
imptype function TQtWSCustomPanel.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  QtFrame: TQtFrame;
begin
  QtFrame := TQtFrame.Create(AWinControl, AParams);
  QtFrame.AttachEvents;

  // Set's initial properties
  QtFrame.setFrameShape(TBorderStyleToQtFrameShapeMap[TCustomPanel(AWinControl).BorderStyle]);
  
  // Return the Handle
  Result := TLCLIntfHandle(QtFrame);
end;

imptype function TQtWSCustomPanel.GetDefaultColor(const AControl: TControl;
  const ADefaultColorType: TDefaultColorType): TColor;
const
  DefColors: array[TDefaultColorType] of TColor = (
 { dctBrush } clBackground,
 { dctFont  } clBtnText
  );
begin
  Result := DefColors[ADefaultColorType];
end;

{ TQtWSCustomRadioGroup }

{------------------------------------------------------------------------------
  Method: TQtWSCustomRadioGroup.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}

imptype function TQtWSCustomRadioGroup.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  QtGroupBox: TQtGroupBox;
  Str: WideString;
begin
  QtGroupBox := TQtGroupBox.Create(AWinControl, AParams);
  QtGroupBox.GroupBoxType := tgbtRadioGroup;

  Str := GetUtf8String(AWinControl.Caption);
  QGroupBox_setTitle(QGroupBoxH(QtGroupBox.Widget), @Str);

  QtGroupBox.AttachEvents;

  Result := TLCLIntfHandle(QtGroupBox);
end;

{ TQtWSCustomCheckGroup }

{------------------------------------------------------------------------------
  Method: TQtWSCustomCheckGroup.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}
imptype function TQtWSCustomCheckGroup.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  QtGroupBox: TQtGroupBox;
  Str: WideString;
begin
  QtGroupBox := TQtGroupBox.Create(AWinControl, AParams);
  QtGroupBox.GroupBoxType := tgbtCheckGroup;

  Str := GetUtf8String(AWinControl.Caption);
  QGroupBox_setTitle(QGroupBoxH(QtGroupBox.Widget), @Str);

  QtGroupBox.AttachEvents;

  Result := TLCLIntfHandle(QtGroupBox);
end;

{ TQtWSCustomTrayIcon }

imptype function TQtWSCustomTrayIcon.Hide(const ATrayIcon: TCustomTrayIcon): Boolean;
var
  SystemTrayIcon: TQtSystemTrayIcon;
begin
  Result := False;

  SystemTrayIcon := TQtSystemTrayIcon(ATrayIcon.Handle);

  SystemTrayIcon.Hide;

  SystemTrayIcon.Free;

  ATrayIcon.Handle := 0;

  Result := True;
end;

imptype function TQtWSCustomTrayIcon.Show(const ATrayIcon: TCustomTrayIcon): Boolean;
var
  Text: WideString;
  SystemTrayIcon: TQtSystemTrayIcon;
  IconH: QIconH;
begin
  Result := False;

  if ATrayIcon.Icon.Handle = 0 then
    IconH := nil
  else
    IconH := TQtIcon(ATrayIcon.Icon.Handle).Handle;

  SystemTrayIcon := TQtSystemTrayIcon.Create(IconH);
  SystemTrayIcon.FTrayIcon := ATrayIcon;

  ATrayIcon.Handle := HWND(SystemTrayIcon);

  Text := UTF8ToUTF16(ATrayIcon.Hint);
  SystemTrayIcon.setToolTip(Text);

  if Assigned(ATrayIcon.PopUpMenu) then
    if TQtMenu(ATrayIcon.PopUpMenu.Handle).Widget <> nil then
      SystemTrayIcon.setContextMenu(QMenuH(TQtMenu(ATrayIcon.PopUpMenu.Handle).Widget));

  SystemTrayIcon.show;

  Result := True;
end;

{*******************************************************************
*  TQtWSCustomTrayIcon.InternalUpdate ()
*
*  DESCRIPTION:    Makes modifications to the Icon while running
*                  i.e. without hiding it and showing again
*******************************************************************}
imptype procedure TQtWSCustomTrayIcon.InternalUpdate(const ATrayIcon: TCustomTrayIcon);
var
  SystemTrayIcon: TQtSystemTrayIcon;
  AIcon: QIconH;
  AHint: WideString;
begin
  if (ATrayIcon.Handle = 0) then Exit;

  SystemTrayIcon := TQtSystemTrayIcon(ATrayIcon.Handle);
  if Assigned(ATrayIcon.Icon) then
  begin
    // animate
    if ATrayIcon.Animate and Assigned(ATrayIcon.Icons) then
      SystemTrayIcon.setIcon(TQtImage(ATrayIcon.Icon.BitmapHandle).AsIcon)
    else
    // normal
    if (ATrayIcon.Icon.Handle <> 0) then
      SystemTrayIcon.setIcon(TQtIcon(ATrayIcon.Icon.Handle).Handle)
    else
    begin
      AIcon := QIcon_create();
      SystemTrayIcon.setIcon(AIcon);
      QIcon_destroy(AIcon);
    end;
  end else
  begin
    AIcon := QIcon_create;
    SystemTrayIcon.setIcon(AIcon);
    QIcon_destroy(AIcon);
  end;


  { PopUpMenu }
  if Assigned(ATrayIcon.PopUpMenu) then
    if TQtMenu(ATrayIcon.PopUpMenu.Handle).Widget <> nil then
      SystemTrayIcon.setContextMenu(QMenuH(TQtMenu(ATrayIcon.PopUpMenu.Handle).Widget));

  AHint := UTF8ToUTF16(ATrayIcon.Hint);
  SystemTrayIcon.setToolTip(AHint);

  SystemTrayIcon.UpdateSystemTrayWidget;
end;

imptype function TQtWSCustomTrayIcon.ShowBalloonHint(
  const ATrayIcon: TCustomTrayIcon): Boolean;
var
  QtTrayIcon: TQtSystemTrayIcon;
begin
  Result := False;
  if (ATrayIcon.Handle = 0) then Exit;
  QtTrayIcon := TQtSystemTrayIcon(ATrayIcon.Handle);

  QtTrayIcon.showBaloonHint(ATrayIcon.BalloonTitle, ATrayIcon.BalloonHint,
    QSystemTrayIconMessageIcon(Ord(ATrayIcon.BalloonFlags)),
    ATrayIcon.BalloonTimeout);

  Result := True;
end;

imptype function TQtWSCustomTrayIcon.GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint;
begin
  Result := Point(0, 0);
  if (ATrayIcon.Handle = 0) then
    exit;
  Result := TQtSystemTrayIcon(ATrayIcon.Handle).GetPosition;
end;

imptype function TQtWSCustomTrayIcon.GetCanvas(const ATrayIcon: TCustomTrayIcon
  ): TCanvas;
begin
  Result := nil;
  if (ATrayIcon.Handle <> 0) then
    Result := TQtSystemTrayIcon(ATrayIcon.Handle).Canvas;
end;

end.
