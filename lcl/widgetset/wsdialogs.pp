{
 *****************************************************************************
 *                               WSDialogs.pp                                * 
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
unit WSDialogs;

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
  LCLType, Dialogs,
////////////////////////////////////////////////////
  {$ifdef wsintf}WSLCLClasses_Intf{$else}WSLCLClasses{$endif}, WSControls, WSFactory;

type
  { TWSCommonDialog }

  {$ifndef wsintf}
  TWSCommonDialogClass = class of TWSCommonDialog;
  {$else}
  IWSCommonDialog = interface(IWSLCLComponent)
    ['{977D5FA4-8595-4F76-8317-641230768055}']
    function  CreateHandle(const ACommonDialog: TCommonDialog): THandle;
    procedure ShowModal(const ACommonDialog: TCommonDialog);
    procedure DestroyHandle(const ACommonDialog: TCommonDialog);
    function QueryWSEventCapabilities(const ACommonDialog: TCommonDialog): TCDWSEventCapabilities;
  end;
  TWSCommonDialogClass = IWSCommonDialog; // for LCL compatibility
  {$endif}
  TWSCommonDialog = class(TWSLCLComponent{$ifdef wsintf},IWSCommonDialog{$endif})
  public class var
    WSCommonDialog_WSClass: TWSCommonDialogClass;
  impsection
    imptype function  CreateHandle(const ACommonDialog: TCommonDialog): THandle; virtual;
    imptype procedure ShowModal(const ACommonDialog: TCommonDialog); virtual;
    imptype procedure DestroyHandle(const ACommonDialog: TCommonDialog); virtual;
    imptype function QueryWSEventCapabilities(const ACommonDialog: TCommonDialog): TCDWSEventCapabilities; virtual;
  end;

  { TWSFileDialog }

  TWSFileDialog = class(TWSCommonDialog)
  published
  end;

  { TWSOpenDialog }

  TWSOpenDialog = class(TWSFileDialog)
  published
  end;

  { TWSSaveDialog }

  TWSSaveDialog = class(TWSOpenDialog)
  published
  end;

  { TWSSelectDirectoryDialog }

  TWSSelectDirectoryDialog = class(TWSOpenDialog)
  published
  end;

  { TWSColorDialog }

  TWSColorDialog = class(TWSCommonDialog)
  published
  end;

  { TWSColorButton }

  TWSColorButton = class(TWSGraphicControl)
  published
  end;

  { TWSFontDialog }

  TWSFontDialog = class(TWSCommonDialog)
  impsection
    imptype function  CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
    imptype procedure ShowModal(const ACommonDialog: TCommonDialog); override;
    imptype procedure DestroyHandle(const ACommonDialog: TCommonDialog); override;
    imptype function QueryWSEventCapabilities(const ACommonDialog: TCommonDialog): TCDWSEventCapabilities; override;
  end;

  { WidgetSetRegistration }

  procedure RegisterCommonDialog;
  procedure RegisterFileDialog;
  procedure RegisterOpenDialog;
  procedure RegisterSaveDialog;
  procedure RegisterSelectDirectoryDialog;
  procedure RegisterColorDialog;
  procedure RegisterColorButton;
  procedure RegisterFontDialog;

function WSCommonDialogClass(AWidgetSetClass: TWSLCLComponentClass): TWSCommonDialogClass; inline;

implementation

uses
  LResources;

imptype function  TWSCommonDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
begin
  Result := 0;
end;

imptype procedure TWSCommonDialog.DestroyHandle(const ACommonDialog: TCommonDialog);
begin
end;

imptype function TWSCommonDialog.QueryWSEventCapabilities(
  const ACommonDialog: TCommonDialog): TCDWSEventCapabilities;
begin
  Result := [];
end;

imptype procedure TWSCommonDialog.ShowModal(const ACommonDialog: TCommonDialog);
begin
end;

{ TWSFontDialog }

imptype function TWSFontDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
begin
  if WSCommonDialog_WSClass = nil then
    WSCommonDialog_WSClass := WSCommonDialogClass(FindWSComponentClass(TCommonDialog));
  if WSCommonDialog_WSClass <> nil then
  begin
    Result := WSCommonDialog_WSClass.CreateHandle(ACommonDialog);
    Exit;
  end;
  Result:=inherited CreateHandle(ACommonDialog)
end;

imptype procedure TWSFontDialog.ShowModal(const ACommonDialog: TCommonDialog);
begin
  if WSCommonDialog_WSClass = nil then
    WSCommonDialog_WSClass := WSCommonDialogClass(FindWSComponentClass(TCommonDialog));
  if WSCommonDialog_WSClass <> nil then
  begin
    WSCommonDialog_WSClass.ShowModal(ACommonDialog);
    Exit;
  end;
  inherited ShowModal(ACommonDialog);
end;

imptype procedure TWSFontDialog.DestroyHandle(const ACommonDialog: TCommonDialog);
begin
  if WSCommonDialog_WSClass = nil then
    WSCommonDialog_WSClass := WSCommonDialogClass(FindWSComponentClass(TCommonDialog));
  if WSCommonDialog_WSClass <> nil then
  begin
    WSCommonDialog_WSClass.DestroyHandle(ACommonDialog);
    Exit;
  end;
  inherited DestroyHandle(ACommonDialog);
end;

imptype function TWSFontDialog.QueryWSEventCapabilities(
  const ACommonDialog: TCommonDialog): TCDWSEventCapabilities;
begin
  if WSCommonDialog_WSClass = nil then
    WSCommonDialog_WSClass := WSCommonDialogClass(FindWSComponentClass(TCommonDialog));
  if WSCommonDialog_WSClass <> nil then
  begin
    Result := WSCommonDialog_WSClass.QueryWSEventCapabilities(ACommonDialog);
    Exit;
  end;
  Result:=inherited QueryWSEventCapabilities(ACommonDialog);
end;

{ WidgetSetRegistration }

procedure RegisterCommonDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  if not WSRegisterCommonDialog then
    RegisterWSComponent(TCommonDialog, TWSCommonDialog{$ifdef wsintf}.Create{$endif});
  RegisterPropertyToSkip(TCommonDialog, 'Ctl3D', 'VCL compatibility property', '');
  Done := True;
end;

procedure RegisterFileDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterFileDialog;
//  if not WSRegisterFileDialog then
//    RegisterWSComponent(TFileDialog, TWSFileDialog);
  Done := True;
end;

procedure RegisterOpenDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterOpenDialog;
//  if not WSRegisterOpenDialog then
//    RegisterWSComponent(TOpenDialog, TWSOpenDialog);
  Done := True;
end;

procedure RegisterSaveDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterSaveDialog;
//  if not WSRegisterSaveDialog then
//    RegisterWSComponent(TSaveDialog, TWSSaveDialog);
  Done := True;
end;

procedure RegisterSelectDirectoryDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterSelectDirectoryDialog;
//  if not WSRegisterSelectDirectoryDialog then
//    RegisterWSComponent(TSelectDirectoryDialog, TWSSelectDirectoryDialog);
  Done := True;
end;

procedure RegisterColorDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterColorDialog;
//  if not WSRegisterColorDialog then
//    RegisterWSComponent(TColorDialog, TWSColorDialog);
  Done := True;
end;

procedure RegisterColorButton;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterColorButton;
//  if not WSRegisterColorButton then
//    RegisterWSComponent(TColorButton, TWSColorButton);
  Done := True;
end;

procedure RegisterFontDialog;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterFontDialog;
//  if not WSRegisterFontDialog then
//    RegisterWSComponent(TFontDialog, TWSFontDialog);
  Done := True;
end;

function WSCommonDialogClass(AWidgetSetClass: TWSLCLComponentClass): TWSCommonDialogClass; inline;
begin
  {$ifdef wsintf}
  Result := (AWidgetSetClass as IWSCommonDialog);
  {$else}
  Result := TWSCommonDialogClass(AWidgetSetClass);
  {$endif}
end;

end.
