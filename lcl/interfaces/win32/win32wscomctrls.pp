{ $Id$}
{
 *****************************************************************************
 *                            Win32WSComCtrls.pp                             * 
 *                            ------------------                             * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit Win32WSComCtrls;

{$mode objfpc}{$H+}
{$I win32defines.inc}

interface

uses        
  // FCL
  CommCtrl, Windows, Classes, SysUtils, Math, Win32Extra,
  // LCL
  ComCtrls, LCLType, Controls, Graphics, Themes,
  ImgList, StdCtrls, Forms, LCLIntf, LCLProc,
  LMessages, LazUTF8, LCLMessageGlue, InterfaceBase,
  // widgetset
  WSComCtrls, {$ifdef wsintf}WSLCLClasses_Intf{$else}WSLCLClasses{$endif}, WSControls, WSProc,
  // win32 widgetset
  Win32Int, Win32Proc, Win32WSControls;

type
  { TWin32WSCustomPage }

  TWin32WSCustomPage = class({$ifndef wsintf}TWSCustomPage{$else}TWin32WSWinControl, IWSCustomPage{$endif})
  public
    class procedure ThemeChange(Wnd: HWND);
  impsection
    imptype function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    imptype procedure DestroyHandle(const AWinControl: TWinControl); override;
    imptype procedure UpdateProperties(const ACustomPage: TCustomPage); rootoverride;
    imptype procedure SetText(const AWinControl: TWinControl; const AText: string); override;
  end;

  { TWin32WSCustomTabControl }

  TWin32WSCustomTabControl = class({$ifndef wsintf}TWSCustomTabControl{$else}TWin32WSWinControl, IWSCustomTabControl{$endif})
  public
    class procedure DeletePage(const ATabControl: TCustomTabControl; const AIndex: integer);
  impsection
    imptype function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    imptype procedure AddAllNBPages(const ATabControl: TCustomTabControl);
    imptype procedure AdjustSizeTabControlPages(const ATabControl: TCustomTabControl);
    imptype procedure AddPage(const ATabControl: TCustomTabControl;
      const AChild: TCustomPage; const AIndex: integer); rootoverride;
    imptype procedure MovePage(const ATabControl: TCustomTabControl;
      const AChild: TCustomPage; const NewIndex: integer); rootoverride;
    imptype procedure RemoveAllNBPages(const ATabControl: TCustomTabControl);
    imptype procedure RemovePage(const ATabControl: TCustomTabControl;
      const AIndex: integer); rootoverride;

    imptype function GetNotebookMinTabHeight(const AWinControl: TWinControl): integer; rootoverride;
    imptype function GetNotebookMinTabWidth(const AWinControl: TWinControl): integer; rootoverride;
    imptype function GetTabIndexAtPos(const ATabControl: TCustomTabControl; const AClientPos: TPoint): integer; rootoverride;
    imptype function GetTabRect(const ATabControl: TCustomTabControl; const AIndex: Integer): TRect; rootoverride;
    imptype function GetCapabilities: TCTabControlCapabilities; rootoverride;
    imptype function GetDesignInteractive(const AWinControl: TWinControl; AClientPos: TPoint): Boolean; override;
    imptype procedure SetTabSize(const ATabControl: TCustomTabControl; const ATabWidth, ATabHeight: integer); rootoverride;
    imptype procedure SetImageList(const ATabControl: TCustomTabControl; const AImageList: TCustomImageListResolution); rootoverride;
    imptype procedure SetPageIndex(const ATabControl: TCustomTabControl; const AIndex: integer); rootoverride;
    imptype procedure SetTabPosition(const ATabControl: TCustomTabControl; const ATabPosition: TTabPosition); rootoverride;
    imptype procedure ShowTabs(const ATabControl: TCustomTabControl; AShowTabs: boolean); rootoverride;
    imptype procedure UpdateProperties(const ATabControl: TCustomTabControl); rootoverride;
    {$ifdef wsintf}
    imptype procedure SetTabCaption(const ATabControl: TCustomTabControl; const AChild: TCustomPage; const AText: string);
    {$endif}
  end;

  { TWin32WSStatusBar }

  TWin32WSStatusBar = class({$ifndef wsintf}TWSStatusBar{$else}TWin32WSWinControl, IWSStatusBar{$endif})
  public
    class procedure DoUpdate(const AStatusBar: TStatusBar);
    class procedure DoSetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer);
    class function GetUpdated(const AStatusBar: TStatusBar): Boolean;
    class procedure SetUpdated(const AStatusBar: TStatusBar; const Value: Boolean);
  impsection
    imptype function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    imptype procedure Update(const AStatusBar: TStatusBar); rootoverride;
    imptype procedure PanelUpdate(const AStatusBar: TStatusBar; PanelIndex: integer); rootoverride;
    imptype procedure SetColor(const AWinControl: TWinControl); override;
    imptype procedure SetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer); rootoverride;
    imptype procedure SetSizeGrip(const AStatusBar: TStatusBar; SizeGrip: Boolean); rootoverride;
    imptype procedure SetText(const AWinControl: TWinControl; const AText: string); override;
    imptype procedure GetPreferredSize(const AWinControl: TWinControl;
                        var PreferredWidth, PreferredHeight: integer;
                        WithThemeSpace: Boolean); override;
  end;

  { TWin32WSTabSheet }

  TWin32WSTabSheet = class({$ifndef wsintf}TWSTabSheet{$else}TWin32WSCustomPage{$endif})
  impsection
  end;

  { TWin32WSPageControl }

  TWin32WSPageControl = class({$ifndef wsintf}TWSPageControl{$else}TWin32WSCustomTabControl{$endif})
  impsection
  end;

  { TWin32WSCustomListView }

  TWin32WSCustomListView = class({$ifndef wsintf}TWSCustomListView{$else}TWin32WSWinControl, IWSCustomListView{$endif})
  private
    imptype procedure ColumnDoAutosize(const ALV: TCustomListView; const AIndex: Integer);
    imptype function  GetHeader(const AHandle: THandle): THandle;
    imptype procedure PositionHeader(const AHandle: THandle);
    imptype procedure UpdateStyle(const AHandle: THandle; const AMask, AStyle: Integer);
    imptype procedure UpdateExStyle(const AHandle: THandle; const AMask, AStyle: Integer);
    imptype procedure LVItemAssign(const ALV: TCustomListView; AItem: TListItem; const AIndex: Integer);
  impsection
    // columns
    imptype procedure ColumnDelete(const ALV: TCustomListView; const AIndex: Integer); rootoverride;
    imptype function  ColumnGetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn): Integer; rootoverride;
    imptype procedure ColumnInsert(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn); rootoverride;
    imptype procedure ColumnMove(const ALV: TCustomListView; const AOldIndex, ANewIndex: Integer; const AColumn: TListColumn); rootoverride;
    imptype procedure ColumnSetAlignment(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AAlignment: TAlignment); rootoverride;
    imptype procedure ColumnSetAutoSize(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AAutoSize: Boolean); rootoverride;
    imptype procedure ColumnSetCaption(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const ACaption: String); rootoverride;
    imptype procedure ColumnSetImage(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AImageIndex: Integer); rootoverride;
    imptype procedure ColumnSetMaxWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AMaxWidth: Integer); rootoverride;
    imptype procedure ColumnSetMinWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AMinWidth: integer); rootoverride;
    imptype procedure ColumnSetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AWidth: Integer); rootoverride;
    imptype procedure ColumnSetVisible(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AVisible: Boolean); rootoverride;
    imptype procedure ColumnSetSortIndicator(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AAndicator: TSortIndicator); rootoverride;

    // items
    imptype procedure ItemDelete(const ALV: TCustomListView; const AIndex: Integer); rootoverride;
    imptype function  ItemDisplayRect(const ALV: TCustomListView; const AIndex, ASubItem: Integer; ACode: TDisplayCode): TRect; rootoverride;
    imptype procedure ItemExchange(const ALV: TCustomListView; AItem: TListItem; const AIndex1, AIndex2: Integer); rootoverride;
    imptype procedure ItemMove(const ALV: TCustomListView; AItem: TListItem; const AFromIndex, AToIndex: Integer); rootoverride;
    imptype function  ItemGetChecked(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem): Boolean; rootoverride;
    imptype function  ItemGetPosition(const ALV: TCustomListView; const AIndex: Integer): TPoint; rootoverride;
    imptype function  ItemGetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; out AIsSet: Boolean): Boolean; rootoverride; // returns True if supported
    imptype function  ItemGetStates(const ALV: TCustomListView; const AIndex: Integer; out AStates: TListItemStates): Boolean; rootoverride;
    imptype procedure ItemInsert(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem); rootoverride;
    imptype procedure ItemSetChecked(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AChecked: Boolean); rootoverride;
    imptype procedure ItemSetImage(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex, AImageIndex: Integer); rootoverride;
    imptype function  ItemSetPosition(const ALV: TCustomListView; const AIndex: Integer; const ANewPosition: TPoint): Boolean; rootoverride;
    imptype procedure ItemSetStateImage(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex, AStateImageIndex: Integer); rootoverride;
    imptype procedure ItemSetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; const AIsSet: Boolean); rootoverride;
    imptype procedure ItemSetText(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex: Integer; const AText: String); rootoverride;
    imptype procedure ItemShow(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const PartialOK: Boolean); rootoverride;
    {$ifdef wsintf}
    imptype procedure ItemUpdate(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem); rootoverride;
    {$endif}
  
    // lv
    imptype function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;

    imptype procedure BeginUpdate(const ALV: TCustomListView); rootoverride;
    imptype procedure EndUpdate(const ALV: TCustomListView); rootoverride;

    imptype function GetBoundingRect(const ALV: TCustomListView): TRect; rootoverride;
    imptype function GetDropTarget(const ALV: TCustomListView): Integer; rootoverride;
    imptype function GetFocused(const ALV: TCustomListView): Integer; rootoverride;
    imptype function GetHitTestInfoAt( const ALV: TCustomListView; X, Y: Integer ) : THitTests; rootoverride;
    imptype function GetHoverTime(const ALV: TCustomListView): Integer; rootoverride;
    imptype function GetItemAt(const ALV: TCustomListView; x,y: Integer): Integer; rootoverride;
    imptype function GetSelCount(const ALV: TCustomListView): Integer; rootoverride;
    imptype function GetSelection(const ALV: TCustomListView): Integer; rootoverride;
    imptype function GetTopItem(const ALV: TCustomListView): Integer; rootoverride;
    imptype function GetViewOrigin(const ALV: TCustomListView): TPoint; rootoverride;
    imptype function GetVisibleRowCount(const ALV: TCustomListView): Integer; rootoverride;
    imptype function GetNextItem(const ALV: TCustomListView; const StartItem: TListItem; const Direction: TSearchDirection; const States: TListItemStates): TListItem; rootoverride;

    imptype procedure SelectAll(const ALV: TCustomListView; const AIsSet: Boolean); rootoverride;
    imptype procedure SetAllocBy(const ALV: TCustomListView; const AValue: Integer); rootoverride;
    imptype procedure SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle); override;
    imptype procedure SetColor(const AWinControl: TWinControl); override;
    imptype procedure SetDefaultItemHeight(const ALV: TCustomListView; const AValue: Integer); rootoverride;
    imptype procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
    imptype procedure SetHotTrackStyles(const ALV: TCustomListView; const AValue: TListHotTrackStyles); rootoverride;
    imptype procedure SetHoverTime(const ALV: TCustomListView; const AValue: Integer); rootoverride;
    imptype procedure SetIconArrangement(const ALV: TCustomListView; const AValue: TIconArrangement); rootoverride;
    imptype procedure SetImageList(const ALV: TCustomListView; const AList: TListViewImageList; const AValue: TCustomImageListResolution); rootoverride;
    imptype procedure SetItemsCount(const ALV: TCustomListView; const AValue: Integer); rootoverride;
    imptype procedure SetOwnerData(const ALV: TCustomListView; const AValue: Boolean); rootoverride;
    imptype procedure SetProperty(const ALV: TCustomListView; const AProp: TListViewProperty; const AIsSet: Boolean); rootoverride;
    imptype procedure SetProperties(const ALV: TCustomListView; const AProps: TListViewProperties); rootoverride;
    imptype procedure SetScrollBars(const ALV: TCustomListView; const AValue: TScrollStyle); rootoverride;
    imptype procedure SetSort(const ALV: TCustomListView; const AType: TSortType; const AColumn: Integer;
      const ASortDirection: TSortDirection); rootoverride;
    imptype procedure SetViewOrigin(const ALV: TCustomListView; const AValue: TPoint); rootoverride;
    imptype procedure SetViewStyle(const ALV: TCustomListView; const Avalue: TViewStyle); rootoverride;
    {$ifdef wsintf}
    imptype function RestoreItemCheckedAfterSort(const ALV: TCustomListView): Boolean; rootoverride;
    {$endif}
  end;

  { TWin32WSListView }

  TWin32WSListView = class({$ifndef wsintf}TWSListView{$else}TWin32WSCustomListView{$endif})
  impsection
  end;

  { TWin32WSProgressBar }

  TWin32WSProgressBar = class({$ifndef wsintf}TWSProgressBar{$else}TWin32WSWinControl, IWSProgressBar{$endif})
  impsection
    imptype function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    imptype procedure ApplyChanges(const AProgressBar: TCustomProgressBar); rootoverride;
    imptype procedure SetPosition(const AProgressBar: TCustomProgressBar; const NewPosition: integer); rootoverride;
    imptype procedure SetStyle(const AProgressBar: TCustomProgressBar; const NewStyle: TProgressBarStyle); rootoverride;
    imptype function GetConstraints(const AControl: TControl; const AConstraints: TObject): Boolean; override;
  end;

  { TWin32WSCustomUpDown }

  TWin32WSCustomUpDown = class({$ifndef wsintf}TWSCustomUpDown{$else}TWin32WSCustomControl{$endif})
  published
  end;

  { TWin32WSUpDown }

  TWin32WSUpDown = class({$ifndef wsintf}TWSUpDown{$else}TWin32WSCustomUpDown{$endif})
  published
  end;

  { TWin32WSToolButton }

  TWin32WSToolButton = class({$ifndef wsintf}TWSToolButton{$else}TWin32WSCustomControl{$endif})
  published
  end;

  { TWin32WSToolBar }

  TWin32WSToolBar = class({$ifndef wsintf}TWSToolBar{$else}TWin32WSCustomControl{$endif})
  published
{$ifdef OldToolbar}
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class function  GetButtonCount(const AToolBar: TToolBar): integer; override;
    class procedure InsertToolButton(const AToolBar: TToolbar; const AControl: TControl); override;
    class procedure DeleteToolButton(const AToolBar: TToolbar; const AControl: TControl); override;
{$endif}
  end;

  { TWin32WSTrackBar }

  TWin32WSTrackBar = class({$ifndef wsintf}TWSTrackBar{$else}TWin32WSWinControl, IWSTrackBar{$endif})
  impsection
    imptype function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    imptype procedure DefaultWndHandler(const AWinControl: TWinControl;
       var AMessage); override;
    imptype procedure ApplyChanges(const ATrackBar: TCustomTrackBar); rootoverride;
    imptype function GetPosition(const ATrackBar: TCustomTrackBar): integer; rootoverride;
    imptype procedure SetPosition(const ATrackBar: TCustomTrackBar; const NewPosition: integer); rootoverride;
    imptype procedure SetTick(const ATrackBar: TCustomTrackBar; const ATick: integer); rootoverride;
    {$ifdef wsintf}
    imptype procedure SetOrientation(const ATrackBar: TCustomTrackBar; const AOrientation: TTrackBarOrientation); rootoverride;
    imptype procedure SetTickStyle(const ATrackBar: TCustomTrackBar; const ATickStyle: TTickStyle); rootoverride;
    {$endif}
  end;

  { TWin32WSCustomTreeView }

  TWin32WSCustomTreeView = class({$ifndef wsintf}TWSCustomTreeView{$else}TWin32WSWinControl{$endif})
  published
  end;

  { TWin32WSTreeView }

  TWin32WSTreeView = class({$ifndef wsintf}TWSTreeView{$else}TWin32WSCustomTreeView{$endif})
  published
  end;

procedure TabControlFocusNewControl(const ATabControl: TCustomTabControl; NewIndex: integer);
function ShowHideTabPage(TabControlHandle: HWnd; Showing: boolean): integer;

implementation

const
  DefMarqueeTime = 50; // ms

{$I win32pagecontrol.inc}
{$I win32treeview.inc}

type
  TStatusPanelAccess = class(TStatusPanel);

{$I win32wscustomlistview.inc }


{ --- Helper routines for TWin32WSStatusBar --- }

var
  PreferredStatusBarHeight: integer = 0;

procedure InitializePreferredStatusBarHeight;
var
  Flags: LongWord;
  Parent: HWND;
  PreferredSizeStatusBar: HWND;
  R: TRect;
  AErrorCode: Cardinal;
begin
  Flags := WS_CHILD or WS_CLIPSIBLINGS or WS_CLIPCHILDREN;
  Parent := TWin32WidgetSet(WidgetSet).AppHandle;
  if ( Parent=0 ) and IsLibrary and Assigned( Screen.ActiveForm ) then
    Parent := Screen.ActiveForm.Handle;
  PreferredSizeStatusBar := CreateWindowExW(0, STATUSCLASSNAMEW,
    nil, Flags,
    0, 0, 0, 0, Parent, 0, HInstance, nil);
  if PreferredSizeStatusBar = 0 then
  begin
    AErrorCode := GetLastError;
    DebugLn(['Failed to create win32 control, error: ', AErrorCode, ' : ', GetLastErrorText(AErrorCode)]);
    raise Exception.Create('Failed to create win32 control, error: ' + IntToStr(AErrorCode) + ' : ' + GetLastErrorText(AErrorCode));
  end;
  GetWindowRect(PreferredSizeStatusBar, R);
  PreferredStatusBarHeight := R.Bottom - R.Top;
  DestroyWindow(PreferredSizeStatusBar);
end;

{------------------------------------------------------------------------------
  Method: UpdateStatusBarPanel
  Params: StatusPanel - StatusPanel which needs to be update
  Returns: Nothing

  Called by StatusBarPanelUpdate and StatusBarSetText
  Everything is updated except the panel width
 ------------------------------------------------------------------------------}
procedure UpdateStatusBarPanel(const StatusPanel: TStatusPanel);
const
  StatusBevelMap: array[TStatusPanelBevel] of Integer =
  (
{ pbNone    } Windows.SBT_NOBORDERS,
{ pbLowered } 0,
{ pbRaised  } Windows.SBT_POPOUT
  );
var
  Text: string;
  WParam: windows.WPARAM;
begin
  Text := StatusPanel.Text;
  //debugln('UpdateStatusBarPanel: Text=',Text);
  case StatusPanel.Alignment of
    taCenter: Text := #9 + Text;
    taRightJustify: Text := #9#9 + Text;
  end;
  WParam := StatusBevelMap[StatusPanel.Bevel];
  if StatusPanel.Style = psOwnerDraw then
    WParam := WParam or SBT_OWNERDRAW;
  //if UseRightToLeftAlignment then set Text on the ((Count - 1) - Index) panel ("mirrored"),
  //because Panels are always counted Left to Right
  //See: http://msdn.microsoft.com/en-us/library/windows/desktop/bb760757%28v=vs.85%29.aspx
  if StatusPanel.StatusBar.UseRightToLeftAlignment then
    WParam := WParam or ((StatusPanel.StatusBar.Panels.Count - 1) - StatusPanel.Index)
  else
    WParam := WParam or StatusPanel.Index;
  if StatusPanel.StatusBar.UseRightToLeftReading then WParam := WParam or SBT_RTLREADING;
    Windows.SendMessageW(StatusPanel.StatusBar.Handle, SB_SETTEXTW, WParam, LPARAM(PWideChar(UTF8ToUTF16(Text))));
end;

procedure UpdateStatusBarPanelWidths(const StatusBar: TStatusBar);
var
  Rights: PInteger;
  PanelIndex: integer;
  CurrentRight: integer;
begin
  //debugln('UpdateStatusBarPanelWidths');
  if StatusBar.Panels.Count = 0 then
  begin
    // SETPARTS 0,0 does not work :S
    Windows.SendMessage(StatusBar.Handle, SB_SIMPLE, 1, 0);
    Windows.SendMessage(StatusBar.Handle, SB_SETTEXT, 255, WPARAM(PChar('')));
    exit;
  end;
  Getmem(Rights, StatusBar.Panels.Count * SizeOf(integer));
  try
    if not StatusBar.UseRightToLeftAlignment then
    begin
      CurrentRight := 0;
      for PanelIndex := 0 to StatusBar.Panels.Count - 2 do
      begin
        CurrentRight := CurrentRight + StatusBar.Panels[PanelIndex].Width;
        Rights[PanelIndex] := CurrentRight;
        //debugln(Format('CurrentRight for Panel[%d] = %d',[PanelIndex,CurrentRight]));
      end;
      Rights[StatusBar.Panels.Count-1] := -1; //Last extends to end;
    end
    else
    begin
      //"Mirror" the width of the panels and align the lot to the right
      //It seems that panels (parts in MS speak) are always counted Left to Right
      //See: http://msdn.microsoft.com/en-us/library/windows/desktop/bb760757%28v=vs.85%29.aspx
      CurrentRight := 0;
      for PanelIndex := 0 to StatusBar.Panels.Count - 1 do
      begin
        CurrentRight := CurrentRight + StatusBar.Panels[(StatusBar.Panels.Count-1) - PanelIndex].Width;
        Rights[PanelIndex] := CurrentRight;
        //debugln(Format('CurrentRight for Panel[%d] = %d',[PanelIndex,CurrentRight]));
      end;
      for PanelIndex := 0 to StatusBar.Panels.Count - 1 do
        Rights[PanelIndex] := Rights[PanelIndex] + (StatusBar.ClientWidth - CurrentRight);
      //Rights[StatusBar.Panels.Count-1] := -1; //Last extends to end;
    end;
    Windows.SendMessage(StatusBar.Handle, SB_SETPARTS, StatusBar.Panels.Count, LPARAM(Rights));
  finally
    Freemem(Rights);
  end;
end;

function StatusBarWndProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
var
  Info: PWin32WindowInfo;
  Control: TWinControl;
  Details: TThemedElementDetails;
begin
  Info := GetWin32WindowInfo(Window);
  if (Info = nil) or (Info^.WinControl = nil) then
  begin
    Result := CallDefaultWindowProc(Window, Msg, WParam, LParam);
    Exit;
  end
  else
    Control := Info^.WinControl;

  if Msg = WM_PAINT then
  begin
    TWin32WSStatusBar.DoUpdate(TStatusBar(Control));
    Result := WindowProc(Window, Msg, WParam, LParam);
  end
  else
  if Assigned(ThemeServices) and ThemeServices.ThemesEnabled then
  begin
    // Paul: next is a slightly modified code of TThemeManager.StatusBarWindowProc
    // of Mike Lischke Theme manager library (Mike granted us permition to use his code)
    case Msg of
      WM_NCCALCSIZE:
        begin
          // We need to override the window class' CS_HREDRAW and CS_VREDRAW styles but the following
          // does the job very well too.
          // Note: this may produce trouble with embedded controls (e.g. progress bars).
          if WParam <> 0 then
            Result := CallDefaultWindowProc(Window, Msg, WParam, LParam) or WVR_REDRAW
          else
            Result := 1;
        end;
      WM_ERASEBKGND:
        begin
          Details := ThemeServices.GetElementDetails(tsStatusRoot);
          ThemeServices.DrawElement(HDC(WParam), Details, Control.ClientRect);
          Result := 1;
        end;
      else
        Result := WindowProc(Window, Msg, WParam, LParam);
    end;
  end
  else
    Result := WindowProc(Window, Msg, WParam, LParam);
end;

{ TWin32WSStatusBar }

class procedure TWin32WSStatusBar.DoUpdate(const AStatusBar: TStatusBar);
var
  PanelIndex: integer;
begin
  // if we catch WM_PAINT and no update is needed then skip processing or we will
  // do endless repaint
  //debugln('TWin32WSStatusBar.DoUpdate');

  if GetUpdated(AStatusBar) then
    Exit;

  // set updated flag here since SB_SETTEXT can call WM_PAINT on some
  // windowses (win98) and we will have endless update
  SetUpdated(AStatusBar, True);

  if AStatusBar.SimplePanel then
    DoSetPanelText(AStatusBar, 0)
  else
  begin
    // we store a flag that we need to update panel in the IntfFlag property
    for PanelIndex := 0 to AStatusBar.Panels.Count - 1 do
      if TStatusPanelAccess(AStatusBar.Panels[PanelIndex]).FIntfFlag <> 1 then
      begin
        TStatusPanelAccess(AStatusBar.Panels[PanelIndex]).FIntfFlag := 1;
        UpdateStatusBarPanel(AStatusBar.Panels[PanelIndex]);
      end;
  end;
end;

imptype function TWin32WSStatusBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    Flags := Flags or CCS_NOPARENTALIGN or CCS_NORESIZE;
    if TStatusBar(AWinControl).SizeGrip and TStatusBar(AWinControl).SizeGripEnabled then
      Flags := Flags or SBARS_SIZEGRIP;
    pClassName := STATUSCLASSNAME;
    WindowTitle := StrCaption;
    SubClassWndProc := @StatusBarWndProc;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Params.WindowInfo^.needParentPaint := false;
  // need to set handle for Update method
  AWinControl.Handle := Params.Window;
  Update(TStatusBar(AWinControl));
  Result := Params.Window;
end;

imptype procedure TWin32WSStatusBar.PanelUpdate(const AStatusBar: TStatusBar; PanelIndex: integer);
var
  ARect: TRect;
begin
  UpdateStatusBarPanelWidths(AStatusBar);
  TStatusPanelAccess(AStatusBar.Panels[PanelIndex]).FIntfFlag := 0;
  SetUpdated(AStatusBar, False);
  // request invalidate of only panel rectange
  SendMessage(AStatusBar.Handle, SB_GETRECT, PanelIndex, LParam(@ARect));
  Windows.InvalidateRect(AStatusBar.Handle, ARect, False);
end;

imptype procedure TWin32WSStatusBar.SetColor(const AWinControl: TWinControl);
begin
  if not WSCheckHandleAllocated(AWinControl, 'TWin32WSStatusBar.SetColor') then
    Exit;
  if AWinControl.Color = clDefault then
    Windows.SendMessage(AWinControl.Handle, SB_SETBKCOLOR, 0, ColorToRGB(AWinControl.GetDefaultColor(dctBrush)))
  else
    Windows.SendMessage(AWinControl.Handle, SB_SETBKCOLOR, 0, ColorToRGB(AWinControl.Color));
end;

imptype procedure TWin32WSStatusBar.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  if (PreferredStatusBarHeight = 0) then
    InitializePreferredStatusBarHeight;

  PreferredHeight := PreferredStatusBarHeight;
end;

class procedure TWin32WSStatusBar.DoSetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer);
const
  SB_SIMPLEID = $FF;
var
  WParam: windows.WPARAM;
begin
  if AStatusBar.SimplePanel then
  begin
    if AStatusBar.UseRightToLeftReading then
      WParam := SB_SIMPLEID or SBT_RTLREADING
    else
      WParam := SB_SIMPLEID;
    Windows.SendMessageW(AStatusBar.Handle, SB_SETTEXTW, WParam, LPARAM(PWideChar(UTF8ToUTF16(AStatusBar.SimpleText))));
  end
  else
    UpdateStatusBarPanel(AStatusBar.Panels[PanelIndex]);
end;

class function TWin32WSStatusBar.GetUpdated(const AStatusBar: TStatusBar): Boolean;
begin
  Result := Windows.GetProp(AStatusBar.Handle, 'lcl-statusbar-updated') = 1;
end;

class procedure TWin32WSStatusBar.SetUpdated(const AStatusBar: TStatusBar;
  const Value: Boolean);
begin
  Windows.SetProp(AStatusBar.Handle, 'lcl-statusbar-updated', Ord(Value));
end;

imptype procedure TWin32WSStatusBar.SetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer);
begin
  if AStatusBar.SimplePanel then
  begin
    SetUpdated(AStatusBar, False);
    AStatusBar.Invalidate;
  end
  else
    PanelUpdate(AStatusBar, PanelIndex);
end;

imptype procedure TWin32WSStatusBar.SetSizeGrip(const AStatusBar: TStatusBar;
  SizeGrip: Boolean);
var
  AStyle: Long;
begin
  if not WSCheckHandleAllocated(AStatusBar, 'SetSizeGrip') then
    Exit;
  AStyle := GetWindowLong(AStatusBar.Handle, GWL_STYLE);
  if ((AStyle and SBARS_SIZEGRIP) <> 0) <> (SizeGrip and AStatusBar.SizeGripEnabled) then
    RecreateWnd(AStatusBar);
end;

imptype procedure TWin32WSStatusBar.SetText(const AWinControl: TWinControl;
  const AText: string);
begin
  // inhibit. StatusBars do not have a caption, simpletext is set by SetPanelText
end;

imptype procedure TWin32WSStatusBar.Update(const AStatusBar: TStatusBar);
var
  i: integer;
begin
  //debugln('TWin32WSStatusBar.Update');
  Windows.SendMessage(AStatusBar.Handle, SB_SIMPLE, WPARAM(AStatusBar.SimplePanel), 0);
  if not AStatusBar.SimplePanel then
  begin
    UpdateStatusBarPanelWidths(AStatusBar);
    for i := 0 to AStatusBar.Panels.Count - 1 do
      TStatusPanelAccess(AStatusBar.Panels[i]).FIntfFlag := 0;
  end;

  // To reduce statusbar flickering it is suggested to wait for WM_PAINT message and
  // to set text there (http://msdn.microsoft.com/en-us/library/bb760728(VS.85).aspx)
  // Lets do so. But changing text on WM_PAINT cause another invalidate. So to
  // prevent endless repaint we need to check whether we already updated statusbar

  SetUpdated(AStatusBar, False);
  AStatusBar.Invalidate;
end;

function ProgressBarWndProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
begin
  // Marquee progress bar on vista/w7 required to call default window proc to
  // setup the timer
  if (Msg = WM_PAINT) and
     (Win32WidgetSet.CommonControlsVersion >= ComCtlVersionIE6) and
     (GetWindowLong(Window, GWL_STYLE) and PBS_MARQUEE = PBS_MARQUEE) then
    CallDefaultWindowProc(Window, Msg, WParam, LParam);
  Result := WindowProc(Window, Msg, WParam, LParam);
end;

{ TWin32WSProgressBar }

imptype function TWin32WSProgressBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    with TCustomProgressBar(AWinControl) do
    begin
      if Smooth then
        Flags := Flags or PBS_SMOOTH;
      if (Orientation = pbVertical) or (Orientation = pbTopDown) then
        Flags := Flags or PBS_VERTICAL;
      if (Win32WidgetSet.CommonControlsVersion >= ComCtlVersionIE6) and
         (Style = pbstMarquee) then
        Flags := Flags or PBS_MARQUEE;
    end;
    pClassName := PROGRESS_CLASS;
    SubClassWndProc := @ProgressBarWndProc;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, False);
  Result := Params.Window;
  if (Win32WidgetSet.CommonControlsVersion >= ComCtlVersionIE6) and
     (TCustomProgressBar(AWinControl).Style = pbstMarquee) then
    SendMessage(Result, PBM_SETMARQUEE, WParam(LongBool(True)), DefMarqueeTime);
end;

imptype procedure TWin32WSProgressBar.ApplyChanges(
  const AProgressBar: TCustomProgressBar);
begin
  with AProgressBar do
  begin
    { smooth and vertical need window recreation }
    if ((GetWindowLong(Handle, GWL_STYLE) and PBS_SMOOTH  ) <>
         PtrInt(Smooth) * PBS_SMOOTH) or
       ((GetWindowLong(Handle, GWL_STYLE) and PBS_VERTICAL) <>
         PtrInt((Orientation = pbVertical) or (Orientation = pbTopDown)) * PBS_VERTICAL) then
      RecreateWnd(AProgressBar);

    SendMessage(Handle, PBM_SETRANGE32, Min, Max);
    SendMessage(Handle, PBM_SETPOS, Position, 0);

{ TODO: Implementable?
    If BarShowText Then
    Begin
      SetWindowText(Handle, StrToPChar((Sender As TControl).Caption));
    End
    Else
      SetWindowText(Handle, Nil);
}
  end;
end;

imptype procedure TWin32WSProgressBar.SetPosition(
  const AProgressBar: TCustomProgressBar; const NewPosition: integer);
begin
  Windows.SendMessage(AProgressBar.Handle, PBM_SETPOS, Windows.WPARAM(NewPosition), 0);
end;

imptype procedure TWin32WSProgressBar.SetStyle(
  const AProgressBar: TCustomProgressBar; const NewStyle: TProgressBarStyle);
var
  Style: DWord;
begin
  if not WSCheckHandleAllocated(AProgressBar, 'SetStyle') then
    Exit;
  if (Win32WidgetSet.CommonControlsVersion >= ComCtlVersionIE6) then
  begin
    // Comctl32 >= 6
    Style := GetWindowLong(AProgressBar.Handle, GWL_STYLE);
    if NewStyle = pbstMarquee then
      Style := Style or PBS_MARQUEE
    else
      Style := Style and not PBS_MARQUEE;
    SetWindowLong(AProgressBar.Handle, GWL_STYLE, Style);
    SendMessage(AProgressBar.Handle, PBM_SETMARQUEE, Ord(NewStyle = pbstMarquee), DefMarqueeTime);
    if NewStyle = pbstNormal then
      SetPosition(AProgressBar, AProgressBar.Position);
  end;
end;

imptype function TWin32WSProgressBar.GetConstraints(const AControl: TControl;
  const AConstraints: TObject): Boolean;
var
  SizeConstraints: TSizeConstraints absolute AConstraints;
  MinWidth, MinHeight, MaxWidth, MaxHeight: Integer;
begin
  Result := True;

  if (AConstraints is TSizeConstraints) then
  begin
    MinWidth := 0;
    MinHeight := 0;
    MaxWidth := 0;
    MaxHeight := 0;

    // The ProgressBar needs a minimum Height of 10 on Windows XP when themed,
    // as required by Windows, otherwise it's image is corrupted
    if (Win32MajorVersion < 6) and ThemeServices.ThemesEnabled then
      MinHeight := 10;

    SizeConstraints.SetInterfaceConstraints(MinWidth, MinHeight, MaxWidth, MaxHeight);
  end;
end;

{ TWin32WSToolbar}

{$ifdef OldToolbar}

class function TWin32WSToolBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    pClassName := TOOLBARCLASSNAME;
    Flags := Flags or CCS_ADJUSTABLE;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;

class function TWin32WSToolbar.GetButtonCount(const AToolBar: TToolBar): integer;
begin
  Result := SendMessage(AToolbar.Handle, TB_BUTTONCOUNT, 0, 0)
end;

class procedure TWin32WSToolbar.InsertToolButton(const AToolBar: TToolbar; const AControl: TControl);
var
  PStr, PStr2: PChar;
  Num: Integer;
  TBB: TBBUTTON;
begin
  // TODO: check correctness / clean up
  If (AControl is TWinControl) Then
  Begin
    PStr := StrAlloc(Length(TToolButton(AControl).Caption) + 1);
    StrPCopy(PStr, TToolButton(AControl).Caption);
    PStr2 := StrAlloc(Length(TControl(AControl).Hint) + 1);
    StrPCopy(PStr2, TControl(AControl).Hint);
  End
  Else
  Begin
    Raise Exception.Create('Can not assign this control to the toolbar');
    Exit;
  End;

  Num := TToolbar(TWinControl(AControl).Parent).Buttonlist.IndexOf(TControl(AControl));
  If Num < 0 Then
    Num := TToolbar(TWinControl(AControl).Parent).Buttonlist.Count + 1;

  With tbb Do
  Begin
    iBitmap := Num;
    idCommand := Num;
    fsState := TBSTATE_ENABLED;
    fsStyle := TBSTYLE_BUTTON;
    iString := Integer(PStr);
  End;

  SendMessage(TWinControl(AControl).Parent.Handle, TB_BUTTONSTRUCTSIZE, SizeOf(TBBUTTON), 0);
  SendMessage(TWinControl(AControl).Parent.Handle, TB_ADDBUTTONS, 1, LParam(LPTBButton(@tbb)));
  StrDispose(pStr);
  StrDispose(pStr2);
end;

class procedure TWin32WSToolbar.DeleteToolButton(const AToolBar: TToolbar; const AControl: TControl);
begin
  // TODO: code buggy, Index of button to delete ?!
  SendMessage(AToolBar.Handle, TB_DELETEBUTTON, 0, 0);
end;

{$endif}

function TrackBarParentMsgHandler(const AWinControl: TWinControl; Window: HWnd;
      Msg: UInt; WParam: Windows.WParam; LParam: Windows.LParam;
      var MsgResult: Windows.LResult; var WinProcess: Boolean): Boolean;
var
  Info: PWin32WindowInfo;
  Message: TLMessage;
begin
  Result := False;
  case Msg of
    WM_HSCROLL,
    WM_VSCROLL:
    begin
      MsgResult := CallDefaultWindowProc(Window, Msg, WParam, LParam);
      Info := GetWin32WindowInfo(HWND(LParam));
      if Assigned(Info^.WinControl) then
      begin
        Message.msg := LM_CHANGED;
        Message.wParam := 0;
        Message.lParam := 0;
        Message.Result := 0;

        //debugln('LOWORD(WPARAM)=%d, HIWORD(WPARAM)=%d', [LOWORD(WParam), HIWORD(WPARAM)]);
        {$ifdef wsintf}
        if GetWSTrackBar(Info^.WinControl.WidgetSetClass).GetPosition(TCustomTrackBar(Info^.WinControl))<>TCustomTrackBar(Info^.WinControl).Position then
        {$else}
        if TWin32WSTrackBar.GetPosition(TCustomTrackBar(Info^.WinControl))<>TCustomTrackBar(Info^.WinControl).Position then
        {$endif}
          DeliverMessage(Info^.WinControl, Message);
      end;
      Result := True;
    end;
  end;
end;

{ TWin32WSTrackBar }

imptype function TWin32WSTrackBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    pClassName := TRACKBAR_CLASS;
    WindowTitle := StrCaption;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Params.WindowInfo^.ParentMsgHandler := @TrackBarParentMsgHandler;
  Params.WindowInfo^.ThemedCustomDraw := true;
  Result := Params.Window;
end;

imptype procedure TWin32WSTrackBar.DefaultWndHandler(
  const AWinControl: TWinControl; var AMessage);
var
  WindowInfo: PWin32WindowInfo;
  Control: TWinControl;
  FocusBorderWidth,
  FocusBorderHeight, Offset: Integer;
  R: TRect;
  Rgn: HRGN;
  Details: TThemedElementDetails;
  NMHdr: PNMHDR;
begin
  // Paul: next is a slightly modified code of TThemeManager.TrackBarWindowProc
  // of Mike Lischke Theme manager library (Mike granted us permition to use his code)
  with TLMessage(AMessage) do
    case Msg of
      CN_NOTIFY:
        if ThemeServices.ThemesEnabled then
        begin
          NMHdr := PNMHDR(LParam);
          if NMHdr^.code = NM_CUSTOMDRAW then
          begin
            WindowInfo := GetWin32WindowInfo(PNMHdr(LParam)^.hwndFrom);
            Control := WindowInfo^.WinControl;
            case PNMCustomDraw(LParam)^.dwDrawStage of
              CDDS_PREPAINT:
              begin
                Result := CDRF_NOTIFYITEMDRAW;
              end;
              CDDS_ITEMPREPAINT:
              begin
                case PNMCustomDraw(LParam)^.dwItemSpec of
                  TBCD_TICS: // Before re-painting ticks redo whole background.
                    begin
                      R := Control.ClientRect;
                      // Leave room for the focus rectangle if there is one.
                      if Control.Focused and
                         ((Control.Perform(WM_QUERYUISTATE, 0, 0) and UISF_HIDEFOCUS) = 0) then
                      begin
                        SystemParametersInfo(SPI_GETFOCUSBORDERWIDTH, 0, @FocusBorderWidth, 0);
                        SystemParametersInfo(SPI_GETFOCUSBORDERHEIGHT, 0, @FocusBorderHeight, 0);
                        InflateRect(R, -FocusBorderWidth, -FocusBorderHeight);
                      end;
                      ThemeServices.DrawParentBackground(AWinControl.Handle, PNMCustomDraw(LParam)^.hDC, nil, False, @R);
                    end;
                  TBCD_CHANNEL:
                    begin
                      // Retrieve the bounding box for the thumb.
                      SendMessage(AWinControl.Handle, TBM_GETTHUMBRECT, 0, PtrInt(@R));
                      // Extend this rectangle to the top/bottom or left/right border, respectively.
                      Offset := 0;
                      if TCustomTrackBar(Control).Orientation = trHorizontal then
                      begin
                        // Leave room for the focus rectangle if there is one.
                        if Control.Focused then
                        begin
                          SystemParametersInfo(SPI_GETFOCUSBORDERWIDTH, 0, @FocusBorderWidth, 0);
                          Inc(Offset, FocusBorderWidth);
                        end;
                        R.Left := Control.ClientRect.Left + Offset;
                        R.Right := Control.ClientRect.Right - Offset;
                      end
                      else
                      begin
                        // Leave room for the focus rectangle if there is one.
                        if Control.Focused then
                        begin
                          SystemParametersInfo(SPI_GETFOCUSBORDERHEIGHT, 0, @FocusBorderHeight, 0);
                          Inc(Offset, FocusBorderHeight);
                        end;
                        R.Top := Control.ClientRect.Top + Offset;
                        R.Bottom := Control.ClientRect.Bottom - Offset;
                      end;
                      Rgn := CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);
                      SelectClipRgn(PNMCustomDraw(LParam)^.hDC, Rgn);
                      Details := ThemeServices.GetElementDetails(ttbThumbTics);
                      ThemeServices.DrawParentBackground(AWinControl.Handle, PNMCustomDraw(LParam)^.hDC, @Details, False);
                      DeleteObject(Rgn);
                      SelectClipRgn(PNMCustomDraw(LParam)^.hDC, 0);
                    end;
                end;
                Result := CDRF_DODEFAULT;
              end;
            end;
          end;
        end
      else
        inherited DefaultWndHandler(AWinControl, AMessage);
      else
        inherited DefaultWndHandler(AWinControl, AMessage);
    end;
end;

imptype procedure TWin32WSTrackBar.ApplyChanges(const ATrackBar: TCustomTrackBar);
var
  wHandle: HWND;
  NewStyle: integer;
  lTickStyle: DWORD;
const
  StyleMask = TBS_AUTOTICKS or TBS_NOTICKS or TBS_VERT or TBS_TOP or TBS_BOTH or
    TBS_ENABLESELRANGE or TBS_REVERSED;
  TickStyleStyle: array[TTickStyle] of DWORD = (TBS_NOTICKS, TBS_AUTOTICKS, 0);
  OrientationStyle: array[TTrackBarOrientation] of DWORD = (TBS_HORZ, TBS_VERT);
  TickMarksStyle: array[TTickMark] of DWORD = (TBS_BOTTOM, TBS_TOP, TBS_BOTH);
  SelRangeStyle: array[Boolean] of DWORD = (0, TBS_ENABLESELRANGE);
  ReversedStyle: array[Boolean] of DWORD = (0, TBS_REVERSED);
begin
  with ATrackBar do
  begin
    { cache handle }
    wHandle := Handle;
    lTickStyle := TickStyleStyle[TickStyle];
    {$IFNDEF WIN32}
    if Max - Min > $7FFF then  // Workaround for #36046:
      lTickStyle := 0;         // No ticks to avoid hanging if range is too large
    {$ENDIF}
    NewStyle := lTickStyle or OrientationStyle[Orientation] or
                TickMarksStyle[TickMarks] or SelRangeStyle[ShowSelRange] or ReversedStyle[Reversed];
    UpdateWindowStyle(wHandle, NewStyle, StyleMask);
    Windows.SendMessage(wHandle, TBM_SETRANGEMAX, Windows.WPARAM(True), Max);
    Windows.SendMessage(wHandle, TBM_SETRANGEMIN, Windows.WPARAM(True), Min);
    if Reversed then
      Windows.SendMessage(wHandle, TBM_SETPOS, Windows.WPARAM(True), Max + Min - Position)
    else
      Windows.SendMessage(wHandle, TBM_SETPOS, Windows.WPARAM(True), Position);
    Windows.SendMessage(wHandle, TBM_SETLINESIZE, 0, LineSize);
    Windows.SendMessage(wHandle, TBM_SETPAGESIZE, 0, PageSize);
    Windows.SendMessage(wHandle, TBM_SETTICFREQ, Frequency, 0);
    if ((SelStart = 0) and (SelEnd = 0)) or not ShowSelRange then
      Windows.SendMessage(wHandle, TBM_CLEARSEL, Windows.WPARAM(True), 0)
    else
    begin
      Windows.SendMessage(wHandle, TBM_SETSELSTART, Windows.WParam(False), SelStart);
      Windows.SendMessage(wHandle, TBM_SETSELEND, Windows.WParam(True), SelEnd)
    end;
  end;
end;

imptype function TWin32WSTrackBar.GetPosition(const ATrackBar: TCustomTrackBar): integer;
begin
  Result := SendMessage(ATrackBar.Handle, TBM_GETPOS, 0, 0);
  if (GetWindowLong(ATrackBar.Handle, GWL_STYLE) and TBS_REVERSED) <> 0 then
    Result := ATrackBar.Max + ATrackBar.Min - Result;
end;

imptype procedure TWin32WSTrackBar.SetPosition(const ATrackBar: TCustomTrackBar; const NewPosition: integer);
begin
  if (GetWindowLong(ATrackBar.Handle, GWL_STYLE) and TBS_REVERSED) <> 0 then
    Windows.SendMessage(ATrackBar.Handle, TBM_SETPOS, Windows.WPARAM(true), Windows.LPARAM(ATrackBar.Max + ATrackBar.Min - NewPosition))
  else
    Windows.SendMessage(ATrackBar.Handle, TBM_SETPOS, Windows.WPARAM(true), Windows.LPARAM(NewPosition));
end;

imptype procedure TWin32WSTrackBar.SetTick(const ATrackBar: TCustomTrackBar;
  const ATick: integer);
begin
  Windows.SendMessage(ATrackBar.Handle, TBM_SETTIC, 0, Windows.LPARAM(ATick));
end;

{$ifdef wsintf}
imptype procedure TWin32WSTrackBar.SetOrientation(const ATrackBar: TCustomTrackBar; const AOrientation: TTrackBarOrientation);
begin
end;

imptype procedure TWin32WSTrackBar.SetTickStyle(const ATrackBar: TCustomTrackBar; const ATickStyle: TTickStyle);
begin
end;
{$endif}

end.
