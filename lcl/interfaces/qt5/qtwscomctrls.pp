{
 *****************************************************************************
 *                              QtWSComCtrls.pp                              *
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
unit QtWSComCtrls;

{$mode objfpc}{$H+}

interface

{$I qtdefines.inc}

uses
  // Bindings
  qt5,
  qtwidgets, qtobjects, qtproc,
  // LCL
  SysUtils, Classes, Types, ComCtrls, Controls, LCLType, Graphics, StdCtrls,
  LCLProc, LCLIntf, Forms, ImgList,
  // Widgetset
  WSProc, WSComCtrls, {$ifndef wsintf}WSLCLClasses{$else}QtWSControls, WSLCLClasses_Intf{$endif};

type
  { TQtWSCustomPage }

  TQtWSCustomPage = class({$ifndef wsintf}TWSCustomPage{$else}TQtWSWinControl, IWSCustomPage{$endif})
  protected
    class procedure UpdateTabFontColor(APage: TCustomPage; AFont: TFont);
  impsection
    imptype function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): TLCLIntfHandle; override;
    imptype procedure DestroyHandle(const AWinControl: TWinControl); override;
    imptype procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
    imptype procedure UpdateProperties(const ACustomPage: TCustomPage); rootoverride;
  end;

  { TQtWSCustomTabControl }

  TQtWSCustomTabControl = class({$ifndef wsintf}TWSCustomTabControl{$else}TQtWSWinControl, IWSTabControl{$endif})
  impsection
    imptype function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): TLCLIntfHandle; override;
    imptype function GetDefaultClientRect(const AWinControl: TWinControl;
             const {%H-}aLeft, {%H-}aTop, aWidth, aHeight: integer; var aClientRect: TRect
             ): boolean; override;
    imptype procedure AddPage(const ATabControl: TCustomTabControl;
      const AChild: TCustomPage; const AIndex: integer); rootoverride;
    imptype procedure MovePage(const ATabControl: TCustomTabControl;
      const AChild: TCustomPage; const NewIndex: integer); rootoverride;
    imptype procedure RemovePage(const ATabControl: TCustomTabControl;
      const AIndex: integer); rootoverride;

    imptype function GetNotebookMinTabHeight(const AWinControl: TWinControl
      ): integer; rootoverride;
    imptype function GetNotebookMinTabWidth(const AWinControl: TWinControl
      ): integer; rootoverride;
    imptype function GetCapabilities: TCTabControlCapabilities; rootoverride;
    imptype function GetDesignInteractive(const AWinControl: TWinControl; AClientPos: TPoint): Boolean; override;
    imptype function GetTabIndexAtPos(const ATabControl: TCustomTabControl; const AClientPos: TPoint): integer; rootoverride;
    imptype function GetTabRect(const ATabControl: TCustomTabControl; const AIndex: Integer): TRect; rootoverride;
    imptype procedure SetPageIndex(const ATabControl: TCustomTabControl; const AIndex: integer); rootoverride;
    imptype procedure SetTabCaption(const ATabControl: TCustomTabControl; const AChild: TCustomPage; const AText: string); rootoverride;
    imptype procedure SetTabPosition(const ATabControl: TCustomTabControl; const ATabPosition: TTabPosition); rootoverride;
    imptype procedure SetTabSize(const ATabControl: TCustomTabControl; const ATabWidth, ATabHeight: integer); rootoverride;
    imptype procedure ShowTabs(const ATabControl: TCustomTabControl; AShowTabs: boolean); rootoverride;
    imptype procedure UpdateProperties(const ATabControl: TCustomTabControl); rootoverride;
    {$ifdef wsintf}
    imptype procedure SetImageList(const ATabControl: TCustomTabControl; const AImageList: TCustomImageListResolution); rootoverride;
    {$endif}
  end;

  { TQtWSStatusBar }

  TQtWSStatusBar = class({$ifndef wsintf}TWSStatusBar{$else}TQtWSWinControl, IWSStatusBar{$endif})
  protected
    class procedure ClearPanels(const Widget: TQtStatusBar);
    class procedure RecreatePanels(const AStatusBar: TStatusBar; const Widget: TQtStatusBar);
  impsection
    imptype function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    imptype procedure DestroyHandle(const AWinControl: TWinControl); override;
    imptype procedure PanelUpdate(const AStatusBar: TStatusBar; PanelIndex: integer); rootoverride;
    imptype procedure SetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer); rootoverride;
    imptype procedure SetSizeGrip(const AStatusBar: TStatusBar; SizeGrip: Boolean); rootoverride;
    imptype procedure Update(const AStatusBar: TStatusBar); rootoverride;
  end;

  { TQtWSTabSheet }

  TQtWSTabSheet = class(TWSTabSheet)
  published
  end;

  { TQtWSPageControl }

  TQtWSPageControl = class(TWSPageControl)
  published
  end;

  { TQtWSCustomListView }

  TQtWSCustomListView = class({$ifndef wsintf}TWSCustomListView{$else}TQtWSWinControl, IWSCustomListView{$endif})
  protected
    imptype function IsIconView(const AList: TCustomListView): boolean;
    imptype procedure InternalUpdateItems(const AList: TCustomListView);
    imptype procedure GetCurrentImages(const ALV: TCustomListView;
      out AImgListRes: TScaledImageListResolution);
  impsection
    imptype function CreateHandle(const AWinControl: TWinControl;
     const AParams: TCreateParams): TLCLIntfHandle; override;
    imptype procedure ColumnDelete(const ALV: TCustomListView; const AIndex: Integer); rootoverride;
    imptype procedure ColumnInsert(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn); rootoverride;
    imptype function  ColumnGetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn): Integer; rootoverride;
    imptype procedure ColumnSetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AWidth: Integer); rootoverride;
    imptype procedure ColumnSetVisible(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AVisible: Boolean); rootoverride;
    imptype procedure ColumnSetAlignment(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AAlignment: TAlignment); rootoverride;
    imptype procedure ColumnSetAutoSize(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AAutoSize: Boolean); rootoverride;
    imptype procedure ColumnSetCaption(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const ACaption: String); rootoverride;
    imptype procedure ColumnSetImage(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AImageIndex: Integer); rootoverride;

    imptype procedure ColumnSetMinWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AMinWidth: integer); rootoverride;
    imptype procedure ColumnMove(const ALV: TCustomListView; const AOldIndex, ANewIndex: Integer; const AColumn: TListColumn); rootoverride;

    imptype procedure ColumnSetSortIndicator(const ALV: TCustomListView; const AIndex: Integer;
      const AColumn: TListColumn; const ASortIndicator: TSortIndicator);
      rootoverride;

    {items}
    imptype procedure ItemInsert(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem); rootoverride;
    imptype procedure ItemDelete(const ALV: TCustomListView; const AIndex: Integer); rootoverride;
    imptype procedure ItemExchange(const ALV: TCustomListView; AItem: TListItem; const AIndex1, AIndex2: Integer); rootoverride;
    imptype procedure ItemMove(const ALV: TCustomListView; AItem: TListItem; const AFromIndex, AToIndex: Integer); rootoverride;
    imptype function  ItemGetChecked(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem): Boolean; rootoverride;
    imptype procedure ItemSetChecked(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AChecked: Boolean); rootoverride;
    imptype function  ItemGetPosition(const ALV: TCustomListView; const AIndex: Integer): TPoint; rootoverride;
    imptype function  ItemGetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; out AIsSet: Boolean): Boolean; rootoverride; // returns True if supported
    imptype procedure ItemSetImage(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex, AImageIndex: Integer); rootoverride;
    imptype procedure ItemSetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; const AIsSet: Boolean); rootoverride;
    imptype procedure ItemSetStateImage(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex, AStateImageIndex: Integer); rootoverride;
    imptype procedure ItemSetText(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex: Integer; const AText: String); rootoverride;
    imptype procedure ItemShow(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const PartialOK: Boolean); rootoverride;
    imptype function  ItemDisplayRect(const ALV: TCustomListView; const AIndex, ASubItem: Integer; ACode: TDisplayCode): TRect; rootoverride;

    {parent}
    imptype procedure BeginUpdate(const ALV: TCustomListView); rootoverride;
    imptype procedure EndUpdate(const ALV: TCustomListView); rootoverride;

    imptype function GetFocused(const ALV: TCustomListView): Integer; rootoverride;
    imptype function GetHitTestInfoAt( const ALV: TCustomListView; X, Y: Integer ) : THitTests; rootoverride;
    imptype function GetItemAt(const ALV: TCustomListView; x,y: integer): Integer; rootoverride;
    imptype function GetSelCount(const ALV: TCustomListView): Integer; rootoverride;
    imptype function GetSelection(const ALV: TCustomListView): Integer; rootoverride;
    imptype function GetTopItem(const ALV: TCustomListView): Integer; rootoverride;
    imptype procedure SetSort(const ALV: TCustomListView; const AType: TSortType; const AColumn: Integer;
      const ASortDirection: TSortDirection); rootoverride;

    imptype function GetBoundingRect(const ALV: TCustomListView): TRect; rootoverride;
    imptype function GetViewOrigin(const ALV: TCustomListView): TPoint; rootoverride;
    imptype function GetVisibleRowCount(const ALV: TCustomListView): Integer; rootoverride;

    imptype procedure SelectAll(const ALV: TCustomListView; const AIsSet: Boolean); rootoverride;

    imptype procedure SetAllocBy(const ALV: TCustomListView; const AValue: Integer); rootoverride;
    imptype procedure SetIconArrangement(const ALV: TCustomListView; const AValue: TIconArrangement); rootoverride;
    imptype procedure SetImageList(const ALV: TCustomListView; const AList: TListViewImageList; const AValue: TCustomImageListResolution); rootoverride;
    imptype procedure SetItemsCount(const ALV: TCustomListView; const Avalue: Integer); rootoverride;
    imptype procedure SetOwnerData(const ALV: TCustomListView; const AValue: Boolean); rootoverride;

    imptype procedure SetProperty(const ALV: TCustomListView; const AProp: TListViewProperty; const AIsSet: Boolean); rootoverride;
    imptype procedure SetProperties(const ALV: TCustomListView; const AProps: TListViewProperties); rootoverride;

    imptype procedure SetScrollBars(const ALV: TCustomListView; const AValue: TScrollStyle); rootoverride;
    imptype procedure SetViewStyle(const ALV: TCustomListView; const Avalue: TViewStyle); rootoverride;

    {$ifdef wsintf}
    // Column
    imptype procedure ColumnSetMaxWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AMaxWidth: Integer); rootoverride;
    // Item
    imptype function ItemSetPosition(const ALV: TCustomListView; const AIndex: Integer; const ANewPosition: TPoint): Boolean; rootoverride;
    imptype function ItemGetStates(const ALV: TCustomListView; const AIndex: Integer; out AStates: TListItemStates): Boolean; rootoverride;
    imptype procedure ItemUpdate(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem); rootoverride;
    imptype function GetNextItem(const ALV: TCustomListView; const StartItem: TListItem; const Direction: TSearchDirection; const States: TListItemStates): TListItem; rootoverride;
    // LV
    imptype function GetDropTarget(const ALV: TCustomListView): Integer; rootoverride;
    imptype function GetHoverTime(const ALV: TCustomListView): Integer; rootoverride;
    imptype procedure SetDefaultItemHeight(const ALV: TCustomListView; const AValue: Integer); rootoverride;
    imptype procedure SetHotTrackStyles(const ALV: TCustomListView; const AValue: TListHotTrackStyles); rootoverride;
    imptype procedure SetHoverTime(const ALV: TCustomListView; const AValue: Integer); rootoverride;
    imptype procedure SetImageList(const ALV: TCustomListView; const AList: TListViewImageList; const AValue: TCustomImageList); rootoverride;
    imptype procedure SetViewOrigin(const ALV: TCustomListView; const AValue: TPoint); rootoverride;
    imptype function RestoreItemCheckedAfterSort(const ALV: TCustomListView): Boolean; rootoverride;
    {$endif}
  end;

  { TQtWSListView }

  TQtWSListView = class(TWSListView)
  published
  end;

  { TQtWSProgressBar }

  TQtWSProgressBar = class({$ifndef wsnintf}TWSProgressBar{$else}TQtWSWinControl, IWSProgressBar{$endif})
  protected
    class procedure SetRangeStyle(AProgressBar: TQtProgressBar;
      AStyle: TProgressBarStyle; AMin, AMax: Integer; const AIsDesign: Boolean);
  impsection
    imptype function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    imptype procedure ApplyChanges(const AProgressBar: TCustomProgressBar); rootoverride;
    imptype procedure SetPosition(const AProgressBar: TCustomProgressBar; const NewPosition: integer); rootoverride;
    imptype procedure SetStyle(const AProgressBar: TCustomProgressBar; const NewStyle: TProgressBarStyle); rootoverride;
  end;

  { TQtWSCustomUpDown }

  TQtWSCustomUpDown = class(TWSCustomUpDown)
  published
  end;

  { TQtWSUpDown }

  TQtWSUpDown = class(TWSUpDown)
  published
  end;

  { TQtWSToolButton }

  TQtWSToolButton = class(TWSToolButton)
  published
  end;

  { TQtWSToolBar }

  TQtWSToolBar = class({$ifndef wsintf}TWSToolBar{$else}TQtWSWinControl{$endif})
  impsection
    imptype function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TQtWSTrackBar }

  TQtWSTrackBar = class({$ifndef wsintf}TWSTrackBar{$else}TQtWSWinControl,IWSTrackBar{$endif})
  impsection
    imptype function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    imptype procedure ApplyChanges(const ATrackBar: TCustomTrackBar); rootoverride;
    imptype function  GetPosition(const ATrackBar: TCustomTrackBar): integer; rootoverride;
    imptype procedure SetPosition(const ATrackBar: TCustomTrackBar; const NewPosition: integer); rootoverride;
    imptype procedure SetOrientation(const ATrackBar: TCustomTrackBar; const AOrientation: TTrackBarOrientation); rootoverride;
    {$ifdef wsintf}
    imptype procedure SetTick(const ATrackBar: TCustomTrackBar; const ATick: integer); rootoverride;
    imptype procedure SetTickStyle(const ATrackBar: TCustomTrackBar; const ATickStyle: TTickStyle); rootoverride;
    {$endif}
  end;

  { TQtWSCustomTreeView }

  TQtWSCustomTreeView = class(TWSCustomTreeView)
  published
  end;

  { TQtWSTreeView }

  TQtWSTreeView = class(TWSTreeView)
  published
  end;


implementation
uses qtint, math;

{$include qtpagecontrol.inc}

const
  TickMarkToQtSliderTickPositionMap: array[TTickMark] of QSliderTickPosition =
  (
{tmBottomRight} QSliderTicksBelow,
{tmTopLeft    } QSliderTicksAbove,
{tmBoth       } QSliderTicksBothSides
  );

  TrackBarOrientationToQtOrientationMap: array[TTrackBarOrientation] of QtOrientation =
  (
{trHorizontal} QtHorizontal,
{trVertical  } QtVertical
  );

  AlignmentToQtAlignmentMap: array[TAlignment] of QtAlignment =
  (
{taLeftJustify } QtAlignLeft,
{taRightJustify} QtAlignRight,
{taCenter      } QtAlignCenter
  );

  IconArngToQListFlow: array[TIconArrangement] of QListViewFlow =
  (
{iaTop} QListViewLeftToRight,
{iaLeft}QListViewTopToBottom
  );

{ TQtWSToolBar }

imptype function TQtWSToolBar.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  QtToolBar: TQtCustomControl;
begin
  {$note TToolBar implementation under LCL is wrong. TToolBar isn't
  TCustomControl but TWinControl.
  To avoid theoretical crashes we use TQtCustomControl here, but indeed it
  should be TQtWidget - so no viewport.}
  QtToolBar := TQtCustomControl.Create(AWinControl, AParams);
  QtToolBar.setFrameShape(QFrameNoFrame);
  QtToolBar.viewportNeeded;
  QtToolBar.setFocusPolicy(QtTabFocus);
  QtToolBar.AttachEvents;
  Result := TLCLIntfHandle(QtToolBar);
end;

{ TQtWSTrackBar }

imptype function TQtWSTrackBar.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  QtTrackBar: TQtTrackBar;
begin
  QtTrackBar := TQtTrackBar.Create(AWinControl, AParams);
  QtTrackBar.AttachEvents;

  Result := TLCLIntfHandle(QtTrackBar);
end;

function TrackBarReversed(const ATrackBar: TCustomTrackBar;
  const AQtTrackBar: TQtTrackBar): Boolean;
begin
  Result :=
    ((ATrackBar.Orientation = trHorizontal) and
    (AQtTrackbar.getInvertedAppereance <> ATrackBar.Reversed))
    or
    ((ATrackBar.Orientation = trVertical) and
    (AQtTrackbar.getInvertedAppereance <> not ATrackBar.Reversed))
end;

imptype procedure TQtWSTrackBar.ApplyChanges(const ATrackBar: TCustomTrackBar);
var
  QtTrackBar: TQtTrackBar;
begin

  if not WSCheckHandleAllocated(ATrackBar, 'ApplyChanges') then
    Exit;

  QtTrackBar := TQtTrackBar(ATrackBar.Handle);

  QtTrackBar.BeginUpdate;
  try
    QtTrackBar.setRange(ATrackBar.Min, ATrackBar.Max);

    if ATrackBar.TickStyle = tsNone then
      QtTrackBar.SetTickPosition(QSliderNoTicks)
    else
      QtTrackBar.SetTickPosition(TickMarkToQtSliderTickPositionMap[ATrackBar.TickMarks]);

    if QtTrackBar.getPageStep <> ATrackBar.PageSize then
      QtTrackBar.setPageStep(ATrackBar.PageSize);
    if QtTrackBar.getSingleStep <> ATrackBar.LineSize then
      QtTrackBar.setSingleStep(ATrackBar.LineSize);
    if QtTrackBar.getTickInterval <> ATrackBar.Frequency then
      QtTrackBar.setTickInterval(ATrackBar.Frequency);
    if QtTrackBar.getSliderPosition <> ATrackBar.Position then
      QtTrackBar.setSliderPosition(ATrackBar.Position);

    if (QtTrackBar.getOrientation <>
      TrackBarOrientationToQtOrientationMap[ATrackBar.Orientation])
      or TrackBarReversed(ATrackBar, QtTrackBar) then
    begin
      QtTrackBar.Hide;
      QtTrackBar.setOrientation(TrackBarOrientationToQtOrientationMap[ATrackBar.Orientation]);
      if ATrackBar.Orientation = trHorizontal then
        QtTrackBar.setInvertedAppereance(ATrackBar.Reversed)
      else
        {make it delphi and msdn compatibile when vertical then 0 = top}
        QtTrackBar.setInvertedAppereance(not ATrackBar.Reversed);
      QtTrackBar.setInvertedControls(False);
      QtTrackBar.Show;
    end;
  finally
    QtTrackBar.EndUpdate;
  end;
end;

imptype function  TQtWSTrackBar.GetPosition(const ATrackBar: TCustomTrackBar): integer;
var
  QtTrackBar: TQtTrackBar;
begin
  Result := 0;
  if not WSCheckHandleAllocated(ATrackBar, 'GetPosition') then
    Exit;
  QtTrackBar := TQtTrackBar(ATrackBar.Handle);
  Result := QtTrackBar.getSliderPosition;
end;

imptype procedure TQtWSTrackBar.SetPosition(const ATrackBar: TCustomTrackBar; const NewPosition: integer);
var
  QtTrackBar: TQtTrackBar;
begin
  if not WSCheckHandleAllocated(ATrackBar, 'SetPosition') then
    Exit;
  QtTrackBar := TQtTrackBar(ATrackBar.Handle);
  QtTrackBar.BeginUpdate;
  try
    QtTrackBar.setSliderPosition(NewPosition);
  finally
    QtTrackBar.EndUpdate;
  end;
end;

imptype procedure TQtWSTrackBar.SetOrientation(const ATrackBar: TCustomTrackBar;
  const AOrientation: TTrackBarOrientation);
var
  QtTrackBar: TQtTrackBar;
begin
  if not WSCheckHandleAllocated(ATrackBar, 'SetOrientation') then
    Exit;
  QtTrackBar := TQtTrackBar(ATrackBar.Handle);
  QtTrackBar.BeginUpdate;
  try
    if (QtTrackBar.getOrientation <>
      TrackBarOrientationToQtOrientationMap[ATrackBar.Orientation])
      or TrackBarReversed(ATrackBar, QtTrackBar) then
    begin
      QtTrackBar.Hide;
      QtTrackBar.setOrientation(TrackBarOrientationToQtOrientationMap[ATrackBar.Orientation]);
      if ATrackBar.Orientation = trHorizontal then
        QtTrackBar.setInvertedAppereance(ATrackBar.Reversed)
      else
        {make it delphi and msdn compatibile when vertical then 0 = top}
        QtTrackBar.setInvertedAppereance(not ATrackBar.Reversed);
      QtTrackBar.setInvertedControls(False);
      QtTrackBar.Show;
    end;
  finally
    QtTrackBar.EndUpdate;
  end;
end;

{$ifdef wsintf}
imptype procedure TQtWSTrackBar.SetTick(const ATrackBar: TCustomTrackBar; const ATick: integer);
begin
end;

imptype procedure TQtWSTrackBar.SetTickStyle(const ATrackBar: TCustomTrackBar; const ATickStyle: TTickStyle);
begin
  RecreateWnd(ATrackBar);
end;

{$endif}

{ TQtWSProgressBar }

class procedure TQtWSProgressBar.SetRangeStyle(AProgressBar: TQtProgressBar;
  AStyle: TProgressBarStyle; AMin, AMax: Integer; const AIsDesign: Boolean);
begin
  if AStyle = pbstNormal then
  begin
    if (AMin = 0) and (AMax = 0) then
      AProgressBar.setRange(0, 1)
    else
      AProgressBar.setRange(AMin, AMax)
  end
  else
    AProgressBar.setRange(0, Integer(AIsDesign));
end;

imptype function TQtWSProgressBar.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  QtProgressBar: TQtProgressBar;
begin
  QtProgressBar := TQtProgressBar.Create(AWinControl, AParams);
  QtProgressBar.AttachEvents;
  Result := TLCLIntfHandle(QtProgressBar);
end;

imptype procedure TQtWSProgressBar.ApplyChanges(const AProgressBar: TCustomProgressBar);
var
  QtProgressBar: TQtProgressBar;
begin
  QtProgressBar := TQtProgressBar(AProgressBar.Handle);

  // AProgressBar.Smooth is not supported by qt

  case AProgressBar.Orientation of
    pbVertical:
      begin
        QtProgressBar.setOrientation(QtVertical);
        QtProgressBar.setInvertedAppearance(False);
      end;
    pbRightToLeft:
      begin
        QtProgressBar.setOrientation(QtHorizontal);
        QtProgressBar.setInvertedAppearance(True);
      end;
    pbTopDown:
      begin
        QtProgressBar.setOrientation(QtVertical);
        QtProgressBar.setInvertedAppearance(True);
      end;
  else { pbHorizontal is default }
    begin
      QtProgressBar.setOrientation(QtHorizontal);
      QtProgressBar.setInvertedAppearance(False);
    end;
  end;

  QtProgressBar.setTextVisible(AProgressBar.BarShowText);

  // The position, minumum and maximum values
  SetRangeStyle(QtProgressBar, AProgressBar.Style,
    AProgressBar.Min, AProgressBar.Max,
    csDesigning in AProgressBar.ComponentState);
  QtProgressBar.BeginUpdate;
  QtProgressBar.setValue(AProgressBar.Position);
  QtProgressBar.EndUpdate;
end;

imptype procedure TQtWSProgressBar.SetPosition(const AProgressBar: TCustomProgressBar; const NewPosition: integer);
begin
  TQtProgressBar(AProgressBar.Handle).BeginUpdate;
  TQtProgressBar(AProgressBar.Handle).setValue(NewPosition);
  TQtProgressBar(AProgressBar.Handle).EndUpdate;
end;

imptype procedure TQtWSProgressBar.SetStyle(
  const AProgressBar: TCustomProgressBar; const NewStyle: TProgressBarStyle);
var
  QProgressBar: TQtProgressBar;
begin
  if not WSCheckHandleAllocated(AProgressBar, 'SetStyle') then
    Exit;
  QProgressBar := TQtProgressBar(AProgressBar.Handle);
  QProgressBar.reset;
  SetRangeStyle(QProgressBar, NewStyle, AProgressBar.Min, AProgressBar.Max,
    csDesigning in AProgressBar.ComponentState);
  if NewStyle = pbstNormal then
    QProgressBar.setValue(AProgressBar.Position);
end;

{ TQtWSStatusBar }

class procedure TQtWSStatusBar.ClearPanels(const Widget: TQtStatusBar);
var
  i: integer;
begin
  if length(Widget.Panels) > 0 then
  begin
    Widget.setUpdatesEnabled(False);
    for i := High(Widget.Panels) downto 0 do
    begin
      Widget.removeWidget(Widget.Panels[i].Widget);
      Widget.Panels[i].DetachEvents;
      QLabel_destroy(QLabelH(Widget.Panels[i].Widget));
      Widget.Panels[i].Widget := nil;
      Widget.Panels[i].Free;
    end;
    Widget.setUpdatesEnabled(True);
    SetLength(Widget.Panels, 0);
  end;
end;

class procedure TQtWSStatusBar.RecreatePanels(const AStatusBar: TStatusBar;
  const Widget: TQtStatusBar);
var
  Str: WideString;
  i: Integer;
begin
  // issues #18683 and #28307
  QStatusBar_clearMessage(QStatusBarH(Widget.Widget));
  ClearPanels(Widget);
  if AStatusBar.SimplePanel then
  begin
    Str := GetUtf8String(AStatusBar.SimpleText);
    Widget.showMessage(@Str);
  end else
  if AStatusBar.Panels.Count > 0 then
  begin
    Widget.setUpdatesEnabled(False);
    SetLength(Widget.Panels, AStatusBar.Panels.Count);
    for i := 0 to AStatusBar.Panels.Count - 1 do
    begin
      Str := GetUtf8String(AStatusBar.Panels[i].Text);
      Widget.Panels[i] := TQtStatusBarPanel.CreateFrom(AStatusBar,
        QLabel_create(@Str, Widget.Widget));
      Widget.Panels[i].HasPaint := AStatusBar.Panels[i].Style = psOwnerDraw;
      Widget.Panels[i].ID := AStatusBar.Panels[i].ID;
      QLabel_setText(QLabelH(Widget.Panels[i].Widget), @Str);
      QLabel_setAlignment(QLabelH(Widget.Panels[i].Widget),
        AlignmentToQtAlignmentMap[AStatusBar.Panels[i].Alignment]);
      QWidget_setMinimumWidth(Widget.Panels[i].Widget, Max(0, AStatusBar.Panels[i].Width));
      QWidget_setVisible(Widget.Panels[i].Widget,
        AStatusBar.Panels[i].Width > 0);
      Widget.Panels[i].AttachEvents;
      Widget.addWidget(Widget.Panels[i].Widget, ord(i = AStatusBar.Panels.Count - 1));
    end;
    Widget.setUpdatesEnabled(True);
  end;
end;

imptype function TQtWSStatusBar.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  QtStatusBar: TQtStatusBar;
begin
  QtStatusBar := TQtStatusBar.Create(AWinControl, AParams);
  QtStatusBar.setSizeGripEnabled(TStatusBar(AWinControl).SizeGrip and
    TStatusBar(AWinControl).SizeGripEnabled);

  RecreatePanels(TStatusBar(AWinControl), QtStatusBar);

  QtStatusBar.AttachEvents;

  // Return handle

  Result := TLCLIntfHandle(QtStatusBar);
end;

imptype procedure TQtWSStatusBar.DestroyHandle(const AWinControl: TWinControl);
var
  QtStatusBar: TQtStatusBar;
begin
  QtStatusBar := TQtStatusBar(AWinControl.Handle);
  ClearPanels(QtStatusBar);
  QtStatusBar.Release;
end;

imptype procedure TQtWSStatusBar.PanelUpdate(const AStatusBar: TStatusBar; PanelIndex: integer);
var
  QtStatusBar: TQtStatusBar;
  Str: Widestring;
begin
  QtStatusBar := TQtStatusBar(AStatusBar.Handle);
  if AStatusBar.SimplePanel then
  begin
    ClearPanels(QtStatusBar);
    Str := GetUtf8String(AStatusBar.SimpleText);
    QtStatusBar.showMessage(@Str);
  end else
  if AStatusBar.Panels.Count > 0 then
  begin
    QStatusBar_clearMessage(QStatusBarH(QtStatusBar.Widget));

    if (PanelIndex >= Low(QtStatusBar.Panels)) and
      (PanelIndex <= High(QtStatusBar.Panels)) then
    begin
      Str := GetUtf8String(AStatusBar.Panels[PanelIndex].Text);
      QLabel_setText(QLabelH(QtStatusBar.Panels[PanelIndex].Widget), @Str);
      QLabel_setAlignment(QLabelH(QtStatusBar.Panels[PanelIndex].Widget),
        AlignmentToQtAlignmentMap[AStatusBar.Panels[PanelIndex].Alignment]);
      QWidget_setMinimumWidth(QtStatusBar.Panels[PanelIndex].Widget,
        Max(0, AStatusBar.Panels[PanelIndex].Width));
      QWidget_setVisible(QtStatusBar.Panels[PanelIndex].Widget,
        AStatusBar.Panels[PanelIndex].Width > 0);
    end;
  end;
end;

imptype procedure TQtWSStatusBar.SetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer);
var
  QtStatusBar: TQtStatusBar;
  Str: Widestring;
begin
  QtStatusBar := TQtStatusBar(AStatusBar.Handle);
  if AStatusBar.SimplePanel then
  begin
    Str := GetUtf8String(AStatusBar.SimpleText);
    QtStatusBar.showMessage(@Str);
  end else
  begin
    if (PanelIndex >= Low(QtStatusBar.Panels)) and
      (PanelIndex <= High(QtStatusBar.Panels)) then
    begin
      Str := GetUtf8String(AStatusBar.Panels[PanelIndex].Text);
      QLabel_setText(QLabelH(QtStatusBar.Panels[PanelIndex].Widget), @Str);
    end;
  end;
end;

imptype procedure TQtWSStatusBar.SetSizeGrip(const AStatusBar: TStatusBar;
  SizeGrip: Boolean);
var
  QtStatusBar: TQtStatusBar;
begin
  if not WSCheckHandleAllocated(AStatusBar, 'SetSizeGrip') then
    Exit;
  QtStatusBar := TQtStatusBar(AStatusBar.Handle);
  QtStatusBar.setSizeGripEnabled(SizeGrip and AStatusBar.SizeGripEnabled);
end;

imptype procedure TQtWSStatusBar.Update(const AStatusBar: TStatusBar);
var
  QtStatusBar: TQtStatusBar;
begin
  QtStatusBar := TQtStatusBar(AStatusBar.Handle);
  RecreatePanels(AStatusBar, QtStatusBar);
end;

{ TQtWSCustomListView }

type
  TCustomListViewHack = class(TCustomListView);

imptype function TQtWSCustomListView.IsIconView(const AList: TCustomListView): boolean;
begin
  Result := TCustomListViewHack(AList).ViewStyle <> vsReport;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.CreateHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
imptype function TQtWSCustomListView.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  QtTreeWidget: TQtTreeWidget;
  QtListWidget: TQtListWidget;
  ALV: TCustomListViewHack;
begin
  ALV := TCustomListViewHack(AWinControl);
  if IsIconView(TCustomListView(AWinControl)) then
  begin
    QtListWidget := TQtListWidget.Create(AWinControl, AParams);
    QtListWidget.ViewStyle := Ord(TCustomListViewHack(AWinControl).ViewStyle);
    if TCustomListViewHack(AWinControl).ViewStyle in [vsIcon, vsSmallIcon] then
    begin
      // emabarcadero docs says
      // vsIcon, vsSmallIcon
      // Each item appears as a full-sized icon with a label below it.
      // The user can drag the items to any location in the list view window.
      QtListWidget.setViewMode(QListViewIconMode);
      QtListWidget.setResizeMode(QListViewAdjust);
      QtListWidget.setMovement(QListViewFree);

      with TCustomListView(AWinControl) do
      begin
        QtListWidget.setWrapping(IconOptions.AutoArrange);
        QtListWidget.setViewFlow(IconArngToQListFlow[IconOptions.Arrangement]);
        QtListWidget.setWordWrap(IconOptions.WrapText);
      end;

    end else
      QtListWidget.setViewMode(QListViewListMode);

    QtListWidget.Checkable := TCustomListView(AWinControl).Checkboxes;
    QtListWidget.OwnerDrawn := ALV.IsCustomDrawn(dtControl, cdPrePaint);
    QtListWidget.AttachEvents;
    Result := TLCLIntfHandle(QtListWidget);
  end else
  begin
    QtTreeWidget := TQtTreeWidget.Create(AWinControl, AParams);
    QtTreeWidget.ViewStyle := Ord(TCustomListViewHack(AWinControl).ViewStyle);
    QtTreeWidget.OwnerDrawn := ALV.IsCustomDrawn(dtControl, cdPrePaint) or (ALV.OwnerDraw and
      (ALV.ViewStyle = vsReport));
    QtTreeWidget.setStretchLastSection(ALV.AutoWidthLastColumn);
    QTreeView_setItemsExpandable(QTreeViewH(QtTreeWidget.Widget), False);
    QtTreeWidget.setRootIsDecorated(False);
    QtTreeWidget.AttachEvents;
    Result := TLCLIntfHandle(QtTreeWidget);
  end;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ColumnDelete
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
imptype procedure TQtWSCustomListView.ColumnDelete(const ALV: TCustomListView;
  const AIndex: Integer);
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnDelete') then
    Exit;

  // TODO: columns in vsIcon mode
  if IsIconView(ALV) then
    exit;

  // we must recreate handle since there's no column removal support
  // in our bindings (protected methods in qt).
  RecreateWnd(ALV);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ColumnInsert
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
imptype procedure TQtWSCustomListView.ColumnInsert(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn);
var
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
  TWIChild: QTreeWidgetItemH;
  Str: WideString;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnInsert') then
    Exit;

  // TODO: columns in vsIcon mode
  if IsIconView(ALV) then
    exit;

  QtTreeWidget := TQtTreeWidget(ALV.Handle);

  if QtTreeWidget.ColCount <> TCustomListViewHack(ALV).Columns.Count then
   	QtTreeWidget.ColCount := TCustomListViewHack(ALV).Columns.Count;

  if (QtTreeWidget.ColCount <= 1) and TCustomListViewHack(ALV).ShowColumnHeaders then
    QtTreeWidget.setHeaderVisible(True);

  TWI := QtTreeWidget.headerItem;

  if QTreeWidgetItem_childCount(TWI) < (AIndex + 1) then
  begin
    TWIChild := QTreeWidgetItem_create(Ord(QTreeWidgetItemType));
    QTreeWidgetItem_setFlags(TWIChild, QtItemIsEnabled);
    QTreeWidgetItem_addChild(TWI, TWIChild);
    Str := GetUtf8String(ALV.Column[AIndex].Caption);
    QTreeWidgetItem_setText(TWI, AIndex, @Str);
  end;

  if (csDesigning in ALV.ComponentState) then
    exit;

	QtTreeWidget.Header.Clickable := TCustomListViewHack(ALV).ColumnClick;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ColumnGetWidth
  Params:  None
  Returns: Integer
 ------------------------------------------------------------------------------}
imptype function  TQtWSCustomListView.ColumnGetWidth(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn): Integer;
var
  QtTreeWidget: TQtTreeWidget;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnGetWidth') then
    Exit(-1);

  // TODO: columns in vsIcon mode
  if IsIconView(ALV) then
    exit(0);

  QtTreeWidget := TQtTreeWidget(ALV.Handle);
  Result := QtTreeWidget.ColWidth[AIndex];
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ColumnMove
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
imptype procedure TQtWSCustomListView.ColumnMove(const ALV: TCustomListView;
  const AOldIndex, ANewIndex: Integer; const AColumn: TListColumn);
var
  QtTreeWidget: TQtTreeWidget;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnMove') then
    Exit;

  if (csDesigning in ALV.ComponentState) then
    exit;

  // TODO: columns in vsIcon mode
  if IsIconView(ALV) then
    exit;

  QtTreeWidget := TQtTreeWidget(ALV.Handle);
  QtTreeWidget.Header.moveSection(AOldIndex, ANewIndex);
end;

imptype procedure TQtWSCustomListView.ColumnSetSortIndicator(
  const ALV: TCustomListView; const AIndex: Integer;
  const AColumn: TListColumn; const ASortIndicator: TSortIndicator);
const
  QtSortOrder : array [TSortIndicator] of QtSortOrder = (QtAscendingOrder, QtAscendingOrder, QtDescendingOrder);
var
  QtTreeWidget: TQtTreeWidget;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetSortIndicator') then
    Exit;

  if IsIconView(ALV) then
    exit;

  QtTreeWidget := TQtTreeWidget(ALV.Handle);
  if Assigned(QtTreeWidget) then
  begin
    if ASortIndicator = siNone then
      QtTreeWidget.Header.SetSortIndicatorVisible(false)
    else
    begin
      QtTreeWidget.Header.SetSortIndicatorVisible(true);
      QtTreeWidget.Header.SetSortIndicator(AIndex, QtSortOrder[ASortIndicator]);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ColumnSetAlignment
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
imptype procedure TQtWSCustomListView.ColumnSetAlignment(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AAlignment: TAlignment);
var
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
  i: Integer;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetAlignment') then
    Exit;

  // TODO: columns in vsIcon mode
  if IsIconView(ALV) then
    exit;

  QtTreeWidget := TQtTreeWidget(ALV.Handle);
  TWI := QtTreeWidget.headerItem;
  QTreeWidgetItem_setTextAlignment(TWI, AIndex,
    AlignmentToQtAlignmentMap[AAlignment]);


  if not (csLoading in ALV.ComponentState) then
    for i := 0 to QtTreeWidget.ItemCount - 1 do
    begin
      TWI := QtTreeWidget.topLevelItem(i);
      if TWI <> nil then
        QTreeWidgetItem_setTextAlignment(TWI, AIndex,
          AlignmentToQtAlignmentMap[AAlignment]);
    end;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ColumnSetAutoSize
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
imptype procedure TQtWSCustomListView.ColumnSetAutoSize(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AAutoSize: Boolean);
var
  QtTreeWidget: TQtTreeWidget;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetAutoSize') then
    Exit;

  if (csDesigning in ALV.ComponentState) then
    exit;

  // TODO: columns in vsIcon mode
  if IsIconView(ALV) then
    exit;

  QtTreeWidget := TQtTreeWidget(ALV.Handle);
  if AAutoSize then
    QtTreeWidget.Header.setResizeMode(AIndex, QHeaderViewResizeToContents)
  else
    QtTreeWidget.Header.setResizeMode(AIndex, QHeaderViewInteractive);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ColumnSetCaption
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
imptype procedure TQtWSCustomListView.ColumnSetCaption(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const ACaption: String);
var
  Str: WideString;
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetCaption') then
    Exit;

  // TODO: columns in vsIcon mode
  if IsIconView(ALV) then
    exit;

  QtTreeWidget := TQtTreeWidget(ALV.Handle);
  TWI := QtTreeWidget.headerItem;
  if TWI <> NiL then
  begin
    Str := GetUtf8String(ACaption);
    QTreeWidgetItem_setText(TWI, AIndex, @Str);
  end;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ColumnSetImage
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
imptype procedure TQtWSCustomListView.ColumnSetImage(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AImageIndex: Integer);
var
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
  Bmp: TBitmap;
  ImgListRes: TScaledImageListResolution;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetImage') then
    Exit;

  // TODO: columns in vsIcon mode
  if IsIconView(ALV) then
    exit;

  QtTreeWidget := TQtTreeWidget(ALV.Handle);
  TWI := QtTreeWidget.headerItem;
  if TWI <> NiL then
  begin
    GetCurrentImages(ALV, ImgListRes);
    if ImgListRes.Valid and (ImgListRes.Count > 0) and
      ((AImageIndex >= 0) and (AImageIndex < ImgListRes.Count)) then
    begin
      Bmp := TBitmap.Create;
      try
        ImgListRes.GetBitmap(AImageIndex, Bmp);
        QTreeWidgetItem_setIcon(TWI, AIndex, TQtImage(Bmp.Handle).AsIcon);
      finally
        Bmp.Free;
      end;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ColumnSetMinWidth
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
imptype procedure TQtWSCustomListView.ColumnSetMinWidth(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AMinWidth: integer);
var
  QtTreeWidget: TQtTreeWidget;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetMinWidth') then
    Exit;

  // TODO: columns in vsIcon mode
  if IsIconView(ALV) then
    exit;

  QtTreeWidget := TQtTreeWidget(ALV.Handle);
  QtTreeWidget.MinColSize[AIndex] := AMinWidth;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ColumnSetWidth
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
imptype procedure TQtWSCustomListView.ColumnSetWidth(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AWidth: Integer);
var
  QtTreeWidget: TQtTreeWidget;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetWidth') then
    Exit;

  // TODO: columns in vsIcon mode
  if IsIconView(ALV) then
    exit;

  QtTreeWidget := TQtTreeWidget(ALV.Handle);
  QtTreeWidget.ColWidth[AIndex] := AWidth;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ColumnSetVisible
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
imptype procedure TQtWSCustomListView.ColumnSetVisible(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AVisible: Boolean);
var
  QtTreeWidget: TQtTreeWidget;
begin
  if not WSCheckHandleAllocated(ALV, 'ColumnSetVisible') then
    Exit;

  // TODO: columns in vsIcon mode
  if IsIconView(ALV) then
    exit;

  QtTreeWidget := TQtTreeWidget(ALV.Handle);
  QtTreeWidget.ColVisible[AIndex] := AVisible;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ItemDelete
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
imptype procedure TQtWSCustomListView.ItemDelete(const ALV: TCustomListView;
  const AIndex: Integer);
var
  QtTreeWidget: TQtTreeWidget;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemDelete') then
    Exit;
  if IsIconView(ALV) then
    TQtListWidget(ALV.Handle).removeItem(AIndex)
  else
  begin
    TQtListWidget(ALV.Handle).BeginUpdate;
    try
      QtTreeWidget := TQtTreeWidget(ALV.Handle);
      QtTreeWidget.DeleteItem(AIndex);
    finally
      TQtListWidget(ALV.Handle).EndUpdate;
    end;
  end;
end;

imptype procedure TQtWSCustomListView.ItemExchange(const ALV: TCustomListView;
  AItem: TListItem; const AIndex1, AIndex2: Integer);
var
  QtTreeWidget: TQtTreeWidget;
  QtListWidget: TQtListWidget;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemExchange') then
    Exit;
  if IsIconView(ALV) then
  begin
    QtListWidget := TQtListWidget(ALV.Handle);
    QtListWidget.BeginUpdate;
    QtListWidget.ExchangeItems(AIndex1, AIndex2);
    QtListWidget.EndUpdate;
  end else
  begin
    QtTreeWidget := TQtTreeWidget(ALV.Handle);
    QtTreeWidget.BeginUpdate;
    QtTreeWidget.ExchangeItems(AIndex1, AIndex2);
    QtTreeWidget.EndUpdate;
  end;
end;

imptype procedure TQtWSCustomListView.ItemMove(const ALV: TCustomListView;
  AItem: TListItem; const AFromIndex, AToIndex: Integer);
var
  QtTreeWidget: TQtTreeWidget;
  QtListWidget: TQtListWidget;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemMove') then
    Exit;
  if IsIconView(ALV) then
  begin
    QtListWidget := TQtListWidget(ALV.Handle);
    QtListWidget.BeginUpdate;
    QtListWidget.MoveItem(AFromIndex, AToIndex);
    QtListWidget.EndUpdate;
  end else
  begin
    QtTreeWidget := TQtTreeWidget(ALV.Handle);
    QtTreeWidget.BeginUpdate;
    QtTreeWidget.MoveItem(AFromIndex, AToIndex);
    QtTreeWidget.EndUpdate;
  end;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ItemGetChecked
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
imptype function  TQtWSCustomListView.ItemGetChecked(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem): Boolean;
var
  QtTreeWidget: TQtTreeWidget;
  LWI: QListWidgetItemH;
  AState: QtCheckState;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemGetChecked') then
    Exit(False);

  Result := ALV.CheckBoxes;
  if not Result then
    exit;

  if IsIconView(ALV) then
  begin
    AState := QtUnChecked;
    LWI := TQtListWidget(ALV.Handle).getItem(AIndex);
    if LWI <> nil then
      AState := TQtListWidget(ALV.Handle).GetItemLastCheckState(LWI);
    Result := AState = QtChecked;
  end else
  begin
    QtTreeWidget := TQtTreeWidget(ALV.Handle);
    Result := QtTreeWidget.ItemChecked[AIndex];
  end;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ItemGetPosition
  Params:  None
  Returns: TPoint
 ------------------------------------------------------------------------------}
imptype function  TQtWSCustomListView.ItemGetPosition(const ALV: TCustomListView;
  const AIndex: Integer): TPoint;
var
  QtListWidget: TQtListWidget;
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
  R: TRect;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemGetPosition') then
    Exit(Point(-1,-1));

  R := Rect(0, 0, 0, 0);
  if IsIconView(ALV) then
  begin
    QtListWidget := TQtListWidget(ALV.Handle);
    R := QtListWidget.getVisualItemRect(QtListWidget.getItem(AIndex));
  end else
  begin
    QtTreeWidget := TQtTreeWidget(ALV.Handle);
    TWI := QtTreeWidget.topLevelItem(AIndex);
    R := QtTreeWidget.visualItemRect(TWI);
  end;
  Result.X := R.Left;
  Result.Y := R.Top;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ItemGetState
  Params:  None
  Returns: TPoint
 ------------------------------------------------------------------------------}
imptype function  TQtWSCustomListView.ItemGetState(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem;
  const AState: TListItemState; out AIsSet: Boolean): Boolean;
var
  QtListWidget: TQtListWidget;
  LWI: QListWidgetItemH;
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
  i: Integer;
  Arr: TPtrIntArray;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemGetState') then
    Exit(False);

  AIsSet := False;
  if IsIconView(ALV) then
  begin
    QtListWidget := TQtListWidget(ALV.Handle);
    LWI := QtListWidget.getItem(AIndex);
    if LWI <> nil then
      case AState of
        lisFocused: AIsSet := LWI = QtListWidget.currentItem;
        lisSelected: AIsSet := QtListWidget.getItemSelected(LWI);
      end;
  end else
  begin
    QtTreeWidget := TQtTreeWidget(ALV.Handle);
    TWI := QtTreeWidget.topLevelItem(AIndex);
    if TWI <> nil then
    begin
      case AState of
        lisFocused: AIsSet := TWI = QtTreeWidget.currentItem;
        lisSelected:
        begin
          Arr := QtTreeWidget.selectedItems;
          for i := 0 to High(Arr) do
          begin
            TWI := QTreeWidgetItemH(Arr[i]);
            if AIndex = QtTreeWidget.getRow(TWI) then
            begin
              AIsSet := True;
              break;
            end;
          end;
        end;
      end;
    end;
  end;
  Result := True;

end;

imptype procedure TQtWSCustomListView.ItemSetImage(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const ASubIndex,
  AImageIndex: Integer);
var
  QtListWidget: TQtListWidget;
  LWI: QListWidgetItemH;
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
  Bmp: TBitmap;
  ImgListRes: TScaledImageListResolution;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemSetImage') then
    Exit;

  if not Assigned(TCustomListViewHack(ALV).LargeImages) and not
    Assigned(TCustomListViewHack(ALV).SmallImages) then
      exit;
  TWI := nil;
  LWI := nil;
  if IsIconView(ALV) then
  begin
    if ASubIndex > 0 then
      exit;
    QtListWidget := TQtListWidget(ALV.Handle);
    LWI := QtListWidget.getItem(AIndex);
  end else
  begin
    QtTreeWidget := TQtTreeWidget(ALV.Handle);
    TWI := QtTreeWidget.topLevelItem(AIndex);
  end;
  if (TWI <> nil) or (LWI <> nil) then
  begin
    GetCurrentImages(ALV, ImgListRes);

    if ImgListRes.Valid and
      ((AImageIndex >= 0) and (AImageIndex < ImgListRes.Count)) then
    begin
      Bmp := TBitmap.Create;
      try
        ImgListRes.GetBitmap(AImageIndex, Bmp);
        if LWI <> nil then
          QListWidgetItem_setIcon(LWI, TQtImage(Bmp.Handle).AsIcon)
        else
          QTreeWidgetItem_setIcon(TWI, ASubIndex, TQtImage(Bmp.Handle).AsIcon);
      finally
        Bmp.Free;
      end;
    end else
    begin
      if Assigned(TCustomListViewHack(ALV).StateImages) and (AItem.StateIndex >= 0) then
        exit;
      if LWI <> nil then
        QListWidgetItem_setIcon(LWI, nil)
      else
        QTreeWidgetItem_setIcon(TWI, ASubIndex, nil);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ItemSetChecked
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
imptype procedure TQtWSCustomListView.ItemSetChecked(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const AChecked: Boolean);
var
  QtListWidget: TQtListWidget;
  QtTreeWidget: TQtTreeWidget;
  B: Boolean;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemSetChecked') then
    Exit;

  if not ALV.CheckBoxes then
    exit;

  if IsIconView(ALV) then
  begin
    QtListWidget := TQtListWidget(ALV.Handle);
    B := QtListWidget.GetItemLastCheckState(QtListWidget.getItem(AIndex)) = QtChecked;
    if B <> AChecked then
      QtListWidget.ItemChecked[AIndex] := AChecked;
  end else
  begin
    QtTreeWidget := TQtTreeWidget(ALV.Handle);
    if QtTreeWidget.ItemChecked[AIndex] <> AChecked then
      QtTreeWidget.ItemChecked[AIndex] := AChecked;
  end;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ItemSetState
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
imptype procedure TQtWSCustomListView.ItemSetState(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem;
  const AState: TListItemState; const AIsSet: Boolean);
var
  QtListWidget: TQtListWidget;
  LWI: QListWidgetItemH;
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemSetState') then
    Exit;
  if IsIconView(ALV) then
  begin
    QtListWidget := TQtListWidget(ALV.Handle);
    LWI := QtListWidget.getItem(AIndex);
    QtListWidget.BeginUpdate;
    case AState of
      lisFocused: QtListWidget.setCurrentItem(LWI, AIsSet);
      lisSelected:
      begin
        if AIsSet and not ALV.MultiSelect then
          QtListWidget.setCurrentItem(LWI);
        QtListWidget.setItemSelected(LWI, AIsSet);
      end;
    end;
    QtListWidget.EndUpdate;
  end else
  begin
    QtTreeWidget := TQtTreeWidget(ALV.Handle);
    TWI := QtTreeWidget.topLevelItem(AIndex);
    QtTreeWidget.BeginUpdate;
    case AState of
      lisFocused: QtTreeWidget.setCurrentItem(TWI);
      lisSelected:
      begin
        if ALV.RowSelect and AIsSet and not ALV.MultiSelect then
          QtTreeWidget.setCurrentItem(TWI);
        QtTreeWidget.setItemSelected(TWI, AIsSet);
      end;
    end;
    QtTreeWidget.EndUpdate;
  end;
end;

imptype procedure TQtWSCustomListView.ItemSetStateImage(
  const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem;
  const ASubIndex, AStateImageIndex: Integer);
var
  QtListWidget: TQtListWidget;
  LWI: QListWidgetItemH;
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
  Bmp: TBitmap;
  ImgListRes: TScaledImageListResolution;
  AImgList: TCustomImageList;
  AImgListWidth: Integer;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemSetStateImage') then
    Exit;

  if not Assigned(TCustomListViewHack(ALV).StateImages) then
    exit;
  TWI := nil;
  LWI := nil;
  if IsIconView(ALV) then
  begin
    if ASubIndex > 0 then
      exit;
    QtListWidget := TQtListWidget(ALV.Handle);
    LWI := QtListWidget.getItem(AIndex);
  end else
  begin
    QtTreeWidget := TQtTreeWidget(ALV.Handle);
    TWI := QtTreeWidget.topLevelItem(AIndex);
  end;
  if (TWI <> nil) or (LWI <> nil) then
  begin
    AImgList := TCustomListViewHack(ALV).StateImages;
    AImgListWidth := TCustomListViewHack(ALV).StateImagesWidth;
    ImgListRes := AImgList.ResolutionForControl[AImgListWidth, ALV];

    if ImgListRes.Valid and
      ((AStateImageIndex >= 0) and (AStateImageIndex < ImgListRes.Count)) then
    begin
      Bmp := TBitmap.Create;
      try
        ImgListRes.GetBitmap(AStateImageIndex, Bmp);
        if LWI <> nil then
          QListWidgetItem_setIcon(LWI, TQtImage(Bmp.Handle).AsIcon)
        else
          QTreeWidgetItem_setIcon(TWI, ASubIndex, TQtImage(Bmp.Handle).AsIcon);
      finally
        Bmp.Free;
      end;
    end else
    begin
      if LWI <> nil then
        QListWidgetItem_setIcon(LWI, nil)
      else
        QTreeWidgetItem_setIcon(TWI, ASubIndex, nil);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ItemInsert
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
imptype procedure TQtWSCustomListView.ItemInsert(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem);
var
  QtListWidget: TQtListWidget;
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
  Str: WideString;
  i: Integer;
  AAlignment: QtAlignment;
  //AImages: TCustomImageList;
  //AMetric: Integer;
  //ASizeHint: TSize;
  //AIconWidth: Integer;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemInsert') then
    Exit;

  if IsIconView(ALV) then
  begin
    QtListWidget := TQtListWidget(ALV.Handle);
    QtListWidget.Checkable := ALV.Checkboxes;
    QtListWidget.insertItem(AIndex, AItem.Caption);
  end else
  begin
    QtTreeWidget := TQtTreeWidget(ALV.Handle);
    TWI := QTreeWidgetItem_create(Ord(QTreeWidgetItemType));
    if AItem.Caption <> '' then
      Str := GetUtf8String(AItem.Caption)
    else
      Str := '';

    if ALV.CheckBoxes then
    begin
      if AItem.Checked then
      	QTreeWidgetItem_setCheckState(TWI, 0, QtChecked)
      else
      	QTreeWidgetItem_setCheckState(TWI, 0, QtUnchecked);
    end;

    AAlignment := QtAlignLeft or QtAlignVCenter;
    if TCustomListViewHack(ALV).Columns.Count > 0 then
      AAlignment := AlignmentToQtAlignmentMap[ALV.Column[0].Alignment] or QtAlignVCenter;

    if Str <> '' then
      QtTreeWidget.setItemText(TWI, 0, Str, AAlignment);

    QtTreeWidget.setItemData(TWI, 0, AItem);

    for i := 0 to AItem.SubItems.Count - 1 do
    begin
      AAlignment := QtAlignLeft or QtAlignVCenter;
      if (TCustomListViewHack(ALV).Columns.Count > 0) and (i + 1 < TCustomListViewHack(ALV).Columns.Count) then
        AAlignment := AlignmentToQtAlignmentMap[ALV.Column[i + 1].Alignment] or QtAlignVCenter;
      if AItem.Subitems.Strings[i] <> '' then
      begin
        Str := GetUtf8String(AItem.Subitems.Strings[i]);
        QtTreeWidget.setItemText(TWI, i + 1, Str, AAlignment);
        QtTreeWidget.setItemData(TWI, i + 1, AItem);
      end;
    end;

    QtTreeWidget.insertTopLevelItem(AIndex, TWI);
  end;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ItemSetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
imptype procedure TQtWSCustomListView.ItemSetText(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const ASubIndex: Integer;
  const AText: String);
var
  QtListWidget: TQtListWidget;
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
  Str: WideString;
  AAlignment: QtAlignment;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemSetText') then
    Exit;

  if IsIconView(ALV) then
  begin
    if ASubIndex >0 Then exit;
    QtListWidget := TQtListWidget(ALV.Handle);
    if not QtListWidget.Checkable and (TCustomListViewHack(ALV).ViewStyle = vsIcon) then
      AAlignment := QtAlignHCenter
    else
      AAlignment := QtAlignLeft;
    if (TCustomListViewHack(ALV).Columns.Count > 0) and (ASubIndex < TCustomListViewHack(ALV).Columns.Count)  then
      AAlignment := AlignmentToQtAlignmentMap[ALV.Column[ASubIndex].Alignment];
    QtListWidget.setItemText(AIndex, AText, AAlignment);
  end else
  begin
    QtTreeWidget := TQtTreeWidget(ALV.Handle);
    Str := GetUtf8String(AText);
    TWI := QtTreeWidget.topLevelItem(AIndex);
    if TWI <> NiL then
    begin
      AAlignment := QtAlignLeft or QtAlignVCenter;
      if (TCustomListViewHack(ALV).Columns.Count > 0) and (ASubIndex < TCustomListViewHack(ALV).Columns.Count)  then
        AAlignment := AlignmentToQtAlignmentMap[ALV.Column[ASubIndex].Alignment]  or QtAlignVCenter;
      QtTreeWidget.setItemText(TWI, ASubIndex, Str, AAlignment);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ItemShow
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
imptype procedure TQtWSCustomListView.ItemShow(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const PartialOK: Boolean);
var
  QtListWidget: TQtListWidget;
  LWI: QListWidgetItemH;
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemShow') then
    Exit;
  if IsIconView(ALV) then
  begin
    QtListWidget := TQtListWidget(ALV.Handle);
    LWI := QtListWidget.getItem(AIndex);
    QtListWidget.setItemVisible(LWI, True);
    if not PartialOK then
      QtListWidget.scrollToItem(AIndex, QAbstractItemViewEnsureVisible);
  end else
  begin
    QtTreeWidget := TQtTreeWidget(ALV.Handle);
    TWI := QtTreeWidget.topLevelItem(AIndex);
    QtTreeWidget.setItemVisible(TWI, True);
    if not PartialOK then
      QtTreeWidget.scrollToItem(TWI, QAbstractItemViewEnsureVisible);
  end;
end;


{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.ItemDisplayRect
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
imptype function  TQtWSCustomListView.ItemDisplayRect(const ALV: TCustomListView;
  const AIndex, ASubItem: Integer; ACode: TDisplayCode): TRect;
var
  QtListWidget: TQtListWidget;
  LWI: QListWidgetItemH;
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
  Size: TSize;
  AIcon: QIconH;
  IconRect, ChkBoxRect: TRect;
  i: Integer;
  APixelMetric: Integer;
  ImgListRes: TScaledImageListResolution;
begin
  if not WSCheckHandleAllocated(ALV, 'ItemDisplayRect') then
    Exit;

  GetCurrentImages(ALV, ImgListRes);
  if IsIconView(ALV) then
  begin
    QtListWidget := TQtListWidget(ALV.Handle);
    LWI := QtListWidget.getItem(AIndex);
    Result := QtListWidget.getVisualItemRect(LWI);
    if TCustomListViewHack(ALV).ViewStyle = vsList then
    begin
      if ACode in [drBounds, drSelectBounds] then
        exit;
      APixelMetric := QStyle_pixelMetric(QApplication_style(), QStylePM_FocusFrameHMargin, nil, QtListWidget.Widget);
      IconRect := Result;
      // always get icon size
      AIcon := QIcon_create();
      try
        QListWidgetItem_icon(LWI, AIcon);
        if QIcon_isNull(AIcon) then
          IconRect := Rect(Result.Left, Result.Top, Result.Right, Result.Bottom)
        else
        begin
          Size.cx := 0;
          Size.cy := 0;
          QIcon_actualSize(AIcon, @Size, @Size);
          if (Size.cx = 0) or (Size.cy = 0) then
          begin
            if ImgListRes.Valid then
              IconRect.Right := IconRect.Left + ImgListRes.Width;
          end else
          begin
            IconRect.Right := IconRect.Left + Size.cx;
            IconRect.Bottom := IconRect.Top + Size.cy;
          end;
        end;
      finally
        QIcon_destroy(AIcon);
      end;

      IconRect.Left += APixelMetric;
      IconRect.Right += APixelMetric;

      if ACode = drLabel then
      begin
        inc(Result.Left, APixelMetric + (IconRect.Right - IconRect.Left));
        if (IconRect.Right - IconRect.Left > 0) then
          Result.Left += APixelMetric;
      end else
      if ACode in [drIcon] then
        Result := IconRect;
    end;
  end else
  begin
    QtTreeWidget := TQtTreeWidget(ALV.Handle);
    TWI := QtTreeWidget.topLevelItem(AIndex);
    if (QTreeWidgetItem_columnCount(TWI) > 1) and (ASubItem >= 0) then
    begin
      Result := QtTreeWidget.visualItemRect(TWI);
      Result.Left := QTreeView_columnViewportPosition(QTreeWidgetH(QtTreeWidget.Widget), ASubItem);
      Result.Right := QtTreeWidget.ColWidth[ASubItem] + Result.Left;
    end else
    begin
      Result := QtTreeWidget.visualItemRect(TWI);
    end;

    if ACode = drBounds then
    begin
      QWidget_rect(QtTreeWidget.viewportWidget, @IconRect);
      for i := ASubItem + 1 to QtTreeWidget.ColCount - 1 do
        Result.Right += QtTreeWidget.ColWidth[i];
      if Result.Right > IconRect.Right - IconRect.Left then
        Result.Right := IconRect.Right - IconRect.Left;
      exit;
    end else
    begin
      IconRect := Result;
      // always get icon size
      AIcon := QIcon_create();
      try
        QTreeWidgetItem_icon(TWI, AIcon, ASubItem);
        if QIcon_isNull(AIcon) then
          IconRect := Rect(0, 0, 0, 0)
        else
        begin
          Size.cx := 0;
          Size.cy := 0;
          QIcon_actualSize(AIcon, @Size, @Size);
          if (Size.cx = 0) or (Size.cy = 0) then
          begin
            if ImgListRes.Valid then
              IconRect.Right := IconRect.Left + ImgListRes.Width;
          end else
          begin
            IconRect.Right := IconRect.Left + Size.cx;
            IconRect.Bottom := IconRect.Top + Size.cy;
          end;
        end;
      finally
        QIcon_destroy(AIcon);
      end;
    end;

    ChkBoxRect := Rect(0, 0, 0, 0);
    if QtTreeWidget.Checkable then
    begin
      APixelMetric := QStyle_pixelMetric(QApplication_style(), QStylePM_IndicatorWidth, nil, QtTreeWidget.Widget);
      APixelMetric += QStyle_pixelMetric(QApplication_style(), QStylePM_CheckBoxLabelSpacing, nil, QtTreeWidget.Widget);
      ChkBoxRect := Rect(0, 0, APixelMetric, APixelMetric);
      if (ChkBoxRect.Bottom > Result.Bottom - Result.Top) then
        ChkBoxRect.Bottom := Result.Bottom - Result.Top;
    end;

    APixelMetric := QStyle_pixelMetric(QApplication_style(), QStylePM_FocusFrameHMargin, nil, QtTreeWidget.Widget);
    if ACode = drLabel then
    begin
      inc(Result.Left, APixelMetric + (IconRect.Right - IconRect.Left) + (ChkBoxRect.Right - ChkBoxRect.Left));
      if (IconRect.Right - IconRect.Left > 0) then
        Result.Left += APixelMetric;
    end else
    if ACode = drSelectBounds then
    begin
      Result.Left += APixelMetric + (ChkBoxRect.Right - ChkBoxRect.Left);
      Result.Right := Result.Left + (QtTreeWidget.ColWidth[ASubItem] - APixelMetric);
    end else
    if ACode in [drIcon] then
    begin
      if IsRectEmpty(IconRect) and ImgListRes.Valid and
        (QtTreeWidget.OwnerData or QtTreeWidget.OwnerDrawn) then
      begin
        IconRect := Rect(0, 0, ImgListRes.Width, ImgListRes.Height);
        OffsetRect(IconRect, Result.Left, Result.Top + APixelMetric);
      end;
      IconRect.Left += APixelMetric + (ChkBoxRect.Right - ChkBoxRect.Left);
      IconRect.Right += APixelMetric;
      Result := IconRect;
    end;
  end;
end;

imptype procedure TQtWSCustomListView.BeginUpdate(const ALV: TCustomListView);
var
  QtWidget: TQtWidget;
begin
  if not WSCheckHandleAllocated(ALV, 'BeginUpdate') then
    Exit;
  QtWidget := TQtWidget(ALV.Handle);
  if not QtWidget.InUpdate then
    QtWidget.setUpdatesEnabled(False);
  QtWidget.BeginUpdate;
end;

imptype procedure TQtWSCustomListView.EndUpdate(const ALV: TCustomListView);
var
  QtWidget: TQtWidget;
begin
  if not WSCheckHandleAllocated(ALV, 'EndUpdate') then
    Exit;
  QtWidget := TQtWidget(ALV.Handle);
  QtWidget.EndUpdate;
  if not QtWidget.InUpdate then
    QtWidget.setUpdatesEnabled(True);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.GetFocused
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
imptype function TQtWSCustomListView.GetFocused(const ALV: TCustomListView): Integer;
var
  QtListWidget: TQtListWidget;
  LWI: QListWidgetItemH;
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
  i: Integer;
begin
  if not WSCheckHandleAllocated(ALV, 'GetFocused') then
    Exit(-1);

  if IsIconView(ALV) then
  begin
    QtListWidget := TQtListWidget(ALV.Handle);
    LWI := QtListWidget.currentItem;
    if QtListWidget.getItemSelected(LWI) then
      Result := QtListWidget.getRow(LWI)
    else
      Result := -1;
  end else
  begin
    QtTreeWidget := TQtTreeWidget(ALV.Handle);
    TWI := QtTreeWidget.currentItem;
    i := QtTreeWidget.getRow(TWI);
    if QTreeWidgetItem_isSelected(TWI) then
      Result := i
    else
      Result := -1;
  end;
end;

imptype function TQtWSCustomListView.GetHitTestInfoAt(const ALV: TCustomListView;
  X, Y: Integer): THitTests;
var
  I, AImgListWidth, Ax: Integer;
  QtListWidget: TQtListWidget;
  QtTreeWidget: TQtTreeWidget;
  LWI: QListWidgetItemH;
  TWI: QTreeWidgetItemH;
  AImgList: TCustomImageList;
  AImgListRes: TScaledImageListResolution;
  AListSpacing, AFocusFrame: integer;
begin
  Result := [];
  if not WSCheckHandleAllocated(ALV, 'GetHitTestInfoAt') then
    Exit;
  I := GetItemAt(ALV, x, y);
  AFocusFrame := 2;
  AListSpacing := 4; {default, we are using pixelMetric() for real spacing}
  AX := 0;
  if I >= 0 then
  begin
    Include(Result, htOnItem);
    if Assigned(TCustomListViewHack(ALV).LargeImages) or Assigned(TCustomListViewHack(ALV).SmallImages) or
      Assigned(TCustomListViewHack(ALV).StateImages) then
    begin

      if IsIconView(ALV) then
      begin
        QtListWidget := TQtListWidget(ALV.Handle);
        LWI := QtListWidget.getItem(I);
        AFocusFrame := QStyle_pixelMetric(QApplication_style(), QStylePM_FocusFrameHMargin, nil, QtListWidget.Widget);
        AListSpacing := QStyle_pixelMetric(QApplication_style(), QStylePM_CheckBoxLabelSpacing, nil, QtListWidget.Widget);
        // AItemRect := QtListWidget.getVisualItemRect(LWI);
      end else
      begin
        QtTreeWidget := TQtTreeWidget(ALV.Handle);
        TWI := QtTreeWidget.topLevelItem(I);
        AFocusFrame := QStyle_pixelMetric(QApplication_style(), QStylePM_FocusFrameHMargin, nil, QtTreeWidget.Widget);
        AListSpacing := QStyle_pixelMetric(QApplication_style(), QStylePM_CheckBoxLabelSpacing, nil, QtTreeWidget.Widget);
        // AItemRect := QtTreeWidget.visualItemRect(TWI);
      end;

      if Assigned(TCustomListViewHack(ALV).StateImages) and (ALV.Items[I].StateIndex >= 0) then
      begin
        if Assigned(TWI) or Assigned(LWI) then
        begin
          AImgList := TCustomListViewHack(ALV).StateImages;
          AImgListWidth := TCustomListViewHack(ALV).StateImagesWidth;
          AImgListRes := AImgList.ResolutionForControl[AImgListWidth, ALV];
          if AImgListRes.Valid then
          begin
            if (x >= AListSpacing) and (x <= AImgListRes.Width + AFocusFrame) then
              include(Result, htOnStateIcon);
            Ax += AImgListRes.Width + AListSpacing;
          end;
        end;
      end;

      if Assigned(TCustomListViewHack(ALV).SmallImages) and (ALV.Items[I].ImageIndex >= 0) then
      begin
        if Assigned(TWI) or Assigned(LWI) then
        begin
          AImgList := TCustomListViewHack(ALV).SmallImages;
          AImgListWidth := TCustomListViewHack(ALV).SmallImagesWidth;
          AImgListRes := AImgList.ResolutionForControl[AImgListWidth, ALV];

          if AImgListRes.Valid and (x >= AListSpacing) and (x <= AImgListRes.Width + AFocusFrame) then
          begin
            include(Result, htOnIcon);
            Ax += AImgListRes.Width + AListSpacing;
          end;
        end;
      end else

      if Assigned(TCustomListViewHack(ALV).LargeImages) and (ALV.Items[I].ImageIndex >= 0) then
      begin
        if Assigned(TWI) or Assigned(LWI) then
        begin
          AImgList := TCustomListViewHack(ALV).LargeImages;
          AImgListWidth := TCustomListViewHack(ALV).LargeImagesWidth;
          AImgListRes := AImgList.ResolutionForControl[AImgListWidth, ALV];

          if AImgListRes.Valid and (x >= AListSpacing) and (x <= AImgListRes.Width + AFocusFrame) then
          begin
            include(Result, htOnIcon);
            Ax += AImgListRes.Width + AListSpacing;
          end;
        end;
      end;

      if [htOnIcon, htOnStateIcon] * Result = [] then
      begin
        if x >= (AX + AFocusFrame) then
          include(Result, htOnLabel);
      end;
    end;
  end else
  begin
    if PtInRect(ALV.ClientRect, Point(x, y)) then
      Result := [THitTest.htNowhere];
  end;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.GetItemAt
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
imptype function TQtWSCustomListView.GetItemAt(const ALV: TCustomListView; x,y: integer): Integer;
var
  QtListWidget: TQtListWidget;
  LWI: QListWidgetItemH;
  QtTreeWidget: TQtTreeWidget;
  TWI: QTreeWidgetItemH;
begin
  if not WSCheckHandleAllocated(ALV, 'GetItemAt') then
    Exit(-1);
  if IsIconView(ALV) then
  begin
    QtListWidget := TQtListWidget(ALV.Handle);
    LWI := QtListWidget.itemAt(x, y);
    Result := QtListWidget.getRow(LWI);
  end else
  begin
    QtTreeWidget := TQtTreeWidget(ALV.Handle);
    TWI := QtTreeWidget.itemAt(x, y);
    Result := QtTreeWidget.getRow(TWI);
  end;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.GetSelCount
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
imptype function TQtWSCustomListView.GetSelCount(const ALV: TCustomListView): Integer;
begin
  if not WSCheckHandleAllocated(ALV, 'GetSelCount') then
    Exit(-1);
  if IsIconView(ALV) then
    Result := TQtListWidget(ALV.Handle).getSelCount
  else
    Result := TQtTreeWidget(ALV.Handle).selCount;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.GetSelection
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
imptype function TQtWSCustomListView.GetSelection(const ALV: TCustomListView): Integer;
var
  QtListWidget: TQtListWidget;
  QtTreeWidget: TQtTreeWidget;
  FPInts: TPtrIntArray;
begin
  if not WSCheckHandleAllocated(ALV, 'GetSelection') then
    Exit(-1);
  if IsIconView(ALV) then
  begin
    QtListWidget := TQtListWidget(ALV.Handle);
    FPInts := QtListWidget.selectedItems;
  end else
  begin
    {implement selection event so we can return Alv.Selected.Index}
    QtTreeWidget := TQtTreeWidget(ALV.Handle);
    FPInts := QtTreeWidget.selectedItems;
  end;
  if Length(FPInts)>0 then
    Result := FPInts[0]
  else
    Result := -1;
end;

imptype function TQtWSCustomListView.GetTopItem(const ALV: TCustomListView): Integer;
var
  QtItemView: TQtAbstractItemView;
begin
  Result := -1;
  if not WSCheckHandleAllocated(ALV, 'GetTopItem') then
    Exit;
  // according to embarcadero docs this should return
  // only for vsList and vsReport
  if not (TCustomListViewHack(ALV).ViewStyle in [vsList, vsReport]) then
    exit;
  QtItemView := TQtAbstractItemView(ALV.Handle);
  Result := QtItemView.getTopItem;
end;


{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.InternalUpdateItems
  Params:  TCustomListView
  Returns: Nothing
  Sync TCustomListView with QTreeWidget items.
 ------------------------------------------------------------------------------}
imptype procedure TQtWSCustomListView.InternalUpdateItems(
  const AList: TCustomListView);
var
  QtTreeWidget: TQtTreeWidget;
  i, j: Integer;
  AItem: TListItem;
  WStr: WideString;
  Item: QTreeWidgetItemH;
  AAlignment: QtAlignment;
  Bmp: TBitmap;
  ImgListRes: TScaledImageListResolution;
begin
  QtTreeWidget := TQtTreeWidget(AList.Handle);

  GetCurrentImages(AList, ImgListRes);

  BeginUpdate(AList);
  try
    for i := 0 to AList.Items.Count - 1 do
    begin
      AItem := AList.Items[i];
      WStr := GetUTF8String(AItem.Caption);
      Item := QtTreeWidget.topLevelItem(i);
      QtTreeWidget.setItemText(Item, 0, WStr, AlignmentToQtAlignmentMap[AList.Column[0].Alignment]);
      QtTreeWidget.setItemData(Item, 0, AItem);
      if AList.Checkboxes then
      begin
        if AItem.Checked then
          QTreeWidgetItem_setCheckState(Item, 0, QtChecked)
        else
          QTreeWidgetItem_setCheckState(Item, 0, QtUnChecked);
      end;

      if ImgListRes.Valid and (ImgListRes.Count > 0) and
        ((AItem.ImageIndex >= 0) and (AItem.ImageIndex < ImgListRes.Count)) then
      begin
        Bmp := TBitmap.Create;
        try
          ImgListRes.GetBitmap(AItem.ImageIndex, Bmp);
          QTreeWidgetItem_setIcon(Item, 0, TQtImage(Bmp.Handle).AsIcon);
        finally
          Bmp.Free;
        end;
      end else
        QTreeWidgetItem_setIcon(Item, 0, nil);

      // subitems
      for j := 0 to AItem.SubItems.Count - 1 do
      begin
        AAlignment := QtAlignLeft;
        if (TCustomListViewHack(AList).Columns.Count > 0) and (j + 1 < TCustomListViewHack(AList).Columns.Count) then
          AAlignment := AlignmentToQtAlignmentMap[TCustomListViewHack(AList).Column[j + 1].Alignment];
        WStr := GetUtf8String(AItem.Subitems.Strings[j]);
        QtTreeWidget.setItemText(Item, j + 1, WStr, AAlignment);
        QtTreeWidget.setItemData(Item, j + 1, AItem);
      end;
    end;

  finally
    EndUpdate(AList);
  end;
end;

imptype procedure TQtWSCustomListView.GetCurrentImages(
  const ALV: TCustomListView; out AImgListRes: TScaledImageListResolution);
var
  LV: TCustomListViewHack;
  AImgList: TCustomImageList;
  AImgListWidth: Integer;
begin
  LV := TCustomListViewHack(ALV);
  case LV.ViewStyle of
    vsIcon:
    begin
      AImgList := LV.LargeImages;
      AImgListWidth := LV.LargeImagesWidth;
    end;
    vsSmallIcon, vsReport, vsList:
    begin
      AImgList := LV.SmallImages;
      AImgListWidth := LV.SmallImagesWidth;
    end;
  else
    AImgList := nil;
    AImgListWidth := 0;
  end;
  if AImgList = nil then
  begin
    AImgList := LV.StateImages;
    AImgListWidth := LV.StateImagesWidth;
  end;
  if AImgList<>nil then
    AImgListRes := AImgList.ResolutionForControl[AImgListWidth, ALV]
  else
    AImgListRes := TScaledImageListResolution.Create(nil, 0);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.SetSort
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
imptype procedure TQtWSCustomListView.SetSort(const ALV: TCustomListView;
  const AType: TSortType; const AColumn: Integer; const ASortDirection: TSortDirection);
var
  QtTreeWidget: TQtTreeWidget;
  {$IFDEF TEST_QT_SORTING}
  StdModel: QStandardItemModelH;
  {$ENDIF}
begin
  if not WSCheckHandleAllocated(ALV, 'SetSort') then
    Exit;

  if (csDesigning in ALV.ComponentState) then
    exit;

  if IsIconView(ALV) then
    exit;

  QtTreeWidget := TQtTreeWidget(ALV.Handle);

  if AType = stNone then
    QtTreeWidget.Header.SetSortIndicatorVisible(False)
  else
  begin
    {$IFDEF TEST_QT_SORTING}
    // QTreeWidget crashes sometimes on changing sort role (possible qt bug).
    // need deeper investigation.
    if QtTreeWidget.ItemCount > 0 then
    begin
      StdModel := QStandardItemModelH(QtTreeWidget.getModel);
      if QStandardItemModel_sortRole(StdModel) <> Ord(QtUserRole) then
        QStandardItemModel_setSortRole(StdModel, Ord(QtUserRole));
    end;
    {$ELSE}
    with QtTreeWidget do
    begin
      Header.SetSortIndicatorVisible(True);
      if (AColumn >= 0) and (AColumn < ColCount) then
      begin
        Header.SetSortIndicator(AColumn, QtSortOrder(Ord(ASortDirection)));
        if ItemCount > 0 then
          InternalUpdateItems(ALV);
      end;
    end;
    {$ENDIF}
  end;
end;


{------------------------------------------------------------------------------
  Method: TQtWSCustomListView.GetBoundingRect
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
imptype function TQtWSCustomListView.GetBoundingRect(const ALV: TCustomListView): TRect;
begin
  if not WSCheckHandleAllocated(ALV, 'GetBoundingRect') then
    Exit(Rect(0,0,0,0));
  Result := TQtWidget(ALV.Handle).getFrameGeometry;
end;

imptype function TQtWSCustomListView.GetViewOrigin(const ALV: TCustomListView): TPoint;
var
  QtItemView: TQtAbstractItemView;
begin
  Result := Point(0, 0);
  if not WSCheckHandleAllocated(ALV, 'GetViewOrigin') then
    Exit;
  QtItemView := TQtAbstractItemView(ALV.Handle);
  Result := QtItemView.getViewOrigin;
end;

imptype function TQtWSCustomListView.GetVisibleRowCount(const ALV: TCustomListView
  ): Integer;
begin
  Result := 0;
  if not WSCheckHandleAllocated(ALV, 'GetVisibleRowCount') then
    Exit;
  Result := TQtAbstractItemView(ALV.Handle).getVisibleRowCount;
end;

imptype procedure TQtWSCustomListView.SelectAll(const ALV: TCustomListView;
  const AIsSet: Boolean);
begin
  if not WSCheckHandleAllocated(ALV, 'SelectAll') then
    Exit;
  if AIsSet then
    QAbstractItemView_selectAll(QAbstractItemViewH(TQtWidget(ALV.Handle).Widget))
  else
    QAbstractItemView_clearSelection(QAbstractItemViewH(TQtWidget(ALV.Handle).Widget));
end;

imptype procedure TQtWSCustomListView.SetAllocBy(const ALV: TCustomListView;
  const AValue: Integer);
var
  QtList: TQtListWidget;
  NewValue: integer;
begin
  if not WSCheckHandleAllocated(ALV, 'SetAllocBy') then
    Exit;
  if TCustomListViewHack(ALV).ViewStyle <> vsReport then
  begin
    NewValue := AValue;
    if NewValue < 0 then
      NewValue := 0;
    QtList := TQtListWidget(ALV.Handle);
    if NewValue > 0 then
    begin
      QtList.setLayoutMode(QListViewBatched);
      QtList.BatchSize := NewValue;
    end else
      QtList.setLayoutMode(QListViewSinglePass);
  end;
end;

imptype procedure TQtWSCustomListView.SetIconArrangement(
  const ALV: TCustomListView; const AValue: TIconArrangement);
var
  QtList: TQtListWidget;
begin
  if not WSCheckHandleAllocated(ALV, 'SetIconArrangement') then
    Exit;
  if IsIconView(ALV) then
  begin
    QtList := TQtListWidget(ALV.Handle);
    if QtList.ViewStyle <> Ord(vsList) then
      QtList.setViewFlow(IconArngToQListFlow[AValue]);
  end;
end;

imptype procedure TQtWSCustomListView.SetImageList(const ALV: TCustomListView;
  const AList: TListViewImageList; const AValue: TCustomImageListResolution);
begin
  if not WSCheckHandleAllocated(ALV, 'SetImageList') then
    Exit;
  if not ALV.OwnerData then
    RecreateWnd(ALV);
end;

imptype procedure TQtWSCustomListView.SetItemsCount(const ALV: TCustomListView;
  const Avalue: Integer);
var
  QtListWidget: TQtListWidget;
  QtTreeWidget: TQtTreeWidget;
begin
  if not WSCheckHandleAllocated(ALV, 'SetItemsCount') then
    Exit;
  if IsIconView(ALV) then
  begin
    QtListWidget := TQtListWidget(ALV.Handle);
    QtListWidget.ItemCount := AValue;
  end else
  begin
    QtTreeWidget := TQtTreeWidget(ALV.Handle);
    QtTreeWidget.ItemCount := AValue;
  end;
end;

imptype procedure TQtWSCustomListView.SetOwnerData(const ALV: TCustomListView;
  const AValue: Boolean);
var
  QtItemView: TQtAbstractItemView;
begin
  if not WSCheckHandleAllocated(ALV, 'SetOwnerData') then
    Exit;
  QtItemView := TQtAbstractItemView(ALV.Handle);
  QtItemView.OwnerData := AValue;
end;

imptype procedure TQtWSCustomListView.SetProperty(const ALV: TCustomListView;
  const AProp: TListViewProperty; const AIsSet: Boolean);
const
  BoolToSelectionMode: array[Boolean] of QAbstractItemViewSelectionMode =
  (
    QAbstractItemViewSingleSelection,
    QAbstractItemViewExtendedSelection
  );
  BoolToSelectionBehavior: array[Boolean] of QAbstractItemViewSelectionBehavior =
  (
    QAbstractItemViewSelectItems,
    QAbstractItemViewSelectRows
  );
  BoolToEditTriggers: array[Boolean] of QAbstractItemViewEditTriggers =
  (
    QAbstractItemViewNoEditTriggers, // QAbstractItemViewSelectedClicked,
    QAbstractItemViewNoEditTriggers
  );
var
  SavedCheckable: Boolean;
  QtItemView: TQtAbstractItemView;
begin
  if not WSCheckHandleAllocated(ALV, 'SetProperty')
  then Exit;
  QtItemView := TQtAbstractItemView(ALV.Handle);
  case AProp of
    lvpAutoArrange:
      begin
        if IsIconView(ALV) and
          (TQtListWidget(ALV.Handle).ViewStyle <> Ord(vsList)) then
          TQtListWidget(ALV.Handle).setWrapping(AIsSet);
      end;
    lvpCheckboxes:
      begin
        SavedCheckable := QtItemView.Checkable;
        QtItemView.Checkable := AIsSet;
        if SavedCheckable <> AIsSet then
          RecreateWnd(ALV);
      end;
    lvpMultiSelect:
      begin
        if (QtItemView.getSelectionMode <> QAbstractItemViewNoSelection) then
          QtItemView.setSelectionMode(BoolToSelectionMode[AIsSet]);
      end;
    lvpShowColumnHeaders:
      begin
        if not IsIconView(ALV) then
          with TQtTreeWidget(ALV.Handle) do
            setHeaderVisible(AIsSet and (TCustomListViewHack(ALV).ViewStyle = vsReport)
              and (TCustomListViewHack(ALV).Columns.Count > 0) );
      end;
    lvpOwnerDraw: ; // utilized automatically.
    lvpReadOnly: QtItemView.setEditTriggers(BoolToEditTriggers[AIsSet]);
    lvpRowSelect:
      begin
        if not IsIconView(ALV) then
          TQtTreeWidget(ALV.Handle).setAllColumnsShowFocus(AIsSet);
        QtItemView.setSelectionBehavior(BoolToSelectionBehavior[AIsSet]);
      end;
    lvpWrapText: QtItemView.setWordWrap(AIsSet);
    lvpHideSelection: QtItemView.HideSelection := AIsSet;
  end;
end;

imptype procedure TQtWSCustomListView.SetProperties(const ALV: TCustomListView;
  const AProps: TListViewProperties);
var
  i: TListViewProperty;
begin
  if not WSCheckHandleAllocated(ALV, 'SetProperties')
  then Exit;
  for i := Low(TListViewProperty) to High(TListViewProperty) do
    SetProperty(ALV, i, i in AProps);
end;

imptype procedure TQtWSCustomListView.SetScrollBars(const ALV: TCustomListView;
  const AValue: TScrollStyle);
var
  QtItemView: TQtAbstractItemView;
begin
  if not WSCheckHandleAllocated(ALV, 'SetScrollBars') then
    Exit;
  QtItemView := TQtAbstractItemView(ALV.Handle);
  {always reset before applying new TScrollStyle}
  QtItemView.setScrollStyle(ssNone);
  if AValue <> ssNone then
    QtItemView.setScrollStyle(AValue);
end;

imptype procedure TQtWSCustomListView.SetViewStyle(const ALV: TCustomListView;
  const Avalue: TViewStyle);
var
  QtItemView: TQtAbstractItemView;
  QtListWidget: TQtListWidget;
  LWI: QListWidgetItemH;
  QtTreeWidget: TQtTreeWidget;
  ItemViewWidget: QAbstractItemViewH;
  Item: QTreeWidgetItemH;
  Size: TSize;
  x: Integer;
  j: Integer;
  ImgListRes: TScaledImageListResolution;
begin
  if not WSCheckHandleAllocated(ALV, 'SetViewStyle') then
    Exit;
  QtItemView := TQtAbstractItemView(ALV.Handle);

  if (QtItemView.ViewStyle <> Ord(AValue)) then
  begin
    RecreateWnd(ALV);
    exit;
  end;

  if IsIconView(ALV) then
  begin
    QtTreeWidget := Nil; // Suppress compiler warning.
    QtListWidget := TQtListWidget(ALV.Handle);
    ItemViewWidget := QListWidgetH(QtListWidget.Widget);
    QtListWidget.OwnerDrawn := False;
  end else
  begin
    QtListWidget := Nil; // Suppress compiler warning.
    QtTreeWidget := TQtTreeWidget(ALV.Handle);
    ItemViewWidget := QTreeWidgetH(QtTreeWidget.Widget);
    with QtTreeWidget do
      setHeaderVisible(TCustomListViewHack(ALV).ShowColumnHeaders and (AValue = vsReport)
        and (TCustomListViewHack(ALV).Columns.Count > 0) );
  end;
  GetCurrentImages(ALV, ImgListRes);
  if ImgListRes.Valid then
  begin
    Size.cy := ImgListRes.Height;
    Size.cx := ImgListRes.Width;
  end else
  begin
    case AValue of
      vsIcon: Size.cx := GetPixelMetric(QStylePM_IconViewIconSize, nil, ItemViewWidget);
      vsSmallIcon: Size.cx := GetPixelMetric(QStylePM_ListViewIconSize, nil, ItemViewWidget);
    else
      Size.cx := 0;
    end;
    Size.cy := Size.cx;
  end;
  if AValue in [vsList, vsReport] then
    TQtAbstractItemView(ALV.Handle).OwnerDrawn :=
      TCustomListViewHack(ALV).IsCustomDrawn(dtControl, cdPrePaint) or
      (TCustomListViewHack(ALV).OwnerDraw and
      (TCustomListViewHack(ALV).ViewStyle = vsReport));
  TQtAbstractItemView(ALV.Handle).IconSize := Size;

  if IsIconView(ALV) then
  begin
    LWI := QtListWidget.getItem(0);
    if LWI <> nil then
    begin
      X := Size.CY;
      QListWidgetItem_sizeHint(LWI, @Size);
      Size.Cy := X;
      QListWidgetItem_setSizeHint(LWI, @Size);
    end;
  end else
  begin
    Item := QtTreeWidget.topLevelItem(0);
    if Item <> nil then
    begin
      X := Size.CY;
      QTreeWidgetItem_sizeHint(Item, @Size, 0);
      Size.Cy := X;
      QTreeWidgetItem_setSizeHint(Item, 0, @Size);

      for j := 0 to QtTreeWidget.ColCount - 1 do
      begin
        Item := QtTreeWidget.itemAt(j, 0);
        QTreeWidgetItem_setSizeHint(Item, j, @Size);
      end;
    end;
    QtTreeWidget.UniformRowHeights := True;
  end;
end;

{$ifdef wsintf}
// Column
imptype procedure TQtWSCustomListView.ColumnSetMaxWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AMaxWidth: Integer);
begin
end;

imptype function TQtWSCustomListView.ItemSetPosition(const ALV: TCustomListView; const AIndex: Integer; const ANewPosition: TPoint): Boolean;
begin
  Result := false;
end;

imptype function TQtWSCustomListView.ItemGetStates(const ALV: TCustomListView; const AIndex: Integer; out AStates: TListItemStates): Boolean;
begin
  // returns True if supported
  Result := False;
end;

imptype procedure TQtWSCustomListView.ItemUpdate(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem);
begin
end;

imptype function TQtWSCustomListView.GetNextItem(const ALV: TCustomListView; const StartItem: TListItem; const Direction: TSearchDirection; const States: TListItemStates): TListItem;
var
  ACount: Integer;
  StartIndex, AIndex: Integer;
begin
  Result := nil;
  if StartItem = nil then
    Exit;
  StartIndex := StartItem.Index;
  AIndex := StartIndex;
  ACount := ALV.Items.Count;
  case Direction of
    sdAbove:
      while AIndex>0 do
      begin
        dec(AIndex);
        if States <= ALV.Items[AIndex].GetStates then
          Exit(ALV.Items[AIndex]);
      end;
    sdBelow:
      while AIndex < ACount-1 do
      begin
        inc(AIndex);
        if States <= ALV.Items[AIndex].GetStates then
          Exit(ALV.Items[AIndex]);
      end;
    sdAll:
      while True do
      begin
        inc(AIndex);
        Assert(AIndex <> StartIndex, 'TWSCustomListView.GetNextItem: AIndex=StartIndex');
        if AIndex >= ACount then
          Exit;
{       begin           Do not wrap around. Will never return Nil. Issue #38565.
          AIndex := -1;  continue;
        end;  }
        if States <= ALV.Items[AIndex].GetStates then
          Exit(ALV.Items[AIndex]);
      end;
  end;
end;

imptype function TQtWSCustomListView.GetDropTarget(const ALV: TCustomListView): Integer;
begin
  Result := -1;
end;

imptype function TQtWSCustomListView.GetHoverTime(const ALV: TCustomListView): Integer;
begin
  Result := -1;
end;

imptype procedure TQtWSCustomListView.SetDefaultItemHeight(const ALV: TCustomListView; const AValue: Integer);
begin
end;

imptype procedure TQtWSCustomListView.SetHotTrackStyles(const ALV: TCustomListView; const AValue: TListHotTrackStyles);
begin
end;

imptype procedure TQtWSCustomListView.SetHoverTime(const ALV: TCustomListView; const AValue: Integer);
begin
end;

imptype procedure TQtWSCustomListView.SetImageList(const ALV: TCustomListView; const AList: TListViewImageList; const AValue: TCustomImageList);
begin
end;

imptype procedure TQtWSCustomListView.SetViewOrigin(const ALV: TCustomListView; const AValue: TPoint);
begin
end;

imptype function TQtWSCustomListView.RestoreItemCheckedAfterSort(const ALV: TCustomListView): Boolean;
begin
  Result := false;
end;
{$endif}

end.
