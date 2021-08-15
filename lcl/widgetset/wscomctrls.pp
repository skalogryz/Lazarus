{
 *****************************************************************************
 *                               WSComCtrls.pp                               * 
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
unit WSComCtrls;

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
  Classes,
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Graphics, ImgList, Controls, StdCtrls, ComCtrls,
////////////////////////////////////////////////////
  {$ifdef wsintf}WSLCLClasses_Intf{$else}WSLCLClasses{$endif}, WSControls, WSExtCtrls, WSToolwin, WSFactory;

type
  { TWSCustomPage }
  {$ifndef wsintf}
  TWSCustomPageClass = class of TWSCustomPage;
  {$else}
  TWSCustomPageClass = interface(TWSWinControlClass)
    procedure UpdateProperties(const ACustomPage: TCustomPage);
  end;
  {$endif}
  TWSCustomPage = class(TWSWinControl{$ifdef wsintf},TWSCustomPageClass{$endif})
  impsection
    imptype procedure UpdateProperties(const ACustomPage: TCustomPage); virtual;
  end;

  { TWSCustomTabControl }
  {$ifdef wsintf}
  TWSCustomTabControlClass = interface(TWSWinControlClass)
    procedure AddPage(const ATabControl: TCustomTabControl; const AChild: TCustomPage; const AIndex: integer);
    procedure MovePage(const ATabControl: TCustomTabControl; const AChild: TCustomPage; const NewIndex: integer);
    procedure RemovePage(const ATabControl: TCustomTabControl; const AIndex: integer);

    function GetNotebookMinTabHeight(const AWinControl: TWinControl): integer;
    function GetNotebookMinTabWidth(const AWinControl: TWinControl): integer;
    function GetTabIndexAtPos(const ATabControl: TCustomTabControl; const AClientPos: TPoint): integer;
    function GetTabRect(const ATabControl: TCustomTabControl; const AIndex: Integer): TRect;
    function GetCapabilities: TCTabControlCapabilities;
    procedure SetTabSize(const ATabControl: TCustomTabControl; const ATabWidth, ATabHeight: integer);
    procedure SetImageList(const ATabControl: TCustomTabControl; const AImageList: TCustomImageListResolution);
    procedure SetPageIndex(const ATabControl: TCustomTabControl; const AIndex: integer);
    procedure SetTabCaption(const ATabControl: TCustomTabControl; const AChild: TCustomPage; const AText: string);
    procedure SetTabPosition(const ATabControl: TCustomTabControl; const ATabPosition: TTabPosition);
    procedure ShowTabs(const ATabControl: TCustomTabControl; AShowTabs: boolean);
    procedure UpdateProperties(const ATabControl: TCustomTabControl);
  end;
  {$endif}

  TWSCustomTabControl = class(TWSWinControl{$ifdef wsintf},TWSCustomTabControlClass{$endif})
  impsection
    imptype procedure AddPage(const ATabControl: TCustomTabControl; const AChild: TCustomPage; const AIndex: integer); virtual;
    imptype procedure MovePage(const ATabControl: TCustomTabControl; const AChild: TCustomPage; const NewIndex: integer); virtual;
    imptype procedure RemovePage(const ATabControl: TCustomTabControl; const AIndex: integer); virtual;

    imptype function GetNotebookMinTabHeight(const AWinControl: TWinControl): integer; virtual;
    imptype function GetNotebookMinTabWidth(const AWinControl: TWinControl): integer; virtual;
    imptype function GetTabIndexAtPos(const ATabControl: TCustomTabControl; const AClientPos: TPoint): integer; virtual;
    imptype function GetTabRect(const ATabControl: TCustomTabControl; const AIndex: Integer): TRect; virtual;
    imptype function GetCapabilities: TCTabControlCapabilities; virtual;
    imptype procedure SetTabSize(const ATabControl: TCustomTabControl; const ATabWidth, ATabHeight: integer); virtual;
    imptype procedure SetImageList(const ATabControl: TCustomTabControl; const AImageList: TCustomImageListResolution); virtual;
    imptype procedure SetPageIndex(const ATabControl: TCustomTabControl; const AIndex: integer); virtual;
    imptype procedure SetTabCaption(const ATabControl: TCustomTabControl; const AChild: TCustomPage; const AText: string); virtual;
    imptype procedure SetTabPosition(const ATabControl: TCustomTabControl; const ATabPosition: TTabPosition); virtual;
    imptype procedure ShowTabs(const ATabControl: TCustomTabControl; AShowTabs: boolean); virtual;
    imptype procedure UpdateProperties(const ATabControl: TCustomTabControl); virtual;
  end;
  {$ifndef wsintf}TWSCustomTabControlClass = class of TWSCustomTabControl;{$endif}

  { TWSStatusBar }
  {$ifndef wsintf}
  TWSStatusBarClass = class of TWSStatusBar;
  {$else}
  TWSStatusBarClass = interface(TWSWinControlClass)
    procedure PanelUpdate(const AStatusBar: TStatusBar; PanelIndex: integer);
    procedure SetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer);
    procedure SetSizeGrip(const AStatusBar: TStatusBar; SizeGrip: Boolean);
    procedure Update(const AStatusBar: TStatusBar);
  end;
  {$endif}
  TWSStatusBar = class(TWSWinControl{$ifdef wsintf},TWSStatusBarClass{$endif})
  impsection
    imptype procedure PanelUpdate(const AStatusBar: TStatusBar; PanelIndex: integer); virtual;
    imptype procedure SetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer); virtual;
    imptype procedure SetSizeGrip(const AStatusBar: TStatusBar; SizeGrip: Boolean); virtual;
    imptype procedure Update(const AStatusBar: TStatusBar); virtual;
    imptype function GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor; override;
  end;

  { TWSTabSheet }

  TWSTabSheet = class(TWSCustomPage)
  impsection
    imptype function GetDefaultColor(const AControl: TControl;
      const ADefaultColorType: TDefaultColorType): TColor; override;
  end;

  { TWSPageControl }

  TWSPageControl = class(TWSCustomTabControl)
  impsection
  end;

  { TWSCustomListView }
  TWSListViewItemChange = (lvicText, lvicImage);
  TWSListViewItemChanges = set of TWSListViewItemChange;
  {$ifdef wsintf}
  TWSCustomListViewClass = interface(TWSWinControlClass)
    // Column
    procedure ColumnDelete(const ALV: TCustomListView; const AIndex: Integer);
    function  ColumnGetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn): Integer;
    procedure ColumnInsert(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn);
    procedure ColumnMove(const ALV: TCustomListView; const AOldIndex, ANewIndex: Integer; const AColumn: TListColumn);
    procedure ColumnSetAlignment(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AAlignment: TAlignment);
    procedure ColumnSetAutoSize(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AAutoSize: Boolean);
    procedure ColumnSetCaption(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const ACaption: String);
    procedure ColumnSetImage(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AImageIndex: Integer);
    procedure ColumnSetMaxWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AMaxWidth: Integer);
    procedure ColumnSetMinWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AMinWidth: integer);
    procedure ColumnSetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AWidth: Integer);
    procedure ColumnSetVisible(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AVisible: Boolean);
    procedure ColumnSetSortIndicator(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const ASortIndicator: TSortIndicator);

    // Item
    procedure ItemDelete(const ALV: TCustomListView; const AIndex: Integer);
    function  ItemDisplayRect(const ALV: TCustomListView; const AIndex, ASubItem: Integer; ACode: TDisplayCode): TRect;
    procedure ItemExchange(const ALV: TCustomListView; AItem: TListItem; const AIndex1, AIndex2: Integer);
    procedure ItemMove(const ALV: TCustomListView; AItem: TListItem; const AFromIndex, AToIndex: Integer);
    function  ItemGetChecked(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem): Boolean;
    function  ItemGetPosition(const ALV: TCustomListView; const AIndex: Integer): TPoint;
    function  ItemGetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; out AIsSet: Boolean): Boolean; // returns True if supported
    function  ItemGetStates(const ALV: TCustomListView; const AIndex: Integer; out AStates: TListItemStates): Boolean; // returns True if supported
    procedure ItemInsert(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem);
    procedure ItemSetChecked(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AChecked: Boolean);
    procedure ItemSetImage(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex, AImageIndex: Integer);
    function ItemSetPosition(const ALV: TCustomListView; const AIndex: Integer; const ANewPosition: TPoint): Boolean;
    procedure ItemSetStateImage(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex, AStateImageIndex: Integer);
    procedure ItemSetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; const AIsSet: Boolean);
    procedure ItemSetText(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex: Integer; const AText: String);
    procedure ItemShow(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const PartialOK: Boolean);
    procedure ItemUpdate(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem);

    // LV
    procedure BeginUpdate(const ALV: TCustomListView);
    procedure EndUpdate(const ALV: TCustomListView);

    function GetBoundingRect(const ALV: TCustomListView): TRect;
    function GetDropTarget(const ALV: TCustomListView): Integer;
    function GetFocused(const ALV: TCustomListView): Integer;
    function GetHitTestInfoAt( const ALV: TCustomListView; X, Y: Integer ) : THitTests;
    function GetHoverTime(const ALV: TCustomListView): Integer;
    function GetItemAt(const ALV: TCustomListView; x,y: integer): Integer;
    function GetSelCount(const ALV: TCustomListView): Integer;
    function GetSelection(const ALV: TCustomListView): Integer;
    function GetTopItem(const ALV: TCustomListView): Integer;
    function GetViewOrigin(const ALV: TCustomListView): TPoint;
    function GetVisibleRowCount(const ALV: TCustomListView): Integer;
    function GetNextItem(const ALV: TCustomListView; const StartItem: TListItem; const Direction: TSearchDirection; const States: TListItemStates): TListItem;

    procedure SelectAll(const ALV: TCustomListView; const AIsSet: Boolean);
    procedure SetAllocBy(const ALV: TCustomListView; const AValue: Integer);
    procedure SetDefaultItemHeight(const ALV: TCustomListView; const AValue: Integer);
    procedure SetHotTrackStyles(const ALV: TCustomListView; const AValue: TListHotTrackStyles);
    procedure SetHoverTime(const ALV: TCustomListView; const AValue: Integer);
    procedure SetIconArrangement(const ALV: TCustomListView; const AValue: TIconArrangement);
    procedure SetImageList(const ALV: TCustomListView; const AList: TListViewImageList; const AValue: TCustomImageListResolution);
    procedure SetItemsCount(const ALV: TCustomListView; const Avalue: Integer);
    procedure SetOwnerData(const ALV: TCustomListView; const AValue: Boolean);
    procedure SetProperty(const ALV: TCustomListView; const AProp: TListViewProperty; const AIsSet: Boolean);
    procedure SetProperties(const ALV: TCustomListView; const AProps: TListViewProperties);
    procedure SetScrollBars(const ALV: TCustomListView; const AValue: TScrollStyle);
    procedure SetSort(const ALV: TCustomListView; const AType: TSortType; const AColumn: Integer;
      const ASortDirection: TSortDirection);
    procedure SetViewOrigin(const ALV: TCustomListView; const AValue: TPoint);
    procedure SetViewStyle(const ALV: TCustomListView; const Avalue: TViewStyle);
    // if returns true, then LCL will call SetItemChecked after calling SetSort
    // for every item previously checked. Only widgetsets that don't support native sort
    // AND/OR that don't support native checkboxes should have this method return true
    function RestoreItemCheckedAfterSort(const ALV: TCustomListView): Boolean;
  end;
  {$endif}
  TWSCustomListView = class(TWSWinControl{$ifdef wsintf}, TWSCustomListViewClass{$endif})
  impsection
    // Column
    imptype procedure ColumnDelete(const ALV: TCustomListView; const AIndex: Integer); virtual;
    imptype function  ColumnGetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn): Integer; virtual;
    imptype procedure ColumnInsert(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn); virtual;
    imptype procedure ColumnMove(const ALV: TCustomListView; const AOldIndex, ANewIndex: Integer; const AColumn: TListColumn); virtual;
    imptype procedure ColumnSetAlignment(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AAlignment: TAlignment); virtual;
    imptype procedure ColumnSetAutoSize(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AAutoSize: Boolean); virtual;
    imptype procedure ColumnSetCaption(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const ACaption: String); virtual;
    imptype procedure ColumnSetImage(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AImageIndex: Integer); virtual;
    imptype procedure ColumnSetMaxWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AMaxWidth: Integer); virtual;
    imptype procedure ColumnSetMinWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AMinWidth: integer); virtual;
    imptype procedure ColumnSetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AWidth: Integer); virtual;
    imptype procedure ColumnSetVisible(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AVisible: Boolean); virtual;
    imptype procedure ColumnSetSortIndicator(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const ASortIndicator: TSortIndicator); virtual;
              
    // Item          
    imptype procedure ItemDelete(const ALV: TCustomListView; const AIndex: Integer); virtual;
    imptype function  ItemDisplayRect(const ALV: TCustomListView; const AIndex, ASubItem: Integer; ACode: TDisplayCode): TRect; virtual;
    imptype procedure ItemExchange(const ALV: TCustomListView; AItem: TListItem; const AIndex1, AIndex2: Integer); virtual;
    imptype procedure ItemMove(const ALV: TCustomListView; AItem: TListItem; const AFromIndex, AToIndex: Integer); virtual;
    imptype function  ItemGetChecked(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem): Boolean; virtual;
    imptype function  ItemGetPosition(const ALV: TCustomListView; const AIndex: Integer): TPoint; virtual;
    imptype function  ItemGetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; out AIsSet: Boolean): Boolean; virtual; // returns True if supported
    imptype function  ItemGetStates(const ALV: TCustomListView; const AIndex: Integer; out AStates: TListItemStates): Boolean; virtual; // returns True if supported
    imptype procedure ItemInsert(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem); virtual;
    imptype procedure ItemSetChecked(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AChecked: Boolean); virtual;
    imptype procedure ItemSetImage(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex, AImageIndex: Integer); virtual;
    imptype function ItemSetPosition(const ALV: TCustomListView; const AIndex: Integer; const ANewPosition: TPoint): Boolean; virtual;
    imptype procedure ItemSetStateImage(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex, AStateImageIndex: Integer); virtual;
    imptype procedure ItemSetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; const AIsSet: Boolean); virtual;
    imptype procedure ItemSetText(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex: Integer; const AText: String); virtual;
    imptype procedure ItemShow(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const PartialOK: Boolean); virtual;
    imptype procedure ItemUpdate(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem); virtual;
    
    // LV
    imptype procedure BeginUpdate(const ALV: TCustomListView); virtual;
    imptype procedure EndUpdate(const ALV: TCustomListView); virtual;

    imptype function GetBoundingRect(const ALV: TCustomListView): TRect; virtual;
    imptype function GetDropTarget(const ALV: TCustomListView): Integer; virtual;
    imptype function GetFocused(const ALV: TCustomListView): Integer; virtual;
    imptype function GetHitTestInfoAt( const ALV: TCustomListView; X, Y: Integer ) : THitTests; virtual;
    imptype function GetHoverTime(const ALV: TCustomListView): Integer; virtual;
    imptype function GetItemAt(const ALV: TCustomListView; x,y: integer): Integer; virtual;
    imptype function GetSelCount(const ALV: TCustomListView): Integer; virtual;
    imptype function GetSelection(const ALV: TCustomListView): Integer; virtual;
    imptype function GetTopItem(const ALV: TCustomListView): Integer; virtual;
    imptype function GetViewOrigin(const ALV: TCustomListView): TPoint; virtual;
    imptype function GetVisibleRowCount(const ALV: TCustomListView): Integer; virtual;
    imptype function GetNextItem(const ALV: TCustomListView; const StartItem: TListItem; const Direction: TSearchDirection; const States: TListItemStates): TListItem; virtual;

    imptype procedure SelectAll(const ALV: TCustomListView; const AIsSet: Boolean); virtual;
    imptype procedure SetAllocBy(const ALV: TCustomListView; const AValue: Integer); virtual;
    imptype procedure SetDefaultItemHeight(const ALV: TCustomListView; const AValue: Integer); virtual;
    imptype procedure SetHotTrackStyles(const ALV: TCustomListView; const AValue: TListHotTrackStyles); virtual;
    imptype procedure SetHoverTime(const ALV: TCustomListView; const AValue: Integer); virtual;
    imptype procedure SetIconArrangement(const ALV: TCustomListView; const AValue: TIconArrangement); virtual;
    imptype procedure SetImageList(const ALV: TCustomListView; const AList: TListViewImageList; const AValue: TCustomImageListResolution); virtual;
    imptype procedure SetItemsCount(const ALV: TCustomListView; const Avalue: Integer); virtual;
    imptype procedure SetOwnerData(const ALV: TCustomListView; const AValue: Boolean); virtual;
    imptype procedure SetProperty(const ALV: TCustomListView; const AProp: TListViewProperty; const AIsSet: Boolean); virtual;
    imptype procedure SetProperties(const ALV: TCustomListView; const AProps: TListViewProperties); virtual;
    imptype procedure SetScrollBars(const ALV: TCustomListView; const AValue: TScrollStyle); virtual;
    imptype procedure SetSort(const ALV: TCustomListView; const AType: TSortType; const AColumn: Integer;
      const ASortDirection: TSortDirection); virtual;
    imptype procedure SetViewOrigin(const ALV: TCustomListView; const AValue: TPoint); virtual;
    imptype procedure SetViewStyle(const ALV: TCustomListView; const Avalue: TViewStyle); virtual;
    // if returns true, then LCL will call SetItemChecked after calling SetSort
    // for every item previously checked. Only widgetsets that don't support native sort
    // AND/OR that don't support native checkboxes should have this method return true
    imptype function RestoreItemCheckedAfterSort(const ALV: TCustomListView): Boolean; virtual;
  end;

  {$ifndef wsintf}TWSCustomListViewClass = class of TWSCustomListView;{$endif}

  { TWSListView }                             

  TWSListView = class(TWSCustomListView)
  impsection
  end;

  { TWSProgressBar }
  {$ifndef wsintf}
  TWSProgressBarClass = class of TWSProgressBar;
  {$else}
  TWSProgressBarClass = interface(TWSWinControlClass)
    procedure ApplyChanges(const AProgressBar: TCustomProgressBar);
    procedure SetPosition(const AProgressBar: TCustomProgressBar; const NewPosition: integer);
    procedure SetStyle(const AProgressBar: TCustomProgressBar; const NewStyle: TProgressBarStyle);
  end;
  {$endif}
  TWSProgressBar = class(TWSWinControl{$ifdef wsintf},TWSProgressBarClass{$endif})
  impsection
    imptype procedure ApplyChanges(const AProgressBar: TCustomProgressBar); virtual;
    imptype procedure SetPosition(const AProgressBar: TCustomProgressBar; const NewPosition: integer); virtual;
    imptype procedure SetStyle(const AProgressBar: TCustomProgressBar; const NewStyle: TProgressBarStyle); virtual;
  end;

  { TWSCustomUpDown }
  {$ifdef wsintf}
  TWSCustomUpDownClass = interface(TWSWinControlClass)
    procedure SetIncrement(const AUpDown: TCustomUpDown; AValue: Double);
    procedure SetMaxPosition(const AUpDown: TCustomUpDown; AValue: Double);
    procedure SetMinPosition(const AUpDown: TCustomUpDown; AValue: Double);
    procedure SetOrientation(const AUpDown: TCustomUpDown; AOrientation: TUDOrientation);
    procedure SetPosition(const AUpDown: TCustomUpDown; AValue: Double);
    // procedure SetRepeatInterval(const AUpDown: TWSCustomUpDown; ms: Integer);
    procedure SetUseArrowKeys(const AUpDown: TCustomUpDown; AUseArrow: Boolean);
    procedure SetWrap(const AUpDown: TCustomUpDown; ADoWrap: Boolean);
  end;
  {$endif}
  TWSCustomUpDown = class(TWSCustomControl{$ifdef wsintf},TWSCustomUpDownClass{$endif})
  impsection
    imptype procedure SetIncrement(const AUpDown: TCustomUpDown; AValue: Double); virtual;
    imptype procedure SetMaxPosition(const AUpDown: TCustomUpDown; AValue: Double); virtual;
    imptype procedure SetMinPosition(const AUpDown: TCustomUpDown; AValue: Double); virtual;
    imptype procedure SetOrientation(const AUpDown: TCustomUpDown; AOrientation: TUDOrientation); virtual;
    imptype procedure SetPosition(const AUpDown: TCustomUpDown; AValue: Double); virtual;
    // class procedure SetRepeatInterval(const AUpDown: TWSCustomUpDown; ms: Integer); virtual;
    imptype procedure SetUseArrowKeys(const AUpDown: TCustomUpDown; AUseArrow: Boolean); virtual;
    imptype procedure SetWrap(const AUpDown: TCustomUpDown; ADoWrap: Boolean); virtual;
  end;
  {$ifndef wsintf}TWSCustomUpDownClass = class of TWSCustomUpDown;{$endif}

  { TWSUpDown }

  TWSUpDown = class(TWSCustomUpDown)
  impsection
  end;

  { TWSToolButton }

  TWSToolButton = class(TWSCustomControl)
  impsection
  end;

  { TWSToolBar }

  TWSToolbarClass = class of TWSToolbar;
  TWSToolBar = class(TWSToolWindow)
  impsection
{$ifdef OldToolbar}  
    imptype function  GetButtonCount(const AToolBar: TToolBar): integer; virtual;
    imptype procedure InsertToolButton(const AToolBar: TToolbar; const AControl: TControl); virtual;
    imptype procedure DeleteToolButton(const AToolBar: TToolbar; const AControl: TControl); virtual;
{$endif}    
  end;

  { TWSTrackBar }
  {$ifdef wsintf}
  TWSTrackBarClass = interface(TWSWinControlClass)
    procedure ApplyChanges(const ATrackBar: TCustomTrackBar);
    function GetPosition(const ATrackBar: TCustomTrackBar): integer;
    procedure SetOrientation(const ATrackBar: TCustomTrackBar; const AOrientation: TTrackBarOrientation);
    procedure SetPosition(const ATrackBar: TCustomTrackBar; const NewPosition: integer);
    procedure SetTick(const ATrackBar: TCustomTrackBar; const ATick: integer);
    procedure SetTickStyle(const ATrackBar: TCustomTrackBar; const ATickStyle: TTickStyle);
  end;
  {$endif}

  TWSTrackBar = class(TWSWinControl{$ifdef wsintf},TWSTrackBarClass{$endif})
  impsection
    imptype procedure ApplyChanges(const ATrackBar: TCustomTrackBar); virtual;
    imptype function GetPosition(const ATrackBar: TCustomTrackBar): integer; virtual;
    imptype procedure SetOrientation(const ATrackBar: TCustomTrackBar; const AOrientation: TTrackBarOrientation); virtual;
    imptype procedure SetPosition(const ATrackBar: TCustomTrackBar; const NewPosition: integer); virtual;
    imptype procedure SetTick(const ATrackBar: TCustomTrackBar; const ATick: integer); virtual;
    imptype procedure SetTickStyle(const ATrackBar: TCustomTrackBar; const ATickStyle: TTickStyle); virtual;
  end;
  {$ifndef wsintf}TWSTrackBarClass = class of TWSTrackBar;{$endif}

  { TWSCustomTreeView }

  TWSCustomTreeView = class(TWSCustomControl)
  impsection
  end;

  { TWSTreeView }

  TWSTreeView = class(TWSCustomTreeView)
  impsection
  end;

  { WidgetSetRegistration }

  procedure RegisterStatusBar;
  procedure RegisterTabSheet;
  procedure RegisterPageControl;
  procedure RegisterCustomListView;
  procedure RegisterCustomProgressBar;
  procedure RegisterCustomUpDown;
  procedure RegisterCustomToolButton;
  procedure RegisterToolBar;
  procedure RegisterCustomTrackBar;
  procedure RegisterCustomTreeView;

implementation

uses
  LResources;

{ TWSCustomUpDown }

imptype procedure TWSCustomUpDown.SetUseArrowKeys(const AUpDown: TCustomUpDown;
  AUseArrow: Boolean);
begin

end;

imptype procedure TWSCustomUpDown.SetMinPosition(const AUpDown: TCustomUpDown;
  AValue: Double);
begin

end;

imptype procedure TWSCustomUpDown.SetMaxPosition(const AUpDown: TCustomUpDown;
  AValue: Double);
begin

end;

imptype procedure TWSCustomUpDown.SetPosition(const AUpDown: TCustomUpDown;
  AValue: Double);
begin

end;

imptype procedure TWSCustomUpDown.SetIncrement(const AUpDown: TCustomUpDown;
  AValue: Double);
begin

end;

imptype procedure TWSCustomUpDown.SetOrientation(const AUpDown: TCustomUpDown;
  AOrientation: TUDOrientation);
begin

end;

imptype procedure TWSCustomUpDown.SetWrap(const AUpDown: TCustomUpDown;
  ADoWrap: Boolean);
begin

end;

{ TWSTabSheet }

imptype function TWSTabSheet.GetDefaultColor(const AControl: TControl;
  const ADefaultColorType: TDefaultColorType): TColor;
begin
  Result:=DefBtnColors[ADefaultColorType];
end;

{ TWSCustomPage }

imptype procedure TWSCustomPage.UpdateProperties(const ACustomPage: TCustomPage);
begin
end;

{ TWSCustomTabControl }

{ -----------------------------------------------------------------------------
  Method: TWSCustomTabControl.AddPage
  Params: ATabControl - A notebook control
          AChild - Page to insert
          AIndex  - The position in the notebook to insert the page
  Returns: Nothing

  Adds a new page to a notebook
 ------------------------------------------------------------------------------}
imptype procedure TWSCustomTabControl.AddPage(const ATabControl: TCustomTabControl; const AChild: TCustomPage; const AIndex: integer);
begin
end;

{------------------------------------------------------------------------------
  Method: TWSCustomTabControl.MovePage
  Params: ATabControl - The notebook control
          AChild    - The page to move
          NewIndex  - The new index of the page
  Returns: Nothing

  Moves a page in a notebook control
 ------------------------------------------------------------------------------}
imptype procedure TWSCustomTabControl.MovePage(const ATabControl: TCustomTabControl;
  const AChild: TCustomPage; const NewIndex: integer);
begin
end;

{------------------------------------------------------------------------------
  Method: TWSCustomTabControl.RemovePage
  Params: ATabControl - The notebook control
          AIndex    - The index of the page to delete
  Returns: Nothing

  Removes a page from a notebook control
 ------------------------------------------------------------------------------}
imptype procedure TWSCustomTabControl.RemovePage(const ATabControl: TCustomTabControl; const AIndex: integer);
begin
end;

{-------------------------------------------------------------------------------
  function TWSCustomTabControl.GetNotebookMinTabHeight(
    const AWinControl: TWinControl): integer;

  Returns the minimum height of the horizontal tabs of a notebook. That is the
  Notebook with TabPosition in [tpTop,tpBottom] without the client panel.
-------------------------------------------------------------------------------}
imptype function  TWSCustomTabControl.GetNotebookMinTabHeight(
  const AWinControl: TWinControl): integer;
begin
  Result:=30;
end;

{-------------------------------------------------------------------------------
  function TWSCustomTabControl.GetNotebookMinTabWidth(
    const AWinControl: TWinControl): integer;

  Returns the minimum width of the vertical tabs of a notebook. That is the
  Notebook with TabPosition in [tpLeft,tpRight] without the client panel.
-------------------------------------------------------------------------------}
imptype function TWSCustomTabControl.GetNotebookMinTabWidth(const AWinControl: TWinControl
  ): integer;
begin
  Result:=60;
end;

imptype function TWSCustomTabControl.GetTabIndexAtPos(const ATabControl: TCustomTabControl;
  const AClientPos: TPoint): integer;
begin
  Result := -1;
end;

imptype function TWSCustomTabControl.GetTabRect(const ATabControl: TCustomTabControl;
  const AIndex: Integer): TRect;
begin
  Result := Rect(-1,-1,-1,-1);
end;

imptype function TWSCustomTabControl.GetCapabilities: TCTabControlCapabilities;
begin
  Result:=[];
end;

imptype procedure TWSCustomTabControl.SetTabSize(
  const ATabControl: TCustomTabControl; const ATabWidth, ATabHeight: integer);
begin
end;

imptype procedure TWSCustomTabControl.SetImageList(
  const ATabControl: TCustomTabControl; const AImageList: TCustomImageListResolution);
begin
end;

imptype procedure TWSCustomTabControl.SetPageIndex(const ATabControl: TCustomTabControl;
  const AIndex: integer);
begin
end;

imptype procedure TWSCustomTabControl.SetTabCaption(const ATabControl: TCustomTabControl;
  const AChild: TCustomPage; const AText: string);
begin
end;

imptype procedure TWSCustomTabControl.SetTabPosition(const ATabControl: TCustomTabControl;
  const ATabPosition: TTabPosition);
begin
end;

imptype procedure TWSCustomTabControl.ShowTabs(const ATabControl: TCustomTabControl;
  AShowTabs: boolean);
begin
end;

imptype procedure TWSCustomTabControl.UpdateProperties(
  const ATabControl: TCustomTabControl);
begin

end;

{ TWSStatusBar }

imptype procedure TWSStatusBar.PanelUpdate(const AStatusBar: TStatusBar; PanelIndex: integer);
begin
end;

imptype procedure TWSStatusBar.SetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer);
begin
end;

imptype procedure TWSStatusBar.SetSizeGrip(const AStatusBar: TStatusBar;
  SizeGrip: Boolean);
begin
end;

imptype procedure TWSStatusBar.Update(const AStatusBar: TStatusBar);
begin
end;

imptype function TWSStatusBar.GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor;
begin
  Result := DefBtnColors[ADefaultColorType];
end;
    
{ TWSCustomListView }

imptype procedure TWSCustomListView.ColumnDelete(const ALV: TCustomListView;
  const AIndex: Integer);
begin
end;

imptype function TWSCustomListView.ColumnGetWidth(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn): Integer;
begin
  Result := -1;
end;

imptype procedure TWSCustomListView.ColumnInsert(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn);
begin
end;

imptype procedure TWSCustomListView.ColumnMove(const ALV: TCustomListView;
  const AOldIndex, ANewIndex: Integer; const AColumn: TListColumn);
begin
end;

imptype procedure TWSCustomListView.ColumnSetAlignment(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn;
  const AAlignment: TAlignment);
begin
end;

imptype procedure TWSCustomListView.ColumnSetAutoSize(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AAutoSize: Boolean);
begin
end;

imptype procedure TWSCustomListView.ColumnSetCaption(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const ACaption: String);
begin
end;

imptype procedure TWSCustomListView.ColumnSetImage(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AImageIndex: Integer);
begin
end;

imptype procedure TWSCustomListView.ColumnSetMaxWidth(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AMaxWidth: Integer);
begin
end;

imptype procedure TWSCustomListView.ColumnSetMinWidth(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AMinWidth: integer);
begin
end;

imptype procedure TWSCustomListView.ColumnSetWidth(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AWidth: Integer);
begin
end;

imptype procedure TWSCustomListView.ColumnSetVisible(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn; const AVisible: Boolean);
begin
end;

imptype procedure TWSCustomListView.ColumnSetSortIndicator(
  const ALV: TCustomListView; const AIndex: Integer;
  const AColumn: TListColumn; const ASortIndicator: TSortIndicator);
begin

end;

imptype procedure TWSCustomListView.ItemDelete(const ALV: TCustomListView;
  const AIndex: Integer);
begin
end;

imptype function TWSCustomListView.ItemDisplayRect(const ALV: TCustomListView; const AIndex, ASubItem: Integer; ACode: TDisplayCode): TRect;
begin
  Result := Rect(0,0,0,0);
end;

imptype procedure TWSCustomListView.ItemExchange(const ALV: TCustomListView;
  AItem: TListItem; const AIndex1, AIndex2: Integer);
begin
end;

imptype procedure TWSCustomListView.ItemMove(const ALV: TCustomListView;
  AItem: TListItem; const AFromIndex, AToIndex: Integer);
begin
end;

imptype function TWSCustomListView.ItemGetChecked(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem): Boolean;
begin
  Result := False;
end;

imptype function TWSCustomListView.ItemGetPosition(const ALV: TCustomListView;
  const AIndex: Integer): TPoint;
begin
  Result := Point(0, 0);
end;

imptype function TWSCustomListView.ItemGetState(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const AState: TListItemState;
  out AIsSet: Boolean): Boolean;
begin
  // returns True if supported
  Result := False;
  AIsSet:=false;
end;

imptype function TWSCustomListView.ItemGetStates(const ALV: TCustomListView; const AIndex: Integer; out AStates: TListItemStates): Boolean;
begin
  // returns True if supported
  Result := False;
end;

imptype procedure TWSCustomListView.ItemInsert(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem);
begin
end;

imptype procedure TWSCustomListView.ItemSetChecked(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AChecked: Boolean);
begin
end;

imptype procedure TWSCustomListView.ItemSetImage(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem;
  const ASubIndex, AImageIndex: Integer);
begin
end;

imptype function TWSCustomListView.ItemSetPosition(const ALV: TCustomListView;
  const AIndex: Integer; const ANewPosition: TPoint): Boolean;
begin
  Result := False;
end;

imptype procedure TWSCustomListView.ItemSetStateImage(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem;
  const ASubIndex, AStateImageIndex: Integer);
begin
end;

imptype procedure TWSCustomListView.ItemSetState(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const AState: TListItemState;
  const AIsSet: Boolean);
begin
end;

imptype procedure TWSCustomListView.ItemSetText(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const ASubIndex: Integer;
  const AText: String);
begin
end;

imptype procedure TWSCustomListView.ItemShow(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const PartialOK: Boolean);
begin
end;

imptype procedure TWSCustomListView.ItemUpdate(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem);
begin
end;

imptype procedure TWSCustomListView.BeginUpdate(const ALV: TCustomListView);
begin
end;

imptype procedure TWSCustomListView.EndUpdate(const ALV: TCustomListView);
begin
end;

imptype function TWSCustomListView.GetBoundingRect(const ALV: TCustomListView): TRect;
begin
  Result := Rect(0,0,0,0);
end;

imptype function TWSCustomListView.GetDropTarget(const ALV: TCustomListView): Integer;
begin       
  Result := -1;
end;

imptype function TWSCustomListView.GetFocused(const ALV: TCustomListView): Integer;
begin
  Result := -1;
end;

imptype function TWSCustomListView.GetHitTestInfoAt( const ALV: TCustomListView; X, Y: Integer ) : THitTests;
begin
  Result := [];
end;

imptype function TWSCustomListView.GetHoverTime(const ALV: TCustomListView): Integer;
begin
  Result := -1;
end;

imptype function TWSCustomListView.GetItemAt(const ALV: TCustomListView; x,y: integer): Integer;
begin
  result:=-1;
end;

imptype function TWSCustomListView.GetSelCount(const ALV: TCustomListView): Integer;
begin
  Result := 0;
end;

imptype function TWSCustomListView.GetSelection(const ALV: TCustomListView): Integer;
begin
  Result := -1;
end;

imptype function TWSCustomListView.GetTopItem(const ALV: TCustomListView): Integer;
begin
  Result := -1;
end;

imptype function TWSCustomListView.GetViewOrigin(const ALV: TCustomListView): TPoint;
begin
  Result := Point(0, 0);
end;

imptype function TWSCustomListView.GetVisibleRowCount(const ALV: TCustomListView): Integer;
begin
  Result := 0;
end;

imptype procedure TWSCustomListView.SetAllocBy(const ALV: TCustomListView; const AValue: Integer);
begin
end;

imptype procedure TWSCustomListView.SetDefaultItemHeight(const ALV: TCustomListView; const AValue: Integer);
begin
end;

imptype procedure TWSCustomListView.SetHotTrackStyles(const ALV: TCustomListView; const AValue: TListHotTrackStyles);
begin
end;

imptype procedure TWSCustomListView.SetHoverTime(const ALV: TCustomListView; const AValue: Integer);
begin
end;

imptype procedure TWSCustomListView.SetIconArrangement(
  const ALV: TCustomListView; const AValue: TIconArrangement);
begin
end;

imptype procedure TWSCustomListView.SetImageList(const ALV: TCustomListView;
  const AList: TListViewImageList; const AValue: TCustomImageListResolution);
begin
end;

imptype procedure TWSCustomListView.SetOwnerData(const ALV: TCustomListView;
  const AValue: Boolean);
begin
end;

imptype procedure TWSCustomListView.SetProperty(const ALV: TCustomListView; const AProp: TListViewProperty; const AIsSet: Boolean);
begin
end;

// Default implementation based on SetProperty
imptype procedure TWSCustomListView.SetProperties(const ALV: TCustomListView; const AProps: TListViewProperties);
begin
  SetProperty(ALV, lvpAutoArrange, lvpAutoArrange in AProps);
  SetProperty(ALV, lvpCheckboxes, lvpCheckboxes in AProps);
  SetProperty(ALV, lvpColumnClick, lvpColumnClick in AProps);
  SetProperty(ALV, lvpFlatScrollBars, lvpFlatScrollBars in AProps);
  SetProperty(ALV, lvpFullDrag, lvpFullDrag in AProps);
  SetProperty(ALV, lvpGridLines, lvpGridLines in AProps);
  SetProperty(ALV, lvpHideSelection, lvpHideSelection in AProps);
  SetProperty(ALV, lvpHotTrack, lvpHotTrack in AProps);
  SetProperty(ALV, lvpMultiSelect, lvpMultiSelect in AProps);
  SetProperty(ALV, lvpOwnerDraw, lvpOwnerDraw in AProps);
  SetProperty(ALV, lvpReadOnly, lvpReadOnly in AProps);
  SetProperty(ALV, lvpRowSelect, lvpRowSelect in AProps);
  SetProperty(ALV, lvpShowColumnHeaders, lvpShowColumnHeaders in AProps);
  SetProperty(ALV, lvpShowWorkAreas, lvpShowWorkAreas in AProps);
  SetProperty(ALV, lvpWrapText, lvpWrapText in AProps);
  SetProperty(ALV, lvpToolTips, lvpToolTips in AProps);
end;

imptype procedure TWSCustomListView.SetScrollBars(const ALV: TCustomListView; const AValue: TScrollStyle);
begin
end;

imptype procedure TWSCustomListView.SetSort(const ALV: TCustomListView;
  const AType: TSortType; const AColumn: Integer;
  const ASortDirection: TSortDirection);
begin
end;

imptype procedure TWSCustomListView.SetViewOrigin(const ALV: TCustomListView; const AValue: TPoint);
begin
end;

imptype procedure TWSCustomListView.SetViewStyle(const ALV: TCustomListView; const Avalue: TViewStyle);
begin
end;

imptype function TWSCustomListView.RestoreItemCheckedAfterSort(const ALV: TCustomListView
  ): Boolean;
begin
  Result := false;
end;

imptype procedure TWSCustomListView.SetItemsCount(const ALV: TCustomListView; const Avalue: Integer);
begin
end;

imptype procedure TWSCustomListView.SelectAll(const ALV: TCustomListView;
  const AIsSet: Boolean);
begin

end;

//Default implementation
imptype function TWSCustomListView.GetNextItem(const ALV: TCustomListView;
  const StartItem: TListItem; const Direction: TSearchDirection; const States: TListItemStates): TListItem;
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

{ TWSProgressBar }

imptype procedure TWSProgressBar.ApplyChanges(const AProgressBar: TCustomProgressBar);
begin
end;

imptype procedure TWSProgressBar.SetPosition(const AProgressBar: TCustomProgressBar;
  const NewPosition: integer);
begin
end;

imptype procedure TWSProgressBar.SetStyle(const AProgressBar: TCustomProgressBar;
  const NewStyle: TProgressBarStyle);
begin
end;

{ TWSToolbar }

{$ifdef OldToolbar}

imptype function TWSToolbar.GetButtonCount(const AToolBar: TToolBar): integer;
begin
  Result := 0;
end;

imptype procedure TWSToolbar.InsertToolButton(const AToolBar: TToolbar; const AControl: TControl);
begin
end;

imptype procedure TWSToolbar.DeleteToolButton(const AToolBar: TToolbar; const AControl: TControl);
begin
end;

{$endif}

{ TWSTrackBar }

imptype procedure TWSTrackBar.ApplyChanges(const ATrackBar: TCustomTrackBar);
begin
end;

imptype function  TWSTrackBar.GetPosition(const ATrackBar: TCustomTrackBar): integer;
begin
  Result := 0;
end;

imptype procedure TWSTrackBar.SetOrientation(const ATrackBar: TCustomTrackBar;
  const AOrientation: TTrackBarOrientation);
begin
  RecreateWnd(ATrackBar);
end;

imptype procedure TWSTrackBar.SetPosition(const ATrackBar: TCustomTrackBar; const NewPosition: integer);
begin
end;

imptype procedure TWSTrackBar.SetTick(const ATrackBar: TCustomTrackBar; const ATick: integer);
begin
end;

imptype procedure TWSTrackBar.SetTickStyle(const ATrackBar: TCustomTrackBar; const ATickStyle: TTickStyle);
begin
  RecreateWnd(ATrackBar);
end;

{ WidgetSetRegistration }

procedure RegisterStatusBar;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterStatusBar;
  RegisterPropertyToSkip(TStatusBar, 'Font', 'VCL compatibility property', '');
  RegisterPropertyToSkip(TStatusBar, 'TabOrder', 'VCL compatibility property', '');
  RegisterPropertyToSkip(TStatusBar, 'TabStop', 'VCL compatibility property', '');
  RegisterPropertyToSkip(TStatusBar, 'UseSystemFont', 'VCL compatibility property', '');
//  if not WSRegisterStatusBar then
//    RegisterWSComponent(TStatusBar, TWSStatusBar);
  Done := True;
end;

procedure RegisterTabSheet;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterTabSheet;
//  if not WSRegisterTabSheet then
//    RegisterWSComponent(TTabSheet, TWSTabSheet)
  Done := True;
end;

procedure RegisterPageControl;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterPageControl;
  RegisterPropertyToSkip(TPageControl, 'OnPageChanged', 'Was removed in Laz 0.9.31 due to incompatibilities with OnChange, which does the same thing.', '');
//  if not WSRegisterPageControl then
//    RegisterWSComponent(TPageControl, TWSPageControl);
  Done := True;
end;

procedure RegisterCustomListView;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomListView;
  RegisterPropertyToSkip(TListColumn, 'WidthType', 'VCL compatibility property', '');
//  if not WSRegisterCustomListView then
//    RegisterWSComponent(TCustomListView, TWSCustomListView);
  Done := True;
end;

procedure RegisterCustomProgressBar;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomProgressBar;
//  if not WSRegisterCustomProgressBar then
//    RegisterWSComponent(TCustomProgressBar, TWSCustomProgressBar);
  Done := True;
end;

procedure RegisterCustomUpDown;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomUpDown;
//  if not WSRegisterCustomUpDown then
//    RegisterWSComponent(TCustomUpDown, TWSCustomUpDown);
  Done := True;
end;

procedure RegisterCustomToolButton;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomToolButton;
//  if not WSRegisterCustomToolButton then
//    RegisterWSComponent(TCustomToolButton, TWSToolButton);
  Done := True;
end;

procedure RegisterToolBar;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterToolBar;
//  if not WSRegisterToolBar then
//    RegisterWSComponent(TToolBar, TWSToolBar);
  Done := True;
end;

procedure RegisterCustomTrackBar;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomTrackBar;
  RegisterPropertyToSkip(TCustomTrackBar, 'ThumbLength', 'VCL compatibility property', '');
//  if not WSRegisterCustomTrackBar then
//    RegisterWSComponent(TCustomTrackBar, TWSCustomTrackBar);
  Done := True;
end;

procedure RegisterCustomTreeView;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomTreeView;
  RegisterPropertyToSkip(TCustomTreeView, 'BevelInner', 'VCL compatibility property', '');
  RegisterPropertyToSkip(TCustomTreeView, 'MultiSelect', 'VCL compatibility property', '');
//  if not WSRegisterStatusBar then
//    RegisterWSComponent(TCustomTreeView, TWSCustomTreeView);
  Done := True;
end;

end.
