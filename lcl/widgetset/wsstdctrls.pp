{ $Id$}
{
 *****************************************************************************
 *                               WSStdCtrls.pp                               * 
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
unit WSStdCtrls;

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
// To get as little as possible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Graphics, Controls, StdCtrls,
////////////////////////////////////////////////////
  Clipbrd, LazUTF8, WSLCLClasses, WSControls, WSFactory;

type
  { TWSScrollBar }
  {$ifdef wsintf}
  TWSScrollBarClass = interface(TWSWinControlClass)
    procedure SetParams(const AScrollBar: TCustomScrollBar);
    procedure SetKind(const AScrollBar: TCustomScrollBar; const AIsHorizontal: Boolean);
  end;
  {$endif}

  TWSScrollBar = class(TWSWinControl{$ifdef wsintf},TWSScrollBarClass{$endif})
  impsection
    imptype procedure SetParams(const AScrollBar: TCustomScrollBar); virtual;
    imptype procedure SetKind(const AScrollBar: TCustomScrollBar; const AIsHorizontal: Boolean); virtual;
  end;
  {$ifndef wsintf}TWSScrollBarClass = class of TWSScrollBar;{$endif}

  { TWSCustomGroupBox }

  TWSCustomGroupBox = class(TWSCustomControl)
  impsection
  end;

  { TWSGroupBox }

  TWSGroupBox = class(TWSCustomGroupBox)
  impsection
    imptype function GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor; override;
  end;

  { TWSCustomComboBox }
  {$ifdef wsintf}
  TWSCustomComboBoxClass = interface(TWSWinControlClass)
    function GetDroppedDown(const ACustomComboBox: TCustomComboBox): Boolean;
    function GetSelStart(const ACustomComboBox: TCustomComboBox): integer;
    function GetSelLength(const ACustomComboBox: TCustomComboBox): integer;
    function GetItemIndex(const ACustomComboBox: TCustomComboBox): integer;
    function GetMaxLength(const ACustomComboBox: TCustomComboBox): integer;

    procedure SetArrowKeysTraverseList(const ACustomComboBox: TCustomComboBox;
      NewTraverseList: boolean);
    procedure SetDropDownCount(const ACustomComboBox: TCustomComboBox; NewCount: Integer);
    procedure SetDroppedDown(const ACustomComboBox: TCustomComboBox; ADroppedDown: Boolean);
    procedure SetSelStart(const ACustomComboBox: TCustomComboBox; NewStart: integer);
    procedure SetSelLength(const ACustomComboBox: TCustomComboBox; NewLength: integer);
    procedure SetItemIndex(const ACustomComboBox: TCustomComboBox; NewIndex: integer);
    procedure SetMaxLength(const ACustomComboBox: TCustomComboBox; NewLength: integer);
    procedure SetStyle(const ACustomComboBox: TCustomComboBox; NewStyle: TComboBoxStyle);
    procedure SetReadOnly(const ACustomComboBox: TCustomComboBox; NewReadOnly: boolean);
    procedure SetTextHint(const ACustomComboBox: TCustomComboBox; const ATextHint: string);

    function  GetItems(const ACustomComboBox: TCustomComboBox): TStrings;
    procedure FreeItems(var AItems: TStrings);
    procedure Sort(const ACustomComboBox: TCustomComboBox; AList: TStrings; IsSorted: boolean);

    function GetItemHeight(const ACustomComboBox: TCustomComboBox): Integer;
    procedure SetItemHeight(const ACustomComboBox: TCustomComboBox; const AItemHeight: Integer);
  end;
  {$endif}

  TWSCustomComboBox = class(TWSWinControl{$ifdef wsintf},TWSCustomComboBoxClass{$endif})
  impsection
    imptype function GetDroppedDown(const ACustomComboBox: TCustomComboBox): Boolean; virtual;
    imptype function GetSelStart(const ACustomComboBox: TCustomComboBox): integer; virtual;
    imptype function GetSelLength(const ACustomComboBox: TCustomComboBox): integer; virtual;
    imptype function GetItemIndex(const ACustomComboBox: TCustomComboBox): integer; virtual;
    imptype function GetMaxLength(const ACustomComboBox: TCustomComboBox): integer; virtual;
    
    imptype procedure SetArrowKeysTraverseList(const ACustomComboBox: TCustomComboBox;
      NewTraverseList: boolean); virtual;
    imptype procedure SetDropDownCount(const ACustomComboBox: TCustomComboBox; NewCount: Integer); virtual;
    imptype procedure SetDroppedDown(const ACustomComboBox: TCustomComboBox; ADroppedDown: Boolean); virtual;
    imptype procedure SetSelStart(const ACustomComboBox: TCustomComboBox; NewStart: integer); virtual;
    imptype procedure SetSelLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); virtual;
    imptype procedure SetItemIndex(const ACustomComboBox: TCustomComboBox; NewIndex: integer); virtual;
    imptype procedure SetMaxLength(const ACustomComboBox: TCustomComboBox; NewLength: integer); virtual;
    imptype procedure SetStyle(const ACustomComboBox: TCustomComboBox; NewStyle: TComboBoxStyle); virtual;
    imptype procedure SetReadOnly(const ACustomComboBox: TCustomComboBox; NewReadOnly: boolean); virtual;
    imptype procedure SetTextHint(const ACustomComboBox: TCustomComboBox; const ATextHint: string); virtual;

    imptype function  GetItems(const ACustomComboBox: TCustomComboBox): TStrings; virtual;
    imptype procedure FreeItems(var AItems: TStrings); virtual;
    imptype procedure Sort(const ACustomComboBox: TCustomComboBox; AList: TStrings; IsSorted: boolean); virtual;
    
    imptype function GetItemHeight(const ACustomComboBox: TCustomComboBox): Integer; virtual;
    imptype procedure SetItemHeight(const ACustomComboBox: TCustomComboBox; const AItemHeight: Integer); virtual;
  end;
  {$ifndef wsintf}TWSCustomComboBoxClass = class of TWSCustomComboBox;{$endif}

  { TWSComboBox }

  TWSComboBox = class(TWSCustomComboBox)
  impsection
  end;

  { TWSCustomListBox }

   {$ifdef wsintf}
   TWSCustomListBoxClass = interface(TWSWinControlClass)
    procedure DragStart(const ACustomListBox: TCustomListBox);

    function GetIndexAtXY(const ACustomListBox: TCustomListBox; X, Y: integer): integer;
    function GetItemIndex(const ACustomListBox: TCustomListBox): integer;
    function GetItemRect(const ACustomListBox: TCustomListBox; Index: integer; var ARect: TRect): boolean;
    function GetScrollWidth(const ACustomListBox: TCustomListBox): Integer;
    function GetSelCount(const ACustomListBox: TCustomListBox): integer;
    function GetSelected(const ACustomListBox: TCustomListBox; const AIndex: integer): boolean;
    function GetStrings(const ACustomListBox: TCustomListBox): TStrings;
    procedure FreeStrings(var AStrings: TStrings);
    function GetTopIndex(const ACustomListBox: TCustomListBox): integer;

    imptype procedure SelectItem(const ACustomListBox: TCustomListBox;
      AIndex: integer; ASelected: boolean);
    procedure SelectRange(const ACustomListBox: TCustomListBox;
      ALow, AHigh: integer; ASelected: boolean);

    procedure SetBorder(const ACustomListBox: TCustomListBox);
    procedure SetColumnCount(const ACustomListBox: TCustomListBox; ACount: Integer);
    procedure SetItemIndex(const ACustomListBox: TCustomListBox; const AIndex: integer);
    procedure SetScrollWidth(const ACustomListBox: TCustomListBox; const AScrollWidth: Integer);
    procedure SetSelectionMode(const ACustomListBox: TCustomListBox; const AExtendedSelect,
      AMultiSelect: boolean);
    procedure SetStyle(const ACustomListBox: TCustomListBox);
    procedure SetSorted(const ACustomListBox: TCustomListBox; AList: TStrings; ASorted: boolean);
    procedure SetTopIndex(const ACustomListBox: TCustomListBox; const NewTopIndex: integer);
   end;
   {$endif}


  TWSCustomListBox = class(TWSWinControl{$ifdef wsintf},TWSCustomListBoxClass{$endif})
  impsection
    imptype procedure DragStart(const ACustomListBox: TCustomListBox); virtual;

    imptype function GetIndexAtXY(const ACustomListBox: TCustomListBox; X, Y: integer): integer; virtual;
    imptype function GetItemIndex(const ACustomListBox: TCustomListBox): integer; virtual;
    imptype function GetItemRect(const ACustomListBox: TCustomListBox; Index: integer; var ARect: TRect): boolean; virtual;
    imptype function GetScrollWidth(const ACustomListBox: TCustomListBox): Integer; virtual;
    imptype function GetSelCount(const ACustomListBox: TCustomListBox): integer; virtual;
    imptype function GetSelected(const ACustomListBox: TCustomListBox; const AIndex: integer): boolean; virtual;
    imptype function GetStrings(const ACustomListBox: TCustomListBox): TStrings; virtual;
    imptype procedure FreeStrings(var AStrings: TStrings); virtual;
    imptype function GetTopIndex(const ACustomListBox: TCustomListBox): integer; virtual;

    imptype procedure SelectItem(const ACustomListBox: TCustomListBox;
      AIndex: integer; ASelected: boolean); virtual;
    imptype procedure SelectRange(const ACustomListBox: TCustomListBox;
      ALow, AHigh: integer; ASelected: boolean); virtual;

    imptype procedure SetBorder(const ACustomListBox: TCustomListBox); virtual;
    imptype procedure SetColumnCount(const ACustomListBox: TCustomListBox; ACount: Integer); virtual;
    imptype procedure SetItemIndex(const ACustomListBox: TCustomListBox; const AIndex: integer); virtual;
    imptype procedure SetScrollWidth(const ACustomListBox: TCustomListBox; const AScrollWidth: Integer); virtual;
    imptype procedure SetSelectionMode(const ACustomListBox: TCustomListBox; const AExtendedSelect,
      AMultiSelect: boolean); virtual;
    imptype procedure SetStyle(const ACustomListBox: TCustomListBox); virtual;
    imptype procedure SetSorted(const ACustomListBox: TCustomListBox; AList: TStrings; ASorted: boolean); virtual;
    imptype procedure SetTopIndex(const ACustomListBox: TCustomListBox; const NewTopIndex: integer); virtual;
  end;
  {$ifndef wsintf}TWSCustomListBoxClass = class of TWSCustomListBox;{$endif}
  
  { TWSListBox }

  TWSListBox = class(TWSCustomListBox)
  impsection
  end;

  { TWSCustomEdit }
  {$ifdef wsintf}
  TWSCustomEditClass = interface(TWSWinControlClass)
    function GetCanUndo(const ACustomEdit: TCustomEdit): Boolean;
    function GetCaretPos(const ACustomEdit: TCustomEdit): TPoint;
    function GetSelStart(const ACustomEdit: TCustomEdit): integer;
    function GetSelLength(const ACustomEdit: TCustomEdit): integer;

    procedure SetAlignment(const ACustomEdit: TCustomEdit; const AAlignment: TAlignment);
    procedure SetCaretPos(const ACustomEdit: TCustomEdit; const NewPos: TPoint);
    procedure SetCharCase(const ACustomEdit: TCustomEdit; NewCase: TEditCharCase);
    procedure SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode);
    procedure SetHideSelection(const ACustomEdit: TCustomEdit; NewHideSelection: Boolean);
    procedure SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer);
    procedure SetNumbersOnly(const ACustomEdit: TCustomEdit; NewNumbersOnly: Boolean);
    procedure SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char);
    procedure SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean);
    procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer);
    procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer);
    procedure SetSelText(const ACustomEdit: TCustomEdit; const NewSelText: string);
    procedure SetTextHint(const ACustomEdit: TCustomEdit; const ATextHint: string);

    procedure Cut(const ACustomEdit: TCustomEdit);
    procedure Copy(const ACustomEdit: TCustomEdit);
    procedure Paste(const ACustomEdit: TCustomEdit);
    procedure Undo(const ACustomEdit: TCustomEdit);
  end;
  {$endif}

  TWSCustomEdit = class(TWSWinControl{$ifdef wsintf},TWSCustomEditClass{$endif})
  impsection
    imptype function GetCanUndo(const ACustomEdit: TCustomEdit): Boolean; virtual;
    imptype function GetCaretPos(const ACustomEdit: TCustomEdit): TPoint; virtual;
    imptype function GetSelStart(const ACustomEdit: TCustomEdit): integer; virtual;
    imptype function GetSelLength(const ACustomEdit: TCustomEdit): integer; virtual;

    imptype procedure SetAlignment(const ACustomEdit: TCustomEdit; const AAlignment: TAlignment); virtual;
    imptype procedure SetCaretPos(const ACustomEdit: TCustomEdit; const NewPos: TPoint); virtual;
    imptype procedure SetCharCase(const ACustomEdit: TCustomEdit; NewCase: TEditCharCase); virtual;
    imptype procedure SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode); virtual;
    imptype procedure SetHideSelection(const ACustomEdit: TCustomEdit; NewHideSelection: Boolean); virtual;
    imptype procedure SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer); virtual;
    imptype procedure SetNumbersOnly(const ACustomEdit: TCustomEdit; NewNumbersOnly: Boolean); virtual;
    imptype procedure SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char); virtual;
    imptype procedure SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean); virtual;
    imptype procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); virtual;
    imptype procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); virtual;
    imptype procedure SetSelText(const ACustomEdit: TCustomEdit; const NewSelText: string); virtual;
    imptype procedure SetTextHint(const ACustomEdit: TCustomEdit; const ATextHint: string); virtual;

    imptype procedure Cut(const ACustomEdit: TCustomEdit); virtual;
    imptype procedure Copy(const ACustomEdit: TCustomEdit); virtual;
    imptype procedure Paste(const ACustomEdit: TCustomEdit); virtual;
    imptype procedure Undo(const ACustomEdit: TCustomEdit); virtual;
  end;
  {$ifndef wsintf}TWSCustomEditClass = class of TWSCustomEdit;{$endif}

  { TWSCustomMemo }
  {$ifdef wsintf}
  TWSCustomMemoClass = interface(TWSCustomEditClass)
    procedure AppendText(const ACustomMemo: TCustomMemo; const AText: string);
    function  GetStrings(const ACustomMemo: TCustomMemo): TStrings;
    procedure FreeStrings(var AStrings: TStrings);
    procedure SetScrollbars(const ACustomMemo: TCustomMemo; const NewScrollbars: TScrollStyle);
    procedure SetWantTabs(const ACustomMemo: TCustomMemo; const NewWantTabs: boolean);
    procedure SetWantReturns(const ACustomMemo: TCustomMemo; const NewWantReturns: boolean);
    procedure SetWordWrap(const ACustomMemo: TCustomMemo; const NewWordWrap: boolean);
  end;
  {$endif}

  TWSCustomMemo = class(TWSCustomEdit{$ifdef wsintf},TWSCustomMemoClass{$endif})
  impsection
    imptype procedure AppendText(const ACustomMemo: TCustomMemo; const AText: string); virtual;
    imptype function  GetStrings(const ACustomMemo: TCustomMemo): TStrings; virtual;
    imptype procedure FreeStrings(var AStrings: TStrings); virtual;
    imptype procedure SetScrollbars(const ACustomMemo: TCustomMemo; const NewScrollbars: TScrollStyle); virtual;
    imptype procedure SetWantTabs(const ACustomMemo: TCustomMemo; const NewWantTabs: boolean); virtual;
    imptype procedure SetWantReturns(const ACustomMemo: TCustomMemo; const NewWantReturns: boolean); virtual;
    imptype procedure SetWordWrap(const ACustomMemo: TCustomMemo; const NewWordWrap: boolean); virtual;
    imptype procedure SetSelText(const ACustomEdit: TCustomEdit; const NewSelText: string); override;
  end;
  {$ifndef wsintf}TWSCustomMemoClass = class of TWSCustomMemo;{$endif}

  { TWSEdit }

  TWSEdit = class(TWSCustomEdit)
  impsection
  end;

  { TWSMemo }

  TWSMemo = class(TWSCustomMemo)
  impsection
  end;

  { TWSCustomStaticText }

  {$ifndef wsintf}
  TWSCustomStaticTextClass = class of TWSCustomStaticText;
  {$else}
  TWSCustomStaticTextClass = interface(TWSWinControlClass)
    procedure SetAlignment(const ACustomStaticText: TCustomStaticText; const NewAlignment: TAlignment);
    procedure SetStaticBorderStyle(const ACustomStaticText: TCustomStaticText; const NewBorderStyle: TStaticBorderStyle);
  end;
  {$endif}

  TWSCustomStaticText = class(TWSWinControl{$ifdef wsintf},TWSCustomStaticTextClass{$endif})
  impsection
    imptype procedure SetAlignment(const ACustomStaticText: TCustomStaticText; const NewAlignment: TAlignment); virtual;
    imptype procedure SetStaticBorderStyle(const ACustomStaticText: TCustomStaticText; const NewBorderStyle: TStaticBorderStyle); virtual;
    imptype function GetDefaultColor(const AControl: TControl;
      const ADefaultColorType: TDefaultColorType): TColor; override;
  end;

  { TWSStaticText }

  TWSStaticText = class(TWSCustomStaticText)
  impsection
  end;

  { TWSButtonControl }

  TWSButtonControl = class(TWSWinControl)
  impsection
    imptype function GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor; override;
  end;

  { TWSButton }
  {$ifdef wsintf}
  TWSButtonControlClass = interface(TWSWinControlClass)
  end;

  TWSButtonClass = interface(TWSButtonControlClass)
    procedure SetDefault(const AButton: TCustomButton; ADefault: Boolean);
    procedure SetShortCut(const AButton: TCustomButton; const ShortCutK1, ShortCutK2: TShortCut);
  end;
  {$endif}

  TWSButton = class(TWSButtonControl{$ifdef wsintf},TWSButtonClass{$endif})
  impsection
    imptype procedure SetDefault(const AButton: TCustomButton; ADefault: Boolean); virtual;
    imptype procedure SetShortCut(const AButton: TCustomButton; const ShortCutK1, ShortCutK2: TShortCut); virtual;
  end;
  {$ifndef wsintf}TWSButtonClass = class of TWSButton;{$endif}

  { TWSCustomCheckBox }

  {$ifdef wsintf}
  TWSCustomCheckBoxClass = interface(TWSButtonControlClass)
    function  RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState;
    procedure SetShortCut(const ACustomCheckBox: TCustomCheckBox; const ShortCutK1, ShortCutK2: TShortCut);
    procedure SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState);
    procedure SetAlignment(const ACustomCheckBox: TCustomCheckBox; const NewAlignment: TLeftRight);
  end;
  {$endif}

  TWSCustomCheckBox = class(TWSButtonControl{$ifdef wsintf},TWSCustomCheckBoxClass{$endif})
  impsection
    imptype function  RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState; virtual;
    imptype procedure SetShortCut(const ACustomCheckBox: TCustomCheckBox; const ShortCutK1, ShortCutK2: TShortCut); virtual;
    imptype procedure SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState); virtual;
    imptype procedure SetAlignment(const ACustomCheckBox: TCustomCheckBox; const NewAlignment: TLeftRight); virtual;
  end;
  {$ifndef wsintf}TWSCustomCheckBoxClass = class of TWSCustomCheckBox;{$endif}

  { TWSCheckBox }

  TWSCheckBox = class(TWSCustomCheckBox)
  impsection
  end;

  { TWSToggleBox }

  TWSToggleBox = class(TWSCustomCheckBox)
  impsection
  end;

  { TWSRadioButton }

  TWSRadioButton = class(TWSCustomCheckBox)
  impsection
  end;

  { WidgetSetRegistration }

  procedure RegisterCustomScrollBar;
  procedure RegisterCustomGroupBox;
  procedure RegisterCustomComboBox;
  procedure RegisterCustomListBox;
  procedure RegisterCustomEdit;
  procedure RegisterCustomMemo;
  procedure RegisterButtonControl;
  procedure RegisterCustomButton;
  procedure RegisterCustomCheckBox;
  procedure RegisterToggleBox;
  procedure RegisterRadioButton;
  procedure RegisterCustomStaticText;
  procedure RegisterCustomLabel;

implementation

uses
  LResources;

{ TWSGroupBox }

imptype function TWSGroupBox.GetDefaultColor(const AControl: TControl;
  const ADefaultColorType: TDefaultColorType): TColor;
begin
  Result:=DefBtnColors[ADefaultColorType];
end;

{ TWSScrollBar }

imptype procedure TWSScrollBar.SetParams(const AScrollBar: TCustomScrollBar);
begin
end;

imptype procedure TWSScrollBar.SetKind(const AScrollBar: TCustomScrollBar;
  const AIsHorizontal: Boolean);
begin
  RecreateWnd(AScrollBar);
end;

{ TWSCustomListBox }

imptype procedure TWSCustomListBox.DragStart(const ACustomListBox: TCustomListBox);
begin
end;

imptype function TWSCustomListBox.GetIndexAtXY(
  const ACustomListBox: TCustomListBox; X, Y: integer): integer;
begin
  Result := -1;
end;

imptype function  TWSCustomListBox.GetItemIndex(const ACustomListBox: TCustomListBox): integer;
begin
  Result := 0;
end;

imptype function TWSCustomListBox.GetItemRect(
  const ACustomListBox: TCustomListBox; Index: integer; var ARect: TRect
  ): boolean;
begin
  FillChar(ARect,SizeOf(ARect),0);
  Result:=false;
end;

imptype function TWSCustomListBox.GetScrollWidth(
  const ACustomListBox: TCustomListBox): Integer;
begin
  Result := 0;
end;

imptype function  TWSCustomListBox.GetSelCount(const ACustomListBox: TCustomListBox): integer;
begin
  Result := 0;
end;

imptype function  TWSCustomListBox.GetSelected(const ACustomListBox: TCustomListBox; const AIndex: integer): boolean;
begin
  Result := false;
end;

imptype function  TWSCustomListBox.GetStrings(const ACustomListBox: TCustomListBox): TStrings;
begin
  Result := nil;
end;

imptype procedure TWSCustomListBox.FreeStrings(var AStrings: TStrings);
begin
  AStrings.Free;
  AStrings := nil;
end;

imptype function  TWSCustomListBox.GetTopIndex(const ACustomListBox: TCustomListBox): integer;
begin
  Result := 0;
end;

imptype procedure TWSCustomListBox.SelectItem(const ACustomListBox: TCustomListBox;
  AIndex: integer; ASelected: boolean);
begin
end;

imptype procedure TWSCustomListBox.SelectRange(const ACustomListBox: TCustomListBox;
  ALow, AHigh: integer; ASelected: boolean);
var
  OldTopIndex, i: Integer;
begin  // A default implementation. A widgetset can override it with a better one.
  OldTopIndex := ACustomListBox.TopIndex; //prevent scrolling to last Item selected on Windows, Issue #0036929
  ACustomListBox.Items.BeginUpdate; //prevent visual update when selecting large ranges on Windows, Issue #0036929
  try
    for i := ALow to AHigh do
      SelectItem(ACustomListBox, i, ASelected);
    ACustomListBox.TopIndex := OldTopIndex;
  finally
    ACustomListBox.Items.EndUpdate;
  end;
end;

imptype procedure TWSCustomListBox.SetBorder(const ACustomListBox: TCustomListBox);
begin
end;

imptype procedure TWSCustomListBox.SetColumnCount(const ACustomListBox: TCustomListBox;
  ACount: Integer);
begin
end;

imptype procedure TWSCustomListBox.SetItemIndex(const ACustomListBox: TCustomListBox; const AIndex: integer);
begin
end;

imptype procedure TWSCustomListBox.SetScrollWidth(
  const ACustomListBox: TCustomListBox; const AScrollWidth: Integer);
begin

end;

imptype procedure TWSCustomListBox.SetSelectionMode(const ACustomListBox: TCustomListBox;
  const AExtendedSelect, AMultiSelect: boolean);
begin
end;

imptype procedure TWSCustomListBox.SetStyle(const ACustomListBox: TCustomListBox);
begin
end;

imptype procedure TWSCustomListBox.SetSorted(const ACustomListBox: TCustomListBox;
  AList: TStrings; ASorted: boolean);
begin
end;

imptype procedure TWSCustomListBox.SetTopIndex(const ACustomListBox: TCustomListBox;
  const NewTopIndex: integer);
begin
end;

{ TWSCustomComboBox }

imptype function TWSCustomComboBox.GetDroppedDown(
  const ACustomComboBox: TCustomComboBox): Boolean;
begin
  Result := False;
end;

imptype function  TWSCustomComboBox.GetSelStart(const ACustomComboBox: TCustomComboBox
  ): integer;
begin
  Result := -1;
end;

imptype function  TWSCustomComboBox.GetSelLength(const ACustomComboBox: TCustomComboBox
  ): integer;
begin
  Result := 0;
end;

imptype function  TWSCustomComboBox.GetItemIndex(const ACustomComboBox: TCustomComboBox
  ): integer;
begin
  Result := -1;
end;

imptype function  TWSCustomComboBox.GetMaxLength(const ACustomComboBox: TCustomComboBox
  ): integer;
begin
  Result := 0;
end;

imptype procedure TWSCustomComboBox.SetArrowKeysTraverseList(
  const ACustomComboBox: TCustomComboBox; NewTraverseList: boolean);
begin
end;

imptype procedure TWSCustomComboBox.SetDropDownCount(
  const ACustomComboBox: TCustomComboBox; NewCount: Integer);
begin
end;

imptype procedure TWSCustomComboBox.SetDroppedDown(
  const ACustomComboBox: TCustomComboBox; ADroppedDown: Boolean);
begin
end;

imptype procedure TWSCustomComboBox.SetMaxLength(const ACustomComboBox: TCustomComboBox;
  NewLength: integer);
begin
end;

imptype procedure TWSCustomComboBox.SetSelStart(const ACustomComboBox: TCustomComboBox;
  NewStart: integer);
begin
end;

imptype procedure TWSCustomComboBox.SetSelLength(const ACustomComboBox: TCustomComboBox;
  NewLength: integer);
begin
end;

imptype procedure TWSCustomComboBox.SetItemIndex(const ACustomComboBox: TCustomComboBox;
  NewIndex: integer);
begin
end;

imptype procedure TWSCustomComboBox.SetStyle(const ACustomComboBox: TCustomComboBox;
  NewStyle: TComboBoxStyle);
begin
end;

imptype procedure TWSCustomComboBox.SetReadOnly(const ACustomComboBox: TCustomComboBox;
  NewReadOnly: boolean);
begin
end;

imptype procedure TWSCustomComboBox.SetTextHint(
  const ACustomComboBox: TCustomComboBox; const ATextHint: string);
begin
end;

imptype function  TWSCustomComboBox.GetItems(const ACustomComboBox: TCustomComboBox
  ): TStrings;
begin
  Result := nil;
end;

imptype procedure TWSCustomComboBox.FreeItems(var AItems: TStrings);
begin
  AItems.Free;
  AItems := nil;
end;

imptype procedure TWSCustomComboBox.Sort(const ACustomComboBox: TCustomComboBox;
  AList: TStrings; IsSorted: boolean);
begin
end;

imptype function TWSCustomComboBox.GetItemHeight(const ACustomComboBox: TCustomComboBox): Integer;
begin
  Result := 0;
end;

imptype procedure TWSCustomComboBox.SetItemHeight(const ACustomComboBox: TCustomComboBox; const AItemHeight: Integer);
begin
end;

{ TWSCustomEdit }

imptype function TWSCustomEdit.GetCanUndo(const ACustomEdit: TCustomEdit
  ): Boolean;
begin
  Result := False;
end;

imptype function TWSCustomEdit.GetCaretPos(const ACustomEdit: TCustomEdit): TPoint;
begin
  Result := Point(0, 0);
end;

imptype function  TWSCustomEdit.GetSelStart(const ACustomEdit: TCustomEdit): integer;
begin
  result := -1;
end;

imptype function  TWSCustomEdit.GetSelLength(const ACustomEdit: TCustomEdit): integer;
begin
  result := 0;
end;

imptype procedure TWSCustomEdit.SetAlignment(const ACustomEdit: TCustomEdit;
  const AAlignment: TAlignment);
begin
end;

imptype procedure TWSCustomEdit.SetCaretPos(const ACustomEdit: TCustomEdit; const NewPos: TPoint);
begin
end;

imptype procedure TWSCustomEdit.SetCharCase(const ACustomEdit: TCustomEdit; NewCase: TEditCharCase);
begin
end;

imptype procedure TWSCustomEdit.SetEchoMode(const ACustomEdit: TCustomEdit; NewMode: TEchoMode);
begin
end;

imptype procedure TWSCustomEdit.SetHideSelection(const ACustomEdit: TCustomEdit;
  NewHideSelection: Boolean);
begin
end;

imptype procedure TWSCustomEdit.SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer);
begin
end;

imptype procedure TWSCustomEdit.SetNumbersOnly(const ACustomEdit: TCustomEdit;
  NewNumbersOnly: Boolean);
begin
end;

imptype procedure TWSCustomEdit.SetPasswordChar(const ACustomEdit: TCustomEdit; NewChar: char);
begin
end;

imptype procedure TWSCustomEdit.SetReadOnly(const ACustomEdit: TCustomEdit; NewReadOnly: boolean);
begin
end;

imptype procedure TWSCustomEdit.SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer);
begin
end;

imptype procedure TWSCustomEdit.SetTextHint(const ACustomEdit: TCustomEdit;
  const ATextHint: string);
begin
end;

imptype procedure TWSCustomEdit.SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer);
begin
end;

imptype procedure TWSCustomEdit.SetSelText(const ACustomEdit: TCustomEdit;
  const NewSelText: string);
var
  OldText, NewText: string;
  OldPos: Integer;
begin
  OldPos := ACustomEdit.SelStart;
  OldText := ACustomEdit.Text;
  NewText := UTF8Copy(OldText, 1, OldPos) +
             NewSelText +
             UTF8Copy(OldText, OldPos + ACustomEdit.SelLength + 1, MaxInt);
  ACustomEdit.Text := NewText;
  ACustomEdit.SelStart := OldPos + UTF8Length(NewSelText);
end;

imptype procedure TWSCustomEdit.Cut(const ACustomEdit: TCustomEdit);
begin
  ACustomEdit.CopyToClipboard;
  ACustomEdit.ClearSelection;
end;

imptype procedure TWSCustomEdit.Copy(const ACustomEdit: TCustomEdit);
begin
  if (ACustomEdit.EchoMode = emNormal) and (ACustomEdit.SelLength > 0) then
    Clipboard.AsText := ACustomEdit.SelText;
end;

imptype procedure TWSCustomEdit.Paste(const ACustomEdit: TCustomEdit);
begin
  if Clipboard.HasFormat(CF_TEXT) then
    ACustomEdit.SelText := Clipboard.AsText;
end;

imptype procedure TWSCustomEdit.Undo(const ACustomEdit: TCustomEdit);
begin
  // nothing
end;

{ TWSCustomMemo }

imptype procedure TWSCustomMemo.AppendText(const ACustomMemo: TCustomMemo; const AText: string);
begin
end;

imptype function TWSCustomMemo.GetStrings(const ACustomMemo: TCustomMemo): TStrings;
begin
  Result := ACustomMemo.Lines; //use default if the WS has not defined any
end;

imptype procedure TWSCustomMemo.FreeStrings(var AStrings: TStrings);
begin
  AStrings.Free;
  AStrings := nil;
end;

imptype procedure TWSCustomMemo.SetScrollbars(const ACustomMemo: TCustomMemo; const NewScrollbars: TScrollStyle);
begin
end;

imptype procedure TWSCustomMemo.SetSelText(const ACustomEdit: TCustomEdit;
  const NewSelText: string);
begin
  TCustomMemo(ACustomEdit).Lines.BeginUpdate;
  try
    {$ifndef wsintf}TWSCustomEdit.{$else}inherited {$endif}SetSelText(ACustomEdit, NewSelText);
  finally
    TCustomMemo(ACustomEdit).Lines.EndUpdate;
  end;
end;

imptype procedure TWSCustomMemo.SetWantTabs(const ACustomMemo: TCustomMemo; const NewWantTabs: boolean);
begin
end;

imptype procedure TWSCustomMemo.SetWantReturns(const ACustomMemo: TCustomMemo; const NewWantReturns: boolean);
begin
end;

imptype procedure TWSCustomMemo.SetWordWrap(const ACustomMemo: TCustomMemo; const NewWordWrap: boolean);
begin
end;

{ TWSCustomStaticText }

imptype procedure TWSCustomStaticText.SetAlignment(const ACustomStaticText: TCustomStaticText; const NewAlignment: TAlignment);
begin
end;

imptype procedure TWSCustomStaticText.SetStaticBorderStyle(
  const ACustomStaticText: TCustomStaticText;
  const NewBorderStyle: TStaticBorderStyle);
begin
  // nothing
end;

imptype function TWSCustomStaticText.GetDefaultColor(const AControl: TControl;
  const ADefaultColorType: TDefaultColorType): TColor;
begin
  Result:=DefBtnColors[ADefaultColorType];
end;

{ TWSButton }

imptype procedure TWSButton.SetDefault(const AButton: TCustomButton; ADefault: Boolean);
begin
end;

imptype procedure TWSButton.SetShortCut(const AButton: TCustomButton;
  const ShortCutK1, ShortCutK2: TShortCut);
begin;
end;

{ TWSCustomCheckBox }

imptype function  TWSCustomCheckBox.RetrieveState(const ACustomCheckBox: TCustomCheckBox): TCheckBoxState;
begin
  Result := cbUnchecked;
end;

imptype procedure TWSCustomCheckBox.SetShortCut(const ACustomCheckBox: TCustomCheckBox;
  const ShortCutK1, ShortCutK2: TShortCut);
begin
end;

imptype procedure TWSCustomCheckBox.SetState(const ACustomCheckBox: TCustomCheckBox; const NewState: TCheckBoxState);
begin
end;

imptype procedure TWSCustomCheckBox.SetAlignment(
  const ACustomCheckBox: TCustomCheckBox; const NewAlignment: TLeftRight);
begin
end;

{ WidgetSetRegistration }

procedure RegisterCustomScrollBar;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomScrollBar;
//  if not WSRegisterCustomScrollBar then
//    RegisterWSComponent(TCustomScrollBar, TWSCustomScrollBar);
  Done := True;
end;

procedure RegisterCustomGroupBox;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomGroupBox;
//  if not WSRegisterCustomGroupBox then
//    RegisterWSComponent(TCustomGroupBox, TWSCustomGroupBox);
  Done := True;
end;

procedure RegisterCustomComboBox;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomComboBox;
//  if not WSRegisterCustomComboBox then
//    RegisterWSComponent(TCustomComboBox, TWSCustomComboBox);
  Done := True;
end;

procedure RegisterCustomListBox;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomListBox;
//  if not WSRegisterCustomListBox then
//    RegisterWSComponent(TCustomListBox, TWSCustomListBox);
  Done := True;
end;

procedure RegisterCustomEdit;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomEdit;
//  if not WSRegisterCustomEdit then
//    RegisterWSComponent(TCustomEdit, TWSCustomEdit);
  Done := True;
end;

procedure RegisterCustomMemo;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomMemo;
  RegisterPropertyToSkip(TCustomMemo, 'BevelInner', 'VCL compatibility property', '');
  RegisterPropertyToSkip(TCustomMemo, 'BevelOuter', 'VCL compatibility property', '');
  RegisterPropertyToSkip(TCustomMemo, 'BevelEdges', 'VCL compatibility property', '');
  RegisterPropertyToSkip(TCustomMemo, 'Margins',    'VCL compatibility property', '');
//  if not WSRegisterCustomMemo then
//    RegisterWSComponent(TCustomMemo, TWSCustomMemo);
  Done := True;
end;

procedure RegisterButtonControl;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterButtonControl;
  RegisterPropertyToSkip(TButtonControl, 'UseOnChange',
    'Removed in 0.9.27. It was an old workaround which is not needed anymore.',
    '');
//  if not WSRegisterButtonControl then
//    RegisterWSComponent(TButtonControl, TWSButtonControl);
  Done := True;
end;

procedure RegisterCustomButton;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomButton;
//  if not WSRegisterCustomButton then
//    RegisterWSComponent(TCustomButton, TWSButton);
  Done := True;
end;

procedure RegisterCustomCheckBox;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomCheckBox;
  RegisterPropertyToSkip(TCustomCheckBox, 'Alignment', 'VCL compatibility property', '');
  RegisterPropertyToSkip(TCustomCheckBox, 'WordWrap', 'VCL compatibility property', '');
//  if not WSRegisterCustomCheckBox then
//    RegisterWSComponent(TCustomCheckBox, TWSCustomCheckBox);
  Done := True;
end;

procedure RegisterToggleBox;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterToggleBox;
//  if not WSRegisterToggleBox then
//    RegisterWSComponent(TToggleBox, TWSToggleBox);
  Done := True;
end;

procedure RegisterRadioButton;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterRadioButton;
  RegisterPropertyToSkip(TRadioButton, 'State', 'Removed in 0.9.29. It should not be allowed to set the State directly', '');
  RegisterPropertyToSkip(TRadioButton, 'AllowGrayed', 'Removed in 0.9.29. Grayed state is not supported by TRadioButton', '');
//  if not WSRegisterRadioButton then
//    RegisterWSComponent(TRadioButton, TWSRadioButton);
  Done := True;
end;

procedure RegisterCustomStaticText;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomStaticText;
//  if not WSRegisterCustomStaticText then
//    RegisterWSComponent(TCustomStaticText, TWSCustomStaticText);
  Done := True;
end;

procedure RegisterCustomLabel;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterCustomLabel;
//  if not WSRegisterCustomLabel then
//    RegisterWSComponent(TCustomLabel, TWSCustomLabel);
  Done := True;
end;

{ TWSButtonControl }

imptype function TWSButtonControl.GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor;
begin
  Result := DefBtnColors[ADefaultColorType];
end;

end.
