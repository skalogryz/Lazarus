{  $Id$  }
{
 /***************************************************************************
                             pairsplitter.pas
                             ----------------
                        Component Library Controls


 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner
  
  Abstract:
    TPairSplitter component. A component with two TPairSplitterSide children.
    Both child components can contain other components and the children are
    divided by a splitter which can be dragged by the user.
}
unit PairSplitter;

{$mode objfpc}{$H+}

interface

uses
  Types, Classes, SysUtils,
  // LazUtils
  LazTracer,
  // LCL
  LCLType, LCLIntf, LMessages, Graphics, Controls, ExtCtrls;

type
  TCustomPairSplitter = class;

  { TPairSplitterSide }
  
  TPairSplitterSide = class(TWinControl)
  private
    function GetSplitter: TCustomPairSplitter;
  protected
    class procedure WSRegisterClass; override;
    procedure SetParent(AParent: TWinControl); override;
    procedure WMPaint(var PaintMessage: TLMPaint); message LM_PAINT;
    procedure Paint; virtual;
    property Align;
    property Anchors;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  public
    property Splitter: TCustomPairSplitter read GetSplitter;
    property Visible;
    property Left;
    property Top;
    property Width;
    property Height;
  published
    property ChildSizing;
    property ClientWidth;
    property ClientHeight;
    property Constraints;
    property Cursor;
    property Enabled;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property ShowHint;
    property ParentShowHint;
    property PopupMenu;
  end;
  
  { TCustomPairSplitter }
  
  TPairSplitterType = (
    pstHorizontal,
    pstVertical
    );
    
  TCustomPairSplitter = class(TWinControl)
  private
    FPosition: integer;
    FSides: array[0..1] of TPairSplitterSide;
    FSplitterType: TPairSplitterType;
    FDoNotCreateSides: boolean;
    FLoadCursor: TCursor;
    function GetPosition: integer;
    function GetSides(Index: integer): TPairSplitterSide;
    procedure SetPosition(const AValue: integer);
    procedure SetSplitterType(const AValue: TPairSplitterType);
    procedure AddSide(ASide: TPairSplitterSide);
    procedure RemoveSide(ASide: TPairSplitterSide);
  protected
    class procedure WSRegisterClass; override;
    function GetCursor: TCursor; override;
    procedure SetCursor(Value: TCursor); override;
    class function GetControlClassDefaultSize: TSize; override;
    function WSAddSide(ASide: TPairSplitterSide; Side: integer): Boolean;
    function WSGetPosition: Integer;
    function WSSetPosition(var NewPosition: integer): Boolean;
    function WSRemoveSide(ASide: TPairSplitterSide; Side: integer): Boolean;
    function WSGetSplitterCursor(var ACursor: TCursor): Boolean;
    function WSSetSplitterCursor(ACursor: TCursor): Boolean;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateWnd; override;
    procedure UpdatePosition;
    procedure CreateSides;
    procedure Loaded; override;
    function ChildClassAllowed(ChildClass: TClass): boolean; override;
  public
    property Cursor default crHSplit;
    property Sides[Index: integer]: TPairSplitterSide read GetSides;
    property SplitterType: TPairSplitterType read FSplitterType
                                    write SetSplitterType default pstHorizontal;
    property Position: integer read GetPosition write SetPosition;
  end;
  
  
  { TPairSplitter }

  TPairSplitter = class(TCustomPairSplitter)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Constraints;
    property Color;
    property Cursor;
    property Enabled;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnChangeBounds;
    property ParentShowHint;
    property PopupMenu;
    property Position;
    property ShowHint;
    property SplitterType;
    property Visible;
  end;
  
procedure Register;
  
implementation

uses
  WSPairSplitter;
  
procedure Register;
begin
  RegisterComponents('Additional',[TPairSplitter]);
  RegisterNoIcon([TPairSplitterSide]);
end;

function GetInternalSplitter(ASplitter: TCustomPairSplitter): TSplitter;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to ASplitter.ControlCount - 1 do
    if ASplitter.Controls[i] is TSplitter then
    begin
      Result := TSplitter(ASplitter.Controls[i]);
      break;
    end;
end;

{ TPairSplitterSide }

function TPairSplitterSide.GetSplitter: TCustomPairSplitter;
begin
  if Parent is TCustomPairSplitter then
    Result:=TCustomPairSplitter(Parent)
  else
    Result:=nil;
end;

class procedure TPairSplitterSide.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterPairSplitterSide;
end;

procedure TPairSplitterSide.SetParent(AParent: TWinControl);
var
  ASplitter: TCustomPairSplitter;
  DeletingSplitter: Boolean;
begin
  CheckNewParent(AParent);
  // remove from side list of old parent
  ASplitter := Splitter;
  if ASplitter <> nil then begin
    ASplitter.RemoveSide(Self);
    DeletingSplitter := (csDestroying in ASplitter.ComponentState)
                     or (wcfDesignerDeleting in FWinControlFlags);
  end
  else
    DeletingSplitter := False;

  inherited SetParent(AParent);

  if not DeletingSplitter then begin
    // add to side list of new parent
    ASplitter:=Splitter;
    if ASplitter <> nil then
      ASplitter.AddSide(Self);
  end;
end;

procedure TPairSplitterSide.WMPaint(var PaintMessage: TLMPaint);
begin
  if (csDestroying in ComponentState) or (not HandleAllocated) then
    Exit;
  Include(FControlState, csCustomPaint);
  inherited WMPaint(PaintMessage);
  Paint;
  Exclude(FControlState, csCustomPaint);
end;

procedure TPairSplitterSide.Paint;
var
  ACanvas: TControlCanvas;
begin
  if csDesigning in ComponentState then
  begin
    ACanvas := TControlCanvas.Create;
    with ACanvas do
    begin
      Control := Self;
      Pen.Style := psDash;
      Frame(0,0,Width-1,Height-1);
      Free;
    end;
  end;
end;

constructor TPairSplitterSide.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FCompStyle := csPairSplitterSide;
  ControlStyle := ControlStyle + [csAcceptsControls];
  // A flag custom made for TPairSplitterSide.
  Include(FWinControlFlags, wcfSpecialSubControl);
end;

destructor TPairSplitterSide.Destroy;
begin
  inherited Destroy;
end;

{ TCustomPairSplitter }

function TCustomPairSplitter.GetSides(Index: integer): TPairSplitterSide;
begin
  if (Index < 0) or (Index > 1) then
    RaiseGDBException('TCustomPairSplitter.GetSides: Index out of bounds');
  Result := FSides[Index];
end;

function TCustomPairSplitter.GetPosition: integer;
begin
  if HandleAllocated and (not (csLoading in ComponentState)) then
    UpdatePosition;
  Result := FPosition;
end;

procedure TCustomPairSplitter.SetPosition(const AValue: integer);
begin
  if (FPosition = AValue) and
    (WSGetPosition() = FPosition)
  then
    Exit;

  FPosition := AValue;
  if FPosition < 0 then
    FPosition := 0;
  if HandleAllocated and (not (csLoading in ComponentState)) then
    WSSetPosition(FPosition);
end;

procedure TCustomPairSplitter.SetSplitterType(const AValue: TPairSplitterType);
const
  DefaultCursors: array[TPairSplitterType] of TCursor =
  (
{ pstHorizontal } crHSplit,
{ pstVertical   } crVSplit
  );
begin
  if FSplitterType = AValue then
    Exit;

  if Cursor = DefaultCursors[FSplitterType] then
    Cursor := DefaultCursors[AValue];

  FSplitterType := AValue;
  
  // TODO: Remove RecreateWnd
  if HandleAllocated then
    RecreateWnd(Self);
end;

procedure TCustomPairSplitter.AddSide(ASide: TPairSplitterSide);
var
  i: Integer;
begin
  if ASide = nil then
    Exit;
  i := Low(FSides);
  repeat
    if FSides[i] = ASide then
      Exit;
    if FSides[i] =nil then
    begin
      FSides[i] := ASide;
      if HandleAllocated then
        WSAddSide(ASide, i);
      break;
    end;
    inc(i);
    if i > High(FSides) then
    RaiseGDBException('TCustomPairSplitter.AddSide no free side left');
  until False;
end;

procedure TCustomPairSplitter.RemoveSide(ASide: TPairSplitterSide);
var
  i: Integer;
begin
  if ASide = nil then
    Exit;
  for i := Low(FSides) to High(FSides) do
    if FSides[i]=ASide then
    begin
      if HandleAllocated and ASide.HandleAllocated then
        WSRemoveSide(ASide, i);
      FSides[i] := nil;
    end;
  // if the user deletes a side at designtime, autocreate a new one
  if (ComponentState * [csDesigning,csDestroying] = [csDesigning])
  and not (wcfDesignerDeleting in FWinControlFlags) then
    CreateSides;
end;

class procedure TCustomPairSplitter.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCustomPairSplitter;
end;

function TCustomPairSplitter.GetCursor: TCursor;
begin
  // Paul Ishenin: I do not know another method to tell internal splitter about
  // cursor changes
  
  // if widgetset class do not want to get cursor (has no internal splitter) then
  // use default lcl handler
  if not WSGetSplitterCursor(Result) then
    Result := inherited GetCursor;
end;

procedure TCustomPairSplitter.SetCursor(Value: TCursor);
begin
  FLoadCursor := Value;
  if not HandleAllocated then
    Exit;
  // if widgetset class do not want to set cursor (has no internal splitter) then
  // use default lcl handler
  if not WSSetSplitterCursor(Value) then
    inherited SetCursor(Value);
end;

class function TCustomPairSplitter.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 90;
  Result.CY := 90;
end;

constructor TCustomPairSplitter.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FCompStyle := csPairSplitter;
  ControlStyle := ControlStyle - [csAcceptsControls];
  FSplitterType := pstHorizontal;
  Cursor := crHSplit;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  FPosition:=45;
  if not (csDesigning in ComponentState) then
    CreateSides;
end;

destructor TCustomPairSplitter.Destroy;
begin
  fDoNotCreateSides:=true;
  inherited Destroy;
end;

procedure TCustomPairSplitter.CreateWnd;
{}
var
  i: Integer;
  APosition: Integer;
begin
  CreateSides;
  inherited CreateWnd;
  for i := Low(FSides) to High(FSides) do
    if FSides[i] <> nil then
      WSAddSide(FSides[i], i);
  APosition := FPosition;
  WSSetPosition(APosition);
  SetCursor(FLoadCursor);
  if not (csLoading in ComponentState) then
    FPosition := APosition;
end;

procedure TCustomPairSplitter.UpdatePosition;
var
  CurPosition: Integer;
begin
  if HandleAllocated then
  begin
    CurPosition := -1;
    WSSetPosition(CurPosition);
    FPosition := CurPosition;
  end;
end;

procedure TCustomPairSplitter.CreateSides;
var
  ASide: TPairSplitterSide;
  i: Integer;
begin
  if fDoNotCreateSides or (ComponentState * [csLoading,csDestroying] <> [])
  or ((Owner<>nil) and (csLoading in Owner.ComponentState)) then exit;
  // create the missing side controls
  for i := Low(FSides) to High(FSides) do
    if FSides[i]=nil then
    begin
      // For streaming it is important that the side controls are owned by
      // the owner of the splitter
      ASide:=TPairSplitterSide.Create(Owner);
      ASide.Parent:=Self;
    end;
end;

procedure TCustomPairSplitter.Loaded;
begin
  inherited Loaded;
  CreateSides;
  if HandleAllocated then
    WSSetPosition(FPosition);
end;

function TCustomPairSplitter.ChildClassAllowed(ChildClass: TClass): boolean;
begin
  Result := ChildClass.InheritsFrom(TPairSplitterSide) or
            ChildClass.InheritsFrom(TSplitter);
end;


function TCustomPairSplitter.WSAddSide(ASide: TPairSplitterSide; Side: integer): Boolean;
var
  sp : TWSCustomPairSplitterClass;
  InternalSplitter: TSplitter;
begin
  sp := GetWSCustomPairSplitter(WidgetSetClass);
  if Assigned(sp) then
    Result := sp.AddSide(Self, ASide, Side)
  else begin
    Result := false;
    if (Side < 0) or (Side > 1) then exit;

    if Side = 0 then
    begin
      if SplitterType = pstHorizontal then
        ASide.Align := alLeft
      else
        ASide.Align := alTop;
    end else begin
      InternalSplitter := GetInternalSplitter(Self);
      if InternalSplitter = nil then
      begin
        InternalSplitter := TSplitter.Create(Self);
        InternalSplitter.AutoSnap := False;
        InternalSplitter.MinSize := 1;
        InternalSplitter.Parent := Self;
      end;
      InternalSplitter.Align := Self.Sides[0].Align;
      if SplitterType = pstHorizontal then
        InternalSplitter.Left := Self.Sides[0].Width + 1
      else
        InternalSplitter.Top := Sides[0].Height + 1;
      ASide.Align := alClient;
    end;
    Result := True;
  end;
end;

function TCustomPairSplitter.WSGetPosition: Integer;
var
  sp : TWSCustomPairSplitterClass;
  InternalSplitter: TSplitter;
begin
  sp := GetWSCustomPairSplitter(WidgetSetClass);
  if Assigned(sp) then
    Result := sp.GetPosition(Self)
  else begin
    if not HandleAllocated then
      Result := Position
    else if SplitterType = pstHorizontal then
      Result := Sides[0].Width
    else
      Result := Sides[0].Height;
  end;
end;

function TCustomPairSplitter.WSSetPosition(var NewPosition: integer): Boolean;
var
  sp : TWSCustomPairSplitterClass;
  InternalSplitter: TSplitter;
begin
  sp := GetWSCustomPairSplitter(WidgetSetClass);
  if Assigned(sp) then
    Result := sp.SetPosition(Self, NewPosition)
  else begin
    Result := False;
    if not HandleAllocated then Exit;

    if NewPosition >= 0 then
    begin
      InternalSplitter := GetInternalSplitter(Self);
      if SplitterType = pstHorizontal then
      begin
        Sides[0].Width := NewPosition;
        if InternalSplitter <> nil then
          InternalSplitter.Left := NewPosition + 1;
      end else
      begin
        Sides[0].Height := NewPosition;
        if InternalSplitter <> nil then
          InternalSplitter.Top := NewPosition + 1;
      end;
    end;
    if SplitterType = pstHorizontal then
      NewPosition := Sides[0].Width
    else
      NewPosition := Sides[0].Height;

    Result := True;
  end;
end;

function TCustomPairSplitter.WSRemoveSide(ASide: TPairSplitterSide;
  Side: integer): Boolean;
var
  sp : TWSCustomPairSplitterClass;
begin
  sp := GetWSCustomPairSplitter(WidgetSetClass);
  if Assigned(sp) then
    Result := sp.RemoveSide(Self, ASide, Side)
  else
    Result := false;
end;

function TCustomPairSplitter.WSGetSplitterCursor(var ACursor: TCursor): Boolean;
var
  sp : TWSCustomPairSplitterClass;
  InternalSplitter: TSplitter;
begin
  sp := GetWSCustomPairSplitter(WidgetSetClass);
  if Assigned(sp) then begin
    Result := sp.GetSplitterCursor(Self, ACursor);
    Exit;
  end;
  Result := True;
  InternalSplitter := GetInternalSplitter(Self);
  if InternalSplitter <> nil then
    ACursor := InternalSplitter.Cursor
  else
    ACursor := crDefault;
end;

function TCustomPairSplitter.WSSetSplitterCursor(ACursor: TCursor): Boolean;
var
  sp : TWSCustomPairSplitterClass;
  InternalSplitter: TSplitter;
begin
  sp := GetWSCustomPairSplitter(WidgetSetClass);
  if Assigned(sp) then begin
    Result := sp.SetSplitterCursor(Self, ACursor);
    Exit;
  end;
  Result := True;
  InternalSplitter := GetInternalSplitter(Self);
  if InternalSplitter <> nil then
  begin
    InternalSplitter.Cursor := ACursor;
    Sides[0].Cursor := crArrow;
    Sides[1].Cursor := crArrow;
  end;
end;

end.
