{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

Authors: Alexander Klenin

}
unit TALegend;

{$H+}

interface

uses
  Classes, Contnrs, FPCanvas, Graphics, SysUtils,
  TAChartUtils, TADrawUtils, TATypes;

const
  DEF_LEGEND_SPACING = 4;
  DEF_LEGEND_MARGIN = 4;
  DEF_LEGEND_SYMBOL_WIDTH = 20;
  LEGEND_ITEM_ORDER_AS_ADDED = -1;

type
  { TLegendItem }

  TLegendItem = class
  private
    FColor: TColor;
    FOrder: Integer;
    FText: String;
  public
    constructor Create(const AText: String; AColor: TColor = clTAColor);
    procedure Draw(ADrawer: IChartDrawer; const ARect: TRect); virtual;
  public
    property Color: TColor read FColor write FColor;
    property Order: Integer read FOrder write FOrder;
  end;

  TLegendItemDrawEvent = procedure (
    ACanvas: TCanvas; const ARect: TRect; AIndex: Integer; var AText: String
  ) of object;

  { TLegendItemUserDrawn }

  TLegendItemUserDrawn = class(TLegendItem)
  private
    FIndex: Integer;
    FOnDraw: TLegendItemDrawEvent;
  public
    constructor Create(
      AIndex: Integer; AOnDraw: TLegendItemDrawEvent; const AText: String);
    procedure Draw(ADrawer: IChartDrawer; const ARect: TRect); override;
    property OnDraw: TLegendItemDrawEvent read FOnDraw;
  end;

  { TLegendItemLine }

  TLegendItemLine = class(TLegendItem)
  private
    FPen: TFPCustomPen;
  public
    constructor Create(APen: TFPCustomPen; const AText: String);
    procedure Draw(ADrawer: IChartDrawer; const ARect: TRect); override;
  end;

  { TLegendItemLinePointer }

  TLegendItemLinePointer = class(TLegendItemLine)
  protected
    FPointer: TSeriesPointer;
  public
    constructor Create(
      APen: TPen; APointer: TSeriesPointer; const AText: String);
    procedure Draw(ADrawer: IChartDrawer; const ARect: TRect); override;
  end;

  { TLegendItemBrushRect }

  TLegendItemBrushRect = class(TLegendItem)
  private
    FBrush: TFPCustomBrush;
  public
    constructor Create(ABrush: TFPCustomBrush; const AText: String);
    procedure Draw(ADrawer: IChartDrawer; const ARect: TRect); override;
  end;

  { TChartLegendItems }

  TChartLegendItems = class(TObjectList)
  private
    function GetItem(AIndex: Integer): TLegendItem;
    procedure SetItem(AIndex: Integer; AValue: TLegendItem);
  public
    property Items[AIndex: Integer]: TLegendItem
      read GetItem write SetItem; default;
  end;

  TChartLegendBrush = class(TBrush)
  published
    property Color default clWhite;
  end;

  TLegendAlignment = (
    laTopLeft, laCenterLeft, laBottomLeft,
    laTopCenter, laBottomCenter, // laCenterCenter makes no sense.
    laTopRight, laCenterRight, laBottomRight);

  { TChartLegend }

  TChartLegend = class(TChartElement)
  private
    FAlignment: TLegendAlignment;
    FBackgroundBrush: TChartLegendBrush;
    FFont: TFont;
    FFrame: TChartPen;
    FMarginX: TChartDistance;
    FMarginY: TChartDistance;
    FSpacing: TChartDistance;
    FSymbolFrame: TChartPen;
    FSymbolWidth: TChartDistance;
    FUseSidebar: Boolean;

    procedure SetAlignment(AValue: TLegendAlignment);
    procedure SetBackgroundBrush(AValue: TChartLegendBrush);
    procedure SetFont(AValue: TFont);
    procedure SetFrame(AValue: TChartPen);
    procedure SetMargin(AValue: TChartDistance);
    procedure SetMarginX(AValue: TChartDistance);
    procedure SetMarginY(AValue: TChartDistance);
    procedure SetSpacing(AValue: TChartDistance);
    procedure SetSymbolFrame(AValue: TChartPen);
    procedure SetSymbolWidth(AValue: TChartDistance);
    procedure SetUseSidebar(AValue: Boolean);
  public
    constructor Create(AOwner: TCustomChart);
    destructor Destroy; override;

  public
    procedure Assign(Source: TPersistent); override;
    procedure Draw(
      ADrawer: IChartDrawer; AItems: TChartLegendItems; const ABounds: TRect);
    function MeasureItem(
      ADrawer: IChartDrawer; AItems: TChartLegendItems): TPoint;
    function Prepare(
      ADrawer: IChartDrawer; AItems: TChartLegendItems;
      var AClipRect: TRect): TRect;
    // Not includes the margins around item.
  published
    property Alignment: TLegendAlignment
      read FAlignment write SetAlignment default laTopRight;
    property BackgroundBrush: TChartLegendBrush
      read FBackgroundBrush write SetBackgroundBrush;
    property Font: TFont read FFont write SetFont;
    property Frame: TChartPen read FFrame write SetFrame;
    property Margin: TChartDistance
      read FMarginX write SetMargin stored false; deprecated;
    property MarginX: TChartDistance
      read FMarginX write SetMarginX default DEF_LEGEND_MARGIN;
    property MarginY: TChartDistance
      read FMarginY write SetMarginY default DEF_LEGEND_MARGIN;
    property Spacing: TChartDistance
      read FSpacing write SetSpacing default DEF_LEGEND_SPACING;
    property SymbolFrame: TChartPen read FSymbolFrame write SetSymbolFrame;
    property SymbolWidth: TChartDistance
      read FSymbolWidth write SetSymbolWidth default DEF_LEGEND_SYMBOL_WIDTH;
    property UseSidebar: Boolean read FUseSidebar write SetUseSidebar default true;
    property Visible default false;
  end;

  TLegendMultiplicity = (lmSingle, lmPoint);

  { TChartSeriesLegend }

  TChartSeriesLegend = class(TChartElement)
  private
    FMultiplicity: TLegendMultiplicity;
    FOnDraw: TLegendItemDrawEvent;
    FOrder: Integer;
    FUserItemsCount: Integer;
    procedure SetMultiplicity(AValue: TLegendMultiplicity);
    procedure SetOnDraw(AValue: TLegendItemDrawEvent);
    procedure SetOrder(AValue: Integer);
    procedure SetUserItemsCount(AValue: Integer);
  public
    constructor Create(AOwner: TCustomChart);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Multiplicity: TLegendMultiplicity
      read FMultiplicity write SetMultiplicity default lmSingle;
    property Order: Integer
      read FOrder write SetOrder default LEGEND_ITEM_ORDER_AS_ADDED;
    property UserItemsCount: Integer
      read FUserItemsCount write SetUserItemsCount default 1;
    property Visible default true;

  published
    property OnDraw: TLegendItemDrawEvent read FOnDraw write SetOnDraw;
  end;

  function LegendItemCompare(AItem1, AItem2: Pointer): Integer;

implementation

uses
  Math, PropEdits, Types, TADrawerCanvas;

const
  SYMBOL_TEXT_SPACING = 4;

function LegendItemCompare(AItem1, AItem2: Pointer): Integer;
begin
  Result := Sign(TLegendItem(AItem1).Order - TLegendItem(AItem2).Order);
end;

{ TChartLegendItems }

function TChartLegendItems.GetItem(AIndex: Integer): TLegendItem;
begin
  Result := TLegendItem(inherited GetItem(AIndex));
end;

procedure TChartLegendItems.SetItem(AIndex: Integer; AValue: TLegendItem);
begin
  inherited;
end;

{ TLegendItem }

constructor TLegendItem.Create(const AText: String; AColor: TColor);
begin
  FColor := AColor;
  FOrder := LEGEND_ITEM_ORDER_AS_ADDED;
  FText := AText;
end;

procedure TLegendItem.Draw(ADrawer: IChartDrawer; const ARect: TRect);
begin
  ADrawer.TextOut.
    Pos(ARect.Right + SYMBOL_TEXT_SPACING, ARect.Top).Text(FText).Done;
end;

{ TLegendItemUserDrawn }

constructor TLegendItemUserDrawn.Create(
  AIndex: Integer; AOnDraw: TLegendItemDrawEvent; const AText: String);
begin
  inherited Create(AText);
  FIndex := AIndex;
  FOnDraw := AOnDraw;
end;

procedure TLegendItemUserDrawn.Draw(ADrawer: IChartDrawer; const ARect: TRect);
var
  ic: IChartTCanvasDrawer;
begin
  if Supports(ADrawer, IChartTCanvasDrawer, ic) and Assigned(FOnDraw) then
    FOnDraw(ic.Canvas, ARect, FIndex, FText);
  inherited Draw(ADrawer, ARect);
end;

{ TLegendItemLine }

constructor TLegendItemLine.Create(APen: TFPCustomPen; const AText: String);
begin
  inherited Create(AText);
  FPen := APen;
end;

procedure TLegendItemLine.Draw(ADrawer: IChartDrawer; const ARect: TRect);
var
  y: Integer;
begin
  inherited Draw(ADrawer, ARect);
  if FPen = nil then exit;
  ADrawer.Pen := FPen;
  y := (ARect.Top + ARect.Bottom) div 2;
  ADrawer.Line(ARect.Left, y, ARect.Right, y);
end;

{ TLegendItemLinePointer }

constructor TLegendItemLinePointer.Create(
  APen: TPen; APointer: TSeriesPointer; const AText: String);
begin
  inherited Create(APen, AText);
  FPointer := APointer;
end;

procedure TLegendItemLinePointer.Draw(
  ADrawer: IChartDrawer; const ARect: TRect);
var
  c, sz: TPoint;
begin
  inherited Draw(ADrawer, ARect);
  if FPointer = nil then exit;
  c := CenterPoint(ARect);
  // Max width slightly narrower then ARect to leave place for the line.
  sz.X := Min(FPointer.HorizSize, (ARect.Right - ARect.Left) div 3);
  sz.Y := Min(FPointer.VertSize, (ARect.Bottom - ARect.Top) div 2);
  FPointer.DrawSize(ADrawer, c, sz, Color);
end;

{ TLegendItemBrushRect }

constructor TLegendItemBrushRect.Create(
  ABrush: TFPCustomBrush; const AText: String);
begin
  inherited Create(AText);
  FBrush := ABrush;
end;

procedure TLegendItemBrushRect.Draw(ADrawer: IChartDrawer; const ARect: TRect);
begin
  inherited Draw(ADrawer, ARect);
  if FBrush = nil then
    ADrawer.SetBrushParams(bsSolid, ColorDef(Color, clRed))
  else begin
    ADrawer.Brush := FBrush;
    if Color <> clTAColor then
      ADrawer.SetBrushParams(FBrush.Style, Color);
  end;
  ADrawer.Rectangle(ARect);
end;

{ TChartLegend }

procedure TChartLegend.Assign(Source: TPersistent);
begin
  if Source is TChartLegend then
    with TChartLegend(Source) do
      Self.FAlignment := FAlignment;

  inherited Assign(Source);
end;

constructor TChartLegend.Create(AOwner: TCustomChart);
begin
  inherited Create(AOwner);
  FAlignment := laTopRight;
  FMarginX := DEF_LEGEND_MARGIN;
  FMarginY := DEF_LEGEND_MARGIN;
  FSpacing := DEF_LEGEND_SPACING;
  FSymbolWidth := DEF_LEGEND_SYMBOL_WIDTH;
  FUseSidebar := true;
  Visible := false;

  InitHelper(FBackgroundBrush, TChartLegendBrush);
  InitHelper(FFont, TFont);
  InitHelper(FFrame, TChartPen);
  InitHelper(FSymbolFrame, TChartPen);
end;

destructor TChartLegend.Destroy;
begin
  FreeAndNil(FBackgroundBrush);
  FreeAndNil(FFont);
  FreeAndNil(FFrame);
  FreeAndNil(FSymbolFrame);

  inherited;
end;

procedure TChartLegend.Draw(
  ADrawer: IChartDrawer; AItems: TChartLegendItems; const ABounds: TRect);
var
  i, itemHeight: Integer;
  r: TRect;
begin
  // Draw the background and the border.
  ADrawer.Brush := BackgroundBrush;
  if Frame.Visible then
    ADrawer.Pen := Frame
  else
    ADrawer.SetPenParams(psClear, clTAColor);
  ADrawer.Rectangle(ABounds);
  if AItems.Count = 0 then exit;

  try
    r := ABounds;
    r.Right -= 1;
    ADrawer.ClippingStart(r);

    itemHeight :=
      (ABounds.Bottom - ABounds.Top - Spacing) div AItems.Count - Spacing;
    r := Bounds(
      ABounds.Left + Spacing, ABounds.Top + Spacing, SymbolWidth, itemHeight);
    for i := 0 to AItems.Count - 1 do begin
      ADrawer.Font := Font;
      ADrawer.Brush := BackgroundBrush;
      if SymbolFrame.Visible then
        ADrawer.Pen := SymbolFrame
      else
        ADrawer.SetPenParams(psClear, clTAColor);
      AItems[i].Draw(ADrawer, r);
      OffsetRect(r, 0, itemHeight + Spacing);
    end;
  finally
    ADrawer.ClippingStop;
  end;
end;

function TChartLegend.MeasureItem(
  ADrawer: IChartDrawer; AItems: TChartLegendItems): TPoint;
var
  i: Integer;
begin
  ADrawer.Font := Font;
  Result := Point(0, 0);
  for i := 0 to AItems.Count - 1 do
    with ADrawer.TextExtent(AItems[i].FText) do begin
      Result.X := Max(X, Result.X);
      Result.Y := Max(Y, Result.Y);
    end;

  Result.X += SYMBOL_TEXT_SPACING + SymbolWidth;
end;

function TChartLegend.Prepare(
  ADrawer: IChartDrawer; AItems: TChartLegendItems; var AClipRect: TRect): TRect;
var
  x, y: Integer;
  sidebar, legendSize: TPoint;
begin
  with MeasureItem(ADrawer, AItems) do
    legendSize := Point(X + 2 * Spacing, Spacing + AItems.Count * (Y + Spacing));

  sidebar.X := 2 * MarginX;
  with AClipRect do
    legendSize.X := EnsureRange(legendSize.X, 0, Right - Left - sidebar.X);
  sidebar.X += legendSize.X;

  sidebar.Y := 2 * MarginX;
  with AClipRect do
    legendSize.Y := EnsureRange(legendSize.Y, 0, Bottom - Top - sidebar.Y);
  sidebar.Y += legendSize.Y;

  // Determine position according to the alignment.
  case Alignment of
    laTopLeft, laCenterLeft, laBottomLeft:
      x := AClipRect.Left + MarginX;
    laTopRight, laCenterRight, laBottomRight:
      x := AClipRect.Right - legendSize.X - MarginX;
    laTopCenter, laBottomCenter:
      x := (AClipRect.Right + AClipRect.Left - legendSize.X) div 2;
  end;
  case Alignment of
    laTopLeft, laTopCenter, laTopRight:
      y := AClipRect.Top + MarginY;
    laBottomLeft, laBottomCenter, laBottomRight:
      y := AClipRect.Bottom - MarginY - legendSize.Y;
    laCenterLeft, laCenterRight:
      y := (AClipRect.Top + AClipRect.Bottom - legendSize.Y) div 2;
  end;
  if UseSidebar then
    case Alignment of
      laTopLeft, laCenterLeft, laBottomLeft:
        AClipRect.Left += sidebar.X;
      laTopRight, laCenterRight, laBottomRight:
        AClipRect.Right -= sidebar.X;
      laTopCenter:
        AClipRect.Top += legendSize.Y + 2 * MarginY;
      laBottomCenter:
        AClipRect.Bottom -= legendSize.Y + 2 * MarginY;
    end;
  Result := Bounds(x, y, legendSize.X, legendSize.Y);
end;

procedure TChartLegend.SetAlignment(AValue: TLegendAlignment);
begin
  if FAlignment = AValue then exit;
  FAlignment := AValue;
  StyleChanged(Self);
end;

procedure TChartLegend.SetBackgroundBrush(AValue: TChartLegendBrush);
begin
  FBackgroundBrush.Assign(AValue);
  StyleChanged(Self);
end;

procedure TChartLegend.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue);
  StyleChanged(Self);
end;

procedure TChartLegend.SetFrame(AValue: TChartPen);
begin
  FFrame.Assign(AValue);
  StyleChanged(Self);
end;

procedure TChartLegend.SetMargin(AValue: TChartDistance);
begin
  SetMarginX(AValue);
  SetMarginY(AValue);
end;

procedure TChartLegend.SetMarginX(AValue: TChartDistance);
begin
  if FMarginX = AValue then exit;
  FMarginX := AValue;
  StyleChanged(Self);
end;

procedure TChartLegend.SetMarginY(AValue: TChartDistance);
begin
  if FMarginY = AValue then exit;
  FMarginY := AValue;
  StyleChanged(Self);
end;

procedure TChartLegend.SetSpacing(AValue: TChartDistance);
begin
  if FSpacing = AValue then exit;
  FSpacing := AValue;
  StyleChanged(Self);
end;

procedure TChartLegend.SetSymbolFrame(AValue: TChartPen);
begin
  if FSymbolFrame = AValue then exit;
  FSymbolFrame := AValue;
  StyleChanged(Self);
end;

procedure TChartLegend.SetSymbolWidth(AValue: TChartDistance);
begin
  if FSymbolWidth = AValue then exit;
  FSymbolWidth := AValue;
  StyleChanged(Self);
end;

procedure TChartLegend.SetUseSidebar(AValue: Boolean);
begin
  if FUseSidebar = AValue then exit;
  FUseSidebar := AValue;
  StyleChanged(Self);
end;

{ TChartSeriesLegend }

procedure TChartSeriesLegend.Assign(Source: TPersistent);
begin
  if Source is TChartSeriesLegend then
    with TChartSeriesLegend(Source) do begin
      Self.FMultiplicity := FMultiplicity;
      Self.FOnDraw := FOnDraw;
      Self.FUserItemsCount := FUserItemsCount;
    end;

  inherited Assign(Source);
end;

constructor TChartSeriesLegend.Create(AOwner: TCustomChart);
begin
  inherited Create(AOwner);
  FOrder := LEGEND_ITEM_ORDER_AS_ADDED;
  FVisible := true;
  FUserItemsCount := 1;
end;

procedure TChartSeriesLegend.SetMultiplicity(AValue: TLegendMultiplicity);
begin
  if FMultiplicity = AValue then exit;
  FMultiplicity := AValue;
  StyleChanged(Self);
end;

procedure TChartSeriesLegend.SetOnDraw(AValue: TLegendItemDrawEvent);
begin
  if TMethod(FOnDraw) = TMethod(AValue) then exit;
  FOnDraw := AValue;
  StyleChanged(Self);
end;

procedure TChartSeriesLegend.SetOrder(AValue: Integer);
begin
  if FOrder = AValue then exit;
  FOrder := AValue;
  StyleChanged(Self);
end;

procedure TChartSeriesLegend.SetUserItemsCount(AValue: Integer);
begin
  if FUserItemsCount = AValue then exit;
  FUserItemsCount := AValue;
  StyleChanged(Self);
end;

procedure SkipObsoleteProperties;
begin
  RegisterPropertyEditor(
    TypeInfo(TChartDistance), TChartLegend, 'Margin', THiddenPropertyEditor);
end;

initialization
  SkipObsoleteProperties;

end.

