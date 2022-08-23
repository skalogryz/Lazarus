unit IdeDebuggerOpts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IDEOptionsIntf, Laz2_XMLCfg, LazFileUtils,
  IdeDebuggerStringConstants, IdeDebuggerFpDbgValueConv;

type

  { TDebuggerOptions }

  TDebuggerOptions = class(TAbstractIDEEnvironmentOptions)
  private
    FFilename: string;
    FFpDbgConverterConfig: TIdeDbgValueConvertSelectorList;
    FPrimaryConfigPath: String;
    FXMLCfg: TRttiXMLConfig;
  protected
    procedure InitXMLCfg(CleanConfig: boolean);
  public
    class function GetGroupCaption:string; override;
    class function GetInstance: TAbstractIDEOptions; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load;
    procedure Save;
    function GetDefaultConfigFilename: string;
    procedure CreateConfig;

    property Filename: string read FFilename;
    property PrimaryConfigPath: String read FPrimaryConfigPath write FPrimaryConfigPath;

    property FpDbgConverterConfig: TIdeDbgValueConvertSelectorList read FFpDbgConverterConfig write FFpDbgConverterConfig;
  end;

function GetDebuggerOptions: TDebuggerOptions;
property DebuggerOptions: TDebuggerOptions read GetDebuggerOptions;


implementation

const
  DebuggerOptsConfFileName = 'debuggeroptions.xml';
var
  TheDebuggerOptions: TDebuggerOptions = nil;

function GetDebuggerOptions: TDebuggerOptions;
begin
  if TheDebuggerOptions = nil then
    TheDebuggerOptions := TDebuggerOptions.Create;
  Result := TheDebuggerOptions;
end;

{ TDebuggerOptions }

procedure TDebuggerOptions.InitXMLCfg(CleanConfig: boolean);
begin
  FreeAndNil(FXMLCfg);
  if CleanConfig then
    FXMLCfg:=TRttiXMLConfig.CreateClean(Filename)
  else
    FXMLCfg:=TRttiXMLConfig.Create(Filename);
end;

class function TDebuggerOptions.GetGroupCaption: string;
begin
  Result := dlgIdeDbgDebugger;
end;

class function TDebuggerOptions.GetInstance: TAbstractIDEOptions;
begin
  Result := DebuggerOptions;
end;

constructor TDebuggerOptions.Create;
begin
  inherited Create;
  FpDbgConverterConfig := TIdeDbgValueConvertSelectorList.Create;
end;

destructor TDebuggerOptions.Destroy;
begin
  inherited Destroy;
  FpDbgConverterConfig.Free;
  FXMLCfg.Free;
end;

procedure TDebuggerOptions.Load;
var
  Path: String;
begin
  InitXMLCfg(False);

  Path := 'Debugger/';

  FFpDbgConverterConfig.LoadDataFromXMLConfig(FXMLCfg, Path + 'FpDebug/ValueConvert/');
end;

procedure TDebuggerOptions.Save;
var
  Path: String;
begin
  InitXMLCfg(False); // Dont delete old content
  Path := 'Debugger/';

  if FFpDbgConverterConfig.Changed then
    FFpDbgConverterConfig.SaveDataToXMLConfig(FXMLCfg, Path + 'FpDebug/ValueConvert/');
  FFpDbgConverterConfig.Changed := False;

  FXMLCfg.Flush;
end;

function TDebuggerOptions.GetDefaultConfigFilename: string;
begin
  Result:=TrimFilename(AppendPathDelim(PrimaryConfigPath)+DebuggerOptsConfFileName);

end;

procedure TDebuggerOptions.CreateConfig;
begin
  FFilename:=GetDefaultConfigFilename;
end;

finalization
  TheDebuggerOptions.Free;

end.

