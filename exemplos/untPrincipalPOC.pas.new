unit untPrincipalPOC;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.UITypes,
  FMX.Forms, FMX.Controls, FMX.StdCtrls, FMX.Layouts, FMX.Memo, FMX.Types,
  FMX.ScrollBox, FMX.Controls.Presentation, FMX.Memo.Types, FMX.Graphics,
  FMX.Grid, FMX.Grid.Style, System.Generics.Collections,
  {$IFDEF ANDROID}
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.App,
  Androidapi.JNI.Os,
  Androidapi.Helpers,
  Androidapi.JNIBridge,
  Androidapi.JNI.Embarcadero
  {$ENDIF};

type
  // Definição do callback para processar EPCs lidos
  TEPCProcessadoEvent = procedure(const AEPC: string; const ARSSI: Integer = 0; const APhase: Integer = 0) of object;

  // Broadcast Receiver para capturar os EPCs enviados pelo serviço RFID
  // Seguindo o padrão do exemplo funcional untScanPal60kIntent
  TTagRFID = class
  private
    FEPC: string;
    FCount: Integer;
    FRSSI: Integer;
    FPhase: Integer;
    FPrimeiraLeitura: TDateTime;
    FUltimaLeitura: TDateTime;
  public
    property EPC: string read FEPC write FEPC;
    property Count: Integer read FCount write FCount;
    property RSSI: Integer read FRSSI write FRSSI;
    property Phase: Integer read FPhase write FPhase;
    property PrimeiraLeitura: TDateTime read FPrimeiraLeitura;
    property UltimaLeitura: TDateTime read FUltimaLeitura write FUltimaLeitura;
    
    constructor Create(const AEPC: string; const ARSSI: Integer = 0; const APhase: Integer = 0);
    procedure IncrementarContagem(const ARSSI: Integer = 0; const APhase: Integer = 0);
    function TempoDesdeUltimaLeitura: TDateTime;
  end;

  TRFIDBroadcastReceiver = class(TJavaLocal, JFMXBroadcastReceiverListener)
  public
    constructor Create;
    procedure onReceive(context: JContext; intent: JIntent); cdecl;
  end;

  TFormRFIDDemo = class(TForm)
    LayoutTopo: TLayout;
    btnIniciarServico: TButton;
    btnPararServico: TButton;
    MemoLog: TMemo;
    StringGridTags: TStringGrid;
    LayoutGrid: TLayout;
    btnLimparLista: TButton;
    lblTotalTags: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnIniciarServicoClick(Sender: TObject);
    procedure btnPararServicoClick(Sender: TObject);
  private
    {$IFDEF ANDROID}
    FRFIDListener: TRFIDBroadcastReceiver;
    FBroadcastReceiver: JFMXBroadcastReceiver;
    {$ENDIF}
    FServicoAtivo: Boolean;
    FEPCCount: Integer;
    FUltimoEPC: string;
    FTagsList: TObjectList<TTagRFID>;

    procedure LogMensagem(const AMensagem: string);
    procedure ProcessarEPC(const AEPC: string; const ARSSI: Integer = 0; const APhase: Integer = 0);
    procedure ConfigurarGrid;
    procedure AtualizarGrid;
    procedure LimparListaTags;
    {$IFDEF ANDROID}
    procedure RegistrarBroadcastReceiver;
    procedure DesregistrarBroadcastReceiver;
    procedure EnviarComandoRFID(const AComando: string; const AParametros: string = '');
    {$ENDIF}
    procedure AtualizarInterface;
  end;

var
  FormRFIDDemo: TFormRFIDDemo;

implementation

{$R *.fmx}

uses
  System.SysUtils, System.DateUtils, System.Generics.Collections;

{ TTagRFID }

constructor TTagRFID.Create(const AEPC: string; const ARSSI: Integer = 0; const APhase: Integer = 0);
begin
  inherited Create;
  FEPC := AEPC;
  FRSSI := ARSSI;
  FPhase := APhase;
  FCount := 1;
  FPrimeiraLeitura := Now;
  FUltimaLeitura := FPrimeiraLeitura;
end;

procedure TTagRFID.IncrementarContagem(const ARSSI: Integer = 0; const APhase: Integer = 0);
begin
  Inc(FCount);
  FUltimaLeitura := Now;
  
  // Atualiza RSSI e Phase se fornecidos
  if ARSSI <> 0 then
    FRSSI := ARSSI;
    
  if APhase <> 0 then
    FPhase := APhase;
end;

function TTagRFID.TempoDesdeUltimaLeitura: TDateTime;
begin
  Result := Now - FUltimaLeitura;
end;

{ TRFIDBroadcastReceiver }

constructor TRFIDBroadcastReceiver.Create;
begin
  inherited Create;
end;

procedure TRFIDBroadcastReceiver.onReceive(context: JContext; intent: JIntent);
var
  LAction, LEPC: string;
  LRSSI, LPhase: Integer;
begin
  try
    // Obtém a ação do intent recebido
    LAction := JStringToString(intent.getAction);

    // Verifica se é a ação de leitura de EPC
    if LAction = 'com.rfid.EPC_READED' then
    begin
      // Extrai o EPC do intent e remove espaços e quebras de linha
      LEPC := StringReplace(StringReplace(JStringToString(intent.getStringExtra(StringToJString('epc'))).Trim, #13, '', [rfReplaceAll]), #10, '', [rfReplaceAll]);

      // Tenta obter o RSSI (força do sinal)
      try
        LRSSI := StrToIntDef(JStringToString(intent.getStringExtra(StringToJString('rssi'))), 0);
      except
        LRSSI := 0;
      end;
      
      // Tenta obter a Phase (se disponível)
      try
        LPhase := StrToIntDef(JStringToString(intent.getStringExtra(StringToJString('phase'))), 0);
      except
        LPhase := 0;
      end;

      // Processa o EPC se não estiver vazio
      if not LEPC.IsEmpty then
        FormRFIDDemo.ProcessarEPC(LEPC, LRSSI, LPhase);
    end;
  except
    on E: Exception do
      FormRFIDDemo.LogMensagem('✗ Erro no BroadcastReceiver: ' + E.Message);
  end;
end;

procedure TFormRFIDDemo.FormCreate(Sender: TObject);
begin
  // Inicializa as variáveis
  FServicoAtivo := False;
  FEPCCount := 0;
  FUltimoEPC := '';
  
  // Cria a lista de tags
  FTagsList := TObjectList<TTagRFID>.Create(True); // True para ser owner dos objetos
  
  // Configura a grid
  ConfigurarGrid;

  // Limpa e inicializa o log
  MemoLog.Lines.Clear;
  LogMensagem('=== Leitor RFID Iniciado ===');
  LogMensagem('Versão: 1.0 | ' + FormatDateTime('dd/mm/yyyy', Now));

  {$IFDEF ANDROID}
  // Registra o receptor de broadcast para EPCs
  RegistrarBroadcastReceiver;
  LogMensagem('✓ Executando no Android - Pronto para comunicação com serviço RFID');
  {$ELSE}
  LogMensagem('⚠ Executando no Windows - Modo Desenvolvimento');
  LogMensagem('⚠ No Windows, a comunicação com o serviço RFID não está disponível');
  {$ENDIF}

  // Atualiza a interface
  AtualizarInterface;
end;

procedure TFormRFIDDemo.FormDestroy(Sender: TObject);
begin
  try
    // Garante que o serviço seja parado ao fechar o app
    if FServicoAtivo then
      btnPararServicoClick(nil);

    // Libera a lista de tags
    if Assigned(FTagsList) then
      FreeAndNil(FTagsList);

    {$IFDEF ANDROID}
    // Remove o receptor de broadcast
    DesregistrarBroadcastReceiver;
    {$ENDIF}
  except
    on E: Exception do
      LogMensagem('✗ Erro no FormDestroy: ' + E.Message);
  end;
end;

procedure TFormRFIDDemo.ProcessarEPC(const AEPC: string; const ARSSI: Integer; const APhase: Integer);
var
  EPC: string;
  TagEncontrada: Boolean;
  i: Integer;
  Tag: TTagRFID;
  MensagemEPC: string;
begin
  // Executa em thread segura para UI
  TThread.Synchronize(nil, procedure
  begin
    try
      // Evita processamento duplicado do mesmo EPC em sequência
      if FUltimoEPC = AEPC then
        Exit;

      // Limpa e formata o EPC recebido para evitar problemas
      EPC := StringReplace(StringReplace(Trim(AEPC), #13, '', [rfReplaceAll]), #10, '', [rfReplaceAll]);
      
      if EPC.IsEmpty then
        Exit;
        
      // Atualiza variáveis de controle
      Inc(FEPCCount);
      FUltimoEPC := EPC;

      // Prepara mensagem de log
      if ARSSI > 0 then
        MensagemEPC := Format('📡 EPC LIDO [%d]: %s (RSSI: %d)', [FEPCCount, EPC, ARSSI])
      else
        MensagemEPC := Format('📡 EPC LIDO [%d]: %s', [FEPCCount, EPC]);
      
      // Adiciona informação de fase se disponível
      if APhase > 0 then
        MensagemEPC := MensagemEPC + Format(' (Phase: %d)', [APhase]);
        
      LogMensagem(MensagemEPC);
      
      // Verifica se o EPC já existe na lista
      TagEncontrada := False;
      for i := 0 to FTagsList.Count - 1 do
      begin
        if FTagsList[i].EPC = EPC then
        begin
          TagEncontrada := True;
          FTagsList[i].IncrementarContagem(ARSSI, APhase);
          Break;
        end;
      end;
      
      // Se não encontrou, adiciona um novo
      if not TagEncontrada then
      begin
        Tag := TTagRFID.Create(EPC, ARSSI, APhase);
        FTagsList.Add(Tag);
      end;
      
      // Atualiza a grid
      AtualizarGrid;

      // Atualiza a interface com o contador
      AtualizarInterface;
      
      // Informações adicionais no log
      LogMensagem(Format('   ✓ Formato: %s | Comprimento: %d caracteres', 
        [IfThen(Length(EPC) >= 8, 'Válido', 'Inválido'), Length(EPC)]));
    except
      on E: Exception do
        LogMensagem('✗ Erro ao processar EPC: ' + E.Message);
    end;
  end);
end;

procedure TFormRFIDDemo.ConfigurarGrid;
begin
  // Configura as colunas da grid
  with StringGridTags do
  begin
    // Limpa as colunas existentes
    Columns.Clear;

    // Adiciona as colunas necessárias
    with Columns.Add do
    begin
      Header := 'EPC';
      Width := 180;
    end;

    with Columns.Add do
    begin
      Header := 'Count';
      Width := 60;
    end;

    with Columns.Add do
    begin
      Header := 'RSSI';
      Width := 60;
    end;

    with Columns.Add do
    begin
      Header := 'Phase';
      Width := 60;
    end;

    // Configurações gerais da grid
    RowCount := 0;
    Options := Options + [TGridOption.AlternatingRowBackground, TGridOption.ColumnResize, TGridOption.RowSelect];
    ShowHint := True;

    // Evento para limpar a lista
    btnLimparLista.OnClick := LimparListaTags;
  end;
end;

procedure TFormRFIDDemo.AtualizarGrid;
var
  i: Integer;
  Tag: TTagRFID;
begin
  // Atualiza a grid com os dados da lista de tags
  StringGridTags.BeginUpdate;
  try
    // Limpa as linhas existentes
    StringGridTags.RowCount := 0;

    // Adiciona cada tag à grid
    for i := 0 to FTagsList.Count - 1 do
    begin
      Tag := FTagsList[i];

      StringGridTags.RowCount := StringGridTags.RowCount + 1;
      StringGridTags.Cells[0, i] := Tag.EPC;
      StringGridTags.Cells[1, i] := IntToStr(Tag.Count);
      StringGridTags.Cells[2, i] := IntToStr(Tag.RSSI);
      StringGridTags.Cells[3, i] := IntToStr(Tag.Phase);
    end;

    // Atualiza o contador de tags
    lblTotalTags.Text := Format('Total de Tags: %d', [FTagsList.Count]);
  finally
    StringGridTags.EndUpdate;
  end;
end;

procedure TFormRFIDDemo.LimparListaTags;
begin
  // Limpa a lista de tags
  FTagsList.Clear;

  // Atualiza a grid
  AtualizarGrid;

  // Log
  LogMensagem('✓ Lista de tags limpa');
end;

{$IFDEF ANDROID}
procedure TFormRFIDDemo.RegistrarBroadcastReceiver;
var
  Filter: JIntentFilter;
begin
  try
    // Cria o listener seguindo o padrão do exemplo funcional
    FRFIDListener := TRFIDBroadcastReceiver.Create;

    // Cria o broadcast receiver com o listener
    FBroadcastReceiver := TJFMXBroadcastReceiver.JavaClass.init(FRFIDListener);

    // Configura o filtro para a ação de EPCs lidos
    Filter := TJIntentFilter.JavaClass.init(StringToJString('com.rfid.EPC_READED'));

    // Registra o receptor no sistema Android
    TAndroidHelper.context.getApplicationContext.registerReceiver(FBroadcastReceiver, Filter);

    LogMensagem('✓ Receptor de broadcast para EPCs registrado');
  except
    on E: Exception do
      LogMensagem('✗ Erro ao registrar receptor: ' + E.Message);
  end;
end;

procedure TFormRFIDDemo.DesregistrarBroadcastReceiver;
begin
  try
    // Se o receptor existe, remove o registro
    if Assigned(FBroadcastReceiver) then
    begin
      TAndroidHelper.Context.getApplicationContext.unregisterReceiver(FBroadcastReceiver);
      FBroadcastReceiver := nil;
      LogMensagem('✓ Receptor de broadcast desregistrado');
    end;
  except
    on E: Exception do
      LogMensagem('✗ Erro ao desregistrar receptor: ' + E.Message);
  end;
end;

procedure TFormRFIDDemo.EnviarComandoRFID(const AComando: string; const AParametros: string);
var
  LIntent: JIntent;
begin
  try
    LogMensagem('➤ Enviando comando: ' + AComando);

    // Para iniciar o serviço, usa a activity transparente
    if AComando = 'com.rfid.START_SERVICE' then
    begin
      // Cria intent para a activity de inicialização
      LIntent := TJIntent.Create;
      LIntent.setAction(StringToJString(AComando));

      if AParametros <> '' then
        LIntent.putExtra(StringToJString('params'), StringToJString(AParametros));

      // Define a classe da activity que inicia o serviço
      LIntent.setClassName(
        StringToJString('com.example.uhf'),
        StringToJString('com.example.uhf.activity.RFIDServiceActivity')
      );

      // Inicia a activity transparente
      TAndroidHelper.Activity.startActivity(LIntent);
    end
    else
    begin
      // Para outros comandos, envia diretamente para o serviço
      LIntent := TJIntent.Create;
      LIntent.setAction(StringToJString(AComando));

      if AParametros <> '' then
        LIntent.putExtra(StringToJString('params'), StringToJString(AParametros));

      // Define a classe do serviço
      LIntent.setClassName(
        StringToJString('com.example.uhf'),
        StringToJString('com.example.uhf.service.RFIDService')
      );

      // Envia o intent para o serviço
      TAndroidHelper.Activity.startService(LIntent);
    end;

    LogMensagem('✓ Comando enviado com sucesso');
  except
    on E: Exception do
      LogMensagem('✗ Erro ao enviar comando: ' + E.Message);
  end;
end;
{$ENDIF}

procedure TFormRFIDDemo.btnIniciarServicoClick(Sender: TObject);
begin
  LogMensagem('➤ INICIANDO SERVIÇO RFID...');

  {$IFDEF ANDROID}
  // Envia o comando para iniciar o serviço RFID
  EnviarComandoRFID('com.rfid.START_SERVICE');

  // Atualiza a interface
  FServicoAtivo := True;
  AtualizarInterface;
  {$ELSE}
  // No Windows, apenas simula a interface
  FServicoAtivo := True;
  AtualizarInterface;
  LogMensagem('⚠ No Windows, o serviço RFID é apenas simulado');
  {$ENDIF}

  LogMensagem('✓ Serviço de leitura RFID iniciado');
end;

procedure TFormRFIDDemo.btnPararServicoClick(Sender: TObject);
begin
  LogMensagem('➤ PARANDO SERVIÇO RFID...');

  {$IFDEF ANDROID}
  // Envia o comando para parar o serviço
  EnviarComandoRFID('com.rfid.STOP_SERVICE');
  {$ENDIF}

  // Atualiza a interface
  FServicoAtivo := False;
  AtualizarInterface;

  LogMensagem('✓ Serviço de leitura RFID parado');
end;

procedure TFormRFIDDemo.AtualizarInterface;
begin
  // Atualiza o estado dos botões conforme o estado do serviço
  btnIniciarServico.Enabled := not FServicoAtivo;
  btnPararServico.Enabled := FServicoAtivo;
  btnLimparLista.Enabled := True;

  // Atualiza o título dos botões para melhor feedback
  if FServicoAtivo then
  begin
    btnIniciarServico.Text := 'Iniciar Serviço (Ativo)';
    btnPararServico.Text := '■ Parar Serviço';
  end
  else
  begin
    btnIniciarServico.Text := '▶ Iniciar Serviço';
    btnPararServico.Text := 'Parar Serviço (Inativo)';
  end;
end;

procedure TFormRFIDDemo.LogMensagem(const AMensagem: string);
var
  TimeStamp: string;
begin
  // Formata o timestamp
  TimeStamp := FormatDateTime('hh:nn:ss', Now);

  // Adiciona ao log
  MemoLog.Lines.Add(TimeStamp + ' ' + AMensagem);

  // Limita a 100 linhas para performance
  while MemoLog.Lines.Count > 100 do
    MemoLog.Lines.Delete(0);

  // Rolagem automática
  MemoLog.GoToTextEnd;

  // Atualiza visualização
  MemoLog.Repaint;
end;

end.
