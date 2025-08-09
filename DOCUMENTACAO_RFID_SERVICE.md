# Documentação do RFIDService - Serviço de Leitura RFID

## Visão Geral

O `RFIDService` é um serviço Android que permite a leitura contínua de tags RFID UHF via UART. O serviço oferece dois modos de operação:

1. **Modo Normal**: Envia cada EPC lido imediatamente via broadcast (comportamento original)
2. **Modo Batch**: Acumula todos os EPCs lidos e os envia apenas quando solicitado

## Modos de Operação

### Modo Normal (Padrão)
- Envia cada EPC lido imediatamente via broadcast
- Controle de duplicação com intervalo mínimo de 100ms
- Compatível com implementações existentes

### Modo Batch (Novo)
- Acumula todos os EPCs lidos em memória
- Envia atualizações periódicas apenas com a contagem (a cada 5 segundos)
- Envia todos os dados acumulados apenas quando solicitado
- Monitora se o aplicativo cliente está ativo (heartbeat de 30 segundos)
- Para automaticamente se o cliente não responder

## Actions (Comandos) Disponíveis

### Comandos de Controle do Serviço

#### `com.rfid.START_SERVICE`
- **Descrição**: Inicia o serviço no modo normal
- **Uso**: Comportamento original - envia EPCs imediatamente
- **Exemplo**:
```java
Intent intent = new Intent("com.rfid.START_SERVICE");
startService(intent);
```

#### `com.rfid.START_BATCH_SERVICE`
- **Descrição**: Inicia o serviço no modo batch
- **Uso**: Acumula EPCs e envia apenas quando solicitado
- **Exemplo**:
```java
Intent intent = new Intent("com.rfid.START_BATCH_SERVICE");
startService(intent);
```

#### `com.rfid.STOP_SERVICE`
- **Descrição**: Para o serviço (ambos os modos)
- **Uso**: Encerra a leitura e para o serviço
- **Exemplo**:
```java
Intent intent = new Intent("com.rfid.STOP_SERVICE");
startService(intent);
```

#### `com.rfid.GET_BATCH_DATA`
- **Descrição**: Solicita o envio de todos os dados acumulados (modo batch)
- **Uso**: Envia todos os EPCs lidos e para o serviço
- **Exemplo**:
```java
Intent intent = new Intent("com.rfid.GET_BATCH_DATA");
startService(intent);
```

#### `com.rfid.CLIENT_HEARTBEAT`
- **Descrição**: Envia sinal de vida do cliente (modo batch)
- **Uso**: Deve ser enviado pelo menos a cada 30 segundos no modo batch
- **Exemplo**:
```java
Intent intent = new Intent("com.rfid.CLIENT_HEARTBEAT");
startService(intent);
```

## Broadcasts Recebidos

### Modo Normal

#### `com.rfid.EPC_READED`
- **Descrição**: EPC lido e enviado imediatamente
- **Extras**:
  - `epc` (String): Código EPC da tag
  - `rssi` (String): Força do sinal (opcional)

### Modo Batch

#### `com.rfid.BATCH_COUNT_UPDATE`
- **Descrição**: Atualização periódica da contagem (a cada 5 segundos)
- **Extras**:
  - `count` (int): Quantidade de EPCs únicos lidos

#### `com.rfid.BATCH_DATA_RESPONSE`
- **Descrição**: Todos os dados acumulados (enviado após GET_BATCH_DATA)
- **Extras**:
  - `epc_list` (String[]): Array com todos os EPCs lidos
  - `rssi_list` (String[]): Array com os RSSIs correspondentes
  - `count` (int): Quantidade total de itens

## Exemplos de Implementação

### Exemplo 1: Uso no Modo Normal (Delphi/Pascal)

```pascal
// Registrar receiver para receber EPCs
procedure TForm1.RegisterRFIDReceiver;
begin
  // Registrar broadcast receiver
  FRFIDReceiver := TJBroadcastReceiver.JavaClass.init;
  // Configurar filtro para ACTION_EPC_READED
  // Implementar onReceive para processar EPCs
end;

// Iniciar serviço no modo normal
procedure TForm1.StartNormalMode;
var
  Intent: JIntent;
begin
  Intent := TJIntent.JavaClass.init(StringToJString('com.rfid.START_SERVICE'));
  TAndroidHelper.Context.startService(Intent);
end;

// Parar serviço
procedure TForm1.StopService;
var
  Intent: JIntent;
begin
  Intent := TJIntent.JavaClass.init(StringToJString('com.rfid.STOP_SERVICE'));
  TAndroidHelper.Context.startService(Intent);
end;
```

### Exemplo 2: Uso no Modo Batch (Delphi/Pascal)

```pascal
// Variáveis para controle do modo batch
var
  FHeartbeatTimer: TTimer;
  FBatchEPCs: TStringList;

// Iniciar serviço no modo batch
procedure TForm1.StartBatchMode;
var
  Intent: JIntent;
begin
  Intent := TJIntent.JavaClass.init(StringToJString('com.rfid.START_BATCH_SERVICE'));
  TAndroidHelper.Context.startService(Intent);
  
  // Iniciar heartbeat timer
  FHeartbeatTimer.Interval := 25000; // 25 segundos
  FHeartbeatTimer.Enabled := True;
end;

// Enviar heartbeat periodicamente
procedure TForm1.HeartbeatTimerExecute(Sender: TObject);
var
  Intent: JIntent;
begin
  Intent := TJIntent.JavaClass.init(StringToJString('com.rfid.CLIENT_HEARTBEAT'));
  TAndroidHelper.Context.startService(Intent);
end;

// Solicitar dados acumulados
procedure TForm1.GetBatchData;
var
  Intent: JIntent;
begin
  Intent := TJIntent.JavaClass.init(StringToJString('com.rfid.GET_BATCH_DATA'));
  TAndroidHelper.Context.startService(Intent);
end;

// Processar broadcast de contagem
procedure TForm1.OnBatchCountUpdate(Count: Integer);
begin
  // Atualizar interface com a contagem atual
  lblCount.Text := 'EPCs lidos: ' + IntToStr(Count);
end;

// Processar broadcast de dados completos
procedure TForm1.OnBatchDataReceived(EPCs: TJavaObjectArray<JString>; RSSIs: TJavaObjectArray<JString>);
var
  i: Integer;
  EPC, RSSI: string;
begin
  FBatchEPCs.Clear;
  
  for i := 0 to EPCs.Length - 1 do
  begin
    EPC := JStringToString(EPCs.Items[i]);
    RSSI := JStringToString(RSSIs.Items[i]);
    
    FBatchEPCs.Add(EPC + '|' + RSSI);
  end;
  
  // Processar dados conforme necessário
  ProcessBatchData(FBatchEPCs);
end;
```

### Exemplo 3: Uso no Modo Batch (Java/Android)

```java
public class RFIDClientActivity extends AppCompatActivity {
    private BroadcastReceiver rfidReceiver;
    private Timer heartbeatTimer;
    private List<String> batchEPCs = new ArrayList<>();
    
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        
        // Registrar receiver
        registerRFIDReceiver();
    }
    
    private void registerRFIDReceiver() {
        rfidReceiver = new BroadcastReceiver() {
            @Override
            public void onReceive(Context context, Intent intent) {
                String action = intent.getAction();
                
                if ("com.rfid.BATCH_COUNT_UPDATE".equals(action)) {
                    int count = intent.getIntExtra("count", 0);
                    updateCountDisplay(count);
                    
                } else if ("com.rfid.BATCH_DATA_RESPONSE".equals(action)) {
                    String[] epcs = intent.getStringArrayExtra("epc_list");
                    String[] rssis = intent.getStringArrayExtra("rssi_list");
                    processBatchData(epcs, rssis);
                }
            }
        };
        
        IntentFilter filter = new IntentFilter();
        filter.addAction("com.rfid.BATCH_COUNT_UPDATE");
        filter.addAction("com.rfid.BATCH_DATA_RESPONSE");
        registerReceiver(rfidReceiver, filter);
    }
    
    public void startBatchMode() {
        Intent intent = new Intent("com.rfid.START_BATCH_SERVICE");
        startService(intent);
        
        // Iniciar heartbeat
        startHeartbeat();
    }
    
    private void startHeartbeat() {
        heartbeatTimer = new Timer();
        heartbeatTimer.scheduleAtFixedRate(new TimerTask() {
            @Override
            public void run() {
                Intent intent = new Intent("com.rfid.CLIENT_HEARTBEAT");
                startService(intent);
            }
        }, 0, 25000); // A cada 25 segundos
    }
    
    public void getBatchData() {
        Intent intent = new Intent("com.rfid.GET_BATCH_DATA");
        startService(intent);
    }
    
    private void processBatchData(String[] epcs, String[] rssis) {
        // Processar dados recebidos
        for (int i = 0; i < epcs.length; i++) {
            String epc = epcs[i];
            String rssi = rssis[i];
            // Processar cada EPC
        }
    }
    
    @Override
    protected void onDestroy() {
        if (rfidReceiver != null) {
            unregisterReceiver(rfidReceiver);
        }
        if (heartbeatTimer != null) {
            heartbeatTimer.cancel();
        }
        super.onDestroy();
    }
}
```

## Configurações e Constantes

### Intervalos de Tempo
- **MIN_BROADCAST_INTERVAL**: 100ms (controle de duplicação)
- **COUNT_UPDATE_INTERVAL**: 5000ms (atualização de contagem no modo batch)
- **HEARTBEAT_TIMEOUT**: 30000ms (timeout para heartbeat do cliente)

### Recomendações
- No modo batch, envie heartbeat a cada 25 segundos para garantir que o serviço não pare
- Sempre registre os broadcast receivers antes de iniciar o serviço
- No modo normal, processe os EPCs imediatamente para evitar perda de dados
- No modo batch, monitore as atualizações de contagem para feedback ao usuário

## Permissões Necessárias

Adicione as seguintes permissões no AndroidManifest.xml:

```xml
<uses-permission android:name="android.permission.FOREGROUND_SERVICE" />
<uses-permission android:name="android.permission.WAKE_LOCK" />
```

## Troubleshooting

### Problema: Serviço para inesperadamente no modo batch
**Solução**: Verifique se o heartbeat está sendo enviado regularmente (máximo 30 segundos)

### Problema: EPCs duplicados no modo normal
**Solução**: O serviço já controla duplicação com intervalo de 100ms. Ajuste MIN_BROADCAST_INTERVAL se necessário

### Problema: Não recebe broadcasts
**Solução**: Verifique se o BroadcastReceiver está registrado corretamente com os filtros apropriados

### Problema: Serviço não inicia
**Solução**: Verifique se o dispositivo RFID está conectado e se as permissões estão concedidas

## Notas Importantes

1. **Compatibilidade**: O modo normal mantém total compatibilidade com implementações existentes
2. **Performance**: O modo batch é mais eficiente para leitura de grandes volumes de tags
3. **Memória**: No modo batch, os dados são mantidos em memória até serem solicitados
4. **Segurança**: O heartbeat previne que o serviço continue executando se o cliente falhar
5. **Logs**: Todos os eventos são logados com tag "RFIDService" para debug

## Changelog

### Versão 2.0
- Adicionado modo batch para acúmulo de dados
- Implementado sistema de heartbeat para detecção de cliente inativo
- Adicionadas atualizações periódicas de contagem
- Mantida compatibilidade total com versão anterior
- Melhorada documentação e exemplos de uso
