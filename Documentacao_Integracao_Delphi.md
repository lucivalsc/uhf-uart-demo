# 📱 Documentação de Integração com Delphi FMX

## 📋 Visão Geral

Esta biblioteca fornece um serviço Android para leitura de tags RFID usando o SDK Chainway (RFIDWithUHFUART) que pode ser facilmente integrado com aplicativos Delphi FMX. O serviço funciona em segundo plano e envia os códigos EPCs lidos via Broadcast Intent.

## ⚙️ Como Funciona

1. O app Delphi inicia o serviço RFID Android
2. O serviço lê continuamente as tags RFID
3. Cada EPC lido é enviado via Broadcast Intent
4. O app Delphi recebe os EPCs usando um BroadcastReceiver

## 🔧 Instruções de Integração com Delphi

### Para iniciar o serviço RFID

```pascal
procedure IniciarServicoRFID;
var
  Intent: JIntent;
begin
  Intent := TJIntent.Create;
  Intent.setAction(StringToJString('com.rfid.START_SERVICE'));
  Intent.setClassName(
    StringToJString('com.example.uhf'), 
    StringToJString('com.example.uhf.activity.RFIDServiceActivity')
  );
  SharedActivity.startActivity(Intent);
end;
```

### Para receber os EPCs lidos

```pascal
type
  TMyBroadcastReceiver = class(TJavaLocal, JBroadcastReceiver)
  public
    procedure onReceive(context: JContext; intent: JIntent); override;
  end;

procedure TMyBroadcastReceiver.onReceive(context: JContext; intent: JIntent);
var
  Action: string;
  EPC: string;
begin
  Action := JStringToString(intent.getAction);
  
  if Action = 'com.rfid.EPC_READED' then
  begin
    EPC := JStringToString(intent.getStringExtra(StringToJString('epc')));
    
    // Faça algo com o EPC recebido
    ShowMessage('EPC lido: ' + EPC);
  end;
end;

// Para registrar o receptor de broadcast:
procedure RegistrarReceiver;
var
  IntentFilter: JIntentFilter;
  Receiver: TMyBroadcastReceiver;
begin
  Receiver := TMyBroadcastReceiver.Create;
  IntentFilter := TJIntentFilter.Create;
  IntentFilter.addAction(StringToJString('com.rfid.EPC_READED'));
  
  TAndroidHelper.Context.registerReceiver(Receiver, IntentFilter);
end;
```

### Para parar o serviço RFID

```pascal
procedure PararServicoRFID;
var
  Intent: JIntent;
begin
  Intent := TJIntent.Create;
  Intent.setClassName(
    StringToJString('com.example.uhf'), 
    StringToJString('com.example.uhf.service.RFIDService')
  );
  Intent.setAction(StringToJString('com.rfid.STOP_SERVICE'));
  SharedActivity.startService(Intent);
end;
```

## 📝 Especificação do Broadcast

- **Action:** `com.rfid.EPC_READED`
- **Extra:** chave `"epc"`, valor string com o código EPC lido
- **Tipo:** Broadcast local

## 💡 Dicas de Implementação

- Inicie o serviço quando seu aplicativo Delphi for aberto
- Configure seu BroadcastReceiver durante a inicialização
- Processe os EPCs recebidos de acordo com a lógica do seu aplicativo
- Pare o serviço quando seu aplicativo for fechado
- Considere implementar lógica para evitar duplicação de EPCs

## ⚠️ Requisitos

- Android SDK mínimo API 23 (Android 6.0)
- Permissões de Bluetooth concedidas ao aplicativo
- Instalação do APK de serviço RFID no dispositivo
