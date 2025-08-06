md_content = """

# 🤖 SCRIPT PARA IA — CRIAÇÃO DE APP PONTE ENTRE RFID (JAVA) E DELPHI (FMX ANDROID)

## ⚙️ CONTEXTO

Um aplicativo Android em Java, já funcional, realiza leitura de RFID usando o SDK da Chainway (`RFIDWithUHFUART`). Esse app será **convertido em um serviço de ponte** que:

- Roda como **serviço Android em segundo plano**
- Lê EPCs continuamente via SDK
- Envia os EPCs via **Broadcast Intent**
- Deve ser **iniciado automaticamente** pelo app principal (feito em Delphi FMX)
- Deve ser **interpretado por Delphi** usando `BroadcastReceiver`

## ✅ OBJETIVO GERAL

Converter o app Java existente em um serviço que:

- Roda no Android
- Usa o SDK nativo do fabricante (via `.aar`)
- Fica ativo enquanto o app Delphi estiver aberto
- Envia os EPCs lidos em tempo real via intent (`com.rfid.EPC_READED`)

## 🧩 INSTRUÇÕES DE CONVERSÃO

1. **Crie uma nova classe de Serviço (`Service`)**
   - Transforme a lógica de leitura RFID já existente (uso de `startInventoryTag()` e `readTagFromBuffer()`) para rodar dentro do ciclo de vida de um `Service`
   - Adicione uma thread que fique escutando os EPCs continuamente
   - A cada EPC lido, envie um `Broadcast Intent` com action `com.rfid.EPC_READED` e um extra chamado `"epc"` contendo o valor da tag lida

2. **Atualize o `AndroidManifest.xml`**
   - Dentro da tag `<application>`, declare o novo serviço com os atributos `enabled=true` e `exported=true`
   - Mantenha permissões de:
     - Bluetooth
     - Leitura/Escrita de armazenamento (caso necessário)
     - Qualquer outra exigida pelo SDK

3. **Garanta a inicialização controlada**
   - O serviço não deve iniciar automaticamente por padrão
   - Deve ser iniciado **manualmente pelo app Delphi**, via `startService()`, quando ele for aberto
   - O serviço deve continuar ativo enquanto o Delphi estiver em primeiro plano
   - Pode ser finalizado por `stopService()` vindo do Delphi (opcional)

4. **Evite interface gráfica**
   - Toda lógica de exibição de tags, listas, tabelas ou radar deve ser descartada
   - A ponte precisa funcionar 100% em background
   - Nenhuma `Activity` ou `Fragment` é necessária

## 🛰️ ESPECIFICAÇÃO DO BROADCAST

- **Action:** `com.rfid.EPC_READED`
- **Extra:** chave `"epc"`, valor string com o código EPC lido
- **Tipo:** Local Broadcast (não exige `exported receiver` do lado Delphi)
- **Frequência:** imediato, a cada EPC lido (com controle opcional de duplicação)

## 🧠 FLUXO DE EXECUÇÃO ESPERADO

1. App Delphi (FMX) inicia
2. Delphi dispara `Intent` com `startService()` para iniciar a ponte
3. Serviço Java é inicializado e começa a escanear as tags RFID
4. A cada EPC detectado, o serviço envia um `Broadcast Intent`
5. O app Delphi recebe via `BroadcastReceiver` e processa os dados
6. Ao encerrar o app Delphi, o serviço pode ser parado via `stopService()` (se necessário)

## 🧾 SAÍDA ESPERADA

Ao final da execução deste script, a IA ou agente responsável deve entregar:

1. Um projeto Android Studio com:
   - Serviço funcional
   - Manifesto configurado
   - Dependência `.aar` do SDK incluída
2. Um APK compilável que:
   - Pode ser iniciado por outro app
   - Envia EPCs por Broadcast
3. Instruções para o app Delphi com:
   - Nome da `action`
   - Chave do `extra`
   - Exemplo de fluxo de recepção

## 🔐 REQUISITOS TÉCNICOS

- O projeto usa o SDK da Chainway (`RFIDWithUHFUART`)
- Requer Android SDK mínimo API 23 (Android 6.0)
- A ponte pode ser entregue como APK separado ou embutido via plugin/module
- Serviço deve ser leve, estável e funcionar mesmo com a tela desligada
"""

md_path = "/mnt/data/script_ponte_rfid_java_delphi.md"
with open(md_path, "w") as f:
    f.write(md_content)

md_path
