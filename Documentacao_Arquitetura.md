# Documentação de Arquitetura do Projeto UHF-UART-Demo

## 1. Visão Geral do Projeto

O projeto "uhf-uart-demo" é uma aplicação Android desenvolvida para gerenciar e interagir com leitores RFID UHF (Ultra High Frequency) através de comunicação UART. A aplicação permite realizar diversas operações com tags RFID, como leitura, escrita, bloqueio, desativação, localização e atualização de firmware do leitor.

## 2. Estrutura do Projeto

### 2.1 Organização do Projeto
- **Tipo de Projeto**: Aplicação Android com Gradle
- **Módulos**: app (principal)
- **Linguagem**: Java
- **Versão mínima do SDK**: Definida no arquivo build.gradle
- **Bibliotecas Externas**:
  - DeviceAPI_ver20250209_release.aar: API principal para comunicação com o hardware RFID
  - jxl.jar e poi-*.jar: Bibliotecas para manipulação de arquivos Excel
  - xUtils-2.5.5.jar: Biblioteca de utilidades para Android

### 2.2 Estrutura de Pacotes
- **com.example.uhf.activity**: Contém as atividades principais da aplicação
- **com.example.uhf.fragment**: Contém os fragmentos para diferentes funcionalidades RFID
- **com.example.uhf.tools**: Utilitários e ferramentas auxiliares
- **com.example.uhf.view**: Componentes de visualização personalizados
- **com.example.uhf.filebrowser**: Componentes para navegação de arquivos
- **com.example.uhf.adapter**: Adaptadores para listas e ViewPagers
- **com.example.uhf.widget**: Widgets personalizados para a interface

## 3. Componentes Principais

### 3.1 Atividades

#### 3.1.1 UHFMainActivity
- **Função**: Atividade principal que gerencia o ciclo de vida da aplicação e hospeda os fragmentos
- **Características**:
  - Estende BaseTabFragmentActivity
  - Configura o FragmentTabHost para hospedar múltiplos fragmentos
  - Gerencia o ciclo de vida do leitor RFID
  - Implementa feedback sonoro para operações
  - Gerencia permissões de armazenamento externo
  - Exporta dados de tags escaneadas

#### 3.1.2 BaseTabFragmentActivity
- **Função**: Classe base que fornece funcionalidades comuns para atividades com abas
- **Características**:
  - Gerencia a instância do leitor RFIDWithUHFUART
  - Trata solicitações de permissão para armazenamento externo
  - Inicializa o leitor UHF de forma assíncrona
  - Gerencia opções de menu para versão UHF e exportação de dados
  - Trata eventos de teclas para teclas de hardware
  - Fornece métodos utilitários para validação de entrada e recuperação de versão

#### 3.1.3 FileManagerActivity
- **Função**: Gerencia a navegação e seleção de arquivos no sistema de arquivos
- **Características**:
  - Estende ListActivity para exibir listas de arquivos e diretórios
  - Permite navegação entre diretórios
  - Exibe ícones diferentes para diferentes tipos de arquivos
  - Envia broadcasts com o caminho do arquivo selecionado
  - Utilizada principalmente para seleção de arquivos de firmware para atualização

### 3.2 API de Hardware

#### 3.2.1 RFIDWithUHFUART
- **Função**: API principal que gerencia a comunicação com o hardware RFID UHF
- **Características**:
  - Fornece métodos para todas as operações RFID (leitura, escrita, bloqueio, etc.)
  - Implementa callbacks para notificação de eventos
  - Gerencia o ciclo de vida do hardware
  - Fornece constantes para bancos de memória e outros parâmetros RFID
  - Disponibilizada através da biblioteca DeviceAPI_ver20250209_release.aar

### 3.3 Fragmentos

#### 3.3.1 UHFReadTagFragment
- **Função**: Gerencia a leitura de tags RFID
- **Características**:
  - Interface para controle de escaneamento
  - Suporte a modos de inventário único e contínuo
  - Filtragem de tags por EPC, TID ou USER
  - Exibição de lista de tags com contagem e RSSI
  - Feedback sonoro para leitura de tags

#### 3.3.2 UHFSetFragment
- **Função**: Configura parâmetros do leitor RFID
- **Características**:
  - Ajuste de potência de transmissão
  - Configuração de frequência
  - Seleção de protocolo
  - Configuração de parâmetros de link
  - Seleção de banco de memória
  - Configuração de sessão e modos de inventário rápido

#### 3.3.3 UHFReadWriteFragment
- **Função**: Gerencia operações de leitura e escrita em tags RFID
- **Características**:
  - Seleção de banco de memória (EPC, TID, USER)
  - Especificação de endereço e comprimento
  - Configuração de senha de acesso
  - Filtragem de tags para operações específicas
  - Validação de entrada e feedback de operações
  - Suporte a escrita segmentada para dados grandes

#### 3.3.4 UHFLockFragment
- **Função**: Gerencia o bloqueio de tags RFID
- **Características**:
  - Proteção por senha
  - Opções de filtragem
  - Interface para geração de código de bloqueio
  - Diferentes níveis de bloqueio (aberto, bloqueado, permanente)
  - Feedback sonoro para operações de bloqueio

#### 3.3.5 UHFKillFragment
- **Função**: Gerencia a desativação permanente de tags RFID
- **Características**:
  - Proteção por senha
  - Opções de filtragem
  - Confirmação de operação irreversível
  - Feedback sonoro para operações de desativação

#### 3.3.6 UHFLocationFragment
- **Função**: Localiza tags RFID específicas
- **Características**:
  - Controles para iniciar/parar rastreamento
  - Ajuste de potência para controle de sensibilidade
  - Feedback sonoro proporcional à proximidade da tag
  - Filtragem por EPC

#### 3.3.7 UHFRadarLocationFragment
- **Função**: Visualiza a localização de tags em formato de radar
- **Características**:
  - Animação de radar para visualização
  - Controle de potência para ajuste de sensibilidade
  - Entrada de EPC para rastreamento específico
  - Feedback sonoro baseado na proximidade
  - Rotação do radar conforme ângulo da tag

#### 3.3.8 UHFLightFragment
- **Função**: Controla a iluminação de tags RFID específicas
- **Características**:
  - Modos de iluminação único e contínuo
  - Filtragem por EPC, TID ou USER
  - Interface para configuração de parâmetros de filtragem
  - Utiliza a API readData para acionar a iluminação

#### 3.3.9 BlockWriteFragment
- **Função**: Realiza escrita em bloco em tags RFID
- **Características**:
  - Seleção de banco de memória
  - Especificação de endereço e comprimento
  - Configuração de senha de acesso
  - Filtragem de tags para operações específicas
  - Validação de entrada hexadecimal
  - Feedback sonoro para operações de escrita

#### 3.3.10 BlockPermalockFragment
- **Função**: Gerencia o bloqueio permanente de blocos de memória em tags RFID
- **Características**:
  - Interface com checkboxes para seleção de blocos
  - Geração de máscara de bloqueio
  - Filtragem por EPC, TID ou USER
  - Proteção por senha
  - Feedback visual e sonoro para operações

#### 3.3.11 UHFUpgradeFragment
- **Função**: Gerencia a atualização de firmware do leitor RFID
- **Características**:
  - Seleção de arquivo de firmware (.bin)
  - Suporte a diferentes tipos de módulos (UHF e EX10)
  - Exibição de progresso de atualização
  - Verificação de versão antes e após atualização
  - Feedback visual e sonoro para operações

### 3.4 Classes de Modelo

#### 3.4.1 UhfInfo
- **Função**: Encapsula informações sobre tags UHF
- **Características**:
  - Lista de tags
  - Tempo de inventário
  - Contagem de leituras
  - Contagem de tags
  - Índice de tag selecionada
  - String EPC de tag selecionada

#### 3.4.2 RadarTAGInfo
- **Função**: Armazena informações específicas para visualização de radar
- **Características**:
  - Dados de posição da tag
  - Intensidade do sinal
  - Identificação da tag

### 3.5 Componentes de Visualização Personalizados

#### 3.5.1 RadarView
- **Função**: Visualização em formato de radar para localização de tags
- **Características**:
  - Animação de varredura de radar
  - Exibição de tags como pontos no radar
  - Rotação conforme ângulo da tag
  - Limpeza e atualização dinâmica do painel

#### 3.5.2 CircleSeekBar
- **Função**: Controle deslizante circular para ajuste de potência
- **Características**:
  - Interface visual para ajuste de potência
  - Feedback visual de nível selecionado

#### 3.5.3 UhfLocationCanvasView
- **Função**: Visualização para localização de tags UHF
- **Características**:
  - Exibição gráfica da proximidade da tag
  - Atualização dinâmica baseada em dados de RSSI

### 3.6 Utilitários

#### 3.6.1 ExcelUtils e ExportExcelAsyncTask
- **Função**: Exportação de dados para arquivos Excel
- **Características**:
  - Criação de planilhas com dados de tags
  - Exportação assíncrona para não bloquear a UI
  - Suporte a diferentes formatos de dados

#### 3.6.2 UIHelper
- **Função**: Utilitário para operações de interface do usuário
- **Características**:
  - Exibição de mensagens Toast
  - Formatação de mensagens

#### 3.6.3 StringUtils e NumberTool
- **Função**: Utilitários para manipulação de strings e números
- **Características**:
  - Validação de strings
  - Conversão entre formatos
  - Verificação de valores nulos ou vazios

## 4. Fluxos Principais

### 4.1 Inicialização da Aplicação
1. UHFMainActivity é iniciada
2. Verificação de permissões de armazenamento
3. Inicialização do leitor RFID via BaseTabFragmentActivity
4. Configuração do FragmentTabHost com os fragmentos disponíveis
5. Carregamento do fragmento inicial (geralmente UHFReadTagFragment)

### 4.2 Leitura de Tags
1. Usuário navega para UHFReadTagFragment
2. Configuração de parâmetros de leitura (filtros, etc.)
3. Início da operação de leitura (única ou contínua)
4. Processamento de tags lidas via callback
5. Atualização da interface com informações das tags
6. Feedback sonoro para confirmação de leitura

### 4.3 Escrita em Tags
1. Usuário navega para UHFReadWriteFragment
2. Seleção de banco de memória, endereço e comprimento
3. Entrada de dados a serem escritos
4. Configuração de filtros (opcional)
5. Execução da operação de escrita
6. Feedback sonoro para confirmação de sucesso/falha

### 4.4 Localização de Tags
1. Usuário navega para UHFLocationFragment ou UHFRadarLocationFragment
2. Entrada de EPC da tag a ser localizada
3. Início da operação de localização
4. Processamento contínuo de dados de proximidade via callback
5. Atualização da interface (indicador de proximidade ou visualização de radar)
6. Feedback sonoro proporcional à proximidade

### 4.5 Atualização de Firmware
1. Usuário navega para UHFUpgradeFragment
2. Seleção do arquivo de firmware (.bin)
3. Seleção do tipo de módulo (UHF ou EX10)
4. Início do processo de atualização
5. Transição do leitor para modo de boot
6. Transferência do firmware em blocos
7. Reinicialização do leitor
8. Verificação da nova versão

## 5. Interações entre Componentes

### 5.1 Comunicação Fragmento-Atividade
- Os fragmentos acessam a instância do leitor RFID através da atividade principal (mContext.mReader)
- A atividade principal fornece métodos para feedback sonoro (playSound)
- Os fragmentos notificam a atividade sobre mudanças de estado através de callbacks

### 5.2 Comunicação com o Hardware RFID
- Toda interação com o hardware RFID é feita através da API RFIDWithUHFUART
- As operações assíncronas utilizam callbacks para notificar resultados
- O ciclo de vida do hardware é gerenciado pela atividade principal

### 5.3 Persistência de Dados
- Os dados de tags são mantidos em memória durante a sessão
- Funcionalidade de exportação para arquivos Excel/TXT para persistência externa
- Configurações do leitor são aplicadas diretamente ao hardware

## 6. Considerações de Design

### 6.1 Padrões de Design
- **Fragmentos**: Separação de funcionalidades em fragmentos independentes
- **Callbacks**: Utilização de callbacks para operações assíncronas
- **Singleton**: Instância única do leitor RFID gerenciada pela atividade base
- **Herança**: Fragmentos específicos estendem classes base para compartilhar funcionalidades comuns

### 6.2 Tratamento de Erros
- Feedback visual e sonoro para operações bem-sucedidas e falhas
- Validação de entrada para prevenir operações inválidas
- Tratamento de exceções para operações de hardware

### 6.3 Interface do Usuário
- Design baseado em abas para navegação entre funcionalidades
- Componentes visuais personalizados para funcionalidades específicas (radar)
- Feedback sonoro para confirmação de operações
- Indicadores visuais de estado (contagem de tags, RSSI, etc.)

## 7. Conclusão

O projeto "uhf-uart-demo" é uma aplicação Android completa para gerenciamento de leitores RFID UHF. Sua arquitetura modular baseada em fragmentos permite uma clara separação de funcionalidades, facilitando a manutenção e extensão. A integração com a API RFIDWithUHFUART fornece acesso a todas as capacidades do hardware RFID, permitindo operações avançadas como leitura, escrita, bloqueio, localização e atualização de firmware.

A aplicação demonstra boas práticas de desenvolvimento Android, incluindo gerenciamento de ciclo de vida, tratamento de permissões, feedback ao usuário e separação de responsabilidades. O design da interface proporciona acesso intuitivo às diversas funcionalidades, com feedback visual e sonoro para melhorar a experiência do usuário.

Esta documentação fornece uma visão abrangente da arquitetura e funcionamento do projeto, servindo como referência para desenvolvedores que precisam entender, manter ou estender a aplicação.

## 8. Referências

Para informações detalhadas sobre cada arquivo Java do projeto e suas funções específicas, consulte o arquivo [Documentacao_Arquivos_Java.md](Documentacao_Arquivos_Java.md) na pasta raiz do projeto.
