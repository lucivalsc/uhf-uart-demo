# Documentação dos Arquivos Java do Projeto UHF-UART-Demo

Este documento relaciona todos os arquivos Java do projeto e suas respectivas funções.

## Pacote Principal (com.example.uhf)

| Arquivo | Descrição |
|---------|-----------|
| BootBroadcastReceiver.java | Receptor de broadcast para inicialização do dispositivo, permitindo que a aplicação seja executada automaticamente na inicialização do sistema. |
| ErrorCodeManage.java | Gerenciador de códigos de erro, responsável por traduzir códigos de erro em mensagens compreensíveis. |
| FileUtils.java | Utilitário para operações com arquivos, como leitura, escrita e manipulação de arquivos no sistema. |
| RadarTAGInfo.java | Classe de modelo para armazenar informações de tags RFID no modo radar. |
| UhfInfo.java | Classe de modelo que encapsula informações sobre tags UHF, incluindo lista de tags, tempo de inventário, contagem de leituras e tags selecionadas. |

## Atividades (com.example.uhf.activity)

| Arquivo | Descrição |
|---------|-----------|
| BaseTabFragmentActivity.java | Classe base para atividades com abas, gerencia a instância do leitor RFID, permissões de armazenamento e inicialização do hardware. |
| TestActivity.java | Atividade para testes de funcionalidades específicas. |
| UHFMainActivity.java | Atividade principal que gerencia o ciclo de vida da aplicação, hospeda os fragmentos e controla o leitor RFID. |

## Adaptadores (com.example.uhf.adapter)

| Arquivo | Descrição |
|---------|-----------|
| ViewPagerAdapter.java | Adaptador para o ViewPager, gerenciando a navegação entre fragmentos. |

## Navegador de Arquivos (com.example.uhf.filebrowser)

| Arquivo | Descrição |
|---------|-----------|
| FileManagerActivity.java | Atividade para navegação e seleção de arquivos no sistema de arquivos do dispositivo. |
| IResultData.java | Interface para retorno de dados do navegador de arquivos. |
| IconifiedText.java | Classe de modelo para representar itens de arquivo com ícones. |
| IconifiedTextListAdapter.java | Adaptador para exibir itens IconifiedText em uma lista. |
| IconifiedTextView.java | View personalizada para exibir itens IconifiedText. |

## Fragmentos (com.example.uhf.fragment)

| Arquivo | Descrição |
|---------|-----------|
| BlockPermalockFragment.java | Fragmento para bloqueio permanente de blocos de memória em tags RFID. |
| BlockWriteFragment.java | Fragmento para escrita em bloco em tags RFID. |
| KeyDwonFragment.java | Fragmento base que implementa tratamento de eventos de teclas. |
| UHFKillFragment.java | Fragmento para desativação permanente (kill) de tags RFID. |
| UHFLightFragment.java | Fragmento para controle de iluminação de tags RFID específicas. |
| UHFLocationFragment.java | Fragmento para localização de tags RFID específicas. |
| UHFLockFragment.java | Fragmento para bloqueio de tags RFID com diferentes níveis de proteção. |
| UHFRadarLocationFragment.java | Fragmento para visualização da localização de tags em formato de radar. |
| UHFReadTagFragment.java | Fragmento para leitura de tags RFID com suporte a modos único e contínuo. |
| UHFReadWriteFragment.java | Fragmento para operações de leitura e escrita em tags RFID. |
| UHFSetFragment.java | Fragmento para configuração de parâmetros do leitor RFID. |
| UHFUpgradeFragment.java | Fragmento para atualização de firmware do leitor RFID. |

## Ferramentas (com.example.uhf.tools)

| Arquivo | Descrição |
|---------|-----------|
| CheckUtils.java | Utilitário para validação de dados e verificações diversas. |
| ExcelUtils.java | Utilitário para manipulação de arquivos Excel. |
| ExportExcelAsyncTask.java | Tarefa assíncrona para exportação de dados para Excel. |
| NumberTool.java | Utilitário para manipulação e conversão de números. |
| StringUtils.java | Utilitário para manipulação de strings. |
| UIHelper.java | Utilitário para operações de interface do usuário, como exibição de mensagens Toast. |

## Componentes de Visualização (com.example.uhf.view)

| Arquivo | Descrição |
|---------|-----------|
| CircleSeekBar.java | Controle deslizante circular para ajuste de potência. |
| RadarBackgroundView.java | View para o fundo da visualização de radar. |
| RadarPanelView.java | View para o painel de radar que exibe as tags. |
| RadarView.java | View composta que integra o fundo e o painel para visualização completa do radar. |
| UhfLocationCanvasView.java | View para visualização da localização de tags UHF. |

## Widgets (com.example.uhf.widget)

| Arquivo | Descrição |
|---------|-----------|
| LazyViewPager.java | ViewPager personalizado com carregamento preguiçoso de fragmentos. |
| NoScrollViewPager.java | ViewPager que desabilita o deslizamento entre páginas. |

## Testes

| Arquivo | Descrição |
|---------|-----------|
| ExampleInstrumentedTest.java | Testes instrumentados para a aplicação. |
| ExampleUnitTest.java | Testes unitários para a aplicação. |
