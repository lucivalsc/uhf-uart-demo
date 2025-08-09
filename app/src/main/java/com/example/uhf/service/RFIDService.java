package com.example.uhf.service;

import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.os.Build;
import android.os.IBinder;
import android.os.SystemClock;
import android.util.Log;

import androidx.core.app.NotificationCompat;

import com.example.uhf.R;
import com.rscja.deviceapi.RFIDWithUHFUART;
import com.rscja.deviceapi.entity.InventoryParameter;
import com.rscja.deviceapi.entity.UHFTAGInfo;
import com.rscja.deviceapi.interfaces.IUHFInventoryCallback;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;

/**
 * Serviço para leitura contínua de tags RFID e envio via Broadcast Intent
 * Criado para integração com aplicativos Delphi FMX
 * 
 * Este serviço executa em segundo plano e não possui interface visual.
 * Ele continuará funcionando mesmo quando o aplicativo não estiver em foco.
 */
public class RFIDService extends Service {
    private static final String TAG = "RFIDService";
    private static final String NOTIFICATION_CHANNEL_ID = "rfid_service_channel";
    private static final int NOTIFICATION_ID = 1;
    private static final long MIN_BROADCAST_INTERVAL = 100; // Intervalo mínimo entre broadcasts em milissegundos

    public static final String ACTION_START_SERVICE = "com.rfid.START_SERVICE";
    public static final String ACTION_START_BATCH_SERVICE = "com.rfid.START_BATCH_SERVICE";
    public static final String ACTION_STOP_SERVICE = "com.rfid.STOP_SERVICE";
    public static final String ACTION_GET_BATCH_DATA = "com.rfid.GET_BATCH_DATA";
    public static final String ACTION_CLIENT_HEARTBEAT = "com.rfid.CLIENT_HEARTBEAT";
    
    public static final String ACTION_EPC_READED = "com.rfid.EPC_READED";
    public static final String ACTION_BATCH_COUNT_UPDATE = "com.rfid.BATCH_COUNT_UPDATE";
    public static final String ACTION_BATCH_DATA_RESPONSE = "com.rfid.BATCH_DATA_RESPONSE";
    
    public static final String EXTRA_EPC = "epc";
    public static final String EXTRA_RSSI = "rssi";   // Força do sinal (opcional)
    public static final String EXTRA_COUNT = "count"; // Contagem de itens no modo batch
    public static final String EXTRA_EPC_LIST = "epc_list"; // Lista de EPCs no modo batch
    public static final String EXTRA_RSSI_LIST = "rssi_list"; // Lista de RSSIs no modo batch
    public static final String EXTRA_OPERATION_MODE = "operation_mode"; // Modo de operação

    private RFIDWithUHFUART mReader;
    private boolean isReading = false;
    private Thread inventoryThread;
    
    // Cache para controle de leituras repetidas
    private final Map<String, Long> epcLastReadMap = new HashMap<>();
    
    // Variáveis para modo batch
    private boolean isBatchMode = false;
    private final List<String> batchEpcList = new ArrayList<>();
    private final List<String> batchRssiList = new ArrayList<>();
    private Timer countUpdateTimer;
    private Timer heartbeatTimer;
    private long lastHeartbeat = 0;
    private static final long HEARTBEAT_TIMEOUT = 30000; // 30 segundos
    private static final long COUNT_UPDATE_INTERVAL = 10000; // 10 segundos

    @Override
    public void onCreate() {
        super.onCreate();
        Log.d(TAG, "Serviço RFID criado");
        
        // Inicializa o leitor RFID
        initUHFReader();
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        if (intent != null) {
            String action = intent.getAction();
            if (ACTION_START_SERVICE.equals(action)) {
                isBatchMode = false;
                startForegroundService();
                startInventory();
            } else if (ACTION_START_BATCH_SERVICE.equals(action)) {
                isBatchMode = true;
                startForegroundService();
                startBatchInventory();
            } else if (ACTION_STOP_SERVICE.equals(action)) {
                stopInventory();
                stopBatchMode();
                stopSelf();
            } else if (ACTION_GET_BATCH_DATA.equals(action)) {
                sendBatchData();
                stopInventory();
                stopBatchMode();
                stopSelf();
            } else if (ACTION_CLIENT_HEARTBEAT.equals(action)) {
                updateHeartbeat();
            }
        }
        
        // Se o serviço for encerrado pelo sistema, não é necessário reiniciá-lo automaticamente
        return START_NOT_STICKY;
    }

    @Override
    public IBinder onBind(Intent intent) {
        return null; // Este serviço não suporta ligação
    }

    @Override
    public void onDestroy() {
        stopInventory();
        stopBatchMode();
        releaseUHFReader();
        Log.d(TAG, "Serviço RFID destruído");
        super.onDestroy();
    }

    /**
     * Inicializa o leitor RFID
     */
    private void initUHFReader() {
        try {
            mReader = RFIDWithUHFUART.getInstance();
            if (mReader != null) {
                if (!mReader.init(this)) {
                    Log.e(TAG, "Falha ao inicializar o leitor RFID");
                } else {
                    Log.d(TAG, "Leitor RFID inicializado com sucesso");
                }
            }
        } catch (Exception e) {
            Log.e(TAG, "Erro ao inicializar o leitor RFID", e);
        }
    }

    /**
     * Libera recursos do leitor RFID
     */
    private void releaseUHFReader() {
        if (mReader != null) {
            mReader.free();
            mReader = null;
        }
    }

    /**
     * Configura o serviço como foreground service para evitar que seja encerrado pelo sistema
     * com notificação visível na barra de status
     */
    private void startForegroundService() {
        updateForegroundNotification();
    }
    
    /**
     * Atualiza a notificação do foreground service baseada no modo atual
     */
    private void updateForegroundNotification() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            // Cria um canal de notificação com prioridade média para garantir visibilidade
            NotificationChannel channel = new NotificationChannel(
                    NOTIFICATION_CHANNEL_ID,
                    "Serviço RFID",
                    NotificationManager.IMPORTANCE_DEFAULT); // Alterado para DEFAULT para maior visibilidade
            channel.setDescription("Canal para notificação do serviço RFID");
            channel.enableLights(true); // Ativar luzes de notificação
            channel.setLightColor(android.graphics.Color.BLUE); // Cor azul para destaque
            channel.enableVibration(false);
            channel.setShowBadge(true); // Mostra badge no ícone do aplicativo
            channel.setSound(null, null);
            
            NotificationManager manager = (NotificationManager) getSystemService(Context.NOTIFICATION_SERVICE);
            manager.createNotificationChannel(channel);
        }

        // Cria intent para parar o serviço quando a notificação for clicada
        Intent stopIntent = new Intent(this, RFIDService.class);
        stopIntent.setAction(ACTION_STOP_SERVICE);
        PendingIntent pendingStopIntent = PendingIntent.getService(
                this,
                0,
                stopIntent,
                PendingIntent.FLAG_UPDATE_CURRENT | (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M ? PendingIntent.FLAG_IMMUTABLE : 0)
        );
        
        // Cria uma ação de "parar" mais destacada para o serviço
        NotificationCompat.Action stopAction = new NotificationCompat.Action.Builder(
                android.R.drawable.ic_delete, // Ícone mais visível
                "Parar Serviço RFID", // Nome mais claro
                pendingStopIntent
        ).build();

        // Cria intent para abrir a activity (caso precise reabrir depois)
        Intent activityIntent = new Intent(this, RFIDService.class); // Substituir pelo nome da activity se necessário
        activityIntent.setAction(ACTION_START_SERVICE);
        PendingIntent pendingActivityIntent = PendingIntent.getService(
                this,
                1, // ID diferente para diferenciar do intent de parar
                activityIntent,
                PendingIntent.FLAG_UPDATE_CURRENT | (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M ? PendingIntent.FLAG_IMMUTABLE : 0)
        );

        // Define textos baseados no modo de operação
        String title, text, bigText;
        if (isBatchMode) {
            title = "Leitor RFID Ativo (Modo Batch)";
            text = "Acumulando dados - Enviará quando solicitado";
            bigText = "O serviço RFID está no modo BATCH. Os dados estão sendo acumulados e serão enviados apenas quando solicitados. Toque em 'Parar Serviço RFID' para encerrar.";
        } else {
            title = "Leitor RFID Ativo (Modo Normal)";
            text = "Enviando dados imediatamente";
            bigText = "O serviço RFID está no modo NORMAL. Os dados são enviados imediatamente quando lidos. Toque em 'Parar Serviço RFID' para encerrar.";
        }
        
        // Constrói a notificação com ação para parar o serviço
        NotificationCompat.Builder builder = new NotificationCompat.Builder(this, NOTIFICATION_CHANNEL_ID)
                .setContentTitle(title)
                .setContentText(text)
                .setStyle(new NotificationCompat.BigTextStyle() // Estilo expandido para mais informações
                        .bigText(bigText))
                .setSmallIcon(R.drawable.ic_launcher)
                .setPriority(NotificationCompat.PRIORITY_HIGH) // Prioridade alta para maior visibilidade
                .setOngoing(true) // Impede que o usuário deslize para remover
                .setUsesChronometer(true) // Mostra o tempo de execução do serviço
                .addAction(stopAction) // Ação para parar
                .setContentIntent(pendingStopIntent) // Toque na notificação também para o serviço
                .setVisibility(NotificationCompat.VISIBILITY_PUBLIC)
                .setShowWhen(true); // Mostra quando a notificação foi criada

        // Inicia o serviço como foreground com a notificação melhorada
        startForeground(NOTIFICATION_ID, builder.build());
        
        // Log para depuração
        String modeText = isBatchMode ? "modo batch" : "modo normal";
        Log.d(TAG, "Serviço RFID iniciado em foreground com notificação visível (" + modeText + ")");
    }

    /**
     * Inicia a leitura contínua de tags RFID (modo normal)
     */
    private void startInventory() {
        if (mReader != null && !isReading) {
            isReading = true;
            Log.d(TAG, "Iniciando leitura de tags RFID (modo normal)");

            // Limpa o cache de leituras ao iniciar novo ciclo
            epcLastReadMap.clear();
            
            // Define o callback para receber as tags lidas
            mReader.setInventoryCallback(new IUHFInventoryCallback() {
                @Override
                public void callback(UHFTAGInfo info) {
                    if (info != null && info.getEPC() != null && !info.getEPC().isEmpty()) {
                        // Envia o EPC e RSSI via broadcast com controle de cache
                        String rssi = null;
                        try {
                            rssi = info.getRssi();
                        } catch (Exception e) {
                            // Alguns dispositivos podem não suportar RSSI
                        }
                        sendEPCBroadcast(info.getEPC(), rssi);
                    }
                }
            });

            // Configura parâmetros de inventário para otimização
            InventoryParameter inventoryParameter = new InventoryParameter();
            // Pode-se ajustar parâmetros adicionais aqui se necessário
            
            // Inicia o inventário contínuo
            if (mReader.startInventoryTag(inventoryParameter)) {
                Log.d(TAG, "Inventário RFID iniciado com sucesso (modo normal)");
            } else {
                Log.e(TAG, "Falha ao iniciar inventário RFID");
                isReading = false;
            }
        }
    }

    /**
     * Inicia a leitura contínua de tags RFID (modo batch)
     */
    private void startBatchInventory() {
        if (mReader != null && !isReading) {
            isReading = true;
            Log.d(TAG, "Iniciando leitura de tags RFID (modo batch)");

            // Limpa os dados do modo batch
            synchronized (batchEpcList) {
                batchEpcList.clear();
                batchRssiList.clear();
            }
            epcLastReadMap.clear();
            
            // Inicia o heartbeat e timer de contagem
            startHeartbeatMonitoring();
            startCountUpdateTimer();
            
            // Define o callback para receber as tags lidas
            mReader.setInventoryCallback(new IUHFInventoryCallback() {
                @Override
                public void callback(UHFTAGInfo info) {
                    if (info != null && info.getEPC() != null && !info.getEPC().isEmpty()) {
                        String rssi = null;
                        try {
                            rssi = info.getRssi();
                        } catch (Exception e) {
                            // Alguns dispositivos podem não suportar RSSI
                        }
                        addToBatchList(info.getEPC(), rssi);
                    }
                }
            });

            // Configura parâmetros de inventário para otimização
            InventoryParameter inventoryParameter = new InventoryParameter();
            
            // Inicia o inventário contínuo
            if (mReader.startInventoryTag(inventoryParameter)) {
                Log.d(TAG, "Inventário RFID iniciado com sucesso (modo batch)");
            } else {
                Log.e(TAG, "Falha ao iniciar inventário RFID (modo batch)");
                isReading = false;
                stopBatchMode();
            }
        }
    }

    /**
     * Para a leitura de tags RFID
     */
    private void stopInventory() {
        if (mReader != null && isReading) {
            if (mReader.stopInventory()) {
                Log.d(TAG, "Inventário RFID parado com sucesso");
            } else {
                Log.e(TAG, "Falha ao parar inventário RFID");
            }
            isReading = false;
        }
    }

    /**
     * Envia um EPC via Broadcast Intent com controle de cache para evitar duplicações
     * @param epc O código EPC lido
     * @param rssi Valor RSSI (opcional, pode ser null)
     */
    private void sendEPCBroadcast(String epc, String rssi) {
        if (epc == null || epc.isEmpty()) {
            return;
        }
        
        long currentTime = System.currentTimeMillis();
        
        // Verifica se o EPC já foi lido recentemente para evitar duplicações
        Long lastReadTime = epcLastReadMap.get(epc);
        if (lastReadTime != null && (currentTime - lastReadTime) < MIN_BROADCAST_INTERVAL) {
            // Ignora leituras muito próximas do mesmo EPC
            return;
        }
        
        // Atualiza o cache com o tempo da última leitura
        epcLastReadMap.put(epc, currentTime);
        
        // Envia o broadcast com os dados
        Intent intent = new Intent(ACTION_EPC_READED);
        intent.putExtra(EXTRA_EPC, epc);
        if (rssi != null) {
            intent.putExtra(EXTRA_RSSI, rssi);
        }
        
        sendBroadcast(intent);
        Log.d(TAG, "EPC enviado via broadcast: " + epc + (rssi != null ? " (RSSI: " + rssi + ")" : ""));
    }
    
    /**
     * Sobrecarga do método sendEPCBroadcast para compatibilidade
     */
    private void sendEPCBroadcast(String epc) {
        sendEPCBroadcast(epc, null);
    }
    
    /**
     * Adiciona EPC à lista do modo batch com controle de duplicação
     */
    private void addToBatchList(String epc, String rssi) {
        if (epc == null || epc.isEmpty()) {
            return;
        }
        
        long currentTime = System.currentTimeMillis();
        
        // Verifica se o EPC já foi lido recentemente para evitar duplicações
        Long lastReadTime = epcLastReadMap.get(epc);
        if (lastReadTime != null && (currentTime - lastReadTime) < MIN_BROADCAST_INTERVAL) {
            // Ignora leituras muito próximas do mesmo EPC
            return;
        }
        
        // Atualiza o cache com o tempo da última leitura
        epcLastReadMap.put(epc, currentTime);
        
        // Adiciona à lista batch de forma sincronizada
        synchronized (batchEpcList) {
            batchEpcList.add(epc);
            batchRssiList.add(rssi != null ? rssi : "");
        }
        
        Log.d(TAG, "EPC adicionado ao batch: " + epc + (rssi != null ? " (RSSI: " + rssi + ")" : ""));
    }
    
    /**
     * Inicia o timer para envio periódico da contagem de itens
     */
    private void startCountUpdateTimer() {
        if (countUpdateTimer != null) {
            countUpdateTimer.cancel();
        }
        
        countUpdateTimer = new Timer("CountUpdateTimer");
        countUpdateTimer.scheduleAtFixedRate(new TimerTask() {
            @Override
            public void run() {
                sendCountUpdate();
            }
        }, COUNT_UPDATE_INTERVAL, COUNT_UPDATE_INTERVAL);
        
        Log.d(TAG, "Timer de atualização de contagem iniciado");
    }
    
    /**
     * Inicia o monitoramento de heartbeat do cliente
     */
    private void startHeartbeatMonitoring() {
        lastHeartbeat = System.currentTimeMillis(); // ✅ INICIALIZA CORRETAMENTE
        
        if (heartbeatTimer != null) {
            heartbeatTimer.cancel();
        }
        
        heartbeatTimer = new Timer("HeartbeatTimer");
        heartbeatTimer.scheduleAtFixedRate(new TimerTask() {
            @Override
            public void run() {
                checkClientAlive();
            }
        }, HEARTBEAT_TIMEOUT, HEARTBEAT_TIMEOUT / 2);
        
        Log.d(TAG, "Monitoramento de heartbeat iniciado - timeout: " + HEARTBEAT_TIMEOUT + "ms");
    }
    
    /**
     * Atualiza o timestamp do último heartbeat recebido
     */
    private void updateHeartbeat() {
        lastHeartbeat = System.currentTimeMillis();
        Log.d(TAG, "Heartbeat do cliente atualizado - próximo timeout em: " + HEARTBEAT_TIMEOUT + "ms");
    }
    
    /**
     * Verifica se o cliente ainda está vivo baseado no heartbeat
     * ✅ CORRIGIDO: Adiciona verificações de segurança para evitar crash
     */
    private void checkClientAlive() {
        try {
            long currentTime = System.currentTimeMillis();
            long timeSinceLastHeartbeat = currentTime - lastHeartbeat;
            
            Log.d(TAG, "Verificando heartbeat - tempo desde último: " + timeSinceLastHeartbeat + "ms");
            
            if (timeSinceLastHeartbeat > HEARTBEAT_TIMEOUT) {
                Log.w(TAG, "Cliente não responde há " + timeSinceLastHeartbeat + "ms - iniciando parada segura");
                
                // ✅ PARADA SEGURA: Para inventário primeiro, depois envia dados, depois mata serviço
                stopInventory();
                
                // ✅ Envia dados acumulados antes de parar (evita perda de dados)
                if (isBatchMode) {
                    Log.i(TAG, "Enviando dados batch antes de parar por timeout");
                    sendBatchData();
                    
                    // ✅ Aguarda um pouco para o broadcast ser processado
                    try {
                        Thread.sleep(1000);
                    } catch (InterruptedException e) {
                        Thread.currentThread().interrupt();
                    }
                }
                
                stopBatchMode();
                
                // ✅ Para o serviço com delay para evitar crash
                new Timer().schedule(new TimerTask() {
                    @Override
                    public void run() {
                        Log.i(TAG, "Parando serviço após timeout de heartbeat");
                        stopSelf();
                    }
                }, 500); // 500ms de delay
            }
        } catch (Exception e) {
            Log.e(TAG, "Erro ao verificar heartbeat do cliente", e);
            // ✅ Em caso de erro, não mata o serviço abruptamente
        }
    }
    
    /**
     * Envia atualização da contagem de itens lidos
     */
    private void sendCountUpdate() {
        int count;
        synchronized (batchEpcList) {
            count = batchEpcList.size();
        }
        
        Intent intent = new Intent(ACTION_BATCH_COUNT_UPDATE);
        intent.putExtra(EXTRA_COUNT, count);
        sendBroadcast(intent);
        
        // Atualiza a notificação com a contagem atual no modo batch
        if (isBatchMode) {
            updateNotificationWithCount(count);
        }
        
        Log.d(TAG, "Atualização de contagem enviada: " + count + " itens");
    }
    
    /**
     * Atualiza a notificação com a contagem atual de itens no modo batch
     */
    private void updateNotificationWithCount(int count) {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            NotificationChannel channel = new NotificationChannel(
                    NOTIFICATION_CHANNEL_ID,
                    "Serviço RFID",
                    NotificationManager.IMPORTANCE_DEFAULT);
            channel.setDescription("Canal para notificação do serviço RFID");
            channel.enableLights(true);
            channel.setLightColor(android.graphics.Color.BLUE);
            channel.enableVibration(false);
            channel.setShowBadge(true);
            channel.setSound(null, null);
            
            NotificationManager manager = (NotificationManager) getSystemService(Context.NOTIFICATION_SERVICE);
            manager.createNotificationChannel(channel);
        }

        Intent stopIntent = new Intent(this, RFIDService.class);
        stopIntent.setAction(ACTION_STOP_SERVICE);
        PendingIntent pendingStopIntent = PendingIntent.getService(
                this,
                0,
                stopIntent,
                PendingIntent.FLAG_UPDATE_CURRENT | (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M ? PendingIntent.FLAG_IMMUTABLE : 0)
        );

        NotificationCompat.Action stopAction = new NotificationCompat.Action.Builder(
                android.R.drawable.ic_delete,
                "Parar Serviço RFID",
                pendingStopIntent
        ).build();

        String title = "Leitor RFID Ativo (Modo Batch)";
        String text = "Acumulados: " + count + " itens - Enviará quando solicitado";
        String bigText = "O serviço RFID está no modo BATCH. " + count + " itens foram acumulados e serão enviados quando solicitados. Toque em 'Parar Serviço RFID' para encerrar.";
        
        NotificationCompat.Builder builder = new NotificationCompat.Builder(this, NOTIFICATION_CHANNEL_ID)
                .setContentTitle(title)
                .setContentText(text)
                .setStyle(new NotificationCompat.BigTextStyle()
                        .bigText(bigText))
                .setSmallIcon(R.drawable.ic_launcher)
                .setPriority(NotificationCompat.PRIORITY_HIGH)
                .setOngoing(true)
                .setUsesChronometer(true)
                .addAction(stopAction)
                .setContentIntent(pendingStopIntent)
                .setVisibility(NotificationCompat.VISIBILITY_PUBLIC)
                .setShowWhen(true);

        NotificationManager notificationManager = (NotificationManager) getSystemService(Context.NOTIFICATION_SERVICE);
        notificationManager.notify(NOTIFICATION_ID, builder.build());
    }
    
    /**
     * Envia todos os dados acumulados no modo batch
     */
    private void sendBatchData() {
        String[] epcArray;
        String[] rssiArray;
        
        synchronized (batchEpcList) {
            epcArray = batchEpcList.toArray(new String[0]);
            rssiArray = batchRssiList.toArray(new String[0]);
        }
        
        Intent intent = new Intent(ACTION_BATCH_DATA_RESPONSE);
        intent.putExtra(EXTRA_EPC_LIST, epcArray);
        intent.putExtra(EXTRA_RSSI_LIST, rssiArray);
        intent.putExtra(EXTRA_COUNT, epcArray.length);
        sendBroadcast(intent);
        
        Log.d(TAG, "Dados do batch enviados: " + epcArray.length + " itens");
    }
    
    /**
     * Para o modo batch e limpa recursos
     */
    private void stopBatchMode() {
        if (countUpdateTimer != null) {
            countUpdateTimer.cancel();
            countUpdateTimer = null;
        }
        
        if (heartbeatTimer != null) {
            heartbeatTimer.cancel();
            heartbeatTimer = null;
        }
        
        synchronized (batchEpcList) {
            batchEpcList.clear();
            batchRssiList.clear();
        }
        
        isBatchMode = false;
        Log.d(TAG, "Modo batch parado e recursos limpos");
    }
}
