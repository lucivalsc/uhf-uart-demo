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

import java.util.HashMap;
import java.util.Map;

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
    public static final String ACTION_STOP_SERVICE = "com.rfid.STOP_SERVICE";
    public static final String ACTION_EPC_READED = "com.rfid.EPC_READED";
    public static final String EXTRA_EPC = "epc";
    public static final String EXTRA_RSSI = "rssi";   // Força do sinal (opcional)

    private RFIDWithUHFUART mReader;
    private boolean isReading = false;
    private Thread inventoryThread;
    
    // Cache para controle de leituras repetidas
    private final Map<String, Long> epcLastReadMap = new HashMap<>();

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
                startForegroundService();
                startInventory();
            } else if (ACTION_STOP_SERVICE.equals(action)) {
                stopInventory();
                stopSelf();
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

        // Constrói a notificação com ação para parar o serviço
        NotificationCompat.Builder builder = new NotificationCompat.Builder(this, NOTIFICATION_CHANNEL_ID)
                .setContentTitle("Leitor RFID Ativo")
                .setContentText("Serviço de leitura RFID em execução")
                .setStyle(new NotificationCompat.BigTextStyle() // Estilo expandido para mais informações
                        .bigText("O serviço de leitura RFID está em execução. Toque em 'Parar Serviço RFID' para encerrar."))
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
        Log.d(TAG, "Serviço RFID iniciado em foreground com notificação visível");
    }

    /**
     * Inicia a leitura contínua de tags RFID
     */
    private void startInventory() {
        if (mReader != null && !isReading) {
            isReading = true;
            Log.d(TAG, "Iniciando leitura de tags RFID");

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
                Log.d(TAG, "Inventário RFID iniciado com sucesso");
            } else {
                Log.e(TAG, "Falha ao iniciar inventário RFID");
                isReading = false;
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
}
