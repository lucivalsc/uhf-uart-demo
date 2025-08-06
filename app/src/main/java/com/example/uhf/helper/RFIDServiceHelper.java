package com.example.uhf.helper;

import android.content.Context;
import android.content.Intent;
import android.os.Build;
import android.util.Log;

import com.example.uhf.activity.RFIDServiceActivity;
import com.example.uhf.service.RFIDService;

/**
 * Classe auxiliar para facilitar a integração com o serviço RFID
 * Fornece métodos estáticos para iniciar e parar o serviço
 */
public class RFIDServiceHelper {
    private static final String TAG = "RFIDServiceHelper";

    /**
     * Inicia o serviço RFID
     * @param context O contexto da aplicação
     * @return true se o serviço foi iniciado com sucesso
     */
    public static boolean iniciarServicoRFID(Context context) {
        try {
            Log.d(TAG, "Iniciando serviço RFID");
            
            // Usando a Activity transparente para iniciar o serviço
            Intent activityIntent = new Intent();
            activityIntent.setAction("com.rfid.START_SERVICE");
            activityIntent.setClassName("com.example.uhf", "com.example.uhf.activity.RFIDServiceActivity");
            activityIntent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            context.startActivity(activityIntent);
            
            return true;
        } catch (Exception e) {
            Log.e(TAG, "Erro ao iniciar serviço RFID: " + e.getMessage());
            return false;
        }
    }

    /**
     * Para o serviço RFID
     * @param context O contexto da aplicação
     * @return true se o serviço foi parado com sucesso
     */
    public static boolean pararServicoRFID(Context context) {
        try {
            Log.d(TAG, "Parando serviço RFID");
            Intent serviceIntent = new Intent();
            serviceIntent.setClassName("com.example.uhf", "com.example.uhf.service.RFIDService");
            serviceIntent.setAction(RFIDService.ACTION_STOP_SERVICE);
            
            context.startService(serviceIntent);
            return true;
        } catch (Exception e) {
            Log.e(TAG, "Erro ao parar serviço RFID: " + e.getMessage());
            return false;
        }
    }
}
