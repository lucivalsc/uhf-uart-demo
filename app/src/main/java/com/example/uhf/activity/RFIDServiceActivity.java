package com.example.uhf.activity;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.view.Window;
import android.view.WindowManager;
import android.os.Handler;
import android.widget.Toast;

import com.example.uhf.service.RFIDService;

/**
 * Atividade totalmente transparente para iniciar o serviço RFID sem exibir nenhuma interface visual.
 * Serve como ponto de entrada para o aplicativo e para intents de ativação do serviço.
 */
public class RFIDServiceActivity extends Activity {
    private static final String TAG = "RFIDServiceActivity";

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        
        // Configura a janela como totalmente transparente e sem decoração
        requestWindowFeature(Window.FEATURE_NO_TITLE);
        getWindow().setFlags(
                WindowManager.LayoutParams.FLAG_TRANSLUCENT_STATUS |
                WindowManager.LayoutParams.FLAG_TRANSLUCENT_NAVIGATION |
                WindowManager.LayoutParams.FLAG_NOT_TOUCHABLE |
                WindowManager.LayoutParams.FLAG_NOT_FOCUSABLE,
                WindowManager.LayoutParams.FLAG_TRANSLUCENT_STATUS |
                WindowManager.LayoutParams.FLAG_TRANSLUCENT_NAVIGATION |
                WindowManager.LayoutParams.FLAG_NOT_TOUCHABLE |
                WindowManager.LayoutParams.FLAG_NOT_FOCUSABLE);
        
        // Define o tema transparente
        setTheme(android.R.style.Theme_Translucent_NoTitleBar);
        
        Log.d(TAG, "Atividade de inicialização do serviço RFID iniciada");
        
        // Verifica a intent que iniciou a atividade
        Intent startIntent = getIntent();
        String action = startIntent != null ? startIntent.getAction() : null;
        
        // Inicia o serviço RFID
        iniciarServicoRFID();
        
        // Finaliza imediatamente a atividade para que nenhuma interface seja exibida
        new Handler().postDelayed(new Runnable() {
            @Override
            public void run() {
                finish();
                // Remove a animação de transição
                overridePendingTransition(0, 0);
            }
        }, 100); // Pequeno delay para garantir que o serviço seja iniciado
    }
    
    /**
     * Inicia o serviço RFID em foreground
     */
    private void iniciarServicoRFID() {
        try {
            Intent serviceIntent = new Intent(this, RFIDService.class);
            serviceIntent.setAction(RFIDService.ACTION_START_SERVICE);
            
            // Em Android 8.0 (API 26) ou superior, é necessário usar startForegroundService
            if (android.os.Build.VERSION.SDK_INT >= android.os.Build.VERSION_CODES.O) {
                startForegroundService(serviceIntent);
            } else {
                startService(serviceIntent);
            }
            
            // Exibe uma mensagem rápida para o usuário
            Toast.makeText(this, "Serviço de leitura RFID iniciado", Toast.LENGTH_SHORT).show();
            
            Log.d(TAG, "Comando para iniciar serviço RFID enviado com sucesso");
        } catch (Exception e) {
            Log.e(TAG, "Erro ao iniciar serviço RFID: " + e.getMessage());
            Toast.makeText(this, "Erro ao iniciar o serviço RFID", Toast.LENGTH_LONG).show();
        }
    }
}
