@echo off
echo Iniciando limpeza do projeto RFID Service...
echo Removendo componentes de UI desnecessários...

rem Removendo activities
del /Q "app\src\main\java\com\example\uhf\activity\BaseTabFragmentActivity.java"
del /Q "app\src\main\java\com\example\uhf\activity\TestActivity.java"
del /Q "app\src\main\java\com\example\uhf\activity\UHFMainActivity.java"

rem Removendo adapters
del /Q "app\src\main\java\com\example\uhf\adapter\ViewPagerAdapter.java"

rem Removendo file browser
del /Q "app\src\main\java\com\example\uhf\filebrowser\FileManagerActivity.java"
del /Q "app\src\main\java\com\example\uhf\filebrowser\IResultData.java"
del /Q "app\src\main\java\com\example\uhf\filebrowser\IconifiedText.java"
del /Q "app\src\main\java\com\example\uhf\filebrowser\IconifiedTextListAdapter.java"
del /Q "app\src\main\java\com\example\uhf\filebrowser\IconifiedTextView.java"

rem Removendo fragments
del /Q "app\src\main\java\com\example\uhf\fragment\BlockPermalockFragment.java"
del /Q "app\src\main\java\com\example\uhf\fragment\BlockWriteFragment.java"
del /Q "app\src\main\java\com\example\uhf\fragment\KeyDwonFragment.java"
del /Q "app\src\main\java\com\example\uhf\fragment\UHFKillFragment.java"
del /Q "app\src\main\java\com\example\uhf\fragment\UHFLightFragment.java"
del /Q "app\src\main\java\com\example\uhf\fragment\UHFLocationFragment.java"
del /Q "app\src\main\java\com\example\uhf\fragment\UHFLockFragment.java"
del /Q "app\src\main\java\com\example\uhf\fragment\UHFRadarLocationFragment.java"
del /Q "app\src\main\java\com\example\uhf\fragment\UHFReadTagFragment.java"
del /Q "app\src\main\java\com\example\uhf\fragment\UHFReadWriteFragment.java"
del /Q "app\src\main\java\com\example\uhf\fragment\UHFSetFragment.java"
del /Q "app\src\main\java\com\example\uhf\fragment\UHFUpgradeFragment.java"

rem Removendo views personalizadas
del /Q "app\src\main\java\com\example\uhf\view\CircleSeekBar.java"
del /Q "app\src\main\java\com\example\uhf\view\RadarBackgroundView.java"
del /Q "app\src\main\java\com\example\uhf\view\RadarPanelView.java"
del /Q "app\src\main\java\com\example\uhf\view\RadarView.java"
del /Q "app\src\main\java\com\example\uhf\view\UhfLocationCanvasView.java"

rem Removendo widgets personalizados
del /Q "app\src\main\java\com\example\uhf\widget\LazyViewPager.java"
del /Q "app\src\main\java\com\example\uhf\widget\NoScrollViewPager.java"

rem Removendo layouts e recursos visuais
rmdir /S /Q "app\src\main\res\layout"
rmdir /S /Q "app\src\main\res\menu"

echo Limpeza concluída! O projeto agora está configurado apenas como serviço em segundo plano.
pause
