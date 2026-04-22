Impostazione per il running del codice.

Il codice corre automaticamente.


Per far girare il codice è necessario SEMPRE:
1) trasportare tutta la cartella main "reportino3" con dentro "input_data" e "script.R"

2) trascrivere il percorso del main nello script in riga 42, ad esempio: wd_main = "C:/Users/Windows10/Desktop/work/reportino3/"

3) per ottenere i file di output (tabelle, figure, dati) impostare il parametro "doouts" (riga 71) ad 1


File di input

i file di input sono contenuti nella cartella "input_data",dove sono presenti i seguenti file:
- cartella "prediction_ingr_apr25" che contiene i dataset delle predizioni prima e dopo il frame filtering delle variabili di outcome per ogni algoritmo e per ogni modello prediction_recipe_apr25 nelle rispettive sottocartelle ("EnergyPredicted_IngredientsPlus5TaskTrain" e "EnergyPredicted_FrameFiltering_IngredientsPlus5TaskTrain"), e il file xlsx "matrix_frames_kept_full_dataset" che contiene le informazioni dei frame disponibili da Nutrition5k con lo status di "mantenuto o scartato" del frame filtering
- cartella "reportino2_rerun_3alg" che contiene i risultati del rerun di Bianco et al. 2025 in relazione a tre algoritmi e due dataset
- file xlsx "dishes_merged_csv" dei valori nutrizionali osservati delle ricette di Nutrition5k (proveniente da Bianco et al. 2024)


File di output

gli output sono organizzati i 3 cartelle: le due cartelle "Energy*", speculari alle sottocartelle di input e contengono gli output per scenario di frame filtering, e la cartella "sensitivity analysis" che invece ha informazioni sul confronto dei due scenari. 

Le prime due cartelle contngono ulteriori cartelle:
"data" (contentente i dataset di output/di interesse), "figures", "tables". 
- data (file xlsx o csv) dove sono generati i file:
-- algorithm_median_performances: misure di performance algoritmi (anche valori mediani per dataset, algoritmo)  (nello script ####)
-- bland-altman_dishes_complete: informazioni sui plot di Bland-altman  (nello script ####)
-- df_fin_reportino3: valori nutrizionali + ingredienti predetti/osservati per dataset, algoritmo, run (nello script ####)
-- df_fin_reportino3_quantiles: df_fin_reportino3 con quartili dei nutritional values
-- dishes_mispredicted_reportino3: info sui piatti mispredetti. (fogli: dati completi tutti i piatti, dati completi solo mispredetti, lista unica piatti mispredettisecondario)
	dishes_quantile_complete: risultati dagli outliers (95% top percentile dei residui assoluti) (secondario, proveniente da file temporanei posti in tables)
	dishes_single_performances: performance del singolo piatto 
	algorithms_median_performances: performance aggregate per target variable, dataset e algoritmo
	ingr_metric_performances: performance degli ingredienti (secodnario)
	ingr_dish_performances: singole performance degli ingredienti
 

tabelle: tabella1: dati osservati
	 tabelle2: performances
	 tabelleS: tabelle dei predetti, degli accordi, del Wilcoxon
	 ...file excel identici, utili per visionare e filtrare i dati
	 list_*_*: liste dei piatti malpredetti in almeno un contesto (secondario, per confronti)

figure: supfig2_BA_*: plot di Bland-Altman

in sensitivity:
plot_*_gt-scenarios: differenza fra valori predetti e osservati dei macronutrienti

 
Sezioni:
0 - creazione del dato: crea "df_fin_reportino3" che è il dato della singola analisi contenente la composizione nutrizionale, calcolo dei quartili e quintili. Tabelle di confronto fra dati in analisi e dati dalla pubblicazione precedente (Reportino1, doi: 10.3390/nu16193339).

1 - performance degli algoritmi: calcolo delle performance e confronti multipli

2 - confronto fra valori predetti e osservati, tabelle e grafici di Bland-Altman:
flag = 0 i grafici NON individuano i piatti della lista "warned_dish_list"; = 1 i grafici li individuano

3 - lista piatti malpredetti
occorre che doouts = 1 per ottenere le liste
crea le liste dei piatti malpredetti, e le liste dei piatti malpredetti o meno nella combinazione nutriente*dataset*algoritmo.

4 - analisi di sensitività: confronto del calcolo dell'energia nei 2 scenari; calcolo delle differenze fra gli scenari after vs before frame filtering e reportino 3 vs reportino 2 before frame filtering


