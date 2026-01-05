library(FactoMineR)
library(factoextra)
library(missMDA)

df_networks_unique <- df_networks %>%
  arrange(network, desc(nb_facilities)) %>%  # trier par réseau puis nb_facilities décroissant
  distinct(network, .keep_all = TRUE)        # ne garder qu’une ligne par réseau

df_af <- df_networks %>%
  select(scale_grp, continent, nb_facilities, nb_transfers, transfer_type) %>% # sélection des colonnes d'intérêt
  mutate(scale_grp = as.factor(scale_grp),
         continent = as.factor(continent),
         transfer_type = as.factor(transfer_type))

summary(df_af)

# imputation valeurs manquantes
nb_dim <- estim_ncpFAMD(df_af, ncp.max = 2) # estimation nb optimal dimensions pour l'imputation
df_af_imp <- imputeFAMD(df_af, ncp = nb_dim$ncp)$completeObs

res_famd <- FAMD(df_af_imp, graph=FALSE)
#summary(res_famd)

fviz_famd_var(res_famd,
              repel = TRUE) 

# graphe avec quali+quanti+modalités
fviz_famd_var(res_famd, repel = TRUE)

plot(res_famd,
     choix = "ind",        # individus
     habillage = "scale_grp", # ou autre variable quali
     invisible = NULL)     # rien de masqué

plot(res_famd,
     choix = "var",        # variables + modalités
     invisible = NULL)

