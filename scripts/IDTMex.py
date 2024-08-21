import numpy as np
import pandas as pd

def componente_acceso(hogares: pd.DataFrame)->float:
    """
    Funcion para calcular el valor de la componente 
    de acceso inspirado en el IDTMex propuesto por 
    Micheli Thirión, J., y Valle Zarate, J. (2018)
    ----------------------------------------------
    
    PARAMETROS:
    @hogares: DataFrame con informacion de vivienda, hogar y residente

    REGRESA:
    Flotante en el intervalo [0,1] que representa el valor de esta componente
    """

    #Eliminamos los duplicados
    hogares = hogares.drop_duplicates(subset=["UPM","VIV_SEL","HOGAR"])

    #Porcentaje de hogares con telefonia fija
    tel_fija = hogares[hogares.P5_5==1].FAC_HOG.sum()/hogares[~hogares.P5_5.isna()].FAC_HOG.sum()

    #Porcentaje de hogares con celular
    cel = hogares[hogares.P4_1_6==1].FAC_HOG.sum()/hogares[~hogares.P4_1_6.isna()].FAC_HOG.sum()

    #Porcentaje de hogares con computadora (escritorio, portatil, tablet)
    compu = hogares[(hogares.P4_2_1==1)|(hogares.P4_2_2==1)|(hogares.P4_2_3==1)].FAC_HOG.sum()/hogares[(~hogares.P4_2_1.isna())|(~hogares.P4_2_2.isna())|(~hogares.P4_2_1.isna())].FAC_HOG.sum()

    #Porcentaje de hogares con internet
    internet = hogares[hogares.P4_4==1].FAC_HOG.sum()/hogares[~hogares.P4_4.isna()].FAC_HOG.sum()

    return np.mean([tel_fija,cel,compu, internet])

def componente_uso(residentes: pd.DataFrame)->float:
    """
    Funcion para calcular el valor de la componente 
    de uso inspirado en el IDTMex propuesto por 
    Micheli Thirión, J., y Valle Zarate, J. (2018)
    ----------------------------------------------
    
    PARAMETROS:
    @residentes: DataFrame con informacion de vivienda, hogar y residente

    REGRESA:
    Flotante en el intervalo [0,1] que representa el valor de esta componente
    """

    #Eliminamos los duplicados
    hogares = residentes.drop_duplicates(subset=["UPM","VIV_SEL","HOGAR"])

    #Porcentaje de residentes usuarios de Internet
    internet = residentes[residentes.P3_9_2==1].FAC_HOGAR.sum()/residentes[~residentes.P3_9_2.isna()].FAC_HOGAR.sum()

    #Tipo de conexion a internet
    respondieron = hogares[hogares.P4_5.notna()]
    ## Conexion alambrica
    alambrica = hogares[((hogares.P4_5==1)|(hogares.P4_5==3))].FAC_HOG.sum()/respondieron.FAC_HOG.sum()
    ## Conexion inalambrica
    inalambrica = hogares[((hogares.P4_5==2)|(hogares.P4_5==3))].FAC_HOG.sum()/respondieron.FAC_HOG.sum()

    return np.mean([internet, alambrica, inalambrica])

def componente_aptitudes(residentes: pd.DataFrame)->float:
    """
    Funcion para calcular el valor de la componente 
    de aptitudes inspirado en el IDTMex propuesto por 
    Micheli Thirión, J., y Valle Zarate, J. (2018)
    ----------------------------------------------
    
    PARAMETROS:
    @residentes: DataFrame con informacion de residente

    REGRESA:
    Flotante en el intervalo [0,1] que representa el valor de esta componente
    """ 
    
    #Porcentaje de residentes adultos que cursaron al menos la primaria 
    alfabetas_proxy = residentes[(residentes.EDAD>=18)&(residentes.NIVEL>=2)].FAC_HOGAR.sum()/residentes[residentes.EDAD>=18].FAC_HOGAR.sum()

    #Porcentaje de residentes adultos que cursaron al menos el bachillerato 
    bachillerato = residentes[(residentes.EDAD>=18)&(residentes.NIVEL>=6)].FAC_HOGAR.sum()/residentes[residentes.EDAD>=18].FAC_HOGAR.sum()

    #Porcentaje de residentes mayores de 23 que fueron a la universidad
    universidad = residentes[(residentes.EDAD>=23)&(residentes.NIVEL>=8)].FAC_HOGAR.sum()/residentes[residentes.EDAD>=23].FAC_HOGAR.sum()

    return np.mean([alfabetas_proxy,bachillerato,universidad])


def IDTMex(hogares: pd.DataFrame)->float:
    """
    Funcion para calcular el IDTMex propuesto por 
    Micheli Thirión, J., y Valle Zarate, J. (2018)
    ----------------------------------------------
    
    PARAMETROS:
    @hogares: DataFrame con informacion de vivienda, hogar y residente

    REGRESA:
    Flotante en el intervalo [0,10] que representa el valor del indice
    """
    acceso = componente_acceso(hogares)
    uso = componente_uso(hogares)
    aptitudes = componente_aptitudes(hogares)

    return (0.4*acceso + 0.4*uso + 0.2*aptitudes)*10


def IDTMex_desglosado(hogares: pd.DataFrame)->float:
    """
    Funcion para calcular el IDTMex propuesto por 
    Micheli Thirión, J., y Valle Zarate, J. (2018)
    ----------------------------------------------
    
    PARAMETROS:
    @hogares: DataFrame con informacion de vivienda, hogar y residente

    REGRESA:
    Flotante en el intervalo [0,10] que representa el valor del indice
    """
    acceso = componente_acceso(hogares)
    uso = componente_uso(hogares)
    aptitudes = componente_aptitudes(hogares)
    idtmex = (0.4*acceso + 0.4*uso + 0.2*aptitudes)*10

    return {"acceso":acceso,"uso":uso,"aptitudes":aptitudes,"IDTMex":idtmex}
