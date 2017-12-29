##SCRIPTS R PARA ANALISES SOCIOLINGUISTICAS L. Oushiro

#SCRIPT 1: IDENTIFICACAO DE OCORRENCIAS - v. 0.1.4 (jan/2014)  
#O que faz: Realiza busca em um conjunto de transcricoes por um padrao e retorna arquivo(s) de texto com a marcacao das ocorrencias.

#Requer definicao da pasta padrao - setwd() - como aquela em que estao as transcricoes

identificacao<-function(padrao,
                        simbolo.marcacao="<>",
                        posicao.marcacao=c(T,F),
                        ignorar.linhas=NULL,
                        stoplist=NULL,
                        novos.arquivos="marcacoes"){
  
  #Carrega os nomes dos arquivos .txt do diretorio padrao em um vetor (files)
  files<-dir(getwd(), pattern=".txt", all.files=F) 
  
  #Mensagem de erro se o diretorio padrao nao tiver arquivos .txt.
  if (length(files)==0) {
    cat("Erro: Diretorio nao contem arquivos .txt.\nChecar diretorio padrao com a funcao getwd().\nSe nao for o diretorio correto, especificar com a funcao setwd().")
  }
  
  #Cria um vetor com os nomes dos novos arquivos, que terao as marcacoes das ocorrencias
  files2<-paste(novos.arquivos,"-",files)
  files2<-gsub(" ","",files2)
  
  #Cria uma lista vazia, que abarcara os vetores, cada um dos quais com um arquivo de texto
  all.corpus<-list() 
  
  #Carrega cada arquivo de texto em um vetor (de mesmo nome) dentro da lista all.corpus
  for (i in files) {
    all.corpus[[i]]<-scan(i, what="char", sep="\n", skip=0)
  } 
  
  for (k in 1:length(all.corpus)) {
    all.corpus[[k]]<-gsub("\t", " ", all.corpus[[k]])
  }

  #Cria vetor com especificacao do padrao definido na funcao, no formato adequado para grep abaixo 
  variavel1<-paste("(",padrao,")")
  variavel1<-gsub(" ","",variavel1)
  
  #Vetor com especificacao do stoplist definido na funcao, no formato adequado para funcoes abaixo
  stoplist2<-paste(stoplist,collapse="|")
  stoplist3<-paste("(",stoplist2,")")
  stoplist4<-gsub(" ","",stoplist3)
  
  #Vetor com especificacao do simbolo de marcacao de ocorrencias, definido na funcao
  simbolo<-paste(simbolo.marcacao)
  
  #Se posicao.marcacao==T, simbolo.marcacao e inserido depois do padrao; se posicao.marcacao==F, simbolo.marcacao e inserido antes do padrao
  if (posicao.marcacao==T) {
    stoplist5<-paste(stoplist4,simbolo)
  } else {
    stoplist5<-paste(simbolo,stoplist4)
  }
  
  #Vetor com especificacao das linhas a serem ignoradas pelo grep, se houver definicao de ignorar.linhas
  if (length(ignorar.linhas)!=0) {
    ignorar2<-paste(ignorar.linhas,collapse="|")
  }
  
  #########################################################
  #Loop para marcacao de ocorrencias em todos os arquivos
  for (i in 1:length(files)) {
    
    #NUMERACAO DAS LINHAS DOS ARQUIVOS
    #Identifica o numero de linhas em cada arquivo
    linhas.arquivo<-length(all.corpus[[i]])
    
    #Cria um vetor numerico de 1 ate numero maximo de linhas em cada arquivo (1, 2, ...)
    numero.linhas<-rep(1:linhas.arquivo)
    
    #Enumera as linhas dos arquivos de texto
    linhas.numeradas<-paste(numero.linhas,all.corpus[[i]])
    
    
    #Se houver ignorar.linhas...
    #LINHAS A SEREM IGNORADAS
    #Identifica linhas a serem ignoradas pelo grep
    if (length(ignorar.linhas)!=0) {
      naos<-paste(ignorar2)
      
      #cria um vetor com o numero das linhas a serem ignoradas pelo grep
      naos2<-grep(naos, all.corpus[[i]], ignore.case=T, perl=T, value=F) # se value=T, retorna linha da ocorrencia no corpus
      
      #Cria um vetor com as linhas (texto) a serem ignoradas pelo grep
      fala.doc<-grep(naos, linhas.numeradas, ignore.case=T, perl=T, value=T)
      
      subst.simbolo1<-paste("\\1",simbolo)
      subst.simbolo2<-paste(simbolo,"\\1")
      
      #MARCACAO DE OCORRENCIAS COM simbolo.marcacao ANTES OU DEPOIS DO PADRAO; APAGAMENTO DE CASOS ESPECIFICADOS NO STOPLIST
      if (posicao.marcacao==T){
        ocorrencias<-gsub(variavel1, subst.simbolo1, linhas.numeradas[-naos2], ignore.case=T)
        
        if (length(stoplist)!=0) {
          ocorrencias<-gsub(stoplist5, "\\1", ocorrencias, ignore.case=T)
        }
        
      } else {
        ocorrencias<-gsub(variavel1, subst.simbolo2, linhas.numeradas[-naos2], ignore.case=T)
        if (length(stoplist)!=0) {
          ocorrencias<-gsub(stoplist5, "\\1", ocorrencias, ignore.case=T)
        }      
      }
      
      #RECOMPOSICAO DO ARQUIVO DE TEXTO NA ORDEM CORRETA
      #Junta linhas ignoradas e linhas com ocorrencias novamente
      recomposicao<-c(fala.doc,ocorrencias)
      
      #Divide o vetor com arquivo recomposto em palavras, para separacao do numero das linhas
      recomposicao2<-strsplit(recomposicao," ")
      
      #Cria vetor vazio para abarcara o numero das linhas a serem reordenadas
      recomposicao3<-vector()
      
      #Preenche vetor com numero das linhas a serem reordenadas
      for (k in 1:length(recomposicao2)) {
        recomposicao3<-c(recomposicao3,as.numeric(recomposicao2[[k]][1]))
      }
      
      #Reorganiza vetor com arquivo recomposto na ordem original
      recomposicao4<-order(recomposicao3)
      recomposicao5<-recomposicao[recomposicao4]
      
      #Limpa vetor com arquivo recomposto de dois espacos
      palavras.limpo<-gsub(" +", " ", recomposicao5)            
      
      
      #Se nao houver ignorar.linhas...
    } else {
      subst.simbolo1<-paste("\\1",simbolo)
      subst.simbolo2<-paste(simbolo,"\\1")
      
      #MARCACAO DE OCORRENCIAS COM simbolo.marcacao ANTES OU DEPOIS DO PADRAO; APAGAMENTO DE CASOS ESPECIFICADOS NO STOPLIST
      if (posicao.marcacao==T){
        palavras.limpo<-gsub(variavel1, subst.simbolo1, linhas.numeradas, ignore.case=T)
        if (length(stoplist)!=0) {
          palavras.limpo<-gsub(stoplist5, "\\1", palavras.limpo, ignore.case=T)
        }
      } else {
        palavras.limpo<-gsub(variavel1, subst.simbolo2, linhas.numeradas, ignore.case=T)
        if (length(stoplist)!=0) {
          palavras.limpo<-gsub(stoplist5, "\\1", palavras.limpo, ignore.case=T)
        }
      }
      
    } # fim do if-else ignorar.linhas
    
    tokens0<-gsub("\\b\\d+ ","",palavras.limpo) # apaga numeros inseridos (mas tambem apaga outros numeros, se iniciam palavras...)
    
    #Salva vetores com marcacoes em arquivos .txt 
    cat(tokens0, file=files2[i], sep="\n")
    
  } # fim do Loop de marcacao de ocorrencias
  
} # fim da funcao

#############################

#FUNCAO identificacao()

#identificacao(padrao=,stoplist=NULL,posicao.marcacao=T,ignorar.linhas=NULL,novos.arquivos=)

#     Argumentos
#     padrao            definicao da sequencia de caracteres que identifica as variantes da variavel
#                       deve ser especificado entre aspas
#     simbolo.marcacao  definicao dos simbolos que serao usados para identificar a variavel no corpus.
#                       Valor default = "<>".
#     posicao.marcacao  Logico (T ou F). Se T (default), a marcacao e colocada apos o padrao. 
#                       Se F, a marcacao e colocada antes do padrao.
#                       Nota: Para posicao.marcao=F, a definicao da variavel preferivalmente deve comecar com \\b (levando em conta o inicio da palavra)
#     stoplist          vetor com palavras que devem ignoradas na busca do padrao
#     ignorar.linhas    vetor com identificacao dos participantes cujas falas devem ser ignoradas na busca do padrao.
#     novos.arquivos    Sequencia de caracteres a ser adicionada ao nome dos arquivos originais,
#                       para diferenciar os arquivos com marcacoes de ocorrencia na mesma pasta.
#                       Deve vir entre aspas. 


###############################################################################################################################################################################

#SCRIPTS R PARA ANALISES SOCIOLINGUISTICAS
#L. Oushiro

#SCRIPT 2: EXTRACAO DE OCORRENCIAS - v. 0.1.4 (jan/2014) 
#O que faz: Realiza busca em um conjunto de transcricoes com marcacoes de ocorrencia e retorna uma tabela com a ocorrencia, X palavras do contexto precedente, X palavras do contexto seguinte, e localizacao das ocorrencias (paragrafo do arquivo original). Opcionalmente, retorna codificacao da variavel dependente e codificacao dos grupos de fatores sociais. 

#Requer definicao da pasta padrao - setwd() - como aquela em que estao as transcricoes

#Se se deseja a extracao da codificacao da variavel dependente, deve-se te-la codificada e especificar onde se encontra a codificacao.
#Se se deseja a extracao das variaveis sociais, deve-se especifica-las em linhas separadas no cabecalho dos arquivos de transcricao.


##CONCORDANCIA PRA EXTRAIR DADOS

extracao<-function(padrao,
                   palavras.cont.precedente=5,
                   palavras.ocorrencia=1,
                   palavras.cont.seguinte=5,
                   stoplist=NULL,
                   ignorar.linhas=NULL,
                   var.dependente=NULL,
                   loc.variaveis.sociais=NULL,
                   nomes.colunas.variaveis=NULL,
                   file="DadosExtraidos.txt"
) {
  
  #Carrega os nomes dos arquivos no diretorio padrao em um vetor (files2)
  files2<-dir(getwd(), pattern=".txt", all.files=F) 
  
  #Cria uma lista vazia, que abarcara os vetores, cada um dos quais com um arquivo de transcricao
  all.tokens<-list() 
  
  #Loop para carregar os arquivos de transcricao em vetores, dentro da lista all.tokens
  for (i in files2) {
    all.tokens[[i]]<-scan(i, what="char", sep="\n", skip=0)
  }
  
  
  #PREPARACAO DO ARQUIVO FINAL
  #Nomes das colunas
  if (length(var.dependente)==0){
    Contextos<-c("Contexto.Precedente","Ocorrencia","Contexto.Seguinte")
  } else {
    Contextos<-c("Contexto.Precedente","Ocorrencia","Contexto.Seguinte","Variavel.Dependente")
  }
  
  #Cria vetor com nomes das colunas CP,Oco,CS (e Var.Dependente) e demais colunas especificadas em nomes.colunas.variaveis para nomeacao das colunas da tabela
  if (length(loc.variaveis.sociais)!=0&length(nomes.colunas.variaveis)!=0) {
    Cols<-c(Contextos,nomes.colunas.variaveis)
  } else if (length(loc.variaveis.sociais)!=0&length(nomes.colunas.variaveis)==0) { 
    ColsGFsociaisVAR<-sapply(paste("VAR",1:length(loc.variaveis.sociais)),c)
    Cols<-c(Contextos,ColsGFsociaisVAR)
  } else if (length(loc.variaveis.sociais)==0&length(nomes.colunas.variaveis)!=0) {
    cat("Erro: Especifique um vetor para loc.variaveis.sociais, ou especifique nomes.colunas.variaveis como NULL.")
  } else {
    ColsGFsociaisVAR<-paste("GFs sociais...")
    Cols<-c(Contextos,ColsGFsociaisVAR)
  }
  
  #Cria vetor vazio para GFsociais
  GFsociais<-vector()
  
  #Cria dataframe vazio com nomes das colunas
  tabela.dados<-data.frame(matrix(Cols,nrow=1,ncol=length(Cols)))
  
  #Salva dataframe em arquivo .txt, que abarcara os dados extraidos
  write.table(tabela.dados,file,append=T, quote=F, sep="\t", row.names=F, col.names=F)
  
  #Cria vetor que abarcara o numero das linhas das ocorrencias (para coluna "Localizacao")
  vetor.linhas<-vector()
  
  #Cria lista que abarcara vetores com tag
  all.tokens.tagged<-list()
  
  #IDENTIFICACAO OCORRENCIAS = SCRIPT 1
  #Vetor com especificacao do padrao definido na funcao, no formato adequado para grep abaixo 
  variavel1<-paste("(",padrao,")")
  variavel1<-gsub(" ","",variavel1)
  
  #Vetor com especificacao do simbolo de marcacao de ocorrencias
  tag<-paste("<<TAG>>")
  
  #Vetor com especificacao do stoplist definido na funcao, no formato adequado para funcoes abaixo
  stoplist2<-paste(stoplist,collapse="|")
  stoplist3<-paste("(",stoplist2,")")
  stoplist4<-gsub(" ","",stoplist3)
  
  stoplist5<-paste(stoplist4,tag)
  stoplist5<-gsub(" ","",stoplist5) 
  
  #Vetor com especificacao das linhas a serem ignoradas, se isso foi definido na funcao
  if (length(ignorar.linhas)!=0) {
    ignorar2<-paste(ignorar.linhas,collapse="|")
  }
  
  
  #####################################
  #CONCORDANCIA PRA EXTRAIR DADOS
  
  for (i in 1:length(all.tokens)) {
    
    #Cria string com valores das variaveis sociais, em diferentes cenarios...
    #Se loc.variaveis.sociais e nomes.colunas.variaveis forem especificados na funcao...
    if (length(loc.variaveis.sociais)!=0&length(nomes.colunas.variaveis)!=0){
      
      for (j in 1:length(loc.variaveis.sociais)){
        all.tokens[[i]][loc.variaveis.sociais[j]]->nomes.colunas.variaveis[j]
        GFsociais<-paste(nomes.colunas.variaveis,collapse="\t")
      } # fim do loop de criacao do vetor GFsociais
      
      #Se loc.variaveis.sociais foi definido, mas nomes.colunas.variaveis nao 
    } else if (length(loc.variaveis.sociais)!=0&length(nomes.colunas.variaveis)==0) {
      
      for (j in 1:length(loc.variaveis.sociais)){
        all.tokens[[i]][loc.variaveis.sociais[j]]->ColsGFsociaisVAR[j]
        GFsociais<-paste(ColsGFsociaisVAR,collapse="\t")
      }
      
    } # fim do if-else para criacao do string de valores das variaveis sociais
    
    #IDENTIFICACAO OCORRENCIAS = SCRIPT 1
    #Loop para marcacao de ocorrencias em todos os arquivos
    
    #NUMERAcaO DAS LINHAS DOS ARQUIVOS
    #Identifica o numero de linhas em cada arquivo
    linhas.arquivo<-length(all.tokens[[i]])
    
    #Cria um vetor numerico de 1 ate numero maximo de linhas em cada arquivo (1, 2, ...)
    numero.linhas<-rep(1:linhas.arquivo)
    
    #Enumera as linhas dos arquivos de texto
    linhas.numeradas<-paste(numero.linhas,all.tokens[[i]])
    
    
    #LINHAS A SEREM IGNORADAS
    #Identifica linhas a serem ignoradas pelo grep
    if (length(ignorar.linhas)!=0) { 
      
      #Cria vetor com especificacao das linhas a serem ignoradas, no formato adequado para funcoes abaixo
      naos<-paste(ignorar2)
      
      #cria um vetor com o numero das linhas a serem ignoradas pelo grep
      naos2<-grep(naos, all.tokens[[i]], ignore.case=T, value=F) # se value=T, retorna linha da ocorrencia no corpus
      
      #Cria um vetor com as linhas (texto) a serem ignoradas pelo grep, para recomposicao posterior do arquivo original
      fala.doc<-grep(naos, linhas.numeradas, ignore.case=T, value=T)
      
      #Cria vetor com o padrao definido na funcao e com o tag, no formato adequado para funcoes abaixo
      subst.simbolo1<-paste("\\1",tag)
      subst.simbolo1<-gsub(" ","",subst.simbolo1) 
      
      #MARCAcaO DE OCORRENCIAS COM TAG EM CENARIO COM STOPLIST E SEM STOPLIST 
      if (length(stoplist)!=0){
        ocorrencias<-gsub(variavel1, subst.simbolo1, linhas.numeradas[-naos2], ignore.case=T)
        ocorrencias<-gsub(stoplist5, "\\1", ocorrencias, ignore.case=T)
      } else {
        ocorrencias<-gsub(variavel1, subst.simbolo1, linhas.numeradas[-naos2], ignore.case=T)
      }
      
      #RECOMPOSICAO DO ARQUIVO DE TEXTO NA ORDEM CORRETA
      #Junta linhas ignoradas e linhas com ocorrencias novamente
      recomposicao<-c(fala.doc,ocorrencias)
      
      #Divide o vetor com arquivo recomposto em palavras, para separacao do numero das linhas
      recomposicao2<-strsplit(recomposicao," ")
      
      #Cria vetor vazio para abarcara o numero das linhas a serem reordenadas
      recomposicao3<-vector()
      
      #Preenche vetor com numero das linhas a serem reordenadas
      for (k in 1:length(recomposicao2)) {
        recomposicao3<-c(recomposicao3,as.numeric(recomposicao2[[k]][1]))
      }
      
      #Reorganiza vetor com arquivo recomposto na ordem original
      recomposicao4<-order(recomposicao3)
      recomposicao5<-recomposicao[recomposicao4]
      
      #Limpa vetor com arquivo recomposto de dois espacos
      all.tokens.tagged[[i]]<-gsub(" +", " ", recomposicao5)            
      #################################################################################
      
      #Identifica as ocorrencias (tag) no corpus completo
      matches<-grep(tag, all.tokens.tagged[[i]],perl=T, ignore.case=T)
      
      if (length(matches)==0) {
        stop
        cat("Nao ha ocorrencias do padrao especificado.")
      }
      
      tab.ocorr<-all.tokens.tagged[[i]][matches]  # vetor so com ocorrencias e respectivos tempos
      loc.ocorrencias<-vector()
      loc.ocorrencias<-gregexpr(tag,tab.ocorr)  #localizacao das ocorrencias
      
      n.ocorr.linha<-vector()
      for (n in 1:length(loc.ocorrencias)) {
        n.ocorr.linha[n]<-sapply(length(loc.ocorrencias[[n]]),c)   
      }
      n.ocorr.linha                                 #vetor com numero de ocorrencias por linha
      
      for (m in 1:length(matches)) {
        aa<-rep(matches[m],n.ocorr.linha[m])
        vetor.linhas<-append(vetor.linhas,as.vector(aa))
      }
      vetor.linhas                                  #vetor com locators
      
      
      #Apaga os numeros inseridos para numeracao dos paragrafos
      tokens0<-gsub("\\b\\d+ "," ",all.tokens.tagged[[i]]) # apaga numeros inseridos (mas tambem apaga outros numeros, se iniciam palavras...)
      #Quebra as linhas dos vetores em palavras
      tokens1<-gsub("(\\W)","\\1", tokens0)
      tokens2<-unlist(strsplit(tokens1," +"))
      #Busca as ocorrencias do padrao especificado (agora marcados com tag) 
      (matches.in.socio<-grep(tag,tokens2, perl=T, ignore.case=T))
   
      
      #OUTPUT: salva as ocorrencias com contexto precedente, contexto seguinte, (variavel dependente) e colunas de variaveis sociais (opcionais)
      for(i in matches.in.socio) {
        
        #Se localizacao de var.dependente nao foi especificada na funcao...
        if (length(var.dependente)==0){
          cat(
            tokens2[max(0,i-palavras.cont.precedente+1-palavras.ocorrencia-1):max(0,i-palavras.ocorrencia)],          #contexto precedente
            "\t", tokens2[max(0,i-palavras.ocorrencia+1):max(0,i+0)],          #token
            "\t", tokens2[(i+1):min(i+palavras.cont.seguinte,length(tokens2)+1)],  #contexto seguinte
            "\t", GFsociais,
            file=file, append=T, "\n")
          
          #Se localizacao da var.dependente foi especificada na funcao...
        } else {
          
          cat(
            tokens2[max(0,i-palavras.cont.precedente+1-palavras.ocorrencia-1):max(0,i-palavras.ocorrencia)],          #contexto precedente
            "\t", tokens2[max(0,i-palavras.ocorrencia+1):max(0,i+0)],          #token
            "\t", tokens2[(i+1):min(i+palavras.cont.seguinte,length(tokens2)+1)],  #contexto seguinte
            "\t", substr(tokens2[i], var.dependente,var.dependente),
            "\t", GFsociais,
            file=file, append=T, "\n")
        }  # fim if var.dependente       
        #####
        
      } # fim do loop de matches.in.socio ##
    } else { # fim do if ignorar.linhas!=0//inicio ignorar.linhas==0
      
      
      subst.simbolo1<-paste("\\1",tag)
      subst.simbolo1<-gsub(" ","",subst.simbolo1)
      
      #MARCAcaO DE OCORRENCIAS COM TAG 
      if (length(stoplist)!=0){
        ocorrencias<-gsub(variavel1, subst.simbolo1, linhas.numeradas, ignore.case=T)
        ocorrencias<-gsub(stoplist5, "\\1", ocorrencias, ignore.case=T)
      } else {
        ocorrencias<-gsub(variavel1, subst.simbolo1, linhas.numeradas, ignore.case=T)
      }
      
      
      #Limpa vetor com arquivo recomposto de dois espacos
      all.tokens.tagged[[i]]<-gsub(" +", " ", ocorrencias)            
      #################################################################################
      
      #Identifica as ocorrencias (tag) no corpus completo
      matches<-grep(tag, all.tokens.tagged[[i]],perl=T,ignore.case=T)
      
      if (length(matches)==0) {
        stop
        cat("Nao ha ocorrencias do padrao especificado.")
      }
      
      tab.ocorr<-all.tokens.tagged[[i]][matches]  # vetor so com ocorrencias e respectivos tempos
      loc.ocorrencias<-vector()
      loc.ocorrencias<-gregexpr(tag,tab.ocorr)  #localizacao das ocorrencias
      
      n.ocorr.linha<-vector()
      for (n in 1:length(loc.ocorrencias)) {
        n.ocorr.linha[n]<-sapply(length(loc.ocorrencias[[n]]),c)   
      }
      n.ocorr.linha                                 #vetor com numero de ocorrencias por linha
      
      for (m in 1:length(matches)) {
        aa<-rep(matches[m],n.ocorr.linha[m])
        vetor.linhas<-append(vetor.linhas,as.vector(aa))
      }
      vetor.linhas                                  #vetor com locators
      
      
      #Apaga numeros inseridos para numeracao das linhas
      tokens0<-gsub("\\b\\d+ "," ",all.tokens.tagged[[i]]) # apaga numeros inseridos (mas tambem apaga outros numeros, se iniciam palavras...)
      #Quebra as linhas dos vetores em palavras
      tokens1<-gsub("(\\W)","\\1", tokens0)
      tokens2<-unlist(strsplit(tokens1," +"))
      #Busca as ocorrencias do padrao especificado (agora marcados com tag)
      (matches.in.socio<-grep(tag,tokens2, perl=T,ignore.case=T))
      
      #OUTPUT: salva as ocorrencias com contexto precedente, contexto seguinte, (var.dependente) e colunas de variaveis sociais (opcionais)
      for(i in matches.in.socio) {
        
        #Se localizacao da var.dependente nao foi especificada...
        if (length(var.dependente)==0){
          cat(
            tokens2[max(0,i-palavras.cont.precedente+1-palavras.ocorrencia-1):max(0,i-palavras.ocorrencia)],          #contexto precedente
            "\t", tokens2[max(0,i-palavras.ocorrencia+1):max(0,i+0)],          #token
            "\t", tokens2[(i+1):min(i+palavras.cont.seguinte,length(tokens2)+1)],  #contexto seguinte
            "\t", GFsociais,
            file=file, append=T, "\n")
          
          #Se localizacao da var.dependente foi especificada...
        } else {
          
          cat(
            tokens2[max(0,i-palavras.cont.precedente+1-palavras.ocorrencia-1):max(0,i-palavras.ocorrencia)],          #contexto precedente
            "\t", tokens2[max(0,i-palavras.ocorrencia+1):max(0,i+0)],          #token
            "\t", tokens2[(i+1):min(i+palavras.cont.seguinte,length(tokens2)+1)],  #contexto seguinte
            "\t", substr(tokens2[i], var.dependente,var.dependente),
            "\t", GFsociais,
            file=file, append=T, "\n")
        }  # fim if var.dependente       
        #####
        
      } # fim do loop de matches.in.socio ##
      
      
    } # fim if-else ignorar.linhas==0
  } # fim do loop de concordancia
  
  #Carrega o arquivo de dados extraidos em um dataframe
  complete<-read.table(file=file,header=F,sep="\t",quote="", comment.char="")
  
  #prepara vetor.linhas (que sera nova coluna na tabela de dados extraidos)
  vetor.linhas2<-paste("Paragrafo:",vetor.linhas)
  vetor.linhas3<-append("Localizacao",vetor.linhas2)
  
  #Anexa coluna com o numero das linhas no arquivo completo de dados extraidos
  complete["Localizacao"]<-vetor.linhas3
  
  #Apaga tags das colunas cont.prec, Oco, cont.seg
  complete[,1]<-gsub(tag,"",complete[,1])
  complete[,1]<-gsub(" +"," ",complete[,1])
  complete[,2]<-gsub(tag,"",complete[,2])
  complete[,2]<-gsub(" +"," ",complete[,2])
  complete[,3]<-gsub(tag,"",complete[,3])
  complete[,3]<-gsub(" +"," ",complete[,3])
  
  #Salva o novo arquivo de dados extraidos com a localizacao das ocorrencias
  write.table(complete,file=file,append=F, quote=F, sep="\t", row.names=F, col.names=F) #!!!
  #cat(vetor.linhas3, file="vetor-linhas.txt",append=T,sep="\n")
  
} # fim da funcao

# padrao                    Obrigatorio. Definicao da sequencia de caracteres 
#                           a ser buscada.
#                           Nota: Deve ser especificado entre aspas.
# palavras.cont.precedente  Opcional. Numerico. Numero de palavras do contexto 
#                           precedente a serem extraidas.
#                           Default=5.
# palavras.ocorrencia       Opcional. Numerico. Numero de palavras a serem extraidas
#                           para a coluna de ocorrencia.
#                           Default=1.
# palavras.cont.seguinte    Opcional. Numerico. Numero de palavras do contexto
#                           seguinte a serem extraidas.
#                           Default=5.
# stoplist                  Opcional. Vetor com palavras que devem ignoradas na busca do padrao. 
#                           Default=NULL.          
# ignorar.linhas            Opcional. Vetor com identificacao dos participantes 
#                           cujas falas devem ser ignoradas na busca do padrao.
#                           Default=NULL.          
# var.dependente            Opcional. Numerico. Localizacao do caracter de codificacao
#                           dentro do termo que contem o padrao buscado.
#                           Default=NULL.
# loc.variaveis.sociais     Opcional. Vetor com o numero das linhas em que se encontram
#                           as informacoes das variaveis sociais a serem extraidas.
#                           Default=NULL.
# nomes.colunas.variaveis   Opcional. Vetor com o nome das variaveis sociais a serem extraidas,
#                           na mesma sequencia do vetor com sua localizacao.  
#                           Default=NULL.
# file                      Opcional. Nome do arquivo com dados extraidos.
#                           Default="DadosExtraidos.txt".
#                           Nota: Deve conter a extensao .txt e ser especificado
#                           entre aspas.

###############################################################################################################################################################################
#SCRIPTS R PARA ANALISES SOCIOLINGUISTICAS
#L. Oushiro

#SCRIPT 3: AMOSTRAGEM ALEATORIA - v. 0.1.4 (jan/2014) 
#O que faz: Pega de arquivo de codificacao e retorna uma nova tabela com uma amostra do arquivo completo, de acordo com numero de dados e variavel (coluna de referencia) especificados.

amostragem<-function(numero.dados=50,
                     coluna.referencia,
                     data,
                     novo.arquivo="DadosAmostrados.txt"){
  
  
  
  if (library(NCStats,logical.return=T)==F) {
    source("http://www.rforge.net/NCStats/InstallNCStats.R")
  } 
  library(NCStats)
  
  #Mostra a estrutura do arquivo de dados
  str(data)
  
  #Identifica valores unicos na coluna de referencia
  coluna<-unique(data[,coluna.referencia])
  coluna2<-paste(coluna[1:length(coluna)])
  
  #Cria dataframe que gerara a tabela de dados.
  tabela.dados<-data.frame(matrix(colnames(data),nrow=1,ncol=length(colnames(data))))
  #Salva arquivo .txt que abarcara os dados
  write.table(tabela.dados,file=novo.arquivo,append=T, quote=F, sep="\t", row.names=F, col.names=F)
  
  #Loop para selecionar aleatoriamente X dados de cada valor unico da coluna de referencia
  for (i in coluna2) {
    tabela.dados<-write.table(srsdf(subset(data, data[,coluna.referencia]==i),numero.dados), file=novo.arquivo, append=T, quote=F, sep="\t", row.names=F, col.names=F)
  } # fim do Loop
  
}  # fim da funcao

#############################

#FUNCAO amostragem()

#amostragem(numero.dados=50,coluna.referencia=,data=,novo.arquivo=)

# numero.dados      Opcional. Numerico. Numero de dados a serem selecionados 
#                   aleatoriamente da coluna de referencia.
#                   Default = 50.
# coluna.referencia Obrigatorio. Numerico. Coluna que deve ser usada como referencia 
#                   para amostragem.
#                   A = 1, B = 2, C = 3 etc.
# data              Obrigatorio. Dataframe com planilha de dados a serem amostrados.
# novo.arquivo      Opcional. Nome do novo arquivo com os dados amostrados.
#                   Default="DadosAmostrados.txt"
#                       





