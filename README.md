# State of Data 2021
Carreira em Ciência de Dados: o que pesa mais, Idade ou Instrução?
#
**Índice**

1. Introdução: A Pergunta
2. Primeiro Tratamento dos Dados
3. Analisando as Correlações
4. Reinterpretando os Dados
5. Criando o Modelo Logístico
6. Análise Gráfica
7. Considerações Finais
8. Referências
#
**1. Introdução: A Pergunta**

Para começar qualquer análise sobre um conjunto de dados (dataset) é importante antes elaborar uma pergunta ou perguntas sobre o que se quer enxergar. Normalmente, esta pergunta surge depois de um olhar atencioso, ainda que por vezes superficial sobre os dados em si.

Ao colocar apenas os pés neste lago riquíssimo de dados que originou a pesquisa State of Data 2021, já é possível identificar pelo menos 3 pilares que chamam a atenção: idade, instrução e salário.

A pergunta que surge já neste ponto é: como estes dados se correlacionam? A pertinência dessa questão se dá quando se olha para o Mercado de Trabalho como um todo, independente de ser ou não na área de Dados. Segundo a OMS (Organização Mundial da Saúde) o período pré-idosos se inicia à partir dos 55 anos¹, e não faltam artigos na internet discutindo a dificuldade de inserir pessoas mais velhas dentro do Mercado². Por outro lado, curiosamente, no universo masculino a faixa etária predominante de lideranças no Mercado entre 1995 a 2015 ficou entre 40 a 49 anos, sendo que em 2015 o pessoal dos 50 a 64 anos superou os "quarentões"³. Não parece, no entanto, que muita coisa mudou de lá pra cá.

Olhando dessa forma, parece que se você não é idoso (acima de 55) mas também não tem menos que 40 anos, as chances de você ter melhores cargos e, consequentemente, melhores salários, são maiores. Mas voltando a pergunta anterior sobre o nosso conjunto de dados, será que isso se repete também na área de Ciência de Dados? E sendo essa uma área muito exigente no que diz respeito ao conhecimento ou instrução, qual fator manda mais?

**Conjunto de Dados**

Sobre a dataset (State of Data 2021), é importante dizer que ele é muito rico e possui material para várias análises, desde satisfação dos trabalhadores nessa área até perspectivas de salários por gênero. Resumidamente, a divisão do questionário se deu em 9 partes:

- Dados demográficos
- Dados sobre carreira
- Desafio dos gestores de times de dados
- Conhecimentos na área de dados
- Objetivos na área de dados
- Conhecimentos em Engenharia de Dados
- Conhecimentos em Análise de Dados
- Conhecimentos em Ciência de Dados
- Sobre a comunidade Data Hackers 

No entanto, para a pergunta definida acima basta que nos concentremos em algumas variáveis.
#
**2. Primeiro Tratamento dos Dados**

Para as análises que se verá abaixo foi utilizado a linguagem R e alguns pacotes correspondentes. Sendo assim, o primeiro passo é o carregamento de pendências, ou seja, das bibliotecas.
#
install.packages("pacman")
library(pacman)

pacman::p_load(GGally, dplyr, corrplot, car, mlogit, nnet,
               AER, lmtest, gtsummary, reshape2, 
			   ggplot2)
        
df = read.csv("../input/state-of-data-2021/State of Data 2021 - Dataset - Pgina1.csv",
              header = TRUE)
 #
 Dentre as variáveis que interessam, estão:

- Idade
- Região
- Região de Origem
- Mudou de Estado
- Nível de Ensino
- Gestor
- Faixa Salarial
- Experiência na Área de Dados (Xp_Area de Dados)
- Experiência em Engenharia de Software (Xp_Engenharia de Software)

Sendo que, a segunda, terceira e quarta, dizem respeito a geolocalização do entrevistado. Esse dado é importante, pois permite identificar o quanto a Região de origem foi impactante para a carreira do profissional; sendo assim, utilizou-se o seguindo raciocínio lógico: se mudou de Estado, considera região de origem, se não, considera a variável região.

Outras variáveis que valem a pena explicar a importância são: Experiência ou XP na área de dados e em engenharia de software, isso porque considera-se experiência profissional também como uma forma de instrução ou formação "informal", ou seja, não foi adquirida em um curso de pós-graduação por exemplo.

Por fim, em resumo, todas essas variáveis se traduzem nos 3 pilares ditos anteriormente: idade, instrução e salário.

Depois, visando construir gráficos de correlação é fundamental que se transforme algumas variáveis categóricas (que são constituidas por frases ou afirmações do tipo não númerica) em variáveis numéricas.
#
**3. Analisando as Correlações**

Quanto uma variável impacta na outra? Para responder essa pergunta é necessário criar uma correlação. Nessa análise em específico pode-se obervar esse dado em dois gráficos diferentes, porém, complementares: de pares (pairs) e de correlação (corr).

![Gráfico de Pares](https://i.postimg.cc/0yjDH9mw/PAIRS.png)

Neste primeiro, um pouco mais complexo, pode-se ver 3 pontos de vista: na diagonal principal, onde a correlação é entre uma variável com ela mesma e isso obviamente resulta em 100%, temos no entanto gráficos de distribuição de frequências; na triângulação inferior dessa matriz vê-se gráficos de pontos ou dispersão, que são importantes para enxergar possíveis outliers (dados, nesse caso pontos, que se diferenciam drasticamente de todos os outros); na triângulação superior vê-se o que de fato interessa mais nesse caso, as correlações. No entanto, outro gráfico mais específico pode ser mais intuitivo.

![Gráfico de Correlação](https://i.postimg.cc/Px5j1V0T/CORR.png)

Esse gráfico próprio de correlações traz o percentual e uma cor indicando o quanto ele é mais próximo de 1 (100%) ou de 0. O que se observa logo de cara é que esse conjunto de dados não possui colinearidade, ou seja, as variáveis não são tão correlacionadas, ou ainda, possuem uma correlação menor que 0,9. Outro ponto interessante é que a maior correlação se dá entre salário e experiência (XP) na área de dados (ignora-se a correlação entre o cargo de gestor com o salário, pois trata-se quase de uma obviedade), isso significa que a experiência anterior do profissional na área de dados, muito mais que com engenharia de software, é fundamental nessa amostra quando o assunto é salário.

A correlação descarta ainda para a nossa pergunta principal definida lá no começo, os impactos das variáveis: Gestor, Experiência em Engenharia de Software e Região. Mas, não traz grandes respostas, logo um modelo preditivo torna-se conveniente.
#
**4. Reinterpretando os Dados**

O modelo escolhido para essa análise foi a Regressão Logística Multinominal. Isso porque a predição que se pretende realizar não tem como intuito "prever o futuro" necessariamente, por mais contraditório que isso pareça, mas deduzir com mais certeza os impactos dos pilares idade e instrução no pilar salário. Porém, antes da construção propriamente dita do modelo, é importante reinterpretamos alguns dados e converte-los, no que tange algumas variáveis, novamente em categóricas, pois a regressão logística tem como primeiro pressuposto trabalhar com variável dependente e nominal.

Nesse caso, escolheu-se a Faixa Salarial como variável principal e dependente e dividiu-a em 3 categorias, por isso a metodologia será do tipo multinominal.

Esse ponto foi um dos mais difíceis da análise, pois envolve fatores teóricos mais que meramente matemáticos: para a divisão dos resultados da variável Faixa Salarial em 3 categorias utilizou-se como pressuposto a maior concentração dos salários, neste caso algo entre 6 a 12 mil reais, a isso denominou-se "Média" ou seja, uma situação média de salários na área possibilitando, portanto, o surgimento de uma situação "Abaixo" e "Acima" dessa média.

Note que essa média não é matemática! E sim teórica. Esse recurso foi necessário para uma melhor distribuição das frequências, caso contrário, o modelo preditivo não funcionaria muito bem.
#
**5. Criando o Modelo Logístico**

Para a criação de um modelo de Regressão Logística Muntinominal é preciso antes validar alguns pressupostos. O primeiro deles já foi citado e validado anteriormente; o segundo sugere que as variáveis dependentes sejam mutuamente exclusivas, ou seja, o que está em uma não está na outra, e isso também corresponde a realidade do conjunto de dados que será usado; a terceira pede que as observações, ou seja, as linhas da tabela sejam independentes, isso quer dizer que cada linha deve representar um entrevistado, check!; a quarta é a ausência de multicolinearidade e isso já foi validado lá atrás no gráfico de correlação; por fim, o último e mais polêmico pressuposto, a independência de alternativas irrelevantes. Existe uma forma de validar isso como, por exemplo o teste de Hausman-McFadden, mas devido a discordâncias acadêmicas quanto a esse procedimento⁴ e a dificuldade de se aplicá-lo, esse ponto será deixado de lado e entende-se o pressuposto como validado.

A seguir um passo-a-passo do prosseguimento seguido:

- Verificação dos "levels", ou seja, a eleição de qual categoria será referência, nesse caso a categoria "Acima" da variável Faixa Salarial, pois o obetivo é comparar os resultados com os maiores salários e identificar o que difere;
- Contrução do modelo preditivo e de um modelo nulo para identificar a relevância do primeiro modelo, para isso é necessário que o valor de P entre os dois seja menor que 0,5 (utilizando a técnica ANOVA), e convenientemente nesse caso o valor foi igual a zero;
- Observação dos efeitos globais e específicos do modelo;
- Construção de uma tabela que sinaliza os pontos acima e mais alguns outros.

![Tabela de Análise](https://i.postimg.cc/P5BGCrQb/TABELA.png)

Existem várias interpretações estatísticas possíveis de se fazer baseado nessa tabela. Ela possui na ordem: razões de chance, intervalo de confiança e valor de P. O que chama atenção é o valor de P que praticamente diz que todas as variáveis são a sua medida estatisticamente significativas e a razão de chance que nada mais é que o valor do coeficiente exponencializado indicando quais as chances de uma variável ter importância num cenário A em comparação com o cenário B.

Longe de encher esse tópico de explicações teóricas, basta notar o seguinte, já nessa tabela observa-se o fator Idade aparecendo com uma certa relevância independente do cenário A (que nesse caso pode ser salarios Abaixo ou na Média) em comparação com o cenário B (que nesse caso escolheu-se Acima como categoria de referência).
#
df2 = table(Observado = newdf$`Faixa Salarial`, Previsto = predict(mod))

acuracia = sum(diag(df2)) / sum(df2)
#
Por fim, um cálculo de acuracidade apontando o potencial do nosso modelo em comparação ao que já fora observado. Nesse caso a acuracidade foi de 55%. O resultado pode não ser dos melhores, mas deve ajudar quanto ao objetivo principal.
#
**6. Análise Gráfica**

Para a construção dos gráficos foi utilizado o famoso pacote ggplot2.

![Gráfico 1](https://i.postimg.cc/g0nvTmC7/GR-FICO-1.png)

Nesse primeiro, observa-se a faixa salarial de acordo com a experiência anterior do profissional na área de dados, sendo o eixo Y a probabilidade e o eixo X a idade. Um ponto importante é a ausência de outliers no caso do entrevistado já ter mais de 3 anos de experiência na área de dados, isso significa que nesses casos, poucos fogem a regra. No caso de o profissional ser mais velho e ter menos de 3 anos de experiência em dados a probabilidade dele possuir um salário na Média é de quase 50%, um fato que muda no caso dele ter mais experiência, onde essa probabilidade cai consideravelmente, mas a probabilidade dele ter salários Acima sobem para quase 75%. Por outro lado, pouca experiência ou "instrução informal" - pode-se chamar assim - e pouca idade representa uma probabilidade altíssima de salário Abaixo da média.

![Gráfico 2](https://i.postimg.cc/ryQwKx0S/GR-FICO-2.png)

Já no segundo gráfico, a experiência na área de dados foi substituida pela instrução formal, aqui dividida em quatro possibilidades: Estudante, que pode ser estudantes de graduação ou de formação não identificada; Graduação; Especialização (pós-graduação) e Acadêmico (Mestrado e Doutorado). Em todos os casos nota-se uma certa ausência de outliers e o fenômeno notado anteriormente continua, ou seja, a medida que o entrevistado for do tipo acima de 40 anos, maior a probabilidade dele receber salários na Média ou Acima da média. Da mesma forma, as chances dos menores salários ficarem com os profissionais mais novos em idade é muito maior, sendo incrivelmente maior no caso deste profissional não ter sequer uma graduação reconhecida ou identificada.
#
**7. Considerações Finais**

Analisando tudo o que foi colocado acima, percebe-se que a idade é um fator importante quando o assunto é salário na área de Ciência de Dados, isso significa dizer que ele é mais importante que a instrução seja ela formal ou informal? A resposta é: depende. De fato, segundo a amostra analisada, sobretudo após a aplicação da regressão logística muntinominal, quanto mais velho, ou melhor, se tiver mais de 40 anos, maiores as chances de seu salário ser algo entre 6 mil ou mais dentro dessa área, enquanto que tendo pouco mais de 20 anos, ainda que com boa formação, essa probabilidade cai consideravelmente, ainda assim, o gráfico de correlação deixa claro também a importância da instrução.

Por fim, para os candidatos à área de Ciência de Dados, a melhor das hipóteses, caso o objetivo seja obter os melhores salários, seria algo como a junção entre idade e conhecimento.
#
**8. Referências**

(1) Idosos no mercado de trabalho: Entenda o cenário, vantagens e como incluí-los https://blog.solides.com.br/idosos-no-mercado-de-trabalho/#:~:text=Em%202018%2C%20o%20%C3%ADndice%20aumentou,%2C5%25%20registrados%20em%202013

(2) Recolocação após os 60 é tarefa difícil e individual https://infograficos.estadao.com.br/focas/planeje-sua-vida/recolocacao-apos-os-60-e-tarefa-dificil-e-individual

(3) Trabalhadores em cargos de liderança no mercado de trabalho formal brasileiro entre os anos de 1995, 2005 e 2015 https://periodicos2.uesb.br/index.php/ccsa/article/view/3238

(4) How Relevant is the Independence of Irrelevant Alternatives? https://statisticalhorizons.com/iia
