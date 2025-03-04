      !Subroutine que irá calcular a função f, em cada ponto de
      !interesse e chamará as subroutines que irão calcular e imprimir
      !as derivadas da função f.
      subroutine derivadas(h) 
        !Definindo o intervalo de letras onde desejo que todas as
        !variáveis que iniciem com uma letra dentro desse intervalo
        !sejam de dupla precisão.
        implicit real*8 (a-h,o-z)
        !Definindo o valor no qual se deseja calcular as derivadas.
        x = 0.5d0
        !Calculando os valores de f nos pontos de interesse.
        f0 = dcosh(3*x)*dsin(x/2)
        f_1 = dcosh(3*(x-h))*dsin((x-h)/2)
        f1 = dcosh(3*(x+h))*dsin((x+h)/2)
        f_2 = dcosh(3*(x-2*h))*dsin((x-2*h)/2)
        f2 = dcosh(3*(x+2*h))*dsin((x+2*h)/2)
        !Chamando as funções que, calculam de forma exata e os
        !diferentes métodos numéricos, para calcular os desvios de cada
        !método em relação ao valor exato.
        !Derivada simétrica 3 pontos.
        x1=abs(primeiraexata(0.5d0)-derivadas3p(f_1,f1,h))
        !Derivada 2 pontos para frente.
        x2=abs(primeiraexata(0.5d0)-derivada2ppf(f0,f1,h))
        !Derivada 2 pontos para traz.
        x3=abs(primeiraexata(0.5d0)-derivada2ppt(f_1,f0,h))
        !Derivada simétrica 5 pontos.
        x4=abs(primeiraexata(0.5d0)-derivadas5p(f_2,f_1,f1,f2,h))
        !Derivada segunda simétrica 5 pontos.
        x5=abs(segundaexata(0.5d0)-
     &derivadass5p(f_2,f_1,f0,f1,f2,h))
        !Derivada terceira anti-simétrica 5 pontos.
        x6=abs(terceiraexata(0.5d0)-
     &derivadatas5p(f_2,f_1,f1,f2,h))
        write(2,100) h, x1, x2, x3, x4, x5, x6
        !Definindo o formato de saída do programa.
100     format (1x,'|',g8.3,'|' ,4(f16.12,'|'),f18.12'|',f24.12,'|')
      end subroutine derivadas

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !Função que calcula o valor da primeira derivada de forma
      !analítica.
      function primeiraexata(x)
        !Definindo o intervalo de letras onde desejo que todas as
        !variáveis que iniciem com uma letra dentro desse intervalo
        !sejam de dupla precisão.
        implicit real*8 (a-h,o-z)
        primeiraexata=3*dsinh(3*x)*dsin(x/2)+
     &(1/2.d0)*dcosh(3*x)*dcos(x/2)
        return
      end function primeiraexata

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !Função que calcula o valor da segunda derivada de forma
      !analítica.
      function segundaexata(x)
        !Definindo o intervalo de letras onde desejo que todas as
        !variáveis que iniciem com uma letra dentro desse intervalo
        !sejam de dupla precisão.
        implicit real*8 (a-h,o-z)
        segundaexata=(35/4.d0)*dcosh(3*x)*dsin(x/2)+
     &(3.d0)*dsinh(3*x)*dcos(x/2)
        return
      end function segundaexata

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !Função que calcula o valor da terceira derivada de forma
      !analítica
      function terceiraexata(x)
        !Definindo o intervalo de letras onde desejo que todas as
        !variáveis que iniciem com uma letra dentro desse intervalo
        !sejam de dupla precisão.
        implicit real*8 (a-h,o-z)
        terceiraexata=(99/4.d0)*dsinh(3*x)*dsin(x/2)+
     &(107/8.d0)*dcosh(3*x)*dcos(x/2)
        return
      end function terceiraexata

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !Função que calculará e imprimirá o valor da derivada pelo método
      !de derivada 2 pontos para frente.
      function derivada2ppf(f0,f1,h) 
        !Definindo o intervalo de letras onde desejo que todas as
        !variáveis que iniciem com uma letra dentro desse intervalo
        !sejam de dupla precisão.
        implicit real*8 (a-h,o-z)
        derivada2ppf = (f1-f0)/h
        return
      end function derivada2ppf

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !Função que calculará e imprimirá o valor da derivada pelo método
      !de derivada 2 pontos para traz.
      function derivada2ppt(f_1,f0,h) 
        !Definindo o intervalo de letras onde desejo que todas as
        !variáveis que iniciem com uma letra dentro desse intervalo
        !sejam de dupla precisão.
        implicit real*8 (a-h,o-z)
        derivada2ppt = (f0-f_1)/h
        return
      end function derivada2ppt

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !Função que calculará e imprimirá o valor da derivada pelo método
      !de derivada simétrica 3 pontos.
      function derivadas3p(f_1,f1,h) 
        !Definindo o intervalo de letras onde desejo que todas as
        !variáveis que iniciem com uma letra dentro desse intervalo
        !sejam de dupla precisão.
        implicit real*8 (a-h,o-z)
        derivadas3p = (f1-f_1)/(2*h)
        return
      end function derivadas3p

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !Função que calculará e imprimirá o valor da derivada pelo método
      !de derivada simétrica 5 pontos.
      function derivadas5p(f_2,f_1,f1,f2,h)
        !Definindo o intervalo de letras onde desejo que todas as
        !variáveis que iniciem com uma letra dentro desse intervalo
        !sejam de dupla precisão.
        implicit real*8 (a-h,o-z)
        derivadas5p = (f_2-8*f_1+8*f1-f2)/(12*h)
        return
      end function derivadas5p

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !Função que calculará e imprimirá o valor da segunda deivada pelo
      !método de derivada segunda simétrica 5 pontos.
      function derivadass5p(f_2,f_1,f0,f1,f2,h)
        !Definindo o intervalo de letras onde desejo que todas as
        !variáveis que iniciem com uma letra dentro desse intervalo
        !sejam de dupla precisão.
        implicit real*8 (a-h,o-z)
        derivadass5p = (-f_2+16*f_1-30*f0+16*f1-f2)/(12*(h**2))
        return
      end function derivadass5p

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !Função que calculará e imprimirá o valor da terceira derivada
      !pelo método de terceira derivada anti-simétrica de 5 pontos.
      function derivadatas5p(f_2,f_1,f1,f2,h)
        !Definindo o intervalo de letras onde desejo que todas as
        !variáveis que iniciem com uma letra dentro desse intervalo
        !sejam de dupla precisão.
        implicit real*8 (a-h,o-z)
        derivadatas5p = (-f_2+2*f_1-2*f1+f2)/(2*(h**3))
        return
      end function derivadatas5p

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !Inicio da execução do programa.
      program main
        !Definindo o intervalo de letras onde desejo que todas as
        !variáveis que iniciem com uma letra dentro desse intervalo
        !sejam de dupla precisão.
        implicit real*8 (a-h,o-z)
        open(2,file='saída-1-tarefa-A.')
        !Escrevendo no arquivo de saída o cabeçalho da tabela com os
        !dados obtidos.
        write(2,*) '----------------------------------------------------
     &------------------------------------------------------------------
     &----'
        write(2,*) '|    h   |Derivada simé-  |Derivada p/     |Derivada
     & p/ traz |Derivada simé-  |Derivada segunda  |Derivada terceira an
     &ti-  |'
        write(2,*) '|        | trica 3 pontos | frente 2 pontos|    2 po
     &ntos    | trica 5 pontos |simétrica 5 pontos|simétrica 5 pontos   
     &     |'
        write(2,*) '----------------------------------------------------
     &------------------------------------------------------------------
     &----'
        !Chamando a subroutina que imprime o valor da derivada por todos
        !os métodos solicitados para diferentes valores de h.
        call derivadas(0.5d0)
        call derivadas(0.2d0)
        call derivadas(0.1d0)
        call derivadas(0.05d0)
        call derivadas(0.01d0)
        call derivadas(0.005d0)
        call derivadas(0.001d0)
        call derivadas(0.0005d0)
        call derivadas(0.0001d0)
        call derivadas(0.00005d0)
        call derivadas(0.00001d0)
        call derivadas(0.000001d0)
        call derivadas(0.0000001d0)
        call derivadas(0.00000001d0)
        !Escrevendo no arquivo de saída os valores das derivadas
        !calculadas de forma analítica
        write(2,*) '----------------------------------------------------
     &------------------------------------------------------------------
     &----'
        write(2,50) primeiraexata(0.5d0), segundaexata(0.5d0),
     &terceiraexata(0.5d0)
        write(2,*) '----------------------------------------------------
     &------------------------------------------------------------------
     &----'
        close(2)
        !Definindo os formatos no qual os valores calculados de forma
        !analítica serão escritos no arquivo de saída.
50      format(1x,'|  Exato |',21x,f16.11,30x,'|',2x,f16.11,'|'8x,f16.11
     &,'|')
      end program main              
