      !Definindo a subroutine que calculará o valor da integral
      !integral por todos os métodos (trapézio, simpson, boole e
      !simpson38).
      subroutine integrais(pi, N)
        !Transformando toda a sintaxe que é real*4 por padrão no
        !fortran, para real*8.
        implicit real*8 (a-h,o-z)
        !Calculando o valor de h para N
        h = 1.d0/N
        !Chamando a subroutine que calculará o valor da integral
        !pelo método do trapézio.
        x1 = abs(trapezio(pi, N, h) - exata(pi))
        !Chamando a subroutine que calculará o valor da integral
        !pelo método de simpson.
        x2 = abs(simpson(pi, N, h) - exata(pi))
        !Chamando a subroutine que calculará o valor da integral
        !pelo método de boole.
        x3 = abs(boole(pi, N, h) - exata(pi))
        !Chamando a subroutine que calculará o valor da integral
        !pelo método dde simpson3/8.
        x4 = abs(simpson38(pi, N, h) - exata(pi))
        !Escrevendo um separador na tela, para identificar mais
        !facilmente quando o valor de N mudou.
        write(2,100) N, N, x1, x2, x3
100     format(1x,'|'i4,'|','1/',i4,'|',3(f15.12,'|'))
      end subroutine integrais
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !Definindo a função que calculará o valor da integral pela
      !expressão analítica.
      function exata(pi)
        !Transformando toda a sintaxe que é real*4 por padrão no
        !fortran, para real*8.
        implicit real*8 (a-h,o-z)
        !Calculando o valor da integral pela expressão analítica.
        exata=(-4)*dexp(1/4.d0)*(4*pi*dcos(pi)-dsin(pi))/
     &(1+16*3.14**2)-(-4)*dexp(0.d0)*(4*pi*dcos(0.d0)-dsin(0.d0))/
     &(1+16*pi**2)
        return
      end function exata
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !Definindo a função que calculará o valor da integral pelo
      !método do trapezio.
      function trapezio(pi, N, h)
        !Transformando toda a sintaxe que é real*4 por padrão no
        !fortran, para real*8.
        implicit real*8 (a-h,o-z)
        !Definindo o valor inicial da soma como 0.
        trapezio = 0.d0
        !Definindo o looping que calculará o valor de cada pedaço da
        !integral e adicionará ao valor da mesma.
        do i = 0, N-1, 2
          !Definindo o valor de x para cada valor de i do looping.
          x = (i+1)*h
          !Calculando o valor de f0.
          f0 = dexp(x/4)*dsin(pi*x)
          !Calculando o valor de f_1.
          f_1 = dexp((x-h)/4)*dsin(pi*(x-h))
          !Calculando o valor de f1.
          f1 = dexp((x+h)/4)*dsin(pi*(x+h))
          !Somando todas as partes da integral.
          trapezio = trapezio + (h/2)*(f_1+2*f0+f1)
        end do
        return
      end function trapezio
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      !Definindo a função que calculará o valor da integral pelo método
      !de simpson.
      function simpson(pi, N, h)
        !Transformando toda a sintaxe que é real*4 por padrão no
        !fortran, para real*8.
        implicit real*8 (a-h,o-z)
        !Definindo o valor inicial da soma como 0.
        simpson = 0.d0
        !Definindo o looping que calculará o valor de cada pedaço da
        !integral e adicionará ao valor da mesma.
        do i = 0, N-1, 2
          !Definindo o valor de x para cada valor de i do looping.
          x = (i+1)*h
          !Calculando o valor de f0.
          f0 = dexp(x/4)*dsin(pi*x)
          !Calculando o valor de f_1.
          f_1 = dexp((x-h)/4)*dsin(pi*(x-h))
          !Calculando o valor de f1.
          f1 = dexp((x+h)/4)*dsin(pi*(x+h))
          !Somando todas as partes da integral.
          simpson = simpson + (h/3)*(f_1+4*f0+f1)
        end do
        return
      end function simpson
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !Definindo a função que calculará o valor da integral pelo método
      !de boole
      function boole(pi, N, h)
        !Transformando toda a sintaxe que é real*4 por padrão no
        !fortran, para real*8.
        implicit real*8 (a-h,o-z)
        !Definindo o valor incial da soma como 0.
        boole = 0.d0
        !Definindo o looping que calculará o valor de cada pedaço da
        !integral e adicionará ao valor da mesma.
        do i = 0, N-1, 4
          !Definindo o valor de x para cada valor de i do looping.
          x = i*h
          !Calculando o valor de f0.
          f0 = dexp(x/4)*dsin(pi*x)
          !Calculando o valor de f1.
          f1 = dexp((x+h)/4)*dsin(pi*(x+h))
          !Calculando o valor de f2.
          f2 = dexp((x+2*h)/4)*dsin(pi*(x+2*h))
          !Calculando o valor de f3.
          f3 = dexp((x+3*h)/4)*dsin(pi*(x+3*h))
          !Calculando o valor de f4.
          f4 = dexp((x+4*h)/4)*dsin(pi*(x+4*h))
          !Somando todas as partes da integral.
          boole = boole + ((2*h)/45)*(7*f0+32*f1+12*f2+32*f3
     &+7*f4)
        end do
        return
      end function boole
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !Definindo a função que calculará o valor da integral pelo método
      !de simpson 3/8
      function simpson38(pi, N, h)
        !Transformando toda a sintaxe que é real*4 por padrão no
        !fortran, para real*8.
        implicit real*8 (a-h,o-z)
        !Definindo o valor inicial da soma como 0.
        simpson38 = 0.d0
        !Definindo o looping que calculará o valor de cada pedaço da
        !integral e adicionará ao valor da mesma.
        do i = 0, N-1, 3
          !Definindo o valor de x para cada valor de i do looping.
          x = i*h
          !Calculando o valor de f0.
          f0 = dexp(x/4)*dsin(pi*x)
          !Calculando o valor de f1.
          f1 = dexp((x+h)/4)*dsin(pi*(x+h))
          !Calculando o valor de f2.
          f2 = dexp((x+2*h)/4)*dsin(pi*(x+2*h))
          !Calculando o valor de f3.
          f3 = dexp((x+3*h)/4)*dsin(pi*(x+3*h))
          !Somando todas as partes da integral.
          simpson38 = simpson38 + ((3*h)/8)*(f0+3*f1+3*f2+f3)
        end do
        return
      end function simpson38
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      !Iniciando o programa que executará todos os cálculos
      program main
        !Transformando toda a sintaxe que é real*4 por padrão no
        !fortran, para real*8.
        implicit real*8 (a-h,o-z)
        !Definindo o valor de pi
        pi = dacos(-1d0)
        !Abrindo o arquivo na onde serão armazenados os dados gerados
        !pelo programa.
        open(2,file='saída-1-tarefa-B.')
        !Imprimindo o cabeçalho da tabela que irá conter os dados do
        !programa.
        write(2,*) '----------------------------------------------------
     &---------'
        write(2,*) '| N  |  h   |   Regra do    |   Regra de    |   Regr
     &a de    |'
        write(2,*) '|    |      |     Trapézio  |     Simpson   |     B 
     &oole     |'
        write(2,*) '----------------------------------------------------
     &---------'
        !Chamando a subroutine que calcula o valor da integral por
        !diferentes métodos para diferentes valores de N.
        call integrais(pi, 12)
        call integrais(pi, 24)
        call integrais(pi, 48)
        call integrais(pi, 96)
        call integrais(pi, 192)
        call integrais(pi, 384)
        call integrais(pi, 768)
        call integrais(pi, 1536)
        call integrais(pi, 3072)
        call integrais(pi, 6144)
        !Imprindo o valor exato calculado de forma analítica.
        write(2,*) '----------------------------------------------------
     &---------'
        write(2,50) exata(pi)
        write(2,*) '----------------------------------------------------
     &---------'
50      format(1x,'|   Exato   |',15x,f16.11,16x,'|')
        !Fechando o arquivo na onde os dados do programa foram
        !armazenados.
        close(2)
      end program main
