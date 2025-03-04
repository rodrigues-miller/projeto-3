      !Calculando o valor da raiz da função pelo método de busca direta
      !onde ipar indica qual raiz é desejada, sendo elas primeira,
      !segunda e terceira.
      function direta(ipar)
        !Passando as variáveis que por padrão são REAL*4 no fortran para
        !REAL*8.
        implicit real*8(a-h, o-z)
        !Definindo o valor onde iniciará a busca.
        x0 = -10d0
        !Atribuindo o valor do passo inicial da busca.
        h = 0.1d0
        !Atribuindo o valor do segundo ponto de busca.
        x1 = x0 + h
        !Atribuindo o valor incial ao contator que indicará quantas
        !raizes já foram encontradas.
        icont = 0
        !Calculando o valor da função nos dois primeiros pontos.
        f0 = x0**3.d0-(3.d0/2.d0)*(x0**2.d0)-(3.d0/2.d0)*x0+1.d0
        f1 = x1**3.d0-(3.d0/2.d0)*(x1**2.d0)-(3.d0/2.d0)*x1+1.d0
        !Iniciando o loop da busca das raízes.
        do i = 1, 1000000
          !Definindo a condicional em que a precisão desejada já foi
          !atingida.
          if (abs(x1-x0) < 0.000001d0) then
            h = 0.1d0
            x0 = x1 + h
            icont = icont + 1
            !Condição para sair da função pois já foi encontrada a raíz
            !desejada.
            if (icont == ipar) then
              direta = x1
              return
            end if
            goto 10
          end if
          !Condição para inverter a direção do passo e ir se aproximando
          !mais do valor real da raíz.
          if (f1*f0 < 0) then
            h = (-h)/2
          end if
          !Condição em que uma raíz foi encontrada.
          if (f1*f0 == 0) then
            icont = icont + 1
            !Condição para sair da função pois já foi encontrada a raíz
            !desejada.
            if (icont == ipar) then
              direta = x1
              return
            end if
            !Reajustando o valor do passo "h".
            h = 0.1d0
            !Avançando o passo para continuar procurando a raíz.
            x0 = x1 + h
            goto 10
          end if
          x0 = x1
10        x1 = x0 + h
          !Caculando a função em x0 e x1.
          f0 = x0**3.d0-(3.d0/2.d0)*(x0**2.d0)-(3.e0/2.d0)*x0+1.d0
          f1 = x1**3.d0-(3.d0/2.d0)*(x1**2.d0)-(3.e0/2.d0)*x1+1.d0
        end do 
      end function direta
!-----------------------------------------------------------------------
      !Calculando o valor da raiz da função pelo método de Newton.
      function anewton(x)
        !Passando as variáveis que por padrão são REAL*4 no fortran para
        !REAL*8.
        implicit real*8 (a-h,o-z)
        !Iniciando o loop que utilizará o método de newton para
        !encontrar a raíz da função.
        do i = 1, 100000
          f = x**3.d0-(3.d0/2.d0)*(x**2.d0)-(3.d0/2.d0)*x+1.d0
          df = 3.d0*(x**2.d0)-3.d0*x-(3.d0/2.d0)
          x1 = x - f/df
          !Quando atingida a precisão procurada sair do loop.
          if (abs(x1-x) < 0.000001d0) then
            exit
          end if
          x = x1
        end do
        !Atribuindo o valor a função e retornando.
        anewton = x1
        return
      end function anewton
!-----------------------------------------------------------------------
      !Calculando o valor da raiz da função pelo método da secante.
      function secante(x)
        !Passando as variáveis que por padrão são REAL*4 no fortran para
        !REAL*8.
        implicit real*8 (a-h,o-z)
        h = 0.1d0
        !Iniciando o loop que utilizará o método da secante para
        !encontrar a raíz.
        do i = 1, 100000
          x_1 = x-h
          f = x**3.d0-(3.d0/2.d0)*(x**2.d0)-(3.d0/2.d0)*x+1.d0
          f_1 = x_1**3.d0-(3.d0/2.d0)*(x_1**2.d0)-(3.d0/2.d0)*x_1+1.d0
          df = 3.d0*(x**2.d0)-3.d0*x-(3.d0/2.d0)
          x1 = x - f*((x-x_1)/(f-f_1))
          !Quando atingida a precisão procurada sair do loop.
          if (abs(x1-x) < 0.000001d0) then
            exit
          end if
          x = x1
        end do
        secante = x1
      end function secante
!-----------------------------------------------------------------------
      !Iniciando o programa que será executado
      program main
        !Passando as variáveis que por padrão são REAL*4 no fortran para
        !REAL*8.
        implicit real*8 (a-h,o-z)
        !Abrindo o arquivo que armazena os dados de saída.
        open(2, file='saída-1-tarefa-C.')
        x=-10d0
        write(2,101)
        write(2,102)
        write(2,101)
        write(2,100) direta(1), secante(x), anewton(x)
        x=1
        write(2,100) direta(2), secante(x), anewton(x)
        x=10
        write(2,100) direta(3), secante(x), anewton(x)
        write(2,101)
        close(2)
        !Definindo os formatos utilizados na saída de dados.
100     format('|',f20.15,'|',f20.15,'|',f20.15,'|')
101     format('--------------------------------------------------------
     &--------')
102     format('|    Busca direta    |      Secante       |       Newton
     &       |')
      end program main
