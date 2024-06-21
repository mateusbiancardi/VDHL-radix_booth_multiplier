# Multiplicador Booth Radix4 juntamente com suporte a entrada de dados pela interface PS2 do teclado

Esse repositório armazena o código do multiplicador Booth Radix4 com suporte a interface PS2 do teclado e que faz a FPGA Artix 7 se comunicar com um computador utilizando a UART e a FIFO. Esse código foi produzido para a matéria Sistemas Digitais da Universidade Federal do Espírito Santo (UFES) pelo aluno Mateus Biancardi da Silva, estudante de Engenharia Elétrica, sob orientação da professora Eliete Maria de Oliveira Caldeira.
É necessário ter o programa Vivado para programar a FPGA, e também é preciso da utilização do programa Realterm para realizar a comunicação entre a FPGA e o computador.

A FPGA Artix 7 não tem entrada de dados PS2, então é utilizado a porta USB e depois a tradução para o protocolo PS2.
