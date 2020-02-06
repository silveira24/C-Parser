{
int num1, num2, aux;
printf("Entre com dois numeros inteiros: ");
scanf("%d %d", &num1, &num2);
if (num1 > num2) {
aux = num1;
num1 = num2;
num2 = aux;
printf("Trocou \n");
}
printf("Os numeros ordenados: %d %d\n", num1, num2);
}
