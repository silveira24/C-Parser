int main()
{
	int num;
	printf("Entre com um inteiro: ");
	scanf("%d", &num);
	if (num >= 0) {
	if (num % 2 == 0)
	printf("O numero e par e positivo\n");
	else
	printf("O numero e impar e positivo\n");
	}
	else {
	if (num % 2 == 0)
	printf("O numero e par e negativo\n");
	else
	printf("O numero e impar e negativo\n");
	}

	return 0;
}