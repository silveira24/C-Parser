int main() 
{
	int a, b;
	printf("Entre com uma fracao (numerador and denominador): ");
	scanf("%d %d", &a, &b);
	if (b != 0)
	printf("A fracao decimal e %f\n", 1.0 * a / b);
	else
	printf("Erro: denominador zero!\n");

	return 0;
}
