int main() {
	int x;

	printf("entre um numero de 1 a 3");
	scanf("%d", &x);

	switch(x) {
		case 1:
			printf("%d\n", x);
			break;
		case 2:
			printf("%d\n", x);
			break
		case 3:
			printf("%d\n", x);
			break;
		default:
			printf("valor de entrada invalido\n");
	}


	return 0;
}
