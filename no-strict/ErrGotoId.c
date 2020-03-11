int main() {

	int valor = 0;

	while (1) {
		valor++;
		printf("%d\n", valor);
		if (valor == 3) {
			goto ;
		}
	}
	fim:
		printf("fim do programa");

	return 0;
}
