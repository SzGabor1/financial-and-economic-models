emp <- function(input) {
	Mean <- mean(input);
	print('Atlag:');
	print(Mean);
	frac <- 1/(length(input)-1);
	Sum <- 0;
	for (i in 1:length(input)) {
  		Sum = Sum + (input[i] - Mean)^2;
	}
	print("Empirikus eloszlas:");
	return(frac*Sum);
}
x = rnorm(100)
emp(x)