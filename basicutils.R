sum_vector = function(v) {
	cat("system sum:",sum(v),"\n");
	sum = 0;
	for (i in 1:length(v)){
		sum = sum + v[i];
	}
	cat("my sum:", sum, "\n");
}

avg_vector = function(v) {
	cat("system mean:",mean(v),"\n");
	sum = 0;
	for (i in 1:length(v)){
		sum = sum + v[i];
	}
	cat("my mean:", sum/length(v), "\n");
}

var_vector = function(v) {
	cat("system variance:",var(v),"\n");
	sigma = 0;
	for (i in 1:length(v)){
		sigma = sigma + (v[i] - mean(v))^2 
	}
	cat("my variance:", sigma/(length(v)-1), "\n");
}