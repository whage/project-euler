package main

import (
	"fmt"
)

// expects x > y
func areRelativePrimes(x, y int) bool {
	m := x % y
	for m != 0 {
		if m == 1 { return true }
		x = y
		y = m
		m = x % y
	}
	return false
}

func totient(n int) int {
	count := 1 // "1" is always a coprime
	for i := 2; i < n; i++ {
		if areRelativePrimes(n, i) { count += 1 }
	}
	return count
}

func main() {
	var maxRatio float64 = 0
	max := 2
	for i := 2; i <= 20000; i++ {
		t := totient(i)
		var ratio float64 = float64(i) / float64(t)
		if ratio >= maxRatio {
			maxRatio = ratio
			max = i
		}
		//fmt.Printf("i: %d, totient(i): %d\n", i, totient(i))
	}
	fmt.Printf("maxRatio: %f, max: %d\n", maxRatio, max)
}
