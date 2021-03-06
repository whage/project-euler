package main

import (
	"fmt"
	"math"
)

type ResultPair struct {
	max int
	maxRatio float64
}

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

func findMaxRatioOfRange(from, to int) (float64, int) {
	var maxRatio float64 = 0
	max := 2
	for i := from; i <= to; i+=1 {
		t := totient(i)
		var ratio float64 = float64(i) / float64(t)
		if ratio >= maxRatio {
			maxRatio = ratio
			max = i
		}
		fmt.Printf("i: %d, totient(i): %d\n", i, totient(i))
	}
	return maxRatio, max
}

func runInParallel() {
	c := make(chan int)
	go func() {
		_, m:= findMaxRatioOfRange(2, 25000)
		c <- m
	} ()
	go func() {
		_, m:= findMaxRatioOfRange(25001, 50000)
		c <- m
	} ()
	go func() {
		_, m:= findMaxRatioOfRange(50001, 75000)
		c <- m
	} ()
	go func() {
		_, m:= findMaxRatioOfRange(75001, 100000)
		c <- m
	} ()

	finishedCount := 0
	max := 0
	for result := range c {
		finishedCount += 1
		if finishedCount == 4 { close(c) }
		if result >= max {
			max = result
		}
	}
	fmt.Printf("max: %d\n", max)
}

func runSequentially() {
	maxRatio, max := findMaxRatioOfRange(2, 10000)
	fmt.Printf("maxRatio: %f, max: %d\n", maxRatio, max)
}

func sieveOfEratosthenes(n int) []int {
	A := make([]bool, n)
	for idx, _ := range A {
		A[idx] = true
	}
	for i := 2; float64(i) <= math.Sqrt(float64(n)); i++ {
		if A[i] {
			for j := i*i; j < n; j += i {
				A[j] = false
			}
		}
	}
	results := make([]int,0,n)
	for idx, _ := range A {
		if idx == 0 || idx == 1 { continue }
		if A[idx] {
			results = append(results, idx)
		}
	}
	return results
}

func multiplyPrimes() int {
	primes := sieveOfEratosthenes(200)
	product := 1
	for i := 0; product*primes[i] <= 1_000_000; i++ {
		product *= primes[i]
		fmt.Println(product)
	}
	return product
}

func main() {
	//runSequentially()
	//runInParallel()
	fmt.Println(multiplyPrimes())
}
