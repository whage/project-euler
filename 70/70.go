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

func myTotient(n int) int {
	count := 1 // "1" is always a coprime
	for i := 2; i < n; i++ {
		if areRelativePrimes(n, i) { count += 1 }
	}
	return count
}

// https://cp-algorithms.com/algebra/phi-function.html
func totientUsingPrimeFactorization(n int) int {
	result := n
	for i := 2; i * i <= n; i++ {
		if n % i == 0 {
			for n % i == 0 {
				n /= i
			}
			result -= result / i
		}
	}
	if n > 1 {
		result -= result / n
	}
	return result
}

type DigitCount map[int]int

func (a DigitCount) isEqual(b DigitCount) bool {
	for k, v := range map[int]int(a) {
		other, ok := b[k]
		if !ok || other != v { return false }
	}
	return true
}

func getDigits(x int) DigitCount {
	counter := DigitCount{}
	for x > 0 {
		counter[x%10] += 1
		x /= 10
	}
	return counter
}

func iterate(limit int) {
	var minRatio float64 = 10
	min := 1
	for i := 2; i <= limit; i++ {
		//phi := myTotient(i)
		phi := totientUsingPrimeFactorization(i)
		//fmt.Println(phi)
		d1 := getDigits(i)
		d2 := getDigits(phi)
		if ! d1.isEqual(d2) { continue }
		fmt.Println("candidate:", i, "phi: ", phi)
		ratio := float64(i)/float64(phi)
		if ratio <= minRatio {
			minRatio = ratio
			min = i
			fmt.Println("New min ratio: ", minRatio, "min: ", min)
		}
	}
}

func main() {
	//fmt.Println(getDigits(524).isEqual(getDigits(450)))
	iterate(10000000)
}
