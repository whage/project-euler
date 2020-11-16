package main

import (
	"math"
	"fmt"
)

func isWholeNumber(n float64) bool {
	return n - math.Floor(n) == 0
}

func isPerfectSquare(n int64) bool {
	return isWholeNumber(math.Sqrt(float64(n)))
}

// INCORRECT!
// OVERFLOWS!
// for D=61 it prints:
//                     335159612^2 - 61x42912791^2 = 1
// But that is 3 not 1!
func findValueOfD(dMax int64) int64 {
	var finalD int64
	var largestX int64 = 3
	var finalY int64 = 2
	var D int64
	for D = 2; D <= dMax; D++ {
		if isPerfectSquare(D) { continue; } // assuming no solution exists when D is square
		var y int64 = 1
		for {
			//fmt.Printf("Current D: %d, trying y: %d\n", D, y)
			var ySquared int64 = y*y
			if isPerfectSquare(D*ySquared+1) {
				currentX := int64(math.Sqrt(float64(D*ySquared+1)))
				if currentX >= largestX {
					largestX = currentX
					finalD = D
					finalY = y
				}
				fmt.Printf("%d^2 - %dx%d^2 = 1\n", currentX, D, y)
				break
			}
			y++
		}
	}
	fmt.Println("largest X", largestX)
	fmt.Println("final Y", finalY)
	return finalD
}

func main() {
	fmt.Println("final D", findValueOfD(106))
}

// running times
// 1000 -> 30sec
// 2000 -> 60sec
// 4000 -> 138sec
