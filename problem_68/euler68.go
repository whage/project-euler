package main

import (
	"fmt"
)

type Solution map[string]int
type Constraint func(map[string]int) bool

func isValidSolution(s Solution) bool {
	goal1 := s["A"] + s["i"] + s["j"] == s["B"] + s["j"] + s["k"]
	goal2 := s["B"] + s["j"] + s["k"] == s["c"] + s["k"] + s["i"]
	return goal1 && goal2
}

func main() {
	baseSolution := Solution{
		"A": 4,
		"B": 5,
		"C": 6,
		"i": 1,
		"j": 2,
		"k": 3,
	}
	constraints := []Constraint{
		func(s map[string]int) bool { return s["A"] + s["i"] + s["j"] == 0 },
	}
	fmt.Println(constraints)
	fmt.Println(solve(baseSolution))
}

func next(s Solution) (bool, Solution) {
	// TODO
	return false, s
}

func solve(s Solution) (bool, Solution) {
	if isValidSolution(s) { return true, s }
	if ok, nextStep := next(s); ok {
		solve(nextStep)
	}
	return false, nil
}
