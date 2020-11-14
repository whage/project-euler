package main

import (
	"fmt"
)

type Arrangement [6]int
type Constraint func(Arrangement) bool

func isSolution(s Arrangement) bool {
	goal0 := s[0] + s[1] + s[2] + s[3] + s[4] + s[5] > 0
	goal1 := s[0] + s[3] + s[4] == s[1] + s[4] + s[5]
	goal2 := s[1] + s[4] + s[5] == s[2] + s[5] + s[3]
	return goal0 && goal1 && goal2
}

func main() {
	baseArrangement := Arrangement{1,0,0,0,0,0}
	/*
	constraints := []Constraint{
		func(s Arrangement) bool { return true },
	}
	fmt.Println(constraints)
	*/
	ok, result := solve(baseArrangement, 0)
	fmt.Println("solve", ok, result)
}

func getNextValueForNode(s Arrangement, nodeIndex int) (bool, int) {
	possibleValues := []int{1,2,3,4,5,6}
	for _, candidate := range possibleValues {
		found := false
		for i := 0; i < nodeIndex; i++ {
			if candidate == s[i] { found = true }
		}
		if !found { return true, candidate}
	}

	return false, 0

}

func getNextCandidateArrangement(s Arrangement, nodeIndex int) (bool, Arrangement) {
	if nodeIndex > 5 {
		// reached max number on last node, no more possible Arrangements
		return false, Arrangement{}
	}

	if ok, nextValue := getNextValueForNode(s, nodeIndex); ok {
		s[nodeIndex] = nextValue
		return true, s
	} else {
		return getNextCandidateArrangement(s, nodeIndex+1)
	}
}

func solve(s Arrangement, nodeIndex int) (bool, Arrangement) {
	if isSolution(s) { return true, s }
	if ok, nextCandidate := getNextCandidateArrangement(s, nodeIndex+1); ok {
		fmt.Println(nextCandidate)
		solve(nextCandidate, nodeIndex+1)
	}
	return false, Arrangement{}
}
