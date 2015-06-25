package util

import (
	"fmt"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"syscall"
)

func StartProgram(program string, url string) {
	binary, _ := exec.LookPath(program)
	args := []string{program, url}
	env := os.Environ()

	syscall.Exec(binary, args, env)
}

func Truncate(s string, max int) string {
	if len([]rune(s)) > max {
		return string([]rune(s)[:max])
	} else {
		return s
	}
}

func Pick(max int) int {
	var input string
	for {
		fmt.Print("Pick: ")
		_, err := fmt.Scanln(&input)
		if err != nil {
			fmt.Println(err)
			continue
		}
		out, err := strconv.ParseInt(input, 10, 32)
		outi := int(out)
		if err != nil {
			fmt.Println(err)
			continue
		}
		if outi < 1 || outi > max {
			fmt.Println("Out of range")
			continue
		}

		return outi
	}
}

type PadSet struct {
	Str string
	Pad PadMode
}

func P(str string, mode PadMode) *PadSet {
	return &PadSet{
		Str: str,
		Pad: mode,
	}
}

type PadMode int

const (
	LeftPad PadMode = iota
	RightPad
)

func Pad(str string, pad rune, length int, mode PadMode) string {
	if len([]rune(str)) >= length {
		return str
	} else {
		c := length - len([]rune(str))
		b := make([]rune, c)
		for i := range b {
			b[i] = pad
		}

		if mode == LeftPad {
			return string(b) + str
		} else {
			return str + string(b)
		}
	}

}

func PadFormat(input [][]PadSet) []string {
	output := make([]string, len(input))
	paddings := make([]int, len(input[0]))

	for _, s := range input {
		plen := len(s)
		if plen > len(paddings) {
			newp := make([]int, plen)
			copy(paddings, newp)
			paddings = newp
		}

		for ci, p := range s {
			cl := len([]rune(p.Str))
			if cl > paddings[ci] {
				paddings[ci] = cl
			}
		}
	}

	for sid, s := range input {
		outcol := make([]string, len(paddings))
		for ci, c := range s {
			outcol[ci] = Pad(c.Str, ' ', paddings[ci], c.Pad)
		}
		output[sid] = strings.Join(outcol, " ")
	}

	return output
}
