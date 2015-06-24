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

func Pick(max int) int {
	var input string
	for {
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

type PadMode int

const (
	LeftPad PadMode = iota
	RightPad
)

func Pad(str string, pad rune, length int, mode PadMode) string {
	if len(str) >= length {
		return str
	} else {
		c := length - len(str)
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

func FormatStreams(input [][]string, pad []PadMode) []string {
	output := make([]string, len(input))
	paddings := make([]int, len(pad))

	for _, s := range input {
		for ci, c := range paddings {
			cl := len(s[ci])
			if cl > c {
				paddings[ci] = cl
			}
		}
	}

	for sid, s := range input {
		outcol := make([]string, len(paddings))
		for ci, c := range paddings {
			outcol[ci] = Pad(s[ci], ' ', c, pad[ci])
		}
		output[sid] = strings.Join(outcol, " ")
	}

	return output
}
