package util

import (
	"fmt"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"syscall"
	"unsafe"
)

func GetSize() (width, height int) {
	var dimensions [4]uint16
	if _, _, err := syscall.Syscall6(syscall.SYS_IOCTL, uintptr(1), uintptr(syscall.TIOCGWINSZ), uintptr(unsafe.Pointer(&dimensions)), 0, 0, 0); err != 0 {
		return -1, -1
	}

	return int(dimensions[1]), int(dimensions[0])
}

func StartProgram(program string, url string) {
	binary, _ := exec.LookPath(program)
	args := []string{program, url}
	env := os.Environ()

	syscall.Exec(binary, args, env)
}

func Truncate(s string, max int) string {
	r := []rune(s)
	if len(r) > max {
		return string(r[:max])
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
