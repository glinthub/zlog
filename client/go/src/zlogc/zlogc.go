package zlogc
 
import (
	"fmt"
	"flag"
	"net"
	"os"
	"os/exec"
	"strconv"
	"time"
	"runtime"
	"path"
	"strings"
)
 
func checkError(err error) {
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}
 
func make_text(format string, a...interface{}) string {
	timestamp := time.Now().String()
	timestamp = timestamp[:23]
	hostname, _ := os.Hostname()
	_, file, line, _ := runtime.Caller(2)
	file = path.Base(file)
	pid := os.Getpid()
	text := fmt.Sprintf(format, a...)
	text = fmt.Sprintf("%v %v %v ln %v pid %v: " + text, 
		timestamp, hostname, file, line, pid)
	if text[len(text)-1] != '\n' {
		text = text + "\n"
	}
	fmt.Print(text)
	return text
}

func Log_udp(host string, port int, format string, a ...interface{}) {
	address := host + ":" + strconv.Itoa(port)
	udpConn, err := net.Dial("udp", address)
	checkError(err)

	defer udpConn.Close()

	text := make_text(format, a...)
	//var n int
	_, err = udpConn.Write([]byte(text))
	checkError(err)

	//fmt.Printf("%v bytes sent\n", n)
}

func Log_file(format string, a ...interface{}) {
	text := make_text(format, a...)
	logfile, err := os.OpenFile("/tmp/zlog", os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0600)
	checkError(err)
	logfile.Write([]byte(text))
	logfile.Close()
}

func is_localhost(addr string) bool {
	if strings.HasPrefix(addr, "127.") ||
		strings.HasPrefix(addr, "::1") {
		return true
	}
	return false
}

func CleanLoopbackAddress() {
	output, err := exec.Command("ip", "address", "show", "dev", "lo").Output()
	//output, err := exec.Command("cat", "t.txt").Output()
	lines := strings.Split(string(output), "\n")
	//fmt.Printf("%v\n", lines)
	for _,line := range lines {
		line = strings.TrimLeft(line, " ")
		if strings.HasPrefix(line, "inet") {
			//fmt.Printf("%s \n", line)
			fields := strings.Split(line, " ")
			addr := fields[1]
			fmt.Printf("%v \n", addr)
			if is_localhost(addr) {
				continue
			}
			cmd := exec.Command("ip", "address", "del", addr, "dev", "lo")
			cmd.Run()
		}
	}
	if err != nil {
		fmt.Println("error")
	}

}

func TestMain() {
    var text string
    var host string
    var port int

    flag.StringVar(&text, "t", "", "")
    flag.StringVar(&host, "h", "127.0.0.1", "")
    flag.IntVar(&port, "p", 8888, "")
    flag.Parse()

    fmt.Printf("text=%v host=%v port=%v\n", text, host, port)
	Log_udp(host, port, "golang zlogc udp: %v\n", text)
	Log_file("golang zlogc file: %v \n", text)
	Log_file("golang zlogc file without newline")
}
