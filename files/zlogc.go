package zlog
//package main
 
import (
	"fmt"
	"net"
	"os"
	"flag"
	"strconv"
	"time"
)
 
func checkError(err error) {
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}
 
func Log_udp(host string, port int, format string, a ...interface{}) {
	address := host + ":" + strconv.Itoa(port)
	udpConn, err := net.Dial("udp", address)
	checkError(err)

	defer udpConn.Close()

	text := fmt.Sprintf(format, a...)
	timestamp := time.Now().String()
	timestamp = timestamp[:23]
	text = timestamp + " " + text

	var n int
	n, err = udpConn.Write([]byte(text))
	checkError(err)

	fmt.Printf("%v bytes sent\n", n)
}

func Log_file(format string, a ...interface{}) {
	text := fmt.Sprintf(format, a...)
	if text[len(text) - 1] != '\n' {
		text = text + "\n"
	}
	timestamp := time.Now().String()
	timestamp = timestamp[:23]
	text = timestamp + " " + text
	logfile, err := os.OpenFile("/tmp/zlog", os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0600)
	checkError(err)
	logfile.Write([]byte(text))
	logfile.Close()
}

func main() {
    var text string
    var host string
    var port int

    flag.StringVar(&text, "t", "", "")
    flag.StringVar(&host, "h", "localhost", "")
    flag.IntVar(&port, "p", 8888, "")
    flag.Parse()

    fmt.Printf("text=%v host=%v port=%v\n", text, host, port)

	Log_udp(host, port, "hello %v\n", "golang udp")
	Log_file("hello %v", "golang file")

	fmt.Println(time.Now().String())
}

