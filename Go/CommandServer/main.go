package main

import (
    "fmt"
    "net"
    "log"
    "runtime"
    "bufio"
    "strings"
    "os/exec"
)

func clientThread(conn net.Conn) {
    defer conn.Close()

    addrStr := conn.RemoteAddr().String()
    log.Println("client enter :", addrStr)

    reader, writer := bufio.NewReader(conn), bufio.NewWriter(conn)
    writer.WriteString("please input the commmand:\r\n")
    writer.Flush()

    for {
        if line, err := reader.ReadString('\n'); err != nil {
            log.Println("read failed :", addrStr)
            break
        } else {
            line = strings.TrimSpace(line)
            if line == "quit" {
                break
            } else {
                args := strings.Split(line, " ")
                cmd := exec.Command(args[0], args[1:]...)
                str := ""
                if out, err := cmd.Output(); err != nil {
                    str = fmt.Sprintf("%v", err)
                } else {
                    str = string(out)
                }
                writer.WriteString(str + "\r\n")
                writer.Flush()
            }
        }
    }

    log.Println("cleint leave :", addrStr)
}

func serverThread() {
    l, err := net.Listen("tcp", ":7000")
    if err != nil { log.Fatalln("listen failed!") }
    defer l.Close()
    log.Println("listen -> ", l.Addr())

    for {
        if conn, err := l.Accept(); err != nil {
            log.Println("accept failed!")
            continue
        } else {
            go clientThread(conn)
        }
    }
}

func main() {
    runtime.GOMAXPROCS(runtime.NumCPU())
    go serverThread()

    fmt.Println("press any key to shutdown...")
    input := ""
    fmt.Scanf("%s", &input)
}
