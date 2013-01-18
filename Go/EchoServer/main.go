package main

import (
    "net"
    "log"
    "bufio"
    "strings"
    "time"
    "fmt"
)

func clientThread(conn net.Conn) {
    defer conn.Close()
    addrStr := conn.RemoteAddr().String()
    reader, writer := bufio.NewReader(conn), bufio.NewWriter(conn)
    for {
        line, err := reader.ReadString('\n')
        if err != nil {
            log.Printf("[client (%v)] read failed -> %v\n", addrStr, err)
            break
        }
        line = strings.TrimSpace(line)
        log.Printf("[client (%v)] says -> %s\n", addrStr, line)
        if line == "quit" { break }
        writer.WriteString(fmt.Sprintf("[%v] : %s\r\n", time.Now(), line))
        writer.Flush()
    }
    log.Printf("[client (%v)] quit\n", addrStr)
}

func startMgrThread() {
    go func() {
        l, err := net.Listen("tcp", ":7000")
        if err != nil { log.Fatalln(err) }
        defer l.Close()

        log.Println("listen -> ", l.Addr())
        for {
            conn, err := l.Accept()
            if err != nil {
                log.Println("accept failed -> ", err)
                continue
            }
            log.Println("accept -> ", conn.RemoteAddr(), " --> ", conn.LocalAddr())
            go clientThread(conn)
        }
    }()
}

func main() {
    go startMgrThread()

    fmt.Println("press any key to shutdown...")
    input := ""
    fmt.Scanf("%s", &input)
}
