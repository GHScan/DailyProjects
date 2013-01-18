// just for posix
package main

import (
    "net"
    "log"
    "bufio"
    "fmt"
    "os/exec"
    "strings"
    "runtime"
)

func clientThread (conn net.Conn) {
    defer conn.Close()

    addStr := conn.RemoteAddr().String()
    log.Println("client enter -> ", addStr)

    cmd := exec.Command("bash")
    var preader *bufio.Reader
    var pwriter *bufio.Writer
    {
        w, _ := cmd.StdinPipe()
        pwriter = bufio.NewWriter(w)
        r, _ := cmd.StdoutPipe()
        preader = bufio.NewReader(r)
    }
    cmd.Start()

    sreader := bufio.NewReader(conn)
    swriter := bufio.NewWriter(conn)

    go func() {
        for {
            line, err := preader.ReadString('\n')
            if err != nil { break }
            swriter.WriteString(line)
            swriter.Flush()
        }
        log.Println("client 2nd thread exit -> ", addStr)
    }()

    for {
        line, err := sreader.ReadString('\n')
        if err != nil {
            log.Println("client read err", err, addStr)
            break
        }
        line = strings.TrimSpace(line)
        if line == "exit" { break }
        line = strings.TrimSpace(line)
        pwriter.WriteString(line + "\n")
        pwriter.Flush()
    }

    pwriter.WriteString("exit\n")
    pwriter.Flush()

    log.Println("client leave -> ", addStr)
}

func serverThread() {
    l, err := net.Listen("tcp", ":7000")
    if err != nil { log.Fatalln("listen failed!") }
    defer l.Close()

    log.Println("listen -> ", l.Addr())

    for {
        conn, err := l.Accept()
        if err != nil {
            log.Println("accept failed ->", err)
            continue
        }
        go clientThread(conn)
    }
}

func main() {
    runtime.GOMAXPROCS(runtime.NumCPU())

    go serverThread()

    fmt.Println("press any key to shutdown...")
    input := ""
    fmt.Scanf("%s", &input)
}
