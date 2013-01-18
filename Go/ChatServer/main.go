package main

import (
    "fmt"
    "log"
    "net"
    "bufio"
    "sync"
    "strings"
)
// Data define
type Client struct {
    name string
    msgChan chan string
}
type ClientManager struct {
    name2Client map[string]*Client
    mutex *sync.Mutex
}
// ClientManager
func NewClientManager() *ClientManager {
    return &ClientManager{make(map[string]*Client), &sync.Mutex{}}
}
func (this *ClientManager) Enter(name string, client *Client) bool {
    this.mutex.Lock()
    defer this.mutex.Unlock()
    if len(name) == 0 { return false }
    if this.name2Client[name] != nil { return false }
    log.Println(name, "enter")
    this.broadcast(fmt.Sprintf("%s enter", name))
    this.name2Client[name] = client
    return true
}
func (this *ClientManager) Get(name string) *Client {
    this.mutex.Lock()
    defer this.mutex.Unlock()
    return this.name2Client[name]
}
func (this *ClientManager) Leave(name string) {
    this.mutex.Lock()
    defer this.mutex.Unlock()
    if this.name2Client[name] == nil { log.Fatalln("logic error!") }
    delete(this.name2Client, name)
    log.Println(name, "leave")
    this.broadcast(fmt.Sprintf("%s leave", name))
}
func (this *ClientManager) broadcast(msg string) {
    for _, client := range this.name2Client {
        client.Send("system", msg)
    }
}
// Client
func CreateClient(conn net.Conn, mgr *ClientManager) {
    client := Client{}

    reader, writer := bufio.NewReader(conn), bufio.NewWriter(conn)
    for {
        writer.WriteString("[system] -> please enter your name :\r\n")
        writer.Flush()
        if line, err := reader.ReadString('\n'); err != nil {
            conn.Close()
            return
        } else {
            line = strings.TrimSpace(line)
            if mgr.Enter(line, &client) {
                client.name = line
                break
            }
        }
    }

    client.msgChan = make(chan string)
    go func() {
        for {
            msg := <- client.msgChan
            if msg == "onQuit" { break }
            writer.WriteString(msg)
            writer.Flush()
        }
        log.Println("exit msg thread", client.name)
    }()

    go func() {
        defer conn.Close()

        for {
            line, err := reader.ReadString('\n')
            if err != nil  {
                log.Printf("[client (%v)] -> read failed : %v\n", client.name, err)
                break
            }
            line = strings.TrimSpace(line)
            if line == "quit" { break }
            if tokens := strings.Split(line, ":"); len(tokens) < 2 {
                writer.WriteString("[system] -> must specific target\r\n")
                writer.Flush()
            } else {
                if o := mgr.Get(tokens[0]); o == nil {
                    writer.WriteString("[system] -> can't find the one\r\n")
                    writer.Flush()
                } else {
                    o.Send(client.name, tokens[1])
                }
            }
        }
        mgr.Leave(client.name)

        client.msgChan <- "onQuit"
        log.Println("exit net thread", client.name)
    }()
}
func (this *Client) Send(from string, msg string) {
    this.msgChan <- fmt.Sprintf("[%s] -> %s\r\n", from, msg)
}
// server
func StartServer(mgr *ClientManager) {
    l, err := net.Listen("tcp", ":7000")
    if err != nil { log.Fatalln("listen faild") }
    log.Println("listen -> ", l.Addr())

    go func() {
        defer l.Close()

        for {
            if conn, err := l.Accept(); err != nil {
                log.Println("accept failed", err)
                continue
            } else {
                log.Println("accept -> ", conn.RemoteAddr(), "--->", conn.LocalAddr())
                CreateClient(conn, mgr)
            }
        }
    }()
}

func main() {
    mgr := NewClientManager()
    StartServer(mgr)

    fmt.Println("press any key to shutdown...")
    input := ""
    fmt.Scanf("%s", &input)
}
