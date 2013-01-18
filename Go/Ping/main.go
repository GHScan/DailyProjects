package main
import (
    "fmt"
    "regexp"
    "os/exec"
)
func main() {
    reg, _ := regexp.Compile("Average = (\\d+)ms")
    urls := []string{
        "www.baidu.com", "www.qq.com", "www.sina.com", "www.sohu.com",
        "www.taobao.com", "www.163.com", "www.tianya.cn", "www.mop.com",
    }
    pipe := make(chan string)
    for _, url := range urls {
        go func(url string) {
            cmd := exec.Command("ping", url)
            if out, err := cmd.Output(); err == nil {
                pipe <- fmt.Sprintf("%s -> %sms", url,
                    reg.FindStringSubmatch(string(out))[1])
            } else {
                pipe <- fmt.Sprintf("%s -> failed(%v)", url, err)
            }
        }(url)
    }
    for i := 0; i < len(urls); i++ {
        fmt.Println(<- pipe)
    }
}
