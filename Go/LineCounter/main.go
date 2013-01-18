package main

import (
    "fmt"
    "os"
    "path/filepath"
    "strings"
    "sort"
    "io/ioutil"
    "bufio"
)

func main() {
    exts := ".h;.c;.hpp;.cpp;.cc;.cs;.lua;.py;.go"
    {
        fmt.Println("ext list:", exts)
        fmt.Println("append ext:")
        input := ""
        fmt.Scanf("%s", &input)
        if len(input) > 0 {
            if input[0] != ';' { input = ";" + input }
            if input[len(input) - 1] == ';' { input = input[:len(input) - 1]}
        }
        exts += input
    }
    extList := sort.StringSlice(strings.Split(exts, ";"))
    extList.Sort()

    fmt.Println("input the path:")
    path := ""
    fmt.Scanf("%s", &path)

    resultChan := make(chan int)
    remainCnt := 0
    totalLineNum := 0
    filepath.Walk(path, func(path string, info os.FileInfo, err error) error {
        if info.IsDir() { return err }
        ext := filepath.Ext(path)
        if pos := extList.Search(ext); pos >= len(extList) || extList[pos] != ext {
            return err
        }

        remainCnt++
        go func (path string) {
            lineNum := 0
            if bytes, err := ioutil.ReadFile(path); err == nil {
                reader := bufio.NewReader(strings.NewReader(string(bytes)))
                for {
                    if _, isPrefix, err := reader.ReadLine(); err == nil {
                        for err == nil && isPrefix {
                            _, isPrefix, err = reader.ReadLine()
                        }
                        lineNum += 1
                    } else {
                        break
                    }
                }
            } else {
                fmt.Printf("%s open failed(%v)\n", path, err)
            }
            resultChan <- lineNum
        }(path)

        return err
    })

    for ; remainCnt > 0; remainCnt-- {
        totalLineNum += <- resultChan
    }
    fmt.Println("lines: ", totalLineNum)
    fmt.Scanf("%s", &path)
    fmt.Scanf("%s", &path)
}
