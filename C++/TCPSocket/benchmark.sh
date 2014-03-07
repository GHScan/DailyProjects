#! /bin/bash

select benchmarkType in taskGen httpTaskGen
do
    case $benchmarkType in
        taskGen)
                read -p 'Input the concurrent value: ' concurrent
                read -p 'Input the task count: ' taskCount

                cd BlockingClient
                mkdir tmp
                
                echo "benchmark for BlockingClient
                echo "python taskGen.py $taskCount | ./main $concurrent" | python ../benchmark_any.py
                
                rm -rf tmp
                cd ..
                #------------------------------
                cd NonblockingClient
                mkdir tmp
                
                echo "benchmark for NonblockingClient:"
                echo "python taskGen.py $taskCount | ./main -n 1 -p epoll -m $concurrent " | python ../benchmark_any.py
                
                rm -rf tmp
                cd ..
            ;;
        httpTaskGen)
                read -p 'Input page url: ' pageUrl
                read -p 'Input the concurrent value: ' concurrent
                cd BlockingClient
                mkdir tmp

                echo "benchmark for BlockingClient"
                echo "python ../innerUrlPaser.py $pageUrl | python httpTaskGen.py | ./main $concurrent" | python ../benchmark_any.py

                rm -rf tmp
                cd ..
                #------------------------------
                cd NonblockingClient
                mkdir tmp

                echo "benchmark for NonblockingClient:"
                echo "python ../innerUrlPaser.py $pageUrl | python httpTaskGen.py | ./main -n 1 -p epoll -m $concurrent" | python ../benchmark_any.py

                rm -rf tmp
                cd ..
            ;;
    esac
done
