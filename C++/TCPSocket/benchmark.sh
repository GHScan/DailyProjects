#! /bin/bash

select benchmarkType in taskGen httpTaskGen
do
    case $benchmarkType in
        taskGen)
                read -p 'Input the concurrent value: ' concurrent
                read -p 'Input the task count: ' taskCount

                cd TestSyncClient
                mkdir tmp
                
                echo "benchmark for TestSyncClient:"
                echo "python taskGen.py $taskCount | ./main $concurrent" | python ../benchmark_any.py
                
                rm -rf tmp
                cd ..
                #------------------------------
                cd TestAsyncClient
                mkdir tmp
                
                echo "benchmark for TestAsyncClient:"
                echo "python taskGen.py $taskCount | ./main -n 1 -p epoll -m $concurrent " | python ../benchmark_any.py
                
                rm -rf tmp
                cd ..
            ;;
        httpTaskGen)
                read -p 'Input page url: ' pageUrl
                read -p 'Input the concurrent value: ' concurrent
                cd TestSyncClient
                mkdir tmp

                echo "benchmark for TestSyncClient:"
                echo "python ../innerUrlPaser.py $pageUrl | python httpTaskGen.py | ./main $concurrent" | python ../benchmark_any.py

                rm -rf tmp
                cd ..
                #------------------------------
                cd TestAsyncClient
                mkdir tmp

                echo "benchmark for TestAsyncClient:"
                echo "python ../innerUrlPaser.py $pageUrl | python httpTaskGen.py | ./main -n 1 -p epoll -m $concurrent" | python ../benchmark_any.py

                rm -rf tmp
                cd ..
            ;;
    esac
done
