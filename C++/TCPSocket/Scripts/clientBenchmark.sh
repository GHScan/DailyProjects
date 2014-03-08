#! /bin/bash


select benchmarkType in taskGen httpTaskGen
do
    case $benchmarkType in
        taskGen)
                read -p 'Input the task count: ' taskCount
                read -p 'Input the concurrent value: ' concurrent
                read -p 'Input the max active task : ' maxActiveTask

                for clientDir in *Client
                do
                    cd $clientDir
                    mkdir tmp

                    echo "@@@@@@@@@@@@@@@ benchmark for $clientDir"
                    echo "python ../Scripts/taskGen.py $taskCount | ./main -n $concurrent -m $maxActiveTask" | python ../Scripts/benchmark_any.py

                    rm -rf tmp
                    cd ..
                done
            ;;

        httpTaskGen)
                read -p 'Input page url: ' pageUrl
                read -p 'Input the concurrent value: ' concurrent
                read -p 'Input the max active task : ' maxActiveTask

                for clientDir in *Client
                do
                    cd $clientDir
                    mkdir tmp

                    echo "@@@@@@@@@@@@@@@ benchmark for $clientDir"
                    echo "python ../Scripts/innerUrlPaser.py $pageUrl | python ../Scripts/httpTaskGen.py | ./main -n $concurrent -m $maxActiveTask" | python ../Scripts/benchmark_any.py

                    rm -rf tmp
                    cd ..
                done
            ;;
    esac
done
