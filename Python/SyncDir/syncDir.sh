function genHashOfMaster
{
    find . -mindepth 2 -type f | iconv -f utf-8 -t gbk > _list.txt
    python syncDir.py genHash _list.txt _masterHash.txt
    sort _masterHash.txt > masterHash.txt
    rm _masterHash.txt
    rm _list.txt
}
function genHashOfSlave
{
    find . -mindepth 2 -type f | iconv -f utf-8 -t gbk > _list.txt
    python syncDir.py genHash _list.txt _slaveHash.txt
    sort _slaveHash.txt > slaveHash.txt
    rm _slaveHash.txt
    rm _list.txt
}
function moveByMasterHash() 
{
    python syncDir.py moveByHash masterHash.txt slaveHash.txt
    find . -type d -empty | xargs rmdir
}
function genPrivateHashOfMaster() 
{
    comm -23 masterHash.txt slaveHash.txt > masterPrivateHash.txt
}
function backupPrivateFilesOfMater() 
{
    python syncDir.py backupTo masterPrivateHash.txt _backup
}
function restorePrivateFilesOfMasterToSlave() 
{
    python syncDir.py restoreFrom _backup
}
function cleanup() {
    rm -f masterHash.txt slaveHash.txt masterPrivateHash.txt
    rm -rf _backup
}

$@
