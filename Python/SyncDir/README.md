1. gen hash file for master
    - copy scripts to master
    - 'bash syncDir.sh genHashOfMaster'
2. move slave files to correct path
    - copy scripts to slave
    - copy masterHash.txt to slave directory
    - 'bash syncDir.sh genHashOfSlave'
    - 'bash syncDir.sh moveByMasterHash'
    - 'bash syncDir.sh genHashOfSlave'
    - 'comm -23 masterHash.txt slaveHash.txt > masterPrivateHash.txt' or
        'bash syncDir.sh genPrivateHashOfMaster' 
3. backup private files of master
    - copy masterPrivateHash.txt to master directory
    - 'bash syncDir.sh backupPrivateFilesOfMater'
4. restore private files of master to slave
    - copy backup directory to slave
    - 'bash syncDir.sh restorePrivateFilesOfMasterToSlave'
5. cleanup
    - cd master directory
    - 'bash syncDir.sh cleanup'
    - cd slave directory
    - 'bash syncDir.sh cleanup'
6. swap the master and slave, redo things above
