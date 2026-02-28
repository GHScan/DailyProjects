document.addEventListener('DOMContentLoaded', async () => {
    const fileTreeElement = document.getElementById('file-tree');

    try {
        const response = await fetch('/api/files');
        const data = await response.json();
        
        if (data.length === 0) {
            fileTreeElement.textContent = '该目录下没有发现视频文件。';
            return;
        }

        fileTreeElement.innerHTML = '';
        renderTree(data, fileTreeElement);
    } catch (err) {
        console.error('Failed to fetch file list:', err);
        fileTreeElement.textContent = '加载失败，请重试。';
    }

    function renderTree(nodes, parentElement) {
        const ul = document.createElement('ul');
        ul.className = 'tree-list';
        ul.style.listStyle = 'none';
        ul.style.paddingLeft = '0';

        nodes.forEach(node => {
            const li = document.createElement('li');
            li.className = 'tree-item';

            if (node.type === 'directory') {
                const folderSpan = document.createElement('span');
                folderSpan.className = 'folder';
                folderSpan.textContent = node.name;
                
                const childrenDiv = document.createElement('div');
                childrenDiv.className = 'children hidden';
                
                folderSpan.addEventListener('click', () => {
                    folderSpan.classList.toggle('open');
                    childrenDiv.classList.toggle('hidden');
                });

                li.appendChild(folderSpan);
                renderTree(node.children, childrenDiv);
                li.appendChild(childrenDiv);
            } else {
                const fileLink = document.createElement('a');
                fileLink.className = 'file';
                fileLink.href = `player.html?path=${encodeURIComponent(node.path)}&name=${encodeURIComponent(node.name)}`;
                fileLink.textContent = node.name;
                li.appendChild(fileLink);
            }

            ul.appendChild(li);
        });

        parentElement.appendChild(ul);
    }
});
